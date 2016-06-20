%%%-------------------------------------------------------------------
%%% @author azilet
%%% @copyright (C) 2016
%%% @doc
%%%
%%% @end
%%% Created : 10. May 2016 2:41 PM
%%%-------------------------------------------------------------------
-module(gcm_xmpp).

-behaviour(application).
-behaviour(gen_server).

-export([
  start_link/0,
  send/2,
  stop/0
]).

-export([
  start/2,
  stop/1
]).

-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).


-define(SERVER, ?MODULE).

-define(API_KEY, "api-key-here").
-define(SENDER_ID, "sender-id-here").

-define(STREAM_STANZA, <<"<stream:stream to='gcm.googleapis.com' version='1.0' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams'>">>).

-define(STEP_INIT, 0).
-define(STEP_AUTH, 1).
-define(STEP_STRM, 2).
-define(STEP_BIND, 3).
-define(STEP_DONE, 4).

-record(state, {
  socket,
  step
}).


%%%%%%%%%%%%%%%
%%%   API   %%%
%%%%%%%%%%%%%%%

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


init([]) ->
  lager:start(),
  ssl:start(),
  Socket = ccs_connect(),
  {ok, #state{socket = Socket, step = ?STEP_INIT}}.


send(Message, DeviceToken) ->
  gen_server:cast(?SERVER, {send, Message, DeviceToken}).


stop() ->
  gen_server:cast(?SERVER, stop).


%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CALLBACK FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%


start(_Starttype, _StartArgs) ->
  start_link().

stop(_State) ->
  gen_server:cast(?SERVER, stop).


handle_call(_Request, _From, State) ->
  {reply, ok, State}.


handle_cast({send, Message, DeviceToken}, State = #state{socket = Socket}) ->
  Message_id = message_id(),
  BMessage = list_to_binary(Message),
  BToken = list_to_binary(DeviceToken),
  Data = build_payload(Message_id, BMessage, <<"topic">>, BToken),
  Stanza = make_stanza("", binary_to_list(Data)),
  case ssl:send(Socket, Stanza) of
    {error, closed} ->
      lager:error("Trying to send to closed socket.");
    ok ->
      lager:info("GCM XMPP sent:~n~p", [Stanza])
  end,
  {noreply, State};


handle_cast(stop, State) ->
  {stop, normal, State};


handle_cast(_Request, State) ->
  {noreply, State}.


handle_info(Info, State = #state{socket = Socket, step = Step}) ->
  case Info of
    {ssl, _, Data} ->
      lager:info("GCM XMPP raw response:~n~p", [Data]),
      %% TODO: do better recognition of stanza - parse xml and check by xpath
      IsPlainAuthOk = string:str(binary_to_list(Data), "PLAIN") > 0,
      IsAuthSuccess = string:str(binary_to_list(Data), "success") > 0,
      IsBindFeature = string:str(binary_to_list(Data), "bind") > 0,
      IsBindSuccess = string:str(binary_to_list(Data), "jid") > 0,

      NewState = if
        Step =:= ?STEP_INIT, IsPlainAuthOk ->
          ccs_auth(State);

        Step =:= ?STEP_AUTH, IsAuthSuccess ->
          %% if our authentication is successful, we have to send stream stanza
          ssl:send(Socket, ?STREAM_STANZA),
          State#state{step = ?STEP_STRM};

        Step =:= ?STEP_STRM, IsBindFeature ->
          %% in response to our stream stanza, we should receive iq stanza with bind feature, and so we send bind stanza
          IqId = binary_to_list(message_id()),
          BindStanza = "<iq type='set' id='" ++ IqId ++ "'><bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'></bind></iq>",
          ssl:send(Socket, BindStanza),
          State#state{step = ?STEP_BIND};

        Step =:= ?STEP_BIND, IsBindSuccess ->
          %% upon successful binding we receive jid (you can save it if needed) and we are done!
          State#state{step = ?STEP_DONE};

        true ->
          State
      end,
      {noreply, NewState};

    {ssl_closed, Socket} ->
      lager:error("GCM XMPP SSL socket closed: ~p", [Socket]),
      {stop, normal, State};

    {ssl_error, Socket, Reason} ->
      lager:info("GCM XMPP SSL(~p) error: ~p", [Socket, Reason]),
      {stop, normal, State};

    _ ->
      lager:info("GCM XMPP unrecognized info: ~p", [Info]),
      {stop, normal, State}
  end.


terminate(_Reason, #state{socket = Socket}) ->
  ssl:close(Socket),
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%
%%% PRIVATE FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%

ccs_connect() ->

  {ok, Socket} = ssl:connect("gcm-preprod.googleapis.com", 5236, []),
  ssl:setopts(Socket, [
    {mode, binary}
  ]),

  ssl:send(Socket, ?STREAM_STANZA),
  Socket.


ccs_auth(#state{socket = Socket} = State) ->
  %% make auth message as per PLAIN SALS specs
  Sender = ?SENDER_ID ++ "@gcm.googleapis.com",
  Sasl = Sender ++ [0] ++ Sender ++ [0] ++ ?API_KEY,
  Sasl_base64 = base64:encode_to_string(Sasl),

  Auth_xml = "<auth mechanism='PLAIN' xmlns='urn:ietf:params:xml:ns:xmpp-sasl'>" ++ Sasl_base64 ++ "</auth>",
  ssl:send(Socket, Auth_xml),
  State#state{step = ?STEP_AUTH}.


-spec message_id() -> binary().
message_id() ->
  T = erlang:system_time(),
  integer_to_binary(T).


build_payload(Message_id, Message, Topic, DeviceToken) ->
  Payload_map = #{
    <<"to">>                => DeviceToken,
    <<"message_id">>        => Message_id,
    <<"data">>              => #{<<"message">> => Message},
    <<"collapse_key">>      => Topic,
    <<"time_to_live">>      => 600,
    <<"priority">>          => <<"high">>
  },
  jiffy:encode(Payload_map).

-spec make_stanza(list(), list()) -> binary().
make_stanza(MessageId, Payload) ->
  Stanza = {message, [{id, MessageId}],
    [{gcm, [{xmlns, "google:mobile:data"}], [Payload]}]
  },
  Xml = xmerl:export_simple([Stanza], xmerl_xml),
  unicode:characters_to_binary(Xml).

