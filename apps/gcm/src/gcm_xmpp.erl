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

-record(state, {
  socket
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
  {ok, #state{socket = Socket}}.


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
  ok.


handle_call(_Request, _From, State) ->
  {reply, ok, State}.


handle_cast({send, Message, DeviceToken}, State = #state{socket = Socket}) ->
  Message_id = message_id(),
  Data = build_payload(Message_id, Message, "topic", DeviceToken),
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


handle_info(Info, State) ->
  case Info of
    {ssl, _, Data} ->
      lager:info("GCM XMPP raw response:~n~p", [Data]);
    {ssl_closed, Socket} ->
      lager:error("GCM XMPP SSL socket closed: ~p", [Socket]);
    {ssl_error, Socket, Reason} ->
      lager:info("GCM XMPP SSL(~p) error: ~p", [Socket, Reason]);
    _ ->
      lager:info("GCM XMPP unrecognized info: ~p", [Info])
  end,
  {noreply, State}.


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
    {mode, binary},
    {keepalive, true}
  ]),

  Stream_xml = <<"<stream:stream to=\"gcm.googleapis.com\" version=\"1.0\" xmlns=\"jabber:client\" xmlns:stream=\"http://etherx.jabber.org/streams\">">>,
  ssl:send(Socket, Stream_xml),

  %% make auth message as per PLAIN SALS specs
  Sasl = ?SENDER_ID ++ [0] ++ ?SENDER_ID ++ [0] ++ ?API_KEY,
  Sasl_base64 = base64:encode_to_string(Sasl),

  Auth_xml = "<auth mechanism=\"PLAIN\" xmlns=\"urn:ietf:params:xml:ns:xmpp-sasl\">" ++ Sasl_base64 ++ "</auth>",
  ssl:send(Socket, Auth_xml),
  Socket.


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

