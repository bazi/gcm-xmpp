
Compile rebar project:

    ./rebar3 compile

Set API key and sender ID values in https://github.com/bazi/gcm-xmpp/blob/master/apps/gcm/src/gcm_xmpp.erl definedas macros.

Run Erlang console with our compiled beam files:

    erl -pa  _build/default/lib/*/ebin/

In Erlang console, run commands below. There will be messages displayed.

    gcm_xmpp:start_link().
    gcm_xmpp:send("hello", "device-token-here").


