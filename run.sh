clear
rm -fr *_bt.log
./rebar3 compile
erl +pc unicode -pa _build/default/lib/*/ebin -name main@127.0.0.1

