-module(application_test).
-export([start/0]).

start() ->
	io:format("hello"),
	application:start(application_test).