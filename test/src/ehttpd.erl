-module(ehttpd).
-compile(export_all).

start() ->
    start(8888).
start(Port) ->
    SchedulerCount = erlang:system_info(schedulers),
    listen(Port, SchedulerCount),
    io:format("ehttpd ready with ~b schedulers on port ~b~n", [SchedulerCount, Port]),
 
    register(?MODULE, self()),
    receive Any -> io:format("~p~n", [Any]) end.  %% to stop: ehttpd!stop.
 
listen(Port, SchedulerCount) ->
    Opts = [{active, false},
            binary,
            {backlog, 256},
            {packet, http_bin},
            {raw,6,9,<<1:32/native>>}, %defer accept
            %%{delay_send,true},
            %%{nodelay,true},
            {reuseaddr, true}],
 
    {ok, S} = gen_tcp:listen(Port, Opts),
    Spawn = fun(I) ->   
                    register(list_to_atom("acceptor_" ++ integer_to_list(I)),
                             spawn_opt(?MODULE, accept, [S, I], [link, {scheduler, I}]))
            end,
    lists:foreach(Spawn, lists:seq(1, SchedulerCount)).
 
accept(S, I) ->
    case gen_tcp:accept(S) of
        {ok, Socket} -> spawn_opt(?MODULE, loop, [Socket], [{scheduler, I}]);
        Error    -> erlang:error(Error)
    end,
    accept(S, I).
 
loop(S) ->
    case gen_tcp:recv(S, 0) of
        {ok, http_eoh} ->
            Response = <<"HTTP/1.1 200 OK\r\nContent-Length: 12\r\n\r\nhello world!">>,
            gen_tcp:send(S, Response),
            gen_tcp:close(S),
            ok;
 
        {ok, _Data} ->
            loop(S);
 
        Error ->
            Error
    end.
