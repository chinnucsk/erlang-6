-module(upload).
-export([start/0]).

start() ->
    Conn = inets:start(),
    {Megasecs, Secs, Microsecs} = erlang:now(),
    MilliSecondsSinceEpoch = (Megasecs * 1000000 * 1000000) + Secs*1000000 + Microsecs,
    {Status, {{Version, Code, ReasonPhrase}, Headers, Body}} = 
    	httpc:request(post, {"http://t-sync.gionee.com/sync/data/put.do",
    			[{"Authorization", 
			 "MAC id=\"8f239c36659c4f6ebc276037dbb07b6a\",ts=\"" 
			 ++ integer_to_list(MilliSecondsSinceEpoch)
			 ++ "\",nonce=\"lasjdfloqweuro\",mac=\"lsjdfjaldf\""} ], 
    			"html/text",
    			"---body"}, 
    		      [], []),
    io:format("~p ~p ~p ~p ~p ~p~n", [Version, Status, Code, ReasonPhrase, Headers, Body]),
    inets:stop(httpc, Conn).
