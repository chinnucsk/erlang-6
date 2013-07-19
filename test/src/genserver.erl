-module(genserver).
-behaviour(gen_server).
-export([start/0, hello/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start() -> 
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

hello(Name) ->
  Reply = gen_server:call(?MODULE, {hello, Name}),
  io:format("~s~n", [Reply]).

handle_call({hello, Name}, _From, State) ->
  Reply = lists:append(["Hello, ", Name, "!"]),
  {reply, Reply, State}.

init(_Args) ->
  {ok, init}.
handle_cast(_Request, State) ->
  {noreply, State}.
handle_info(_Info, State) ->
  {noreplay, State}. 
terminate(_Reason, _State) ->
  ok.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

