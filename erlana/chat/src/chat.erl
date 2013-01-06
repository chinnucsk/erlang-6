%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(chat).
-author('author <author@example.com>').
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
        
%% @spec start() -> ok
%% @doc Start the chat server.
start() ->
    chat_deps:ensure(),
    ensure_started(crypto),
    application:start(chat).

%% @spec stop() -> ok
%% @doc Stop the chat server.
stop() ->
    Res = application:stop(chat),
    application:stop(crypto),
    Res.
