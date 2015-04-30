
-module(fifo_api).

%% fifo_api: fifo_api library's entry point.

-export([start/0, new/1, auth/3]).


%% API

start() ->
    application:ensure_all_started(fifo_api).

-spec new(Options :: fifo_api_http:con_options())  ->
                 fifo_api_http:connection().

new(Options) ->
    fifo_api_http:new(Options).

auth(Login, Pass, C) ->
    case fifo_users:auth(Login, Pass, C) of
        {ok, Token} ->
            {ok, fifo_api_http:set_token(Token, C)};
        E ->
            E
    end.

%% Internals

