-module(fifo_api).

%% fifo_api: fifo_api library's entry point.

-export([start/0]).
-export_type([connection/0]).

-record(connection, {}).

-type connection() :: #connection{}.

%% API

start() ->
    application:ensure_all_started(fifo_api).

%% Internals

