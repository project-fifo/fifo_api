%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2015, Heinz Nikolaus Gies
%%% @doc
%%% HTTP helper functions for the fifo API.
%%% @end
%%% Created : 29 Jan 2015 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(fifo_api_http).

-export([new/1, get/2, get/3, post/3, put/3, set_token/2, url/2,
        delete/2, delete/3]).

-export([take_last/1, full_list/1]).

-export_type([connection/0, connection_options/0]).

-record(connection, {
          endpoint = "http://localhost" :: string(),
          prefix = "api" :: string(),
          version = "0.2.0" :: string(),
          token :: binary() | undefined
         }).

-type connection_options() ::
        {endpoint, string()} |
        {prefix, string()} |
        {token, binary()} |
        {version, string()}.

-type connection() :: #connection{}.

-define(MSGPACK,  <<"application/x-msgpack">>).
-define(JSON,  <<"application/json">>).

-define(POOL, fifo).
-define(MIN_OPTS, [{timeout, 5000}, {pool, ?POOL}]).
-define(OPTS, [{follow_redirect, true}, {max_redirect, 5} | ?MIN_OPTS]).

new(Options) ->
    case hackney_pool:find_pool(?POOL) of
        undefined ->
            PoolOpts = [{timeout, 5000}, {max_connections, 50}],
            ok = hackney_pool:start_pool(?POOL, PoolOpts);
        _ ->
            ok
    end,
    new(Options, #connection{}).

set_token(Token, C) ->
    C#connection{token = Token}.

get(Path, C) ->
    get(Path, [], C).

delete(Path, C) ->
    delete(Path, [], C).

get(Path, Opts, C) ->
    Method = get,
    URL = url(Path, C),
    ReqHeaders = token_opts([{<<"accept-encoding">>, ?MSGPACK} | Opts], C),
    ReqBody = <<>>,
    case hackney:request(Method, URL, ReqHeaders, ReqBody, ?OPTS) of
        {ok, 200, _H, Ref} ->
            {ok, Body1} = hackney:body(Ref),
            {ok, Body2} = msgpack:unpack(Body1, [jsx]),
            {ok, Body2};
         {ok, Error, _, Ref} ->
            hackney:body(Ref),
            {error, Error}
    end.


delete(Path, Opts, C) ->
    Method = delete,
    URL = url(Path, C),
    ReqHeaders = token_opts([{<<"accept-encoding">>, ?MSGPACK} | Opts], C),
    ReqBody = <<>>,
    case hackney:request(Method, URL, ReqHeaders, ReqBody, ?OPTS) of
        {ok, 200, _H, Ref} ->
            {ok, Body1} = hackney:body(Ref),
            {ok, Body2} = msgpack:unpack(Body1, [jsx]),
            {ok, Body2};
        {ok, 204, _H, Ref} ->
            hackney:body(Ref),
            ok;
        {ok, Error, _, Ref} ->
            hackney:body(Ref),
            {error, Error}
    end.

post(Path, Body, C = #connection{endpoint = Endpoint}) ->
    Method = post,
    URL = url(Path, C),
    ReqHeaders = token_opts([{<<"accept-encoding">>, ?MSGPACK},
                             {<<"content-type">>, ?MSGPACK}], C),
    ReqBody = msgpack:pack(Body, [jsx]),
    case hackney:request(Method, URL, ReqHeaders, ReqBody, ?MIN_OPTS) of
        {ok, 200, _H, Ref} ->
            {ok, Body1} = hackney:body(Ref),
            {ok, Body2} = msgpack:unpack(Body1, [jsx]),
            {ok, Body2};
        {ok, 303, H, Ref} ->
            hackney:body(Ref),
            Location = proplists:get_value(<<"location">>, H),
            case hackney:request(get, [Endpoint, Location],
                                 ReqHeaders, ReqBody, []) of
                {ok, 200, _H, Ref1} ->
                    {ok, Body1} = hackney:body(Ref1),
                    {ok, Body2} = msgpack:unpack(Body1, [jsx]),
                    {ok, Body2};
                {ok, Error, _, Ref1} ->
                    hackney:body(Ref1),
                    {error, Error}
            end;
        Error ->
            {error, Error}
    end.

put(Path, Body, C) ->
    Method = put,
    URL = url(Path, C),
    ReqHeaders = token_opts([{<<"accept-encoding">>, ?MSGPACK},
                             {<<"content-type">>, ?JSON}], C),
    ReqBody = msgpack:pack(Body, [jsx]),
    case hackney:request(Method, URL, ReqHeaders, ReqBody, ?OPTS) of
        {ok, 200, _H, Ref} ->
            {ok, Body1} = hackney:body(Ref),
            {ok, Body2} = msgpack:unpack(Body1, [jsx]),
            {ok, Body2};
        {ok, 204, _H, Ref} ->
            hackney:body(Ref),
            ok;
        {ok, Error, _, Ref} ->
            hackney:body(Ref),
            {error, Error}
    end.


url([$/ | Path], C) ->
    url(Path, C);

url(Path,
    #connection{endpoint = Endpoint, prefix = Prefix, version = Version}) ->
    [Endpoint, $/, Prefix, $/, Version, $/, Path].

take_last(L) ->
    take_last(L, []).
take_last([E], R) ->
    {lists:reverse(R), E};
take_last([E | R], L) ->
    take_last(R, [E | L]).

full_list(L) ->
    list_to_binary(string:join([to_l(E) || E <- L], ",")).


to_l(E) when is_list(E) ->
    E;
to_l(E) when is_binary(E) ->
    binary_to_list(E).

token_opts(O, #connection{token = undefined}) ->
    O;
token_opts(O,  #connection{token = Token}) ->
    [{<<"Authorization">>, <<"Bearer ", Token/binary>>} | O].

new([], C) ->
    C;

new([{endpoint, Endpoint} | R], C) when is_list(Endpoint) ->
    new(R, C#connection{endpoint = Endpoint});

new([{version, Version} | R], C) when is_list(Version) ->
    new(R, C#connection{version = Version});

new([{prefix, Prefix} | R], C) when is_list(Prefix) ->
    new(R, C#connection{prefix = Prefix});

new([{token, Token} | R], C) when is_binary(Token) ->
    new(R, C#connection{token = Token});

new([_ | R], C) ->
    new(R, C).
