%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2015, Heinz Nikolaus Gies
%%% @doc
%%% HTTP helper functions for the fifo API.
%%% @end
%%% Created : 29 Jan 2015 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(fifo_api_http).

-export([new/1, get/2, get/3, post/3, put/3, set_token/2, url/2, full_list/1,
        delete/2, delete/3]).

-export_type([connection/0, connection_options/0]).

-record(connection, {
          endpoint = "http://localhost" :: string(),
          prefix = "api" :: string(),
          version = "0.1.0" :: string(),
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

new(Options) ->
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
    Options = [{follow_redirect, true}, {max_redirect, 5}],
    case hackney:request(Method, URL, ReqHeaders, ReqBody, Options) of
        {ok, 200, H, Ref} ->
            {ok, Body1} = hackney:body(Ref),
            {ok, Body2} = msgpack:unpack(Body1),
            {ok, H, Body2};
         {ok, Error, _, _} ->
            {error, Error}
    end.

delete(Path, Opts, C) ->
    Method = delete,
    URL = url(Path, C),
    ReqHeaders = token_opts([{<<"accept-encoding">>, ?MSGPACK} | Opts], C),
    ReqBody = <<>>,
    Options = [{follow_redirect, true}, {max_redirect, 5}],
    case hackney:request(Method, URL, ReqHeaders, ReqBody, Options) of
        {ok, 200, H, Ref} ->
            {ok, Body1} = hackney:body(Ref),
            {ok, Body2} = msgpack:unpack(Body1),
            {ok, H, Body2};
        {ok, 204, _H, _} ->
            ok;
        {ok, Error, _, _} ->
            {error, Error}
    end.

post(Path, Body, C) ->
    Method = post,
    URL = url(Path, C),
    ReqHeaders = token_opts([{<<"accept-encoding">>, ?MSGPACK},
                             {<<"content-type">>, ?JSON}], C),
    ReqBody = msgpack:pack(Body, [jsx]),
    Options = [{follow_redirect, true}, {max_redirect, 5}],
    case hackney:request(Method, URL, ReqHeaders, ReqBody, Options) of
        {ok, 200, H, Ref} ->
            {ok, Body1} = hackney:body(Ref),
            {ok, Body2} = msgpack:unpack(Body1),
            {ok, H, Body2};
         {ok, Error, _, _} ->
            {error, Error}
    end.

put(Path, Body, C) ->
    Method = put,
    URL = url(Path, C),
    ReqHeaders = token_opts([{<<"accept-encoding">>, ?MSGPACK},
                             {<<"content-type">>, ?JSON}], C),
    ReqBody = msgpack:pack(Body, [jsx]),
    Options = [{follow_redirect, true}, {max_redirect, 5}],
    case hackney:request(Method, URL, ReqHeaders, ReqBody, Options) of
        {ok, 200, H, Ref} ->
            {ok, Body1} = hackney:body(Ref),
            {ok, Body2} = msgpack:unpack(Body1),
            {ok, H, Body2};
        {ok, 204, _H, _} ->
            ok;
         {ok, Error, _, _} ->
            {error, Error}
    end.


url(["/" | Path], C) ->
    url(Path, C);

url(Path,
    #connection{endpoint = Endpoint, prefix = Prefix, version = Version}) ->
    Endpoint ++ "/" ++ Prefix ++ "/" ++ Version ++ "/" ++ Path.

full_list(L) ->
    list_to_binary(string:join([to_l(E) || E <- L], ",")).


to_l(E) when is_list(E) ->
    E;
to_l(E) when is_binary(E) ->
    binary_to_list(E).

token_opts(O, #connection{token = undefined}) ->
    O;
token_opts(O,  #connection{token = Token}) ->
    [{<<"x-snarl-token">>, Token} | O].

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
