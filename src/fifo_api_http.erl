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
        delete/2, delete/3, connect/1, close/1]).

-export([take_last/1, full_list/1]).

-export_type([connection/0, connection_options/0]).

-record(connection, {
          endpoint = "localhost" :: string(),
          port = 80 :: pos_integer(),
          prefix = "api" :: string(),
          version = "3" :: string(),
          token :: binary() | undefined
         }).

-type connection_options() ::
        {endpoint, string()} |
        {port, pos_integer()} |
        {prefix, string()} |
        {token, binary()} |
        {version, string()}.

-type connection() :: #connection{}.



-ifdef(USE_MSGPACK).
-define(ENCODING,  <<"application/x-msgpack">>).
encode(E) ->
    msgpack:pack(E, [{map_format, jsx}]).
decode(E) ->
    {ok, R} = msgpack:unpack(E, [{map_format, jsx}]),
    R.
-else.
-define(ENCODING,  <<"application/json">>).
encode(E) ->
    jsx:encode(E).
decode(E) ->
    jsx:decode(E).
-endif.




new(Options) ->
    new(Options, #connection{}).

connect(#connection{endpoint = Endpoint, port = Port}) ->
    {ok, ConnPid} = gun:open(Endpoint, Port),
    {ok, _} = gun:await_up(ConnPid),
    ConnPid.

close(ConnPid) ->
    gun:close(ConnPid).

set_token(Token, C) ->
    C#connection{token = Token}.

get(Path, C) ->
    get(Path, [], C).

delete(Path, C) ->
    delete(Path, [], C).

get(Path, Opts, C) ->
    get_(url(Path, C), Opts, C).

get_(URL, C) ->
    get_(URL, [], C).

get_(URL, Opts, C) ->
    ConnPid = connect(C),
    ReqHeaders = token_opts([{<<"accept">>, ?ENCODING} | Opts], C),
    StreamRef = gun:get(ConnPid, URL, ReqHeaders),
    case gun:await(ConnPid, StreamRef) of
        {response, fin, Code, _Hdrs} when Code >= 400 ->
            gun:close(ConnPid),
            {error, Code};
        {response, nofin, _Status, _Hdrs} ->
            case  gun:await_body(ConnPid, StreamRef) of
                {ok, Body1} ->
                    Body2 = decode(Body1),
                    gun:close(ConnPid),
                    {ok, Body2};
                E1 ->
                    gun:close(ConnPid),
                    E1
            end;
        E ->
            gun:close(ConnPid),
            E
    end.

delete(Path, Opts, C) ->
    ConnPid = connect(C),
    URL = url(Path, C),
    ReqHeaders = token_opts([{<<"accept">>, ?ENCODING} | Opts], C),
    StreamRef = gun:delete(ConnPid, URL, ReqHeaders),
    case gun:await(ConnPid, StreamRef) of
        {response, fin, Code, _Hdrs} when Code >= 400 ->
            gun:close(ConnPid),
            {error, Code};
        {response, fin, _Status, _Hdrs} ->
            gun:close(ConnPid),
            ok;
        {response, nofin, _Status, _Hdrs} ->
            case gun:await_body(ConnPid, StreamRef) of
                {ok, Body1} ->
                    Body2 = decode(Body1),
                    gun:close(ConnPid),
                    {ok, Body2};
                E1 ->
                    gun:close(ConnPid),
                    E1
            end;
        E ->
            gun:close(ConnPid),
            E
    end.

post(Path, Body, C) ->
    ConnPid = connect(C),
    URL = url(Path, C),
    ReqHeaders = token_opts([{<<"accept">>, ?ENCODING},
                             {<<"content-type">>, ?ENCODING}], C),
    ReqBody = encode(Body),
    StreamRef = gun:post(ConnPid, URL, ReqHeaders),
    gun:data(ConnPid, StreamRef, fin, ReqBody),
    case gun:await(ConnPid, StreamRef) of
        {response, fin, Code, _Hdrs} when Code >= 400 ->
            gun:close(ConnPid),
            {error, Code};
        {response, fin, 200, _Hdrs}  ->
            gun:close(ConnPid),
            ok;
        {response, _, 204, _Hdrs}  ->
            gun:close(ConnPid),
            ok;
        {response, nofin, 200, _Hdrs} ->
            case gun:await_body(ConnPid, StreamRef) of
                {ok, Body1} ->
                    Body2 = decode(Body1),
                    gun:close(ConnPid),
                    {ok, Body2};
                E1 ->
                    gun:close(ConnPid),
                    E1
            end;
        {response, fin, 303, H} ->
            Location = proplists:get_value(<<"location">>, H),
            gun:close(ConnPid),
            get_(Location, C);
        Error ->
            gun:close(ConnPid),
            Error
    end.

put(Path, Body, C) ->
    ConnPid = connect(C),
    URL = url(Path, C),
    ReqHeaders = token_opts([{<<"accept">>, ?ENCODING},
                             {<<"content-type">>, ?ENCODING}], C),
    ReqBody = encode(Body),
    StreamRef = gun:put(ConnPid, URL, ReqHeaders),
    gun:data(ConnPid, StreamRef, fin, ReqBody),
    case gun:await(ConnPid, StreamRef) of
        {response, fin, Code, _Hdrs} when Code >= 400 ->
            gun:close(ConnPid),
            {error, Code};
        {response, fin, 200, _Hdrs}  ->
            gun:close(ConnPid),
            ok;
        {response, _, 204, _Hdrs}  ->
            gun:close(ConnPid),
            ok;
        {response, nofin, 200, _Hdrs} ->
            case gun:await_body(ConnPid, StreamRef) of
                {ok, Body1} ->
                    Body2 = decode(Body1),
                    gun:close(ConnPid),
                    {ok, Body2};
                E1 ->
                    gun:close(ConnPid),
                    E1
            end;
        {response, fin, 303, H} ->
            Location = proplists:get_value(<<"location">>, H),
            gun:close(ConnPid),
            get_(Location, C);
        Error ->
            gun:close(ConnPid),
            Error
    end.

url([$/ | Path], C) ->
    url(Path, C);

url(Path,
    #connection{prefix = Prefix, version = Version}) ->
    [$/, Prefix, $/, Version, $/, Path].

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
