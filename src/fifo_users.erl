%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2015, Heinz Nikolaus Gies
%%% @doc
%%% This module offers a interface to the FIFO API in the /datasets
%%% endpoint
%%% @end
%%% Created : 29 Jan 2015 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(fifo_users).

-export([
         list/3,
         get/2,
         auth/3,
         add_sshkey/4,
         delete_sshkey/3
        ]).

-xref_ignore([
              list/3,
              get/2,
              auth/3,
              add_sshkey/4,
              delete_sshkey/3
             ]).

%%--------------------------------------------------------------------
%% @doc
%% Lists all the Datasets for the current user, when Full is false only
%% the UUID of the Datasets are returned, otehrwise the full data is
%% returned.
%%
%% When returning the full data Fields can be used to filter the
%% parts of the Dataset config that are being returned.
%% @end
%%--------------------------------------------------------------------
-spec list(Full :: boolean(), Fields :: [binary()],
           fifo_api_http:connection()) ->
                  {ok, JSON :: binary()}.

list(false, _, C) ->
    case fifo_api_http:get("/users", C) of
        {ok, _H, B} ->
            {ok, B};
        E ->
            E
    end;

list(true, Fields, C) ->
    Opts = [{<<"x-full-list">>, <<"true">>},
            {<<"x-full-list-fields">>, fifo_api_http:full_list(Fields)}],
    case fifo_api_http:get("/users", Opts, C) of
        {ok, _H, B} ->
            {ok, B};
        E ->
            E
    end.

%%--------------------------------------------------------------------
%% @doc
%% Fetches the config data for a single Dataset.
%% @end
%%--------------------------------------------------------------------
-spec get(UUID :: binary(), fifo_api_http:connection()) ->
                 {ok, JSON :: binary()}.

get(UUID, C) ->
    case fifo_api_http:get("/users/" ++ binary_to_list(UUID), C) of
        {ok, _H, B} ->
            {ok, B};
        E ->
            E
    end.

%%--------------------------------------------------------------------
%% @doc
%% Authenticates a user.
%% @end
%%--------------------------------------------------------------------

-spec auth(Login :: binary(), Pass :: binary(), fifo_api_http:connection()) ->
                  {ok, Token :: binary()}.

auth(Login, Pass, C) ->
    Body = [{<<"user">>, Login}, {<<"password">>, Pass}],
    Method = post,
    URL = fifo_api_http:url("/sessions", C),
    ReqHeaders = [{<<"accept-encoding">>, <<"application/x-msgpack">>},
                  {<<"content-type">>, <<"application/x-msgpack">>}],
    ReqBody = msgpack:pack(Body, [jsx]),
    case hackney:request(Method, URL, ReqHeaders, ReqBody, []) of
        {ok, 303, H, _Ref} ->
            Token = proplists:get_value(<<"x-snarl-token">>, H),
            {ok, Token};
         {ok, Error, _, _} ->
            {error, Error}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Adds a SSH key to a user.
%% @end
%%--------------------------------------------------------------------

-spec add_sshkey(UUID :: binary(), KeyID :: binary(), Key :: binary(),
                 fifo_api_http:connection()) ->
    ok.

add_sshkey(UUID, KeyID, Key, C) ->
    URL = "/users/" ++ binary_to_list(UUID) ++ "/keys",
    Body = [{KeyID, Key}],
    fifo_api_http:put(URL, Body, C).


%%--------------------------------------------------------------------
%% @doc
%% Delete a SSH key from a user.
%% @end
%%--------------------------------------------------------------------

-spec delete_sshkey(UUID :: binary(), KeyID :: binary(),
                    fifo_api_http:connection()) ->
    ok.

delete_sshkey(UUID, KeyID, C) ->
    URL = "/users/" ++ binary_to_list(UUID) ++ "/keys/"
        ++ binary_to_list(KeyID),
    fifo_api_http:delete(URL, C).
