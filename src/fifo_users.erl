%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2015, Heinz Nikolaus Gies
%%% @doc
%%% This module offers a interface to the FIFO API in the /users
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

-define(ENDPOINT, "/users").

%%--------------------------------------------------------------------
%% @doc
%% Lists all the Datasets for the current user, when Full is false only
%% the UUID of the Datasets are returned, otherwise the full data is
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
    fifo_api_http:get(?ENDPOINT, C);

list(true, Fields, C) ->
    Opts = [{<<"x-full-list">>, <<"true">>},
            {<<"x-full-list-fields">>, fifo_api_http:full_list(Fields)}],
    fifo_api_http:get(?ENDPOINT, Opts, C).

%%--------------------------------------------------------------------
%% @doc
%% Fetches the config data for a single Dataset.
%% @end
%%--------------------------------------------------------------------
-spec get(UUID :: binary(), fifo_api_http:connection()) ->
                 {ok, JSON :: binary()}.

get(UUID, C) ->
    fifo_api_http:get([?ENDPOINT, $/, UUID], C).

%%--------------------------------------------------------------------
%% @doc
%% Authenticates a user.
%% @end
%%--------------------------------------------------------------------

-spec auth(Login :: binary(), Pass :: binary(), fifo_api_http:connection()) ->
                  {ok, Token :: binary()} |
                  {error, term()}.

auth(Login, Pass, C) ->
    L1 = list_to_binary(http_uri:encode(binary_to_list(Login))),
    P1 = list_to_binary(http_uri:encode(binary_to_list(Pass))),
    ReqBody = <<"username=", L1/binary,"&password=", P1/binary, "&grant_type=password">>,
    URL = fifo_api_http:url("oauth/token", C),
    ReqHeaders = [{<<"accept-encoding">>, <<"application/json">>},
                  {<<"content-type">>, <<"application/x-www-form-urlencoded">>}],
    ConnPid = fifo_api_http:connect(C),
    StreamRef = gun:post(ConnPid, URL, ReqHeaders),
    gun:data(ConnPid, StreamRef, fin, ReqBody),
    case gun:await(ConnPid, StreamRef) of
        {response, fin, 200, _Hdrs}  ->
            fifo_api_http:close(ConnPid),
            {error, no_login};
        {response, nofin, 200, _Hdrs} ->
            {ok, Body1} = gun:await_body(ConnPid, StreamRef),
            fifo_api_http:close(ConnPid),
            jsxd:get(<<"access_token">>, jsx:decode(Body1));
        Error ->
            fifo_api_http:close(ConnPid),
            Error
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
    URL = [?ENDPOINT, $/, UUID, "/keys"],
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
    URL = [?ENDPOINT, $/, UUID, "/keys/", KeyID],
    fifo_api_http:delete(URL, C).
