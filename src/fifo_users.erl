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

-define(ENDPOINT, "/users").

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
                  {ok, Token :: binary()}.

auth(Login, Pass, C) ->
    Body = [{<<"username">>, Login}, {<<"password">>, Pass},
            {<<"grant_type">>, <<"password">>}],
    Method = post,
    URL = fifo_api_http:url("oauth/token", C),
    ReqHeaders = [{<<"accept-encoding">>, <<"application/json">>},
                  {<<"content-type">>, <<"application/x-www-form-urlencoded">>}],
    case hackney:request(Method, URL, ReqHeaders, {form, Body}, [{timeout, 5000}]) of
        {ok, 200, _H, Ref} ->
            {ok, Body1} = hackney:body(Ref),
            Body2 = jsx:decode(Body1),
            jsxd:get(<<"access_token">>, Body2);
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
