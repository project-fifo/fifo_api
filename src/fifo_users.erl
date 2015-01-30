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
         add_sshkey/3,
         delete_sshkey/3
        ]).

-xref_ignore([
              list/3,
              get/2,
              auth/3,
              add_sshkey/3,
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
-spec list(Full :: boolean(), Fields :: [binary()], fifo_api:connection()) ->
                  {ok, JSON :: binary()}.
list(_Fill, _Fields, _Con) ->
    {ok, <<>>}.

%%--------------------------------------------------------------------
%% @doc
%% Fetches the config data for a single Dataset.
%% @end
%%--------------------------------------------------------------------
-spec get(UUID :: binary(), fifo_api:connection()) ->
                 {ok, JSON :: binary()}.

get(_UUID, _Con) ->
    {ok, <<>>}.

%%--------------------------------------------------------------------
%% @doc
%% Authenticates a user.
%% @end
%%--------------------------------------------------------------------

-spec auth(Login :: binary(), Pass :: binary(), fifo_api:connection()) ->
                  {ok, Token :: binary()}.

auth(_User, _Pass, _Con) ->
    {ok, <<>>}.

%%--------------------------------------------------------------------
%% @doc
%% Adds a SSH key to a user.
%% @end
%%--------------------------------------------------------------------

-spec add_sshkey(UUID :: binary(), Key :: binary(),
                 fifo_api:connection()) ->
    ok.

add_sshkey(_UUID, _Key, _Con) ->
    {ok, <<>>}.

%%--------------------------------------------------------------------
%% @doc
%% Delete a SSH key from a user.
%% @end
%%--------------------------------------------------------------------

-spec delete_sshkey(UUID :: binary(), KeyID :: binary(),
                    fifo_api:connection()) ->
    ok.

delete_sshkey(_UUID, _KeyID, _Con) ->
    {ok, <<>>}.
