%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2015, Heinz Nikolaus Gies
%%% @doc
%%% This module offers a interface to the FIFO API in the /packages
%%% endpoint
%%% @end
%%% Created : 29 Jan 2015 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(fifo_packages).

-export([
         list/3,
         get/2
        ]).

-xref_ignore([
         list/3,
         get/2
        ]).

%%--------------------------------------------------------------------
%% @doc
%% Lists all the Packages for the current user, when Full is false only
%% the UUID of the Packages are returned, otehrwise the full data is
%% returned.
%%
%% When returning the full data Fields can be used to filter the
%% parts of the Package config that are being returned.
%% @end
%%--------------------------------------------------------------------
-spec list(Full :: boolean(), Fields :: [binary()], fifo_api:connection()) ->
                  {ok, JSON :: binary()}.
list(_Fill, _Fields, _Con) ->
    {ok, <<>>}.

%%--------------------------------------------------------------------
%% @doc
%% Fetches the config data for a single Package.
%% @end
%%--------------------------------------------------------------------
-spec get(UUID :: binary(), fifo_api:connection()) ->
                  {ok, JSON :: binary()}.

get(_UUID, _Con) ->
    {ok, <<>>}.
