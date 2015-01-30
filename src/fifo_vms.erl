%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2015, Heinz Nikolaus Gies
%%% @doc
%%% This module offers a interface to the FIFO API in the /vms
%%% endpoint
%%% @end
%%% Created : 29 Jan 2015 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(fifo_vms).

-export([
         list/3,
         get/2,
         create/4,
         delete/2,
         start/2,
         stop/3,
         reboot/3,
         metadata_set/4,
         snapshot_create/3,
         snapshot_delete/3,
         snapshot_rollback/3,
         backup_create/3,
         backup_create/4,
         backup_delete/3,
         backup_rollback/3,
         change/4
        ]).

-xref_ignore([
         list/3,
         get/2,
         create/4,
         delete/2,
         start/2,
         stop/3,
         reboot/3,
         metadata_set/4,
         snapshot_create/3,
         snapshot_delete/3,
         snapshot_rollback/3,
         backup_create/3,
         backup_create/4,
         backup_delete/3,
         backup_rollback/3,
         change/4
        ]).

-define(ENDPOINT, "/vms").

%%--------------------------------------------------------------------
%% @doc
%% Lists all the VM's for the current user, when Full is false only
%% the UUID of the VM's are returned, otehrwise the full data is
%% Returnedn.
%%
%% When returning the full data Fields can be used to filter the
%% parts of the VM config that are being returned.
%% @end
%%--------------------------------------------------------------------
-spec list(Full :: boolean(), Fields :: [binary()], fifo_api:connection()) ->
                  {ok, JSON :: binary()}.
list(false, _, C) ->
    fifo_api_http:get(?ENDPOINT, C);

list(true, Fields, C) ->
    Opts = [{<<"x-full-list">>, <<"true">>},
            {<<"x-full-list-fields">>, fifo_api_http:full_list(Fields)}],
    fifo_api_http:get(?ENDPOINT, Opts, C).

%%--------------------------------------------------------------------
%% @doc
%% Fetches the config data for a single VM.
%% @end
%%--------------------------------------------------------------------
-spec get(UUID :: binary(), fifo_api:connection()) ->
                  {ok, JSON :: binary()}.

get(UUID, C) ->
    fifo_api_http:get([?ENDPOINT, "/", UUID], C).

%%--------------------------------------------------------------------
%% @doc
%% Creates a new VM. Dataset and Package are the UUID's the Config
%% is a additional JSON to configure things like hostname etc...
%% @end
%%--------------------------------------------------------------------
-spec create(Dataset :: binary(), Package :: binary(),
             ConfigJSON :: jsx:json_term(),
          fifo_api:connection()) ->
                 {ok, UUID :: binary()}.
create(Dataset, Package, Config, C) ->
    Body = [{<<"package">>, Package},
            {<<"dataset">>, Dataset},
            {<<"config">>, Config}],
    fifo_api_http:post(?ENDPOINT, Body, C).

%%--------------------------------------------------------------------
%% @doc
%% Deletes a virtual machine.
%% @end
%%--------------------------------------------------------------------
-spec delete(UUID :: binary(), fifo_api:connection()) ->
                    ok.
delete(UUID, C) ->
    URL = [?ENDPOINT, $/, UUID],
    fifo_api_http:delete(URL, C).

%%--------------------------------------------------------------------
%% @doc
%% Starts a virtual machine.
%% @end
%%--------------------------------------------------------------------
-spec start(UUID :: binary(), fifo_api:connection()) ->
                   ok.
start(UUID, C) ->
    URL = [?ENDPOINT, $/, UUID],
    Body = [{<<"action">>, <<"start">>}],
    fifo_api_http:put(URL, Body, C).

%%--------------------------------------------------------------------
%% @doc
%% Stops a VM, if Force is true it will force stop the vm.
%% @end
%%--------------------------------------------------------------------
-spec stop(UUID :: binary(), Force :: boolean(), fifo_api:connection()) ->
                   ok.
stop(UUID, false, C) ->
    URL = [?ENDPOINT, $/, UUID],
    Body = [{<<"action">>, <<"stop">>}],
    fifo_api_http:put(URL, Body, C);

stop(UUID, true, C) ->
    URL = [?ENDPOINT, $/, UUID],
    Body = [{<<"action">>, <<"stop">>}, {<<"force">>, true}],
    fifo_api_http:put(URL, Body, C).

%%--------------------------------------------------------------------
%% @doc
%% Reboots a VM, if Force is true it will force reboot the vm.
%% @end
%%--------------------------------------------------------------------
-spec reboot(UUID :: binary(), Force :: boolean(), fifo_api:connection()) ->
                   ok.
reboot(UUID, false, C) ->
    URL = [?ENDPOINT, $/, UUID],
    Body = [{<<"action">>, <<"reboot">>}],
    fifo_api_http:put(URL, Body, C);

reboot(UUID, true, C) ->
    URL = [?ENDPOINT, $/, UUID],
    Body = [{<<"action">>, <<"reboot">>}, {<<"force">>, true}],
    fifo_api_http:put(URL, Body, C).

%%--------------------------------------------------------------------
%% @doc
%% Sets the metadata on a VM, the path is given in a nested json
%% datastructure. The value is what it should be set to.
%% @end
%%--------------------------------------------------------------------
-spec metadata_set(UUID :: binary(), Path :: [binary()],
                   Value :: binary() | number(), fifo_api:connection()) ->
                          ok.
metadata_set(UUID, Path, Value, C) ->
    {Prefix, K} = fifo_api_http:take_last(Path),
    Body = [{K, Value}],
    URLElements = [?ENDPOINT, binary_to_list(UUID), "metadata" | Prefix],
    URL = strings:join(URLElements, "/"),
    fifo_api_http:put(URL, Body, C).

%%--------------------------------------------------------------------
%% @doc
%% Creates a snapshot fo a VM with a given comment.
%% @end
%%--------------------------------------------------------------------
-spec snapshot_create(UUID :: binary(), Comment :: binary(),
                      fifo_api:connection()) ->
                             ok.
snapshot_create(UUID, Comment, C) ->
    URL = [?ENDPOINT, $/, UUID, "/snapshots"],
    Body = [{<<"comment">>, Comment}],
    fifo_api_http:post(URL, Body, C).

%%--------------------------------------------------------------------
%% @doc
%% Deletes a snapshot for a VM.
%% @end
%%--------------------------------------------------------------------
-spec snapshot_delete(UUID :: binary(), SnapshotUUID :: binary(),
                      fifo_api:connection()) ->
                             ok.
snapshot_delete(UUID, SnapshotUUID, C) ->
    URL = [?ENDPOINT, $/, UUID, "/snapshots/", SnapshotUUID],
    fifo_api_http:delete(URL, C).

%%--------------------------------------------------------------------
%% @doc
%% Restores a snapshot for a VM.
%% @end
%%--------------------------------------------------------------------
-spec snapshot_rollback(UUID :: binary(), SnapshotUUID :: binary(),
                      fifo_api:connection()) ->
                             ok.
snapshot_rollback(UUID, SnapshotUUID, C) ->
    URL = [?ENDPOINT, $/, UUID, "/snapshots/", SnapshotUUID],
    Body = [{<<"action">>, <<"start">>}],
    fifo_api_http:put(URL, Body, C).

%%--------------------------------------------------------------------
%% @doc
%% Creates a new full backup with a comment.
%% @end
%%--------------------------------------------------------------------
-spec backup_create(UUID :: binary(), Comment :: binary(),
                      fifo_api:connection()) ->
                             ok.
backup_create(UUID, Comment, C) ->
    URL = [?ENDPOINT, $/, UUID, "/backups"],
    Body = [{<<"comment">>, Comment}],
    fifo_api_http:post(URL, Body, C).

%%--------------------------------------------------------------------
%% @doc
%% Creates a new incremetal backup with a comment. The parent is the
%% UUID if the parent for the incremental.
%% @end
%%--------------------------------------------------------------------
-spec backup_create(UUID :: binary(), Parent :: binary(), Comment :: binary(),
                      fifo_api:connection()) ->
                             ok.
backup_create(UUID, Parent, Comment, C) ->
    URL = [?ENDPOINT, $/, UUID, "/backups"],
    Body = [{<<"comment">>, Comment}, {<<"parent">>, Parent}],
    fifo_api_http:post(URL, Body, C).

%%--------------------------------------------------------------------
%% @doc
%% Deletes a snapshot for a VM.
%% @end
%%--------------------------------------------------------------------
-spec backup_delete(UUID :: binary(), BackupUUID :: binary(),
                      fifo_api:connection()) ->
                             ok.
backup_delete(UUID, BackupUUID, C) ->
    URL = [?ENDPOINT, $/, UUID, "/backups/", BackupUUID],
    fifo_api_http:delete(URL, C).

%%--------------------------------------------------------------------
%% @doc
%% Restores a snapshot for a VM.
%% @end
%%--------------------------------------------------------------------
-spec backup_rollback(UUID :: binary(), BackupUUID :: binary(),
                      fifo_api:connection()) ->
                             ok.
backup_rollback(UUID, BackupUUID, C) ->
    URL = [?ENDPOINT, $/, UUID, "/backups/", BackupUUID],
    Body = [{<<"action">>, <<"start">>}],
    fifo_api_http:put(URL, Body, C).

%%--------------------------------------------------------------------
%% @doc
%% Updates a VM, resizing it Package is the UUID if a new package,
%% the config can contain further values to change.
%% @end
%%--------------------------------------------------------------------
-spec change(UUID :: binary(), NewPackage :: binary(),
             NewConfig :: jsx:json_term(), fifo_api:connection()) ->
                    ok.
change(UUID, undefined, NewConfig, C) ->
    URL = [?ENDPOINT, $/, UUID],
    Body = [{<<"config">>, NewConfig}],
    fifo_api_http:put(URL, Body, C);

change(UUID, NewPackage, undefined, C) ->
    URL = [?ENDPOINT, $/, UUID],
    Body = [{<<"package">>, NewPackage},
            {<<"config">>, []}],
    fifo_api_http:put(URL, Body, C);

change(UUID, NewPackage, NewConfig, C) ->
    URL = [?ENDPOINT, $/, UUID],
    Body = [{<<"package">>, NewPackage},
            {<<"config">>, NewConfig}],
    fifo_api_http:put(URL, Body, C).
