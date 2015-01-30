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
         restart/3,
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
         restart/3,
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
list(_Fill, _Fields, _Con) ->
    {ok, <<>>}.

%%--------------------------------------------------------------------
%% @doc
%% Fetches the config data for a single VM.
%% @end
%%--------------------------------------------------------------------
-spec get(UUID :: binary(), fifo_api:connection()) ->
                  {ok, JSON :: binary()}.

get(_UUID, _Con) ->
    {ok, <<>>}.

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
create(_Dataset, _Package, _Config, _Con) ->
    {ok, <<>>}.

%%--------------------------------------------------------------------
%% @doc
%% Deletes a virtual machine.
%% @end
%%--------------------------------------------------------------------
-spec delete(UUID :: binary(), fifo_api:connection()) ->
                    ok.
delete(_UUID, _Con) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Starts a virtual machine.
%% @end
%%--------------------------------------------------------------------
-spec start(UUID :: binary(), fifo_api:connection()) ->
                   ok.
start(_UUID, _Con) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Stops a VM, if Force is true it will force stop the vm.
%% @end
%%--------------------------------------------------------------------
-spec stop(UUID :: binary(), Force :: boolean(), fifo_api:connection()) ->
                   ok.
stop(_UUID, _Force, _Con) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Restarts a VM, if Force is true it will force restart the vm.
%% @end
%%--------------------------------------------------------------------
-spec restart(UUID :: binary(), Force :: boolean(), fifo_api:connection()) ->
                   ok.
restart(_UUID, _Force, _Con) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Sets the metadata on a VM, the path is given in a nested json
%% datastructure. The value is what it should be set to.
%% @end
%%--------------------------------------------------------------------
-spec metadata_set(UUID :: binary(), Path :: [binary()],
                   Value :: binary() | number(), fifo_api:connection()) ->
                          ok.
metadata_set(_UUID, _Path, _Value, _Con) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Creates a snapshot fo a VM with a given comment.
%% @end
%%--------------------------------------------------------------------
-spec snapshot_create(UUID :: binary(), Comment :: binary(),
                      fifo_api:connection()) ->
                             ok.
snapshot_create(_UUID, _Comment, _Con) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Deletes a snapshot for a VM.
%% @end
%%--------------------------------------------------------------------
-spec snapshot_delete(UUID :: binary(), SnapshotUUID :: binary(),
                      fifo_api:connection()) ->
                             ok.
snapshot_delete(_UUID, _SnapshotUUID, _Con) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Restores a snapshot for a VM.
%% @end
%%--------------------------------------------------------------------
-spec snapshot_rollback(UUID :: binary(), SnapshotUUID :: binary(),
                      fifo_api:connection()) ->
                             ok.
snapshot_rollback(_UUID, _SnapshotUUID, _Con) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Creates a new full backup with a comment.
%% @end
%%--------------------------------------------------------------------
-spec backup_create(UUID :: binary(), Comment :: binary(),
                      fifo_api:connection()) ->
                             ok.
backup_create(_UUID, _Comment, _Con) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Creates a new incremetal backup with a comment. The parent is the
%% UUID if the parent for the incremental.
%% @end
%%--------------------------------------------------------------------
-spec backup_create(UUID :: binary(), Parent :: binary(), Comment :: binary(),
                      fifo_api:connection()) ->
                             ok.
backup_create(_UUID, _Parent, _Comment, _Con) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Deletes a snapshot for a VM.
%% @end
%%--------------------------------------------------------------------
-spec backup_delete(UUID :: binary(), BackupUUID :: binary(),
                      fifo_api:connection()) ->
                             ok.
backup_delete(_UUID, _BackupUUID, _Con) ->
	ok.

%%--------------------------------------------------------------------
%% @doc
%% Restores a snapshot for a VM.
%% @end
%%--------------------------------------------------------------------
-spec backup_rollback(UUID :: binary(), BackupUUID :: binary(),
                      fifo_api:connection()) ->
                             ok.
backup_rollback(_UUID, _BackupUUID, _Con) ->
	ok.

%%--------------------------------------------------------------------
%% @doc
%% Updates a VM, resizing it Package is the UUID if a new package,
%% the config can contain further values to change.
%% @end
%%--------------------------------------------------------------------
-spec change(UUID :: binary(), NewPackage :: binary(),
             NewConfig :: jsx:json_term(), fifo_api:connection()) ->
                    ok.
change(_UUID, _NewPackage, _NewConfig, _Con) ->
    ok.
