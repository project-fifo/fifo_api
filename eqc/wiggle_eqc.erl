%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2015, Heinz Nikolaus Gies
%%% @doc
%%% Bugs found:
%%%  x "/" instead of $/ in fifo_api_http:url
%%%  x extra / in urls'f in fifo_vm calls
%%%  x blockage of hackney when not reading the body of replies
%%%  x deleting a vm needs to delete the create fms too.
%%%  x Timeout issues due to long bcrypt password checks.
%%%  x Encoding error trying to encode a tuple with atom_to_binary in oauth
%%%    error responses
%%%  x A hardware/os level defect on the test system
%%%  x Read failure when a local node returned not_found before apropper answer
%%%    was generated.
%%%  x bug that deleted new tokesn because of wrong expiery check.
%%%  x problems with OAuth scopes
%%%  x Double IP assignments (somehow o.O, by failing deletes)
%%%  x Bad return value of the free function when freeing an IP that was not
%%%    used.
%%%  x returning 403 on freshy created VM's from the call returning before
%%%    permissions could be granted.
%%%  x A issue with os:cmd when having a too high ulimit (googled)
%%%    https://github.com/elixir-lang/elixir/issues/2571
%%%  x a bug that sone zone FSM's on chunter were not shut down when the zone
%%%    was not found.
%%%  x Vm's in the DB being stuck in deleting, when stage changes happen between
%%%    deleting stage and data being deleted.
%%%  x The create FSM timing out when the creation of an existing VM takes
%%%    longer then retries * delay for retries (this isn't a bug but it needs
%%%    to be set high enough for the test to hold true.
%%%  x Issue with closing a port that did not provide a pid was crashing the
%%     FSM.
%%%  x An unhandled error on connection failures.
%%%  x Timeout on lock server
%%%  x unregister needs to happen as fist event in the vm destruction to prevent
%%%    forbidden errors
%%%  x When requiering decoding during the forbidden phase content was always
%%%    decoded as JOSN.
%%%  x A bug introduced by caching of token permissions:
%%%      -> A programmer has a problem and uses a cache to solve it, now they
%%%         have two problems.
%%%  x Problem with systems being stuck in shutting down.
%%%      -> Timing issue with chunter not re-reading the state before
%%%         updating it leading to possible race codintions with multiple
%%%         endpoints.
%%%  x newly introduced but matching for a wrong value when a vm was
%%%    supposed to be deletd while the creation phase was still ongoing.
%%% @end
%%% Created : 22 Apr 2015 by Heinz Nikolaus Gies <heinz@licenser.net>

-module(wiggle_eqc).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_component.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

-record(login, {
          login,
          password
         }).

-define(ADMIN, #login{login = <<"admin">>, password = <<"admin">>}).
-define(USER1, #login{login = <<"test1">>, password = <<"test1">>}).
-define(USER2, #login{login = <<"test2">>, password = <<"test2">>}).
-define(USERS, #{
          user1 => ?USER1,
          user2 => ?USER2
         }).
-define(DATASET, [<<"b67492c2-055c-11e5-85d8-8b039ac981ec">>]).
-define(PACKAGE, [<<"42bf38f3-5632-49cb-82ce-973a04c3a8f6">>]).
-define(NETWORK, <<"faa25949-e8a2-446f-b01a-ddf17f058016">>).
-define(ENDPOINT, "192.168.1.41").
-define(ENDPOINTS, ["192.168.1.41", "192.168.1.42"]).
-define(CREATION_CONCURRENCY, 2).

-define(CREATE_TIMEOUT, 320).
-define(START_TIMEOUT, 60).
-define(STOP_TIMEOUT, 60).
-define(DELETE_TIMEOUT, 60).
-define(MAX_VMS, 5).
-define(R(C), fifo_api_http:reset(C)).

-record(user, {
          id,
          connection,
          vms = []
         }).

-record(state,{
          %% All created VMs in this run
          vms = [],
          %% The VM's that are not confirmed done creating
          creating = [],

          starting = [],
          running = [],

          stopping = [],
          stopped = [],
          deleting = [],
          deleted = [],
          login_owners = #{},
          %% The map of users
          users,
          %% connection for the amdin user.
          admin = {var, admin}
         }).

%% @doc Returns the state in which each test case starts. (Unless a different
%%      initial state is supplied explicitly to, e.g. commands/2.)
-spec initial_state() -> eqc_statem:symbolic_state().
initial_state() ->
    #state{
       users = maps:from_list([{ID, #user{id = ID}} ||
                                  ID <- maps:keys(?USERS)])}.

command_precondition_common(_S, _Command) ->
    true.

precondition_common(_S, _Call) ->
    true.

%% =============================================================================
%% Connection Opperations
%% =============================================================================

%% -----------------------------------------------------------------------------
%% Grouped operator: connect
%% -----------------------------------------------------------------------------

%% TODO: The handling of changing connections and associating them with users
%% is close to impossible, so we 
%%connect_pre(_) ->
%% false.

connect_args(#state{users = Users}) ->
    [elements(maps:keys(Users)), elements(?ENDPOINTS)].

connect(UserID, Endpoint) ->
    #login{login = Login, password = Pass} =
        maps:get(UserID, ?USERS),
    C = fifo_api:new([{endpoint, Endpoint}]),
    {ok, C1} = fifo_api:auth(Login, Pass, C),
    C1.

connect_next(S = #state{users = Users, login_owners = Owners},
             C, [UserID, _]) ->
    User1 = maps:get(UserID, Users),
    User2 = User1#user{connection = C},
    Users1 = maps:update(UserID, User2, Users),
    Owners1 = maps:put(C, UserID, Owners),
    S1 = S#state{users = Users1, login_owners = Owners1},
    S1.

connect_post(_S, [_UserID, _], Res) ->
    is_tuple(Res) andalso element(1, Res) =:= connection.

%% =============================================================================
%% Wait Opperations
%% =============================================================================

%% -----------------------------------------------------------------------------
%% Grouped operator: wait_for_creation
%% -----------------------------------------------------------------------------

wait_for_creation_pre(S = #state{creating = Creating}) ->
    has_admin(S) andalso Creating /= [].

wait_for_creation_args(#state{admin = Admin, creating = Creating}) ->
    [Admin, elements(Creating)].

wait_for_creation(C, {UUID, T0}) ->
    D = erlang:system_time(seconds) -  T0,
    D1 = max(1, ?CREATE_TIMEOUT - D),
    pool_state(C, UUID, running, D1*2).

wait_for_creation_post(_S, [_C, _Elem], Res) ->
    Res == ok.

wait_for_creation_next(S = #state{creating = Creating, running = Running},
                       _Value, [_C, Elem]) ->
    S#state{creating = [C || C <- Creating, C /= Elem],
            running = [Elem | Running]}.

%% -----------------------------------------------------------------------------
%% Grouped operator: wait_for_delete
%% -----------------------------------------------------------------------------

wait_for_delete_pre(S = #state{deleting = Deleting}) ->
    has_admin(S) andalso Deleting /= [].

wait_for_delete_args(#state{admin = Admin, deleting = Deleting}) ->
    [Admin, elements(Deleting)].

wait_for_delete(C, {UUID, T0}) ->
    D = erlang:system_time(seconds) - T0,
    D1 = max(1, ?DELETE_TIMEOUT - D),
    pool_state(C, UUID, deleted, D1*2).

wait_for_delete_post(_S, [_C, _Elem], Res) ->
    Res == ok.

wait_for_delete_next(S = #state{deleting = Deleting, deleted = Deleted},
                     _Value, [_C, Elem]) ->
    S#state{deleting = [C || C <- Deleting, C /= Elem],
            deleted = [Elem | Deleted]}.

%% -----------------------------------------------------------------------------
%% Grouped operator: wait_for_start
%% -----------------------------------------------------------------------------

wait_for_start_pre(S = #state{starting = Starting}) ->
    has_admin(S) andalso Starting /= [].

wait_for_start_args(#state{admin = Admin, starting = Starting}) ->
    [Admin, elements(Starting)].

wait_for_start(C, {UUID, T0}) ->
    D = erlang:system_time(seconds) - T0,
    D1 = max(1, ?START_TIMEOUT - D),
    pool_state(C, UUID, running, D1*2).

wait_for_start_post(_S, [_C, _Elem], Res) ->
    Res == ok.

wait_for_start_next(S = #state{starting = Starting, running = Running},
                    _Value, [_C, Elem]) ->
    S#state{starting = [C || C <- Starting, C /= Elem],
            running = [Elem | Running]}.

%% -----------------------------------------------------------------------------
%% Grouped operator: wait_for_stop
%% -----------------------------------------------------------------------------

wait_for_stop_pre(S = #state{stopping = Stopping}) ->
    has_admin(S) andalso Stopping /= [].

wait_for_stop_args(#state{admin = Admin, stopping = Stopping}) ->
    [Admin, elements(Stopping)].

wait_for_stop(C, {UUID, T0}) ->
    D = erlang:system_time(seconds) - T0,
    D1 = max(1, ?STOP_TIMEOUT - D),
    pool_state(C, UUID, stopped, D1*2).

wait_for_stop_post(_S, [_C, _Elem], Res) ->
    Res == ok.

wait_for_stop_next(S = #state{stopping = Stopping, stopped = Stopped},
                   _Value, [_C, Elem]) ->
    S#state{stopping = [C || C <- Stopping, C /= Elem],
            stopped = [Elem | Stopped]}.

%% =============================================================================
%% VM Opperations
%% =============================================================================

%% -----------------------------------------------------------------------------
%% Grouped operator: create_vm
%% -----------------------------------------------------------------------------
create_vm_args(#state{users = Users}) ->
    [elements([U || U = #user{connection = _C}
                        <- maps:values(Users), _C /= undefined]),
     elements(?PACKAGE), elements(?DATASET)].

create_vm_pre(S = #state{users = Users, vms = VMs, creating = Creating}) ->
    [C || #user{connection = C} <- maps:values(Users), C /= undefined] /= []
        andalso has_admin(S)
        andalso length(VMs) < ?MAX_VMS
        andalso length(Creating) =< ?CREATION_CONCURRENCY.

create_vm_pre(#state{login_owners = Owners},
              [#user{connection = C, id = ID}, _Package, _Dataset]) ->
    maps:find(C, Owners) == {ok, ID}.

create_vm(#user{connection = C}, Package, Dataset) ->
    Config = [{<<"networks">>, [{<<"net0">>, ?NETWORK}]}],
    {ok, VmData} = fifo_vms:create(Dataset, Package, Config, C),
    {ok, UUID} = jsxd:get(<<"uuid">>, VmData),
    {UUID, erlang:system_time(seconds)}.

create_vm_next(S = #state{vms = VMs, users = Users, creating = Creating}, VM,
               [User = #user{id = ID, vms = UVMs} , _Package, _Dataset]) ->
    User1 = User#user{vms = [VM | UVMs]},
    Users1 = maps:update(ID, User1, Users),
    S1 = S#state{vms = [VM | VMs], creating = [VM | Creating], users = Users1},
    S1.

create_vm_post(_S, [_C, _Package, _Dataset], {Res, _}) ->
    is_binary(Res);

create_vm_post(_S, [_C, _Package, _Dataset], _Res) ->
    false.

%% -----------------------------------------------------------------------------
%% Grouped operator: list_vms
%% This is a bit tricky, we have to remove the VM's that are in
%% the state deleting from both lists since we're not sure if they still
%% exist.
%% An alternative would be to put this into the precodition isntead of
%% the post condition.
%% -----------------------------------------------------------------------------

list_vms_args(#state{users = Users}) ->
    [elements([U || U = #user{connection = _C} <- maps:values(Users), _C /= undefined])].

list_vms_pre(S = #state{users = Users}) ->
    [C || #user{connection = C} <- maps:values(Users), C /= undefined] /= []
        andalso has_admin(S).


list_vms_pre(#state{login_owners = Owners},
             [#user{connection = C, id = ID} | _]) ->
    maps:find(C, Owners) == {ok, ID}.

list_vms(#user{connection = C}) ->
    {ok, VMs} = fifo_vms:list(C),
    VMs.

list_vms_post(#state{deleted = Deleted, deleting = Deleting},
              [#user{vms = UVMs}], VMs) ->
    UVMs1 = (UVMs -- Deleted) -- Deleting,
    Deleting1 = [UUID || {UUID, _} <- Deleting],
    VMs1 = VMs -- Deleting1,
    lists:sort([UUID || {UUID, _} <- UVMs1]) == lists:sort(VMs1).

list_vms_next(S, _Value, [_User]) ->
    S.

%% -----------------------------------------------------------------------------
%% Grouped operator: get_vm
%% -----------------------------------------------------------------------------

get_vm_pre(S = #state{users = Users, vms = VMs}) ->
    [C || #user{connection = C} <- maps:values(Users), C /= undefined] /= []
        andalso has_admin(S)
        andalso VMs /= [].

get_vm_args(#state{users = Users, vms = VMs}) ->
    [elements([U || U = #user{connection = _C} <- maps:values(Users), _C /= undefined]),
     elements(VMs)].

get_vm_pre(#state{users = Users, vms = VMs, login_owners = Owners},
           [#user{connection = C, id = ID}, VM]) ->
    UserVMs = [UVMs || #user{vms = UVMs} <- maps:values(Users)],
    UserVMs1 = lists:flatten(UserVMs),
    lists:member(VM, VMs)
        andalso lists:member(VM, UserVMs1)
        andalso maps:find(C, Owners) == {ok, ID}.

get_vm(#user{connection = C}, {UUID, _}) ->
    case fifo_vms:get(UUID, C) of
        {ok, _} ->
            {ok, UUID};
        {error, 404} ->
            not_found;
        {error, 403} ->
            forbidden;
        E ->
            E
    end.

get_vm_next(S, _Value, [_User, _VM]) ->
    S.

get_vm_post(#state{deleted = Deleted, deleting = Deleting},
            [#user{vms = UVMs}, VM], forbidden) ->
    not lists:member(VM, UVMs)
        orelse lists:member(VM, Deleted)
        orelse lists:member(VM, Deleting);

get_vm_post(#state{deleted = Deleted, deleting = Deleting},
            [#user{vms = UVMs}, VM], not_found) ->
    not lists:member(VM, UVMs)
        orelse lists:member(VM, Deleted)
        orelse lists:member(VM, Deleting);

get_vm_post(_S, [#user{vms = VMs1}, UUID], {ok, _}) ->
    lists:member(UUID, VMs1);

get_vm_post(_S, [_, _], {error, _}) ->
    false.

%% -----------------------------------------------------------------------------
%% Grouped operator: delete_vm
%% -----------------------------------------------------------------------------
delete_vm_pre(S = #state{users = Users,
                         running = Running, stopped = Stopped}) ->
    [C || #user{connection = C} <- maps:values(Users), C /= undefined] /= []
        andalso has_admin(S)
        andalso (Running ++ Stopped) /= [].

delete_vm_pre(#state{login_owners = Owners},
           [#user{connection = C, id = ID, vms = VMs}, VM]) ->
    lists:member(VM, VMs)
        andalso maps:find(C, Owners) == {ok, ID}.

delete_vm_args(#state{users = Users, running = Running, stopped = Stopped}) ->
    [elements([U || U = #user{connection = _C} <- maps:values(Users), _C /= undefined]),
     elements(Running ++ Stopped)].

delete_vm(#user{connection = C}, {UUID, _}) ->
    case fifo_vms:delete(UUID, C) of
        ok ->
            {UUID, erlang:system_time(seconds)};
        {error, 404} ->
            not_found;
        {error, 403} ->
            forbidden;
        E ->
            E
    end.

delete_vm_next(S = #state{deleting = Deleting,
                          running = Running,
                          stopped = Stopped,
                          users = Users,
                          vms = AllVMs}, New,
               [User = #user{id = UserID, vms = VMs}, Old]) ->
    %% Remove the old VM from the user
    User1 = User#user{vms = [New | [C || C <- VMs, C /= Old]]},
    %% Write the new VM to the user
    Users1 = maps:update(UserID, User1, Users),
    S#state{
      %% Update the users hash to reflect the correct VM list
      users = Users1,
      %% Remove the old VM object form VMs and add the new one
      vms = [New | [C || C <- AllVMs, C /= Old]],

      %% Remove the VM from the running VMs
      running = [C || C <- Running, C /= Old],
      %% Remove the VM from the stoppped VMs
      stopped = [C || C <- Stopped, C /= Old],
      %% Add the element to deleting
      deleting = [New | Deleting]
     }.

delete_vm_post(_S, [_C, _VM], {_, _}) ->
    true;

delete_vm_post(_S, [_C, _VM], _) ->
    false.


%% -----------------------------------------------------------------------------
%% Grouped operator: stop_vm
%% -----------------------------------------------------------------------------
stop_vm_pre(S = #state{users = Users,
                       running = Running}) ->
    [C || #user{connection = C} <- maps:values(Users), C /= undefined] /= []
        andalso has_admin(S)
        andalso (Running) /= [].

stop_vm_pre(#state{login_owners = Owners},
           [#user{connection = C, id = ID, vms = VMs}, VM, _]) ->
    lists:member(VM, VMs)
        andalso maps:find(C, Owners) == {ok, ID}.

stop_vm_args(#state{users = Users, running = Running}) ->
    [elements([U || U = #user{connection = _C} <- maps:values(Users), _C /= undefined]),
     elements(Running),
     bool()].

stop_vm(#user{connection = C}, {UUID, _}, Force) ->
    case fifo_vms:stop(UUID, Force, C) of
        ok ->
            {UUID, erlang:system_time(seconds)};
        {error, 404} ->
            not_found;
        {error, 403} ->
            forbidden;
        E ->
            E
    end.

stop_vm_next(S = #state{stopping = Stopping,
                        running = Running,
                        users = Users,
                        vms = AllVMs}, New,
               [User = #user{id = UserID, vms = VMs}, Old, _Force]) ->
    %% Remove the old VM from the user
    User1 = User#user{vms = [New | [C || C <- VMs, C /= Old]]},
    %% Write the new VM to the user
    Users1 = maps:update(UserID, User1, Users),
    S#state{
      %% Update the users hash to reflect the correct VM list
      users = Users1,
      %% Remove the old VM object form VMs and add the new one
      vms = [New | [C || C <- AllVMs, C /= Old]],

      %% Remove the VM from the running VMs
      running = [C || C <- Running, C /= Old],

      %% Add the element to deleting
      stopping = [New | Stopping]
     }.

stop_vm_post(_S, [_C, _VM, _Force], {_, _}) ->
    true;

stop_vm_post(_S, [_C, _VM, _Force], _) ->
    false.

%% -----------------------------------------------------------------------------
%% Grouped operator: stop_vm
%% -----------------------------------------------------------------------------
start_vm_pre(S = #state{users = Users,
                        stopped = Stopped}) ->
    [C || #user{connection = C} <- maps:values(Users), C /= undefined] /= []
        andalso has_admin(S)
        andalso (Stopped) /= [].

start_vm_pre(#state{login_owners = Owners},
           [#user{connection = C, id = ID, vms = VMs}, VM]) ->
    lists:member(VM, VMs)
        andalso maps:find(C, Owners) == {ok, ID}.

start_vm_args(#state{users = Users, stopped = Stopped}) ->
    [elements([U || U = #user{connection = _C} <- maps:values(Users), _C /= undefined]),
     elements(Stopped)].

start_vm(#user{connection = C}, {UUID, _}) ->
    case fifo_vms:start(UUID, C) of
        ok ->
            {UUID, erlang:system_time(seconds)};
        {error, 404} ->
            not_found;
        {error, 403} ->
            forbidden;
        E ->
            E
    end.

start_vm_next(S = #state{starting = Starting,
                         stopped = Stopped,
                         users = Users,
                         vms = AllVMs}, New,
               [User = #user{id = UserID, vms = VMs}, Old]) ->
    %% Remove the old VM from the user
    User1 = User#user{vms = [New | [C || C <- VMs, C /= Old]]},
    %% Write the new VM to the user
    Users1 = maps:update(UserID, User1, Users),
    S#state{
      %% Update the users hash to reflect the correct VM list
      users = Users1,
      %% Remove the old VM object form VMs and add the new one
      vms = [New | [C || C <- AllVMs, C /= Old]],

      %% Remove the VM from the running VMs
      stopped = [C || C <- Stopped, C /= Old],

      %% Add the element to deleting
      starting = [New | Starting]
     }.

start_vm_post(_S, [_C, _VM], {_, _}) ->
    true;

start_vm_post(_S, [_C, _VM], _) ->
    false.

%% =============================================================================
%% Misc
%% =============================================================================

weight(_S, connect) -> 2;
weight(_S, list_vms) -> 2;
weight(_S, get_vm) -> 2;
weight(_S, create_vm) -> 5;
weight(_S, _Cmd) -> 10.

%% =============================================================================
%% Properties
%% =============================================================================

%% @doc Default generated property
-spec prop_wiggle() -> eqc:property().
prop_wiggle() ->
    ?SETUP(fun() ->
                   %% setup mocking here
                   fifo_api:start(),
                   Admin = admin(),
                   cleanup_vms(Admin, [], []),
                   fun () -> cleanup_vms(Admin, [], []) end %% Teardown function
           end,
           ?FORALL(Cmds, commands(?MODULE),
                   begin
                       {H, S, Res} = run_commands(?MODULE, Cmds,
                                                  [{admin, admin()}]),
                       Res1 = cleanup_vms(S#state.admin, S#state.creating,
                                          S#state.deleting),
                       Success =
                           case {Res, Res1} of
                               {ok, ok} ->
                                   true;
                               {_, ok} ->
                                   false;
                               {ok, _} ->
                                   io:format("Res1: ~p~n", [Res1]),
                                   false;
                               _ ->
                                   io:format("Res1: ~p~n", [Res1]),
                                   false
                           end,
                       pretty_commands(?MODULE, Cmds, {H, S, Res}, Success)
                   end)).

%% =============================================================================
%% Helpers
%% =============================================================================

admin() ->
    #login{login = LoginA, password = PassA} = ?ADMIN,
    C = fifo_api:new([{endpoint, ?ENDPOINT}]),
    {ok, Admin} = fifo_api:auth(LoginA, PassA, C),
    Admin.

cleanup_vms(Admin, Creating, Deleting) ->
    #login{login = Login1, password = Pass1} = ?USER1,
    #login{login = Login2, password = Pass2} = ?USER2,
    C = fifo_api:new([{endpoint, ?ENDPOINT}]),
    {ok, User1} = fifo_api:auth(Login1, Pass1, C),
    {ok, User2} = fifo_api:auth(Login2, Pass2, C),
    %% We do it twice to ensure failed vm's are deleted proppelry.
    [wait_for_creation(Admin, VM) || VM <- Creating],
    Deleting1 = [UUID || {UUID, _} <- Deleting],
    ensure_empty(Admin, User1, User2, Deleting1, 0).

ensure_empty(_Admin, _User1, _User2, _Deleting, 480) ->
    {error, timeout};
ensure_empty(Admin, User1, User2, Deleting, I) ->
    {ok, L1} = fifo_vms:list(User1),
    {ok, L2} = fifo_vms:list(User2),
    case L1 ++ L2 of
        [] ->
            ok;
        UUIDs when I == 0 ->
            UUIDs1 = UUIDs -- Deleting,
            [case fifo_vms:delete(UUID, Admin) of
                 ok ->
                     ok;
                 {error, 423} ->
                     %% We have a locked VM that is icky!
                     %% need to wait this out :/
                     timer:sleep(30000);
                 _ ->
                     ok = fifo_vms:delete(UUID, Admin)
             end || UUID <- UUIDs1],
            timer:sleep(500),
            ensure_empty(Admin, User1, User2, Deleting, I + 1);
        _ ->
            timer:sleep(500),
            ensure_empty(Admin, User1, User2, Deleting, I + 1)
    end.


%% @doc API specification for mocked components
-spec api_spec() -> #api_spec{}.
api_spec() -> #api_spec{ language = erlang, mocking = eqc_mocking, modules = [] }.

pool_state(_C, UUID, Req, 0) ->
    %% {ok, VM} = fifo_vms:get(UUID, C),
    %% {ok, Creating} = jsxd:get(<<"creating">>, VM),
    %% {ok, State} = jsxd:get(<<"state">>, VM),
    io:format(user, "timeout: ~s -> ~s~n", [UUID, Req]),
    %% erlang:halt(),
    io:fread("Something went wrong, continue?> ", "~s"),
    {error, timeout};

pool_state(C, UUID, Req, L) ->
    case fifo_vms:get(UUID, C) of
        {ok, VM} ->
            case {jsxd:get(<<"creating">>, VM), jsxd:get(<<"state">>, VM)} of
                {{ok, true}, _} ->
                    timer:sleep(500),
                    wiggle_eqc:pool_state(C, UUID, Req, L - 1);
                {_, {ok, <<"running">>}} when Req == running ->
                    ok;
                {_, {ok, <<"stopped">>}} when Req == stopped ->
                    ok;
                {_, {ok, <<"failed">>}} ->
                    {error, failed};
                {_, {ok, _State}} ->
                    timer:sleep(500),
                    wiggle_eqc:pool_state(C, UUID, Req, L - 1)
            end;
        {error, timeout} ->
            timer:sleep(500),
            wiggle_eqc:pool_state(C, UUID, Req, L - 1);
        {error, 404} when Req == deleted ->
            ok
    end.

%% This should be imported via fqc.hrl but the import has some issues

has_admin(#state{admin = A}) ->
    A /= undefined.
