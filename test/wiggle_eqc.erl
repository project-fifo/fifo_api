%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2015, Heinz Nikolaus Gies
%%% @doc
%%% Bugs found:
%%%  x "/" instead of $/ in fifo_api_http:url
%%%  x extra / in urls'f in fifo_vm calls
%%%  x blockage of hackney when not reading the body of replies
%%%  - deleting a vm needs to delete the create fms too.
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
%%%  - Timeout on lock server
%%%  - unregister needs to happen as fist event in the vm destruction to prevent
%%%    forbidden errors
%%%  - When requiering decoding during the forbidden phase content was always
%%%    decoded as JOSN.
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
-define(DATASET, [<<"75d1b5d8-e509-11e4-a51f-2fd538c62d87">>]).
-define(PACKAGE, [<<"c009ae95-d893-440e-b3f9-abd40d5f4d4a">>]).
-define(NETWORK, <<"b524adbf-9605-4c28-9129-8173f405334e">>).
-define(ENDPOINT, "http://192.168.1.41").
-define(ENDPOINTS, ["http://192.168.1.41", "http://192.168.1.42"]).
-define(CREATION_CONCURRENCY, 2).

-define(CREATE_TIMEOUT, 320).
-define(START_TIMEOUT, 60).
-define(STOP_TIMEOUT, 60).
-define(DELETE_TIMEOUT, 60).
-define(MAX_VMS, 5).


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

connect_args(#state{users = Users}) ->
    [elements(maps:keys(Users)), elements(?ENDPOINTS)].

connect(UserID, Endpoint) ->
    #login{login = Login, password = Pass} = maps:get(UserID, ?USERS),
    C = fifo_api:new([{endpoint, Endpoint}]),
    {ok, C1} = fifo_api:auth(Login, Pass, C),
    C1.

connect_callouts(_S, [_User, _]) ->
    ?EMPTY.

connect_next(S = #state{users = Users}, C, [UserID, _]) ->
    User1 = maps:get(UserID, Users),
    User2 = User1#user{connection = {UserID, C}},
    Users1 = maps:update(UserID, User2, Users),
    S1 = S#state{users = Users1},
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
    D = timer:now_diff(now(), T0) div 1000000,
    D1 = max(1, ?CREATE_TIMEOUT - D),
    pool_state(C, UUID, running, D1).

wait_for_creation_post(_S, [_C, _Elem], Res) ->
    Res == ok.

wait_for_creation_callouts(_S, [_C, _Elem]) ->
    ?EMPTY.

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
    D = timer:now_diff(now(), T0) div 1000000,
    D1 = max(1, ?DELETE_TIMEOUT - D),
    pool_state(C, UUID, deleted, D1).

wait_for_delete_post(_S, [_C, _Elem], Res) ->
    Res == ok.

wait_for_delete_callouts(_S, [_C, _Elem]) ->
    ?EMPTY.

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
    D = timer:now_diff(now(), T0) div 1000000,
    D1 = max(1, ?START_TIMEOUT - D),
    pool_state(C, UUID, running, D1).

wait_for_start_post(_S, [_C, _Elem], Res) ->
    Res == ok.

wait_for_start_callouts(_S, [_C, _Elem]) ->
    ?EMPTY.

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
    D = timer:now_diff(now(), T0) div 1000000,
    D1 = max(1, ?STOP_TIMEOUT - D),
    pool_state(C, UUID, stopped, D1).

wait_for_stop_post(_S, [_C, _Elem], Res) ->
    Res == ok.

wait_for_stop_callouts(_S, [_C, _Elem]) ->
    ?EMPTY.

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

create_vm_pre(_S, [#user{connection = {CID, _}, id = CID}, _Package, _Dataset]) ->
    true;

create_vm_pre(_S, [_User, _Package, _Dataset]) ->
    false.

create_vm(#user{connection = {_, C}}, Package, Dataset) ->
    Config = [{<<"networks">>, [{<<"net0">>, ?NETWORK}]}],
    {ok, VmData} = fifo_vms:create(Dataset, Package, Config, C),
    {ok, UUID} = jsxd:get(<<"uuid">>, VmData),
    {UUID, now()}.

create_vm_callouts(_S, [_C, _Package, _Dataset]) ->
    ?EMPTY.

create_vm_next(S = #state{vms = VMs, users = Users, creating = Creating}, VM,
               [User = #user{id = ID, vms = UVMs} , _Package, _Dataset]) ->
    User1 = User#user{vms = [VM | UVMs]},
    Users1 = maps:update(ID, User1, Users),
    S#state{vms = [VM | VMs], creating = [VM | Creating], users = Users1}.

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

list_vms_pre(_S, [#user{connection = {CID, _}, id = CID}]) ->
    true;
list_vms_pre(_S, [_User]) ->
    false.

list_vms(#user{connection = {_, C}}) ->
    {ok, VMs} = fifo_vms:list(C),
    VMs.

list_vms_callouts(_S, [_User]) ->
    ?EMPTY.

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

get_vm_pre(_S, [#user{connection = {CID, _}, id = CID}, _VM]) ->
    true;

get_vm_pre(_S, [_User, _VM]) ->
    false.

get_vm(#user{connection = {_, C}}, {UUID, _}) ->
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

get_vm_callouts(_S, [_User, _VM]) ->
    ?EMPTY.

get_vm_next(S, _Value, [_User, _VM]) ->
    S.

get_vm_post(#state{vms = VMs, deleted = Deleted},
            [_, UUID], not_found) ->
    lists:member(UUID, Deleted)
        orelse not lists:member(UUID, VMs);

get_vm_post(#state{vms = VMs, deleted = Deleted},
            [#user{vms = VMs1}, UUID], forbidden) ->
    not lists:member(UUID, VMs1)
        andalso lists:member(UUID, VMs)
        andalso not lists:member(UUID, Deleted);


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

delete_vm_pre(_S, [#user{connection = {CID, _}, id = CID, vms = VMs}, VM]) ->
    lists:member(VM, VMs);

delete_vm_pre(_S, [_, _]) ->
    false.


delete_vm_args(#state{users = Users, running = Running, stopped = Stopped}) ->
    [elements([U || U = #user{connection = _C} <- maps:values(Users), _C /= undefined]),
     elements(Running ++ Stopped)].

delete_vm(#user{connection = {_, C}}, {UUID, _}) ->
    case fifo_vms:delete(UUID, C) of
        ok ->
            {UUID, now()};
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

delete_vm_callouts(_S, [_C, _VM]) ->
    ?EMPTY.

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

stop_vm_pre(_S, [#user{connection = {CID, _}, id = CID, vms = VMs}, VM, _]) ->
    lists:member(VM, VMs);

stop_vm_pre(_S, [_, _, _]) ->
    false.

stop_vm_args(#state{users = Users, running = Running}) ->
    [elements([U || U = #user{connection = _C} <- maps:values(Users), _C /= undefined]),
     elements(Running),
     bool()].

stop_vm(#user{connection = {_, C}}, {UUID, _}, Force) ->
    case fifo_vms:stop(UUID, Force, C) of
        ok ->
            {UUID, now()};
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

stop_vm_callouts(_S, [_C, _VM, _Force]) ->
    ?EMPTY.

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

start_vm_pre(_S, [#user{connection = {CID, _}, id = CID, vms = VMs}, VM]) ->
    lists:member(VM, VMs);

start_vm_pre(_S, [_, _]) ->
    false.

start_vm_args(#state{users = Users, stopped = Stopped}) ->
    [elements([U || U = #user{connection = _C} <- maps:values(Users), _C /= undefined]),
     elements(Stopped)].

start_vm(#user{connection = {_, C}}, {UUID, _}) ->
    case fifo_vms:start(UUID, C) of
        ok ->
            {UUID, now()};
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

start_vm_callouts(_S, [_C, _VM]) ->
    ?EMPTY.

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

                       {H, S, Res} = run_commands(?MODULE,Cmds, [{admin, admin()}]),

                       Res1 = cleanup_vms(S#state.admin, S#state.creating, S#state.creating),
                       pretty_commands(?MODULE, Cmds, {H, S, Res},
                                       Res == ok andalso Res1 == ok)
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

ensure_empty(_Admin, _User1, _User2, _Deleting, 240) ->
    {error, timeout};
ensure_empty(Admin, User1, User2, Deleting, I) ->
    {ok, L1} = fifo_vms:list(User1),
    {ok, L2} = fifo_vms:list(User2),
    case L1 ++ L2 of
        [] ->
            ok;
        UUIDs when I == 0 ->
            UUIDs1 = UUIDs -- Deleting,
            [ok = fifo_vms:delete(UUID, Admin) || UUID <- UUIDs1],
            timer:sleep(1000),
            ensure_empty(Admin, User1, User2, Deleting, I + 1);
        _ ->
            timer:sleep(1000),
            ensure_empty(Admin, User1, User2, Deleting, I + 1)
    end.

%% @doc API specification for mocked components
-spec api_spec() -> #api_spec{}.
api_spec() -> #api_spec{ language = erlang, mocking = eqc_mocking, modules = [] }.

pool_state(_, _, _, 0) ->
    {error, timeout};

pool_state(C, UUID, Req, L) ->
    case fifo_vms:get(UUID, C) of
        {ok, VM} ->
            case jsxd:get(<<"state">>, VM) of
                {ok, <<"running">>} when Req == running ->
                    ok;
                {ok, <<"stopped">>} when Req == stopped ->
                    ok;
                {ok, <<"failed">>} ->
                    {error, failed};
                {ok, _State} ->
                    timer:sleep(1000),
                    wiggle_eqc:pool_state(C, UUID, Req, L - 1)
            end;
        {error, 404} when Req == deleted ->
            ok
    end.

-define(OUT(P),
        on_output(fun
                      (".", []) ->
                         io:fwrite(user, <<"\e[0;32m*\e[0m">>, []);
                      ("x", []) ->
                         io:format(user, <<"\e[0;33mx\e[0m">>, []);
                      ("Failed! ", []) ->
                         io:format(user, <<"\e[0;31mFailed! \e[0m">>, []);
                      (S, F) ->
                         io:format(user, S, F)
                 end, P)).

-define(EQC_NUM_TESTS, 10).

-ifndef(EQC_NUM_TESTS).

-ifdef(EQC_LONG_TESTS).
-define(EQC_NUM_TESTS, 5000).
-else.  % EQC_LONG_TESTS
-ifdef(EQC_SHORT_TEST).
-define(EQC_NUM_TESTS, 100).
-else.  % EQC_SHORT_TEST
-define(EQC_NUM_TESTS, 500).
-endif. % EQC_SHORT_TEST
-endif. % EQC_LONG_TESTS

-endif. % EQC_NUM_TESTS

-ifndef(EQC_EUNIT_TIMEUT).
-define(EQC_EUNIT_TIMEUT, (?EQC_NUM_TESTS div 5)).
-endif.

run_test_() ->
    [{exports, E} | _] = module_info(),
    E1 = [{atom_to_list(N), N} || {N, 0} <- E],
    E2 = [{N, A} || {"prop_" ++ N, A} <- E1],
    [{"Running " ++ N ++ " propperty test",
      {timeout, ?EQC_EUNIT_TIMEUT,
       ?_assert(quickcheck(numtests(?EQC_NUM_TESTS,  ?OUT(?MODULE:A()))))}}
     || {N, A} <- E2].

has_admin(#state{admin = A}) ->
    A /= undefined.
