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
%%%  - Double IP assignments (somehow o.O)
%%%  x Bad return value of the free function when freeing an IP that was not
%%%    used.
%%%  x returning 403 on freshy created VM's from the call returning before
%%%    permissions could be granted.
%%%  - A issue with os:cmd when having a too high ulimit (googled)
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
-define(PACKAGE, [<<"9394cc93-9d1e-4042-9de4-df496622d5bd">>]).
-define(NETWORK, <<"a3cc7f65-c3e8-4869-a4eb-06cf0c6931cc">>).
-define(ENDPOINT, "http://192.168.1.41").

-record(user, {
          id,
          connection,
          vms = []
         }).

-record(state,{
          vms = [],
          users
         }).

%% @doc Returns the state in which each test case starts. (Unless a different
%%      initial state is supplied explicitly to, e.g. commands/2.)
-spec initial_state() -> eqc_statem:symbolic_state().
initial_state() ->
    #state{users = maps:from_list([{ID, #user{id = ID}} || 
                                      ID <- maps:keys(?USERS)])}.

%% ------ Common pre-/post-conditions
%% @doc General command filter, checked before a command is generated.
-spec command_precondition_common(S :: eqc_statem:symbolic_state(),
                                  Command :: atom()) -> boolean().
command_precondition_common(_S, _Command) ->
    true.

%% @doc General precondition, applied *before* specialized preconditions.
-spec precondition_common(S :: eqc_statem:symbolic_state(),
                          C :: eqc_statem:call()) -> boolean().
precondition_common(_S, _Call) ->
    true.

%% ------ Grouped operator: connect
connect_args(#state{users = Users}) ->
    [elements(maps:keys(Users))].

connect(UserID) ->
    #login{login = Login, password = Pass} = maps:get(UserID, ?USERS),
    C = fifo_api:new([{endpoint, ?ENDPOINT}]),
    {ok, C1} = fifo_api:auth(Login, Pass, C),
    C1.

connect_callouts(_S, [_User]) ->
    ?EMPTY.

connect_next(S = #state{users = Users}, C, [UserID]) ->
    User1 = maps:get(UserID, Users),
    User2 = User1#user{connection = {UserID, C}},
    Users1 = maps:update(UserID, User2, Users),
    S1 = S#state{users = Users1},
    S1.

connect_post(_S, [_UserID], Res) ->
    is_tuple(Res) andalso element(1, Res) =:= connection.

%% ------ Grouped operator: create_vm
create_vm_args(#state{users = Users}) ->
    [elements([U || U = #user{connection = _C}
                        <- maps:values(Users), _C /= undefined]),
     elements(?PACKAGE), elements(?DATASET)].

create_vm_pre(#state{users = Users, vms = VMs}) ->
    [C || #user{connection = C} <- maps:values(Users), C /= undefined] /= []
        andalso length(VMs) < 5. %% Condition for S

create_vm_pre(_S, [#user{connection = {CID, _}, id = CID}, _Package, _Dataset]) ->
    true;

create_vm_pre(_S, [User, _Package, _Dataset]) ->
    io:format("bad user: ~p~n", [User]),
    false.

create_vm(#user{connection = {_, C}}, Package, Dataset) ->
    Config = [{<<"networks">>, [{<<"net0">>, ?NETWORK}]}],
    {ok, VmData} = fifo_vms:create(Dataset, Package, Config, C),
    {ok, UUID} = jsxd:get(<<"uuid">>, VmData),
    {ok, running} = pool_state(C, UUID, 320),
    UUID.

create_vm_callouts(_S, [_C, _Package, _Dataset]) ->
    ?EMPTY.

create_vm_next(S = #state{vms = VMs, users = Users}, UUID,
               [User = #user{id = ID, vms = UVMs} , _Package, _Dataset]) ->
    User1 = User#user{vms = [UUID | UVMs]},
    Users1 = maps:update(ID, User1, Users),
    S#state{vms = [UUID | VMs], users = Users1}.

create_vm_post(_S, [_C, _Package, _Dataset], Res) ->
    is_binary(Res).

create_vm_return(_S, [_C, _Package, _Dataset]) ->
    ok.

%% ------ Grouped operator: list_vms
list_vms_args(#state{users = Users}) ->
    [elements([U || U = #user{connection = _C} <- maps:values(Users), _C /= undefined])].

list_vms_pre(#state{users = Users}) ->
    [C || #user{connection = C} <- maps:values(Users), C /= undefined] /= [].

list_vms_pre(_S, [#user{connection = {CID, _}, id = CID}]) ->
    true;
list_vms_pre(_S, [_User]) ->
    false.

list_vms(#user{connection = {_, C}}) ->
    {ok, VMs} = fifo_vms:list(C),
    VMs.

list_vms_callouts(_S, [_User]) ->
    ?EMPTY.

list_vms_next(S, _Value, [_User]) ->
    S.

list_vms_post(_S, [#user{vms = VMs1}], VMs2) ->
    lists:sort(VMs1) == lists:sort(VMs2).

list_vms_return(_S, [_User]) ->
    ok.

%% ------ Grouped operator: get_vm

get_vm_pre(#state{users = Users, vms = VMs}) ->
    [C || #user{connection = C} <- maps:values(Users), C /= undefined] /= []
        andalso VMs /= [].

get_vm_args(#state{users = Users, vms = VMs}) ->
    [elements([U || U = #user{connection = _C} <- maps:values(Users), _C /= undefined]),
     elements(VMs)].

get_vm_pre(_S, [#user{connection = {CID, _}, id = CID}, _VM]) ->
    true;
get_vm_pre(_S, [_User, _VM]) ->
    false.

%%get_vm(#user{connection = {_, C}}, VM) ->
get_vm(#user{connection = {_, C}}, VM) ->
    case fifo_vms:get(VM, C) of
        {ok, _} ->
            {ok, VM};
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

get_vm_post(#state{vms = VMs}, [#user{vms = VMs1}, UUID], not_found) ->
    not lists:member(UUID, VMs1)
        andalso not lists:member(UUID, VMs);

get_vm_post(#state{vms = VMs}, [#user{vms = VMs1}, UUID], forbidden) ->
    not lists:member(UUID, VMs1)
        andalso lists:member(UUID, VMs);

get_vm_post(_S, [#user{vms = VMs1}, UUID], {ok, _}) ->
    lists:member(UUID, VMs1);

get_vm_post(_S, [_, _], {error, _}) ->
    false.

get_vm_return(_S, [_User, _VM]) ->
    ok.

%% ------ ... more operations

%% @doc <i>Optional callback</i>, Invariant, checked for each visited state
%%      during test execution.
%% -spec invariant(S :: eqc_statem:dynamic_state()) -> boolean().
%% invariant(_S) ->
%%   true.

%% @doc weight/2 - Distribution of calls
-spec weight(S :: eqc_statem:symbolic_state(), Command :: atom()) -> integer().
weight(_S, start) -> 1;
weight(_S, _Cmd) -> 1.

%% @doc Default generated property
-spec prop_wiggle() -> eqc:property().
prop_wiggle() ->
    ?SETUP(fun() ->
                   %% setup mocking here
                   fifo_api:start(),
                   cleanup_vms(),
                   fun cleanup_vms/0 %% Teardown function
           end,
           ?FORALL(Cmds, commands(?MODULE),
                   begin
                       application:ensure_all_started(fifo_api),
                       {H, S, Res} = run_commands(?MODULE,Cmds),
                       cleanup_vms(),
                       pretty_commands(?MODULE, Cmds, {H, S, Res},
                                       Res == ok)
                   end)).

cleanup_vms() ->
    #login{login = LoginA, password = PassA} = ?ADMIN,
    #login{login = Login1, password = Pass1} = ?USER1,
    #login{login = Login2, password = Pass2} = ?USER2,
    C = fifo_api:new([{endpoint, ?ENDPOINT}]),
    {ok, Admin} = fifo_api:auth(LoginA, PassA, C),
    {ok, User1} = fifo_api:auth(Login1, Pass1, C),
    {ok, User2} = fifo_api:auth(Login2, Pass2, C),
    %% We do it twice to ensure failed vm's are deleted proppelry.
    ensure_empty(Admin, User1, User2, 0).

ensure_empty(Admin, User1, User2, I) ->
    {ok, L1} = fifo_vms:list(User1),
    {ok, L2} = fifo_vms:list(User2),
    case L1 ++ L2 of
        [] ->
            ok;
        UUIDs when I == 0 ->
            [ok = fifo_vms:delete(UUID, Admin) || UUID <- UUIDs, is_binary(UUID)],
            timer:sleep(1000),
            ensure_empty(Admin, User1, User2, I + 1);
        _ ->
            timer:sleep(1000),
            ensure_empty(Admin, User1, User2, I + 1)
    end.

%% @doc API specification for mocked components
-spec api_spec() -> #api_spec{}.
api_spec() -> #api_spec{ language = erlang, mocking = eqc_mocking, modules = [] }.

pool_state(_, _, 0) ->
    {error, timeout};

pool_state(C, UUID, L) ->
    {ok, VM} = fifo_vms:get(UUID, C),
    case jsxd:get(<<"state">>, VM) of
        {ok, <<"running">>} ->
            {ok, running};
        {ok, <<"failed">>} ->
            {ok, failed};
        _ ->
            timer:sleep(1000),
            wiggle_eqc:pool_state(C, UUID, L - 1)
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
      {timeout, ?EQC_EUNIT_TIMEUT, ?_assert(quickcheck(numtests(?EQC_NUM_TESTS,  ?OUT(?MODULE:A()))))}}
     || {N, A} <- E2].
