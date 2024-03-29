%%%
%%% Registry server
%%%
%%% NOTES
%%%
%%% In order to register a name exactly once over whole cluster we should acquire a lock keyed by that name
%%% under this node's session. But in order to denounce a registration we should _delete_ a lock not just
%%% release it despite the fact that we pay for it with extra Consul roundtrip (not with extra consensus round
%%% though fortunately). This is because we strive to keep working set of the Consul KV store _as minimal as
%%% possible_. Whether it's the objective worth striving to is unclear yet, we should probably gather some
%%% evidence and review it.
%%%
%%% We must treat literally _any_ unexpected error we receive as an evidence of _unknowness_ in the sense
%%% that we do not know for sure what state there is on the remote side (Consul, specifically), which we must
%%% reconcile eventually. There are few error classes which do not obviously produce _unknowness_, HTTP
%%% client errors, failed connect attempts among others. But we must make _undefined by default_ as our
%%% default strategy, conservative but reliable.
%%%
%%% Upon registration we're actively and synchronously trying to acquire a lock in Consul. If we fail then the
%%% process will exit as well so we're kinda safe here. Nevertheless we must reconcile any _unknowness_ if
%%% any and rollback registration.
%%%
%%% Denouncing a registration should always succeed anyway, so any _unknowness_ we get here when trying to
%%% release a lock in Consul we should reconcile through consequent retries. It means that we should keep a
%%% queue of deregistrations.
%%%

-module(consuela_registry_server).

%% api

-type name() :: consuela_registry:name().
-type rid() :: consuela_registry:rid().

-export([start_link/4]).
-export([test/1]).

-export([register/3]).
-export([unregister/3]).
-export([lookup/2]).

-export([select/2]).

-export_type([name/0]).

%% gen server

-behaviour(gen_server).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%% pulse

-type beat() ::
    {
        {deadline_call, deadline(), call()},
        accepted
        | rejected
    }
    | {
        {register | unregister, {name(), pid()} | reg()},
        started
        | {finished, ok | {error, _Reason}}
        | {failed, _FailureReason}
    }
    | {unexpected, {{call, from()} | cast | info, _Msg}}.

-callback handle_beat(beat(), _PulseOpts) -> _.

-export([handle_beat/2]).

-export_type([beat/0]).

%%

%% ETC = Estimated Time to Completion

% TODO too short?
-define(REGISTRATION_ETC, 100).
% TODO too short?
-define(REGISTRATION_TIMEOUT, 2000).

-type ref() :: atom().

-type opts() :: #{
    pulse => {module(), _PulseOpts}
}.

-export_type([ref/0]).
-export_type([opts/0]).

-spec start_link(ref(), consuela_registry:t(), consuela_zombie_reaper:ref(), opts()) -> {ok, pid()}.
start_link(Ref, Registry, ReaperRef, Opts) ->
    gen_server:start_link({local, Ref}, ?MODULE, {Ref, Registry, ReaperRef, Opts}, []).

-spec test(ref()) -> ok | {error, Reason :: term()}.
test(Ref) ->
    Name = <<"test", (integer_to_binary(erlang:unique_integer()))/binary>>,
    handle_result(deadline_call(Ref, {test, {Name, self()}}, ?REGISTRATION_ETC, registration_timeout())).

-spec register(ref(), name(), pid()) -> ok | {error, exists}.
register(Ref, Name, Pid) when is_pid(Pid) ->
    handle_result(deadline_call(Ref, {register, {Name, Pid}}, ?REGISTRATION_ETC, registration_timeout())).

-spec unregister(ref(), name(), pid()) -> ok | {error, notfound}.
unregister(Ref, Name, Pid) when is_pid(Pid) ->
    handle_result(deadline_call(Ref, {unregister, {Name, Pid}}, ?REGISTRATION_ETC, registration_timeout())).

-spec lookup(ref(), name()) -> {ok, pid()} | {error, notfound}.
lookup(Ref, Name) ->
    Registry = get_cached_value(registry, Ref),
    handle_result(lookup(Ref, Name, Registry)).

handle_result({done, Done}) ->
    Done;
handle_result({failed, Error}) ->
    erlang:exit({consuela, Error}).

%%

-spec select(ref(), _NamePattern :: ets:match_pattern()) -> [{name(), pid()}].
select(Ref, Pattern) ->
    Tid = mk_store_tid(Ref),
    lists:map(
        fun(Reg) -> erlang:delete_element(3, Reg) end,
        ets:select(Tid, [{{Pattern, '_', '_'}, [], ['$_']}])
    ).

%%

-type etc() :: pos_integer().
-type deadline() :: integer() | infinity.

-spec deadline_call(ref(), _Call, etc(), timeout()) -> _Result.

-define(IS_SHUTDOWN(R), (R == noproc orelse R == normal orelse R == shutdown)).

deadline_call(Ref, Call, ETC, Timeout) when is_integer(ETC), ETC > 0, Timeout > ETC ->
    try
        gen_server:call(Ref, {deadline_call, compute_call_deadline(ETC, Timeout), Call}, Timeout)
    catch
        exit:{timeout, _} ->
            {failed, {unknown, timeout}};
        exit:Reason when ?IS_SHUTDOWN(Reason) ->
            {failed, registry_terminated};
        exit:{Reason, _} when ?IS_SHUTDOWN(Reason) orelse Reason == killed ->
            {failed, registry_terminated}
    end.

compute_call_deadline(ETC, Timeout) when is_integer(Timeout) ->
    get_now() + Timeout - ETC.

get_now() ->
    erlang:monotonic_time(millisecond).

%%

-type cache() :: ets:tid().
-type store() :: ets:tid().

-type reg() :: {rid(), name(), pid()}.

-type st() :: #{
    store := store(),
    monitors := #{reference() => reg(), name() => reference()},
    registry := consuela_registry:t(),
    reaper := consuela_zombie_reaper:ref(),
    cache := cache(),
    pulse := {module(), _PulseOpts}
}.

-type from() :: {pid(), reference()}.

-spec init({ref(), consuela_registry:t(), consuela_zombie_reaper:ref(), opts()}) -> {ok, st()}.
init({Ref, Registry, ReaperRef, Opts}) ->
    _ = erlang:process_flag(trap_exit, true),
    Store = create_local_store(Ref),
    Cache = create_cache(Ref),
    St = maps:fold(
        fun(pulse, {Module, _} = V, St) when is_atom(Module) ->
            St#{pulse => V}
        end,
        #{
            store => Store,
            monitors => #{},
            registry => Registry,
            reaper => ReaperRef,
            cache => Cache,
            pulse => {?MODULE, []}
        },
        Opts
    ),
    ok = cache_value(registry, Registry, St),
    {ok, St}.

-type call() ::
    {test | register | unregister, {name(), pid()}}.

-type result() :: {done, _Done} | {failed, _Failed}.

-spec handle_call({deadline, deadline(), call()}, from(), st()) -> {reply, result(), st()} | {noreply, st()}.
handle_call({deadline_call, Deadline, Call} = Subject, From, St) ->
    case get_now() of
        T when Deadline > T ->
            % infinity > integer()
            % We still have some time
            _ = beat({Subject, accepted}, St),
            handle_regular_call(Call, From, St);
        _ ->
            % We may not complete operation in time, so just reject it
            _ = beat({Subject, rejected}, St),
            {noreply, St}
    end;
handle_call(Call, From, St) ->
    _ = beat({unexpected, {{call, From}, Call}}, St),
    {noreply, St}.

-spec handle_regular_call(call(), from(), st()) -> {reply, _Result, st()}.
handle_regular_call({Action, Association}, _From, St0) when
    Action == test orelse Action == register orelse Action == unregister
->
    {Result, St1} = handle_activity(Action, Association, St0),
    {reply, Result, St1}.

-spec handle_cast(_Cast, st()) -> {noreply, st()}.
handle_cast(Cast, St) ->
    _ = beat({unexpected, {cast, Cast}}, St),
    {noreply, St}.

-type down() :: {'DOWN', reference(), process, pid(), _Reason}.

-spec handle_info(down(), st()) -> {noreply, st()} | {noreply, st(), 0}.
handle_info({'DOWN', MRef, process, Pid, _Reason}, St) ->
    % TODO beat?
    {noreply, handle_down(MRef, Pid, St)};
handle_info(Info, St) ->
    _ = beat({unexpected, {info, Info}}, St),
    {noreply, St}.

-spec terminate(_Reason, st()) -> ok.
terminate(shutdown, St) ->
    % Implying that it's our duty to denounce every registration we know of upon regular shutdown. Otherwise
    % someone will have to wait for _LockDelay_ to pass until, for example, planned node restart may proceed.
    ok = handle_purge(St);
terminate({shutdown, _Reason}, St) ->
    ok = handle_purge(St);
terminate(_Reason, _St) ->
    ok.

-spec code_change(_Vsn | {down, _Vsn}, st(), _Extra) -> {ok, st()}.
code_change(_Vsn, St, _Extra) ->
    {ok, St}.

%%

handle_activity(Action, {Name, Pid} = Association, St0) ->
    Subject = {Action, Association},
    _ = beat({Subject, started}, St0),
    {Result, St1} =
        case Action of
            test ->
                handle_test(Name, St0);
            register ->
                handle_register(Name, Pid, St0);
            unregister ->
                handle_unregister(Name, Pid, St0)
        end,
    _ =
        case Result of
            {done, Done} ->
                beat({Subject, {finished, Done}}, St1);
            {failed, _} ->
                beat({Subject, Result}, St1)
        end,
    {Result, St1}.

handle_test(TestKey, St = #{registry := Registry}) ->
    {consuela_registry:test(TestKey, Registry), St}.

handle_register(Name, Pid, St) ->
    case lookup_local_store(Name, St) of
        {ok, {_Rid, _Name, Pid}} ->
            % If it's already registered locally do nothing
            {{done, ok}, St};
        {error, notfound} ->
            % If it's not there time to go global
            handle_register_global(Name, Pid, St);
        {ok, _} ->
            % If the name already taken locally error out
            {{done, {error, exists}}, St}
    end.

handle_register_global(Name, Pid, St0 = #{registry := Registry}) ->
    Rid = erlang:unique_integer(),
    Reg = {Rid, Name, Pid},
    Result = consuela_registry:register(Reg, Registry),
    ok = try_drain_reaper(Result, St0),
    ok = try_enqueue_zombie(Result, register, Reg, St0),
    case Result of
        {done, ok} ->
            % Store registration locally and monitor it
            ok = store_local(Reg, St0),
            St1 = monitor_name(Reg, St0),
            {Result, St1};
        {done, {error, _}} ->
            % Someone on another node probably taken it already
            {Result, St0};
        {failed, _} ->
            {Result, St0}
    end.

handle_unregister(Name, Pid, St) ->
    case lookup_local_store(Name, St) of
        {ok, {_Rid, _Name, Pid} = Reg} ->
            % Found it, need to go global then
            handle_unregister_global(Reg, St);
        {ok, _} ->
            % There is another process with this name
            {{done, {error, notfound}}, St};
        {error, notfound} ->
            % There was no such registration
            {{done, {error, notfound}}, St}
    end.

handle_unregister_global(Reg, St0 = #{registry := Registry}) ->
    Result = consuela_registry:unregister(Reg, Registry),
    ok = try_drain_reaper(Result, St0),
    ok = try_enqueue_zombie(Result, unregister, Reg, St0),
    ok = remove_local(Reg, St0),
    St1 = demonitor_name(Reg, St0),
    {Result, St1}.

try_drain_reaper({done, _}, #{reaper := ReaperRef}) ->
    consuela_zombie_reaper:drain(ReaperRef);
try_drain_reaper({failed, _}, _St) ->
    ok.

try_enqueue_zombie({done, _}, _Context, _Reg, _St) ->
    % Everything's alright
    ok;
try_enqueue_zombie({failed, {failed, _}}, register, _Reg, _St) ->
    % Nothing to reconcile anyway
    ok;
try_enqueue_zombie({failed, {Class, _}}, Context, Reg, St) when Context == unregister orelse Class == unknown ->
    % Lock may still be held from the Consul's point of view, need to ensure it will be deleted eventually
    enqueue_zombie(Reg, St).

enqueue_zombie(Reg, #{reaper := ReaperRef}) ->
    consuela_zombie_reaper:enqueue(ReaperRef, [Reg]).

enqueue_zombie(Reg, Opts, #{reaper := ReaperRef}) ->
    consuela_zombie_reaper:enqueue(ReaperRef, [Reg], Opts).

lookup(Ref, Name, Registry) ->
    % Doing local lookup first
    case lookup_local_store(Name, Ref) of
        {ok, Reg} ->
            {done, {ok, get_reg_pid(Reg)}};
        {error, notfound} ->
            case consuela_registry:lookup(Name, Registry) of
                {done, {ok, Pid}} ->
                    % Process may be alive from the global point of view but already dead from the
                    % local point of view. In the latter case the registration should be queued in
                    % the zombie reaper already. Since local liveness check is cheap we perform it
                    % here. This is a kind of safeguard for scenarios where one tries to register
                    % process under the name of a process that just died but have not been reaped
                    % yet.
                    {done, ensure_alive_if_local(Pid)};
                Result ->
                    Result
            end
    end.

ensure_alive_if_local(Pid) ->
    This = erlang:node(),
    case erlang:node(Pid) of
        This ->
            case erlang:is_process_alive(Pid) of
                true -> {ok, Pid};
                false -> {error, notfound}
            end;
        _ ->
            {ok, Pid}
    end.

%%

monitor_name(Reg = {_Rid, Name, Pid}, St = #{monitors := Monitors}) ->
    MRef = erlang:monitor(process, Pid),
    St#{
        monitors := Monitors#{
            MRef => Reg,
            Name => MRef
        }
    }.

demonitor_name({_Rid, Name, _Pid} = Reg, St = #{monitors := Monitors}) ->
    #{Name := MRef} = Monitors,
    #{MRef := Reg} = Monitors,
    true = erlang:demonitor(MRef, [flush]),
    St#{monitors := maps:without([Name, MRef], Monitors)}.

handle_down(MRef, Pid, St0 = #{monitors := Monitors}) ->
    #{MRef := Reg = {_Rid, _Name, Pid}} = Monitors,
    % No need to lookup local store, it must be there
    handle_unregister_dead(Reg, St0).

handle_unregister_dead(Reg, St0) ->
    % Delegate actual deregistration to zombie reaper for now. Nobody would need the result anyway so we can
    % safely rely on another process. This way any storm of process deaths coming in would not block
    % registry.
    ok = enqueue_zombie(Reg, #{drain => true}, St0),
    ok = remove_local(Reg, St0),
    demonitor_name(Reg, St0).

handle_purge(St = #{reaper := ReaperRef}) ->
    consuela_zombie_reaper:enqueue(ReaperRef, list_local(St), #{sync => true}).

%%

mk_store_tid(Ref) ->
    RefBin = erlang:atom_to_binary(Ref, latin1),
    erlang:binary_to_atom(<<"$consuela_registry_store/", RefBin/binary>>, latin1).

create_local_store(Ref) ->
    ets:new(mk_store_tid(Ref), [protected, named_table, {read_concurrency, true}]).

store_local({Rid, Name, Pid}, #{store := Tid}) ->
    true = ets:insert_new(Tid, [{Name, Pid, Rid}]),
    ok.

remove_local({_Rid, Name, _Pid}, #{store := Tid}) ->
    true = ets:delete(Tid, Name),
    ok.

list_local(#{store := Tid}) ->
    ets:foldl(fun({Name, Pid, Rid}, Acc) -> [{Rid, Name, Pid} | Acc] end, [], Tid).

lookup_local_store(Name, #{store := Tid}) ->
    do_lookup(Name, Tid);
lookup_local_store(Name, Ref) ->
    do_lookup(Name, mk_store_tid(Ref)).

do_lookup(Name, Tid) ->
    case ets:lookup(Tid, Name) of
        [{Name, Pid, Rid}] ->
            {ok, {Rid, Name, Pid}};
        [] ->
            {error, notfound}
    end.

get_reg_pid({_Rid, _Name, Pid}) ->
    Pid.

%%

mk_cache_tid(Ref) ->
    RefBin = erlang:atom_to_binary(Ref, latin1),
    erlang:binary_to_atom(<<"$consuela_registry_cache/", RefBin/binary>>, latin1).

create_cache(Ref) ->
    ets:new(mk_cache_tid(Ref), [protected, named_table, {read_concurrency, true}]).

cache_value(Name, Value, #{cache := Tid}) ->
    true = ets:insert(Tid, [{Name, Value}]),
    ok.

get_cached_value(Name, Ref) ->
    Tid = mk_cache_tid(Ref),
    case ets:info(Tid, name) of
        Tid ->
            ets:lookup_element(Tid, Name, 2);
        undefined ->
            erlang:exit({consuela, registry_terminated})
    end.

%%

-spec beat(beat(), st()) -> _.
beat(Beat, #{pulse := {Module, PulseOpts}}) ->
    % TODO handle errors?
    Module:handle_beat(Beat, PulseOpts).

-spec handle_beat(beat(), [trace]) -> ok.
handle_beat(Beat, [trace]) ->
    logger:debug("[~p] ~p", [?MODULE, Beat]);
handle_beat(_Beat, []) ->
    ok.

registration_timeout() ->
    genlib_app:env(consuela, registration_timeout, ?REGISTRATION_TIMEOUT).
