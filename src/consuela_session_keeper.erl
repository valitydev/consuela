%%%
%%% Consul session keeper

-module(consuela_session_keeper).

%% api

-type deadline() :: integer().
-type session() :: consuela_session:t().
-type interval() ::
    % seconds
    pos_integer()
    % fraction of TTL
    | genlib_rational:t().

-export([start_link/3]).

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
    {session,
        {renewal,
            {succeeded, session(), deadline()}
            | {failed, _Reason}}
        | destroyed
        | expired}
    | {
        {timer, reference()},
        {started, timeout()}
        | fired
        | reset
    }
    | {unexpected, {{call, from()} | cast | info, _Msg}}.

-callback handle_beat(beat(), _PulseOpts) -> _.

-export([handle_beat/2]).

-export_type([beat/0]).

%%

-type opts() :: #{
    interval => interval(),
    pulse => {module(), _PulseOpts}
}.

-export_type([opts/0]).

-spec start_link(session(), consuela_client:t(), opts()) -> {ok, pid()}.
start_link(Session, Client, Opts) ->
    gen_server:start_link(?MODULE, {Session, Client, Opts}, []).

%%

-type st() :: #{
    session := consuela_session:t(),
    deadline := deadline(),
    client := consuela_client:t(),
    interval := interval(),
    timer => reference(),
    pulse := {module(), _PulseOpts}
}.

-type from() :: {pid(), reference()}.

-spec init({consuela_session:t(), consuela_client:t(), opts()}) -> {ok, st(), 0}.
init({Session, Client, Opts}) ->
    _ = erlang:process_flag(trap_exit, true),
    St = maps:fold(
        fun
            (pulse, {Module, _} = V, St) when is_atom(Module) ->
                St#{pulse => V};
            (interval, {P, Q}, St) when is_integer(P), is_integer(Q), Q > 0, P > 0 ->
                St#{interval => genlib_rational:new(P, Q)};
            (interval, Timeout, St) when is_integer(Timeout), Timeout > 0 ->
                St#{interval => Timeout}
        end,
        #{
            session => Session,
            deadline => compute_deadline(Session),
            client => Client,
            % half of a TTL by default
            interval => {1, 2},
            pulse => {?MODULE, []}
        },
        Opts
    ),
    {ok, St, 0}.

-spec handle_call(_Call, from(), st()) -> {noreply, st()}.
handle_call(Call, From, St) ->
    _ = beat({unexpected, {{call, From}, Call}}, St),
    {noreply, St}.

-spec handle_cast(_Cast, st()) -> {noreply, st()}.
handle_cast(Cast, St) ->
    _ = beat({unexpected, {cast, Cast}}, St),
    {noreply, St}.

-type info() :: {timeout, reference(), renew} | timeout.

-spec handle_info(info(), st()) -> {noreply, st()}.
handle_info({timeout, TimerRef, renew}, St = #{timer := TimerRef}) ->
    _ = beat({{timer, TimerRef}, fired}, St),
    {noreply, restart_timer(try_renew_session(St))};
handle_info(timeout, St) ->
    {noreply, restart_timer(try_renew_session(St))};
handle_info(Info, St) ->
    _ = beat({unexpected, {info, Info}}, St),
    {noreply, St}.

-spec terminate(_Reason, st()) -> _.
terminate(_Reason, St) ->
    destroy_session(St).

-spec code_change(_Vsn | {down, _Vsn}, st(), _Extra) -> {ok, st()}.
code_change(_Vsn, St, _Extra) ->
    {ok, St}.

%%

try_renew_session(St0 = #{deadline := Deadline}) ->
    case renew_session(St0) of
        {succeeded, St} ->
            St;
        {failed, St} ->
            case get_timestamp() of
                Now when Now < Deadline ->
                    St;
                _ ->
                    _ = beat({session, expired}, St),
                    exit(session_expired)
            end
    end.

renew_session(St0 = #{session := #{id := ID}, client := Client}) ->
    try consuela_session:renew(ID, Client) of
        {ok, Session} ->
            Deadline = compute_deadline(Session),
            St = St0#{session := Session, deadline := Deadline},
            _ = beat({session, {renewal, {succeeded, Session, Deadline}}}, St),
            {succeeded, St}
    catch
        error:Reason = {Class, _} when Class == failed; Class == unknown ->
            _ = beat({session, {renewal, {failed, Reason}}}, St0),
            {failed, St0}
    end.

destroy_session(St = #{session := #{id := ID}, client := Client}) ->
    ok = consuela_session:destroy(ID, Client),
    _ = beat({session, destroyed}, St),
    St.

compute_deadline(#{ttl := TTL}) ->
    get_timestamp() + TTL.

%%

restart_timer(St) ->
    start_timer(try_reset_timer(St)).

start_timer(St0) ->
    Timeout = try_clamp_timeout(compute_timeout(St0), St0),
    TimerRef = consuela_timer:start(Timeout * 1000, renew),
    St = St0#{timer => TimerRef},
    _ = beat({{timer, TimerRef}, {started, Timeout}}, St),
    St.

compute_timeout(#{interval := Timeout}) when is_integer(Timeout) ->
    Timeout;
compute_timeout(#{interval := Ratio, session := #{ttl := TTL}}) ->
    genlib_rational:round(genlib_rational:mul(Ratio, genlib_rational:new(TTL))).

try_clamp_timeout(Timeout, #{deadline := Deadline}) ->
    erlang:min(Timeout, Deadline - get_timestamp()).

try_reset_timer(St0 = #{timer := TimerRef}) ->
    ok = consuela_timer:reset(TimerRef),
    _ = beat({{timer, TimerRef}, reset}, St0),
    St1 = maps:remove(timer, St0),
    St1;
try_reset_timer(St) ->
    St.

get_timestamp() ->
    % NOTE
    % We assume here that Consul clocks used to keep track of session expiration timestamps are better aligned
    % with OS system time than ERTS instance time. Nevertheless, Consul seems to use system monotonic clock
    % readings to schedule session invalidations so beware of weird effects when both of those clock readings
    % differ much.
    os:system_time(second).

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
