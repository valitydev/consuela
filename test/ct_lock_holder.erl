-module(ct_lock_holder).

-export([start_link/4]).

-export([get/2]).

%%

-behaviour(gen_server).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%%

-type nodename() :: consuela_session:nodename().
-type ttl() :: consuela_session:ttl().
-type lock() :: consuela_lock:lock().
-type client() :: consuela_client:t().

-spec start_link(_Name, nodename(), ttl(), client()) -> {ok, pid()}.
start_link(Name, Nodename, TTL, Client) ->
    gen_server:start_link(?MODULE, {Name, Nodename, TTL, #{client => Client}}, []).

-spec get(_Name, client()) -> {ok, lock()} | {error, notfound}.
get(Name, Client) ->
    consuela_lock:get({<<?MODULE_STRING>>, Name}, Client).

%%

-type st() :: #{
    client := client(),
    session => consuela_session:id(),
    lock => consuela_lock:lock()
}.

-spec init({_Name, nodename(), ttl(), st()}) -> {ok, st()}.
init({Name, Nodename, TTL, St = #{client := Client}}) ->
    _Was = erlang:process_flag(trap_exit, true),
    {ok, SessionID} = consuela_session:create(genlib:to_binary(Name), Nodename, TTL, Client),
    LockID = {<<?MODULE_STRING>>, Name},
    ok = consuela_lock:hold(LockID, self(), SessionID, Client),
    {ok, Lock} = consuela_lock:get(LockID, Client),
    {ok, St#{session => SessionID, lock => Lock}}.

-spec handle_call(_Call, _From, st()) -> {noreply, st()}.
handle_call(_Call, _From, St) ->
    {noreply, St}.

-spec handle_cast(_Cast, st()) -> {noreply, st()}.
handle_cast(_Cast, St) ->
    {noreply, St}.

-spec handle_info(_Info, st()) -> {noreply, st()}.
handle_info(_Info, St) ->
    {noreply, St}.

-spec terminate(_Reason, st()) -> _.
terminate(_Reason, _St = #{lock := Lock, session := SessionID, client := Client}) ->
    ok = consuela_lock:delete(Lock, Client),
    ok = consuela_session:destroy(SessionID, Client),
    ok.

-spec code_change(_Vsn | {down, _Vsn}, st(), _Extra) -> {ok, st()}.
code_change(_Vsn, St, _Extra) ->
    {ok, St}.
