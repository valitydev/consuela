%%%
%%% OTP Registry interface

-module(consuela).

%%

-type name() :: consuela_registry:name().

-export([test/0]).

-export([all/0]).
-export([select/1]).

-export([register_name/2]).
-export([unregister_name/1]).
-export([whereis_name/1]).

%%

-spec test() -> ok | {error, Reason :: term()}.
test() ->
    consuela_registry_server:test(consuela).

-spec all() -> [{name(), pid()}].
all() ->
    consuela_registry_server:select(consuela, '_').

-spec select(_NamePattern :: ets:match_pattern()) -> [{name(), pid()}].
select(NamePattern) ->
    consuela_registry_server:select(consuela, NamePattern).

%%

-spec register_name(name(), pid()) ->
    % Lol why
    yes | no.

-spec unregister_name(name()) -> _.

-spec whereis_name(name()) -> pid() | undefined.

register_name(Name, Pid) ->
    case consuela_registry_server:register(consuela, Name, Pid) of
        ok ->
            yes;
        {error, exists} ->
            no
    end.

unregister_name(Name) ->
    ok = consuela_registry_server:unregister(consuela, Name, self()),
    ok.

whereis_name(Name) ->
    case consuela_registry_server:lookup(consuela, Name) of
        {ok, Pid} ->
            Pid;
        {error, notfound} ->
            undefined
    end.
