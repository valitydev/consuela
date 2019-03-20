%%%
%%% Discovery service supervisor

-module(consuela_discovery_sup).

%%

-type opts() :: #{
    name   := consuela_discovery_server:service(),
    tags   => [consuela_discovery_server:tag()], % [] by default
    consul := consul_opts(),
    opts   => consuela_discovery_server:opts() % #{} by default
}.

-type consul_opts() :: #{
    url  := consuela_client:url(),
    opts => consuela_client:opts() % #{} by default
}.

-export([start_link/2]).
-export([stop/1]).

-export_type([opts/0]).

%%

-type ref() :: atom().

-spec start_link(ref(), opts()) ->
    {ok, pid()} | {error, _Reason}.

start_link(Ref, Opts) ->
    Name = maps:get(name, Opts),
    Tags = maps:get(tags, Opts, []),
    Client = mk_consul_client(maps:get(consul, Opts)),
    DiscoveryOpts = maps:get(opts, Opts, #{}),
    genlib_adhoc_supervisor:start_link(
        {local, Ref},
        #{strategy => one_for_one, intensity => 20, period => 5},
        [
            #{
                id    => {Ref, discovery},
                start => {consuela_discovery_server, start_link, [Name, Tags, Client, DiscoveryOpts]}
            }
        ]
    ).

-spec mk_consul_client(consul_opts()) ->
    consuela_client:t().

mk_consul_client(Opts) ->
    Url = maps:get(url, Opts),
    consuela_client:new(Url, maps:get(opts, Opts, #{})).

-spec stop(ref() | pid()) ->
    ok.

stop(Ref) ->
    proc_lib:stop(Ref).