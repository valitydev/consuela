-module(session_keeper_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-type test_name() :: atom().
-type config() :: [{atom(), _}].

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

-export([session_renews/1]).
-export([session_destroyed_on_exit/1]).

%% Pulse

-export([handle_beat/2]).

%%

-spec all() -> [test_name()].
all() ->
    [
        session_renews,
        session_destroyed_on_exit
    ].

-spec init_per_suite(config()) -> config().
init_per_suite(C) ->
    Apps =
        genlib_app:start_application(ranch) ++
            genlib_app:start_application(consuela),
    ok = ct_consul:await_ready(),
    Client = consuela_client:new(
        "http://consul0:8500",
        #{
            pulse => {?MODULE, {client, debug}}
        }
    ),
    [{suite_apps, Apps}, {client, Client} | C].

-spec end_per_suite(config()) -> _.
end_per_suite(C) ->
    genlib_app:test_application_stop(?config(suite_apps, C)).

%%

-spec session_renews(config()) -> _.
-spec session_destroyed_on_exit(config()) -> _.

session_renews(C) ->
    TTL = 10,
    Client = ?config(client, C),
    Session = create_session(?FUNCTION_NAME, TTL, Client),
    KeeperOpts = #{
        interval => TTL div 2,
        pulse => {?MODULE, {keeper, debug}}
    },
    {ok, Pid} = consuela_session_keeper:start_link(Session, Client, KeeperOpts),
    ok = timer:sleep(erlang:convert_time_unit(TTL * 2, second, millisecond)),
    ?assertEqual(
        {ok, Session},
        consuela_session:get(maps:get(id, Session), Client)
    ),
    ok = ct_helper:stop_linked(Pid, shutdown).

session_destroyed_on_exit(C) ->
    Client = ?config(client, C),
    Session = create_session(?FUNCTION_NAME, 10, Client),
    KeeperOpts = #{pulse => {?MODULE, {keeper, debug}}},
    {ok, Pid} = consuela_session_keeper:start_link(Session, Client, KeeperOpts),
    ok = ct_helper:stop_linked(Pid, shutdown),
    ?assertEqual(
        {error, notfound},
        consuela_session:get(maps:get(id, Session), Client)
    ).

create_session(Name, TTL, Client) ->
    {ok, ID} = consuela_session:create(genlib:to_binary(Name), consul0, TTL, Client),
    {ok, Session} = consuela_session:get(ID, Client),
    Session.

%%

-type category() :: atom().

-spec handle_beat
    (consuela_client:beat(), {client, category()}) -> ok;
    (consuela_session_keeper:beat(), {keeper, category()}) -> ok.
handle_beat(Beat, {Producer, Category}) ->
    ct:pal(Category, "[~p] ~p", [Producer, Beat]);
handle_beat(_Beat, _) ->
    ok.
