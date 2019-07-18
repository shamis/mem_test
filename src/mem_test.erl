-module(mem_test).

%% API exports
-export([main/1]).

-record(mafiapp_friends, {name,
                          contact=[],
                          info=[],
                          expertise}).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main([CountStr]) ->
    Count = try list_to_integer(CountStr) of
        C -> C
    catch
        _:_ ->
            io:format("Use integer ~n"),
            erlang:halt(0)
    end,
    net_kernel:start([mem_test, shortnames]),
    ok = mnesia:delete_schema([node()]),
    ok = application:start(recon),
    ok = mnesia:create_schema([node()]),
    application:start(mnesia),
    timer:sleep(5),
    mnesia:create_table(mafiapp_friends,
                        [{attributes, record_info(fields, mafiapp_friends)}]),
    io:format("Table created~n"),
    recon_alloc:set_unit(megabyte),
    io:format("Memory used : ~p MB~nMemory unused : ~p MB~n", [recon_alloc:memory(used), recon_alloc:memory(unused)]),
    io:format("Adding ~p rows~n", [Count]),
    _ = [add_friend("Don Corleone" ++ integer_to_list(N), [], [boss], boss) || N <- lists:seq(1, 1000000)],
    io:format("Added ~p rows~n", [Count]),
    io:format("Memory used : ~p MB~nMemory unused : ~p MB~n", [recon_alloc:memory(used), recon_alloc:memory(unused)]),
    mnesia:delete_table(mafiapp_friends),
    io:format("Deleted the table~n"),
    io:format("Memory used : ~p MB~nMemory unused : ~p MB~n", [recon_alloc:memory(used), recon_alloc:memory(unused)]),
    ok = application:stop(mnesia),
    io:format("Stopped mnesia~n"),
    io:format("Memory used : ~p MB~nMemory unused : ~p MB~n", [recon_alloc:memory(used), recon_alloc:memory(unused)]),
    ok = mnesia:delete_schema([node()]),
    io:format("end~n"),
    erlang:halt(0);
main(Args) ->
    io:format("Use integer ~p ~n", [Args]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

add_friend(Name, Contact, Info, Expertise) ->
    F = fun() ->
                mnesia:write(#mafiapp_friends{name=Name,
                                              contact=Contact,
                                              info=Info,
                                              expertise=Expertise})
        end,
    mnesia:activity(transaction, F).