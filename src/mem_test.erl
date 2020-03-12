-module(mem_test).

%% API exports

-export([main/1]).

-record(mafiapp_friends, {name, contact = [], info = [], expertise}).

%%====================================================================
%% API functions
%%====================================================================
%% escript Entry point

main([CountStr]) ->
    Count = try list_to_integer(CountStr) of C -> C catch
            _:_ ->
                io:format("Use integer ~n"),
                erlang:halt(0) end,
    net_kernel:start([mem_test, shortnames]),
    mnesia:delete_schema([node()]),
    application:start(recon),
    mnesia:create_schema([node()]),
    application:ensure_all_started(mnesia_rocksdb),
    application:start(mnesia),
    mnesia_rocksdb:register(),
    io:format("Creating table~n"),
    mnesia:create_table(
        mafiapp_friends,
        % [{attributes, record_info(fields, mafiapp_friends)}]
        % [{disc_copies, [node()]}, {attributes, record_info(fields, mafiapp_friends)}]
        [{rocksdb_copies, [node()]}, {attributes, record_info(fields, mafiapp_friends)}]
    ),
    io:format("Table created~n"),
    recon_alloc:set_unit(megabyte),
    io:format(
        "Memory used : ~p MB~nMemory unused : ~p MB~n",
        [recon_alloc:memory(used), recon_alloc:memory(unused)]
    ),
    io:format("Adding ~p rows~n", [Count]),
    % Result = mnesia:transaction(fun add_friend_2/6, ["Don Corleone", [], [boss], boss, true, Count]),
    % Result = add_friend_2("Don Corleone", [], [boss], boss, false, Count),
    % _ = add_friend("Don Corleone", [], [boss], boss, Count),
    % _ = add_friend_3("Don Corleone", [], [boss], boss, Count),
    % _ = [add_friend_1("Don Corleone" ++ integer_to_list(N), [], [boss], boss) || N <- lists:seq(1, Count)],
    _ = add_friend_4(Count),
    % io:format("Result : ~p~n", [Result]),
    io:format("Rows in the table : ~p~n", [mnesia:table_info(mafiapp_friends, size)]),
    io:format(
        "Memory used : ~p MB~nMemory unused : ~p MB~n",
        [recon_alloc:memory(used), recon_alloc:memory(unused)]
    ),
    mnesia:delete_table(mafiapp_friends),
    io:format("Deleted the table~n"),
    timer:sleep(3000),
    io:format(
        "Memory used : ~p MB~nMemory unused : ~p MB~n",
        [recon_alloc:memory(used), recon_alloc:memory(unused)]
    ),
    io:format("Garbage collecting on all processes~n"),
    [erlang:garbage_collect(X, [{type, major}]) || X <- erlang:processes()],
    timer:sleep(3000),
    io:format(
        "Memory used : ~p MB~nMemory unused : ~p MB~n",
        [recon_alloc:memory(used), recon_alloc:memory(unused)]
    ),
    ok = application:stop(mnesia),
    timer:sleep(3000),
    io:format("Stopped mnesia~n"),
    io:format(
        "Memory used : ~p MB~nMemory unused : ~p MB~n",
        [recon_alloc:memory(used), recon_alloc:memory(unused)]
    ),
    ok = mnesia:delete_schema([node()]),
    io:format("end~n"),
    erlang:halt(0);
main(Args) ->
    io:format("Use integer ~p ~n", [Args]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

add_friend(Name, Contact, Info, Expertise, Count) ->
    F =
        fun
            () ->
                lists:map(
                    fun
                        (C) ->
                            mnesia:write(
                                #mafiapp_friends{
                                    name = Name ++ integer_to_list(C),
                                    contact = Contact,
                                    info = Info,
                                    expertise = Expertise
                                }
                            )
                    end,
                    lists:seq(1, Count)
                )
        end,
    mnesia:activity(transaction, F).

add_friend_1(Name, Contact, Info, Expertise) ->
    F =
        fun
            () ->
                mnesia:write(
                    #mafiapp_friends{
                        name = Name,
                        contact = Contact,
                        info = Info,
                        expertise = Expertise
                    }
                )
        end,
    mnesia:activity(transaction, F).

add_friend_2(_Name, _Contact, _Info, _Expertise, _IsTransaction, 0) -> ok;
add_friend_2(Name, Contact, Info, Expertise, true, Count) ->
    mnesia:write(
        #mafiapp_friends{
            name = Name ++ integer_to_list(Count),
            contact = Contact,
            info = Info,
            expertise = Expertise
        }
    ),
    add_friend_2(Name, Contact, Info, Expertise, true, Count - 1);
add_friend_2(Name, Contact, Info, Expertise, false, Count) ->
    mnesia:dirty_write(
        #mafiapp_friends{
            name = Name ++ integer_to_list(Count),
            contact = Contact,
            info = Info,
            expertise = Expertise
        }
    ),
    add_friend_2(Name, Contact, Info, Expertise, false, Count - 1).

add_friend_3(Name, Contact, Info, Expertise, Count) ->
    Self = self(),
    spawn_link(
        fun
            () ->
                add_friend(Name, Contact, Info, Expertise, Count),
                Self ! finished
        end
    ),
    receive
        finished -> io:format("Finished ~n");
        Other -> io:format("got other : ~p~n", [Other])
    end.

add_friend_4(Count) ->
    Self = self(),
    spawn_link(
        fun
            () ->
                _ =
                    [
                        add_friend_1("Don Corleone" ++ integer_to_list(N), [], [boss], boss)
                        || N <- lists:seq(1, Count)
                    ],
                Self ! finished
        end
    ),
    receive
        finished -> io:format("Finished ~n");
        Other -> io:format("got other : ~p~n", [Other])
    end.
