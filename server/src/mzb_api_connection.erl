-module(mzb_api_connection).

-export([start_and_link_with/6,
         send_message/2,
         wait_close/2,
         monitor/1,
         demonitor/1]).

start_and_link_with(PidToLinkWith, Purpose, Host, Port, Dispatcher, State) ->
    Self = self(),
    Pid = spawn(fun () ->
        link(PidToLinkWith),
        try gen_tcp:connect(Host, Port, [{active, false}, {packet, 4}, binary]) of
            {ok, Socket} ->
                logger:info("Connection is started for ~tp on ~ts", [Purpose, Host]),
                Self ! {self(), connected, Socket},
                process_data(Purpose, Host, Socket, Dispatcher, State);
            {error, Reason} ->
                Self ! {self(), failed, Reason}
        catch
            C:E:ST ->
                logger:info("Error ~p:~p ~p", [C, E, ST ]),
                Self ! {self(), failed, {C, E, ST}}
        end
    end),
    receive
        {Pid, connected, Socket} -> {Pid, Socket, Purpose, Host};
        {Pid, failed, Reason} ->
            logger:error("Connection '~tp' is failed to start on host ~ts with reason ~tp", [Purpose, Host, Reason]),
            erlang:error({catch_collector_connect_failed, Host, Reason})
    after 30000 ->
        logger:error("Connection '~tp' is timed-out to start on host ~ts", [Purpose, Host]),
        erlang:error({catch_collector_connect_timedout, Host})
    end.

send_message({_, Socket, _, _}, Message) ->
    gen_tcp:send(Socket, erlang:term_to_binary(Message)).

monitor({Pid, _, _, _}) ->
    erlang:monitor(process, Pid).

demonitor(MonRef) ->
    erlang:demonitor(MonRef, [flush]).

wait_close({Pid, _, Purpose, Host}, Timeout) ->
    Ref = erlang:monitor(process, Pid),
    receive
        {'DOWN', Ref, process, _, _Info} -> ok
    after Timeout ->
        erlang:error({connection_close_timeout, Purpose, Host})
    end.

process_data(Purpose, Host, Socket, Dispatcher, State) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            {ok, NewState} = Dispatcher({message, Data}, State),
            process_data(Purpose, Host, Socket, Dispatcher, NewState);
        {error, closed} ->
            logger:info("Connection '~tp' is closed on host ~ts", [Purpose, Host]),
            Dispatcher({error, closed}, State);
        {error, Reason} ->
            logger:error("Connection '~tp' is failed on host ~ts with reason ~tp", [Purpose, Host, Reason]),
            Dispatcher({error, Reason}, State)
    end.
