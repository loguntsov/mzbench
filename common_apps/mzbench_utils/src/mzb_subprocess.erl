-module(mzb_subprocess).

-export([
    remote_cmd/5,
    remote_cmd/6,
    exec_format/4,
    check_output/4
]).

remote_cmd(UserName, Hosts, Executable, Args, Logger) ->
    remote_cmd(UserName, Hosts, Executable, Args, Logger, [stderr_to_stdout]).

remote_cmd(UserName, Hosts, Executable, Args, Logger, Opts) ->
    Args2 = lists:map(
        fun (A) when is_atom(A) -> erlang:atom_to_list(A);
            (A) -> A
        end, Args),

    CmdFormater =
        case Hosts of
            [Host] when is_list(Host) -> error({bad_host, Host});
            [Host] when Host == <<"localhost">>; Host == <<"127.0.0.1">> ->
                fun (_) ->
                    OrigPath = os:getenv("ORIG_PATH"),
                    mzb_string:format("bash -c -l \"export PATH='~ts'; ~ts ~ts\"",
                        [OrigPath, Executable, string:join(Args2, " ")])
                end;
            _ ->
                fun (H) ->
                    UserNameParam =
                        case UserName of
                            undefined -> "";
                            _ -> mzb_string:format("~ts@", [UserName])
                        end,
                    mzb_string:format("ssh -A -o LogLevel=ERROR -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no ~ts~ts \"source /etc/profile; ~ts ~ts\"",
                            [UserNameParam, H, Executable, string:join(Args2, " ")])
                end
        end,

    logger:info( "[ REMOTE EXEC ] ~ts~n  at ~tp", [CmdFormater("<HOST>"), Hosts]),

    try
        mzb_lists:pmap(
            fun (Host) ->
                % path might contain ~ so we need to replace it with ~~ to let mzb_string:format work correctly
                CmdStr = re:replace(CmdFormater(Host), "~", "~~", [{return, list}, global]),
                exec_format(CmdStr, [], Opts, Logger)
            end, Hosts)
    catch
        C:{cmd_failed, Cmd, Code, Output} = E:ST ->
            logger:error( "[ REMOTE EXEC ] Command execution failed:~nCmd: ~ts~nExit code: ~tp~nOutput: ~ts", [Cmd, Code, Output]),
            erlang:raise(C, E, ST);
        C:E:ST ->
            logger:error( "[ REMOTE EXEC ] Command execution unnormally failed: ~tp~nCmd: ~tp~nArgs: ~tp~nHosts: ~tp~nStacktrace: ~tp", [E, Executable, Args, Hosts, ST]),
            erlang:raise(C, E, ST)
    end.

exec_format(Format, Args, Opts, Logger) ->
    Handler = fun
        (eof, Acc) -> iolist_to_binary(lists:reverse(Acc));
        (Data, Acc) -> [Data|Acc]
    end,
    exec_format(Format, Args, Opts, Logger, Handler, []).

exec_format(Format, Args, Opts, Logger, Handler, InitState) ->
    Command = mzb_string:format(Format, Args),
    BeforeExec = os:timestamp(),
    logger:info( "[ EXEC ] ~ts (~tp)", [Command, self()]),
    Port = open_port({spawn, Command}, [stream, eof, exit_status | Opts]),
    case get_data(Port, Handler, InitState, Logger) of
        {0, Output} ->
            unicode:characters_to_binary(string:strip(unicode:characters_to_list(Output), right, $\n));
        {Code, Output} ->
            Duration = timer:now_diff(os:timestamp(), BeforeExec),
            logger:error( "[ EXEC ] Command execution failed in ~tp ms~nCmd: ~ts~nExit code: ~tp~nOutput: ~ts",
                [Duration / 1000, Command, Code, Output]),
            erlang:error({cmd_failed, lists:flatten(Command), Code, Output})
    end.

-type logger() :: any(). % FIXME
-spec check_output(string(), [any()], [any()], logger()) -> {integer(), string()}.
check_output(Format, Args, Opts, Logger) ->
    Handler = fun (eof, Acc) -> iolist_to_binary(lists:reverse(Acc));
                  (Data, Acc) -> [Data|Acc]
              end,
    Command = mzb_string:format(Format, Args),
    BeforeExec = os:timestamp(),
    logger:info( "[ EXEC ] ~ts (~tp)", [Command, self()]),
    Port = open_port({spawn,Command}, [stream, eof, exit_status, binary | Opts]),
    {Code, _Output} = Res = get_data(Port, Handler, [], Logger),
    Duration = timer:now_diff(os:timestamp(), BeforeExec),
    logger:info( "[ EXEC ] Command executed in ~tp ms~nCmd: ~ts~nExit code: ~tp~n",
        [Duration / 1000, Command, Code]),
    Res.

%% INTERNAL

get_data(Port, Handler, State, Logger) ->
    receive
        {Port, {data, Bytes}} ->
            get_data(Port, Handler, Handler(Bytes, State), Logger);
        {Port, eof} ->
            Port ! {self(), close},
            get_data(Port, Handler, State, Logger);
        stop ->
            Port ! {self(), close},
            get_data(Port, Handler, State, Logger);
        {Port, closed} ->
            ExitCode =
                receive
                    {Port, {exit_status, Code}} -> Code
                end,
            Out = Handler(eof, State),
            {ExitCode, Out}
    end.

