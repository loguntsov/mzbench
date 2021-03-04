-module(mzb_subprocess).

-export([
    cmd/1,
    who_am_i/0,
    remote_format/4,
    remote_cmd/5,
    remote_cmd/6,
    exec_format/2, exec_format/3, exec_format/4,
    check_output/4
]).

-include_lib("mzbench_utils/include/localhost.hrl").

cmd(Cmd) ->
    Output = unicode:characters_to_binary(os:cmd(Cmd)),
    mzb_string:trim_right(Output, $\n).

who_am_i() ->
    cmd("whoami").

remote_format(UserName, Hosts, Formats, Logger) ->
    List = lists:map(fun({Format, Args}) ->
        mzb_string:format(Format, Args)
    end, Formats),
    Bin = mzb_string:join(List, <<" && ">>),
    remote_cmd(UserName, Hosts, Bin, [], Logger).

remote_cmd(UserName, Hosts, Executable, Args, Logger) ->
    remote_cmd(UserName, Hosts, Executable, Args, Logger, []).

remote_cmd(UserName, Hosts, Executable, Args, Logger, Opts) ->
    Args2 = lists:map(
        fun (A) when is_atom(A) -> erlang:atom_to_binary(A);
            (A) -> A
        end, Args),

    CmdFormater =
        case Hosts of
            [Host] when is_list(Host) -> error({bad_host, Host});
            [Host] when ?IS_LOCALHOST(Host) ->
                fun (_) ->
                    OrigPath = os:getenv("ORIG_PATH"),
                    mzb_string:format("bash -c -l \"export PATH='~ts'; ~ts ~ts\"",
                        [OrigPath, Executable, mzb_string:join(Args2, <<" ">>)])
                end;
            _ ->
                fun (H) ->
                    UserNameParam =
                        case UserName of
                            undefined -> <<"">>;
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

exec_format(Format, Logger) ->
    exec_format(Format, [], Logger).

exec_format(Formats, Opts, Logger) ->
    List = lists:map(fun({Format, Args}) ->
        mzb_string:format(Format, Args)
    end, Formats),
    Bin = mzb_string:join(List, <<" && ">>),
    exec_format(Bin, [], Opts, Logger).

exec_format(Format, Args, Opts, Logger) ->
    Handler = fun
        (eof, Acc) -> iolist_to_binary(lists:reverse(Acc));
        (Data, Acc) -> [Data|Acc]
    end,
    exec_format(Format, Args, Opts, Logger, Handler, []).

exec_format(Format, Args, Opts, _Logger, _Handler, _InitState) ->
    Command = mzb_string:format(Format, Args),
    BeforeExec = os:timestamp(),
    logger:info( "[ EXEC ] ~ts (~tp)", [Command, self()]),
    %Port = open_port({spawn, Command}, [stream, eof, exit_status, stderr_to_stdout | Opts]),
    %{Code, _Output} = Res = get_data(Port, Handler, [], Logger),
    {Code, _Output} = Res = exec(Command, Opts),
    case Res of
        {0, Output} ->
            Result = mzb_string:trim_right(Output, $\n),
            logger:info( "[ EXEC_RESULT ] (~tp) ~ts ~n ~ts", [self(), Command, Result]),
            Result;
        {Code, Output} ->
            Duration = timer:now_diff(os:timestamp(), BeforeExec),
            logger:error( "[ EXEC ] Command execution failed in ~tp ms~nCmd: ~ts~nExit code: ~tp~nOutput: ~ts",
                [Duration / 1000, Command, Code, Output]),
            erlang:error({cmd_failed, Command, Code, Output})
    end.

-type logger() :: any(). % FIXME
-spec check_output(string(), [any()], [any()], logger()) -> {integer(), string()}.
check_output(Format, Args, Opts, _Logger) ->
%%    _Handler = fun (eof, Acc) -> iolist_to_binary(lists:reverse(Acc));
%%                  (Data, Acc) -> [Data|Acc]
%%              end,
    Command = mzb_string:format(Format, Args),
    BeforeExec = os:timestamp(),
    logger:info( "[ EXEC ] ~ts (~tp)", [Command, self()]),
    % Port = open_port({spawn,Command}, [stream, eof, exit_status, binary | Opts]),
    % {Code, _Output} = Res = get_data(Port, Handler, [], Logger),
    {Code, _Output} = Res = exec(Command, Opts),
    Duration = timer:now_diff(os:timestamp(), BeforeExec),
    logger:info( "[ EXEC ] Command executed in ~tp ms~nCmd: ~ts~nExit code: ~tp~n",
        [Duration / 1000, Command, Code]),
    Res.

%% INTERNAL

exec(Cmd, Opts) ->
    case exec:run(Cmd, [ sync, stdout, { stderr, stdout }] ++ Opts) of
        { ok, []} -> { 0, <<>>};
        { ok, Info} ->
            Bin = mzb_string:merge(proplists:get_value(stdout, Info )),
            { 0, Bin };
        { error, Info} ->
            Bin = mzb_string:merge(proplists:get_value(stdout, Info )),
            { proplists:get_value(exit_status, Info), Bin}
    end.

%%get_data(Port, Handler, State, Logger) ->
%%    receive
%%        {Port, {data, Bytes}} ->
%%            get_data(Port, Handler, Handler(Bytes, State), Logger);
%%        {Port, eof} ->
%%            Port ! {self(), close},
%%            get_data(Port, Handler, State, Logger);
%%        stop ->
%%            Port ! {self(), close},
%%            get_data(Port, Handler, State, Logger);
%%        {Port, closed} ->
%%            ExitCode =
%%                receive
%%                    {Port, {exit_status, Code}} -> Code
%%                end,
%%            Out = Handler(eof, State),
%%            timer:sleep(1000),
%%            {ExitCode, Out}
%%    end.

