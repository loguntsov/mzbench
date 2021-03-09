#!/usr/bin/env escript
%%! -kernel error_logger false

-mode(compile).

main([Script | Params]) ->
    _ = os:cmd("epmd -daemon"),
    Args = parse_args(Params, []),

    Pa = proplists:get_all_values(pa, Args),
    Validate = proplists:get_value(validate, Args, false),

    ScriptDir = filename:dirname(filename:absname(escript:script_name())),

    LocalCodePaths = lists:foldl(
                    fun (P, Acc) ->
                        filelib:wildcard(filename:join(ScriptDir, P)) ++ Acc
                    end, [],
                    ["../apps/*/ebin/",
                     "../../workers/*/ebin/",
                     "../_build/default/deps/*/ebin/",
                     "../_build/default/checkouts/*/ebin"
                     ]),

    BinDir = filename:dirname(escript:script_name()),
    RpmCodePaths = filelib:wildcard(filename:join(BinDir, "../lib/*/ebin/")),

    code:add_pathsa(RpmCodePaths ++ LocalCodePaths ++ Pa),

    Env = proplists:get_all_values(env, Args),
    Env1 = [{"mzb_script_name", Script} | Env],

    case Validate of
        true -> validate(Script, Env1);
        _    -> run_script(Script, Env1)
    end;

main(_) ->
    usage().

run_script(Script, Env) ->
    ok = application:start(inets),
    {ok, _} = net_kernel:start([nodename_gen(), shortnames]),

    setup_logger([
         {handler, default, logger_std_h, #{
           level => info,
           formatter => {logger_formatter, #{
             template => [time," [", level, "][",pid,"][", mfa, " #", line,"]\n\t",msg,"\n\n"] }
           },
           filters => [
             {skip_progress_info, {fun logger_filters:progress/2, stop}}
           ]
         }},
         {handler, info, logger_std_h, #{
           level => info,
           config => #{
             file => "log/info.log"
           },
           formatter => {logger_formatter, #{
             template => [time," [", level, "][",pid,"][", mfa, " #", line,"]\n\t",msg,"\n\n"] }
           },
           filters => [
             {skip_progress_info, {fun logger_filters:progress/2, stop}}
           ]
         }},
         {handler, error, logger_std_h, #{
           level => error,
           config => #{
             file => "log/error.log"
           },
           formatter => {logger_formatter, #{
             template => [time," [", level, "][",pid,"][", mfa, " #", line,"]\n\t",msg,"\n\n"] }
           },
           filters => [
             {skip_progress_info, {fun logger_filters:progress/2, stop}}
           ]
         }}
       ]),

    {ok, _} = application:ensure_all_started(mzbench),

    mzb_interconnect:set_director([]),

    case mzb_bench_sup:run_bench(filename:absname(Script), Env) of
        ok ->
            Res =
                case mzb_bench_sup:get_results() of
                    {ok, R, _} -> R;
                    {error, _, R, _} -> R
                end,
            io:format("~ts~n", [Res]);
        {error, Messages} ->
            terminate_node(1, mzb_string:join(Messages, "\n"))
    end.

parse_args([], Res) -> Res;
parse_args(["--validate"|T], Res) -> parse_args(T, [{validate, true}|Res]);
parse_args(["--env", KV | T], Res) ->
    {Key, [_Eq | Value]} = lists:splitwith(fun(A) -> A =/= $= end, KV),
    parse_args(T, [{env, {Key, Value}}|Res]);
parse_args(["--pa", P | T], Res) ->
    parse_args(T, [{pa, P}|Res]).

validate(Script, Env) ->
    setup_logger([]),

    ok = application:load(mzbench),

    case mzb_script_validator:read_and_validate(filename:absname(Script), Env) of
        {ok, Warnings, _, _} ->
            terminate_node(0, string:join(Warnings, "\n"));
        {error, _, _, _, Messages} ->
            terminate_node(1, string:join(Messages, "\n"))
    end.

nodename_gen() ->
    {N1,N2,N3} = os:timestamp(),
    Str = lists:flatten(io_lib:format("~tp-~tp~tp", [N1,N2,N3])),
    erlang:list_to_atom(Str).

usage() ->
    io:format("Usage: ~ts ScriptName [--validate] [--env name=value...]~n", [escript:script_name()]).

setup_logger(Handlers) ->
    logger:add_handlers(Handlers),

    application:load(sasl),
    application:set_env(sasl, sasl_error_logger, {file, "/dev/null"}),
    {ok, _} = application:ensure_all_started(sasl).

terminate_node(ExitCode, Message) ->
    %% application:stop(lager),
    case ExitCode == 0 of
        true  -> io:format("~ts~n", [Message]);
        false -> io:format(standard_error, "~ts~n", [Message])
    end,
    erlang:halt(ExitCode).
