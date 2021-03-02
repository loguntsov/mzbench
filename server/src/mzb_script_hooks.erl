-module(mzb_script_hooks).

-export([pre_hooks/5, post_hooks/5]).

-include_lib("mzbench_language/include/mzbl_types.hrl").

pre_hooks(DirFun, Body, Env, Config, Logger) -> process_hooks(DirFun, pre_hook, Body, Env, Config, Logger).
post_hooks(DirFun, Body, Env, Config, Logger) -> process_hooks(DirFun, post_hook, Body, Env, Config, Logger).

process_hooks(DirFun, HookKind, Body, Env, Config, Logger) ->
    Hooks = [Args || #operation{name = Name, args = Args} <- Body, HookKind == Name],

    NewEnv = lists:foldl(fun ([Operations], AccEnv) ->
                             lists:foldl(fun (O, Acc) -> run_hook(DirFun, O, Acc, Config, Logger) end, AccEnv, Operations)
                         end, Env, Hooks),
    NewEnv.

run_hook(_DirFun, #operation{name=exec, args=[Target, Cmd]}, Env, Config, Logger) ->
    logger:info( "Run exec hook ~tp", [Cmd]),

    #{director_host:= DHost, worker_hosts:= WHosts, user_name:= UserName} = Config,

    Hosts = case Target of
        director -> [DHost];
        all -> [DHost | WHosts]
    end,

    _ = mzb_subprocess:remote_cmd(UserName, Hosts, Cmd, [], Logger, [stderr_to_stdout]),
    Env;
run_hook(DirFun, #operation{name=worker_call, args=[Method | WorkerType]}, Env, _, _Logger) ->
    logger:info( "Run worker hook ~tp:~tp", [WorkerType, Method]),

    case DirFun({call_worker, WorkerType, Method, Env}) of
        {ok, NewEnv} -> NewEnv;
        % special case for languages without atom/keywork type
        {"ok", NewEnv} -> NewEnv;
        IncorrectReturn ->
            logger:info( "Incorrect return value from worker hook ~tp:~tp", [WorkerType, Method]),
            erlang:error({incorrect_hook_return, WorkerType, Method, IncorrectReturn})
    end;

run_hook(_, Operation, _Env, _, _) ->
    erlang:error({unknown_hook_format, Operation}).
