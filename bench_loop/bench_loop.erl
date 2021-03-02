-module(bench_loop).

-export([main/1]).

rates() ->
    [
         1000
    ,  500000
    , 1000000
    , 1500000
    , 2000000
    , 2500000
    , 2800000
    , 3000000
    , 3300000
    , 3600000
    , 4000000
    ].

duration() -> 5.

main(_Args) ->
    Stats = [[R, bench_rps(constant, R), bench_rps(ramp, R)]
        || R <- rates()],
    UnboundedRPS = bench_rps(unbounded, undefined),
    io:format([io_lib:format("~tp, ~tp, ~tp, ~tp\n", Stat ++ [UnboundedRPS]) || Stat <- Stats]).

bench_rps(RateProfile, TargetRPS) ->
    Script = make_script(RateProfile, TargetRPS),
    {Iterations, Elapsed} = run(Script),
    ActualRPS = Iterations div duration(),
    io:format(standard_error,
        "Expected ~tp rps, got ~tp rps, time elapsed ~tp.~n",
        [TargetRPS, ActualRPS, Elapsed div 1000]),
    ActualRPS.

make_script(RateProfile, TargetRPS) ->
    RateSpec = case RateProfile of
        unbounded -> "";
        constant ->
            lists:flatten(io_lib:format(
                ", {rate, {~tp, rps}}",
                [TargetRPS]));
        ramp ->
            lists:flatten(io_lib:format(
                ", {rate, {ramp, linear, {~tp, rps}, {~tp, rps}}}",
                [TargetRPS, TargetRPS + 1]))
    end,
    lists:flatten(io_lib:format(
        "[{loop, [{time, {~tp, sec}}~ts],
           [{inc}]}].",
        [duration(), RateSpec])).

run(Script) ->
    AST = mzbl_script:parse(Script),
    TimeBefore = mzbl_loop:msnow(),
    InitialState = mzb_erl_worker:init(counter_worker),
    X = mzbl_interpreter:eval(
        AST,
        InitialState,
        [],
        mzb_erl_worker),
    {_, {counter_worker, R}} = X,
    TimeAfter = mzbl_loop:msnow(),
    {R, TimeAfter - TimeBefore}.
