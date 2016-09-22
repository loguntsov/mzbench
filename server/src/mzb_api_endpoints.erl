-module(mzb_api_endpoints).

-export([init/2, info/3, terminate/3, format_results/1]).

-include_lib("kernel/include/file.hrl").

-spec init(cowboy_req:req(), any()) -> {ok, cowboy_req:req(), term()}.
init(Req, _Opts) ->
    try
        lager:debug("REQUEST: ~p", [Req]),
        Path = cowboy_req:path(Req),
        Method = cowboy_req:method(Req),
        lager:info("[ ~s ] ~s", [Method, Path]),
        handle(Method, Path, Req)
    catch
        error:{not_found, Reason} ->
            Req2 = reply_error(404, <<"not_found">>, Reason, Req),
            {ok, Req2, #{}};

        error:{not_supported, Reason} ->
            Req2 = reply_error(501, <<"not_supported">>, Reason, Req),
            {ok, Req2, #{}};

        error:not_running ->
            Req2 = reply_error(400, <<"not_running">>, "Benchmark is not running yet", Req),
            {ok, Req2, #{}};

        error:{badarg, Reason} ->
            Req2 = reply_error(400, <<"badarg">>, Reason, Req),
            {ok, Req2, #{}};

        error:server_inactive ->
            Description = "Server is going to shutdown",
            Req2 = reply_error(503, <<"service_unavailable">>, Description, Req),
            {ok, Req2, #{}};

        _:E ->
            Description = io_lib:format("Server Internal Error: ~p~n~nReq: ~p~n~nStacktrace: ~p", [E, Req, erlang:get_stacktrace()]),
            Req2 = reply_error(500, <<"internal_error">>, Description, Req),
            lager:error(Description),
            {ok, Req2, #{}}
    end.

handle(<<"POST">>, <<"/start">>, Req) ->
    Params = parse_start_params(Req),
    RequestedHost = cowboy_req:header(<<"host">>, Req, undefined),
    case cowboy_req:parse_header(<<"content-type">>, Req) of
        {<<"multipart">>, <<"form-data">>, _} ->
            {Files, Req2} = multipart(Req, []),
            [{ScriptName, ScriptBody}] = proplists:get_all_values(<<"bench">>, Files),
            Includes = proplists:get_all_values(<<"include">>, Files),
            Resp = mzb_api_server:start_bench(
                    Params#{script => #{name => ScriptName, body => ScriptBody},
                            includes => Includes,
                            req_host => RequestedHost}),
            {ok, reply_json(200, Resp, Req2), #{}};
        _ ->
            erlang:error({badarg, "Missing script file"})
    end;

handle(<<"GET">>, <<"/restart">>, Req) ->
    with_bench_id(Req, fun (Id) ->
        Resp = mzb_api_server:restart_bench(Id),
        {ok, reply_json(200, Resp, Req), #{}}
    end);

handle(<<"GET">>, <<"/stop">>, Req) ->
    with_bench_id(Req, fun(Id) ->
        ok = mzb_api_server:stop_bench(Id),
        {ok, reply_json(200, #{status => <<"stopped">>}, Req), #{}}
    end);

handle(<<"GET">>, <<"/change_env">>, Req) ->
    with_bench_id(Req, fun (Id) ->
        NewEnv = cowboy_req:parse_qs(Req),
        ok = mzb_api_server:change_env(Id, proplists:delete(<<"id">>, NewEnv)),
        {ok, reply_json(200, #{status => <<"set">>}, Req), #{}}
    end);

handle(<<"GET">>, <<"/status">>, Req) ->
    with_bench_id(Req, fun(Id) ->
        {ok, reply_json(200, format_status(mzb_api_server:status(Id)), Req), #{}}
    end);

handle(<<"GET">>, <<"/results">>, Req) ->
    with_bench_id(Req, fun(Id) ->
        {ok, reply_json(200, format_results(mzb_api_server:status(Id)), Req), #{}}
    end);

handle(<<"GET">>, <<"/log">>, Req) ->
    with_bench_id(Req, fun(Id) ->
        #{config:= Config} = mzb_api_server:status(Id),
        #{log_compression:= Compression} = Config,
        Filename = mzb_api_bench:log_file(Config),
        {ok, stream_from_file(Filename, Compression, Id, Req), #{}}
    end);

handle(<<"GET">>, <<"/userlog">>, Req) ->
    with_bench_id(Req, fun(Id) ->
        #{config:= Config} = mzb_api_server:status(Id),
        #{log_compression:= Compression} = Config,
        Filename = mzb_api_bench:log_user_file(Config),
        {ok, stream_from_file(Filename, Compression, Id, Req), #{}}
    end);

handle(<<"GET">>, <<"/data">>, Req) ->
    with_bench_id(Req, fun(Id) ->
        #{config:= Config, metrics:= Metrics} =
            fun WaitMetricsCreations() ->
                #{status:= S} = Status = mzb_api_server:status(Id),
                case lists:member(S, [running, stopped, complete, crashed, zombie]) of
                    true -> Status;
                    false ->
                        timer:sleep(1000),
                        WaitMetricsCreations()
                end
            end (),
        MetricNames = mzb_api_metrics:extract_metric_names(Metrics),
        Filenames = [{N, mzb_api_bench:metrics_file(N, Config)} || N <- MetricNames],
        {ok, stream_metrics_from_files(Filenames, Id, Req), #{}}
    end);

handle(<<"GET">>, <<"/email_report">>, Req) ->
    with_bench_id(Req, fun (Id) ->
        #{addr:= Addrs} = cowboy_req:match_qs([{addr, fun check_string_multi_param/1}], Req),
        ok = mzb_api_server:email_report(Id, Addrs),
        {ok, reply_json(200, #{}, Req), #{}}
    end);

handle(<<"GET">>, <<"/server_logs">>, Req) ->
    Headers = [{<<"content-type">>, <<"text/plain">>}],
    #{severity:= Severity} = cowboy_req:match_qs([{severity, fun check_severity/1, info}], Req),
    Req2 = cowboy_req:chunked_reply(200, Headers, Req),
    Id = {mzb_api_slogs_backend, self()},
    ok = gen_event:add_handler(lager_event, Id, [Severity, self()]),
    lager:set_loglevel(mzb_api_slogs_backend, self(), Severity),
    {cowboy_loop, Req2, #{lager_backend_id => Id}};

handle(<<"GET">>, <<"/graphs">>, Req) ->
    with_bench_id(Req, fun(Id) ->
        Location = list_to_binary(mzb_string:format("/#/bench/~p/overview", [Id])),
        Headers = [{<<"Location">>, Location}],
        {ok, cowboy_req:reply(302, Headers, <<>>, Req), #{}}
    end);

%% obsolete endpoint, to be removed soon
handle(<<"GET">>, <<"/report.json">>, Req) ->
    Filter = fun (I) -> mzb_api_ws_handler:normalize([I]) end,
    {_BenchInfo, _, _} = mzb_api_server:get_info(Filter, undefined, undefined, undefined, 1),
    {ok, reply_json(200, #{}, Req), #{}};

handle(<<"GET">>, <<"/clusters_info">>, Req) ->
    List = mzb_api_cloud:clusters_info(),
    Keys = [id, state, n, bench_id, timestamp, provider, hosts, reason],
    F = fun (D) ->
            lists:map(
            fun ({state, S}) -> {state, erlang:atom_to_binary(S, latin1)};
                ({provider, P}) -> {provider, erlang:atom_to_binary(P, latin1)};
                ({hosts, Hosts}) -> {hosts, [erlang:list_to_binary(H) || H <- Hosts]};
                ({reason, Reason}) -> {reason, erlang:iolist_to_binary(io_lib:format("~p", [Reason]))};
                ({K, V}) -> {K, V}
            end, D)
        end,
    Info = [maps:from_list(F([{K,V} || {K,V} <- P, lists:member(K, Keys)])) || P <- List],
    Sorted = lists:usort(
        fun (#{timestamp:= T, id:= Id1}, #{timestamp:= T, id:=Id2}) -> Id1 =< Id2;
            (#{timestamp:= T1}, #{timestamp:= T2}) -> T1 =< T2
        end, Info),
    {ok, reply_json(200, Sorted, Req), #{}};

handle(<<"GET">>, <<"/deallocate_cluster">>, Req) ->
    ClusterId =
        try
            #{id:= Id} = cowboy_req:match_qs([{id, int}], Req),
            Id
        catch
            error:bad_key ->
                erlang:error({badarg, "Missing id argument"});
            error:{case_clause, _} ->
                % case_clause exception is cowboy's way of saying that
                % provided id is not an int
                erlang:error({badarg, "Provided id is not an int"})
        end,

    try
        mzb_api_cloud:destroy_cluster(ClusterId),
        {ok, reply_json(200, #{}, Req), #{}}
    catch
        _:not_found -> erlang:error({not_found, "Cluster not found"});
        _:no_cluster -> erlang:error({not_found, "Cluster is not allocated"})
    end;

handle(<<"GET">>, <<"/remove_cluster_info">>, Req) ->
    ClusterId =
        try
            #{id:= Id} = cowboy_req:match_qs([{id, int}], Req),
            Id
        catch
            error:bad_key ->
                erlang:error({badarg, "Missing id argument"});
            error:{case_clause, _} ->
                % case_clause exception is cowboy's way of saying that
                % provided id is not an int
                erlang:error({badarg, "Provided id is not an int"})
        end,

    try
        mzb_api_cloud:remove_cluster_info(ClusterId),
        {ok, reply_json(200, #{}, Req), #{}}
    catch
        _:not_found -> erlang:error({not_found, "Cluster not found"})
    end;

handle(<<"GET">>, <<"/add_tags">>, Req) ->
    with_bench_id(Req, fun(Id) ->
        Tags =
            try
                #{tags:= TagsStr} = cowboy_req:match_qs([{tags, nonempty}], Req),
                parse_tags(TagsStr)
            catch
                error:bad_key ->
                    erlang:error({badarg, "Missing tags argument"})
            end,

        ok = mzb_api_server:add_tags(Id, Tags),
        {ok, reply_json(200, #{}, Req), #{}}
    end);

handle(<<"GET">>, <<"/remove_tags">>, Req) ->
    with_bench_id(Req, fun(Id) ->
        Tags =
            try
                #{tags:= TagsStr} = cowboy_req:match_qs([{tags, nonempty}], Req),
                parse_tags(TagsStr)
            catch
                error:bad_key ->
                    erlang:error({badarg, "Missing tags argument"})
            end,

        ok = mzb_api_server:remove_tags(Id, Tags),
        {ok, reply_json(200, #{}, Req), #{}}
    end);

handle(Method, Path, Req) ->
    lager:error("Unknown request: ~p ~p~n~p", [Method, Path, Req]),
    erlang:error({not_found, io_lib:format("Wrong endpoint: ~p ~p", [Method, Path])}).

with_bench_id(Req, Action) ->
    Id2 =
        try
            #{id:= Id} = cowboy_req:match_qs([{id, int}], Req),
            Id
        catch
            error:bad_key ->
                erlang:error({badarg, "Missing id argument"});
            error:{case_clause, _} ->
                % case_clause exception is cowboy's way of saying that
                % provided id is not an int
                erlang:error({badarg, "Provided id is not an int"})
        end,
    Action(Id2).

info({log, Msg}, Req, State) ->
    % this code is executed when you write something to log
    % so please don't log inside the function
    ok = cowboy_req:chunk([Msg], Req),
    {ok, Req, State}.

terminate(_Reason, _Req, #{lager_backend_id:= Id}) ->
    gen_event:delete_handler(lager_event, Id, []);
terminate(_Reason, _Req, _State) ->
    ok.

multipart(Req, Res) ->
    case cowboy_req:part(Req) of
        {ok, Headers, Req2} ->
            {ok, Body, Req3} = read_big_file(Req2),
            {file, Field, Filename, _CT, _Enc} = cow_multipart:form_data(Headers),
            multipart(Req3, [{Field, {binary_to_list(Filename), Body}}|Res]);
        {done, Req2} ->
            {Res, Req2}
    end.

read_big_file(Req) ->
    read_big_file(Req, <<>>).

read_big_file(Req, Acc) ->
    case cowboy_req:part_body(Req) of
        {ok, Body, Req2} -> {ok, <<Acc/binary, Body/binary>>, Req2};
        {more, Body, Req2} -> read_big_file(Req2, <<Acc/binary, Body/binary>>)
    end.

reply_json(Code, Map, Req) ->
    case Code of
        200 -> lager:info( "[ RESPONSE ] : ~p ~p", [Code, Map]);
        _   -> lager:error("[ RESPONSE ] : ~p ~p~n~p", [Code, Map, Req])
    end,
    cowboy_req:reply(Code, [{<<"content-type">>, <<"application/json">>}], jiffy:encode(Map), Req).

reply_error(HttpCode, Code, Description, Req) ->
    reply_json(HttpCode,
       #{
            reason_code => Code,
            reason => list_to_binary(Description)
        }, Req).

format_status(#{status:= failed, reason:= {crashed, _Reason}, config:= undefined}) ->
    #{status => failed, reason => crashed};
format_status(#{status:= Status, start_time:= StartTime, finish_time:= FinishTime}) ->
    Data = #{status => Status, start_time => list_to_binary(iso_8601_fmt(StartTime))},
    Data1 = case FinishTime of
        undefined -> Data;
        _ -> Data#{finish_time => list_to_binary(iso_8601_fmt(FinishTime))}
    end,
    Data1.

format_results(#{results:= undefined}) ->
    #{};
% BC code start
format_results(#{results:= [{_, _}|_] = Results}) ->
    maps:from_list([{list_to_binary(Name), #{type => undefined, value => Value}} || {Name, Value} <- Results, Value /= undefined]);
% BC code end
format_results(#{results:= Results}) ->
    Formated = lists:map(
        fun ({Name, counter, {undefined, Percentiles}}) ->
            {list_to_binary(Name), #{type => counter, rps => format_percentiles(Percentiles)}};
            ({Name, counter, {Val, Percentiles}}) ->
            {list_to_binary(Name), #{type => counter, value => Val, rps => format_percentiles(Percentiles)}};
            ({Name, Type, Percentiles}) ->
            {list_to_binary(Name), #{type => Type, percentiles => format_percentiles(Percentiles)}}
        end, Results),
    maps:from_list(Formated);
format_results(#{}) ->
    #{}.

format_percentiles(Percentiles) ->
    maps:from_list([{list_to_binary(Name), Value} || {Name, Value} <- Percentiles]).

check_severity(<<"debug">>) -> {true, debug};
check_severity(<<"info">>) -> {true, info};
check_severity(<<"warning">>) -> {true, warning};
check_severity(<<"error">>) -> {true, error};
check_severity(E) -> erlang:error({badarg, io_lib:format("Invalid severity: ~p", [E])}).

check_string_multi_param(List) when is_list(List) -> {true, [binary_to_list(E)|| E <- List]};
check_string_multi_param(Bin) when is_binary(Bin) -> {true, [binary_to_list(Bin)]};
check_string_multi_param(_) -> false.

parse_update_interval(Bin) ->
    N = erlang:binary_to_integer(Bin),
    case N >= 1000 of
        true -> N;
        false -> erlang:error(invalid_update_interval)
    end.

check_nodes([Nodes]) ->
    List = binary_to_list(Nodes),
    try
        {true, list_to_integer(List)}
    catch
        _:_ -> {true, [binary_to_list(Nodes)]}
    end;
check_nodes(Nodes) when is_list(Nodes) -> {true, [binary_to_list(N) || N <- Nodes]}.


iso_8601_fmt(Seconds) ->
    {{Year,Month,Day},{Hour,Min,Sec}} = calendar:now_to_universal_time({Seconds div 1000000, Seconds rem 1000000, 0}),
    io_lib:format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0BZ",
        [Year, Month, Day, Hour, Min, Sec]).

binary_to_bool(<<"true">>) -> true;
binary_to_bool(<<"false">>) -> false.

parse_start_params(Req) ->
    % List of parameters definition format:
    %   {ParamName,                 single_value,   binary_to_value conversion function,                        DefaultValue},
    %   {ParamName,                 list,           list_of_binaries_to_value conversion function,              DefaultValue}
    ParamsDefs = [
        {nodes,                     list,           fun(List) ->
                                                        {true, List2} = check_nodes(List),
                                                        List2
                                                    end,                                                        1},
        {email,                     list,           fun(List) ->
                                                        {true, List2} = check_string_multi_param(List),
                                                        List2
                                                    end,                                                        []},
        {node_git,                  single_value,   fun erlang:binary_to_list/1,                                undefined},
        {node_commit,               single_value,   fun erlang:binary_to_list/1,                                undefined},
        {emulate_bench_crash,       single_value,   fun binary_to_bool/1,                                       false},
        {deallocate_after_bench,    single_value,   fun binary_to_bool/1,                                       true},
        {provision_nodes,           single_value,   fun binary_to_bool/1,                                       true},
        {exclusive_node_usage,      single_value,   fun binary_to_bool/1,                                       true},
        {benchmark_name,            single_value,   fun erlang:binary_to_list/1,                                undefined},
        {cloud,                     single_value,   fun (N) -> erlang:binary_to_atom(N, latin1) end,            undefined},
        {vm_args,                   list,           fun (List) ->
                                                        {true, List2} = check_string_multi_param(List),
                                                        List2
                                                    end,                                                        []},
        {metric_update_interval_ms, single_value,   fun parse_update_interval/1,                                undefined},
        {tags,                      single_value,   fun parse_tags/1,                                           []}
    ],

    {Params, Env} = lists:mapfoldl(
        fun (K, Acc) ->
            K1 = erlang:atom_to_binary(K, latin1),
            V = proplists:get_all_values(K1, Acc),
            {{K, V}, proplists:delete(K1, Acc)}
        end,
        cowboy_req:parse_qs(Req),
        [ParamName || {ParamName, _, _, _} <- ParamsDefs]),

    Params2 = lists:map(
        fun({ParamName, ValuesList}) ->
            {ParamName,
                case lists:keyfind(ParamName, 1, ParamsDefs) of
                    {_, single_value, BinaryToValueFun, DefaultValue} ->
                        case ValuesList of
                            [Value|_] ->
                                try
                                    BinaryToValueFun(Value)
                                catch
                                    _:_ ->
                                        erlang:error({badarg, io_lib:format("Invalid value \"~s\" for ~s", [Value, ParamName])})
                                end;
                            [] -> DefaultValue
                        end;

                    {_, list, ListOfBinariesToValueFun, DefaultValue} ->
                        case  ValuesList of
                            [] -> DefaultValue;
                            L ->
                                try
                                    ListOfBinariesToValueFun(L)
                                catch
                                    _:_ ->
                                        erlang:error({badarg, io_lib:format("Invalid value \"~p\" for ~s", [L, ParamName])})
                                end
                        end
                end
            }
        end,
        Params),

    Env2 = lists:usort(fun ({K1, _}, {K2, _}) -> K1 =< K2 end, Env),

    maps:from_list([{env, Env2}|Params2]).

stream_from_file(File, Compression, BenchId, Req) ->
    ContentEncoding =
        case Compression of
            none -> <<"identity">>;
            deflate -> <<"deflate">>
        end,
    Headers = [{<<"content-type">>, <<"text/plain">>},
               {<<"content-encoding">>, ContentEncoding}],
    IsFinished =
        fun () ->
            mzb_api_server:is_datastream_ended(BenchId)
        end,

    case IsFinished() of
        true ->
            {ok, #file_info{size = FileSize}} = file:read_file_info(File),
            F = fun (Socket, Transport) -> Transport:sendfile(Socket, File) end,
            Req2 = cowboy_req:set_resp_body_fun(FileSize, F, Req),
            cowboy_req:reply(200, Headers, Req2);
        false ->
            Req2 = cowboy_req:chunked_reply(200, Headers, Req),
            Streamer = fun (Bin) -> cowboy_req:chunk(Bin, Req2) end,
            ReadAtOnce = application:get_env(mzbench_api, bench_read_at_once, undefined),
            {ok, H} = file:open(File, [raw, read, binary, {read_ahead, ReadAtOnce}]),
            try
                PollTimeout = application:get_env(mzbench_api, bench_poll_timeout, undefined),
                stream_data_from_file(H, Streamer, IsFinished, PollTimeout),
                Req2
            after
                file:close(H)
            end
    end.

stream_data_from_file(H, Streamer, IsFinished, Timeout) ->
    IsLastTime = IsFinished(),
    case file:read(H, 1024) of
        {ok, D} ->
            Streamer(D),
            stream_data_from_file(H, Streamer, IsFinished, Timeout);

        eof ->
            case IsLastTime of
                true  ->
                    ok;
                false ->
                    timer:sleep(Timeout),
                    stream_data_from_file(H, Streamer, IsFinished, Timeout)
            end;

        {error, Reason} ->
            erlang:error({log_read_error, Reason})
    end.

stream_metrics_from_files(Files, BenchId, Req) ->
    Headers = [{<<"content-type">>, <<"text/plain">>},
               {<<"content-encoding">>, <<"identity">>}],
    IsFinished =
        fun () ->
            mzb_api_server:is_datastream_ended(BenchId)
        end,

    Req2 = cowboy_req:chunked_reply(200, Headers, Req),
    Streamer = fun (Bin) -> cowboy_req:chunk(Bin, Req2) end,
    ReadAtOnce = application:get_env(mzbench_api, bench_read_at_once, undefined),
    hd(mzb_lists:pmap(
        fun ({Name, File}) ->
            case file:open(File, [raw, read, binary, {read_ahead, ReadAtOnce}]) of
                {ok, H} ->
                    try
                        PollTimeout = application:get_env(mzbench_api, bench_poll_timeout, undefined),
                        fun R() ->
                            IsLastTime = IsFinished(),
                            case file:read_line(H) of
                                {ok, <<>>} ->
                                    R();
                                {ok, D} ->
                                    [Timestamp, Value] = binary:split(D, <<"\t">>),
                                    Streamer(<<Timestamp/binary, "\t", (erlang:list_to_binary(Name))/binary, "\t", Value/binary>>),
                                    R();
                                eof when IsLastTime ->
                                    ok;
                                eof ->
                                    timer:sleep(PollTimeout),
                                    R();
                                {error, Reason} ->
                                    erlang:error({metrics_read_error, Reason})
                            end
                        end (),
                        Req2
                    after
                        file:close(H)
                    end;
                {error, enoent} -> Req
            end
        end, Files)).

parse_tags(Binary) when is_binary(Binary) -> parse_tags(erlang:binary_to_list(Binary));
parse_tags(Str) -> string:tokens(Str, ", ").

