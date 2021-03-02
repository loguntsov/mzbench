-module(mzb_lager_tcp_protocol).

-behaviour(ranch_protocol).
-behaviour(gen_server).

%% API
-export([start_link/4]).

%% gen_server
-export([init/1,
         init/4,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {socket, transport}).

start_link(Ref, Socket, Transport, Opts) ->
    proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).

dispatch(close_req, #state{socket = Socket, transport = Transport} = State) ->
    Transport:close(Socket),
    {stop, normal, State};

dispatch(Unhandled, State) ->
    logger:error("Unhandled tcp message: ~tp", [Unhandled]),
    {noreply, State}.

init([State]) -> {ok, State}.

init(Ref, Socket, Transport, Opts) ->
    ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{active, once}, {packet, 4}, {keepalive, true}, binary]),

    ok = case lists:member(user, Opts) of
        true ->
            LogQueueMax = application:get_env(mzbench, log_queue_max_len, undefined),
            LogRateLimit = application:get_env(mzbench, log_rate_limit, undefined),
            gen_event:add_handler(lager_event, {mzb_lager_tcp, Socket}, [info, Socket, LogQueueMax, LogRateLimit, "errors.user"]);
        _ -> ok
    end,
    ok = case lists:member(system, Opts) of
        true -> gen_event:add_handler(system_log_lager_event, {mzb_lager_tcp, Socket}, [info, Socket, undefined, 0, "errors.system"]);
        _ -> ok
    end,
    lager:set_loglevel(mzb_lager_tcp, Socket, info),
    gen_server:enter_loop(?MODULE, [], #state{socket=Socket, transport=Transport}).

handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};

handle_info(timeout, State) ->
    {stop, normal, State};

handle_info({tcp, Socket, Msg}, State = #state{socket = Socket}) ->
    dispatch(erlang:binary_to_term(Msg), State);

handle_info({tcp_error, _, Reason}, State) ->
    logger:warning("~tp was closed with reason: ~tp", [?MODULE, Reason]),
    {stop, Reason, State};

handle_info(Info, State) ->
    logger:error("~tp has received unexpected info: ~tp", [?MODULE, Info]),
    {stop, normal, State}.

handle_cast(Msg, State) ->
    logger:error("~tp has received unexpected cast: ~tp", [?MODULE, Msg]),
    {noreply, State}.

handle_call(Request, _From, State) ->
    logger:error("~tp has received unexpected call: ~tp", [?MODULE, Request]),
    {reply, ignore, State}.

terminate(_Reason, #state{socket = Socket}) ->
    gen_event:delete_handler(lager_event, {mzb_lager_tcp, Socket}, []),
    gen_event:delete_handler(system_log_lager_event, {mzb_lager_tcp, Socket}, []),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

