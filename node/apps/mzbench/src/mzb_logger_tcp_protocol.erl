-module(mzb_logger_tcp_protocol).

-behaviour(ranch_protocol).
-export([
  start_link/3,
  init/3
]).

-include("mzb_logger_tcp.hrl").


start_link(Ref, Transport, Opts) ->
  proc_lib:start_link(?MODULE, init, [Ref, Transport, Opts]).

init(Ref, Transport, Opts) ->
  {ok, Socket} = ranch:handshake(Ref),
  ok = proc_lib:init_ack({ok, self()}),
  ok = Transport:setopts(Socket, [{active, once}, {packet, 4}, {keepalive, true}, binary]),

  Ref = make_ref(),
  State = #state{
    ref = Ref,
    transport = Transport,
    socket = Socket
  },
  try
    case lists:member(user, Opts) of
      true ->
        HandlerConfig0 = #{
          config => State#state{
            error_metric = <<"errors.user">>
          },
          level => info,
          filter_default => log,
          formatter => {logger_formatter, #{
            template => [time," [", level, "][",pid,"][", mfa, " #", line,"]\n\t",msg,"\n\n"] }
          },
          filters => [
            {skip_progress_info, {fun logger_filters:progress/2, stop}}
          ]
        },
        logger:add_handler(user_logger, mzb_logger_tcp, HandlerConfig0);
      _ -> ok
    end,
    ok = case lists:member(system, Opts) of
      true ->
        HandlerConfig1 = #{
          config => State#state{
            error_metric = <<"errors.system">>
          },
          level => info,
          filter_default => log,
          formatter => {logger_formatter, #{
            template => [time," [", level, "][",pid,"][", mfa, " #", line,"]\n\t",msg,"\n\n"] }
          },
          filters => [
            {skip_progress_info, {fun logger_filters:progress/2, stop}}
          ]
        },
        logger:add_handler(system_logger, mzb_logger_tcp, HandlerConfig1);
      _ -> ok
    end,
    loop(State)
  after
    logger:remove_handler(Ref)
  end.

loop(State) ->
  Transport = State#state.transport,
  Socket = State#state.socket,
  case Transport:recv(Socket, 0, 1000) of
    {ok, Data} ->
      process({ recv, Data}, State),
      loop(State);
    { error, timeout } ->
      recv_loop(State);
    { error, Reason } ->
      logger:error("Socket closed with reason ~p", [ Reason ]),
      ok = Transport:close(Socket)
  end.

recv_loop(State) ->
  Result = receive
    Msg -> { ok, Msg}
  after 0 ->
    timeout
  end,
  { Flag, NewState } = case Result of
    { ok, Any } ->
      process(Any, State);
    timeout ->
      { leave, State }
  end,
  case Flag of
    ok -> recv_loop(NewState);
    leave -> loop(NewState);
    stop ->
      logger:error("Socket stopped"),
      Transport = State#state.transport,
      Socket = State#state.socket,
      ok = Transport:close(Socket)
  end.

process({recv, _Data}, State) ->
  { ok, State };

process(_, State) ->
  { ok, State }.