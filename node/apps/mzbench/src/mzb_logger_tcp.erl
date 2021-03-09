-module(mzb_logger_tcp).

-export([
  log/2
]).

-include("mzb_logger_tcp.hrl").

log(Message, Config) ->
  #{
    config := State,
    formatter := { Formatter, FormatterConfig }
  } = Config,
  #state{
    socket = Socket
  } = State,
  gen_tcp:send(Socket, Formatter:format(Message,FormatterConfig)),
  mzb_metrics:notify({"logs.written", counter}, 1),
  ok.
