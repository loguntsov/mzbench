-record(state, {
  ref :: reference(),
  transport :: atom(),
  socket :: port(),
  socket_pid :: pid(),
  error_metric :: binary()
}).