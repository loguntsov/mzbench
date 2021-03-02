-module(mzb_watchdog).

-behaviour(gen_server).

%% API
-export([start_link/0, activate/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(s, {
}).

-define(INTERVAL, 1000).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    logger:info("[ watchdog ] Start"),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

activate() ->
    gen_server:cast(?MODULE, activate).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #s{}}.

handle_call(_Request, _From, State) ->
   {noreply, State}.

handle_cast(activate, State) ->
    start_check_timer(),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(trigger, State) ->
    {noreply, check(State)};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_check_timer() ->
    _ = erlang:send_after(?INTERVAL, self(), trigger),
    ok.

check(State) ->
    try mzb_interconnect:call_director(is_director_alive, 30000) of
        true  ->
            start_check_timer(),
            State;
        false ->
            logger:warning("[ watchdog ] Node ~tp is going to shutdown in 20 sec because director process is down", [node()]),
            {ok, _} = timer:apply_after(20000, erlang, halt, [1]),
            State
    catch
        _:Error ->
            logger:warning("[ watchdog ] Node ~tp is going to shutdown in 20 sec because director check failed with reason: ~tp", [node(), Error]),
            {ok, _} = timer:apply_after(20000, erlang, halt, [1]),
            State
    end.

