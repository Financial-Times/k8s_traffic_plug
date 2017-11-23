%% signal event handler.
%% Replacement for default `erl_signal_handler`.
%% See Elixir module `FT.K8S.TrafficDrainHandler`.

-module(k8s_signal_handler).
-behaviour(gen_event).

-export([start_link/1, init/1, handle_event/2, handle_info/2, handle_call/2, terminate/2]).

%% Arguments:
%% Table - ETS table name.
%% Delay - delay (in ms) before stopping the VM after `sigterm`.
%% Test - if not `false`, then enters test mode.
%%
%% In test mode no actual `init:stop/1` call is made, since this would interfere with tests!
%%
%% Supplying a PID as the `Test` parameter will cause
%% the handler to send a message to that process for:
%% a) the initial `sigterm` receipt (atom `draining`)
%% b) entering the delayed 'stop' phase (tuple `{stopping, State}`)
%%
%% Supplying `true` for `Test` simply supresses the `init:stop/1` call.
%%
%% Returns `ignore` since this doesn't actually start a process itself,
%% it gets the `gen_event` server to start one.
%%
start_link([Table, Delay, Test]) ->
    % create a named table to store the draining state; it will
    % 'belong' to the calling proc, e.g. a supervisor.
    case ets:info(Table) of
      undefined ->
        ets:new(Table, [named_table, public, {read_concurrency, true}]);
      _info ->
        ok
    end,

    ok = gen_event:swap_sup_handler(
        erl_signal_server,
        {erl_signal_handler, []},
        {k8s_signal_handler, [Table, Delay, Test]}),

    ignore.

% note weird signature because we use gen_event:swap_sup_handler/3
init({[Table, Delay, Test], _}) ->
    {ok, {Table, Delay, Test}}.

handle_event(sigterm, {Table, Delay, Test} = State) ->
    io:format("~n~n***K8STrafficDrain: SIGTERM received. Draining and then stopping in ~p ms~n", [Delay]),
    ets:insert(Table, {draining, true}),
    case Test of
        Pid when is_pid(Pid) ->
            erlang:send(Pid, draining);
        _ -> ok
    end,
    erlang:send_after(Delay, self(), stop),
    {ok, State}
    ;
handle_event(ErrorMsg, S) ->
    % everything else goes to default handler
    erl_signal_handler:handle_event(ErrorMsg, S),
    {ok, S}.

handle_info(stop, {_, _, Test} = State) ->
    io:format("~n~n***K8STrafficDrain: Stopping due to earlier SIGTERM~n", []),
    case Test of
        Pid when is_pid(Pid) ->
            erlang:send(Pid, {stopping, State});
        true ->
            ok;
        _ ->
            ok = init:stop()
    end,
    {ok, State}
    ;
handle_info(_, State) ->
    {ok, State}.

handle_call(_Request, State) ->
    {ok, ok, State}.

terminate(_Args, {_, _, false}) ->
    ok;
terminate(_Args, _State) ->
    io:format("~n~n***K8STrafficDrain: Handler Terminating~n", []),
    ok.
