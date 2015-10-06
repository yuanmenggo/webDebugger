%%%-------------------------------------------------------------------
%%% Created :  2 Feb 2013 by yuanmenggo,shoumuyushan
%%%-------------------------------------------------------------------
-module(we).
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

start() ->
    ensure_started(sasl),
    ensure_started(ranch),
    ensure_started(cowboy),
  %%  ensure_started(ranch),
    ensure_started(jsx),
    application:start(weDebugger).

stop() ->
    application:stop(weDebugger).
