%%%-------------------------------------------------------------------
%%% Created :  2 Feb 2013 by Guoyuanhua <>
%%%-------------------------------------------------------------------
-module(we_timer).
-compile(export_all).
-behaviour(gen_server).
-include("common.hrl").
%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {snapshot_timer_ref = undefined}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

cast_debugger_data(Req) ->
    gen_server:cast({global,?SERVER}, Req).

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.


handle_call(_Request, _From, State) ->
    {reply, ignore, State}.


handle_cast(Request, State) ->
    NewState = try 
                    do_handle_cast(Request, State)
               catch _:Reason ->
                            io:format("---we_timer--catch!!!-----Reason=~w~n",[Reason]),
                            State
                end,   
    {noreply, NewState}.


handle_info(Info, State) ->
    NewState = try
                    do_handle_info(Info, State)
               catch _:Reason ->
                            io:format("---we_timer--catch!!!-----Reason=~w~n",[Reason]),
                            State
               end,
    {noreply, NewState}.

terminate(Reason, _State) ->
    io:format("---we_timer--over!!!-----Reason=~w~n",[Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_handle_cast(start_snapshot, State) ->
    snapshot_timer(State);
do_handle_cast(stop_snapshot, State) ->
     Ref = State#state.snapshot_timer_ref,
    case Ref of
        undefined ->
             ok;
        _ ->
            erlang:cancel_timer(Ref)
    end,
    State#state{snapshot_timer_ref = undefined}; 
do_handle_cast(Msg, State) ->
    io:format("unknow cast request ~p", [Msg]),
    State.
     
do_handle_info({timeout, _, snapshot}, State) ->
    SnapShotList2 = case int:snapshot() of
                        [] ->
                            [];
                        SnapShotList ->
                            do_snapshot_record(SnapShotList, [])
                    end,
    Msg = {processes_snapshot, SnapShotList2},
    we_server_pub:notify(Msg),
    snapshot_timer(State);
do_handle_info(Msg, State) ->
    io:format("unknow info request ~p----~n", [Msg]),
    State.


-define(TIMER_FUN, [fun process_snapshot/1]).

snapshot_timer(State) ->
    lists:foldl(fun(F, S) ->F(S) end, State, ?TIMER_FUN).

process_snapshot(State) ->
    Ref = State#state.snapshot_timer_ref,
    case Ref of
        undefined ->
            ok;
        _ ->
            erlang:cancel_timer(Ref)
    end,
    State#state{snapshot_timer_ref = erlang:start_timer(?ONLINE_INTERVAL, self(), snapshot)}.

do_snapshot_record([], Acc) ->
    Acc;
do_snapshot_record([{Pid, {Mod, Fun, _Param}, Status, Msg}|List], Acc) ->
    if Status =/= exit ->
            Result =  dbg_iserver:call({get_attpid, Pid}),
            SnapShotRecord = case Result of
                                 {ok, undefined} ->
                                     [{pid, Pid}, {mod, Mod}, {funs, Fun},{status, Status}, {msg, Msg},{attached, false}];
                                 {ok, _AttaPid} ->
                                     [{pid, Pid}, {mod, Mod}, {funs, Fun} , {status, Status}, {msg, Msg},{attached, true}];
                                 _ ->
                                     [{pid, Pid}, {mod, Mod}, {funs, Fun} , {status, Status}, {msg, Msg}, {attached, false}]
                             end,
            do_snapshot_record(List, [SnapShotRecord|Acc]);
       true ->
            do_snapshot_record(List, Acc)
    end;
do_snapshot_record([_|List], Acc) ->
    do_snapshot_record(List, Acc).