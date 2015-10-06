%%%-------------------------------------------------------------------
%%% Created :  2 Feb 2013 by yuanmenggo,shoumuyushan
%%%-------------------------------------------------------------------
-module(we_break).
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

request_debugger_data(Req) -> 
    gen_server:call({global,?SERVER}, Req, infinity).

cast_debugger_data(Req) ->
    gen_server:cast({global,?SERVER}, Req).

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.


handle_call(Request, _From, State) ->
    {Reply, NewState} = try
                            do_handle_call(Request, State)
                        catch _:Reason ->
                            io:format("---we_break--catch!!!-----Reason=~w~n",[Reason]),
                            {[], State}
                        end,
    {reply, Reply, NewState}.


handle_cast(Request, State) ->
    NewState = try 
                    do_handle_cast(Request, State)
               catch _:Reason ->
                            io:format("---we_break--catch!!!-----Reason=~w~n",[Reason]),
                            State
                end,   
    {noreply, NewState}.


handle_info(Info, State) ->
    NewState = try
                    do_handle_info(Info, State)
               catch _:Reason ->
                            io:format("---we_break--catch!!!-----Reason=~w~n",[Reason]),
                            State
               end,
    {noreply, NewState}.

terminate(Reason, _State) ->
    io:format("---we_break--over!!!-----Reason=~w~n",[Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_handle_call({mod_break, ModName, Line, Flag}, State) ->
    FunString = case Flag of
                     "self" ->
                         "int:break(" ++ ModName ++ ","++Line++"),int:all_breaks("++ModName++").";
                     "all" ->
                          "int:break(" ++ ModName ++ ","++Line++"),int:all_breaks()."
                end,
    Result = common_process:eval(FunString, []),
    Result2 = do_format_break_list(Result, []),
    {Result2, State};
do_handle_call({del_mod_break, ModName, Line}, State) ->
    Result = int:delete_break(we_common:list_to_atom(ModName),list_to_integer(Line)),
    {Result, State};  
do_handle_call(mod_breaks, State) ->
    Result = int:all_breaks(),
    Result2 = do_format_break_list(Result, []),
    {Result2, State};
do_handle_call({mod_breaks, ModName}, State) ->
    Result = int:all_breaks(we_common:list_to_atom(ModName)),
    Result2 = do_format_break_list(Result, []),
    {Result2, State};    
do_handle_call(Info,State) ->
    io:format("unknow request ~p", [Info]),
    {{error, unknow_request},State}.
 

do_handle_cast(Msg, State) ->
    io:format("unknow cast request ~p", [Msg]),
    State.
     

do_handle_info(Msg, State) ->
    io:format("unknow info request ~p----~n", [Msg]),
    State.


do_format_break_list([], Acc) ->
    lists:reverse(Acc);
do_format_break_list([{{ModName, Line},[Status, Action, _, _]}|List],Acc)->
    Path = int:file(ModName),
    case Path of 
        {error, Result} ->
            Result;
        _->
            FullPath = filename:rootname(Path, ".erl"),
            FullPath2 = string:tokens(FullPath, "/"),
            FullPath3 = string:join(FullPath2, "__"),
            BreakRecord = [{name, ModName},{line, Line},{status, Status}, {action, Action}, {path, FullPath3}],
            do_format_break_list(List, [BreakRecord|Acc])
    end;
do_format_break_list([_Break|List], Acc) ->
    do_format_break_list(List, Acc).

