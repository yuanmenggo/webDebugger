%%%-------------------------------------------------------------------
%%% @author bisonwu <guoyuanhua@mingchao.com>
%%% @copyright (C) 2010, mingchao.com
%%% @doc
%%%     for process
%%% @end
%%% Created : 2010-10-25
%%%-------------------------------------------------------------------
-module(common_process).

-compile(export_all).

%%
%% Exported Functions
%%
-export([]).

%%
%% API Functions
%%
info(PID) ->
    info(PID,all).

%% @doc process_info
info(PID,Key) when erlang:is_pid(PID)->
    case Key of
        all->
            Args = [PID];
        _ ->
            Args = [PID, Key]
    end,
    Info = rpc:block_call(node(PID), erlang, process_info, Args),
    case Key of
        all ->
            Info;
        _ ->
            element(2, Info)
    end;
info(RegName,Key) when is_list(RegName) andalso length(RegName) > 3 ->
    case string:substr(RegName, 1,3) of
        [60,48,46]->
            info( list_to_pid(RegName),Key );
        _ ->
            info_name(RegName,Key)
    end;
info(RegName,Key) when is_list(RegName)  ->
    info_name(RegName,Key);
info(RegName,Key) when is_atom(RegName)->
    info_name(RegName,Key).

info_name(RegName,Key)->
    case pid(RegName) of
        undefined->
            undefined;
        PID ->
            info(PID,Key)
    end.

%%发送调试方法信息给进程
debug(RegName,F,A)->
    Msg = {debug,{F,A}},
    erlang:send( common_process:pid(RegName), Msg).
   
%%杀死某进程, 此时进程不收到消息，直接杀死。慎用。
kill(RegName)->
    kill(RegName,kill).

%%杀死某进程
kill(RegName,Reason)->
    exit( common_process:pid(RegName),Reason). 

%% @doc get pid
pid(RegName) when erlang:is_list(RegName) ->
    global:whereis_name(RegName);
pid(RegName)->
    case erlang:whereis(RegName) of
        undefined->
            global:whereis_name(RegName);
        PID->
            PID
    end.

%% @doc messages
m(ProcessName)->
    info(ProcessName,messages).


db(RamTab) when is_atom(RamTab)->
    SubscriberName = common_tool:to_atom( lists:concat([RamTab,"_subscriber"]) ),
    info(SubscriberName).


%% @doc length of messages
mlength(ProcessName)->
    info(ProcessName,message_queue_len).



is_alive(Pid) when is_pid(Pid) ->
    rpc:call(node(Pid) , erlang, is_process_alive, [Pid]);
is_alive(RegName) ->
    case pid(RegName) of
        undefined ->
            false;
        Pid ->
            is_process_alive(Pid)
    end.
    


eval(Str, Binding) ->
    {ok, Ts, _} = erl_scan:string(Str),
    Ts1 = case lists:reverse(Ts) of
              [{dot, _}|_] -> Ts;
              TsR -> lists:reverse([{dot,1}|TsR])
          end,
    {ok, Expr} = erl_parse:parse_exprs(Ts1),
    {value, Value, _} = erl_eval:exprs(Expr, Binding),
    Value.


debugger_attach(StatPid, Pid, Node) ->
    AppPID =  dbg_iserver:call({get_attpid, Pid}),
    case AppPID of
        {ok, undefined}->
            spawn(fun() ->
                          Ref = erlang:monitor(process, StatPid),
                          gen_server:call(dbg_iserver, {attached, self(), Pid}),
                          handle_msg(StatPid, Node, Pid, Ref)
                  end);
        {error, _} ->
            spawn(fun() ->
                          Ref = erlang:monitor(process, StatPid),
                          gen_server:call(dbg_iserver, {attached, self(), Pid}),
                          handle_msg(StatPid, Node, Pid, Ref)
                  end);
        {ok, _PID} ->
            ok
    end.
            
handle_msg(StatPid, Node, Pid, Ref) ->
        receive
            {'DOWN', Ref, _, _, noconnection} ->
                exit(normal);
            {'DOWN', Ref, _, _, Reason} ->
                exit(Reason);
            {'DOWN', _, _, _, _Reason} ->
                erlang:send(StatPid, {attach_pid_down, Pid}, [noconnect]),
                exit(normal);
            Msg ->
                {ok, Meta} = dbg_iserver:call({get_meta, Pid}),
                Bindings = int:meta(Meta, bindings, nostack),
                io:format(">>>>>>>>>>>>>>>>>>>>>>>attach info:~w~n", [Msg]),
                erlang:send(StatPid, {Msg, Pid, Bindings})
        end,
    handle_msg(StatPid, Node, Pid, Ref).
       
