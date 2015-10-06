%%%-------------------------------------------------------------------
%%% Created :  2 Feb 2013 by Guoyuanhua <>
%%%-------------------------------------------------------------------
-module(we_debugger).
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
                            io:format("---we_debugger--catch!!!-----Reason=~w~n",[Reason]),
                            {[], State}
                        end,
    {reply, Reply, NewState}.


handle_cast(Request, State) ->
    NewState = try 
                    do_handle_cast(Request, State)
               catch _:Reason ->
                            io:format("---we_debugger--catch!!!-----Reason=~w~n",[Reason]),
                            State
                end,   
    {noreply, NewState}.


handle_info(Info, State) ->
    NewState = try
                    do_handle_info(Info, State)
               catch _:Reason ->
                            io:format("---we_debugger--catch!!!-----Reason=~w~n",[Reason]),
                            State
               end,
    {noreply, NewState}.

terminate(Reason, _State) ->
    io:format("---we_debugger--over!!!-----Reason=~w~n",[Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_handle_call({debugger_tree, Path}, State) ->
    FunString = get_source_directory_fun(Path),
    DirList =  common_process:eval(FunString, []),
    DirList2 =  lists:foldl(fun({Type, Name}, Acc) ->
                        FullPath = Path ++ "/" ++ Name,
                        FullPath2 = string:tokens(FullPath, "/"),
                        FullPath3 = string:join(FullPath2, "__"),
                        case Type of
                            d ->
                              Data = [{text,Name},{expanded, false},{classes,important},{hasChildren, true},{id, FullPath3},{type, dir}],
                              [Data|Acc];
                            f ->
                              S = string:right(Name,4),
                              if S =:= ".erl" ->
                                Len = string:len(FullPath3),
                                FullPath4 = string:sub_string(FullPath3, 1, Len - 4),
                                Data = [{text, Name},{id, FullPath4},{type, file}],
                                [Data|Acc];
                              true ->
                                Acc
                              end
                        end
                      end, [], DirList),
    {DirList2, State};
do_handle_call({debugger_file, ModName}, State) ->
    FunString = "os:cmd(\"./cl  "++ ModName ++ "\").",
    DirList = common_process:eval(FunString, []),
    {DirList, State};
do_handle_call({chunks_file, FullPath, ModName}, State) ->
    AppPath = web_common:get_root_dir(),
    {ok, Bin, _F} = erl_prim_loader:get_file(filename:absname(AppPath++"/ebin/"++atom_to_list(ModName)++".beam")),
    Result = int:i({ModName,FullPath,AppPath++"/ebin/"++ atom_to_list(ModName)++".beam", Bin}),
    {Result, State};
    % FunString = "{ok,[[AppPath]]} = init:get_argument(we_root),
    %              {ok, Bin, _F} = erl_prim_loader:get_file(filename:absname(AppPath++\"/ebin/\"++atom_to_list(ModName)++\".beam\")),
    %              int:i({ModName,FullPath,AppPath++\"/ebin/\"++ atom_to_list(ModName)++\".beam\", Bin}).",
    % Binding =  [{'FullPath',FullPath}, {'ModName',ModName}],
    % Result = common_process:eval(FunString, Binding),
    % {Result, State};
do_handle_call({read_file, FullPath}, State) ->
    Result = case file:read_file(FullPath) of
                {ok, FileContent} ->
                    binary_to_list(FileContent);
                {error, Reason} ->
                    Reason
            end,
    {Result, State};
do_handle_call({pid_info, Pid}, State) ->
    Pid2 = list_to_pid(Pid),
    {ok, Meta} = dbg_iserver:call({get_meta, Pid2}),
    try erlang:monitor(process, Meta) of
        Mref ->
            catch erlang:send(Meta, {user, {get, bindings, self(), nostack}}, [noconnect]),
            receive
                {'DOWN', Mref, _, _, noconnection} ->
                    {error, State};
                {'DOWN', Mref, _, _, Reason} ->
                    {Reason, State};
                {Meta, bindings, Bindings} ->
                    erlang:demonitor(Mref, [flush]),
                    Bindings2 = binding_to_record(Bindings, []),
                    {Bindings2, State};
                Result ->
                    erlang:demonitor(Mref, [flush]),
                    {Result, State}
            end
    catch
        error:_ ->
            ignore
    end;                                             
do_handle_call({mod_path, ModName}, State) ->
    % Parent = ?SOURCE_ROOT,
    AppPath = web_common:get_root_dir(),
    Parent = AppPath ++ "/src",
    FullPath = case catch find_file(ModName, Parent) of
                    {ok, MyPath} ->
                        MyPath;
                    none ->
                         ""
               end,
    FullPath2 = filename:rootname(FullPath),
    FullPath3 = string:tokens(FullPath2, "/"),
    FullPath4 = string:join(FullPath3, "__"),
    {FullPath4, State};
do_handle_call({mod_funs, ModName}, State) ->
    FunString = find_mod_fun_str(),
    Result = common_process:eval(FunString, [{'ModName', ModName}]),
    {Result, State};
do_handle_call(Info,State) ->
    io:format("unknow request ~p", [Info]),
    {{error, unknow_request},State}.
 

do_handle_cast({attach_pid, Pid}, State) ->
    Pid2 = list_to_pid(Pid),
    common_process:debugger_attach(self(), Pid2, node()),
    State;
do_handle_cast({step, Pid}, State) ->
    Pid2 = list_to_pid(Pid),
    int:step(Pid2),
    State;
do_handle_cast({next, Pid}, State) ->
    Pid2 = list_to_pid(Pid),
    int:next(Pid2),
    State;
do_handle_cast({finish, Pid}, State) ->
    Pid2 = list_to_pid(Pid),
    int:finish(Pid2),
    State;
do_handle_cast({del_interpreted, Mod}, State) ->
    dbg_iserver:cast({delete, Mod}),
    State;
do_handle_cast(clear, State) ->
    dbg_iserver:cast(clear),
    State;
do_handle_cast(stop, State) ->
    dbg_iserver:cast(stop),
    State;
do_handle_cast(Msg, State) ->
    io:format("unknow cast request ~p", [Msg]),
    State.
     

do_handle_info({{_,{attached,undefined,-1,false}},Pid, Bindings}, State) ->
    Bindings2 = binding_to_record(Bindings, []),
    Msg = {debugger_break, [{pid, Pid},{status, attached}, {bindings, Bindings2},{attached, true}]},
    we_server_pub:notify(Msg),
    State;
do_handle_info({{_,{attached,Mod,Line,false}},Pid,Bindings}, State) ->
    Bindings2 = binding_to_record(Bindings, []),
    Msg = {debugger_break, [{pid, Pid},{status, break}, {bindings, Bindings2},{mod, Mod}, {line, Line},{attached, true}]},
    we_server_pub:notify(Msg),
    State;
do_handle_info({{_, running},Pid, Bindings}, State) ->
    Bindings2 = binding_to_record(Bindings, []),
    Msg = {debugger_break, [{pid, Pid}, {status, running}, {bindings, Bindings2}]},
    we_server_pub:notify(Msg),
    State;
do_handle_info({{_,{re_entry, Mod, Fun}},Pid, Bindings}, State) ->
    Bindings2 = binding_to_record(Bindings, []),
    Msg = {debugger_break, [{pid, Pid}, {status, re_entry}, {mod, Mod}, {func, Fun}, {bindings, Bindings2},{attached, true}]},
    we_server_pub:notify(Msg),
    State;
do_handle_info({{_,{break_at, Mod, Line,_}}, Pid, Bindings}, State) ->

    Bindings2 = binding_to_record(Bindings, []),
    Msg ={debugger_break, [{status, break}, {mod,Mod}, {line, Line}, {pid, Pid},{bindings, Bindings2},{attached, true}]},
    we_server_pub:notify(Msg),
    State;
do_handle_info({attach_pid_down, Pid}, State) ->
    Msg = {debugger_break, [{attached, false}, {pid, Pid}]},
    we_server_pub:notify(Msg),
    State;
do_handle_info(Msg, State) ->
    io:format("unknow info request ~p----~n", [Msg]),
    State.

get_source_directory_fun(Path) ->
    "Path = \"" ++ Path ++ "\",
    DirectoryOrFileList = filelib:wildcard(\"*\", Path),
    lists:foldl(fun(Name, Acc) -> 
        FullPath =  Path ++ \"/\" ++ Name, 
        case filelib:is_dir(FullPath) of 
            true -> [{d,Name}|Acc]; 
            false -> [{f, Name}|Acc] 
        end 
    end, [], DirectoryOrFileList).".



binding_to_record([], Acc) ->
    lists:reverse(Acc);
binding_to_record([{Key, Value}|BindingList], Acc) ->
    Binds = if  is_integer(Value) orelse is_atom(Value) orelse is_function(Value) orelse is_pid(Value) orelse Value=:= [] -> 
                    {Key, Value};
                is_tuple(Value) ->
                    % {Key, we_shell:record(Value)};
                    % io:format("~n~w=~w~n----------~n", [Key,Value]),
                    {Key, Value};
                true ->
                    % io:format("~n~w=~w~n----~n", [Key,Value]),
                    % {Key, we_shell:record(Value)}
                    {Key, Value}
            end,
    binding_to_record(BindingList, [Binds|Acc]).


find_file(ModName, Parent) -> 
    FileName =
    case filename:extension(ModName) of
        "" ->
            ModName ++ ".erl";
        ".erl" ->
            ModName
    end,
    Root = filename:dirname(Parent),
    [begin
        case filelib:wildcard(FindPath) of
            [] ->
                ok;
            [File] ->
                throw({ok, File})
        end
    end || FindPath <- 
        [
        filename:join([Root, "src", FileName]),
        filename:join([Root, "src", "*", FileName]),
        filename:join([Root, "src", "*", "*", FileName]),
        filename:join([Root, "src", "*", "*", "*", FileName])
        ]
    ],
    none.

find_file_str() ->
    "FileName =
    case filename:extension(Mod) of
        \"\" ->
            Mod ++ \".erl\";
        \".erl\" ->
            Mod
    end,
    Root = filename:dirname(Path),
    [begin
        case filelib:wildcard(FindPath) of
            [] ->
                ok;
            [File] ->
                throw({ok, File})
        end
    end || FindPath <- 
    [
    filename:join([Root, \"src\", FileName]),
    filename:join([Root, \"src\", \"*\", FileName]),
    filename:join([Root, \"src\", \"*\", \"*\", FileName]),
    filename:join([Root, \"src\", \"*\", \"*\", \"*\", FileName]),
    filename:join([Root, \"update\", FileName])
    ]],
    none. ".  


find_mod_fun_str() ->
    "File = code:which(ModName),
    case beam_lib:chunks(File,[abstract_code,\"CInf\"]) of
        {ok, {_Mod,[{abstract_code, {Version, Forms}}, {\"CInf\", _CB}]}} ->
            FunsSttr = [{FunName, Line, Args} ||  {function, Line, FunName, Args, _ } <- Forms],
            case FunsSttr of
                [] when Version =:= raw_abstract_v1 ->
                    [];
                [] -> [];
                Funs -> Funs
            end;
        {ok, {_Mod, [{abstract_code, no_abstract_code},{\"CInf\", _CB}]}} ->
            no_abstract_code;
        Error ->
            Error
    end.".

