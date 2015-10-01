%%%-------------------------------------------------------------------
%%% @author Guoyuanhua <>
%%% @copyright (C) 2013, Guoyuanhua
%%% @doc
%%%
%%% @end
%%% Created :  9 Feb 2013 by Guoyuanhua <>
%%%-------------------------------------------------------------------
-module(we_server_pub).

-behaviour(gen_server).
-compile(export_all).
%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {listeners=[], tpl_listeners=[], role_listeners=[]}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

notify(Msg) ->
    gen_server:cast(?SERVER, {notify, Msg}).

unregister_client(PeerAddr) ->
    gen_server:cast(?SERVER, {unregister_client, PeerAddr}).

unregister_tpl_client(Pid) ->
    gen_server:cast(?SERVER, {unregister_tpl_client, Pid}).

register_client({PeerAddr, Pid, MapID}) ->
    gen_server:call(?SERVER, {register_client, PeerAddr, Pid, MapID}).

register_tpl_client({PeerAddr, Pid}) ->
    gen_server:call(?SERVER, {register_tpl_client, PeerAddr, Pid}).

find_client(PeerAddr, MapID) ->
    gen_server:call(?SERVER, {find_client, PeerAddr, MapID}).

find_tpl_client(PeerAddr) ->
    gen_server:call(?SERVER, {find_tpl_client, PeerAddr}).


init([]) ->
    {ok, #state{}}.

handle_call({register_client, PeerAddr, Pid, MapID}, _From, State) ->
    NewListeners = [{PeerAddr,Pid, MapID}|State#state.listeners],
    erlang:monitor(process, Pid),
    {reply, ok, State#state{listeners = NewListeners}};

handle_call({register_tpl_client, PeerAddr, Pid}, _From, State) ->
    NewListeners = [{PeerAddr,Pid}|State#state.tpl_listeners],
    erlang:monitor(process, Pid),
    {reply, ok, State#state{tpl_listeners = NewListeners}};
handle_call({find_client, PeerAddr, MapID}, _From, State) ->
    Result = case lists:any(fun({ClientPeerAddr, _, ClientMapID}) ->
                                    PeerAddr=:= ClientPeerAddr andalso ClientMapID =:= MapID
                            end, State#state.listeners) of              
                false ->
                    no_exist;
                true ->
                    exist
            end,
    {reply, Result, State};
handle_call({find_tpl_client, PeerAddr}, _From, State) ->
    Result = case lists:keymember(PeerAddr, 1, State#state.tpl_listeners) of              
                false ->
                    no_exist;
                true ->
                    exist
            end,
    {reply, Result, State};
handle_call(Request, _From, State) ->
    io:format('unknown Request=~w---~n', [Request]),
    Reply = ok,
    {reply, Reply, State}.

handle_cast({unregister_client, PeerAddr}, State) ->
    NewListeners = lists:keydelete(PeerAddr, 1, State#state.listeners),
    {noreply, State#state{listeners = NewListeners}};

handle_cast({unregister_tpl_client, Pid}, State) ->
    NewListeners = lists:keydelete(Pid, 2, State#state.tpl_listeners),
    {noreply, State#state{listeners = NewListeners}};

handle_cast({notify, {Tag, Msg}}, State) ->
    %io:format("--tpl_listeners=~w---tag=~w-~n",[State#state.tpl_listeners, Tag]),
    [P! {we, {Tag, Msg}}|| {_,P} <- State#state.tpl_listeners],
    {noreply, State};
handle_cast({notify, {Tag, MapID, Msg}}, State) ->
    lists:foreach(fun({_, P, ClientMapID})->
                        case ClientMapID =:= MapID of
                                true ->
                                     P! {we, {Tag, Msg}}; 
                                false ->
                                     ignore
                        end
                   end, State#state.listeners),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _MonitorRef, process, Pid, Reason}, State)->
    io:format('-~n--client--stream--down=~w----Reason=~w~n',[Pid, Reason]),
     NewTplListeners = lists:keydelete(Pid, 2, State#state.tpl_listeners),
     NewListeners = lists:keydelete(Pid, 2, State#state.listeners),
     NewRoleListeners = lists:keydelete(Pid, 2, State#state.role_listeners),
    {noreply, State#state{listeners = NewListeners, tpl_listeners =NewTplListeners, role_listeners = NewRoleListeners}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
