%%%-------------------------------------------------------------------
%%% Created :  2 Feb 2013 by yuanmenggo,shoumuyushan
%%%-------------------------------------------------------------------
-module(we_stream_web).
-behaviour(cowboy_http_handler).
-behaviour(cowboy_http_websocket_handler).
-export([init/3, handle/2, terminate/2, terminate/3]).
-export([websocket_init/3, websocket_handle/3, websocket_terminate/3,
				 websocket_info/3]).


init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_http_websocket}.

handle(_Req, _State) ->
    exit(websockets_only).

terminate(_Req, _State) ->
    exit(websockets_only).

terminate(_Reason, _Req, _State) ->
    exit(websockets_only).

websocket_init(_TransportName, Req, _Opts) ->
    {PeerAddr, _} = cowboy_http_req:peer_addr(Req),
    we_server_pub:register_tpl_client({PeerAddr, self()}),
    {ok, Req, undefined_state}.

websocket_info({we, {DataTag, DataList}}, Req, State) ->   
    Reply = jsx:term_to_json([{DataTag, DataList}]),
    {reply, {text, Reply}, Req, State};

websocket_info({we, _}, Req, State) ->
    {ok, Req, State};

websocket_info(Info, Req, State) ->
    io:format("Unhandled msg to ~p ~p\n", [?MODULE, Info]),
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) -> 
    io:format('---web--stream---terminate--self()=~w-~n',[self()]),
    ok.

websocket_handle(_Msg, Req, State) ->
	{ok, Req, State}.
