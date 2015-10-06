%%%-------------------------------------------------------------------
%%% Created :  2 Feb 2013 by yuanmenggo,shoumuyushan
%%%-------------------------------------------------------------------
-module(we_http_catchall).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined_state}.

handle(Req, State) ->
    Body = <<"<h1>404</h1>">>,
    {ok, Req2} = cowboy_http_req:reply(404, [], Body, Req),
    {ok, Req2, State}.

terminate(_Req, _State) ->
    ok.
