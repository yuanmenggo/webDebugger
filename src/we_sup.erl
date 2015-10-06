%%%-------------------------------------------------------------------
%%% Created :  2 Feb 2013 by yuanmenggo,shoumuyushan
%%%-------------------------------------------------------------------
-module(we_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    SocketPub = ?CHILD(we_server_pub, worker),
    Debugger = ?CHILD(we_debugger, worker),
    Timer = ?CHILD(we_timer, worker),
    Break = ?CHILD(we_break, worker),
    Http = ?CHILD(we_http, worker),
    Specs = [SocketPub, Debugger, Timer, Break, Http],
    {ok, {{one_for_one, 5, 10}, Specs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
