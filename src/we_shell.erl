-module(we_shell).
-compile(export_all).

-define(config(A,B),config(A,B)).
-record(state, {bin, reply, leader}).


config(priv_dir,_) ->
    ".".
record(Data) ->
    t(Data).


t({Node,Bin}) when is_atom(Node),is_binary(Bin) ->
    {Shell, Result} = t0(Bin, fun() -> start_new_shell(Node) end),
    exit(Shell, normal),
    Result;
t(Bin) when is_binary(Bin) ->
    {Shell, Result} = t0(Bin, fun() -> start_new_shell() end),
    exit(Shell, normal),
    Result;
t(Tuple) when is_tuple(Tuple) orelse Tuple =:=undefined ->
    List = "rp("++lists:flatten(io_lib:format("~w",[Tuple]))++").",
    Bin = list_to_binary(List),
    {Shell, Result} = t0(Bin, fun() -> start_new_shell() end),
    exit(Shell, normal),
    Result;
t([]) ->
    <<"[]">>;
t(List) when is_list(List) ->
    List2 = list_to_tuple(List),
    t(List2);
    %t("rp("++lists:flatten(io_lib:format("~w",[List2]))++").");
t(Any) ->
    Any.


t0(Bin, F) ->
    %% Spawn a process so that io_request messages do not interfer.
    P = self(),
    C = spawn(fun() -> t1(P, Bin, F) end),
    receive {C, R} 
            -> {C, R}
    after 2000 ->
            {C, Bin}
    end.

t1(Parent, Bin, F) ->
    %% io:format("*** Testing ~s~n", [binary_to_list(Bin)]),
    S = #state{bin = Bin, reply = [], leader = group_leader()},
    group_leader(self(), self()),
    _Shell = F(),
    try 
        server_loop(S)
    catch exit:R -> Parent ! {self(), R};
          throw:{?MODULE,LoopReply} ->
                   L0 = binary_to_list(list_to_binary(LoopReply)),
                   [$\n | L1] = lists:dropwhile(fun(X) -> X =/= $\n end, L0),
                   Parent ! {self(), dotify(L1)}
    after group_leader(S#state.leader, self())
    end.

start_new_shell() ->
    process_flag(trap_exit, true),
    Shell = shell:start(),
    link(Shell),
    Shell.

start_new_shell(Node) ->
    process_flag(trap_exit, true),
    Shell = rpc:call(Node,shell,start,[]),
    link(Shell),
    Shell.


server_loop(S) ->
    receive 
        {io_request, From, ReplyAs, Request} when is_pid(From) ->
	    server_loop(do_io_request(Request, From, S, ReplyAs));
	NotExpected ->
            exit(NotExpected)
    end.
            
do_io_request(Req, From, S, ReplyAs) ->
    case io_requests([Req], [], S) of
        {_Status,{eof,_},S1} ->
	    io_reply(From, ReplyAs, {error,terminated}),
	    throw({?MODULE,S1#state.reply});
	{_Status,Reply,S1} ->
	    io_reply(From, ReplyAs, Reply),
	    S1
    end.

io_reply(From, ReplyAs, Reply) ->
    From ! {io_reply, ReplyAs, Reply}.

io_requests([{requests, Rs1} | Rs], Cont, S) ->
    io_requests(Rs1, [Rs | Cont], S);
io_requests([R | Rs], Cont, S) ->
    case io_request(R, S) of
        {ok, ok, S1} ->
            io_requests(Rs, Cont, S1);
        Reply ->
            Reply
    end;
io_requests([], [Rs|Cont], S) ->
    io_requests(Rs, Cont, S);
io_requests([], [], S) -> 
    {ok,ok,S}.

io_request({get_geometry,columns}, S) ->
    {ok,80,S};
io_request({get_geometry,rows}, S) ->
    {ok,24,S};
io_request({put_chars,Chars}, S) ->
    {ok,ok,S#state{reply = [S#state.reply | Chars]}};
io_request({put_chars,_,Chars}, S) ->
    {ok,ok,S#state{reply = [S#state.reply | Chars]}};
io_request({put_chars,Mod,Func,Args}, S) ->
    case catch apply(Mod, Func, Args) of
        Chars when is_list(Chars) -> 
            io_request({put_chars,Chars}, S)
    end;
io_request({put_chars,Enc,Mod,Func,Args}, S) ->
    case catch apply(Mod, Func, Args) of
        Chars when is_list(Chars) -> 
            io_request({put_chars,Enc,Chars}, S)
    end;
io_request({get_until,_Prompt,Mod,Func,ExtraArgs}, S) ->
    get_until(Mod, Func, ExtraArgs, S, latin1);
io_request({get_until,Enc,_Prompt,Mod,Func,ExtraArgs}, S) ->
    get_until(Mod, Func, ExtraArgs, S, Enc).

get_until(Mod, Func, ExtraArgs, S, Enc) ->
    get_until_loop(Mod, Func, ExtraArgs, S, {more,[]}, Enc).

get_until_loop(M, F, As, S, {more,Cont}, Enc) ->
    Bin = S#state.bin,
    case byte_size(Bin) of
        0 ->
            get_until_loop(M, F, As, S, 
                           catch apply(M, F, [Cont,eof|As]), Enc);
	_ ->
	    get_until_loop(M, F, As, S#state{bin = <<>>},
			   catch apply(M, F, [Cont,binary_to_list(Bin)|As]), Enc)
    end;
get_until_loop(_M, _F, _As, S, {done,Res,Buf}, Enc) ->
    {ok,Res,S#state{bin = buf2bin(Buf, Enc)}};
get_until_loop(_M, F, _As, S, _Other, _Enc) ->
    {error,{error,F},S}.

buf2bin(eof,_) ->
    <<>>;
buf2bin(Buf,latin1) ->
    list_to_binary(Buf);
buf2bin(Buf,unicode) ->
    unicode:characters_to_binary(Buf,unicode,unicode).

run_file(Config, Module, Test) ->
    FileName = filename(lists:concat([Module, ".erl"]), Config),
    BeamFile = filename(lists:concat([Module, ".beam"]), Config),
    LoadBeamFile = filename(Module, Config),
    ok = file:write_file(FileName, Test),
    ok = compile_file(Config, FileName, Test, []),
    code:purge(Module),
    {module, Module} = code:load_abs(LoadBeamFile), 
    ok = Module:t(),
    file:delete(FileName),
    file:delete(BeamFile),
    ok.

filename(Name, Config) when is_atom(Name) ->
    filename(atom_to_list(Name), Config);
filename(Name, Config) ->
    filename:join(?config(priv_dir, Config), Name).

compile_file(Config, File, Test, Opts0) ->
    Opts = [export_all,return,{outdir,?config(priv_dir, Config)}|Opts0],
    ok = file:write_file(File, Test),
    case compile:file(File, Opts) of
              {ok, _M, _Ws} -> ok;
              _ -> error
          end.

dotify([$., $\n | L]) ->
    [$., $\n | dotify(L)];
dotify([$,, $\n | L]) ->
    [$,, $\n | dotify(L)];
dotify("ok\n" ++ L) ->
    "ok.\n" ++ dotify(L);
dotify("\nok\n" ++ L) ->
    ".\nok.\n" ++ dotify(L);
dotify([$\n]) ->
    [$., $\n];
dotify([C | L]) ->
    [C | dotify(L)];
dotify([]) ->
    [].


scan(B) ->
    F = fun(Ts) -> 
                case erl_parse:parse_term(Ts) of
                    {ok,Term} ->
                        Term;
                    _Error ->
                        {ok,Form} = erl_parse:parse_form(Ts),
                        Form
                end
        end,
    scan(t(B), F).

scan(S0, F) ->
    case erl_scan:tokens([], S0, 1) of
        {done,{ok,Ts,_},S} ->
            [F(Ts) | scan(S, F)];
        _Else ->
            []
    end.
