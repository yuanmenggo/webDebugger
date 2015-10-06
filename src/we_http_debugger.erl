-module(we_http_debugger).
-behaviour(cowboy_http_handler).
-include("common.hrl").
-export([init/3, handle/2, terminate/2]).

init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined_state}.

handle(Req0, State) ->
    {Path, Req} = cowboy_http_req:path(Req0),
    {Method, Req1} = cowboy_http_req:method(Req),
    handle_path(Method, Path, Req1, State).
handle_path('GET', [<<"debugger">>,<<"tree">>], Req, State) ->
    AppDir = web_common:get_root_dir(),
    Parent = AppDir++"/src",
    {Data, _} = cowboy_http_req:raw_qs(Req),
    [<<"root">>, Value]= binary:split(Data, <<"=">>),
    Path = case Value of
                  <<"source">> ->
                         Parent;
                  _ ->                        
                        Value2 = binary:replace(Value,<<"__">>,<<"/">>,[global]),
                        Value3 = binary_to_list(Value2),
                        "/"++Value3 ++ "/"                      
            end,
    Info = we_debugger:request_debugger_data({debugger_tree, Path}),
    json_response(Info, Req, State);
handle_path('GET', [<<"debugger">>, <<"start">>, <<"snapshot">>], Req, State) ->
    Info = we_timer:cast_debugger_data(start_snapshot),
    json_response(Info, Req, State);
handle_path('GET', [<<"debugger">>, <<"stop">>, <<"snapshot">>], Req, State) ->
    Info = we_timer:cast_debugger_data(stop_snapshot),
    json_response(Info, Req, State);
handle_path('GET', [<<"debugger">>,<<"break">>, <<"pid_info">>, Pid], Req, State) ->
    Pid2 = binary_to_list(Pid),
    Info = we_debugger:request_debugger_data({pid_info, Pid2}),
    json_response(Info, Req, State);
handle_path('GET', [<<"debugger">>,<<"attach">>, <<"pid_info">>, Pid], Req, State) ->
    Pid2 = binary_to_list(Pid),
    Info = we_debugger:cast_debugger_data({attach_pid, Pid2}),
    json_response(Info, Req, State);
handle_path('GET',[<<"debugger">>,<<"step">>, Pid], Req, State) ->
    Pid2 = binary_to_list(Pid),
    Info = we_debugger:cast_debugger_data({step, Pid2}),
    json_response(Info, Req, State);
handle_path('GET',[<<"debugger">>,<<"next">>, Pid], Req, State) ->
    Pid2 = binary_to_list(Pid),
    Info = we_debugger:cast_debugger_data({next, Pid2}),
    json_response(Info, Req, State);
handle_path('GET',[<<"debugger">>,<<"finish">>, Pid], Req, State) ->
    Pid2 = binary_to_list(Pid),
    Info = we_debugger:cast_debugger_data({finish, Pid2}),
    json_response(Info, Req, State);
handle_path('GET', [<<"debugger">>,<<"file">>,FullPath], Req, State) ->
    FullPath2 = binary:replace(FullPath,<<"__">>,<<"/">>,[global]),
    FullPath3 = binary_to_list(FullPath2), 
    ModName = lists:last(string:tokens(FullPath3,"/")),
    Info = we_debugger:request_debugger_data({debugger_file, ModName}),
    json_response(Info, Req, State);
handle_path('GET', [<<"debugger">>,<<"chunks">>,FullPath], Req, State) ->
    FullPath2 = binary:replace(FullPath,<<"__">>,<<"/">>,[global]),
    FullPath3 = binary_to_list(FullPath2),
    FullPath4 = "/"++FullPath3++".erl" ,
    ModName = we_common:list_to_atom(lists:last(string:tokens(FullPath3,"/"))),   
    Info = we_debugger:request_debugger_data({chunks_file, FullPath4, ModName}),
    json_response(Info, Req, State);
handle_path('GET',[<<"debugger">>,<<"del">>,<<"interprered">>, Mod], Req, State) ->
    Mod2 = we_common:list_to_atom(binary_to_list(Mod)),
    Info = we_debugger:cast_debugger_data({del_interpreted, Mod2}),
    json_response(Info, Req, State);
handle_path('GET',[<<"debugger">>,<<"stop">>], Req, State) ->
    Info = we_debugger:cast_debugger_data(stop),
    json_response(Info, Req, State);
handle_path('GET',[<<"debugger">>,<<"clear">>], Req, State) ->
    Info = we_debugger:cast_debugger_data(clear),
    json_response(Info, Req, State);
handle_path('GET', [<<"debugger">>,<<"read">>,FullPath], Req, State) ->
    FullPath2 = binary:replace(FullPath,<<"__">>,<<"/">>,[global]),
    FullPath3 = binary_to_list(FullPath2),
    FullPath4 = "/"++FullPath3++".erl" , 
    Info = we_debugger:request_debugger_data({read_file, FullPath4}),
    json_response(Info, Req, State);
handle_path('GET', [<<"debugger">>,<<"break">>,<<"del">>,ModName, Line], Req, State) ->
    ModName2 = binary_to_list(ModName),
    Line2 = binary_to_list(Line),   
    Info = we_break:request_debugger_data({del_mod_break, ModName2, Line2}),
    json_response(Info, Req, State);
handle_path('GET', [<<"debugger">>,<<"break">>,ModName, Line, Flag], Req, State) ->
    ModName2 = binary_to_list(ModName),
    Line2 = binary_to_list(Line),   
    Flag2 = binary_to_list(Flag),
    Info = we_break:request_debugger_data({mod_break, ModName2, Line2, Flag2}),
    json_response(Info, Req, State);
handle_path('GET',[<<"debugger">>,<<"breaks">>], Req, State) ->
    Info = we_break:request_debugger_data(mod_breaks),
    json_response(Info, Req, State);
handle_path('GET', [<<"debugger">>,<<"breaks">>,ModName], Req, State) ->
    ModName2 = binary_to_list(ModName),
    Info = we_break:request_debugger_data({mod_breaks, ModName2}),
    json_response(Info, Req, State);
handle_path('GET', [<<"debugger">>,<<"path">>,ModName], Req, State) ->
    ModName2 = binary_to_list(ModName),  
    Info = we_debugger:request_debugger_data({mod_path, ModName2}),
    json_response(Info, Req, State);
handle_path('GET', [<<"debugger">>,<<"funs">>,ModName], Req, State) ->
    ModName2 = we_common:list_to_atom(binary_to_list(ModName)),  
    Info = we_debugger:request_debugger_data({mod_funs, ModName2}),
    json_response(Info, Req, State);
handle_path(_, Path, Req, State) ->
    io:format('not_found http path=~w ~n', [Path]),
    not_found(Req, State).

not_found(Req, State) ->
    {ok, Req2} = cowboy_http_req:reply(404, [], <<"<h1>404</h1>">>, Req),
    {ok, Req2, State}.

terminate(_Req, _State) ->
    ok.





json_response(Info, Req, State) ->
    Body = jsx:term_to_json(Info),
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    {ok, Req2} = cowboy_http_req:reply(200, Headers, Body, Req),
    {ok, Req2, State}.
