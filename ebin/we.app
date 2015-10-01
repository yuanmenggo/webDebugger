{application,we,
             [{description,"web-based suite of tools for interrogating an erlang game system"},
              {vsn,"0.1"},
              {registered,[]},
              {applications,[kernel,stdlib,cowboy,jsx]},
              {mod,{we_app,[]}},
              {env,[]},
              {modules,[common_process,hasdefault_pb,pokemon_pb,protobuffs,
                        protobuffs_compile,protobuffs_parser,user_default,we,
                        we_app,we_break,we_common,we_debugger,we_http,
                        we_http_catchall,we_http_debugger,we_http_static,
                        we_server_pub,we_shell,we_stream_web,we_sup,we_timer,
                        ymg,ymg_app,ymg_appmon,ymg_appmon_info,ymg_break,
                        ymg_common,ymg_db_collector,ymg_debugger,
                        ymg_debugger_server,ymg_http,ymg_http_appmon,
                        ymg_http_catchall,ymg_http_db,ymg_http_debugger,
                        ymg_http_etop,ymg_http_map,ymg_http_pid,ymg_http_role,
                        ymg_http_static,ymg_http_web,ymg_map_collector,
                        ymg_map_loader,ymg_map_server,ymg_map_supervisor,
                        ymg_role_collector,ymg_role_server,
                        ymg_role_supervisor,ymg_server_pub,ymg_shell,
                        ymg_stats_sender,ymg_stream_map,ymg_stream_role,
                        ymg_stream_web,ymg_sup,ymg_timer]}]}.