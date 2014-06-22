{application, weberl, 
[{description, "Small web erlang web framework"},
 {modules, [kernel, stdlib, weberl, weberl_serv, weberl_utils, weberl_routes, weberl_views]},
 {env, [{port, 8001}]},
 {mod, {weberl, []}}
]}.
