-module(weberl_routes).
-export([route/1]).

-include("weberl.hrl").

route(Rq) ->
	case Rq#request.url of
		<<"/">> -> #response{content="Coucou"};
		<<"/file", Path/binary>> -> weberl_views:serve_files(".", bitstring_to_list(Path))
	end.

