-module(weberl_routes).
-export([route/1]).

-include("weberl.hrl").

route(Rq) ->
	case Rq#request.url of
		<<"/">> -> #response{content="Coucou"};
		<<"/static", Path/binary>> -> weberl_views:serve_files("./static", bitstring_to_list(Path));
		<<"/blog", Path/binary>> -> weberl_views:serve_md("./blog", bitstring_to_list(Path))
	end.

