-module(weberl_routes).
-export([route/1]).

-include("weberl.hrl").

route(Rq) ->
	case Rq#request.url of
		<<"/">> ->
			weberl_views:serve_files("./blog", "/home.html");
		<<"/static", Path/binary>> ->
			weberl_views:serve_files("./static", bitstring_to_list(Path));
		<<"/raw", Path/binary>> ->
			weberl_views:serve_files("./blog", [bitstring_to_list(Path), ".md"]);
		<<Path/binary>> ->
			weberl_views:serve_md("./blog", bitstring_to_list(Path));
		_ ->
			#response{status_code=404}
	end.

