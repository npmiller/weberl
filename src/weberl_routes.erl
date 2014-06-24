-module(weberl_routes).
-export([route/1]).

-include("weberl.hrl").

route(Rq) ->
	case Rq#request.url of
		"/" ->
			weberl_views:serve_file(Rq#request{url="./blog/home.html"});
		"/static" ++ Path ->
			weberl_views:serve_dir("./static", Rq#request{url=Path});
		"/raw" ++ Path ->
			weberl_views:serve_dir("./blog", [Path, ".md"]);
		_Path ->
			weberl_views:serve_md("./blog", Rq)
	end.

