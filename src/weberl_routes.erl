-module(weberl_routes).
-export([route/1]).

-include("weberl.hrl").

route(Rq) ->
	case Rq#request.url of
		"/static" ->
			weberl_views:serve_dir("./static/", Rq, "");
		"/static/" ++ Path ->
			weberl_views:serve_dir("./static/", Rq, Path);
		"/raw" ->
			weberl_views:serve_dir("./blog/", Rq, "");
		"/raw/" ++ Path ->
			weberl_views:serve_dir("./blog/", Rq, [Path, ".md"]);
		Path ->
			weberl_views:serve_md("./blog/", Rq, Path)
	end.

