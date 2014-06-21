-module(weberl_views).
-export([serve_files/2]).

-include("weberl.hrl").

serve_files(BaseDir, FilePath) ->
	case file:read_file(lists:concat([BaseDir, FilePath])) of
		{ok, Content} -> #response{content=Content};
		{error, enoent} -> #response{status_code=404,
		                             content="404 Not Found"}
	end.
