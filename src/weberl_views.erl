-module(weberl_views).
-export([serve_dir/3, serve_file/3, serve_md/3]).

-include("weberl.hrl").
-include_lib("kernel/include/file.hrl").

-define(CACHE_DIR, "./blog-cache/").
-define(BASE_TPL, "base.html").
-define(INDEX_TPL, "index.html").

serve_dir(Dir, R, Path) ->
	case filelib:is_dir([Dir, Path]) of
		true -> serve_index(Dir, R, Path);
		false -> serve_file(Dir, R, Path)
	end.

serve_index(Dir, R, Path) ->
	case file:list_dir([Dir, Path]) of
		{ok, L} ->
			{ok, Template} = file:read_file("index.html"),
			Lt = lists:map(fun(E) -> {E, [R#request.url, "/", E]} end, L),
			#response{content=weberl_tpl:render_template(Template, [{'List', Lt}, {'Title', Path}])};
		{error, _} -> 
			#response{status_code=404}
	end.

serve_file(Dir, _R, Path) ->
	case file:read_file([Dir, Path]) of
		{ok, Content} ->
			#response{content=Content, content_type=weberl_utils:get_content_type(filename:extension(Path))};
		{error, _} ->
			#response{status_code=404}
	end.

serve_md(Dir, R, Path) ->
	case check_cache(Dir, Path) of
		error  ->
			serve_pretty_index(Dir, R, Path);
		render ->
			render_md([Dir, Path, ".md"], [?CACHE_DIR, Path, ".html"]),
			{ok, Template} = file:read_file([Dir, ?BASE_TPL]),
			{ok, Content} = file:read_file([?CACHE_DIR, Path, ".html"]),
			#response{content=weberl_tpl:render_template(Template, [{'Content', Content}, {'Title', R#request.url}])};
		ok     -> 
			{ok, Template} = file:read_file([Dir, ?BASE_TPL]),
			{ok, Content} = file:read_file([?CACHE_DIR, Path, ".html"]),
			#response{content=weberl_tpl:render_template(Template, [{'Content', Content}, {'Title', R#request.url}])}
	end.

serve_pretty_index(Dir, R, Path) ->
	{ok, Template} = file:read_file("index.html"),
	#response{content=weberl_tpl:render_template(Template, [
	                                                        {'List', build_entry_list(Dir, filelib:wildcard([Dir, Path, "/**/*.md"]))},
	                                                        {'Title', R#request.url}
	                                                       ])}.

build_entry_list(Dir, L) ->
	lists:map(fun(E) -> 
					  {ok, IoDevice} = file:open(E, [read]),
					  Url = string:substr(filename:rootname(E), string:len(Dir) + 1),
					  {ok, Line} = file:read_line(IoDevice), {Line, Url}
			  end, L).

render_md(MdPath, HtmlPath) ->
	filelib:ensure_dir(HtmlPath),
	os:cmd(["pandoc -o ", HtmlPath, " ", MdPath]).

check_cache(Dir, Path) ->
	case {filelib:last_modified([Dir, Path, ".md"]), filelib:last_modified([?CACHE_DIR, Path, ".html"])} of
		{0, _}            -> error;
		{_, 0}            -> render;
		{Md, Html} when Md =< Html -> ok;
		{Md, Html} when Md > Html -> render
	end.
