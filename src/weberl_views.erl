-module(weberl_views).
-export([serve_dir/2, serve_file/1, serve_md/2]).

-include("weberl.hrl").
-include_lib("kernel/include/file.hrl").

-define(CACHE_SUFFIX, "-cache").
-define(BASE_TPL, "/base.html").
-define(INDEX_TPL, "index.html").

serve_dir(Dir, R) ->
	case filelib:is_dir([Dir, R#request.url]) of
		true -> serve_index(R#request{url=[Dir, R#request.url]});
		false -> serve_file(R#request{url=[Dir, R#request.url]})
	end.

serve_index(R) ->
	case file:list_dir(R#request.url) of
		{ok, L} ->
			{ok, Template} = file:read_file("index.html"),
			#response{content=weberl_tpl:render_template(Template, [{'List', L}, {'Title', R#request.url}])};
		{error, _} -> 
			#response{status_code=404}
	end.

serve_file(R) ->
	case file:read_file(R#request.url) of
		{ok, Content} ->
			#response{content=Content, content_type=weberl_utils:get_content_type(filename:extension(R#request.url))};
		{error, _} ->
			#response{status_code=404}
	end.

serve_md(Dir, R) ->
	case check_cache(Dir, R#request.url) of
		error  ->
			serve_pretty_index(Dir, R);
		render ->
			render_md([Dir, R#request.url, ".md"], [Dir, ?CACHE_SUFFIX, R#request.url, ".html"]),
			{ok, Template} = file:read_file([Dir, ?BASE_TPL]),
			{ok, Content} = file:read_file([Dir, ?CACHE_SUFFIX, R#request.url, ".html"]),
			#response{content=weberl_tpl:render_template(Template, [{'Content', Content}, {'Title', R#request.url}])};
		ok     -> 
			{ok, Template} = file:read_file([Dir, ?BASE_TPL]),
			{ok, Content} = file:read_file([Dir, ?CACHE_SUFFIX, R#request.url, ".html"]),
			#response{content=weberl_tpl:render_template(Template, [{'Content', Content}, {'Title', R#request.url}])}
	end.

serve_pretty_index(Dir, R) ->
	case file:list_dir([Dir, R#request.url]) of
		{ok, L} ->
			{ok, Template} = file:read_file("index.html"),
			#response{content=weberl_tpl:render_template(Template, [
			                                                        {'List', build_entry_list(Dir, R, L)}, 
			                                                        {'Title', R#request.url}
			                                                       ])};
		{error, _} -> 
			#response{status_code=404}
	end.

build_entry_list(Dir, R, L) ->
	F = lists:filter(fun(E) -> Ext = filename:extension(E), if Ext == ".md" -> true; true -> false end end, L),
	lists:map(fun(E) -> 
					  {ok, IoDevice} = file:open([Dir, R#request.url, E], [read]),
					  {ok, Line} = file:read_line(IoDevice), {Line, [R#request.url, filename:rootname(E)]}
			  end, F).

render_md(MdPath, HtmlPath) ->
	filelib:ensure_dir(HtmlPath),
	os:cmd(["pandoc -o ", HtmlPath, " ", MdPath]).

check_cache(Dir, Path) ->
	case {filelib:last_modified([Dir, Path, ".md"]), filelib:last_modified([Dir, ?CACHE_SUFFIX, Path, ".html"])} of
		{0, _}            -> error;
		{_, 0}            -> render;
		{Md, Html} when Md =< Html -> ok;
		{Md, Html} when Md > Html -> render
	end.
