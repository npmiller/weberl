-module(weberl_views).
-export([serve_dir/2, serve_file/1, serve_md/2]).

-include("weberl.hrl").
-include_lib("kernel/include/file.hrl").

-define(CACHE_SUFFIX, "-cache").
-define(BASE_TPL, "/base.html").

serve_dir(Dir, R) ->
	case filelib:is_dir([Dir, R#request.url]) of
		true -> serve_index(R#request{url=[Dir, R#request.url]});
		false -> serve_file(R#request{url=[Dir, R#request.url]})
	end.

serve_index(R) ->
	case file:list_dir(R#request.url) of
		{ok, L} ->
			#response{content=L};
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
			serve_index(R#request{url=[Dir, R#request.url]});
		render ->
			render_md([Dir, R#request.url, ".md"], [Dir, ?CACHE_SUFFIX, R#request.url, ".html"]),
			#response{content=render_content_template([Dir, ?BASE_TPL], [Dir, ?CACHE_SUFFIX, R#request.url, ".html"])};
		ok     -> 
			#response{content=render_content_template([Dir, ?BASE_TPL], [Dir, ?CACHE_SUFFIX, R#request.url, ".html"])}
	end.

render_md(MdPath, HtmlPath) ->
	filelib:ensure_dir(HtmlPath),
	os:cmd(["pandoc -o ", HtmlPath, " ", MdPath]).

render_content_template(TemplatePath, ContentPath) ->
	{ok, Template} = file:read_file(TemplatePath),
	{ok, Content} = file:read_file(ContentPath),
	[Top, Bottom] = binary:split(Template, <<"-content-">>),
	[Top, Content, Bottom].

check_cache(Dir, Path) ->
	case {filelib:last_modified([Dir, Path, ".md"]), filelib:last_modified([Dir, ?CACHE_SUFFIX, Path, ".html"])} of
		{0, _}            -> error;
		{_, 0}            -> render;
		{Md, Html} when Md =< Html -> ok;
		{Md, Html} when Md > Html -> render
	end.
