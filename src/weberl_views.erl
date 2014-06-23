-module(weberl_views).
-export([serve_files/2, serve_md/2]).

-include("weberl.hrl").
-include_lib("kernel/include/file.hrl").

-define(CACHE_SUFFIX, "-cache").

serve_files(BaseDir, FilePath) ->
	case file:read_file([BaseDir, FilePath]) of
		{ok, Content} -> 
			#response{content=Content, content_type=weberl_utils:get_content_type(filename:extension(FilePath))};
		{error, _} -> 
			#response{status_code=404}
	end.

serve_md(BaseDir, FilePath) ->
	case check_cache(BaseDir, FilePath) of
		error  -> 
			#response{status_code=404};
		render ->
			render_md([BaseDir, FilePath, ".md"], [BaseDir, ?CACHE_SUFFIX, FilePath, ".html"]),
			#response{content=render_content_template([BaseDir, "/base.html"], [BaseDir, ?CACHE_SUFFIX, FilePath, ".html"])}; 
		ok     -> 
			#response{content=render_content_template([BaseDir, "/base.html"], [BaseDir, ?CACHE_SUFFIX, FilePath, ".html"])} 
	end.

render_md(MdPath, HtmlPath) ->
	filelib:ensure_dir(HtmlPath),
	os:cmd(["pandoc -o ", HtmlPath, " ", MdPath]).

render_content_template(TemplatePath, ContentPath) ->
	{ok, Template} = file:read_file(TemplatePath),
	{ok, Content} = file:read_file(ContentPath),
	[Top, Bottom] = binary:split(Template, <<"-content-">>),
	[Top, Content, Bottom].

check_cache(BaseDir, FilePath) ->
	case {filelib:last_modified([BaseDir, FilePath, ".md"]), filelib:last_modified([BaseDir, ?CACHE_SUFFIX, FilePath, ".html"])} of
		{0, _}            -> error;
		{_, 0}            -> render;
		{Md, Html} when Md =< Html -> ok;
		{Md, Html} when Md > Html -> render
	end.
