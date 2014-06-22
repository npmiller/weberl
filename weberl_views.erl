-module(weberl_views).
-export([serve_files/2, serve_md/2]).

-include("weberl.hrl").
-include_lib("kernel/include/file.hrl").

serve_files(BaseDir, FilePath) ->
	case file:read_file(lists:concat([BaseDir, FilePath])) of
		{ok, Content} -> 
			#response{content=Content, content_type=weberl_utils:get_content_type(filename:extension(FilePath))};
		{error, _} -> 
			#response{status_code=404}
	end.

serve_md(BaseDir, FilePath) ->
	FileMd = lists:concat([BaseDir, FilePath, ".md"]),
	FileHtml = lists:concat([BaseDir, "-cache", FilePath, ".html"]),
	{ok, Template} = file:read_file(lists:concat([BaseDir, "/base.html"])),
	[Top, Bottom] = binary:split(Template, <<"-content-">>),
	case { file:read_file_info(FileMd), file:read_file_info(FileHtml)} of
		{{ok, MdInfo}, {ok, HtmlInfo}} -> if 
		                                      MdInfo#file_info.mtime =< HtmlInfo#file_info.mtime -> 
		                                          {ok, Content} = file:read_file(FileHtml), #response{content=[Top, Content, Bottom]};
		                                      true -> 
		                                          #response{content=[Top, render_md(FileMd, FileHtml), Bottom]}
		                                  end;
		{{ok, _MdInfo}, {error, enoent}} -> #response{content=[Top, render_md(FileMd, FileHtml), Bottom]};
		{{error, _}, {_,_}} -> #response{status_code=404}
	end.

render_md(File, Out) ->
	filelib:ensure_dir(Out),
	os:cmd(lists:flatten(["pandoc -o ", Out, " ", File])),
	{ok, Content} = file:read_file(Out),
	Content.
