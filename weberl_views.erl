-module(weberl_views).
-export([serve_files/2, serve_md/2]).

-include("weberl.hrl").
-include_lib("kernel/include/file.hrl").

serve_files(BaseDir, FilePath) ->
	case file:read_file(lists:concat([BaseDir, FilePath])) of
		{ok, Content} -> #response{content=Content};
		{error, enoent} -> #response{status_code=404}
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
		                                          #response{content=[Top, renderMd(FileMd, FileHtml), Bottom]} 
		                                  end;
		{{ok, _MdInfo}, {error, enoent}} -> #response{content=[Top, renderMd(FileMd, FileHtml), Bottom]};
		{{error, enoent}, {_,_}} -> #response{status_code=404}
	end.

renderMd(File, Out) ->
	filelib:ensure_dir(Out),
	os:cmd(lists:flatten(["pandoc -o ", Out, " ", File])),
	{ok, Content} = file:read_file(Out),
	Content.
