-module(weberl).
-export([start/1, start_dev/0]).
%-behavior(supervisor).
-record(headers, {url, user_agent}).

start_dev() -> start(8008).

start(Port) ->
	{ok, Socket} = gen_tcp:listen(Port, [binary, {active, false}]),
	accept(Socket),
	gen_tcp:close(Socket).

accept(Socket) ->
	{ok, ClientSocket} = gen_tcp:accept(Socket),
	io:format("accepted !~n"),
	{ok, H} = parseHeaders(ClientSocket, #headers{}),
	io:format("Headers parsed !~n"),
	io:format(H#headers.url),
	{ok, Content} = file:read_file(string:strip(bitstring_to_list(H#headers.url), right, $\n)),
	gen_tcp:send(ClientSocket, ["HTTP/1.0 200 OK\r\n", "Content-Type: text/html; charset=utf-8\r\n", "\r\n", Content, "\r\n"]),
	gen_tcp:close(ClientSocket).

parseHeaders(ClientSocket, H) ->
	inet:setopts(ClientSocket, [{packet, line}, {active, once}]),
	io:format("ParseHeaders~n"),
	receive
		{tcp, _, <<"GET /", Path/binary>>} -> [ Url, _ ] = binary:split(Path, <<" ">>), parseHeaders(ClientSocket, H#headers{url=Url});
		{tcp, _, <<"User-Agent: ", UserAgent/binary>>} -> parseHeaders(ClientSocket, H#headers{user_agent=UserAgent});
		{tcp, _, <<"\r\n">>} -> {ok, H};
		{tcp, _, Msg} -> io:format(Msg), parseHeaders(ClientSocket, H)
	end.
