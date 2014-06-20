-module(weberl).
-export([start/1, start_dev/0]).
%-behavior(supervisor).
-record(headers, {url, user_agent}).

start_dev() -> start(8005).

start(Port) ->
	{ok, Socket} = gen_tcp:listen(Port, [binary, {active, false}]),
	accept(Socket),
	gen_tcp:close(Socket).

accept(Socket) ->
	{ok, ClientSocket} = gen_tcp:accept(Socket),
	io:format("accepted !~n"),
	{ok, L} = receive_and_clean_headers(ClientSocket, []),
	io:format("headers received~n"),
	%{ok, Body} = receive_body(ClientSocket),
	io:format("body received~n"),
	H = list_to_headers(L, #headers{}),
	io:format("Headers parsed !~n"),
	io:format(H#headers.url),
	Filename = H#headers.url,
	io:format(Filename),
	{ok, Content} = file:read_file(Filename),
	gen_tcp:send(ClientSocket, [
		"HTTP/1.0 200 OK\r\n",
		"Content-Type: text/html; charset=utf-8\r\n",
		"\r\n",
		Content
	]),
	gen_tcp:close(ClientSocket).

receive_and_clean_headers(ClientSocket, L) ->
	inet:setopts(ClientSocket, [{packet, line}, {active, once}]),
	receive
		{tcp, ClientSocket, <<"\r\n">>} -> {ok, L};
		{tcp, ClientSocket, Msg} ->
			[C] = binary:split(Msg, <<"\r\n">>, [trim]),
			receive_and_clean_headers(ClientSocket, [C|L])
	end.

list_to_headers([], H) -> H;
list_to_headers(L, H) ->
	[E|Tail] = L,
	case E of
		<<"GET /", Path/binary>> -> [ Url, _ ] = binary:split(Path, <<" ">>), list_to_headers(Tail, H#headers{url=Url});
		<<"User-Agent: ", UserAgent/binary>> -> list_to_headers(Tail, H#headers{user_agent=UserAgent});
		_ -> io:format(E), list_to_headers(Tail, H)
	end.

receive_body(ClientSocket) ->
	inet:setopts(ClientSocket, [binary, {active, once}]),
	receive
		{tcp, ClientSocket, Body} -> {ok, Body}
	end.
