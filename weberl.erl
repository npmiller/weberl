-module(weberl).
-export([start/1, start_dev/0, clean_crlf/1]).
%-behavior(supervisor).
-record(headers, {method, url, http_version, user_agent}).

start_dev() -> start(8005).

start(Port) ->
	{ok, Socket} = gen_tcp:listen(Port, [binary, {active, false}]),
	accept(Socket).
	%gen_tcp:close(Socket).

accept(Socket) ->
	{ok, ClientSocket} = gen_tcp:accept(Socket),
	io:format("accepted !~n"),
	spawn(fun() -> accept(Socket) end),
	handle_request(ClientSocket).

handle_request(ClientSocket) ->
	H = receive_request(ClientSocket),
	{ok, L} = receive_and_clean_headers(ClientSocket, []),
	%{ok, Body} = receive_body(ClientSocket),
	list_to_headers(L, H),
	io:format(H#headers.url),
	{ok, Pwd} = file:get_cwd(),
	{ok, Content} = file:read_file(lists:concat([Pwd, bitstring_to_list(H#headers.url)])),
	gen_tcp:send(ClientSocket, [
		"HTTP/1.0 200 OK\r\n",
		"Content-Type: text/html; charset=utf-8\r\n",
		"\r\n",
		Content
	]),
	gen_tcp:close(ClientSocket).

receive_request(ClientSocket) ->
	inet:setopts(ClientSocket, [{packet, line}, {active, once}]),
	receive
		{tcp, ClientSocket, Msg} ->
			io:format(clean_crlf(Msg)),
		    [Method, Url, Version] = binary:split(clean_crlf(Msg), <<" ">>, [global]),
		    #headers{method=Method, url=Url, http_version=Version}
	end.

receive_and_clean_headers(ClientSocket, L) ->
	inet:setopts(ClientSocket, [{packet, line}, {active, once}]),
	receive
		{tcp, ClientSocket, <<"\r\n">>} -> {ok, L};
		{tcp, ClientSocket, Msg} ->
			receive_and_clean_headers(ClientSocket, [clean_crlf(Msg)|L])
	end.

list_to_headers([], H) -> H;
list_to_headers(L, H) ->
	[E|Tail] = L,
	case E of
		<<"GET ", Args/binary>> -> [ Url, Version ] = binary:split(Args, <<" ">>), 
		                           list_to_headers(Tail, H#headers{url=Url, http_version=Version});
		<<"User-Agent: ", UserAgent/binary>> -> list_to_headers(Tail, H#headers{user_agent=UserAgent});
		_ -> io:format(E), list_to_headers(Tail, H)
	end.

receive_body(ClientSocket) ->
	inet:setopts(ClientSocket, [binary, {active, once}]),
	receive
		{tcp, ClientSocket, Body} -> {ok, Body}
	end.

clean_crlf(BitString) -> [Clean] = binary:split(BitString, <<"\r\n">>, [trim]), Clean.
