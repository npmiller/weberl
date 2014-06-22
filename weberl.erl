-module(weberl).
-export([start/1]).
%-behavior(supervisor).

-include("weberl.hrl").

start(Port) ->
	{ok, Socket} = gen_tcp:listen(list_to_integer(lists:flatten(Port)), [binary, {active, false}]),
	accept(Socket).
	%gen_tcp:close(Socket).

accept(Socket) ->
	{ok, ClientSocket} = gen_tcp:accept(Socket),
	io:format("accepted !~n"),
	spawn(fun() -> accept(Socket) end),
	handle_request(ClientSocket).

handle_request(ClientSocket) ->
	H = receive_request(ClientSocket),
	{ok, L} = receive_and_clean_request(ClientSocket, []),
	list_to_request(L, H),
	io:format(H#request.url),
	Rs = weberl_routes:route(H),
	io:format(Rs#response.content),
	gen_tcp:send(ClientSocket, [
		"HTTP/1.1 ", weberl_utils:get_status_message(Rs#response.status_code) ,"\r\n",
		"Content-Type: ", Rs#response.content_type, "; charset=utf-8\r\n",
		"\r\n",
		Rs#response.content
	]),
	gen_tcp:close(ClientSocket).

receive_request(ClientSocket) ->
	inet:setopts(ClientSocket, [{packet, line}, {active, once}]),
	receive
		{tcp, ClientSocket, Msg} ->
			io:format(clean_crlf(Msg)),
		    [Method, Url, Version] = binary:split(clean_crlf(Msg), <<" ">>, [global]),
		    #request{method=Method, url=Url, http_version=Version}
	end.

receive_and_clean_request(ClientSocket, L) ->
	inet:setopts(ClientSocket, [{packet, line}, {active, once}]),
	receive
		{tcp, ClientSocket, <<"\r\n">>} -> {ok, L};
		{tcp, ClientSocket, Msg} ->
			receive_and_clean_request(ClientSocket, [clean_crlf(Msg)|L])
	end.

list_to_request([], H) -> H;
list_to_request([E|Tail], H) ->
	case E of
		<<"GET ", Args/binary>>               -> [ Url, Version ] = binary:split(Args, <<" ">>), 
		                                         list_to_request(Tail, H#request{url=Url, http_version=Version});
		<<"User-Agent: ", UserAgent/binary>>  -> list_to_request(Tail, H#request{user_agent=UserAgent});
		<<"Content-Length: ", Length/binary>> -> list_to_request(Tail, H#request{content_length=list_to_integer(binary_to_list(Length))});
		_ -> io:format(E), list_to_request(Tail, H)
	end.

receive_body(ClientSocket) ->
	inet:setopts(ClientSocket, [binary, {active, once}]),
	receive
		{tcp, ClientSocket, Body} -> {ok, Body}
	end.

clean_crlf(BitString) -> [Clean] = binary:split(BitString, <<"\r\n">>, [trim]), Clean.
