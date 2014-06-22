-module(weberl_serv).
-behavior(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-include("weberl.hrl").
-record(state, {socket, request=#request{}, headers=true}).

start_link(Socket) ->
	gen_server:start_link(?MODULE, Socket, []).

init(Socket) ->
	gen_server:cast(self(), listen),
	{ok, #state{socket=Socket}}.

handle_call(_E, _From, State) ->
	{noreply, State}.

handle_cast(listen, S = #state{socket=Socket}) ->
	{ok, ClientSocket} = gen_tcp:accept(Socket),
	weberl:start_listener(),
	inet:setopts(ClientSocket, [{packet, line}, {active, once}]),
	{noreply, S#state{socket=ClientSocket}};

handle_cast(received, S = #state{socket=ClientSocket}) ->
	Rs = weberl_routes:route(S#state.request),
	gen_tcp:send(ClientSocket, [
		"HTTP/1.1 ", weberl_utils:get_status_message(Rs#response.status_code) ,"\r\n",
		"Content-Type: ", Rs#response.content_type, "; charset=utf-8\r\n",
		"\r\n",
		Rs#response.content
	]),
	gen_tcp:close(ClientSocket),
	{stop, normal, S}.

handle_info({tcp, _ClientSocket, <<"\r\n">>}, S = #state{headers=true, request=#request{content_length=0}}) ->
	gen_server:cast(self(), received),
	{noreply, S#state{headers=false}};

handle_info({tcp, ClientSocket, <<"\r\n">>}, S = #state{headers=true}) ->
	inet:setopts(ClientSocket, [binary, {active, once}]),
	{noreply, S#state{headers=false}};

handle_info({tcp, ClientSocket, Str}, S = #state{headers=true}) ->
	inet:setopts(ClientSocket, [{packet, line}, {active, once}]),
	case weberl_utils:clean_crlf(Str) of
		<<"GET ", Args/binary>>               -> 
			[ Url, Version ] = binary:split(Args, <<" ">>),
			{noreply, S#state{request=(S#state.request)#request{url=Url, http_version=Version, method=get}}};

		<<"User-Agent: ", UserAgent/binary>>  -> 
			{noreply, S#state{request=(S#state.request)#request{user_agent=UserAgent}}};

		<<"Content-Length: ", Length/binary>> -> 
			{noreply, S#state{request={(S#state.request)#request{content_length=list_to_integer(binary_to_list(Length))}}}};

		_ -> {noreply, S}
	end;

handle_info({tcp, _ClientSocket, Body}, S = #state{headers=false}) ->
	gen_server:cast(self(), received),
	{noreply, S#state.request#request{body=Body}}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(normal, _State) ->
	ok;

terminate(_Reason, _State) ->
	io:format("terminate reason: ~p~n", [_Reason]).

