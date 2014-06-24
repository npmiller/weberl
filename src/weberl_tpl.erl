-module(weberl_tpl).
-export([render_template/2]).

render_template(Template, Bindings) ->
	Env = lists:foldl(fun(E, Env) -> add_to_env(Env, E) end, erl_eval:new_bindings(), Bindings),
	Parts = binary:split(Template, <<"--code--">>, [global]),
	lists:foldl(fun(E, Tpl) -> generate_template(Env, Tpl, E) end, "", lists:zip(lists:seq(1,length(Parts)), Parts)).

generate_template(Env, Tpl, {Index, Part}) ->
	if
		Index rem 2 == 0 -> 
			{value, Value, _} = eval(bitstring_to_list(Part), Env), 
			[Tpl, Value];
		true -> 
			[Tpl, Part]
	end.

add_to_env(Env, {Name, Value}) ->
	erl_eval:add_binding(Name, Value, Env).

eval(S,Env) ->
	{ok, Scanned, _} = erl_scan:string(S),
	{ok,Parsed} = erl_parse:parse_exprs(Scanned),
	erl_eval:exprs(Parsed,Env).
