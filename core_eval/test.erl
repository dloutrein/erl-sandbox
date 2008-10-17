
-module(test).

-compile(export_all).


run() ->
    {ok, _} = compile:file("test.erl", [to_core]),
    {ok, F} = file:read_file("test.core"),
    FF = binary_to_list(F),
    {ok, T, _} = core_scan:string(FF),
    {ok, AST} = core_parse:parse(T),
    %% run the tests
    Bindings = core_eval:eval(AST, orddict:new()),

    {Result, _} = core_eval:call('test/0', [], Bindings),
    io:format("Result=~p, ~p~n", [Result, test()]),
    Result = test(),

    {Result3, _} = core_eval:call('test3/1', 2, Bindings),
    io:format("Result3=~p, ~p~n", [Result3, test3(2)]),
    Result3 = test3(2).
    

test() ->
    XX = 10,
    A = fun(YY) ->
		XX + YY
	end,
    A(22).

test2(Param) ->
    A = Param,
    case Param of
	true ->
	    foo;
	false ->
	    bar;
	{t1, t2} ->
	    buzz;
	[a, b, 12, {"foo", "bar"}] ->
	    great;
	{1, 2.3} = ATuple ->
	    ATuple
    end.

test3(Bar) when is_atom(Bar) ->
    (fun(Foo) ->
	    {Foo}
    end)(Bar);
test3(Bar) ->
    Bar.
