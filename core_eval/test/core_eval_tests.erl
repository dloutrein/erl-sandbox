
-module(core_eval_tests).

-compile(export_all).
    

test1() ->
    XX = 10,
    A = fun(YY) ->
		XX + YY
	end,
    A(22).

test2(Param) ->
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

test4(Param1, Param2) ->
    if Param1 < Param2 ->
	    Param1;
       true ->
	    Param2
    end.

test5(Param) ->
    try
	integer_to_list(Param)
    catch
	_:_ ->
	    foo
    end.

test6(Param) ->
    try dict:append(key, value, Param) of
	A when A < 10 ->
	    lesser;
	A when A >= 10 ->
	    greater_equal
    catch
	_:_ ->
	    the_exception
    end.

test7(Param) ->
    case Param of
	Param when is_integer(Param), Param =< 10 ->
	    lesser_equal;
	Param when is_integer(Param), Param > 10 ->
	    greater;
	_ ->
	    not_integer
    end.
