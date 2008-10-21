
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
	dict:append(key, value, Param)
    catch
	_:_ ->
	    the_exception
    end.

test6(Param) ->
    case Param of
	Param when is_integer(Param), Param =< 10 ->
	    inferior_equal;
	Param when is_integer(Param), Param > 10 ->
	    superior;
	_ ->
	    not_integer
    end.
