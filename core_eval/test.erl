
-module(test).

-compile(export_all).

test() ->
    XX = 10,
    A = fun(YY) ->
		XX + YY
	end,
    A().

test2(Param) ->
    A = Param,
    case Param of
	true ->
	    io:format("entered ~p~n", [A]);
	false ->
	    io:format("entered ~p~n", [A]);
	{foo, B} ->
	    {foo, B}
    end.
