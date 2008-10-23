
-module(core_eval).

-compile(export_all).

-include_lib("compiler/src/core_parse.hrl").

%% display core AST represention of the file test.erl
ex() ->
    {ok, _} = compile:file("test/core_eval_tests.erl", [to_core]),
    {ok, F} = file:read_file("core_eval_tests.core"),
    FF = binary_to_list(F),
    {ok, T, _} = core_scan:string(FF),
    {ok, AST} = core_parse:parse(T),
    AST.

set_binding(Name, Value, BindingStore) ->
    orddict:store(Name, Value, BindingStore).

get_binding(Name, BindingStore) ->
    case orddict:find(Name, BindingStore) of
	{ok, Value} -> {value, Value};
	error -> unbound
    end.

remove_binding(Name, BindingStore) ->
    orddict:erase(Name, BindingStore).

%% just for tests
call(Name, Args, BindingStore) ->
    FunName = list_to_atom(atom_to_list(Name) ++ "/" ++ integer_to_list(length(Args))),
    eval({c_apply, [], {c_var,[], FunName}, [term_to_core(Arg) || Arg <- Args]}, BindingStore).

%% evaluate a module. all functions are bound in the given BindingStore and then returned.
%% Exported functions and attributes are ignored.
eval(#c_module { defs = Functions }, BindingStore) ->
    lists:foldl(fun({ #c_fname { id = Name, arity = Arity}, Function}, FunBS) ->
			{Fun, FunBS2} = eval(Function, FunBS),
			FunName = list_to_atom(atom_to_list(Name) ++ "/" ++ integer_to_list(Arity)),
			set_binding(FunName, Fun, FunBS2)
		end, BindingStore, Functions);

%% evaluate an application
eval(#c_apply { op = Function, args = Args}, BindingStore) ->
    {Fun, FunBS} = eval(Function, BindingStore),
    {Values, _ValuesBS} = lists:mapfoldl(fun(Arg, ArgBS) ->
						eval(Arg, ArgBS)
					end, FunBS, Args),
    %% special case in apply when there is no arguments given ([[]] in Values)
    %% The Fun has only one arguments (which is a list of arguments for the target fun)
    {Result, _} = apply(Fun, [case Values of [[]] -> []; _ -> Values end]),
    {Result, BindingStore};

%% Evaluates a fun
eval(#c_fun { vars = Vars, body = Body }, BindingStore) ->
    F = fun(Args) ->
		eval_fun(Args, Vars , Body, BindingStore)
	end,
    {F, BindingStore};

%% evaluate a Let
eval(#c_let {} = Let, BindingStore) ->
    %% compute the values of each arguments and store them in the bindingStore
    IntermediateBS =
	case Let#c_let.vars of
	    [#c_var { name = Name }] ->
		{Value, NewBindingStore} = eval(Let#c_let.arg, BindingStore),
		set_binding(Name, Value, NewBindingStore);
	    Vars ->
		%% the evaluation of the arguments should produce a list,
		%% as it is (normaly) a c_value
		{Values, NewBindingStore} = eval(Let#c_let.arg, BindingStore),
		lists:foldl(fun({#c_var { name = Name }, Value}, FunBindingStore) ->
				    set_binding(Name, Value, FunBindingStore)
			    end, NewBindingStore, lists:zip(Vars, Values))
	end,
    %% evaluate the result
    {Result, IntermediateBS2} = eval(Let#c_let.body, IntermediateBS),
    %% Remove all binding that has been created
    FinalBinding =
	lists:foldl(fun(#c_var { name = Name }, FoldBindingStore) ->
			    remove_binding(Name, FoldBindingStore)
		    end, IntermediateBS2, Let#c_let.vars),
    {Result, FinalBinding};

%% Litteral evaluation result as returning its value
eval(#c_literal {} = Litteral, BindingStore) ->
    {Litteral#c_literal.val, BindingStore};

%% Return the value of a variable, or error if unbound
eval(#c_var {} = Var, BindingStore) ->
    case get_binding(Var#c_var.name, BindingStore) of
	{value, Value} ->
	    {Value, BindingStore};
	_ ->
	    erlang:error(unbound)
    end;

%% Evalues all values and return a list of evaluation result for each
eval(#c_values { es = Values }, BindingStore) ->
    lists:mapfoldl(fun(Value, FunBindingStore) ->
			   eval(Value, FunBindingStore)
		   end, BindingStore, Values);

%% evaluate a tuple
eval(#c_tuple {es = Values}, BindingStore) ->
    {Result, Bindings} =
	lists:mapfoldl(fun(Value, FunBindingStore) ->
			       eval(Value, FunBindingStore)
		       end, BindingStore, Values),
    {list_to_tuple(Result), Bindings};

%% evaluate a cons.
eval(#c_cons { hd = Head, tl = Tail}, BindingStore) ->
    %% TODO: rework this to be tail recursive
    {TailValues, TailBS} = eval(Tail, BindingStore),
    {HeadValue, HeadBS} = eval(Head, TailBS),
    {[ HeadValue | TailValues], HeadBS};

%% evaluate a do (c_seq)
eval(#c_seq { arg = Arg, body = Body }, BindingStore) ->
    %% we don't use the result of th first evaluation
    {_, _} = eval(Arg, BindingStore),
    {BodyResult, _} = eval(Body, BindingStore),
    {BodyResult, BindingStore};

%% evaluate a function call
eval(#c_call { module = M, name = F, args = Args}, BindingStore) ->
    {Module, ModuleBS} = eval(M, BindingStore),
    {Function, FunctionBS} = eval(F, ModuleBS),
    {RealArgs, ArgsBS} = lists:mapfoldl(fun(Arg, FunBS) ->
						eval(Arg, FunBS)
					end, FunctionBS, Args),
    {apply(Module, Function, RealArgs), ArgsBS};

%% evaluate a primop
eval(#c_primop { name = F, args = Args}, BindingStore) ->
    {Function, FunctionBS} = eval(F, BindingStore),
    {RealArgs, ArgsBS} = lists:mapfoldl(fun(Arg, FunBS) ->
						eval(Arg, FunBS)
					end, FunctionBS, Args),
    {apply(Function, RealArgs), ArgsBS};

%% evaluate a case
eval(#c_case { arg = Arg, clauses = Clauses }, BindingStore) ->
    {ArgValue, ArgBS} = eval(Arg, BindingStore),
    eval_clauses(Clauses, ArgValue, ArgBS);

%% evaluate a try
eval(#c_try { arg = Arg, vars = Vars, body = Body, evars = EVars, handler = Handler },
     BindingStore) ->
    EvalResult = try
		      {success, eval(Arg, BindingStore)}
		  catch
		      Class:Reason ->
			  {exception, Class, Reason}
		  end,
    case EvalResult of
	{success, {ArgValues, ArgBS}} ->
	    %% bind the values to the vars given in Vars
	    VarsBS = case length(Vars) of
			 1 ->
			     Var = hd(Vars),
			     set_binding(Var#c_var.name, ArgValues, ArgBS);
			 _ ->
			     %% @todo find the best way to manage this case
			     erlang:error({not_implemented, "Multiple vars in try"})
		     end,
	    eval(Body, VarsBS);
	{exception, ExClass, ExReason} ->
	    %% EVars must have 3 elements  as defined in specs
	    [ClassVar, ReasonVar, ImplVar] = EVars,
	    BindingStore2 = set_binding(ClassVar#c_var.name, ExClass, BindingStore),
	    BindingStore3 = set_binding(ReasonVar#c_var.name, ExReason, BindingStore2),
	    %% @todo: what to use as implementation dependant ?
	    BindingStore4 = set_binding(ImplVar#c_var.name, erlang:get_stacktrace(), BindingStore3),
	    %% lets go to evaluate the handler of the exception
	    {HandlerResult, _} = eval(Handler, BindingStore4),

	    {HandlerResult, BindingStore}
    end;

%% This element is not implemented for now
eval(Element, _BindingStore) ->
    erlang:error({not_implemented, Element}).

%% evaluate a list of clauses until one match, or raise a error
eval_clauses([], _Value, _BindingStore) ->
    erlang:error(nomatch);
eval_clauses([Clause | Clauses], Value, BindingStore) ->
    case eval_clause(Clause, Value, BindingStore) of
	{true, {Result, _ForgetTheseBindings}} ->
	    %% as the scope of bindings is the clause body, we can forget all
	    %% bindings that have been defined during the clause evaluation,
	    %% they are not valid anymore
	    {Result, BindingStore};
	{false, _} ->
	    eval_clauses(Clauses, Value, BindingStore)
    end.

eval_clause(#c_clause { pats = Patterns, guard = Guard, body = Body}, Values, BindingStore) ->
    %% test if each pattern match
    case eval_patterns(Patterns, Values, BindingStore) of
	{true, PatternsBS} -> 
	    %% all patterns have matched, we can evaluate the guard, and
	    %% if guard is true, evaluate the body
	    case eval_guard(Guard, PatternsBS) of
		{true, GuardBS} ->
		    {true, eval(Body, GuardBS)};
		{false, GuardBS} ->
		    {false, GuardBS}
	    end;
	{false, PatternsBS} ->
	    {false, PatternsBS}
    end.

eval_patterns([], _Value, BindingStore) -> 
    %% there is no more pattern to test, so we can return true and 
    %% the new BindingStore
    {true, BindingStore};
eval_patterns([Pattern], Value, BindingStore) when not is_list(Pattern) ->
    %% in this case, Value is not a list...
    is_pattern_match(Pattern, Value, BindingStore);
eval_patterns([Pattern | Patterns], [Value|Values], BindingStore) ->
    %% test if each pattern match
    case is_pattern_match(Pattern, Value, BindingStore) of
	{true, PatternsBS} ->
	    eval_patterns(Patterns, Values, PatternsBS);
	{false, PatternsBS} ->
	    {false, PatternsBS}
    end.

is_pattern_match(#c_literal {} = Litteral, Value, BindingStore) ->
    if Litteral#c_literal.val =:= Value ->
	    {true, BindingStore};
       true ->
	    {false, BindingStore}
    end;

is_pattern_match(#c_tuple {}, Value, BindingStore)
  when is_tuple(Value) =:= false ->
    {false, BindingStore};
is_pattern_match(#c_tuple { es = Elements}, Value, BindingStore)
  when tuple_size(Value) =/= length(Elements) ->
    {false, BindingStore};
is_pattern_match(#c_tuple { es = Elements}, Value, BindingStore) ->
    match_tuple_elements(Elements, Value, 1, BindingStore);

is_pattern_match(#c_cons {}, Value, BindingStore)
    when is_list(Value) =:= false ->
    {false, BindingStore};
is_pattern_match(#c_cons {} = Cons, Value, BindingStore) ->
    match_cons_elements(Cons, Value, BindingStore);

is_pattern_match(#c_var { name = Name}, Value, BindingStore) ->
    %% if the Variable is already bound, the values must match. Otherwise, we
    %% bind the value to the variable
    case get_binding(Name, BindingStore) of
	{value, Value} ->  %% value is matched here
	    {true, BindingStore};
	unbound ->  %% the variable is not bound
	    NewBS = set_binding(Name, Value, BindingStore),
	    {true, NewBS};
	_ -> %% values doesn't match
	    {false, BindingStore}
    end;

is_pattern_match(#c_alias { var = #c_var { name = Name}, pat = Pattern}, Value, BindingStore) ->
    %% If the pattern match, then create a binding
    case is_pattern_match(Pattern, Value, BindingStore) of
	{true, PatternsBS} ->
	    case get_binding(Name, PatternsBS) of
		{value, Value} ->  %% value is matched here
		    {true, PatternsBS};
		unbound ->  %% the variable is not bound
		    NewBS = set_binding(Name, Value, PatternsBS),
		    {true, NewBS};
		_ ->
		    erlang:error(badmatch)
	    end;
	{false, PatternsBS} ->
	    {false, PatternsBS}
    end;

is_pattern_match(Unknown, _Value, _BindingStore) ->
    erlang:error({not_implemented, is_pattern_match, Unknown}).

match_tuple_elements(_, Value, _Index, BindingStore) when is_tuple(Value) =:= false ->
    {false, BindingStore};
match_tuple_elements([], _Value, _Index, BindingStore) ->
    {true, BindingStore};
match_tuple_elements([Element | Elements], Tuple, Index, BindingStore) ->
    case is_pattern_match(Element, element(Index, Tuple), BindingStore) of
	{false, MatchBS} ->
	    {false, MatchBS};
	{true, MatchBS} ->
	    match_tuple_elements(Elements, Tuple, Index+1, MatchBS)
    end.

%% The last element of a cons is always nil ([])
match_cons_elements({c_literal, [], []}, [], BindingStore) ->
    {true, BindingStore};
match_cons_elements(#c_cons { hd = Head, tl = Tail}, [Value | Values], BindingStore) ->
    case is_pattern_match(Head, Value, BindingStore) of
	{false, MatchBS} ->
	    {false, MatchBS};
	{true, MatchBS} ->
	    match_cons_elements(Tail, Values, MatchBS)
    end.

eval_guard(Guard, BindingStore) ->
    case eval(Guard, BindingStore) of
	{true, GuardBS} ->
	    {true, GuardBS};
	{_, GuardBS} ->
	    {false, GuardBS}
    end.

eval_fun(Args, Vars, Body, BindingStore) when length(Args) =:= length(Vars) ->
    %% create the binding between args and var
    ArgsBS = lists:foldl(fun({#c_var { name = VarName}, Value}, FunBS) ->
				 set_binding(VarName, Value, FunBS)
			 end, BindingStore, lists:zip(Vars, Args)),
    %% we ignore the new binding store, as we return the original one
    {Result, _} = eval(Body, ArgsBS),
    {Result, BindingStore}.

%% Convert an erlang term (int, float, atom, char, list, tuple, []) into its core
%% equivalent.
term_to_core(Term) when is_tuple(Term) ->
    #c_tuple { es = [ term_to_core(E) || E <- tuple_to_list(Term)] };
term_to_core([]) ->
    #c_literal { val = [] };
term_to_core([Head | Tail]) ->
    #c_cons { hd = term_to_core(Head), tl = term_to_core(Tail) };
term_to_core(Term) when is_integer(Term);
		       is_float(Term);
		       is_atom(Term) ->
    #c_literal { val = Term }.
