
-module(core_eval).

-compile(export_all).

-include_lib("compiler/src/core_parse.hrl").

%% display core AST represention of the file test.erl
ex() ->
    {ok, _} = compile:file("test.erl", [to_core]),
    {ok, F} = file:read_file("test.core"),
    FF = binary_to_list(F),
    {ok, T, _} = core_scan:string(FF),
    core_parse:parse(T).

set_binding(Name, Value, BindingStore) ->
    orddict:store(Name, Value, BindingStore).

get_binding(Name, BindingStore) ->
    case orddict:find(Name, BindingStore) of
	{ok, Value} -> {value, Value};
	error -> unbound
    end.

remove_binding(Name, BindingStore) ->
    orddict:erase(Name, BindingStore).

test2() ->
    {ok, _} = compile:file("test2.erl", [to_core]),
    {ok, F} = file:read_file("test2.core"),
    FF = binary_to_list(F),
    {ok, T, _} = core_scan:string(FF),
    {ok, AST} = core_parse:parse(T).
    

test() ->
    L =  {c_let,[],
	  [{c_var,[],'X1'}],
	  {c_literal,[],200},
	  {c_var,[],'X1'}},
    L2 =  {c_let,[],
	   [{c_var,[],'X1'},{c_var,[],'X2'}],
	   {c_values,[],[{c_literal,[],20},{c_literal,[],foo}]},
	   {c_tuple,[],[{c_var,[],'X1'},{c_var,[],'X2'}]}},
    L3 =  {c_call,[],
	   {c_literal,[],io},
	   {c_literal,[],format},
	   [{c_cons,[],
	     {c_literal,[],72},
	     {c_cons,[],
	      {c_literal,[],101},
	      {c_cons,[],
	       {c_literal,[],108},
	       {c_cons,[],
		{c_literal,[],108},
		{c_cons,[],
		 {c_literal,[],111},
		 {c_cons,[],
		  {c_literal,[],32},
		  {c_cons,[],
		   {c_literal,[],119},
		   {c_cons,[],
		    {c_literal,[],111},
		    {c_cons,[],
		     {c_literal,[],114},
		     {c_cons,[],
		      {c_literal,[],108},
		      {c_cons,[],
		       {c_literal,[],100},
		       {c_cons,[],
			{c_literal,[],32},
			{c_cons,[],
			 {c_literal,[],33},
			 {c_literal,[],[]}}}}}}}}}}}}}}]},
    L4 = {c_case,[],
%	  {c_literal,[],aaa},
%	  {c_tuple,[],[{c_literal,[],t1},{c_literal,[],t2}]},
	  {c_tuple,[],[{c_literal,[],1},{c_literal,[],2.30000}]},
%% 	  {c_cons,[],
%% 	   {c_literal,[],a},
%% 	   {c_cons,[],
%% 	    {c_literal,[],b},
%% 	    {c_cons,[],
%% 	     {c_literal,[],12},
%% 	     {c_cons,[],
%% 	      {c_tuple,[],
%% 	       [{c_cons,[],
%% 		 {c_literal,[],102},
%% 		 {c_cons,[],
%% 		  {c_literal,[],111},
%% 		  {c_cons,[],{c_literal,[],111},{c_literal,[],[]}}}},
%% 		{c_cons,[],
%% 		 {c_literal,[],98},
%% 		 {c_cons,[],
%% 		  {c_literal,[],97},
%% 		  {c_cons,[],{c_literal,[],114},{c_literal,[],[]}}}}]},
%% 	      {c_literal,[],[]}}}}},
	  [{c_clause,[],
	    [{c_literal,[],true}],
	    {c_literal,[],true},
	    {c_literal,[],foo}},
	   {c_clause,[],
	    [{c_literal,[],false}],
	    {c_literal,[],true},
	    {c_literal,[],bar}},
	   {c_clause,[],
	    [{c_tuple,[],[{c_literal,[],t1},{c_literal,[],t2}]}],
	    {c_literal,[],true},
	    {c_literal,[],buzz}},
	   {c_clause,[],
	    [{c_cons,[],
	      {c_literal,[],a},
	      {c_cons,[],
	       {c_literal,[],b},
	       {c_cons,[],
		{c_literal,[],12},
		{c_cons,[],
		 {c_tuple,[],
		  [{c_cons,[],
		    {c_literal,[],102},
		    {c_cons,[],
		     {c_literal,[],111},
		     {c_cons,[],{c_literal,[],111},{c_literal,[],[]}}}},
		   {c_cons,[],
		    {c_literal,[],98},
		    {c_cons,[],
		     {c_literal,[],97},
		     {c_cons,[],{c_literal,[],114},{c_literal,[],[]}}}}]},
		 {c_literal,[],[]}}}}}],
	    {c_literal,[],true},
	    {c_literal,[],great}},
	   {c_clause,[],
	    [{c_alias,[],
	      {c_var,[],'ATuple'},
	      {c_tuple,[],[{c_literal,[],1},{c_literal,[],2.30000}]}}],
	    {c_literal,[],true},
	    {c_var,[],'ATuple'}},
	   {c_clause,
	    [compiler_generated],
	    [{c_var,[],'_cor2'}],
	    {c_literal,[],true},
	    {c_primop,[],
	     {c_literal,[],match_fail},
	     [{c_tuple,[],
	       [{c_literal,[],case_clause},
		{c_var,[],'_cor2'}]}]}}]},

    L5 = {c_fun,
	  [{id,{0,17602895,'-test/0-fun-0-'}}],
	  [{c_var,[],'_cor1'}],
	  {c_call,[],
	   {c_literal,[],erlang},
	   {c_literal,[],'+'},
	   [{c_literal,[],10},{c_var,[],'_cor1'}]}},

    L6 = {c_fun,[],
	  [{c_var,[],'_cor0'}],
	  {c_let,[],
	   [{c_var,[],'_cor3'}],
	   {c_fun,
	    [{id,{1,70942264,'-test3/1-fun-0-'}}],
	    [{c_var,[],'_cor1'}],
	    {c_tuple,[],[{c_var,[],'_cor1'}]}},
	   {c_apply,[],{c_var,[],'_cor3'},[{c_var,[],'_cor0'}]}}},

    eval(L, orddict:new()),
    eval(L2, orddict:new()),
    eval(L3, orddict:new()),
    eval(L4, orddict:new()),
    eval(L5, orddict:new()),
    eval(L6, orddict:new()),
    {ok, L7} = ex(),
    %% core_eval:call('test3/1', 10, B)
    eval(L7, orddict:new()).

%% just for tests
call(Name, Arg, BindingStore) ->
    eval({c_apply, [], {c_var,[], Name}, [{c_literal, [], Arg}]}, BindingStore).

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
		eval_fun(Args, Vars, Body, BindingStore)
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
    eval_clauses(Clauses, ArgValue, ArgBS).
	
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
 
%% evaluate a clause. Return {true, NewBindingStore} or {false, BindingStore}
eval_clause(#c_clause { pats = [], guard = Guard, body = Body },
	    _Value, BindingStore) ->
    %% at this point, all patterns have matched, we can evaluate the guard, and
    %% if guard is true, evaluate the body
    case eval_guard(Guard, BindingStore) of
	{true, GuardBS} ->
	    {true, eval(Body, GuardBS)};
	{false, GuardBS} ->
	    {false, GuardBS}
    end;

eval_clause(#c_clause { pats = [Pattern | PatternsTail] } = Patterns,
	    Value, BindingStore) ->
    %% test if each pattern match
    case is_pattern_match(Pattern, Value, BindingStore) of
	{true, PatternsBS} ->
	    eval_clause(Patterns#c_clause { pats = PatternsTail}, Value, PatternsBS);
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
    end.

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
