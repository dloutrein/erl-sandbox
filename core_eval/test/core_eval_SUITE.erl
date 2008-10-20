%%%-------------------------------------------------------------------
%%% File    : core_eval_SUITE.erl
%%% Author  : denis <>
%%% Description :
%%%
%%% Created : 18 Oct 2008 by denis <>
%%%-------------------------------------------------------------------
-module(core_eval_SUITE).

%% module where are defined functions which will be evaluated to test core
%% interpreter.
-define(TEST_MODULE, core_eval_tests).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include("ct.hrl").

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: suite() -> Info
%%
%% Info = [tuple()]
%%   List of key/value pairs.
%%
%% Description: Returns list of tuples to set default properties
%%              for the suite.
%%
%% Note: The suite/0 function is only meant to be used to return
%% default data values, not perform any other operations.
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{minutes,10}}].

%%--------------------------------------------------------------------
%% Function: init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the suite.
%%
%% Description: Initialization before the suite.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    %% read a test.erl file, compile as beam and to core, get the ast and eval it.
    %% The functions of the file will be used to test core_eval module.
    {ok, _} = compile:file("../test/" ++ atom_to_list(?TEST_MODULE) ++ ".erl"),
    {ok, _} = compile:file("../test/" ++ atom_to_list(?TEST_MODULE) ++ ".erl", [to_core]),
    {ok, F} = file:read_file(atom_to_list(?TEST_MODULE) ++ ".core"),
    FF = binary_to_list(F),
    ct:print("~s~n", [FF]),
    {ok, T, _} = core_scan:string(FF),
    {ok, AST} = core_parse:parse(T),
    Bindings = core_eval:eval(AST, orddict:new()),
    ct:print("bindings=~p~n", [Bindings]),
    [{bindings, Bindings} | Config].

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config0) -> void() | {save_config,Config1}
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Cleanup after the suite.
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%%
%% TestCase = atom()
%%   Name of the test case that is about to run.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the test case.
%%
%% Description: Initialization before each test case.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1}
%%
%% TestCase = atom()
%%   Name of the test case that is finished.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Cleanup after each test case.
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: all() -> TestCases | {skip,Reason}
%%
%% TestCases = [TestCase | {sequence,SeqName}]
%% TestCase = atom()
%%   Name of a test case.
%% SeqName = atom()
%%   Name of a test case sequence.
%% Reason = term()
%%   The reason for skipping all test cases.
%%
%% Description: Returns the list of test cases that are to be executed.
%%--------------------------------------------------------------------
all() ->
    [
%%      core_eval_test1,
%%      core_eval_test2,
%%      core_eval_test3,
%%      core_eval_test4,
     core_eval_test5].


%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

core_eval_test1(Config) ->
    Bindings = proplists:get_value(bindings, Config),
    {Result, _} = core_eval:call(test1, [], Bindings),
    Result = ?TEST_MODULE:test1().

core_eval_test2(Config) ->
    Bindings = proplists:get_value(bindings, Config),
    {Result, _} = core_eval:call(test2, [true], Bindings),
    Result = ?TEST_MODULE:test2(true),
    {Result2, _} = core_eval:call(test2, [false], Bindings),
    Result2 = ?TEST_MODULE:test2(false),
    {Result3, _} = core_eval:call(test2, [{t1, t2}], Bindings),
    Result3 = ?TEST_MODULE:test2({t1, t2}),
    {Result4, _} = core_eval:call(test2, [[a, b, 12, {"foo", "bar"}]], Bindings),
    Result4 = ?TEST_MODULE:test2([a, b, 12, {"foo", "bar"}]),
    {Result5, _} = core_eval:call(test2, [{1, 2.3}], Bindings),
    Result5 = ?TEST_MODULE:test2({1, 2.3}).

core_eval_test3(Config) ->
    Bindings = proplists:get_value(bindings, Config),
    {Result, _} = core_eval:call(test3, [2], Bindings),
    Result = ?TEST_MODULE:test3(2),
    {Result2, _} = core_eval:call(test3, [foo], Bindings),
    Result2 = ?TEST_MODULE:test3(foo).

core_eval_test4(Config) ->
    Bindings = proplists:get_value(bindings, Config),
    {Result, _} = core_eval:call(test4, [1, 2], Bindings),
    Result = ?TEST_MODULE:test4(1, 2),
    {Result2, _} = core_eval:call(test4, [3, 2], Bindings),
    Result2 = ?TEST_MODULE:test4(3, 2).

core_eval_test5(Config) ->
    Bindings = proplists:get_value(bindings, Config),
    {Result, _} = core_eval:call(test5, [1], Bindings),
    Result = ?TEST_MODULE:test5(1).
