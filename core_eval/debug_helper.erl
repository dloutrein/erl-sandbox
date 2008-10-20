%%% Author  : Rudolph van Graan <>
%%% Description :
%%% Created : 25 Jul 2006 by Rudolph van Graan <>

-module(debug_helper).

-export([start/0,
         trace/1,
         trace/2,
	 stop/0]).

start() ->
  dbg:tracer(),
  dbg:p(all,[c,sos,sol]).

trace(ModuleName) ->
  dbg:tpl(ModuleName,[{'_',[],[{message,{return_trace}}]}]).

trace(ModuleName,Function) ->
  dbg:tpl(ModuleName,Function,[{'_',[],[{message,{return_trace}}]}]).

stop() ->
    dbg:stop().
