% @author Guidotti Dennis Alessio <guidotti2@gmail.com>
% @author Lanziani Luca <luca@tulug.it>
% @author Loesch Thomas <thomas.loesch@me.com>
% @version 0.5

% @doc 	This module implements the supervisor server that create the dispatcher and the workers.

-module(flowersv).
-behaviour(supervisor).

-export([start/1,stopChild/0]).
-export([init/1]).
-compile(export_all).
  
-define(TIMEOUT, 40000).

% @doc Start the supervisor
% @spec start(Tuple::tuple(flower,non_negative_integer())) -> Result
% 
% Result = {ok,Pid} | ignore | {error,Error}
start(Struct) -> supervisor:start_link({local,?MODULE},?MODULE, Struct).

% @doc Stop all the child of the supervisor
% @spec stopChild() -> Lists::list(Tuple::tuple())
stopChild() -> Spec = supervisor:which_children(?MODULE),
			   lists:keymap(fun(Child) -> {terminate,supervisor:terminate_child(?MODULE,Child),Child} end, 1, Spec).

% @doc Whenever a supervisor is started using supervisor:start_link/2,3, this function is called by the new process to find out about restart strategy, maximum restart frequency and child specifications
% @spec init(Tuple::tuple(flower,non_negative_integer())) -> {ok, {{one_for_one, 1, 60},[Child_spec]}}
% 
% child_spec() = {Id,StartFunc,Restart,Shutdown,Type,Modules}
%	Id = term()
%	StartFunc = {M,F,A}
%	M = atom()
%	F = atom()
%  	A = [term()]
% 	Restart = permanent | transient | temporary
% 	Shutdown = brutal_kill | not_negative_integer() | infinity
% 	Type = worker | supervisor
% 	Modules = [Module] | dynamic
%  		Module = atom()
init(Struct) -> 
		   Child = create_childSpec(Struct),
		   %io:fwrite("~p~n",[Child]),
		  {ok, {{one_for_one, 1, 60},Child}}.

% @doc create the child_spec for all the workers and the dispatcher
% @spec create_childSpec(Tuple::tuple(flower,non_negative_integer())) -> child_spec()
% 
% child_spec() = {Id,StartFunc,Restart,Shutdown,Type,Modules}
%	Id = term()
%	StartFunc = {M,F,A}
%	M = atom()
%	F = atom()
%  	A = [term()]
% 	Restart = permanent
% 	Shutdown = brutal_kill
% 	Type = worker
% 	Modules = [Module]
%  		Module = atom()
create_childSpec({flower,W}) when (W>0) and (is_integer(W)) ->
						LW=create_name({"w",0,W-1}),
						Workers = create_worker({flower,W},LW),
						Dispatcher = [{d, {flower_dispatcher, start, [d,LW,?TIMEOUT]},permanent, brutal_kill, worker, [flower_dispatcher]}],
						lists:append(Workers,Dispatcher).

% @doc create a list of worker (child_spec)
% @spec create_worker(Tuple::tuple(flower,non_negative_integer()),Lists::list(atom())) -> Lists::list(atom())
create_worker({flower,_},LW) ->
				{Worker,_} = lists:foldl(fun(CW,{Work,Nele}) -> {[work_CS(CW,[CW,d,?TIMEOUT])|Work],Nele+1} end,
															{[],1},
															LW
									),
				Worker.

work_CS(Id,Param) ->
		    {Id, {flower_worker, start, Param},permanent, brutal_kill, worker, [flower_worker]}.

% @doc Create a list of atom that contain the names formatted as PrefixFrom....PrefixTo (f.e. w1,w2,...,w9)
% spec create_name(Tuple::tuple(atom(),non_negative_integer(),non_negative_integer())) -> Lists::list(atom())
create_name({Prefix,From,To}) -> lists:map(fun(Ele)->get_atom(Prefix,Ele) end,lists:seq(From,To)).
				
get_atom(Name,Number) when is_integer(Number), 
						   Number >= 0 -> 
					erlang:list_to_atom(lists:flatten(io_lib:format("~s~w",[Name,Number]))).