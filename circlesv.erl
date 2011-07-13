-module(circlesv).
-behaviour(supervisor).

-export([start/1]).
-export([init/1]).
-compile(export_all).
-define(TIMEOUT, 40000).

start(Struct) -> supervisor:start_link({local,?MODULE},?MODULE, Struct).
		
stop() -> Spec=supervisor:which_children(?MODULE),
		  lists:keymap(fun(Child) -> {terminate,supervisor:terminate_child(?MODULE,Child),Child} end, 1, Spec).
 
init(Struct) -> 
		  Child = create_childSpec(Struct),
		  {ok, {{one_for_one, 1, 60},Child}}.

create_childSpec({circle,D,W,TabMatrix,Sd}) when D > 1, W>0 ->
						LD=create_name({"d",0,D-1}),
						Dispatcher = create_dispatcher({circle,D,W,TabMatrix,Sd},LD),
						LW=create_name({"w",0,(D*W)-1}),
						Worker = create_worker({circle,D,W},LW),
						lists:append(Dispatcher,Worker).	


create_dispatcher({circle,D,W,TabMatrix,Sd},LD) -> 
					{Dispatcher,_}=lists:foldl(fun(CD,{Disp,Nele}) -> {[disp_CS(CD, %nome del processo
																    				[	CD, %Chi è
																	 					{lists:nth((Nele rem D)+1,LD), %chi c'è dopo di lui
																						TabMatrix, %la tabella che contiene i dati condivisi da elaborare
																						ets:new(CD,[public]), %tabella in cui salvare il numero di worker attivi sul lavoro
																	 					create_name({"w",(Nele-1)*W,(Nele-1)*W+W-1}),%chi sono i suoi worker 
																						Sd
																						}, 
																						?TIMEOUT
																					]
																				)|Disp],
																		Nele+1
																	  } end,{[],1},LD
											  ),
					Dispatcher.


								                      
disp_CS(Id,Param) ->
		    {Id, {circle_dispatcher, start, Param},permanent, brutal_kill, worker, [circle_dispatcher]}.

create_worker({circle,_,W},LW) ->
				{Worker,_} = lists:foldl(fun(CW,{Work,Nele}) -> {[work_CS(CW,[ CW,
																			   get_atom("d",(Nele-1) div W),
																			   ?TIMEOUT
																			 ])
																  |Work
																 ],Nele+1
																} end,
															{[],1},
															LW
									),
				Worker.
				                   
work_CS(Id,Param) ->
		    {Id, {circle_worker, start, Param},permanent, brutal_kill, worker, [circle_worker]}.

create_name({Prefix,From,To}) -> lists:map(fun(Ele)->get_atom(Prefix,Ele) end,lists:seq(From,To)).
				
				

get_atom(Name,Number) when is_integer(Number), 
						   Number >= 0 -> 
					erlang:list_to_atom(lists:flatten(io_lib:format("~s~w",[Name,Number]))).