% @author Guidotti Dennis Alessio <guidotti2@gmail.com>
% @author Lanziani Luca <luca@tulug.it>
% @author Loesch Thomas <thomas.loesch@me.com>
% @version 0.5

% @doc 	This module make you able to perform the operation of sum, subtraction and product on W different worker.

-module(flower_dispatcher).

-behaviour(gen_server).
-export([start/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3,operation4Row/3,stop/1]).
-import (smm).
-compile(export_all).

-record(state,{tabmatrix=nil,tabworker=nil,listworker=[],workN=nil}).

% @type from() = {pid(),Tag}
% @type state() = #state{tabmatrix,tabworker,listworker,workN}
%
%	tabmatrix = ets::tid()
%	tabworker = ets::tid()
%	listworker = Lists::list(atom())
%	workN = non_neg_integer()

% @doc Start the gen_server for the dispatcher of the flower struct
% @spec start(atom(),Lists::list(atom())) -> {ok,Pid} | ignore | {error,Error}

start(Server,Workers,Timeout) ->   gen_server:start_link({local, Server}, ?MODULE, {Server,Workers}, [{timeout,Timeout}]).
% @doc Stop the gen_server
% @spec stop(atom()) -> term()
stop(Server)  -> gen_server:call(Server, stop).
   
getWork(NWork) -> gen_server:call(?MODULE,{getNwork,NWork}).


% @doc Be you able to ask the execution of an operation on two matrix
% @spec operation4Row(atom(),smm:matrix(),smm:matrix()) -> ok
operation4Row(OpToDo,M1,M2) -> 	%io:fwrite("invio il lavoro",[]),
								gen_server:call(d,{OpToDo,M1,M2}).


% @doc Whenever a gen_server is started using gen_server:start/3,4 or gen_server:start_link/3,4, 
% this function is called by the new process to initialize.
% @spec init(Tuple::tuple(atom(),Lists::list(atom()))) -> {ok,state()} | {ok,state(),Timeout} | {ok,state(),hibernate}
init({Server,Workers}) -> 
							State = #state{
									tabmatrix = ets:new(Server,[public]),
									tabworker = ets:new(Server,[public]),
									listworker=Workers,
									workN = 0
							},
							{ok, State}.

% @private
% @doc Send a job to a worker
% @spec operation(atom(),non_neg_integer(),Tuple::tuple(atom(),smm:matrix(),smm:matrix())) -> ok
operation(Worker,NWork,{prod,M1,M2}) -> gen_server:cast(Worker, {prod,M1,M2,NWork});
operation(Worker,NWork,{sum,M1,M2}) -> gen_server:cast(Worker, {sum,M1,M2,NWork});
operation(Worker,NWork,{sub,M1,M2}) -> gen_server:cast(Worker, {sub,M1,M2,NWork}).
 

sendWork(WorkN,{prod,M1,M2},Workers,TabWorker) ->
	{Row4W,ActiveW} = rowForWorker(smm:nextRows(-1,smm:getElements(M1)),length(Workers)),
	ets:insert(TabWorker,{WorkN,ActiveW}),
	lists:map(fun(I)-> operation(
		lists:nth(I,Workers),
		WorkN,
		{
			prod,
			smm:setMatrix(M1,smm:blockOfRow(lists:nth(I,Row4W),M1)),
			M2
		}) end,
		lists:seq(1,ActiveW));


sendWork(WorkN,{Op,M1,M2},Workers,TabWorker) ->
	{Row4W,ActiveW} = rowForWorker(smm:getRowIndex(M1),length(Workers)),
	ets:insert(TabWorker,{WorkN,ActiveW}),
	lists:map(fun(I)-> operation(
		lists:nth(I,Workers),
		WorkN,
		{
			Op,
			smm:setMatrix(M1,smm:blockOfRow(lists:nth(I,Row4W),M1)),
			smm:setMatrix(M2,smm:blockOfRow(lists:nth(I,Row4W),M2))
		}) end,
		lists:seq(1,ActiveW)).

% @doc Manage the response of a worker
% @spec handle_call(Tuple::tuple(ok,Tuple::tuple(non_neg_integer(),smm:matrix())),from(),state()) -> Result 
%
% Result = 	{reply,String::string(),state()} |
%			{reply,smm:matrix(),state()}
handle_call({respond,{NWork,Result}}, _From, State) ->
	TableOfMatrix = State#state.tabmatrix,
	TableOfWorker = State#state.tabworker,
	[{NWork,Old,StateM}] = ets:lookup(TableOfMatrix,NWork),
	NewM = smm:setMatrix(StateM,lists:keymerge(1,smm:getElements(StateM),smm:getElements(Result))),
	ets:insert(TableOfMatrix,{NWork,Old,NewM}),
	[{NWork,ActiveW}] = ets:lookup(TableOfWorker,NWork),
	NActiveW = ActiveW-1,
	ets:insert(TableOfWorker,{NWork,NActiveW}),
	Reply= {ok,"Thanks"},
	case NActiveW of
		0 -> io:fwrite("~s~n~p~n",["Operazione eseguita. Il risultato è:",NewM]) ,
			{reply,"Finito",State};
		_ -> {reply,Reply,State}
	end;


handle_call({getNwork,NumberOfWork}, _From, State) -> 
    TableOfMatrix=State#state.tabmatrix,
    %inserisco la nuova matrice con il relativo numero di Lavoro
	NWork= case ets:lookup(TableOfMatrix,NumberOfWork) of
				[] -> "Work not present";
				[X] -> X
		   end,	
	Reply = {ok,NWork},
	%aggiorno nella risposta il numero del lavoro
    {reply, Reply, State};             


% @doc Manage the request of stop
%@spec handle_call(atom(),from(),state()) -> Result
%
% Result = 	{stop, normal, stopped, state()}

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

% @doc Manage the request of the operation of product on two matrix
%@spec handle_call(Tuple::tuple(prod,smm:matrix(),smm:matrix()),from(),state()) -> Result
%
% Result = 	{reply,String::string(),state()}
handle_call({prod,M1,M2},_From,State) -> 
	case controldim(prod,M1,M2) of
		true ->
			NWork = State#state.workN+1,
			TabMatrix = State#state.tabmatrix,
			ets:insert(TabMatrix,{NWork,{prod,M1,M2},smm:new({smm:getNCord(1,M1),smm:getNCord(2,M2)})}),
			sendWork(NWork,{prod,M1,M2},State#state.listworker,State#state.tabworker),
			{reply,"Lavoro avviato! Attendere...",State#state{workN=NWork}};
		false -> {reply,"Dimensioni della matrice errate",State}
	end;


% @doc Manage the request of an operation on two matrix
%@spec handle_call(Tuple::tuple(atom(),smm:matrix(),smm:matrix()),from(),state()) -> Result
%
% Result = 	{reply,String::string(),state()}
handle_call({OpToDo,M1,M2},_From,State) -> 
	case controldim(OpToDo,M1,M2) of
		true ->
			NWork = State#state.workN+1,
			TabMatrix = State#state.tabmatrix,
			ets:insert(TabMatrix,{NWork,{OpToDo,M1,M2},smm:new({smm:getNCord(1,M1),smm:getNCord(2,M2)})}),
			sendWork(NWork,{OpToDo,M1,M2},State#state.listworker,State#state.tabworker),
			{reply,"Lavoro avviato! Attendere...",State#state{workN=NWork}};
		false -> {reply,"Dimensioni della matrice errate",State}
	end.

% @doc manage the change of state of the server
% @spec handle_cast(Tuple::tuple(newState,state()),state()) -> {noreply,state()}
handle_cast({newState,NewState},_) -> {noreply,NewState};
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
% @doc This function is called by a gen_server when it is about to terminate. 
% It should be the opposite of Module:init/1 and do any necessary cleaning up. 
% When it returns, the gen_server terminates with Reason. The return value is ignored.
% @spec terminate(Reason,state()) -> ok
% Reason = normal | shutdown | {shutdown,term()} | term()
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _) -> {ok, State}.


% @doc divide the row in equal part depending on NumOfRow per part
% @spec divideRow(List::list(non_neg_integer()),non_neg_integer(),List::list(List::list(non_neg_integer())),List::list(List::list(non_neg_integer()))) -> List::list(List::list(non_neg_integer()))
divideRow([],_,_,Result) -> Result;
divideRow(ListOfRow,NumOfRow,Rest,Result) -> case Rest of 
														0 -> {H,T} = lists:split(NumOfRow,ListOfRow),
															 divideRow(T,NumOfRow,Rest,[H|Result]);
														_ -> {H,T} = lists:split(NumOfRow+1,ListOfRow),
															 divideRow(T,NumOfRow,Rest-1,[H|Result])
													end.
													
% @doc return a list of list of row
% [[1,3],[4,5],[6,7]] ecc ecc
% @spec rowForWorker([non_neg_integer()],non_neg_integer()) -> {List::list(List::list(non_neg_integer())),non_neg_integer()}
rowForWorker(ListOfRow,NumOfWorker) -> case length(ListOfRow)<NumOfWorker of
											true -> RowForWorker=divideRow(ListOfRow,1,0,[]),
													{RowForWorker,length(ListOfRow)};
											false -> NumOfRow=length(ListOfRow) div NumOfWorker,
													 Rest=(length(ListOfRow) rem NumOfWorker),
													 RowForWorker=divideRow(ListOfRow,NumOfRow,Rest,[]),
													 {RowForWorker,NumOfWorker}
									   end.
controldim(prod,M1,M2) -> 	{R1,_} = smm:getDim(M1),
							{_,C2} = smm:getDim(M2),
							R1 == C2;
controldim(_,M1,M2) -> smm:getDim(M1) == smm:getDim(M2).