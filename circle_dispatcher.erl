%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(circle_dispatcher).

-behaviour(gen_server).
-export([start/3]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
-import (smm).
-compile(export_all).

-record(state, {next=nil,tabmatrix=nil,tabworker=nil,listworker=[],sd=nil}). 


start(Server,Data,Timeout) ->   gen_server:start_link({local, Server}, ?MODULE, Data, [{timeout,Timeout}]).
stop(Server)  -> gen_server:call(Server, stop).

init({Next,TabMatrix,TabWorker,Worker,Sd}) ->  State=#state{	
															next=Next,
															tabmatrix=TabMatrix,
															tabworker=TabWorker,
															listworker=Worker,
															sd=Sd
														},
											{ok, State}.
											
% @doc Send the request of pivot to a worker
% @spec pivot(atom(),non_neg_integer(),{non_neg_integer(),non_neg_integer()}) -> none()
%
% State = 	{atom()}                                                     
pivot(Worker,NumberOfWork,Cord,M) -> gen_server:cast(Worker, {pivot,NumberOfWork,Cord,M}).

pivotOnRow(Server,Cord,NumberOfWork) -> gen_server:call(Server,{ppivot,Cord,NumberOfWork}).

sendOkRespond(NumberOfWork,Sd) -> gen_server:call(Sd,{termwork,NumberOfWork}). 
sendErrorRespond(NumberOfWork,Sd,Error) -> gen_server:call(Sd,{nottermwork,NumberOfWork,Error}).


% @doc divide the row in equal part depending on NumOfRow per part
% @spec divideRow(List:list(non_neg_integer()),non_neg_integer(),List::list(List::list(non_neg_integer()))) -> List::list(List::list(non_neg_integer()))
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
													
% @doc 
sendWork(NumberOfWork,{X,Y},TabMatrix,Worker,TabWorker) ->    
		   						 	%estraggo la matrice M
							 		[{NumberOfWork,OldState,M}]=ets:lookup(TabMatrix,NumberOfWork),
									%Sostituisco la matrice M con una nuova matrice vuota
									ets:insert(TabMatrix,{NumberOfWork,OldState,smm:new(smm:getDim(M))}),
									%prendo la riga di pivot
									Rpivot=smm:getRow({X},M),
									%io:fwrite("~s~p~n",["----riga di pivot",Rpivot]),
									%prendo i numeri delle righe esistenti tranne quella di pivot
									Rows=lists:delete(X,smm:nextRows(-1,smm:getElements(M))),
									%divido le righe in modo equo tra i worker
									{RowForWorker,NumOfActiveWorker}=rowForWorker(Rows,length(Worker)),
									%inserisco nella tabella di stato i worker attivi per questo lavoro
									ets:insert(TabWorker,{NumberOfWork,NumOfActiveWorker}),
									%trasformo la lista di righe in lista di matrici
									ListOfPart=lists:map(fun(R) -> smm:blockOfRow(R,M) end,RowForWorker),
									%aggiungo a tutte le matrici la riga di pivot
									ListOfMatrix=lists:map(fun(Part) -> smm:setMatrix(M,lists:keymerge(1,Part,Rpivot)) end,ListOfPart),
									%invio i lavori ai worker 
									lists:map(fun(Num)-> pivot(lists:nth(Num,Worker), %chi deve fare il lavoro
															   NumberOfWork,  %numero del lavoro
															   {X,Y}, %coordinate dell'elemento di pivot
															   lists:nth(Num,ListOfMatrix) %matrice parziale di lavoro
															  ) end,lists:seq(1,NumOfActiveWorker)).                             
			
			                 


handle_call({pivotok,NumberOfWork,{X,Y},M}, _From, State) ->
    								TableOfMatrix = State#state.tabmatrix,
									TableOfWorker = State#state.tabworker,
									Reply = {ok,"Thanks"}, 
									%prendo la matrice dalla tabella delle matrici
									[{NumberOfWork,OldState,StateM}]=ets:lookup(TableOfMatrix,NumberOfWork),
									%aggiungo le righe su cui ho fatto il pivot
									NewM = smm:setMatrix(StateM,lists:ukeymerge(1,smm:getElements(M),smm:getElements(StateM))),
									ets:insert(TableOfMatrix,{NumberOfWork,OldState,NewM}),
									[{NumberOfWork,NumberOfActiveWorker}]=ets:lookup(TableOfWorker,NumberOfWork),
									NewNumberOfActiveWorker=NumberOfActiveWorker-1,
									ets:insert(TableOfWorker,{NumberOfWork,NewNumberOfActiveWorker}),
									case NewNumberOfActiveWorker of
										0 ->io:fwrite("Passo la matrice ~p ~n",[State#state.next]),
											pivotOnRow(State#state.next,{X+1,Y+1},NumberOfWork),
											{reply, Reply, State};   
										_ -> {reply, Reply, State}   
									end;             
    
   
% @doc prepare the matrix for worker
handle_call({ppivot,{X,Y},NumberOfWork},_From, State) ->
	 								TabOfMatrix=State#state.tabmatrix,
									[{NumberOfWork,OldState,StateM}]=ets:lookup(TabOfMatrix,NumberOfWork),
									case (X==smm:getNCord(1,StateM)) or (Y==smm:getNCord(2,StateM)) of
										true -> sendOkRespond(NumberOfWork,State#state.sd),
												{reply,"ok",State};
										_ ->try smm:changeRowIfEZero({X,Y},StateM) of
											   	M -> ets:insert(TabOfMatrix,{NumberOfWork,OldState,M}),
													sendWork(NumberOfWork,{X,Y},TabOfMatrix,State#state.listworker,State#state.tabworker),
											 		{reply,"ok",State}
											catch 
												_:X -> smm:sendErrorRespond(NumberOfWork,State#state.sd,X),
													   {reply,"error",State}
										    end
									end;       

handle_call(stop, _From, State) ->
								    {stop, normal, stopped, State}.

	
	
		
handle_cast({newState,NewState},_) -> {noreply,NewState};
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _) -> {ok, State}.


    
