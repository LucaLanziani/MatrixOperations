-module(launcher).

-behaviour(gen_server).

-import (circlesv, [start/1]).
-import (smm).
-import (dispatcher).

%% API
-export([start/0,sendwork/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
-compile(export_all).
-record(state, {workN=0,tableOfMatrix=nil,supervisor=nil}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
%Struct type {circle,Dispatcher,WorkerForDispatcher}
start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() -> gen_server:call(?MODULE, stop).
stopSupervisor() -> circlesv:stop().
%%====================================================================
%% gen_server callbacks
%%====================================================================
sendwork(What,M) -> gen_server:call(?MODULE,{What,M}).
getWork(NWork) -> gen_server:call(?MODULE,{getNwork,NWork}).
pivotOnRow(Server,Cord,NumberOfWork) -> gen_server:call(Server,{ppivot,Cord,NumberOfWork}).
startSupervisor({circle,D,W}) -> gen_server:call(?MODULE,{startsv,circle,D,W}).
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) -> State=#state {
						workN=0,
						tableOfMatrix=ets:new(?MODULE,[public])
						},
    			{ok, State}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({nottermwork,Work,Error}, _From, State) ->
	%ho terminato il lavoro N° Work
	io:fwrite("Lavoro ~p non terminato~nErrore: ~p",[Work,Error]),
	%estraggo la matrice dalla tabella condivisa
	[{_,{Op,Matrix},Error}]=ets:lookup(State#state.tableOfMatrix,State#state.workN),
	%mostro il risultato
	io:fwrite("-----~p non terminato-----~n-----DA----~n~p~n-----A-----~n~p~n",[Op,Matrix,Error]),
	{reply,"OK Thanks",State}; 
	
handle_call({termwork,Work}, _From, State) ->
	%ho terminato il lavoro N° Work
	io:fwrite("Terminato il lavoro ~p~n",[Work]),
	%estraggo la matrice dalla tabella condivisa
	[{N,{Op,Matrix},Result}]=ets:lookup(State#state.tableOfMatrix,State#state.workN),
	%mostro il risultato
	case Op of
		inverse -> NMatrix=smm:reduceIM(smm:getNCord(1,Matrix),Result),
					ets:insert(State#state.tableOfMatrix,{N,{Op,Matrix},NMatrix}),
				   	io:fwrite("-----Op: ~p-----~n-----DA----~n~p~n-----A-----~n~p~n",[Op,Matrix,NMatrix]),
				   {reply,"OK Thanks",State};
		syslin ->  NMatrix=smm:reduceSLM(smm:getNCord(1,Matrix),Result),
					ets:insert(State#state.tableOfMatrix,{N,{Op,Matrix},NMatrix}),
						io:fwrite("-----Op: ~p-----~n-----DA----~n~p~n-----A-----~n~p~n",[Op,Matrix,NMatrix]),
				   {reply,"OK Thanks",State};
			 _ ->   	io:fwrite("-----Op: ~p-----~n-----DA----~n~p~n-----A-----~n~p~n",[Op,Matrix,Result]),
					{reply,"OK Thanks",State}
	end;

handle_call({getNwork,NumberOfWork}, _From, State) -> 
    TableOfMatrix=State#state.tableOfMatrix,
    %inserisco la nuova matrice con il relativo numero di Lavoro
	NWork= case ets:lookup(TableOfMatrix,NumberOfWork) of
				[] -> "Work not present";
				[X] -> X
		   end,	
	Reply = {ok,NWork},
	%aggiorno nella risposta il numero del lavoro
    {reply, Reply, State};


handle_call({pivot,M}, _From, State) -> 
    Cord={0,0},
	%sto iniziando un nuovo lavoro quindi incremento il numero del lavoro attuale di uno
	NumberOfWork=State#state.workN+1,
	TableOfMatrix=State#state.tableOfMatrix,
    %inserisco la nuova matrice con il relativo numero di Lavoro
	ets:insert(TableOfMatrix,{NumberOfWork,{pivot,M},M}),
	%chiedo al primo dispatcher dell'anello di occuparsi del lavoro
	pivotOnRow(d0,Cord,NumberOfWork),	
	Reply = {ok,NumberOfWork},
	%aggiorno nella risposta il numero del lavoro
    {reply, Reply, State#state{workN=NumberOfWork}};

handle_call({inverse,M}, _From, State) ->          
    Cord={0,0},
	%sto iniziando un nuovo lavoro quindi incremento il numero del lavoro attuale di uno
	NumberOfWork=State#state.workN+1,
	TableOfMatrix=State#state.tableOfMatrix,
    %inserisco la nuova matrice con il relativo numero di Lavoro
	ets:insert(TableOfMatrix,{NumberOfWork,{inverse,M},M}),
	%chiedo al primo dispatcher dell'anello di occuparsi del lavoro
	pivotOnRow(d0,Cord,NumberOfWork),	
	Reply = {ok,NumberOfWork},
	%aggiorno nella risposta il numero del lavoro
    {reply, Reply, State#state{workN=NumberOfWork}};
                    
handle_call({syslin,M}, _From, State) ->          
    Cord={0,0},
	%sto iniziando un nuovo lavoro quindi incremento il numero del lavoro attuale di uno
	NumberOfWork=State#state.workN+1,
	TableOfMatrix=State#state.tableOfMatrix,
    %inserisco la nuova matrice con il relativo numero di Lavoro
	ets:insert(TableOfMatrix,{NumberOfWork,{syslin,M},M}),
	%chiedo al primo dispatcher dell'anello di occuparsi del lavoro
	pivotOnRow(d0,Cord,NumberOfWork),	
	Reply = {ok,NumberOfWork},
	%aggiorno nella risposta il numero del lavoro
    {reply, Reply, State#state{workN=NumberOfWork}};


handle_call({startsv,circle,D,W}, _From,State) ->
	circlesv:start({circle,D,W,State#state.tableOfMatrix,?MODULE}),
	Reply="ok",
	{reply, Reply, State};

handle_call(stop, _From, State) ->
	circlesv:stop(),
    {stop, normal, stopped, State}.


%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
                                                                                 