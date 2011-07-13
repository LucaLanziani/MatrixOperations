% @author Guidotti Dennis Alessio <guidotti2@gmail.com>
% @author Lanziani Luca <luca@tulug.it>
% @author Loesch Thomas <thomas.loesch@me.com>
% @version 0.5

% @doc 	This module implement a worker that can perform a sum, a subtraction (cooming soon also product) of two matrix
-module(flower_worker).

-behaviour(gen_server).
-export([start/3]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3,stop/1]).
-import (smm).
-compile(export_all).

% @doc Start the gen_server for the worker of the flower struct
% @spec start(atom(),atom()) -> {ok,Pid} | ignore | {error,Error}
start(What,ReplyTo,Timeout) -> gen_server:start_link({local, What}, ?MODULE, ReplyTo, [{timeout,Timeout}]).
% @doc Stop the gen_server
% @spec stop(atom()) -> term()
stop(What)  -> gen_server:call(What, stop).

% @doc Send the result of the operation to a gen_server with a gen_server:call
% @spec reply(atom(),smm:matrix()) -> Reply
%
% Reply = term()
reply(Server,Result) -> gen_server:call(Server,{respond,Result}).    

% @doc Whenever a gen_server is started using gen_server:start/3,4 or gen_server:start_link/3,4, this function is called by the new process to initialize.
% @spec init(atom()) -> {ok,atom()}
init(ReplyTo) -> {ok, ReplyTo}.

% @doc Manage the request of stop
%@spec handle_call(atom(),From,State) -> Result
%
% From = 	{pid(),Tag}
% Result = 	{stop, normal, stopped, State}
% State = 	{atom()}
handle_call(stop, _From, Tab) ->
    {stop, normal, stopped, Tab}.

% @doc Manage the request of sum and send to the dispatcher the result
% @spec handle_cast(Tuple::tuple(atom(),smm:matrix(),smm:matrix(),non_neg_integer()),State) -> {noreply,State}
%
% State = 	{atom()}
handle_cast({sum,M1,M2,NWork},State) ->
	Reply = {NWork,smm:matrixOp(sum,M1,M2)},
	reply(State,Reply),
	{noreply,State};

% @doc Manage the request of subtraction and send to the dispatcher the result
% @spec handle_call(Tuple::tuple(atom(),smm:matrix(),smm:matrix(),non_neg_integer()),State) -> {noreply,State}
%
% State = 	{atom()}
handle_cast({sub,M1,M2,NWork},State) ->
	Reply = {NWork,smm:matrixOp(sub,M1,M2)},
	reply(State,Reply),
	{noreply,State};

% @doc Manage the request of subtraction and send to the dispatcher the result
% @spec handle_call(Tuple::tuple(atom(),smm:matrix(),smm:matrix(),non_neg_integer()),State) -> {noreply,State}
%
% State = 	{atom()}
handle_cast({prod,M1,M2,NWork},State) ->
	Reply = {NWork,smm:matrixOp(prod,M1,M2)},
	reply(State,Reply),
	{noreply,State};

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
% @doc This function is called by a gen_server when it is about to terminate. 
% It should be the opposite of Module:init/1 and do any necessary cleaning up. 
% When it returns, the gen_server terminates with Reason. The return value is ignored.
% @spec terminate(Reason,State) -> ok
% Reason = normal | shutdown | {shutdown,term()} | term()
% State = {atom()}
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _) -> {ok, State}.