%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(circle_worker).

-behaviour(gen_server).
-export([start/3]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
-import (smm).
-compile(export_all).

% @doc Start the gen_server for the worker of the circle struct
% @spec start(atom(),atom()) -> {ok,Pid} | ignore | {error,Error}            
start(What,ReplyTo,Timeout) -> gen_server:start_link({local, What}, ?MODULE, ReplyTo, [{timeout,Timeout}]).

% @doc Stop the gen_server
% @spec stop(atom()) -> term()
stop(What)  -> gen_server:call(What, stop).

% @doc Send the result of the operation to a gen_server with a gen_server:call
% @spec reply(atom(),smm:matrix()) -> Reply
%
% Reply = term()  
reply(Server,{NumberOfWork,Cord,Pivot}) -> gen_server:call(Server,{pivotok,NumberOfWork,Cord,Pivot}).    
            
% @doc Whenever a gen_server is started using gen_server:start/3,4 or gen_server:start_link/3,4, this function is called by the new process to initialize.
% @spec init(atom()) -> {ok,atom()}    
init(ReplyTo) -> {ok, ReplyTo}.

% @doc Manage the request of stop
% @spec handle_call(atom(),From,State) -> Result
%
% From = 	{pid(),Tag}
% Result = 	{stop, normal, stopped, State}
% State = 	{atom()}     
handle_call(stop, _From, Tab) ->
    {stop, normal, stopped, Tab}.


% @doc Manage the request of pivot and send to the dispatcher the result
% @spec handle_cast(Tuple::tuple(atom(),non_neg_integer(),{non_neg_integer(),non_neg_integer()},smm:matrix()),State) -> {noreply,State}
%
% State = 	{atom()} 
handle_cast({pivot,NumberOfWork,Cord,M},State) ->
	Reply = {NumberOfWork,Cord,smm:pivot(Cord,M)},
	%io:fwrite("---Fatto il pivot su ---~n~p~n --- Ottenendo ---- ~n~p~n",[M,Reply]),
	reply(State,Reply),
	{noreply,State};
	      
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _) -> {ok, State}.


    
