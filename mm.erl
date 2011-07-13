
-module (mm).
-export  ([	new/1,
		  	newOf/2,
		  	mGet/2,
		  	mSet/3,
			identity/1,
			mDefault/1,
			transp/1,
			getType/1,
			isTransp/1,
			getSize/1,
			scalMatrixOp/3,
			matrixOp/3,
			matrixOp/2,
			export/2,
			import/1,
			getCol/2,
			getRow/2,
			saveToFile/2,
			loadFromFile/1,
			randomm/3]).
-import (array).
-record(property, {type=row,transp=false}). 
        
% @doc Check if MM is a right myMatrix
% @spec isMM(matrix()) -> bool()
isMM(MM) -> try MM of
						{matrix,_,_} -> true;
						_ -> throw(["Questa funzione lavora su myMatrix"])
				   catch
					 	_:X-> X
end. 

%% create a new myMatrix
new({Row,Col}) ->newOf({Row,Col},0).
newOf({Row,Col},Value) -> {matrix,#property{type=row,transp=false},newMatrix({Row,Col},Value)}.



newMatrix({Row,Col},Value) -> aofa({Row-1,Col},array:new(Row,{default,Value})).
 
%% create an array of array with default value of A
aofa({0,Col},A) -> array:set(0, array:new(Col,{default,array:default(A)}),A);
aofa({Row,Col},A) -> aofa({Row-1, Col},array:set(Row, array:new(Col,{default,array:default(A)}),A)).


%%get an element of a matrix
mGet({Row,Col},MM) -> case isMM(MM) of true -> array:get(Col,array:get(Row,getMatrix(MM))) end.

%%set an element of a matrix
mSet({Row,Col},Ele,MM) -> case isMM(MM) of true -> setMyMatrix(MM, 
										array:set(Row,
													array:set(Col,Ele,
																array:get(Row,getMatrix(MM))),getMatrix(MM))) end.
 
%%create a random matrix
randomm(Percent,{Min,Max},{Row,Col}) ->
	 randomize(round(Percent*(Row*Col)/100),{Min-1,Max},{Row,Col},new({Row,Col})).

randomize(0,_,_,MM) -> MM;
randomize(NCel,{Min,Max},{Row,Col},MM) -> 
 	 Val=random:uniform(Max-Min)+Min,
	 CRow=random:uniform(Row)-1,
	 CCol=random:uniform(Col)-1,
	 case mGet({CRow,CCol},MM)==mDefault(MM) of
		true -> %%io:fwrite("~p: ~p~n",[NCel,Val]),
				randomize(NCel-1,{Min,Max},{Row,Col},mSet({CRow,CCol},Val,MM));
				
		false ->%%io:fwrite("~p: duplicato~n",[NCel]),
				randomize(NCel,{Min,Max},{Row,Col},MM)
end.  

%%Create an identity matrix 
identity(Cord) -> setIdentity(Cord-1,new({Cord,Cord})).


%%Trasform a 0-matrix in identity
setIdentity(0,MM) ->  mSet({0,0},1,MM);
setIdentity(Cord,MM) -> setIdentity(Cord-1,mSet({Cord,Cord},1,MM)).

%%Get the default value of a matrix
mDefault(MM) -> array:default(getMatrix(MM)).

%%Set mymatrix from array of array
setMyMatrix({matrix,Prop,_},M) -> {matrix,Prop,M}.

%%Get the traspose of current MyMatrix
transp({matrix,#property{transp=Transp},M}) -> {matrix,#property{transp= not Transp},getTransp({matrix,#property{},M})}.

%%Get the transp of the matrix
getTransp(MM) -> array:map(fun(Pos,_) -> getCol(Pos,MM) end,array:new(getNumberOfCol(MM),{default,mDefault(MM)})).

%%Check if the matrix is transpose
isTransp({matrix,#property{transp=Transp},_}) -> Transp.

%%return the type of matrix
getType({matrix,#property{type=Type},_}) -> Type.

%%Return array of array from mymatrix
getMatrix({matrix,_,M}) -> M.

%%Return the {Row,Col} of myMatrix
getSize(MM)-> {getNumberOfRow(MM),getNumberOfCol(MM)}.

getNumberOfCol({matrix,_,M}) -> array:size(array:get(0,M)).
getNumberOfRow({matrix,_,M}) -> array:size(M).

%Check the dimension for the operation
dimIsTheRight(sum,MM1,MM2)-> getSize(MM1)==getSize(MM2);
dimIsTheRight(sub,MM1,MM2)-> getSize(MM1)==getSize(MM2);
dimIsTheRight(prod,MM1,MM2)-> getNumberOfRow(MM1)==getNumberOfCol(MM2).
dimIsTheRight(identity,MM) -> getNumberOfRow(MM)==getNumberOfCol(MM).

%% Op with scalar (Matrix (+,-,*,/) Scalar)
scalMatrixOp(Op,S,MM) -> array:map(fun(_,Ele) -> scalArrayOp(Op,Ele,S) end,getMatrix(MM)).

scalArrayOp(sum,S,A) -> array:map(fun(_,Ele)-> Ele+S end,A);
scalArrayOp(sub,S,A) -> array:map(fun(_,Ele)-> Ele-S end,A);
scalArrayOp(prod,S,A) -> array:map(fun(_,Ele)-> Ele*S end,A);
scalArrayOp(divs,S,A) -> array:map(fun(_,Ele)-> Ele/S end,A).


arrayOp(sum,A1,A2) -> array:map(fun(Pos,Ele)-> array:get(Pos,A2)+Ele end,A1);
arrayOp(sub,A1,A2) -> array:map(fun(Pos,Ele)-> array:get(Pos,A2)-Ele end,A1);
arrayOp(prod,A1,A2) -> array:foldl(fun(_,Val,Sum)-> Val+Sum end,0,array:map(fun(Pos1,Ele)->Ele*(array:get(Pos1,A2)) end,A1)).

% create a row of the result matrix
op1(R,MM1) -> array:map(fun(_,Ele) -> arrayOp(prod,R,Ele) end,getTransp(MM1)).

%% Posso usare MatrixOp per gestire le operazioni
matrixOp(identity,MM) -> case isMM(MM) of 
							true ->case dimIsTheRight(identity,MM) of
										true -> setIdentity(getNumberOfCol(MM)-1,MM);
										false -> throw(["Dim Errate"])
								   end
						 end.
						
matrixOp(Op,MM,MM1) ->	case isMM(MM) and isMM(MM1) of 
								true ->	case dimIsTheRight(Op,MM,MM1) of
											true -> setMyMatrix(MM,doMatrixOp(Op,MM,MM1));
											false -> throw(["Dim Errate"])  
								   		end
					  	end.

% operazioni tra matrici
doMatrixOp(prod,MM,MM1) -> array:map(fun(_,Ele) -> op1(Ele,MM1) end,getMatrix(MM));
doMatrixOp(Op,MM,MM1) -> array:map(fun(Pos,Ele) -> arrayOp(Op,Ele,array:get(Pos,getMatrix(MM1))) end,getMatrix(MM)).

%% return a col or a row
getCol(N,MM) -> case isMM(MM) of true -> array:map(fun(_,A) -> array:get(N,A) end,getMatrix(MM)) end.
getRow(N,MM) -> case isMM(MM) of true -> array:get(N,getMatrix(MM)) end.

%% save on file the erlang structure
saveToFile(File,MM) -> case isMM(MM) of true ->
	{ok,FD}=file:open(File,[write]),
	io:fwrite(FD,"~w~s",[MM,"."]),
	file:close(FD) end.             
	
%% load from file an erlang structure
loadFromFile(File) ->
	{ok,[MM]}=file:consult(File),
	MM.

    
%% export on a file a formatted rappresentation of matrix 
export(File,MM) -> case isMM(MM) of true ->
	Space=5,
	Format=lists:flatten(io_lib:format("~s~w~s",["\~",Space,"s\~w\~s\~n"])),
	{ok,FD}=file:open(File,[write]),
	io:fwrite(FD,"~s~n",["["]),
	array:map(fun(Pos,Ele) ->  case (Pos<(getNumberOfRow(MM)-1)) of true -> What=["",array:to_list(Ele),","];
															false -> What=["",array:to_list(Ele),""]
														end,
								io:fwrite(FD,Format,What) 
			  end,getMatrix(MM)),
	io:fwrite(FD,"~s~n",["]."]),
	file:close(FD) end.
	
	
%% import from a file a formatted rappresentation of matrix
import(File) ->
	{ok,[List]}=file:consult(File),
	{matrix,#property{type=row,transp=false},lofl2aofa(List)}.
	


lofl2aofa([H|[]]) -> lofl2aofa(H);
lofl2aofa([H|T]) -> try
					    array:from_list([lofl2aofa(H)|lofl2aofa(T)])
				  	catch
						error:_ -> array:from_list(lists:map(fun(E) -> array:from_list(E,0) end,[H|T]),0)
				  	end.
