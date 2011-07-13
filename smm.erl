% @author Guidotti Dennis Alessio <guidotti2@gmail.com>
% @author Lanziani Luca <luca@tulug.it>
% @author Loesch Thomas <thomas.loesch@me.com>
% @version 0.5

% @doc 	This module implement the common operation on Sparse Matrix.
%		You can use this module for declare ad manipulate sparse matrix 
-module (smm). 
-export ([		new/1,
			eGet/2,
			eSet/2,
			transp/1,
			isTransp/1,
			getDefault/1,
			getDim/1,
			getType/1,
			getElements/1,
			scalarMatrixOp/3,
			multipleESet/2,
			multipleEDelete/2,
			eDelete/2,
			getRow/2,
			getCol/2,
			matrixOp/3,
			arrayOp/4,
			pivot/2,
			identity/1,
			isIdentity/1,
			inverse/1,
			systemL/2,
			changeRow/3,
			nextRows/2,
			blockOfRow/2,
			block/3,
			det/1,
			gaussStep1/3,
			import/1,
			export/2,
  			exampleDet/0,
			example1/0,
			example2/0,
			example3/0,
            random/3,
            inferiorTriangular/1,
            superiorTriangular/1]).

-import(guardian).
-record(property, {type=row,transp=false,dim={},default=0}). 
-compile(export_all).


% @type cord() = Tuple::tuple(non_neg_integer()).
% @type typeM() = row|col.
% @type element() = Tuple::tuple(cord(),number()).
% @type elements() = [element()].
% @type transp() = true | false.
% @type property() = #property{}.
% @type matrix() = {matrix,Type::property(),elements()}.   
% @type scalop() = prod | divs.
% @type rowop() = sum | sub.
% @type matrixop() = sum | sub | prod.
% @type whereM() = bef | aft.
         
% myprototype() -> {matrix, #property{type=_,transp=_},{array,_,_,_,_}}.

%% @doc create a new matrix
%% @spec new(cord()) -> matrix()
new(Dim) -> case guardian:is_cord(Dim) of
                true -> newOf(Dim,0);
                false -> throw(["new: Formato dimensione non corretta"]) end.
%% @private
%% @doc create a new matrix with the value of default = Value (currently implementation of the operations is based only on the value of default=0)
%% @spec newOf(non_neg_integer(),cord()) -> matrix()
newOf(Dim,Value) when is_tuple(Dim),is_number(Value) -> Prop=#property{type=row,transp=false,dim=Dim,default=Value},
					{matrix,Prop,[]}.

% @doc Check if the coordinate is in the right range
% @spec isCordIn(cord(),cord()) -> bool()
isCordIn(CCord,Dim) ->  case guardian:is_cord(CCord) and guardian:is_cord(Dim) of
                            true ->LCCord=tuple_to_list(CCord),
                                LDim=tuple_to_list(Dim),
                                case length(LCCord)=<length(LDim) of
                                    true ->	 case lists:foldl(fun(X, {Cond,Acc}) -> Result=(Cond and  (X>=0) and (X<lists:nth(Acc,LDim))),
																				{Result,Acc+1} 
                                                               end,{true,1}, LCCord) of
																				{true,_} -> true;
																				{_,_} -> throw(["isCordIn: Coordinate fuori dall'intervallo consentito"])
                                             end;
                                    _ -> throw(["isCordIn: Attenzione il numero delle coordinate eccede la dimensione della matrice"])
                                end;
                            _ -> throw(["isCordIn: Errore i parametri passati non sono corretti"]) end.
% @doc Get an element of a matrix
% @spec eGet(cord(),matrix()) -> number()
eGet(CCords,MM) -> case (guardian:is_matrix(MM) and isCordIn(CCords,getDim(MM))) of 
                        true  -> case lists:keysearch(CCords,1,getElements(MM)) of
                                        {value,{_,Ele}} -> Ele;
										false -> getDefault(MM)
								 end;
                        _ -> throw(["eGet: Errore i parametri passati non sono corretti"]) end.
																
% @doc Delete an element from matrix
% @spec eDelete(cord(),matrix()) -> matrix()
eDelete(CCords,MM) -> case guardian:is_cord(CCords) and guardian:is_matrix(MM) of
                            true-> setMatrix(MM,lists:keydelete(CCords,1,getElements(MM)));
                            _-> throw(["eDelete: Errore i parametri passati non sono corretti"]) end.

%% @doc Insert a list of elements in the matrix
%% @spec multipleESet(elements(), matrix()) -> matrix()
multipleESet(Elements,MM) -> case guardian:is_elements(Elements) and guardian:is_matrix(MM) of
                                true -> lists:foldl(fun(Cele,Acc) -> eSet(Cele,Acc) end, MM , Elements);
                                _-> throw(["multipleESet: Errore i parametri passati non sono corretti"]) end.
% @doc Delete a list of elements from matrix
% @spec multipleEDelete(elements(),matrix()) -> matrix()
multipleEDelete(Elements,MM) -> case guardian:is_elements(Elements) and guardian:is_matrix(MM) of
                                    true -> lists:foldl(fun({Cord,_},Acc) -> eDelete(Cord,Acc) end, MM, Elements);
                                    _-> throw(["multipleEDelete: Errore i parametri passati non sono corretti"]) end.
% @doc Set or Insert an element in a matrix
% @spec eSet(element(),matrix()) -> matrix()
eSet({CCords,Ele},MM) -> case guardian:is_matrix(MM) and isCordIn(CCords,getDim(MM))of
                            true ->
                                case lists:keysearch(CCords,1,getElements(MM)) of
                                    {value,{_,_}} -> setMatrix(MM,lists:keystore(CCords,1,getElements(MM),{CCords,Ele}));
                                    false -> setMatrix(MM,lists:merge(fun({Cord1,_},{Cord2,_}) -> Cord1<Cord2 end,getElements(MM),[{CCords,Ele}]))
                                end;
                            _ -> throw(["eSet: Errore i parametri passati non sono corretti"]) end.
							

% @doc Get a list of element and return a matrix
% @spec setMatrix(matrix(),elements()) -> matrix()
%
setMatrix({matrix,Prop,_},M) -> case guardian:is_property(Prop) of
                                    true ->{matrix,Prop,M};
                                    _ -> throw(["setMatrix: Errore i parametri passati non sono corretti"]) end.

% @doc Get the transposed of current Matrix
% @spec transp(matrix()) -> matrix()
transp({matrix,Prop,M}) ->  case guardian:is_property(Prop) and guardian:is_elements(M) of
                               true-> case size(Prop#property.dim) of
                                        2-> {X,Y}=Prop#property.dim,
                                            Dim={Y,X},
                                            {matrix,Prop#property{dim=Dim,transp= not Prop#property.transp},transpElements({matrix,Prop,M})};
                                        _ -> throw(["funzione implementata solo per matrici bidimenzionali"])
                                      end;
                               _ -> throw(["transp: Errore i parametri passati non sono corretti"]) end.

transpElements(MM) -> case guardian:is_matrix(MM) of
                         true->case size(getDim(MM)) of
                                    2 -> lists:keysort(1,lists:keymap(fun({X,Y}) -> {Y,X} end, 1, getElements(MM)));
                                    _ -> throw(["transpElements: funzione implementata solo per matrici bidimenzionali"])
                               end;
                         _ -> throw(["transpElements: Errore i parametri passati non sono corretti"]) end.

% @doc Check if the matrix is transposed
% @spec isTransp(matrix()) -> transp()
isTransp({matrix,#property{transp=Transp},_}) -> case is_boolean(Transp) of
                                                        true ->Transp;
                                                        _ -> throw(["isTransp: Errore i parametri passati non sono corretti"]) 
												 end.

% @doc Get the default value of Matrix
% @spec getDefault(matrix()) -> number()
getDefault({matrix,#property{default=Value},_}) -> case is_number(Value) of 
                                                        true ->Value;
                                                        _ -> throw(["getDefault: Errore i parametri passati non sono corretti"]) 
													end.

% @doc Get the dimension of Matrix
% @spec getDim(matrix()) -> cord()
getDim({matrix,#property{dim=Dim},_}) -> case guardian:is_cord(Dim) of
                                            true->Dim;
                                            _ -> throw(["getDim: Errore i parametri passati non sono corretti"]) 
										end.


% @doc Get the dimension of coordinate N
% @spec getNCord(non_neg_integer(),matrix()) -> non_neg_integer
getNCord(N,M) -> case is_integer(N) and (N>=0) and (guardian:is_matrix(M)) of 
                    true -> lists:nth(N,tuple_to_list(getDim(M)));
                    _ -> throw(["getNCord: Errore i parametri passati non sono corretti"]) end.

% @doc Get the type of matrix
% @spec getType(matrix()) -> typeM()
getType({matrix,#property{type=Type},_}) -> case guardian:is_typeM(Type) of 
                                                true ->Type;
                                                _ -> throw(["getType: Errore i parametri passati non sono corretti"]) 
											end.

% @doc Get a list of Element
% @spec getElements(matrix()) -> elements()
getElements({matrix,_,M}) -> case guardian:is_elements(M) of
                                    true ->M;
                                    _ -> throw(["getElements: Errore i parametri passati non sono corretti"]) 
							 end.

% @doc Create a random matrix with density=Percent% of dimension Row,Col with range of elements between Min and Max
% @spec random(non_neg_int(),Tuple::tuple(number(),number()),cord()) -> matrix()
random(Percent,{Min,Max},{Row,Col}) -> case is_number(Percent) and (Percent>=0)
                                            and is_number(Min) and is_number(Max)
                                            and (Max>=Min) and guardian:is_2cord({Row,Col}) of
                                                true->randomize(round(Percent*(Row*Col)/100),
																{Min-1,Max},
																{Row,Col},
																new({Row,Col})
															    );
                                                _ -> throw(["random: Errore i parametri passati non sono corretti"]) 
										end.
randomize(0,_,_,MM) -> MM;
randomize(NCel,{Min,Max},{Row,Col},MM) ->
 	 Val=random:uniform(Max-Min)+Min,
	 CRow=random:uniform(Row)-1,
	 CCol=random:uniform(Col)-1,
	 case (eGet({CRow,CCol},MM)==getDefault(MM)) of
		true -> case Val of 
					0 -> NewM=MM;
					_ -> NewM=eSet({{CRow,CCol},Val},MM)
				end,
				randomize(NCel-1,{Min,Max},{Row,Col},NewM);
		false ->%%io:fwrite("~p: duplicato~n",[NCel]),
				randomize(NCel,{Min,Max},{Row,Col},MM)
end.

% @doc Execute the op on a matrix with a scalar
% @spec scalarMatrixOp(scalop(),number(),matrix()) -> matrix()
scalarMatrixOp(Op,S,MM) -> case guardian:is_scalop(Op) and is_number(S) and guardian:is_matrix(MM) of
								true-> setMatrix(MM,scalElementsOp(Op,S,getElements(MM)));
								_ -> throw(["scalarMatrixOp: Errore i parametri passati non sono corretti"]) 
							end.

% @doc Execute the op on Elements with a scalar
% @spec scalElementsOp(scalop(),number(),elements()) -> elements()
scalElementsOp(prod,S,M) -> case is_number(S) and guardian:is_elements(M) of
				true->lists:keymap(fun(Ele) -> Ele*S end,2,M);
				_->throw(["scalElementsOp: Errore i parametri passati non sono corretti"]) end;
scalElementsOp(divs,S,M) -> case is_number(S) and guardian:is_elements(M) of
				true->  lists:keymap(fun(Ele) -> Ele/S end,2,M);
				_->throw(["scalElementsOp: Errore i parametri passati non sono corretti"]) end.

% @doc Get an identity matrix
% @spec identity(pos_integer) -> matrix()
identity(Dim) -> case is_integer(Dim) and (Dim>0) of
			true->lists:foldl(fun(Ele,Acc) -> eSet({{Ele,Ele},1},Acc) end,new({Dim,Dim}),lists:seq(0,Dim-1));
			_->throw(["identity: Errore i parametri passati non sono corretti"]) end.
%
%CCord [n1,n2,n3]
% @doc Get the N° row of matrix
% @spec getRow(Lists::list(non_neg_integer()),matrix()) -> elements()
getRow(CCord,MM) -> case guardian:is_matrix(MM) of
			true ->lists:filter(fun({Cord,_}) ->lists:prefix(tuple_to_list(CCord),tuple_to_list(Cord)) end, getElements(MM));
			_->throw(["getRow: Errore i parametri passati non sono corretti"]) end.

% @doc Get the N° col of matrix
% @spec getCol(Lists::list(non_neg_integer()),matrix()) -> elements()
getCol(CCord,MM) -> case guardian:is_matrix(MM) of
			true->lists:filter(fun({Cord,_}) ->lists:suffix(tuple_to_list(CCord),tuple_to_list(Cord)) end, getElements(MM));
			_->throw(["getCol: Errore i parametri passati non sono corretti"]) end.
			
% @doc Pivot on a cord element
% @spec pivot({non_neg_integer(),non_neg_integer()},matrix()) -> matrix()
pivot({NRow,NCol},MM) -> case guardian:is_2cord({NRow,NCol}) and guardian:is_matrix(MM) and (eGet({NRow,NCol},MM)/=0) of 
						 true-> 
								Rpivot = scalElementsOp(divs,eGet({NRow,NCol},MM),getRow({NRow},MM)),
								ListOfRow=lists:delete(NRow,getRowIndex(MM)),
								ListOfElements=lists:foldl(fun(CRow,Acc) ->ActRow =  getRow({CRow},MM),
                                        				   				   case  eGet({CRow,NCol},MM) of
                                        											0 -> SubRow = [];
                                            										X -> SubRow = scalElementsOp(prod,X,Rpivot)
                                        								   end,
																		   NewRow = rowOp(sub,CRow,ActRow,0,SubRow,0),			
																		   lists:keymerge(1,NewRow,Acc)           	
														   end,Rpivot,ListOfRow),
								multipleESet(ListOfElements,new(getDim(MM)));
				_->throw(["pivot: Errore i parametri passati non sono corretti"]) end.
 


% @doc Exec the operation Op on two Row      
% @spec rowOp(rowop(),non_neg_integer(),elements(),number(),elements(),number()) -> elements()
rowOp(Op,CRow,Row1,DefM1,Row2,DefM2)->case guardian:is_rowop(Op) and is_integer(CRow) and (CRow>=0)
                                         and guardian:is_elements(Row1) and is_number(DefM1)
                                         and guardian:is_elements(Row2) and is_number(DefM2) of
                                            true->doRowOp(Op,CRow,Row1,DefM1,Row2,DefM2,[]);
                                            _->throw(["rowOp: Errore i parametri passati non sono corretti"]) end.
doRowOp(_,_,[],_,[],_,R) -> R;
doRowOp(Op,CRow,[{Cord,Ele}|T],DMM1,[],DMM2,R) ->  doRowOp(Op,CRow,T,DMM1,[],DMM2,
															lists:merge(fun({Cord3,_},{Cord4,_}) -> Cord3<Cord4 end, 
																		R,
																		eop(Op,Cord,Ele,DMM2)
																	    )
														   );
doRowOp(Op,CRow,[],DMM1,[{{_,Y},Ele}|T],DMM2,R) -> doRowOp(Op,CRow,[],DMM1,T,DMM2,
														   lists:merge(fun({Cord3,_},{Cord4,_}) -> Cord3<Cord4 end, 
														   			   R,
																	   eop(Op,{CRow,Y},DMM1,Ele)
																	  )
														  );
doRowOp(Op,CRow,[{{X1,Y1},Ele1}|T1],DMM1,[{{X2,Y2},Ele2}|T2],DMM2,R) -> 
						  case Y1==Y2 of
								true -> doRowOp(Op,CRow,T1,DMM1,T2,DMM2,
													lists:merge(fun({Cord3,_},{Cord4,_}) -> Cord3<Cord4 end, 
																R,
															
																eop(Op,{CRow,Y1},Ele1,Ele2)
																)
												  );
								false -> case Y1<Y2 of
											   true -> doRowOp(Op,CRow,T1,DMM1,[{{X2,Y2},Ele2}|T2],DMM2,
																	lists:merge(fun({Cord3,_},{Cord4,_}) -> Cord3<Cord4 end, 
																				R,
																				eop(Op,{CRow,Y1},Ele1,DMM2)
																				)
														 );
											   false -> doRowOp(Op,CRow,[{{X1,Y1},Ele1}|T1],DMM1,T2,DMM2,
																   lists:merge(fun({Cord3,_},{Cord4,_}) -> Cord3<Cord4 end, 
																				R,
																				eop(Op,{CRow,Y2},DMM1,Ele2)
																			   )
																  )
										 end
						  end.

% @doc Do Prod Sum or Sub of two matrix
% @spec matrixOp(matrixop(),matrix(),matrix()) -> matrix()
matrixOp(prod,MM,MM1) -> case guardian:is_matrix(MM) and guardian:is_matrix(MM1) of
                                true->setMatrix(MM,doMatrixOp(prod,MM,getDefault(MM),MM1,getDefault(MM1),[]));
                                _->throw(["matrixOp: Errore i parametri passati non sono corretti"]) end;
matrixOp(Op,MM,MM1) ->  case guardian:is_matrix(MM) and guardian:is_matrix(MM1) and guardian:is_matrixop(Op) of
                                true->setMatrix(MM,doMatrixOp(Op,getElements(MM),getDefault(MM),getElements(MM1),getDefault(MM1),[]));
                                _->throw(["matrixOp: Errore i parametri passati non sono corretti"]) end.

% @private
% @doc auxiliar function for the calculation of operations
% @spec doMatrixOp(matrixop(),matrix(),number(),matrix(),number(),elements()) -> elements()
doMatrixOp(prod,MM1,_,MM2,_,R) -> case guardian:is_matrix(MM1) and guardian:is_matrix(MM2) and guardian:is_elements(R) of
                                        true->lists:foldl(fun(CRow,Acc) -> lists:merge(Acc,doProd(getRow({CRow},MM1),MM2,CRow)) end,R,getRowIndex(MM1));
                                        _->throw(["doMatrixOp: Errore i parametri passati non sono corretti"]) end;
doMatrixOp(Op,Row1,_,Row2,_,R) -> case guardian:is_rowop(Op) and guardian:is_elements(Row1) and guardian:is_elements(Row2) and guardian:is_elements(R) of
                                        true->MixRow = lists:keymerge(1,Row1,Row2),
                                              lists:foldl(fun({Cord,Ele},Acc) ->case lists:keysearch(Cord, 1, Acc) of
                                                                                        {value, {Cord,Val}} -> case eop(Op,Cord,Val,Ele) of
                                                                                                                    []->lists:keydelete(Cord,1,Acc);
                                                                                                                    [Value]->lists:keystore(Cord,1,Acc,Value)
                                                                                                                end;
                                                                                        false -> lists:keymerge(1,Acc,[{Cord,Ele}])
                                                                                end
													end,R,MixRow);
                                        _->throw(["doMatrixOp: Errore i parametri passati non sono corretti"]) end.

% @doc operation on singular elements
% @spec eop(rowop(),cord(),number(),number()) -> elements()
eop(sum,Cord,E,E1) -> case guardian:is_2cord(Cord) and is_number(E) and is_number(E1) of
                            true-> case E+E1 of
                                        0.0 -> [];
                                        0 -> [];
                                        X -> [{Cord,X}]
                                    end;
                            _->throw(["eop: Errore i parametri passati non sono corretti"]) end;
eop(sub,Cord,E,E1) -> case guardian:is_2cord(Cord) and is_number(E) and is_number(E1) of
                            true-> case E-E1 of
                                    0 -> [];
                                    0.0 -> [];
                                    X -> [{Cord,X}]
                                    end;
                            _->throw(["eop: Errore i parametri passati non sono corretti"]) end.

% @doc Do the product from a Row and a Matrix 
% @spec doProd(elements(),matrix(),non_neg_integer()) -> elements()
doProd(A,MM1,CRow) -> case guardian:is_elements(A) and guardian:is_matrix(MM1) and is_integer(CRow) and (CRow>=0) of
                            true->lists:foldl(fun(CCol,Acc)-> Ris =  arrayOp(prod,A,getCol({CCol},MM1),CCol),
													case Ris of
														0 -> Acc;
														_ -> lists:merge([{{CRow,CCol},Ris}],Acc) end end,[],getColIndex(MM1));
                            _->throw(["doProd: Errore i parametri passati non sono corretti"]) end.

% @doc Do the product from a row and a column
% @spec arrayOp(atom(),elements(),elements(),non_neg_integer()) -> number()
arrayOp(prod,A,A1,CCol) -> case guardian:is_elements(A) and guardian:is_elements(A1) and is_integer(CCol) and (CCol>=0) of
                                true->lists:foldl(fun({{_,Y},Ele},Acc) -> case lists:keysearch({Y,CCol},1,A1) of
                                                                                false -> Acc;
                                                                                {value,{_,Ele1}} -> Acc+(Ele*Ele1)
                                                                          end
                                                    end,0,A);
                                _->throw(["arrayOp: Errore i parametri passati non sono corretti"]) end.

% @doc Get a List of row after N
% @spec nextRows(integer(),elements()) -> lists()
nextRows(N,M) ->case is_integer(N) and guardian:is_elements(M) of
						true->lists:foldl(fun({{X,_},_},Acc)-> case X>N of
										   							true -> lists:umerge([X], Acc);
																	_ -> Acc
																end
										  end,[],M);
						_->throw(["nextRows: Errore i parametri passati non sono corretti"]) 
				 end.

% @doc Get a list of number of row of Matrix 
% @spec getRowIndex(matrix()) -> lists()
getRowIndex(MM) -> case guardian:is_matrix(MM) of
			true->lists:seq(0,lists:nth(1,tuple_to_list(getDim(MM)))-1);
			_->throw(["getRowIndex: Errore i parametri passati non sono corretti"]) end.

% @doc Get a list of number of col of Matrix 
% @spec getColIndex(matrix()) -> lists()
getColIndex(MM) -> case guardian:is_matrix(MM) of
			true->lists:seq(0,lists:nth(2,tuple_to_list(getDim(MM)))-1);
			_->throw(["getColIndex: Errore i parametri passati non sono corretti"]) end.


% @doc Get the inverse of a matrix
% @spec inverse(matrix()) -> matrix()
inverse(MM) -> case guardian:is_matrix(MM) of
			true-> case getDim(MM) of
                 	   		{Cord,Cord} ->  ExpanseMatrix=expandForInverse(MM),
											MatrixPivoted=pivotOnDiag(0,Cord-1,ExpanseMatrix),
											reduceIM(Cord,MatrixPivoted);
					_-> throw(["inverse: La matrice non è quadrata"])
                	      end;
                	_->throw(["inverse: Errore i parametri passati non sono corretti"]) end.

% @doc Resolve a system linear of equation
% @spec systemL(matrix(),matrix()) -> matrix()
systemL(MM,MTN)-> case guardian:is_matrix(MM) and guardian:is_matrix(MTN) of
			true->case getDim(MM) of
                    			{Cord,Cord} ->reduceSLM(Cord,pivotOnDiag(0,Cord-1,expandForSL(MM,MTN)));
                     			_-> throw(["systemL: La matrice non è quadrata"])
                		end;
                	_->throw(["systemL: Errore i parametri passati non sono corretti"]) end.
                	
% @doc Return the matrix MM with concatenate the identity matrix
% @spec expandForInverse(matrix()) -> matrix()
expandForInverse(MM) -> case guardian:is_matrix(MM) of
				true->	{Cord,Cord}=getDim(MM),
                        		multipleESet(lists:keymerge(1,getElements(MM),expandM({0,Cord},identity(Cord))),new({Cord,Cord*2}));
                        	_->throw(["expandForInverse: Errore i parametri passati non sono corretti"]) end.
% @doc Return the matrix MM with concatenate the matrix MTN
% @spec expandForSL(matrix(),matrix()) -> matrix()
expandForSL(MM,MTN) -> case guardian:is_matrix(MM) and guardian:is_matrix(MTN) of
				true->	{Cord,Cord}=getDim(MM),
                        		multipleESet(lists:keymerge(1,getElements(MM),expandM({0,Cord},MTN)),new({Cord,Cord+1}));
                        	_->throw(["expandForSL: Errore i parametri passati non sono corretti"]) end.
% @doc Shift the elements of a matrix
% @spec expandM(Tuple::tuple(non_neg_integer(),non_neg_integer()),matrix()) -> elements()
expandM({R,C},MM)->case guardian:is_2cord({R,C}) and guardian:is_matrix(MM) of
			true->lists:map(fun({{Row,Col},Value})->{{Row+R,Col+C},Value} end,getElements(MM));
			_->throw(["expandM: Errore i parametri passati non sono corretti"]) end.

% @doc Return a submatrix of matrix
% @spec subMatrix(Tuple::tuple(non_neg_integer(),non_neg_integer()),Tuple::tuple(non_neg_integer(),non_neg_integer()),matrix()) -> elements()
subMatrix({IRow,ICol},{FRow,FCol},MM)->case guardian:is_2cord({IRow,ICol}) and guardian:is_2cord({FRow,FCol}) and guardian:is_matrix(MM) of
						true->SubElements = lists:foldl(fun({{CRow,CCol},Value}, Acc)->
											case (IRow=<CRow) and (ICol=<CCol) and (FRow>=CRow) and (FCol>=CCol) of
        													true -> lists:keymerge(1,[{{CRow-IRow,CCol-ICol},Value}],Acc);
        													_ -> Acc 
        										end
        									end,
										[],getElements(MM)),
							multipleESet(SubElements,new({FRow-IRow+1,FCol-ICol+1}));
						_->throw(["subMatrix: Errore i parametri passati non sono corretti"]) end.

% @doc Return a list of elements of a matrix with only the specified rows
% @spec blockOfRow(Lists::list(non_neg_integer()),matrix()) -> elements()
blockOfRow(ListOfRow,M) -> case is_list(ListOfRow)  of
				true->lists:foldl(fun(CRow,Acc) -> lists:keymerge(1,getRow({CRow},M),Acc) end, [],ListOfRow);
				_->throw(["blockOfRow: Errore i parametri passati non sono corretti"]) end.  

% @doc Return a list of elements of a matrix with only the specified col
% @spec blockOfCol(Lists::list(non_neg_integer()),matrix()) -> elements()
blockOfCol(ListOfCol,M) -> case is_list(ListOfCol) of
				true->lists:foldl(fun(CCol,Acc) -> lists:keymerge(1,getCol({CCol},M),Acc) end, [],ListOfCol);
				_->throw(["blockOfCol: Errore i parametri passati non sono corretti"]) end. 

% @doc Return a set of rows and columns of a matrix
% @spec block(cord(),cord(),matrix()) -> elements()
block({IRow,ICol},{FRow,FCol},MM)-> case guardian:is_2cord({IRow,ICol}) and guardian:is_2cord({FRow,FCol}) and guardian:is_matrix(MM) of
						true->SubElements = lists:filter(fun({{CRow,CCol},_})-> (IRow=<CRow) and (ICol=<CCol) and (FRow>=CRow) and (FCol>=CCol) end,getElements(MM)),
							multipleESet(SubElements,new(getDim(MM)));
						_->throw(["block: Errore i parametri passati non sono corretti"]) end.

% @doc Return the solution of equation
% @spec reduceSLM(non_neg_integer(),matrix()) -> matrix()
reduceSLM(Cord,MM)->case is_integer(Cord) and (Cord>=0) and guardian:is_matrix(MM) of
				true->SubMatrix=subMatrix({0,0},{Cord-1,Cord-1},MM),
					case isIdentity(SubMatrix) of
   							true ->subMatrix({0,Cord},{Cord,Cord+1},MM);
   							_ -> throw(["reduceSLM: Non esiste risoluzione al sistema"]) 
					end;
				_->throw(["reduceSLM: Errore i parametri passati non sono corretti"]) end.

% @doc Return the inverse matrix
% @spec reduceIM(non_neg_integer(),matrix()) -> matrix()
reduceIM(Cord,MM)->case is_integer(Cord) and (Cord>=0) and guardian:is_matrix(MM) of
				true->SubMatrix = subMatrix({0,0},{Cord-1,Cord-1},MM),
					case isIdentity(SubMatrix) of
   						true ->subMatrix({0,Cord},{Cord,Cord*2},MM);
   						_ -> throw(["reduceIM: Non esiste la matrice inversa"])
					end;
				_->throw(["reduceIM: Errore i parametri passati non sono corretti"]) end.

% @doc Return true if is a identity matrix
% @spec isIdentity(matrix) -> bool()
isIdentity(M)-> case guardian:is_matrix(M) of
			true->case length(getElements(M))==getNCord(1,M) of
					true -> doIsIdentity(getNCord(1,M),getElements(M));
					false -> false
				end;
			_->throw(["isIdentity: Errore i parametri passati non sono corretti"]) end.

% @private
% @doc auxiliar method for the check if a matrix is an identity matrix: return true if only the elements with the same coordinate are =1
% @spec doIsIdentity(non_neg_integer,matrix) -> bool()
doIsIdentity(1,M) -> case lists:nth(1,M) of
								{{0,0},1} -> true;
                                {{0,0},1.0} -> true;
								_ -> false
					 end;
					
doIsIdentity(Cord,M) -> case lists:nth(Cord,M) of
								{{X,X},1.0} -> doIsIdentity(Cord-1,M);
                                {{X,X},1} -> doIsIdentity(Cord-1,M);
								_ -> false
						end.

% @doc Change two row on Matrix
% @spec changeRow(non_neg_integer(),non_neg_integer(),matrix()) -> matrix()
changeRow(NRow1,NRow2,MM) ->case is_integer(NRow1) and (NRow1>=0) and is_integer(NRow2) and (NRow2>=0) and guardian:is_matrix(MM) of
                                true->MixRow = lists:keymerge(1,getRow({NRow1},MM),getRow({NRow2},MM)),
                                         MM1=multipleEDelete(MixRow,MM),
                                         lists:foldl(fun({{X,Y},Ele},Acc) ->
                                                         case X of
                                                            NRow1 -> eSet({{NRow2,Y},Ele},Acc);
                                                            NRow2 -> eSet({{NRow1,Y},Ele},Acc)
                                                         end
                                                 end
                                            ,MM1,MixRow);
                                _->throw(["changeRow: Errore i parametri  passati non sono corretti"]) end.

% @doc call the pivot on the main diagonal of the matrix
% @spec pivotOnDiag(non_neg_integer(),non_neg_integer(),matrix()) -> matrix()
pivotOnDiag(Cord,Cord,MM)-> case is_integer(Cord) and (Cord>=0) and guardian:is_matrix(MM) of
                                true->pivot({Cord,Cord},
									  		changeRowIfEZero({Cord,Cord},MM)
										   );
                                _->throw(["pivotOnDiag: Errore i parametri passati non sono corretti"]) end;
				
pivotOnDiag(CordI,CordF,MM)-> case is_integer(CordI) and 
								   (CordI>=0) and 
								   is_integer(CordF) and 
								   (CordF>=0) and 
								   guardian:is_matrix(MM) of
                                				true -> pivotOnDiag(CordI+1,CordF,
																	pivot({CordI,CordI},
																		  changeRowIfEZero({CordI,CordI},MM)
																		 )
															 		);
                                				_->throw(["pivotOnDiag: Errore i parametri passati non sono corretti"]) end.
 
% @doc Change the X row with the first get from findRowWENZ if the element {X,Y} = 0  
% @spec changeRowIfEZero({non_neg_integer(),non_neg_integer()},matrix()) -> matrix()
changeRowIfEZero({X,Y},MM) ->  case eGet({X,Y},MM) of
									0->	case findRowWENZ({X,Y},MM) of
                            				[] -> throw(["changeRowIfEZero: Non è possibile effettuare lo scambi di righe"]);
											Row -> changeRow(X,Row,MM)
										end;
									_ -> MM
							   end.

% @doc Get the first Row with the Y° element different from 0
% @spec findRowWENZ({non_neg_integer(),non_neg_integer()},matrix()) -> matrix()
findRowWENZ({X,Y},MM) ->  case getNCord(1,MM) of
							X -> [];
							_ ->  case eGet({X,Y},MM) of
									0 -> findRowWENZ({X+1,Y},MM);
								 	_ -> X
						  		  end
						  end.
						

% @doc Select all element before and after a Column
% @spec divideFromCol(whereM(),non_neg_integer(),elements()) -> elements()
divideFromCol(aft,N,M) -> case is_integer(N) and (N>=0) and guardian:is_elements(M) of
				true ->{_,Eafter}=lists:partition(fun({{_,Y},_}) -> Y<N end,M), Eafter;
				_->throw(["divideFromCol: Errore i parametri passati non sono corretti"]) end;
divideFromCol(bef,N,M) -> case is_integer(N) and (N>=0) and guardian:is_elements(M) of
				true->{Ebefore,_}=lists:partition(fun({{_,Y},_}) -> Y<N end,M), Ebefore;
				_->throw(["divideFromCol: Errore i parametri passati non sono corretti"]) end.

% @doc Select all element before and after a Row
% @spec divideFromRow(whereM(),non_neg_integer(),elements()) -> elements()
divideFromRow(aft,N,M) -> case is_integer(N) and (N>=0) and guardian:is_elements(M) of
				true->{_,Eafter}=lists:partition(fun({{X,_},_}) -> X<N end,M), Eafter;
				_->throw(["divideFromRow: Errore i parametri passati non sono corretti"]) end;
				
divideFromRow(bef,N,M) -> case is_integer(N) and (N>=0) and guardian:is_elements(M) of
				true->{Ebefore,_}=lists:partition(fun({{X,_},_}) -> X<N end,M), Ebefore;
				_->throw(["divideFromRow: Errore i parametri passati non sono corretti"]) end.

% @doc Calculate the determinant of a matrix usign Gauss method
% @spec det(matrix()) -> number()
det(MM) -> case guardian:is_matrix(MM) of
		true->{Tri,Count}=gaussStep1({0,0},MM,0),
		    	{X,Y}=getDim(Tri),
			math:pow(-1,Count)*doDet({X-1,Y-1},Tri,1);
		_->throw(["det: Errore i parametri passati non sono corretti"]) end.
 
% @private
% @doc auxiliar method for the calculate of the determinant
% @spec doDet({non_neg_integer(),non_neg_integer()},matrix(),integer()) -> number() 		   
doDet({0,0},MM,Det) -> Det*eGet({0,0},MM);
doDet({X,Y},MM,Det) -> doDet({X-1,Y-1},MM,Det*eGet({X,Y},MM)). 

% @doc Trasform a square matrix in triangolar with the Gauss Argorithm
% @spec gaussStep1({non_neg_integer(),non_neg_integer()},matrix(),number()) -> matrix()
gaussStep1({X,Y},MM,ContChangedRow) -> 
						io:fwrite("Sto alle Coordinate ~p~n",[{X,Y}]),
						case getDim(MM)=={X,Y} of
							true -> {MM,ContChangedRow};
							_ ->	case eGet({X,Y},MM) of
	 									0 -> case findRowWENZ({X,Y},MM) of
												[] -> gaussStep1({X+1,Y+1},MM,ContChangedRow);
												Row -> gaussStep1({X,Y},changeRow(X,Row,MM),ContChangedRow+1)
											end;
										_ ->  gaussStep2({X,Y},MM,ContChangedRow)
						 			end
						end.



% @doc Partial Pivot on a {NRow,NCol} element
% @spec gaussStep2({non_neg_integer(),non_neg_integer()},matrix(),number()) -> matrix()
gaussStep2({NRow,NCol},MM,ContChangedRow) -> Rpivot = getRow({NRow},MM),
							   {NRpivot,SubMatrix}= lists:foldl(fun(CRow,{CPivot,Acc}) ->ActRow =  getRow({CRow},MM),
																	case  eGet({CRow,NCol},MM) of
																			0 -> NewRow = ActRow;
																			X -> SubRow = scalElementsOp(prod,-X,
																										scalElementsOp(divs,
																														case lists:keysearch({NRow,NCol},1,CPivot) of
																															{value,{_,Ele}} -> Ele;
																															false -> 0
																														end,
																															  divideFromCol(aft,NCol,CPivot))),
																				 NewPivot = SubRow,
																				 Test1=divideFromCol(aft,NCol,ActRow),
																				 NewRow = rowOp(sum,CRow,Test1,0,NewPivot,0)
																	end,																	
																	{Rpivot,lists:keymerge(1,NewRow,Acc)} 
															end
													 ,{Rpivot,[]},nextRows(NRow,getElements(MM))),
								MM1=multipleESet(divideFromCol(bef,NCol,getElements(MM)),new(getDim(MM))),
								MM2=multipleESet(divideFromRow(bef,NRow,getElements(MM)),MM1),
								NewMM=multipleESet(lists:keymerge(1,NRpivot,SubMatrix),MM2),
								gaussStep1({NRow+1,NCol+1},NewMM,ContChangedRow).
            
% @doc Load a file whitch contain a Matrix
% @spec import(File) -> smm:matrix()
import(File)->
	{ok,[MM]}=file:consult(File),
	MM.
	
% doc Save to file a Matrix
% @spec export(File,smm:matrix())-> ok
export(File,M)->
	{ok,FD}=file:open(File,[write]),
	io:fwrite(FD,"~w~s",[M,"."]),
	file:close(FD).

% @doc  Get a matrix of dimension 5,5 with determinant not zero
% @spec exampleDet()-> matrix()
exampleDet()-> multipleESet(example3(),new({5,5})).
% @doc  Get the elements of a matrix of dimension 2,2
% @spec example1()-> elements()
example1() -> [{{0,1},2},{{1,0},2},{{1,1},3}].
% @doc  Get the elements of a matrix of dimension 4,4 
% @spec example2()-> elements()
example2() -> [{{1,1},2},{{2,2},3},{{1,3},3},{{3,3},7},{{2,1},5},{{0,0},5}].
% @doc  Get the elements of a matrix of dimension 5,5 with determinant =18
% @spec example3()-> elements()
example3() -> [{{0,0},1},{{0,2},1},{{0,4},1},{{1,1},2},{{2,1},3},{{2,2},2},{{2,4},3},{{3,3},4},{{4,2},5},{{4,4},5}].

% @doc  Get the inferior triangular matrix
% @spec inferiorTriangular(bool(), matrix())-> matrix()
inferiorTriangular(M)-> case guardian:is_matrix(M) and guardian:is_quadratic(M) of
                                    true -> multipleESet(elementsInferior(getElements(M)),new(getDim(M)));
                                    _ -> throw(["InferiorTriangolar: Errore i parametri passati non sono corretti"]) end.

% @doc  Get the inferior triangular matrix
% @spec superiorTriangular(matrix())-> matrix()
superiorTriangular(M)-> case guardian:is_matrix(M) and guardian:is_quadratic(M) of
                            true -> multipleESet(elementsSuperior(getElements(M)),new(getDim(M)));
                            _ -> throw(["InferiorTriangolar: Errore i parametri passati non sono corretti"]) end.


% @doc  Get the inferior triangular elements of a matrix
% @spec inferiorTriangular(bool(), matrix())-> matrix()
elementsInferior(M)->  lists:filter(fun({{X,Y},_})-> X>=Y end,M).
% @doc  Get the superior triangular elements of a matrix
% @spec inferiorTriangular(bool(), matrix())-> matrix()
elementsSuperior(M)->  lists:filter(fun({{X,Y},_})-> Y>=X end,M).