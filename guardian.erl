-module(guardian).
-export([is_non_neg_integer/1,
        is_property/1,
        is_cord/1,
        is_2cord/1,
        is_element/1,
        is_elements/1,
        is_typeM/1,
        is_matrix/1,
        is_scalop/1,
        is_rowop/1,
        is_matrixop/1,
        is_whereM/1,
        is_same_dimension/2,
        is_quadratic/1]).
-record(property, {type=row,transp=false,dim={},default=0}).
    
is_non_neg_integer(Value)-> is_integer(Value) and (Value>=0).

is_cord(Value)-> case is_tuple(Value) and (tuple_size(Value)/=0) of
                    true -> ListaCord=tuple_to_list(Value),
                        lists:foldl(fun(Ele,Acc)->Acc and is_non_neg_integer(Ele) end, true, ListaCord);
                    _-> false end.
                    
is_2cord(Value)-> case is_tuple(Value) and (tuple_size(Value)==2) of
                    true -> {Cord1,Cord2}=Value, is_non_neg_integer(Cord1) and is_non_neg_integer(Cord2);
                    _-> false end.

is_typeM(Value)-> (Value==col) or (Value==row).

is_element(Element)->case is_tuple(Element) and (tuple_size(Element)==2) of
                    true -> {Cord,Value}=Element, is_cord(Cord) and is_number(Value);
                    _-> false end.

is_elements(Elements)->case is_list(Elements) of
                    true -> lists:foldl(fun(Element,Acc)->is_element(Element) and Acc end,true,Elements);
                    _ -> false end.

is_property(P) ->case is_record(P,property) of
                    true -> #property{type=Type,transp=Transp,dim=Dim,default=Value}=P,
                        is_typeM(Type) and is_boolean(Transp) and is_cord(Dim) and is_number(Value);
                    _ ->false end.
is_matrix(M) ->case is_tuple(M) and (tuple_size(M)==3)of
                    true -> {matrix,Property,Elements}=M, is_property(Property) and is_elements(Elements);
                    _ ->false end.

is_scalop(Value) -> (Value==divs) or (Value==prod).

is_rowop(Value) -> (Value==sum) or (Value==sub).

is_matrixop(Value) ->(Value==sum)or(Value==sub)or(Value==prod).

is_whereM(Value) -> (Value==bef) or (Value==aft).

is_same_dimension(M,M1) -> smm:getDim(M)==smm:getDim(M1).

is_quadratic(M) -> case smm:getDim(M)of
                        {N,N} -> true;
                        _ -> false end.
