-module (testFlower).
-export ([start/4]).       

-import (smm).

% @doc start an operation (sum,sub,prod) on 2 random matrix of dim {Row,Col} with values between MinValue and MaxValue
% @spec start(atom(),non_neg_integer(),{Row,Col},{MinValue,MaxValue}) -> Result()
start(prod,Worker,{R,C},Values)->
            A = smm:random(50,Values,{C,R}),
			io:fwrite("~s~n",["Finita creazione matrice 1"]),
			B = smm:random(50,Values,{R,C}),
			io:fwrite("~s~n",["Finita creazione matrice 2"]),
			flowersv:start({flower,Worker}),
			flower_dispatcher:operation4Row(prod,A,B);

start(OP,Worker,Dim,Values) -> A = smm:random(50,Values,Dim),
			io:fwrite("~s~n",["Finita creazione matrice 1"]),
			B = smm:random(50,Values,Dim),
			io:fwrite("~s~n",["Finita creazione matrice 2"]),
			flowersv:start({flower,Worker}),
			flower_dispatcher:operation4Row(OP,A,B).