-module (blas2).
-export ([start/3]).

-import (smm).

% @type triangular() = U | u | L | l.

% @doc start an operation sger on one matrix
% @spec start(atom(),non_neg_integer(),{real(),elements(),elements(),matrix()}) -> matrix()
start(sger,Worker,{Num, Vect1, Vect2, Matrix})->
            {M,N}=smm:getDim(Matrix),
            MVect1=smm:multipleESet(Vect1,smm:new({M,1})),
			MVect2=smm:multipleESet(Vect2,smm:new({N,1})),
            M1=smm:scalarMatrixOp(prod,Num,MVect1),
            M2t=smm:transp(MVect2),
            flowersv:start({flower,Worker}),
            MM=flower_dispatcher:operation4Row(prod,M1,M2t),
            flower_dispatcher:operation4Row(sum,MM,Matrix);

% @doc start an operation ssyr on one matrix
% @spec start(atom(),non_neg_integer(),{triangular(),real(),elements(),matrix()}) -> matrix()
start(ssyr,Worker,{Triang, Num, Vect, Matrix})->
            flowersv:start({flower,Worker}),
            {M,M}=smm:getDim(Matrix),
            MVect=smm:multipleESet(Vect,smm:new({M,1})),
            M1=smm:scalarMatrixOp(prod,Num,MVect),
            M2=smm:transp(MVect),
            case Triang of
                u -> M3=smm:superiorTriangular(Matrix);
                "U" -> M3=smm:superiorTriangular(Matrix);
                l -> M3=smm:inferiorTriangular(Matrix);
                "L" -> M3=smm:inferiorTriangular(Matrix)end,
            flower_dispatcher:operation4Row(sum,flower_dispatcher:operation4Row(prod,M1,M2),M3);

% @doc start an operation ssyr2 on one matrix
% @spec start(atom(),non_neg_integer(),{triangular(),real(),elements(),elements(),matrix()}) -> matrix()
start(ssyr2,Worker,{Triang, Num, Vect1, Vect2, Matrix})->
            flowersv:start({flower,Worker}),
            {M,M}=smm:getDim(Matrix),
            MVect1=smm:multipleESet(Vect1,smm:new({M,1})),
			MVect2=smm:multipleESet(Vect2,smm:new({M,1})),
            M1mod=smm:scalarMatrixOp(prod,Num,MVect1),
            M2t=smm:transp(MVect2),
            M2mod=smm:scalMatrixOp(prod,Num,MVect2),
            M1t=smm:transp(MVect1),
            case Triang of
                u -> M3=smm:superiorTriangular(Matrix);
                "U" -> M3=smm:superiorTriangular(Matrix);
                l -> M3=smm:inferiorTriangular(Matrix);
                "L" -> M3=smm:inferiorTriangular(Matrix) end,
            M1=flower_dispatcher:operation4Row(prod,M1mod,M2t),
            M2=flower_dispatcher:operation4Row(prod,M2mod,M1t),
            flower_dispatcher:operation4Row(sum,flower_dispatcher:operation4Row(sum,M1,M2),M3).