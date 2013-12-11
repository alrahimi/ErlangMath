-module(timeMatrix).

-export([testmm/2,testmmcr/2 ,testmmpmap/3,testmorgana/2,test_echelon/2]).
-import(timestat,[timeit/4]).
-import(matrixMorgana,[sequential/2]).
-import(matrix,[multmbym/2]).
-import(matrix,[multmbymCR/2]).
-import(gauss,[echelon/1]).

%test with square matrices only for now
% Ntests: integer =number of time to run the test
%Dim dimention of the matrix

%sample run
%timeMatrix:testmm(1000,1).
%Func=multmbym exec runtime=9.3148e7 (wall_clock_Time=9.8483e7) microseconds
%Total wall_clock_Time=98483000 microseconds
%ok
%this is for 1000000000 integer multiplication or 0.093148 microseconds per integer multiplications.

testmm(Dim,Ntests)->
M1=matrixMorgana:sequential(Dim,Dim),
M2=M1,
timestat:timeit(matrix,multmbym,[M1,M2],Ntests).


%sample output
%timeMatrix:testmmcr(600,1).
%Func=multmbymCR exec runtime=5.9359e7 (wall_clock_Time=8.3445e7) microseconds
%Total wall_clock_Time=83445000 microseconds
%Note: this method is much more memory intensive and for Dim=700 runs out of heap and crashes erl:
%     Crash dump was written to: erl_crash.dump
%        eheap_alloc: Cannot allocate 1373343080 bytes of memory (of type "heap").

testmmcr(Dim,Ntests)->
M1=matrixMorgana:sequential(Dim,Dim),
M2=M1,
timestat:timeit(matrix,multmbymCR,[M1,M2],Ntests).

testmmpmap(Dim,MaxProc,Ntests)->
M1=matrixMorgana:sequential(Dim,Dim),
M2=M1,
timestat:timeit(matrix,multmbympmap,[M1,M2,MaxProc],Ntests).


%times go up exponentialy? 2**4 from 100 to 200
%4> timeMatrix:testmorgana(100,1).
%Total wall_clock_Time=5023000 microseconds
%ok
%5> timeMatrix:testmorgana(200,1).
%Func=multiply exec runtime=8.0668e7 (wall_clock_Time=8.0668e7) microseconds
%Total wall_clock_Time=80668000 microseconds
%ok
testmorgana(Dim,Ntests)->
M1=matrixMorgana:sequential(Dim,Dim),
M2=M1,
timestat:timeit(matrixMorgana,multiply,[M1,M2],Ntests).

%row echelon form

test_echelon(Dim,Ntests)->
M=matrixMorgana:sequential(Dim,Dim),
timestat:timeit(gauss,echelon,[M],Ntests).
