
-module(vecutils).
-import(mathutils,[applybop/3,sumlist/1]).
-import(gauss,[echelon/1]).
-import(matrix,[multsbyv/2,transpose/1,multvbym/2]).

-export([sumtail/2,test_sumtail/0,test_innersv/0]).



%sum two vectors  of different size
sumtail([],Y,Acc)->lists:append(Acc,Y);
%[Acc|Y];
sumtail([X],[],Acc)->lists:append(Acc,X);
%[Acc|X];
sumtail(X,Y,Acc)->
sumtail(tl(X),tl(Y) ,[hd(X)+hd(Y)|Acc]).



sumtail(A,B)->
io:format("sumtail:A=~w   B=~w~n",[A,B]),
Arev=lists:reverse(A),
Brev=lists:reverse(B),
C=sumtail(Arev,Brev,[]),
lists:reverse(C).


%inner scalar by vector for backsub
%but why not use multvbym?
%--------------------
innersv([],_,Acc) ->Acc;
innersv(_,[],Acc) ->Acc;

innersv(V1,V2,Acc)->
[H1|T1]=V1,
[H2|T2]=V2,
innersv(T1,T2,[multsbyv(H1,H2)|Acc]).
%-----------------------

innersv(V1,V2)->
R=lists:reverse(innersv(V1,V2,[])),
R1=sumlist(R),
io:format("V1=~w~nV2=~w~nR=~w~nR1=~w~n",[V1,V2,R,R1]).


	

test_sumtail()->
A=[1,2],
B=[1,2,3,4,5,6],
%expected resukts [1,2,3,4,6,8]

sumtail(A,B).


test_innersv()->
S=[1,2,3],
M=[[1,2,3],[4,5,6],[7,8,9]],
SM=innersv(S,M),
io:format("SM=~w~n",[SM]),
SM2=multvbym(S,M),
io:format("using multvbym SM=~w~n",[SM2]).
	