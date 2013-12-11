-module(gauss).
-import(matrix,[multsbyv/2,transpose/1,multvbym/2]).
-import(mathutils,[add/2,applybop/3,sumlist/1]).
-import(trace,[trace/2]).
-export([echelon/1,test_divpivot/0,test_echelon/0, test_getInverse/0,test_getInverse/1,test_getSolution/0,getSolution/2]).
-export([ test_multvbym/0]).

%@TODO: Handle the case H=0 by swapping rows
getpivot(V)->
	H=hd(V),
	if
		(abs(H) < 1.0e-16 )->
			io:format("abs(H) < 1.0e-16 ? ~w~n",[H]),
			1;
		
		true->1.0/H
	end.

divpivot(A)->
	M=[multsbyv(getpivot(V),V)||V <-A].
%trace("~w~n",[M]).

subFromRows(V,A)->
	Sub=fun(X,Y)->X-Y end,
	[applybop(Sub,W,V)||W<-A].


%also add missing zeros to the beggining of each row
echelon([],Acc)->
	%io:format("rev RE=~w~n",[Acc]),
	LowerTriangular=lists:reverse(Acc),
	%io:format("Row echelon=~w~n",[LowerTriangular]),
	%eliminate upper part. doesn't work
	%Trans=transpose(LowerTriangular),
	%echelon(Trans);
	LowerTriangular;


echelon(A,Acc)->
	trace("A=~w~n",[A]),
	A1=divpivot(A),
	trace("A1=~w~n",[A1]),
	
	[H|T]=A1,
	
	trace("H=~w~n",[H]),
	
	%subtract H from all rows of A1 starting at row N+1
	A2=subFromRows(H,T),
	trace("A2=~w~n",[A2]),
	A3=[tl(V)||V<-A2],
	echelon(A3,[H|Acc]).


echelon(A)->
	%io:format("A=~w~n",[A]),
	echelon(A,[]).

% A is the matrix in echelon form and X is the set of solution found so far: X=[X(K),X(K+1),...X(N-1)]
%Xi=(1/A'ii)(Bi'-sum(j=i+1,n,A'ij*Xj)
%A' is the  enttry in the row echelon form of A
%start X=[-1] for the Bn coefficient
getSolution(A,X) ->
	io:format("A=~w X=~w~n",[A,X]),
	
	%number of rows
	NR=length(A),
	%number of column is the length of the first row
	NC=length(hd(A)),
	NX=length(X),
	if
		NC == NX -> lists:sublist(X,NC-1);
		true->
			%get row for the next Xi
			B=tl(lists:nth(NR-NX+1,A)),
			Xnext= - matrix:inner(B,X),
			io:format("B=~w Xnext=~w NR=~w,NC=~w,NX=~w~n",[B,Xnext,NR,NC,NX]),
			getSolution(A,[Xnext|X])
	end.


% A is the matrix in echelon form and X is the set of solution found so far: X=[[X(K)],[X(K+1)],...[X(N-1)]]

%start X=[] 

getInverse(A,X) ->
	%io:format("A=~w X=~w~n",[A,X]),
	
	%number of rows
	NR=length(A),
	%number of column is the length of the first row /2 since adding I double it
	NC=length(hd(A)),
	NC2=NC div 2,
	NX=length(X),
	
	if
		NC2 == NX -> lists:sublist(X,NC-1);
		true->
			%get row for the next Xi
			A1=tl(lists:nth(NR-NX,A)),
			%split A1 to extract original (A) augmented part(I)
			%The I part has always NC column(as much as the original A)
			%NC=2*colums of original A (since it is augmented by I of the same size)
			K=length(A1)-NC2,
			io:format("A1=~w~nNC=~w~nK=~w~n",[A1,NC,K]),
			
			if
				K ==0 ->A2=[],B=A1;
				true ->
					{A2,B}=lists:split(K,A1),
					io:format("A2=~w~nB=~w~n",[A2,B])
			end,
			
			X1=[B|X],
			A3=[-1|A2],
			
			io:format("X1=~w~nA3=~w~n",[X1,A3]),
			Xnext1= multvbym(A3,X1),
			Xnext=[-1*Z||Z <-Xnext1],
			io:format("Xnext=~w~n",[Xnext]),
			getInverse(A,[Xnext|X])
	end.	

test_divpivot()->
	A=[[10,11,12,13],[20,21,22,23],[30,31,32,33],[40,41,42,43]],
	trace("A=~w~n",[A]),
	A1=divpivot(A),
	trace("A1=~w~n",[A1]),
	
	H=hd(A1),
	trace("H=~w~n",[H]),
	
	%subtract H from all rows of A1 starting at next row
	[H|subFromRows(H,A1)].

test_echelon()->
	A=[[10,11,12,13],[20,21,22,23],[30,31,32,33],[40,41,42,43]],
	%A=[[10,100],[20,400]],
	%A=[[2,3,4],[4,5,6],[8,9,10]],
	echelon(A).

test_getInverse()->
	%inverse from Linear Algebra by Strang page 32 make A to [A|I] first
	A=[[2,1,1,1,0,0],[4,1,0,0,1,0],[-2,2,1,0,0,1]],
	%test to find  op
	%A=[[a,b,c,1,0,0],[4,1,0,0,1,0],[-2,2,1,0,0,1]],
	R1=echelon(A),
	io:format("A=~w~nR1=~w~n",[A,R1]),
	R2=getInverse(R1,[]),
	io:format("R2=~w~n",[R2]).

%test output
%R2=[
%[0.12499999999999989,0.12500000000000006,-0.12499999999999997],
%[-0.49999999999999956,0.4999999999999998,0.4999999999999999],
%[1.2499999999999998,-0.7499999999999999,-0.24999999999999994]]
%verified strang p 32:
%1/8=0.12499999999999989

appendRowwise([],_)->[];
appendRowwise(_,[])->[];

appendRowwise([H1|T1],[H2|T2])->
	io:format("H1=~w T1=~w H2=~w T2=~w~n",[H1,T1,H2,T2]),
	[lists:append(H1,H2)|appendRowwise(T1,T2)].
	
test_getInverse(Dim)->
	A=matrixGen:sequential(Dim,Dim),
	IA=matrixGen:identity(Dim),
	AugA=appendRowwise(A,IA),
	R1=echelon(AugA),
	io:format("A=~w~nR1=~w~n",[AugA,R1]),
	R2=getInverse(R1,[]),
	io:format("R2=~w~n",[R2]).

test_getSolution() ->
	%----------------------
	A=[[2,1,1,7],[4,1,0,6],[-2,2,1,5]],
	%2X1+X2+X3=7
	%4X1+X2=6
	%-2X1+2X2+X3=5
	%Answer X1=1,X2=2,X3=3
	%----------------------
	%A=[[2,1,1,0],[4,1,0,0],[-2,2,1,0]],
	
	A1=echelon(A),
	getSolution(A1,[-1]).

test_multvbym()->
	S=[1,2,3],
	M=[[1,2,3],[4,5,6],[7,8,9]],
	SM=multvbym(S,M),
	io:format("using multvbym SM=~w~n",[SM]).


