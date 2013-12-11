-module(matrix).
-export([column/2]).
-export([transpose/1]).
-export([multsbyv/2]).
-export([multvbym/2]).
-export([multmbym/2]).
-export([multcbyr/2]).
-export([multmbymCR/2]).
-export([test_multmbymCR/0]).
-export([multmbympmap/3]).
-export([splitList/2]).
%-export([pmap2/2]).
-export([test_multmbympmap/0]).
-export([test_multmbym/0]).
-export([sum/1]).
-export([test_inner/0,inner/2]).

%export utils



%-import(lists).
-import(mathutils,[applybop/3,sumlist/1]).
-import(timestat,[timeit/2]).

%lib_misc copied from book code to this folder
-import(lib_misc,[pmap1/2]).
-import(lists,[foreach/2]).

-ifdef(traceon).
traceFlag() -> true.
-else.
traceFlag() -> false.
-endif.

%todo: use array syntax: http://www.erlang.org/doc/man/array.html
%todo: use zipwith  http://www.erlang.org/doc/man/lists.html#zip-2
%zipwith(fun(X, Y) -> {X,Y} end, List1, List2) is equivalent to zip(List1, List2).
%my transpose is zipn?

%column N
%column(N,A)->[lists:nth(N,R)|| R <-A].

column(N,A) ->
	K=length(lists:nth(1,A)),
	%trace("~w~n", [K]),
	if 
		N =<K  ->
			[lists:nth(N,R)|| R <-A];
		true -> 
			trace("column number ~w is greater than ~w~n", [N,K])
	end.

%transpose
transpose(A) -> 
	K=length(lists:nth(1,A)),
	L=lists:seq(1,K,1),
	[column(N,A) || N <-L].


% test matrix
%A=[[11,12,13,14],[21,22,23,24],[31,32,33,34],[41,42,43,44]].

%sum two vectors

sum([],_)->[];
sum([H1|T1],[H2|T2])->
	trace("Sum H1=~w T1=~w  H2=~w T2=~w ~n", [H1,T1,H2,T2]),
	
	%correct this
	%it outputs;M1*M2=[[19,[22,0]],[43,[50,0]]]
	% must be [[19,22],[43,50]]
	[H1+H2]++sum(T1,T2).

%sum a list of two lists
sum([H|T]) when is_list(H) and is_list(T)->
	trace("Sum list : H=~w T=~w~n", [H,T]),
	sum(H,hd(T));
sum(_)->0.

%% Mutrix multipication

trace(Format,List) ->
	case traceFlag() of
		true->io:format(Format,List);
		false->void
	end.

%scalar by vector
multsbyv(S,V) -> 
	trace("S ~w by V ~w~n", [S,V]),
	Rsv=[S*V1||V1 <- V],
	%trace("S*V=~w~n",[Rsv]),
	trace("S*V=~w~n",[Rsv]),
	Rsv.

inner([],_) ->0;
inner(_,[]) ->0;

inner(V1,V2)->
	[H1|T1]=V1,
	[H2|T2]=V2,
	H1*H2+inner(T1,T2).


multvbym(V,M) -> 
	trace("V ~w by M ~w~n", [V,M]),
	
	K=length(V),
	L=lists:seq(1,K,1),
	Rvm=[multsbyv(lists:nth(J,V),lists:nth(J,M))||J<-L],
	trace("V*M=~w~n",[Rvm]),
	sumlist(Rvm).


multmbym(M1,M2) -> 
	trace("M1 ~w by M2 ~w~n", [M1,M2]),
	Rmm=[multvbym(V,M2)|| V <-M1],
	trace("M1*M2=~w~n",[Rmm]),
	%io:format("M1*M2=~w~n",[Rmm]),
	Rmm.


%test
% M1=[[1,2],[3,4]].
% M2=[[5,6],[7,8]].

%colum-row multiplication of matrices:
% let A=[C1,C2,...,Cn] , B=[R1,R2,...Rn] then A*B=C1*R1+C2*R2+...+Cn*Rn  where Cj*Rj is an m*K matrix if A is M*n and B is N*K

multcbyr(C,R) -> 
	trace("C ~w by R ~w~n", [C,R]),
	
	K=length(C),
	L=lists:seq(1,K,1),
	Rvm=[multsbyv(lists:nth(J,C),R)||J<-L],
	trace("C*R=~w~n",[Rvm]),
	Rvm.

multmbymCR(M1,M2) ->
	%trace("M1 ~w by M2 ~w~n", [M1,M2]),
	M1T=transpose(M1), 
	trace("M1T= ~w ~n", [M1T]),
	K=length(M1T),
	L=lists:seq(1,K,1),
	trace("M1T:K=~w,L=~w ~n", [K,L]),
	Rmm=[multcbyr(lists:nth(J,M1T),lists:nth(J,M2))||J<-L],
	%TODO: now do element wise sum on  matrices inside Rmm
	
	Add=fun(X,Y)->X+Y end,
	R1=multcbyr(lists:nth(1,M1T),lists:nth(1,M2)),
	%R2=multcbyr(lists:nth(2,M1T),lists:nth(2,M2)),
	%io:format("R1=~w R2=~w~n",[R1,R2]),
	%R1PlusR2=mathutils:applybop(Add,R1,R2),
	%io:format("R1+R2=~w~n",[R1PlusR2]),
	
	%use R1 (head) as initial value for Sum. So use tail for RmmT
	[_|RmmT]=Rmm,
	Rmm2=lists:foldl(fun(X, Sum) -> applybop(Add,X,Sum) end, R1, RmmT).
%io:format("Rmm2=~w~n",[Rmm2]).
%Rmm.


%only works for square matrices
test_multmbym()->
	M1=[[1,1,1,1],[2,2,2,2],[3,3,3,3],[4,4,4,4]],
	%M2=[[5,6],[7,8],[9,10],[11,12]],
	M2=M1,
	%io:format("M1=~w M2=~w~n",[M1,M2]),
	R=multmbym(M1,M2).
%io:format("R=~w~n",[R]).

test_multmbymCR()->
	M1=[[1,2],[3,4]],
	M2=[[5,6],[7,8]],
	%io:format("M1=~w M2=~w~n",[M1,M2]),
	multmbymCR(M1,M2).


%use pmap
makeRowByMat(M) -> (fun(V) -> multvbym(V, M) end).
makeMatByMat(M) -> (fun(M1) -> multmbym(M1, M) end).

%for M1,M2 1000*1000 the PC freezes totaly. Had to shut down
%becuase pamap1 tries to create 1000 processes (one per row).
multmbympmap(M1,M2,MaxProc)->
	MultMatByMat=makeMatByMat(M2),
	pmap2(MultMatByMat,M1,MaxProc).
%Multrbym=makeRowByMat(M2),
%pmap1(Multrbym,M1).
%test
%MultMatByMat(M1).

test_multmbympmap()->
	M1=[[1,2],[3,4]],
	M2=[[5,6],[7,8]],
	%io:format("M1=~w M2=~w~n",[M1,M2]),
	multmbympmap(M1,M2,1).

%sample test
%12> timeMatrix:testmmpmap(1000,10,1).
%Func=multmbympmap exec runtime=3.93294e8 (wall_clock_Time=9.6393e7) microseconds
%Total wall_clock_Time=96393000 microseconds
%

%pmap2 with  number of processes limited to MaxProc
pmap2(F, L,MaxProc) -> 
	S = self(),
	Ref = erlang:make_ref(),
	SubMatrices=splitList(L,MaxProc),
	%io:format("SubMatrices=~w ~n",[SubMatrices]),
	foreach(fun(I) -> 
					spawn(fun() -> do_f1(S, Ref, F, I) end)
			end, SubMatrices),
	%% gather the results
	gather1(MaxProc, Ref, []).

do_f1(Parent, Ref, F, I) ->	
	%io:format("do_f1:I=~w  F=~w~n",[I,F]),
	Parent ! {Ref, (catch F(I))}.

gather1(0, _, L) -> 
	L1=lists:reverse(L),
	%extra [] solve it!
	%gather1:N=0  L=[[[90,100,110,120],[202,228,254,280]],[[314,356,398,440],[426,484,542,600]]]
	%io:format("gather1:N=~w  L=~w~n",[0,L1]),
	L1;

%One extra pair of []
%gather1:Ret=[[90,100,110,120],[202,228,254,280]]
gather1(N, Ref, L) ->
	%io:format("gather1:N=~w  L=~w~n",[N,L]),
	receive
		{Ref, Ret} -> 
			%io:format("gather1:Ret=~w~n",[Ret]),
			gather1(N-1, Ref, [Ret|L])
	end.



splitList(L,N)->
	Len=length(L),
	SubSize=Len div N,
	if 
		SubSize =:= 0 ->L;
		true->
			Lfrom=lists:seq(1,Len,SubSize),
			%io:format("N=~w SubSize=~w Lfrom=~w~n",[N,SubSize,Lfrom]),
			SplitL=[lists:sublist(L,From,SubSize)|| From <-Lfrom]
	end.

test_inner()->
	V1=[1,2,3,4],
	V2=[12,6,4,3],
	R=inner(V1,V2),
	io:format("V1=~w V2=~w ~n",[V1,V2]),
	io:format("V1.V2=~w~n",[R]).




