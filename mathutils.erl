-module(mathutils).
-export([do/1,add/2,applyop/3,test_add/0, test_add2/0,applybop/3,test_applybop/0,test_sumlist/0]).
-import(lists,[zip/2,map/2]).
-export([sumlist/1]).


%to define traceon compile with: c(group,{d,debug})
-ifdef(traceon).
-define(TRACEON,true).
-else.
-define(TRACEON,false).
-endif.

traceFlag() -> ?TRACEON.

trace(Format,List) ->
case traceFlag() of
 true->io:format(Format,List);
 false->void
end.

%

%TODO: add support for -,*,/ etc
add([],_)->[];
add(_,[])->[];

add([H1|T1],[H2|T2])->
trace("H1=~w T1=~w H2=~w T2=~w~n",[H1,T1,H2,T2]),
[add(H1,H2)|add(T1,T2)];


add(X,Y)->X+Y.

%add({X,Y})->add(X,Y).

%use map for this
%add([X|Y])->add(0,X)+add(Y).

%generalized ops. Applies to scalor ,vectors,matrices, tuples,lists.

applybop(_,[],_)->[];
applybop(_,_,[])->[];

applybop(Bop,[H1|T1],[H2|T2])->
trace("H1=~w T1=~w H2=~w T2=~w~n",[H1,T1,H2,T2]),
[applybop(Bop,H1,H2)|applybop(Bop,T1,T2)];

applybop(Bop,X,Y)->
trace("Bop=~w X=~w Y=~w~n",[Bop,X,Y]),
Bop(X,Y).

%applybop(Bop,{X,Y})->applybop(Bop,X,Y);
%applybop(Bop,[X|Y])->add(0,X)+applybop(Bop,Y).


%use zip and map

applyop(F,A,B)->
if
is_list(A)->
C=zip(A,B),
trace("zip(A,B)=~w~n",[C]),
map(F,C);
true->F(A,B)

end.

%op overloading sort of.
do(L)->
case L of
{X, '+',Y}->
if
is_list(X)->add(X,Y);
true ->X+Y
end;
{X ,'-',Y}->X-Y;
{X, '*',Y}->X*Y;
{X, '/',Y}->X/Y;
_ ->trace("bad arg ~w~n",[L])
end.

sumlist(L)->
[H|T]=L,
Add=fun(X,Y)->X+Y end,
lists:foldl(fun(X, Sum) -> applybop(Add,X,Sum) end, H, T).
%io:format("Sum=~w~n",[Sum]).

test_add()->
A=[1,2,3,4],
B=[10,20,30,40],
add(A,B).

test_add2()->
A=[1,2,3,4],
B=[10,20,30,40],
A2=[A,B],
B2=[B,A],
add(A2,B2).

test_applybop()->
A=[1,2,3,4],
B=[10,20,30,40],
A2=[A,B],
B2=[B,A],
io:format("A2=~w B2=~w~n",[A2,B2]),
Add=fun(X,Y)->X+Y end,
trace("Add A2=~w B2=~w~n",[A2,B2]),
R1=applybop(Add,A2,B2),
io:format("Add A2, B2=~w~n",[R1]),

Mult=fun(X,Y)->X*Y end,
trace("Mult A2=~w ,B2=~w~n",[A2,B2]),

R2=applybop(Mult,A2,B2),
io:format("Mult A2 ,B2=~w~n",[R2]).


%test
%A=[1,2,3,4].
%B=[10,20,30,40].
% Add=fun({X,Y})->X+Y end.
%C=lists:zip(A,B).
% [{1,10},{2,20},{3,30},{4,40}]
% lists:map(group:add,C).
% [11,22,33,44]


test_sumlist()->
L=[[1,1,1,1],[2,2,2,2],[3,3,3,3],[4,4,4,4]],
sumlist(L).

