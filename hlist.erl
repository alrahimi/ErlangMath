-module(hlist).
-import(lists,[seq/3,map/2]).
-export([hlist/2]).

hlist(1,K)->
S=seq(1,K,1),
L=[X||X<-S],
%io:format("~w~n",[L]),
L;

hlist(_,0)->void;

hlist(N,K) when N>1->
S=seq(1,K,1),
L=[hlist(N-1,K) ||X<-S],
%io:format("~w~n",[L]),
L;
hlist(N,K)->void.






