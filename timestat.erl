
-module(timestat).

-export([timeit/4,test/1,sq2/0]).


%% timeit(F) 
%%   Time function F
%%   See how much time this takes

timeit(Mod,Func,Args,N) ->
%io:format("Mod=~w,Func=~w,Args=~w N=~w~n",[Mod,Func,Args,N]),
    statistics(runtime),
    statistics(wall_clock),
	for(1, N, Mod,Func,Args),
    
    {_, Time1} = statistics(runtime),
    {_, Time2} = statistics(wall_clock),
    U1 = Time1 * 1000 / N,
    U2 = Time2 * 1000 / N,
    io:format("Func=~w exec runtime=~p (wall_clock_Time=~p) microseconds~n",
	      [Func,U1, U2]),
    io:format("Total wall_clock_Time=~p microseconds~n",[Time2*1000]).



for(N, N, Mod,F,Args) -> [apply(Mod,F,Args)];
for(I, N, Mod,F,Args) -> [apply(Mod,F,Args)|for(I+1, N, Mod,F,Args)].

sq2() -> 1000000.0 *1000000.0.

%example
test(N)->
%T=fun() -> 2.0*2.0 end,
timestat:timeit(timestat,sq2,[],N).

%sample out
%8> timestat:test(1000000).
%Mod=timestat,Func=sq2,Args=[] N=1000000
%Func=sq2 exec time=0.109 (0.188) microseconds
%ok
