-module(mapdeep).
-export([test_mapd/0]).

mapd(_, []) -> [];
mapd(F, [H|T]) ->
	case is_list(H) of
		true->[mapd(F,H)|mapd(F,T)];
		false -> [F(H)|mapd(F, T)]
	end.
	

%why defining D like this doesn't work	? get mapdeep.erl:12: syntax error before: D
%D=fun(X) -> 2*X end.
			
test_mapd()->
%A=[ [I*10+J|| J<-[1,2,3,4,5,6,7,8,9]]||I<-[1,2,3,4,5,6,7,8,9]].
M1=[[1,2],[3,4]],
M2=[[5,6],[7,8]],
M3=[M1,M2],
R=mapd(fun(X) -> 2*X end,M3),
io:format("Double(M3)=~w~n",[R]).			