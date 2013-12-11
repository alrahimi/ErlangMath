-module(trace).
-export([trace/2]).

%import this module and compile your module with : c(mymodule,{d,{traceon,true}}) to enable trace

-ifdef(traceon).
traceFlag() -> true.
-else.
traceFlag() -> false.
-endif.


trace(Format,List) ->
	case traceFlag() of
		true->io:format(Format,List);
		false->void
	end.
