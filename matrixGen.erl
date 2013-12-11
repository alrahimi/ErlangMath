% Matrix module.
-module(matrixGen).

-export([
    identity/1,
     sequential/2
 
]).



% Creates a rectangular matrix using the specified function to generate elements.

createMat(ColumnDim, RowDim, CellValueGen) ->
    [[CellValueGen(Column, Row, ColumnDim, RowDim) || Column <- lists:seq(1, ColumnDim)] || Row <- lists:seq(1, RowDim)].


random(ColumnDim, RowDim, MaxValue) ->
    random:seed(erlang:now()),
    createMat(ColumnDim, RowDim, fun(_, _, _, _) -> random:uniform(MaxValue) end).


sequential(ColumnDim, RowDim) ->
    createMat(ColumnDim, RowDim, fun(Column, Row, CS, _) -> CS * (Row - 1) + Column end).

identity(Size) ->
    createMat(Size, Size, fun(Column, Row, _, _) -> case Column of Row -> 1; _ -> 0 end end).

