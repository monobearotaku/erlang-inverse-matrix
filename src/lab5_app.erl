%%%-------------------------------------------------------------------
%% @doc lab5 public API
%% @end
%%%-------------------------------------------------------------------

-module(lab5_app).

-behaviour(application).

-export([stop/1, start/2]).

% Function to start the module operations
start(_StartType, _StartArgs) ->
    N = 5,
    A = init_random_matrix(N),
    print_matrix(A),
    {_, Identity} = inverse_matrix(A, N),
    print_matrix(Identity),
    {ok, self()}.

%% Application stop function
stop(_State) ->
    ok.

%% internal functions
print_matrix(Matrix) ->
    lists:map(fun(Row) -> 
        lists:map(fun(Item) -> 
            io:format("~.2f ", [Item])            
        end, Row),
        io:nl()    
    end, Matrix).


init_random_matrix(N) ->
    lists:map(fun(_) ->
        lists:map(fun(_) -> 
            rand:uniform_real() * 10.0 end, lists:seq(1, N))
    end, lists:seq(1, N)).

init_identity_matrix(N) ->
    lists:map(fun(I) -> 
        lists:map(fun(J) ->
            if I == J -> 1.0; true -> 0.0 end
        end, lists:seq(1, N))
    end, lists:seq(1, N)).

inverse_matrix(Matrix, N) ->
    Identity = init_identity_matrix(N),
    lists:foldl(fun(I, {Mat, Id}) -> 
        % print_matrix(Id),
        Pivot = getPivot(Mat, I), 
        ScaledMat = setRow(Mat, scaleRow(getRow(Mat, I), 1/Pivot), I),
        ScaledId = setRow(Id, scaleRow(getRow(Id, I), 1/Pivot), I),

        subtractScaledRowFromMatrix(ScaledMat, ScaledId, I, N)
    end, {Matrix, Identity}, lists:seq(1, N)).

getPivot(Matrix, I) ->
    lists:nth(I, getRow(Matrix, I)).

getItem(Matrix, I, J) ->
    lists:nth(J, getRow(Matrix, I)).

getRow(Matrix, I) ->
    lists:nth(I, Matrix).

scaleRow(Row, Pivot) -> 
    lists:map(fun(Item) -> 
        Item * Pivot
    end, Row).

subtractScaledRowFromMatrix(Mat, Id, I, N) ->
    lists:foldl(fun(J, {AccMat, AccId}) ->
        if
            J == I ->
                {AccMat, AccId};
            true ->
                PivotRow = getRow(AccMat, I),
                CurrentRowMat = getRow(AccMat, J),
                Factor = getItem(AccMat, J, I),


                SubtractedRowMat = subtractRows(CurrentRowMat, scaleRow(PivotRow, Factor)),

                CurrentIdRow = getRow(AccId, J),
                SubtractedRowId = subtractRows(CurrentIdRow, scaleRow(getRow(AccId, I), Factor)),

                UpdatedMat = setRow(AccMat, SubtractedRowMat, J),
                UpdatedId = setRow(AccId, SubtractedRowId, J),
                {UpdatedMat, UpdatedId}
        end
    end, {Mat, Id}, lists:seq(1, N)).

subtractRows(Row1, Row2) ->
    lists:zipwith(fun(X, Y) -> X - Y end, Row1, Row2).

setRow(Matrix, Row, I) ->
    Before = lists:sublist(Matrix, I - 1),   
    After = lists:nthtail(I, Matrix),
    Before ++ [Row] ++ After.