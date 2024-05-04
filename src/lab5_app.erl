%%%-------------------------------------------------------------------
%% @doc lab5 public API
%% @end
%%%-------------------------------------------------------------------

-module(lab5_app).

-behaviour(application).

-export([stop/1, start/2]).

% Function to start the module operations
start(_StartType, _StartArgs) ->
    % Initialize a 3x3 matrix with random values
    Matrix = init_random_matrix(3, 3),
    % Print the matrix
    print_matrix(Matrix),
    {ok, self()}.

%% Application stop function
stop(_State) ->
    ok.

%% internal functions

% Function to print the matrix
print_matrix(Matrix) when is_list(Matrix) ->
    lists:foreach(fun(Row) ->
                      io:format("~p~n", [Row])
                  end, Matrix).

% Function to initialize a matrix with random values
init_random_matrix(Rows, Cols) ->
    % Generate a list of lists with random values
    lists:map(fun(_) ->
                  lists:map(fun(_) -> rand:uniform(100) end, lists:seq(1, Cols))
              end, lists:seq(1, Rows)).
