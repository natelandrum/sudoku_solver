-module(sudoku_solver).
-export([start/0, solve/1, print_grid/1]).

% Sample Sudoku puzzle with 0s as placeholders for empty cells.
sudoku_grid() -> 
    [
        [0, 0, 0, 0, 1, 0, 0, 4, 7],
        [0, 0, 0, 0, 0, 0, 0, 0, 3],
        [5, 0, 7, 8, 0, 3, 0, 0, 0],
        [0, 0, 0, 9, 0, 2, 0, 5, 0],
        [0, 8, 0, 0, 0, 0, 0, 0, 0],
        [2, 7, 0, 5, 0, 0, 0, 8, 6],
        [0, 2, 0, 0, 5, 0, 0, 0, 0],
        [0, 0, 1, 0, 0, 6, 0, 7, 0],
        [3, 5, 0, 7, 0, 0, 4, 9, 0]
    ].

% Start the solver on the sample grid.
start() ->
    Grid = sudoku_grid(),
    SolvedGrid = solve(Grid),
    print_grid(SolvedGrid).

% Solve the Sudoku puzzle.
solve(Grid) ->
    case find_empty(Grid) of
        {error, none} -> 
            % No empty cells, puzzle is solved
            Grid;
        {ok, Row, Col} -> 
            % Try placing numbers in the empty cell
            try_numbers(Grid, Row, Col)
    end.

% Try placing numbers from 1 to 9 in the given empty cell.
try_numbers(Grid, Row, Col) ->
    try_numbers(Grid, Row, Col, lists:seq(1, 9)).

try_numbers(_Grid, _Row, _Col, []) ->
    % No valid number found, backtrack
    false;
try_numbers(Grid, Row, Col, [Num | RestNums]) ->
    case valid_number(Grid, Row, Col, Num) of
        true ->
            NewGrid = update_grid(Grid, Row, Col, Num),
            Solved = solve(NewGrid),
            case Solved of
                false -> 
                    % Backtrack and try next number
                    try_numbers(Grid, Row, Col, RestNums);
                _ ->
                    % Solution found
                    Solved
            end;
        false ->
            % Try next number
            try_numbers(Grid, Row, Col, RestNums)
    end.

% Find the first empty cell (returns {ok, Row, Col} or {error, none}).
find_empty(Grid) ->
    find_empty(Grid, 1).

find_empty([], _) -> {error, none};
find_empty([Row | Rest], RowIndex) ->
    case find_empty_in_row(Row, 1) of
        {ok, ColIndex} -> {ok, RowIndex, ColIndex};
        {error, none} -> find_empty(Rest, RowIndex + 1)
    end.

find_empty_in_row([], _) -> {error, none};
find_empty_in_row([0 | _], ColIndex) -> {ok, ColIndex};
find_empty_in_row([_ | Rest], ColIndex) -> find_empty_in_row(Rest, ColIndex + 1).

% Check if placing Num in position (Row, Col) is valid.
valid_number(Grid, Row, Col, Num) ->
    RowList = lists:nth(Row, Grid),
    not lists:member(Num, RowList) andalso
    not lists:member(Num, column(Grid, Col)) andalso
    not lists:member(Num, subgrid(Grid, Row, Col)).

% Update the grid with the new number.
update_grid(Grid, Row, Col, Num) ->
    lists:sublist(Grid, 1, Row - 1) ++ 
    [update_row(lists:nth(Row, Grid), Col, Num)] ++ 
    lists:nthtail(Row, Grid).

% Update a row with the new number.
update_row(RowList, Col, Num) ->
    lists:sublist(RowList, 1, Col - 1) ++ 
    [Num] ++ 
    lists:nthtail(Col, RowList).

% Extract the column from the grid.
column(Grid, Col) ->
    [lists:nth(Col, Row) || Row <- Grid].

% Get the 3x3 subgrid.
subgrid(Grid, Row, Col) ->
    RowStart = ((Row - 1) div 3) * 3 + 1,
    ColStart = ((Col - 1) div 3) * 3 + 1,
    SubRows = lists:sublist(Grid, RowStart, 3),
    lists:flatten([lists:sublist(R, ColStart, 3) || R <- SubRows]).

% Print the Sudoku grid in a readable format.
print_grid(Grid) ->
    io:format("~nSolved Sudoku Grid:~n"),
    lists:foreach(fun(Row) -> 
        io:format("~s~n", [format_row(Row)])
    end, Grid).

% Format a row for printing.
format_row(Row) ->
    lists:flatten([if N =:= 0 -> "_ "; true -> integer_to_list(N) ++ " " end || N <- Row]).

