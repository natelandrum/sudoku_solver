-module(sudoku_solver).
-export([start/0, solve/1, sudoku_grid/0, validate_grid/1, validate_initial_grid/1]).

% Sample Sudoku puzzle with 0s as placeholders for empty cells.
sudoku_grid() ->
    [
        [0, 8, 3, 0, 7, 0, 0, 0, 0],
        [9, 0, 0, 0, 4, 0, 0, 0, 0],
        [2, 0, 4, 0, 0, 0, 7, 0, 8],
        [3, 0, 0, 4, 6, 0, 5, 1, 0],
        [0, 0, 0, 0, 3, 2, 0, 4, 0],
        [8, 0, 6, 7, 0, 5, 3, 9, 2],
        [0, 1, 8, 0, 9, 7, 0, 0, 3],
        [5, 3, 0, 6, 0, 4, 0, 8, 0],
        [4, 2, 0, 3, 8, 0, 0, 0, 5]
    ].

% Start the solver on the sample grid.
start() ->
    Grid = sudoku_grid(),
    case solve(Grid) of
        {ok, SolvedGrid} -> print_grid(SolvedGrid);
        {error, Reason} -> io:format("Error: ~s~n", [Reason])
    end.

% Solve the Sudoku puzzle.
solve(Grid) ->
    case validate_grid(Grid) of
        {error, Reason} -> {error, Reason};
        {ok, ValidGrid} ->
            case validate_initial_grid(ValidGrid) of
                {error, Reason} -> {error, Reason};
                {ok} ->
                    case solve_grid(ValidGrid) of
                        false -> {error, "Unsolvable puzzle"};
                        SolvedGrid -> {ok, SolvedGrid}
                    end
            end
    end.

% Solve the grid recursively.
solve_grid(Grid) ->
    case find_empty(Grid) of
        {error, none} -> Grid;  % Puzzle solved
        {ok, Row, Col} ->
            try_numbers(Grid, Row, Col, lists:seq(1, 9))
    end.

% Try placing numbers from 1 to 9 in the given empty cell.
try_numbers(_Grid, _Row, _Col, []) ->
    false;  % Backtrack
try_numbers(Grid, Row, Col, [Num | RestNums]) ->
    case valid_number(Grid, Row, Col, Num) of
        true ->
            NewGrid = update_grid(Grid, Row, Col, Num),
            Solved = solve_grid(NewGrid),
            case Solved of
                false -> try_numbers(Grid, Row, Col, RestNums);
                _ -> Solved
            end;
        false -> try_numbers(Grid, Row, Col, RestNums)
    end.

% Validate the grid size and contents.
validate_grid(Grid) ->
    case length(Grid) =:= 9 andalso lists:all(fun(Row) -> length(Row) =:= 9 end, Grid) of
        true ->
            case lists:all(fun(Row) ->
                lists:all(fun(X) -> is_integer(X) andalso X >= 0 andalso X =< 9 end, Row)
            end, Grid) of
                true -> {ok, Grid};
                false -> {error, "Grid contains invalid numbers. Only integers between 0 and 9 are allowed."}
            end;
        false -> {error, "Invalid grid size. Grid must be 9x9."}
    end.

% Validate that the initial grid doesn't violate Sudoku rules.
validate_initial_grid(Grid) ->
    case validate_rows(Grid) of
        {error, Reason} -> {error, Reason};
        {ok} ->
            case validate_columns(Grid) of
                {error, Reason} -> {error, Reason};
                {ok} ->
                    case validate_subgrids(Grid) of
                        {error, Reason} -> {error, Reason};
                        {ok} -> {ok}
                    end
            end
    end.

% Validate rows for duplicates.
validate_rows(Grid) ->
    case lists:all(fun(Row) -> no_duplicates_in_list(Row) end, Grid) of
        true -> {ok};
        false -> {error, "Duplicate numbers found in a row."}
    end.

% Validate columns for duplicates.
validate_columns(Grid) ->
    case lists:all(fun(ColIndex) ->
        Column = column(Grid, ColIndex),
        no_duplicates_in_list(Column)
    end, lists:seq(1, 9)) of
        true -> {ok};
        false -> {error, "Duplicate numbers found in a column."}
    end.

% Validate subgrids for duplicates.
validate_subgrids(Grid) ->
    Positions = [{Row, Col} || Row <- [1,4,7], Col <- [1,4,7]],
    case lists:all(fun({Row, Col}) ->
        Subgrid = subgrid(Grid, Row, Col),
        no_duplicates_in_list(Subgrid)
    end, Positions) of
        true -> {ok};
        false -> {error, "Duplicate numbers found in a subgrid."}
    end.

% Check for duplicates in a list (ignoring zeros).
no_duplicates_in_list(List) ->
    Numbers = lists:filter(fun(X) -> X =/= 0 end, List),
    SortedNumbers = lists:sort(Numbers),
    SortedNumbers == lists:usort(SortedNumbers).

% Find the first empty cell.
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
    ColList = column(Grid, Col),
    Subgrid = subgrid(Grid, Row, Col),
    not lists:member(Num, RowList) andalso
    not lists:member(Num, ColList) andalso
    not lists:member(Num, Subgrid).

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
    lists:foreach(fun({Row, Index}) -> 
        io:format("~s~n", [format_row(Row)]),
        if (Index + 1) rem 3 =:= 0 andalso (Index + 1) =/= length(Grid) -> io:format("~n");
           true -> ok
        end
    end, lists:zip(Grid, lists:seq(0, length(Grid) - 1))).

% Format a row for printing.
format_row(Row) ->
    lists:flatten([if N =:= 0 -> "_ ";
        true -> integer_to_list(N) ++ " " end 
        ++ (if (Index + 1) rem 3 =:= 0 andalso (Index + 1) =/= length(Row) -> "\t"; 
        true -> "" end) || {N, Index} <- lists:zip(Row, lists:seq(0, length(Row) - 1))]).
