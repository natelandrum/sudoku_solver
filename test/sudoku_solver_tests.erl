-module(sudoku_solver_tests).
-include_lib("eunit/include/eunit.hrl").

solve_test() ->
    ?assert(
        begin
            % Define a test grid and expected solution
            TestGrid = [
                    [0, 8, 3, 0, 7, 0, 0, 0, 0],
                    [9, 0, 0, 0, 4, 0, 0, 0, 0],
                    [2, 0, 4, 0, 0, 0, 7, 0, 8],
                    [3, 0, 0, 4, 6, 0, 5, 1, 0],
                    [0, 0, 0, 0, 3, 2, 0, 4, 0],
                    [8, 0, 6, 7, 0, 5, 3, 9, 2],
                    [0, 1, 8, 0, 9, 7, 0, 0, 3],
                    [5, 3, 0, 6, 0, 4, 0, 8, 0],
                    [4, 2, 0, 3, 8, 0, 0, 0, 5]
    ],
            {ok, SolvedGrid} = sudoku_solver:solve(TestGrid),
            
            SolvedGrid /= false andalso
            SolvedGrid == [
                [1, 8, 3, 2, 7, 6, 9, 5, 4],
                [9, 7, 5, 8, 4, 3, 2, 6, 1],
                [2, 6, 4, 1, 5, 9, 7, 3, 8],
                [3, 9, 2, 4, 6, 8, 5, 1, 7],
                [7, 5, 1, 9, 3, 2, 8, 4, 6],
                [8, 4, 6, 7, 1, 5, 3, 9, 2],
                [6, 1, 8, 5, 9, 7, 4, 2, 3],
                [5, 3, 7, 6, 2, 4, 1, 8, 9],
                [4, 2, 9, 3, 8, 1, 6, 7, 5]
                ]
        end
    ).

validate_grid_size_test() ->
    ?assert(
        begin
            % Define a test grid with an invalid size
            TestGrid = [
                    [0, 8, 3, 0, 7, 0, 0, 0, 0],
                    [9, 0, 0, 0, 4, 0, 0, 0, 0],
                    [2, 0, 4, 0, 0, 0, 7, 0, 8],
                    [3, 0, 0, 4, 6, 0, 5, 1, 0],
                    [0, 0, 0, 0, 3, 2, 0, 4, 0],
                    [8, 0, 6, 7, 0, 5, 3, 9, 2],
                    [0, 1, 8, 0, 9, 7, 0, 0, 3],
                    [5, 3, 0, 6, 0, 4, 0, 8, 0],
                    [4, 2, 0, 3, 8, 0, 0, 0, 5],
                    [0, 0, 0, 0, 0, 0, 0, 0, 0]
            ],
            {error, Reason} = sudoku_solver:validate_grid(TestGrid),
            Reason == "Invalid grid size. Grid must be 9x9."
        end
    ).


validate_grid_decimal_test() ->
    ?assert(
        begin
            % Define a test grid with an invalid number
            TestGrid = [
                    [0, 8, 3, 0, 7, 0, 0, 0, 0],
                    [9, 0, 0, 0, 4, 0, 0, 0, 0],
                    [2, 0, 4, 0, 0, 0, 7, 0, 8],
                    [3, 0, 0, 4, 6, 0, 5, 1, 0],
                    [0, 0, 0, 0, 3, 2, 0, 4, 0],
                    [8, 0, 6, 7, 0, 5, 3, 9, 2],
                    [0, 1, 8, 0, 9, 7, 0, 0, 3],
                    [5, 3, 0, 6, 0, 4, 0, 8.1, 0],
                    [4, 2, 0, 3, 8, 0, 0, 0, 5]
            ],
            {error, Reason} = sudoku_solver:validate_grid(TestGrid),
            Reason == "Grid contains invalid numbers. Only integers between 0 and 9 are allowed."
        end
    ).

validate_grid_negative_test() ->
    ?assert(
        begin
            % Define a test grid with an invalid number
            TestGrid = [
                    [0, 8, 3, 0, 7, 0, 0, 0, 0],
                    [9, 0, 0, 0, 4, 0, 0, 0, 0],
                    [2, 0, 4, 0, 0, 0, 7, 0, 8],
                    [3, 0, 0, 4, 6, 0, 5, 1, 0],
                    [0, 0, 0, 0, 3, 2, 0, 4, 0],
                    [8, 0, 6, 7, 0, 5, 3, 9, 2],
                    [0, 1, 8, 0, 9, 7, 0, 0, 3],
                    [5, 3, 0, 6, 0, 4, 0, -8, 0],
                    [4, 2, 0, 3, 8, 0, 0, 0, 5]
            ],
            {error, Reason} = sudoku_solver:validate_grid(TestGrid),
            Reason == "Grid contains invalid numbers. Only integers between 0 and 9 are allowed."
        end
    ).

validate_grid_letter_test() ->
    ?assert(
        begin
            % Define a test grid with an invalid number
            TestGrid = [
                    [0, 8, 3, 0, 7, 0, 0, 0, 0],
                    [9, 0, 0, 0, 4, 0, 0, 0, 0],
                    [2, 0, 4, 0, 0, 0, 7, 0, 8],
                    [3, 0, 0, 4, 6, 0, 5, 1, 0],
                    [0, 0, 0, 0, 3, 2, 0, 4, 0],
                    [8, 0, 6, 7, 0, 5, 3, 9, 2],
                    [0, 1, 8, 0, 9, 7, 0, 0, 3],
                    [5, 3, 0, 6, 0, 4, 0, h, 0],
                    [4, 2, 0, 3, 8, 0, 0, 0, 5]
            ],
            {error, Reason} = sudoku_solver:validate_grid(TestGrid),
            Reason == "Grid contains invalid numbers. Only integers between 0 and 9 are allowed."
        end
    ).

validate_initial_grid_row_test() ->
    ?assert(
        begin
            TestGrid = [
                    [0, 8, 3, 0, 7, 3, 0, 0, 0],
                    [9, 0, 0, 0, 4, 0, 0, 0, 0],
                    [2, 0, 4, 0, 0, 0, 7, 0, 8],
                    [3, 0, 0, 4, 6, 0, 5, 1, 0],
                    [0, 0, 0, 0, 3, 2, 0, 4, 0],
                    [8, 0, 6, 7, 0, 5, 3, 9, 2],
                    [0, 1, 8, 0, 9, 7, 0, 0, 3],
                    [5, 3, 0, 6, 0, 4, 0, 8, 0],
                    [4, 2, 0, 3, 8, 0, 0, 0, 5]
            ],
            {error, Reason} = sudoku_solver:validate_initial_grid(TestGrid),
            Reason == "Duplicate numbers found in a row."
        end
    ).

validate_initial_grid_column_test() ->
    ?assert(
        begin
            TestGrid = [
                    [0, 8, 3, 0, 7, 0, 0, 0, 0],
                    [9, 0, 3, 0, 4, 0, 0, 0, 0],
                    [2, 0, 4, 0, 0, 0, 7, 0, 8],
                    [3, 0, 0, 4, 6, 0, 5, 1, 0],
                    [0, 0, 0, 0, 3, 2, 0, 4, 0],
                    [8, 0, 6, 7, 0, 5, 3, 9, 2],
                    [0, 1, 8, 0, 9, 7, 0, 0, 3],
                    [5, 3, 0, 6, 0, 4, 0, 8, 0],
                    [4, 2, 0, 3, 8, 0, 0, 0, 5]
            ],
            {error, Reason} = sudoku_solver:validate_initial_grid(TestGrid),
            Reason == "Duplicate numbers found in a column."
        end
    ).

validate_initial_grid_subgrid_test() ->
    ?assert(
        begin
            TestGrid = [
                    [0, 8, 3, 0, 7, 0, 0, 0, 0],
                    [9, 0, 0, 0, 4, 0, 0, 0, 7],
                    [2, 0, 4, 0, 0, 0, 7, 0, 8],
                    [3, 0, 0, 4, 6, 0, 5, 1, 0],
                    [0, 0, 0, 0, 3, 2, 0, 4, 0],
                    [8, 0, 6, 7, 0, 5, 3, 9, 2],
                    [0, 1, 8, 0, 9, 7, 0, 0, 3],
                    [5, 3, 0, 6, 0, 4, 0, 8, 0],
                    [4, 2, 0, 3, 8, 0, 0, 0, 5]
            ],
            {error, Reason} = sudoku_solver:validate_initial_grid(TestGrid),
            Reason == "Duplicate numbers found in a subgrid."
        end
    ).