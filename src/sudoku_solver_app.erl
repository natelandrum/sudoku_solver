%%%-------------------------------------------------------------------
%% @doc sudoku_solver public API
%% @end
%%%-------------------------------------------------------------------

-module(sudoku_solver_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    sudoku_solver_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
