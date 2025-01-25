%%%-------------------------------------------------------------------
%% @doc sudoku_solver public API
%% @end
%%%-------------------------------------------------------------------

-module(sudoku_solver_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
    {'_', [
        {"/solve", sudoku_solver_handler, []}
    ]}
]),
{ok, _} = cowboy:start_clear(http_listener,
    [{port, 8080}],
    #{env => #{dispatch => Dispatch}}
),
    sudoku_solver_sup:start_link().

stop(_State) ->
    ok.

%% internal functions