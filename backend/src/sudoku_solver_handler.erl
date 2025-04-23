-module(sudoku_solver_handler).

-behaviour(cowboy_handler).

-export([init/2]).

-define(CONTENT_TYPE_JSON, #{<<"content-type">> => <<"application/json">>}).

init(Req, State) ->
    case cowboy_req:read_body(Req, #{length => 1000000}) of % Add a maximum length limit
        {ok, Body, Req1} ->
            Response = try
                ParsedBody = jsx:decode(Body, [return_maps]),
                case maps:get(<<"grid">>, ParsedBody, undefined) of
                    SudokuBoard when is_list(SudokuBoard) ->
                        % Use timeout to prevent long-running computations
                        try
                            {ok, _} = timer:kill_after(30000), % 30 second timeout
                            case sudoku_solver:solve(SudokuBoard) of
                                {ok, SolvedBoard} ->
                                    {200, jsx:encode(#{status => <<"success">>, solved_board => SolvedBoard})};
                                {error, SolveReason} ->
                                    {400, jsx:encode(#{status => <<"error">>, message => SolveReason})}
                            end
                        catch
                            exit:timeout ->
                                {408, jsx:encode(#{status => <<"error">>, message => <<"Solving operation timed out">>})}
                        end;
                    _ ->
                        {400, jsx:encode(#{status => <<"error">>, message => <<"Invalid Sudoku board structure or missing 'grid' property.">>})}
                end
            catch
                _:Reason ->
                    io:format("Error processing request: ~p~n", [Reason]),
                    {400, jsx:encode(#{status => <<"error">>, message => <<"Invalid JSON body.">>})}
            end,
            {Status, ResponseBody} = Response,
            cowboy_req:reply(Status, ?CONTENT_TYPE_JSON, ResponseBody, Req1);
        {error, ReadReason} ->
            io:format("Error reading body: ~p~n", [ReadReason]),
            ErrorResponse = jsx:encode(#{status => <<"error">>, message => <<"Unable to read request body.">>}),
            cowboy_req:reply(500, ?CONTENT_TYPE_JSON, ErrorResponse, Req)
    end,
    {ok, Req, State}.
