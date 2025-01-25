-module(sudoku_solver_handler).

-behaviour(cowboy_handler).

-export([init/2]).

-define(CONTENT_TYPE_JSON, #{<<"content-type">> => <<"application/json">>}).

init(Req, State) ->
    % Read and parse the JSON body
    case cowboy_req:read_body(Req) of
        {ok, Body, Req1} ->
            try
                ParsedBody = jsx:decode(Body, [return_maps]),
                case maps:get(<<"grid">>, ParsedBody, undefined) of
                    SudokuBoard when is_list(SudokuBoard) ->
                        % Solve the board
                        case sudoku_solver:solve(SudokuBoard) of
                            {ok, SolvedBoard} ->
                                Response = jsx:encode(#{status => <<"success">>, solved_board => SolvedBoard}),
                                {ok, Req2} = cowboy_req:reply(200, ?CONTENT_TYPE_JSON, Response, Req1),
                                {ok, Req2, State};
                            {error, Reason} ->
                                ErrorResponse = jsx:encode(#{status => <<"error">>, message => Reason}),
                                {ok, Req2} = cowboy_req:reply(400, ?CONTENT_TYPE_JSON, ErrorResponse, Req1),
                                {ok, Req2, State}
                        end;
                    _ ->
                        ErrorResponse = jsx:encode(#{status => <<"error">>, message => <<"Invalid Sudoku board structure or missing 'grid' property.">>}),
                        {ok, Req2} = cowboy_req:reply(400, ?CONTENT_TYPE_JSON, ErrorResponse, Req1),
                        {ok, Req2, State}
                end                
            catch
                _: Error ->
                    Error
            end;
        {error, Reason} ->
            io:format("Error reading body: ~p~n", [Reason]),
            ErrorResponse = jsx:encode(#{status => <<"error">>, message => <<"Unable to read request body.">>}),
            {ok, Req2} = cowboy_req:reply(500, ?CONTENT_TYPE_JSON, ErrorResponse, Req),
            {ok, Req2, State}
    end.
