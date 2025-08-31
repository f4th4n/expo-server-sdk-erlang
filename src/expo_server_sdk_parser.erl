-module(expo_server_sdk_parser).
-author("Wildan Fathan").

-export([
    parse/1,
    parse_list/1
]).

-type http_status_code() :: integer().
-type success() :: {ok, map()}.
-type success_list() :: {ok, [map()]}.
-type error() :: {error, string(), http_status_code()}.
-type parsed_response() :: success() | error().
-type parsed_list_response() :: success_list() | error().

-export_type([
    http_status_code/0,
    success/0,
    success_list/0,
    error/0,
    parsed_response/0,
    parsed_list_response/0
]).

%% @doc
%% Parse a response expected to contain a single Map
%% @end
-spec parse(map()) -> success() | error().
parse(Response) ->
    handle_errors(Response, fun(Body) ->
        case json:decode(Body) of
            {error, _} = Error ->
                Error;
            Json ->
                maps:get(<<"data">>, Json)
        end
    end).

%% @doc
%% Parse a response expected to contain a list of Maps
%% @end
-spec parse_list(map()) -> success_list() | error().
parse_list(Response) ->
    handle_errors(Response, fun(Body) ->
        case json:decode(Body) of
            {error, _} = Error ->
                Error;
            Json ->
                maps:get(<<"data">>, Json)
        end
    end).

%% Internal function to handle HTTP errors
-spec handle_errors(map(), fun((binary()) -> any())) -> success() | error().
handle_errors(#{body := Body, status_code := Status}, Fun) when Status =:= 200; Status =:= 201 ->
    case Fun(Body) of
        {error, _} = Error ->
            Error;
        Result ->
            {ok, Result}
    end;
handle_errors(#{body := _, status_code := 204}, _Fun) ->
    ok;
handle_errors(#{body := Body, status_code := Status}, _Fun) ->
    case json:decode(Body) of
        {error, _} = Error ->
            Error;
        Json ->
            Errors = maps:get(<<"errors">>, Json, "Unknown error"),
            {error, Errors, Status}
    end.