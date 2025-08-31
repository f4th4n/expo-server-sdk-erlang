-module(expo_server_sdk_push_notification).
-author("Wildan Fathan").

-export([
    push/2,
    push_list/2,
    get_receipts/2
]).

-include_lib("kernel/include/logger.hrl").

%% @doc
%% Send the push notification request when using a single message map
%% @end
-spec push(map(), string() | nil) -> expo_server_sdk_parser:parsed_response().
push(Message, AccessToken) when is_map(Message) ->
    PushMessage = expo_server_sdk_push_message:create(Message),
    PushMessageMap = expo_server_sdk_push_message:to_map(PushMessage),
    Body = json:encode(PushMessageMap),
    GzipBody = zlib:gzip(Body),
    Headers = process_request_headers([], AccessToken),
    Url = process_url("send"),
    
    case httpc:request(post, {Url, Headers, "application/json", GzipBody}, [], []) of
        {ok, {{_Version, StatusCode, _ReasonPhrase}, _Headers, ResponseBody}} ->
            Response = #{body => list_to_binary(ResponseBody), status_code => StatusCode},
            expo_server_sdk_parser:parse(Response);
        {error, Reason} ->
            {error, atom_to_list(Reason), 0}
    end.

%% @doc
%% Send the push notification request when using a list of message maps
%% @end
-spec push_list([map()], string() | nil) -> expo_server_sdk_parser:parsed_list_response().
push_list(Messages, AccessToken) when is_list(Messages) ->
    MessageStructs = lists:map(fun expo_server_sdk_push_message:create/1, Messages),
    MessageMaps = lists:map(fun expo_server_sdk_push_message:to_map/1, MessageStructs),
    Body = json:encode(MessageMaps),
    GzipBody = zlib:gzip(Body),
    Headers = process_request_headers([], AccessToken),
    Url = process_url("send"),
    
    case httpc:request(post, {Url, Headers, "application/json", GzipBody}, [], []) of
        {ok, {{_Version, StatusCode, _ReasonPhrase}, _Headers, ResponseBody}} ->
            Response = #{body => list_to_binary(ResponseBody), status_code => StatusCode},
            expo_server_sdk_parser:parse_list(Response);
        {error, Reason} ->
            {error, atom_to_list(Reason), 0}
    end.

%% @doc
%% Send the get notification receipts request when using a list of ids
%% @end
-spec get_receipts([string()], string() | nil) -> expo_server_sdk_parser:parsed_response().
get_receipts(Ids, AccessToken) when is_list(Ids) ->
    ValidatedIds = expo_server_sdk_push_message:create_receipt_id_list(Ids),
    RequestBody = #{ids => ValidatedIds},
    Body = json:encode(RequestBody),
    GzipBody = zlib:gzip(Body),
    Headers = process_request_headers([], AccessToken),
    Url = process_url("getReceipts"),
    case httpc:request(post, {Url, Headers, "application/json", GzipBody}, [], []) of
        {ok, {{_Version, StatusCode, _ReasonPhrase}, _Headers, ResponseBody}} ->
            Response = #{body => list_to_binary(ResponseBody), status_code => StatusCode},
            expo_server_sdk_parser:parse(Response);
        {error, Reason} ->
            {error, atom_to_list(Reason), 0}
    end.

%% @doc
%% Automatically adds the correct url to each API request.
%% @end
-spec process_url(string()) -> string().
process_url(Url) ->
    "https://exp.host/--/api/v2/push/" ++ Url.

%% @doc
%% Automatically adds the correct headers to each API request.
%% @end
-spec process_request_headers([{string(), string()}], string() | nil) -> [{string(), string()}].
process_request_headers(Headers, AccessToken) ->
    BaseHeaders = [
        {"Accept", "application/json"},
        {"Accept-Encoding", "gzip, deflate"},
        {"Content-Encoding", "gzip"},
        {"Content-Type", "application/json"}
        | Headers
    ],
    case AccessToken of
        nil -> BaseHeaders;
        Token when is_list(Token) -> [{"Authorization", "Bearer " ++ Token} | BaseHeaders]
    end.
