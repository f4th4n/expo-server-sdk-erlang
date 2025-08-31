-module(expo_server_sdk_push_message).
-author("Wildan Fathan").

-include("expo_server_sdk.hrl").

-export([
    create/1,
    create_from_list/1,
    create_receipt_id_list/1,
    to_map/1
]).

-type push_message() :: #push_message{}.
-export_type([push_message/0]).

%% @doc
%% Create a PushMessage record from a single message map.
%% @end
-spec create(map()) -> push_message().
create(MessageMap) when is_map(MessageMap) ->
    #push_message{
        to = maps:get(to, MessageMap),
        data = maps:get(data, MessageMap, undefined),
        title = maps:get(title, MessageMap, undefined),
        body = maps:get(body, MessageMap, undefined),
        ttl = maps:get(ttl, MessageMap, 0),
        expiration = maps:get(expiration, MessageMap, undefined),
        priority = maps:get(priority, MessageMap, "default"),
        sound = maps:get(sound, MessageMap, "default"),
        badge = maps:get(badge, MessageMap, undefined),
        channel_id = maps:get(channelId, MessageMap, undefined)
    }.

%% @doc
%% Create a List of PushMessage records from a list of maps chunked into lists of 100.
%% @end
-spec create_from_list([map()]) -> [[push_message()]].
create_from_list(Messages) when is_list(Messages) ->
    MessageStructs = lists:map(fun create/1, Messages),
    chunk_list(MessageStructs, 100).

%% @doc
%% Create a List of PushMessageIds from a list.
%% @end
-spec create_receipt_id_list([string()]) -> [string()].
create_receipt_id_list(PushTokenIds) when is_list(PushTokenIds) ->
    PushTokenIds.

%% Internal function to chunk list into sublists of specified size
-spec chunk_list([T], pos_integer()) -> [[T]].
chunk_list(List, ChunkSize) ->
    chunk_list(List, ChunkSize, []).

chunk_list([], _ChunkSize, Acc) ->
    lists:reverse(Acc);
chunk_list(List, ChunkSize, Acc) ->
    {Chunk, Rest} = lists:split(min(ChunkSize, length(List)), List),
    chunk_list(Rest, ChunkSize, [Chunk | Acc]).

%% @doc
%% Convert a PushMessage record to a map suitable for JSON encoding.
%% @end
-spec to_map(push_message()) -> map().
to_map(#push_message{
    to = To,
    data = Data,
    title = Title,
    body = Body,
    ttl = Ttl,
    expiration = Expiration,
    priority = Priority,
    sound = Sound,
    badge = Badge,
    channel_id = ChannelId
}) ->
    BaseMap = #{
        <<"to">> => list_to_binary(To)
    },
    
    % Add optional fields only if they have values
    Map1 = case Data of
        undefined -> BaseMap;
        _ -> BaseMap#{<<"data">> => Data}
    end,
    
    Map2 = case Title of
        undefined -> Map1;
        _ -> Map1#{<<"title">> => list_to_binary(Title)}
    end,
    
    Map3 = case Body of
        undefined -> Map2;
        _ -> Map2#{<<"body">> => list_to_binary(Body)}
    end,
    
    Map4 = case Ttl of
        0 -> Map3;
        _ -> Map3#{<<"ttl">> => Ttl}
    end,
    
    Map5 = case Expiration of
        undefined -> Map4;
        _ -> Map4#{<<"expiration">> => Expiration}
    end,
    
    Map6 = case Priority of
        "default" -> Map5;
        _ -> Map5#{<<"priority">> => list_to_binary(Priority)}
    end,
    
    Map7 = case Sound of
        "default" -> Map6;
        undefined -> Map6;
        _ -> Map6#{<<"sound">> => list_to_binary(Sound)}
    end,
    
    Map8 = case Badge of
        undefined -> Map7;
        _ -> Map7#{<<"badge">> => Badge}
    end,
    
    case ChannelId of
        undefined -> Map8;
        _ -> Map8#{<<"channelId">> => list_to_binary(ChannelId)}
    end.