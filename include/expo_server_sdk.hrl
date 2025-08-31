-record(push_message, {
    to :: string(),
    data = undefined :: map() | undefined,
    title = undefined :: string() | undefined,
    body = undefined :: string() | undefined,
    ttl = 0 :: integer(),
    expiration = undefined :: integer() | undefined,
    priority = "default" :: string(),
    sound = "default" :: string() | undefined,
    badge = undefined :: integer() | undefined,
    channel_id = undefined :: string() | undefined
}).