# ExpoServerSdk (Erlang)

[![Hex.pm](https://img.shields.io/hexpm/v/expo_server_sdk.svg)](https://hex.pm/packages/expo_server_sdk)

An Erlang library for sending push notifications to Expo applications.

## Installation

Add `expo_server_sdk` to your rebar.config dependencies:

```erlang
{deps, [
    {expo_server_sdk, "0.0.2"}
]}.
```

## Usage

### Send Single Message

```erlang
Message = #{
    to => "ExponentPushToken[XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX]",
    title => "Pushed!",
    body => "You got your first message"
},
AccessToken = "$YourAccessToken", % you can set this nil if it's public
{ok, Response} = expo_server_sdk_push_notification:push(Message, AccessToken).
```

### Send Bulk Messages

```erlang
Message = #{
    to => "ExponentPushToken[XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX]",
    title => "Pushed!",
    body => "You got your first message"
},
AccessToken = "$YourAccessToken",
{ok, Response} = expo_server_sdk_push_notification:push_list([Message], AccessToken).
```

### Get Receipts

```erlang
%% Get delivery receipts
Ids = [
    "XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX",
    "XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX"
],
AccessToken = "$YourAccessToken",
expo_server_sdk_push_notification:get_receipts(Ids, AccessToken).
```
