# ExpoServerSdk (Erlang)

An Erlang library for sending push notifications to Expo applications.

## Installation

Add `expo_server_sdk` to your rebar.config dependencies:

```erlang
{deps, [
    {expo_server_sdk, "0.0.2"}
]}.
```

## Usage

### Single Message

```erlang
%% Create a single message map
Message = #{
    to => "ExponentPushToken[XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX]",
    title => "Pushed!",
    body => "You got your first message"
},
AccessToken = "$YourAccessToken",
{ok, Response} = expo_server_sdk_push_notification:push(Message, AccessToken).
```
