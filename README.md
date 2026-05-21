# TinIRC

## What is TinIRC? 

TinIRC ("Teenee RC") is a simple, minimalist, TUI IRC Client made in Haskell.

## Why Haskell?

It's fun and I like it :3c

## How to run?

Simply install [Cabal](https://www.haskell.org/cabal/) and execute `cabal run exes -- config.yaml`

The `config.yaml` file should look like this:
```yaml
server:
  hostname: irc.awesome.cool
  port: 443
  use_tls: true
  check_certificates: false
  channels:
    - "#general"
    - '#off-topic'
user:
  username: Isidore
  mode: 0
  realname: Isidore Beautrelet

client:
  sent_history_length: 65536
  channel_history_length: 65536
```

## Project overview:

```
.
|- app
  |- IrcParsing -> Everything related to the parsing of IRC messages
    |- Parsing.hs -> Code for parsing IRC messages
    |- Types.hs -> Structure for parsed IRC messages
  |- UserInterface -> Everything related to the User Interface (including inputs and outputs)
    |- Events.hs -> Event handling (i.e: displaying received messages, sending messages when enter is pressed, changing and joining channels etc.)
    |- JoinDialog.hs -> DialogBox for joining a new channel or entering a DM
    |- Main.hs -> The main entrypoint for the UserInterface. This is where the App is defined as well as the draw function
    |- MessageHandling.hs -> Everything related to handling of newly received messages (calling the parser and putting the messages at the right place)
    |- Types.hs -> Types for the App (Form and State, as well as channels) and utility functions on these types
    |- Widgets.hs -> Widget creation for the App
  |- Client.hs -> Client handling the networking
  |- Config.hs -> Parsing of the configuration file
  |- Main.hs -> The entrypoint, connecting to TCP/TLS and creating the client and UI App.
|- config/example.yaml -> There's no better documentation than an example :3
|- .gitignore
|- CHANGELOG.md
|- LICENSE
|- README.md
|- TinIRC.cabal
```

## TODO (non-exhaustive, I might do more than that, but these are the ones I plan on doing)

- [ ] Handling certificates
- [ ] Having an unread indicator

--- 

## Was this written by AI?

No. 100% of the code was handwritten!

This is a side project that I made for fun. I don't find using AI fun.
I like having to think hard about the code I write. I like learning new things,
making mistakes, and understanding what I do. I enjoy spending time
looking for documentation and trying to understand it. It's fun,
it's fulfilling.

If my goal was to have an IRC client, I would just download one!

So, I'm proud to say: for the better or for worse, this project was made with no AI! :3c

![No AI stamp from https://www.deviantart.com/triangle-mom/art/F2U-Anti-AI-Stamp-Alternate-colors-941484651](https://images-wixmp-ed30a86b8c4ca887773594c2.wixmp.com/f/80058940-5a81-45c0-bebd-76e65df3844d/dk18j6r-dd0924e5-2e2e-47bf-9980-d7961a8f27e2.png/v1/fill/w_99,h_56/fghjkjhgfdfghjgh_by_triangle_mom_dk18j6r-fullview.png?token=eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJ1cm46YXBwOjdlMGQxODg5ODIyNjQzNzNhNWYwZDQxNWVhMGQyNmUwIiwiaXNzIjoidXJuOmFwcDo3ZTBkMTg4OTgyMjY0MzczYTVmMGQ0MTVlYTBkMjZlMCIsIm9iaiI6W1t7ImhlaWdodCI6Ijw9NTYiLCJwYXRoIjoiXC9mXC84MDA1ODk0MC01YTgxLTQ1YzAtYmViZC03NmU2NWRmMzg0NGRcL2RrMThqNnItZGQwOTI0ZTUtMmUyZS00N2JmLTk5ODAtZDc5NjFhOGYyN2UyLnBuZyIsIndpZHRoIjoiPD05OSJ9XV0sImF1ZCI6WyJ1cm46c2VydmljZTppbWFnZS5vcGVyYXRpb25zIl19.AYrnp7oUHSW4lNVHRP5mUd1TPAPCxMnMY6I-9ofMUlc)
