# Cloud Haskell Game

This is a game server that uses websockets and Cloud Haskell.

## Why Cloud Haskell?

Not for distributed-processing, though that's a nice bonus. I'm
actually using it because I want to separate out the input and
outbound websocket messages, without worrying too much about threading
or synchronisation myself. So Cloud Haskell is really a nice way to
get typed channels.

Why not CHP? It hasn't been updated for the AFP, so I dropped it.

## Structure

There is a GAME_SERVER process, waiting for game commands. Whenever
the state of the game changes, it sends the new game state to the
BROADCASTER.

The BROADCASTER keeps a list of subscribers. It listens for game
changes, and sends the new game to any subscribed PLAYERs. It also
listens for "subscribe me" and "unsubscribe me" messages, which update
that subscriber list.

When a player connects on a websocket, we spawn two processes - one
just does a blocking reading from the websocket, and forwards any
commands to the GAME_SERVER (by default) or BROADCASTER (for sub/unsub
commands).
The other process just listens for new game state messages, and writes
those to the websocket.

So the typical flow of a message is:

```
User
-> Websocket
-> PLAYER READ
-> GAME_SERVER
-> BROADCASTER
-> PLAYER WRITE
-> Websocket
-> User
```

## TODO
- [X] Check win condition.
- [X] Respawn present.
- [X] Randomise present.
- [X] Throttle commands.
- [X] Set color.
- [X] Normalize move distance.

- [ ] Deploy
- [ ] Send board on connect.
- [ ] Visualise

- [ ] Join & Leave aren't useful. Pub and sub would be.
- [ ] Leave on disconnect.
- [ ] Join on connect. Send board.
- [ ] Look command. (Broadcaster could hold the last-seen state.)

- [ ] Make one radar unreliable.

- [X] Include radars in view.
- [ ] Could really do with some proper monitoring.
- [ ] All the channel/process names are horrible.
- [ ] Refactor.
