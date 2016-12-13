# Cloud Haskell Game

This is a game server that uses websockets and Cloud Haskell.

# Building

## Client

```
cd client
make -w
```

## Server

```
cd server
stack build
stack exec cloud-haskell-game
```

...and navigate to http://localhost:9000.


## Developing

Take a look at `server/src/EchoGame.hs` to see how a basic game is constructed.

## Copyright

© Kris Jenkins, 2016.
