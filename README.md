# Santa's Present Drop - A Websockets Game

Santa has given up with chimneys and is just dropping presents from
the sky. Its your job to run through the city picking them up. But
there's a catch - you don't know where the present is! All we can tell
you is its distance from three GPS base-stations.

Break out your websockets library and your finest trigonometry,
connect to the server and triangulate the position of the present
before anyone else does!

![Game Screenshot](action-shot.png?raw=true "Screenshot")

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

...and navigate to http://localhost:8000/.


## Developing

Take a look at [EchoGame](server/src/EchoGame.hs) to see how a basic
game is constructed. Then look
at [PresentDrop](server/src/PresentDrop.hs) for the real game
logic. All the websocket/cloud-haskell wiring is
in [Network.GameEngine](server/src/Network/GameEngine.hs).

## Solutions

* [Scala](https://github.com/mnd999/xmas-hack)
* [Haskell](https://github.com/mattjbray/santas-present-drop/blob/master/app/Main.hs)
* [Go](https://github.com/simonswine/hack-night-xmas-2016)
* [Elm](https://github.com/MatMoore/xmas-present-retrieval-hack)

## Copyright

© Kris Jenkins, 2016.
