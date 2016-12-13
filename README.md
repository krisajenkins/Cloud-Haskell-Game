# Santa's Present Drop - A Websockets Game

Santa has given up with chimneys and is just dropping presents from
the sky. Its your job to run through the city picking them up. But
there's a catch - you can't see the present. All you know is its
distance from three GPS base-stations.

Break out your websockets library and your finest trigonometry,
connect to the server and triangulate the position of the present
before anyone else does!

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

Take a look at [EchoGame](server/src/EchoGame.hs) to see how a basic
game is constructed. Then look
at [PresentDrop](server/src/PresentDrop.hs) for the real game
logic. All the websocket/cloud-haskell wiring is
in [Lib](server/src/Lib.hs).

## Copyright

© Kris Jenkins, 2016.
