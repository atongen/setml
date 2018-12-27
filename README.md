# setml

Online, realtime, multiplayer game of Set, from [Set Enterprises, Inc.](https://www.setgame.com/), built entirely with ocaml (and reasonml).

Play the game here: https://setml.andrewtongen.net/

Outside of the websocket connection, the app server processes are completely stateless.
All messages are synchronized with postgresql using listen/notify.

Server-side ocaml code is found in `src/server`.
Client-side reasonml code is found in `src/client`.
Shared modules are found in `src/shared`.

On the client-side, all cards are rendered to a hidden canvas with svg, and then copied to the visible canvas to update the game board.

## prerequisites

* ocaml 4.06.1
* dune
* opam
* npm
* make
* postgresql 11
* linux (untested on macos)

## building and running

### check out the code

```
git clone git@github.com/atongen/setml
cd setml
```

### envfile

Provision a postgres db for setml and create an `envfile` with the connection information.

The default values look similar to this:

```
DB_NAME=setml_development
DB_HOST=localhost
DB_PORT=5432
DB_USER=`whoami`
DB_PASS=abcd1234
```

Add entries only for what you need to override.

### server-side

```
opam pin add -yn setml .
opam install setml
make
```

### client-side

```
npm install
npm start
```

and in separate terminal:

```
npm run-script webpack
```

You may want to refer to https://reasonml.github.io/reason-react/docs/en/installation for more information about installing bucklescript/reasonml.

## running

With db created, `envfile` in place, you can run

```
script/reload_db.sh
```

to load the database schema.

Then run:

```
script/run.sh
```

to start the server.

Browse to `http://localhost:7777/` to access the application.

## testing

Provision a test database in postgres (this is `setml_test` by convention), then run:

```
SETML_ENV=test script/reload_db.sh
```

To populate the test db schema.

Then run

```
make test
```

To run the test suite against both the server-side code and client-side code.

## docker

Docker images are provided on [dockerhub](https://cloud.docker.com/repository/docker/atongen/setml).

```
docker pull atongen/setml
docker-compose up
```

## server-side resources

* https://www.reddit.com/r/ocaml/comments/510o92/what_would_an_ocaml_entry_to_hashrockets/
* https://gist.github.com/copy/8f71a129e9b7ff64c262cbabf191e2a5
* https://github.com/vbmithr/ocaml-websocket
* https://github.com/mirage/ocaml-cohttp

## client-side resources

* https://reasonml.github.io/reason-react/
* https://react.rocks/tag/Game
* https://github.com/chriz001/Reacteroids
* https://www.heropatterns.com/

## License
This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details
