# confs.info

[![CircleCI](https://circleci.com/gh/robertjlooby/confsinfo/tree/master.svg?style=svg)](https://circleci.com/gh/robertjlooby/confsinfo/tree/master)

## Dependencies

- [elm](https://guide.elm-lang.org/install.html)
- [docker](https://www.docker.com/products/overview)

## Running

#### Compile Elm code:
- `cd frontend`
- `elm package install`
- `elm make src/Main.elm --output ../frontend/dist/elm.min.js`

#### Build and run server
- `cd ../backend`
- `docker-compose up`
- `open http://localhost:3000/index.html`

## Tests

- `cd frontend/test`
- `elm package install`
- `npm install`
- `node_modules/elm-test/bin/elm-test AllTests.elm`

and

- `cd backend`
- `docker-compose -f docker-compose.test.yml run test`

## Rationale

I wanted to make a central place for designers and developers to find
conferences they might be interested in. Conferences in any language, location,
or focus are welcome so long as they:

- Are targeted at designers and/or developers
- Have a set location and date
- Are the type of conference one might travel to (no local meetups, etc.)

If you know of a conference this list is missing (or want to add a feature), open a pull request!
