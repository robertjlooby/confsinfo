# confs.info

[![Build Status](https://travis-ci.org/robertjlooby/confsinfo.svg?branch=master)](https://travis-ci.org/robertjlooby/confsinfo)

## Running

- `cd frontend`
- `elm package install`
- `elm make src/Main.elm --output ../frontend/dist/elm.min.js`
- `cd ../backend`
- `stack build`
- `stack exec confsinfo-backend-exe`
- `open http://localhost:3000`

## Tests

- `cd frontend/test`
- `elm package install`
- `npm install`
- `node_modules/elm-test/bin/elm-test AllTests.elm`

and

- `cd backend`
- `stack test`

## Rationale

I wanted to make a central place for designers and developers to find
conferences they might be interested in. Conferences in any language, location,
or focus are welcome so long as they:

- Are targeted at designers and/or developers
- Have a set location and date
- Are the type of conference one might travel to (no local meetups, etc.)

If you know of a conference this list is missing (or want to add a feature), open a pull request!
