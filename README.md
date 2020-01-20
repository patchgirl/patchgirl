[![Build Status](https://travis-ci.com/matsumonkie/apiTester.svg?branch=master)](https://travis-ci.com/matsumonkie/apiTester)

# apiTester

## Database

### setup

```bash
cd migration/
bundle exec rake db:migrate RAILS_ENV=test|dev
```

### run

```bash
docker-compose -f docker-compose.test.yml up
```

### seed

```bash
bundle exec rake db:seed RAILS_ENV=dev
```

## Backend

```bash
cd api/
stack --nix setup
stack test --fast
```

To execute the test-suite faster while developing, do:
``` bash
chmod go-w .ghci .
stack exec ghci test/Spec.hs
```

and then at the `ghci` prompt do:

``` haskell
:main
```

to run the tests and

``` haskell
:r
:main
```

to reload the code (after making changes) and run the tests again.

To run the app, do:

``` bash
stack exec example-servant-minimal
```

Then you can query the server like this:

``` bash
curl localhost:3000/health
```

## Elm client generation

The front (elm) will need a client to communicate with the back (haskell).
This client can be generated with:

```
cd api/
stack exec -- runhaskell -isrc ../front/generate-elm-client.hs
```

## To Run the front end

1. install elm 0.19 https://guide.elm-lang.org/install.html

```
cd front/
elm make elm/Main.elm --output=../back/public/js/app.js

2. then open the `index.html` file in your browser
