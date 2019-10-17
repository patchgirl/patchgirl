# apiTester

## Backend

``` bash
docker-compose -f  docker-compose.test.yml up # to run DB
cd api/
stack setup
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
