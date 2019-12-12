all: elm/Api/Client.elm app.js

elm/Api/Client.elm: generate-elm-client.hs ../api/src/App.hs
	cd ../api; stack exec -- runhaskell -isrc ../front/generate-elm-client.hs

app.js:
	elm make elm/Main.elm --output=../public/js/app.js
