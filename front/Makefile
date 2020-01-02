all: client app

client: generate-elm-client.hs ../api/src/App.hs
	clear; cd ../api; stack exec -- runhaskell -isrc ../front/generate-elm-client.hs

app:
	clear; elm make elm/Main.elm --output=../public/js/app.js
