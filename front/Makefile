all: client app

client: generate-elm-client.hs ../back/src/App.hs
	clear; cd ../back; stack exec -- runhaskell -isrc ../front/generate-elm-client.hs

app:
	clear; elm make elm/Main.elm --output=../public/js/app.js

format:
	clear; elm-format elm/

analyze:
	clear; elm-analyze -s
