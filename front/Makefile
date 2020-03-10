all: client app

client: ../back/generate-elm-client.hs ../back/src/App.hs
	clear; echo "building front/client"; cd ../back; stack runhaskell generate-elm-client.hs

app:
	clear; echo "building front/app"; elm make elm/Main.elm --output=../public/js/app.js

debug:
	clear; echo "building front/app"; elm make elm/Main.elm --output=../public/js/app.js --debug

format:
	clear; elm-format elm/

analyze:
	clear; elm-analyze -s
