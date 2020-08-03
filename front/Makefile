all: client app

client: ../back/generate-elm-client.hs
	clear; echo "building front/client"; cd ../back/; stack runhaskell --package patchgirl-web --package patchgirl-runner generate-elm-client.hs

app:
	clear; echo "building front/app"; npx elm make elm/Main.elm elm/AppLoader.elm --output=../public/js/app.js

watch-test:
	clear; npx elm-test --watch

watch:
	clear; npx chokidar 'elm/**/*.elm' -c 'clear; elm make elm/Main.elm elm/AppLoader.elm --output=../public/js/app.js'

debug:
	clear; echo "building front/app";  elm make elm/Main.elm elm/AppLoader.elm --output=../public/js/app.js --debug

format:
	clear; npx elm-format elm/

analyze:
	clear; npx elm-analyze -s

check:
	clear; npx elm-review
