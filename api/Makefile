run:
	clear; stack exec example-servant-minimal

app:
	clear; stack build

watch-test:
	ghcid --command 'stack ghci --test --main-is example-servant-minimal:test:spec' --test ':main' --warnings

check-dhall:
	clear; dhall --explain --file ./config.dhall
