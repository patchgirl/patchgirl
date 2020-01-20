run:
	clear; stack exec patchGirl

app:
	clear; stack build

check:
	clear; stack build --ghc-options="-Wall -Werror"

check-test:
	clear; stack test --ghc-options="-Wall -Werror -fno-warn-name-shadowing"

watch-test:
	ghcid --command 'stack ghci --test --main-is patchGirl:test:spec' --test ':main' --warnings

check-dhall:
	clear; dhall --explain --file ./config.dhall

check-hlint:
	hlint --report src/
