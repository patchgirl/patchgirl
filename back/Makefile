run:
	clear; stack exec patchGirl

app:
	clear; stack build

check:
	clear; ghcid --command 'stack ghci --test --main-is patchGirl:test:spec --ghc-options="-Wall -Werror -fno-warn-name-shadowing"' --test ':main' --warnings

watch-test:
	ghcid --command 'stack ghci --test --main-is patchGirl:test:spec' --test ':main' --warnings

check-dhall:
	clear; dhall --explain --file ./config.dhall

check-hlint:
	clear; hlint .
