run:
	clear; stack exec patchGirl

app:
	clear; echo "building back/app"; stack build --ghc-options="-threaded -rtsopts -with-rtsopts=-T"

install:
	clear; echo "installing binary"; stack build --copy-bins

check:
	clear; ghcid --command 'stack ghci --test --main-is patchGirl:test:spec --ghc-options="-Wall -Werror -fno-warn-name-shadowing"' --test ':main' --warnings

watch-test:
	ghcid --command 'stack ghci --test --main-is patchGirl:test:spec' --test ':main' --warnings

check-dhall:
	clear; dhall --explain --file ./config.dhall

hlint:
	clear; hlint .
