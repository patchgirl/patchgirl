run:
	clear; stack exec patchGirl

app:
	clear; echo "building back/app"; stack build --ghc-options="-threaded -rtsopts -with-rtsopts=-T -Werror"

install:
	clear; echo "installing binary"; stack build --copy-bins

check:
	clear; ghcid --command 'stack ghci --test --main-is patchGirl:test:spec --ghc-options="-Werror"' --test ':main' --warnings

watch-test:
	ghcid --command 'stack ghci --test --main-is patchGirl:test:spec' --test ':main' --warnings

check-dhall:
	clear; dhall --explain --file ./config.dhall

hlint:
	clear; hlint .
