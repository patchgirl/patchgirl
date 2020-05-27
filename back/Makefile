run:
	clear; stack exec patchgirl-$(APP)-exe

both:
	clear; make app APP=web; make app APP=desktop

app:
	clear; echo "building back/patchgirl-$(APP)"; stack build patchgirl-$(APP) --ghc-options="-threaded -rtsopts -with-rtsopts=-T -Werror"

test:
	clear; echo "testing back/patchgirl-$(APP)"; stack test patchgirl-$(APP) --ghc-options="-Werror"

watch-test:
	clear; ghcid --command 'stack ghci patchgirl-$(APP) --test --main-is patchgirl-$(APP):test:spec' --test 'main' --warnings

check:
	clear; ghcid --command 'stack ghci patchgirl-$(APP) --test --main-is patchgirl-$(APP):test:spec --ghc-options="-Werror"' --test ':main' --warnings

install:
	clear; echo "installing binary"; stack build --copy-bins

check-dhall:
	clear; dhall --explain --file ./config.dhall

hlint:
	clear; hlint .
