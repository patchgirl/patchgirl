PACKAGE=patchgirl-$(APP)

run:
	clear; stack exec $(PACKAGE)-exe

both:
	clear; make app APP=web; make app APP=runner

app:
	clear; echo "building back/$(PACKAGE)"; stack build $(PACKAGE) --ghc-options="-threaded -rtsopts -with-rtsopts=-T -Werror"

static:
	clear; echo "building static back/$(PACKAGE)"; nix-build -A $(PACKAGE)-static

test:
	clear; echo "testing back/$(PACKAGE)"; LC_ALL=C.UTF-8 stack test $(PACKAGE) --ghc-options="-Werror"

watch-test:
	clear; LC_ALL=C.UTF-8 ghcid --directory="$(PACKAGE)" --command 'stack ghci $(PACKAGE) --test --main-is $(PACKAGE):test:spec' --test 'main' --warnings

check:
	clear; LC_ALL=C.UTF-8 ghcid --directory="$(PACKAGE)" --command 'stack ghci $(PACKAGE) --test --main-is $(PACKAGE):test:spec --ghc-options="-Werror"' --test ':main' --warnings

install:
	clear; echo "installing binary"; stack build --copy-bins

check-dhall:
	clear; dhall --explain --file ./web.dhall

hlint:
	clear; hlint .
