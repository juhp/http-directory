help:
	@echo "devel targets: git-tag sdist version upload git-push copy"

sdist:
	./make-dist $(VERSION)

upload:
	cabal upload dist/$(NAME)-$(VERSION).tar.gz

NAME= http-directory
VERSION := $(shell sed -ne 's/^[Vv]ersion:[[:space:]]*//p' $(NAME).cabal)

version:
	@echo $(VERSION)

git-tag:
	git tag $(VERSION)

git-push:
	git push
	git push --tags

copy:
	cp -p dist/$(NAME)-$(VERSION).tar.gz ~/fedora/haskell/ghc-$(NAME)/

publish:
	cabal upload --publish dist/$(NAME)-$(VERSION).tar.gz
