all: install

install:
	elm-package install

serve:
	elm-app start

test:
	elm-app test
