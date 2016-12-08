#
#
#

all: clean install build

build: ;@echo "-- Buidling project"
# elm-make src/App.elm --output=./dist/app.js
	npm run build

dev: ;@echo "-- Staring dev server"
# elm reactor ./src
	npm run dev

clean: ;@echo "-- Cleaning up dist files"
	rm -rf dist/

install: ;@echo "-- Installing dependencies"
	elm package install
	npm i

watch: ;@echo "-- watching files"
	npm run watch
