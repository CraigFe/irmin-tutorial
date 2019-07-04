MDX_PATH=ocaml-mdx
MDBOOK:=`which mdbook`

build: check
ifndef MDBOOK
	echo "WARNING: mdBook not installed, not building HTML book output"
else
	mdbook build
endif

check:
	$(MDX_PATH) pp src/Introduction.md > src/book.ml
	$(MDX_PATH) pp src/Contents.md  >> src/book.ml
	$(MDX_PATH) pp src/UsingTheCommandLine.md >> src/book.ml
	$(MDX_PATH) pp src/GettingStartedOCaml.md >> src/book.ml
	$(MDX_PATH) pp src/Backend.md  >> src/book.ml
	$(MDX_PATH) pp src/GraphQL.md >> src/book.ml
	ocamlbuild -pkgs checkseum.c,digestif.c,irmin-unix -pkg hiredis src/book.native

run: check
	./book.native

clean:
	rm -rf _build ./book.native

publish: check
	rm -rf .gh-pages
	git clone `git config --get remote.origin.url` .gh-pages --reference .
	git -C .gh-pages checkout --orphan gh-pages
	git -C .gh-pages reset
	git -C .gh-pages clean -dxf
	cp -r book/* .gh-pages/
	git -C .gh-pages add .
	git -C .gh-pages commit -m "Update Pages"
	git -C .gh-pages push origin gh-pages -f
	rm -rf .gh-pages

generate:
	ocamlbuild -pkg omd -pkg str gen/build.native
	./build.native

website: generate
	git clone https://github.com/tarides/irmin.io.git || (cd irmin.io && git pull origin master)
	mkdir -p irmin.io/tutorial
	mv out/* irmin.io/tutorial
