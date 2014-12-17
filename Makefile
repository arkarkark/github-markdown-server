release: contrib/github-markdown-server.el
	gem build github-markdown-server.gemspec
	export VERSION=$(shell ./bin/github-markdown-server --version | cut -d ' ' -f 3); \
	gem install github-markdown-server-$$VERSION.gem; \
	gem push github-markdown-server-$$VERSION.gem

contrib/github-markdown-server.el: ~/Library/emacs/lisp/github-markdown-server.el
	\cp -fv $< $@
