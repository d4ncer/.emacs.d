#!/bin/bash

set -e

mv ~/.emacs.d ~/.emacs.d.bak
git clone git@github.com:d4ncer/.emacs.d.git ~/.emacs.d

go get -u -v github.com/nsf/gocode
go get -u -v github.com/rogpeppe/godef
go get -u -v github.com/kardianos/govendor
go get -u -v golang.org/x/tools/cmd/guru
go get -u -v golang.org/x/tools/cmd/gorename
go get -u -v golang.org/x/tools/cmd/goimports

cd ~/.emacs.d/lisp/tern

npm install

npm install -g stylefmt eslint flow prettier eslint-config-airbnb eslint-config-prettier

cp ~/.emacs.d/.eslintrc.json.sample ~/.eslintrc.json
