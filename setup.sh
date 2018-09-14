#!/bin/bash

set -e

exists () {
    type "$1" >/dev/null 2>/dev/null
}

echo '--> Backing up existing .emacs.d...'
mv ~/.emacs.d ~/.emacs.d.bak
echo '--> Back up done (at ~/.emacs.d.bak)!'
echo '--> Cloning d4ncer/.emacs.d...'
git clone git@github.com:d4ncer/.emacs.d.git ~/.emacs.d
echo '--> Done!'

# Rust setup
if (exists rustup && exists cargo); then
    echo '--> Installing required Rust binaries...'
    rustup component add rust-src
    cargo install racer
    echo '--> Done!'
else
    echo '--> It looks like you do not have rustup and cargo installed.'
    echo '--> Not installing Rust binaries.'
fi

# Go setup
if exists go; then
    echo '--> Installing required Go binaries...'
    go get -u -v github.com/mdempsky/gocode
    go get -u -v github.com/rogpeppe/godef
    go get -u -v github.com/kardianos/govendor
    go get -u -v golang.org/x/tools/cmd/guru
    go get -u -v golang.org/x/tools/cmd/gorename
    go get -u -v golang.org/x/tools/cmd/goimports
    go get -u -v github.com/sqs/goreturns
    go get -u -v github.com/zmb3/gogetdoc
    echo '--> Done!'
else
    echo '--> It looks like you do not have go installed.'
    echo '--> Not installing Go binaries.'
fi

# JS setup
if (exists npm && exists node); then
    echo '--> Installing global NPM binaries...'
    npm install -g eslint flow prettier eslint-config-airbnb eslint-config-prettier \
        eslint-plugin-flowtype eslint-plugin-import eslint-plugin-jsx-a11y \
        eslint-plugin-prettier eslint-plugin-react
    echo '--> Done!'
else
    echo '--> It looks like you do not have npm && node installed.'
    echo '--> Not installing global NPM binaries.'
fi

if exists stylelint; then
    echo '--> Creating a boilerplate Stylelint config at ~/.stylelintrc.json...'
    cp ~/.emacs.d/mk/.stylelintrc.json ~/.stylelintrc.json
    echo '--> Done!'
else
    echo '--> It looks like you do not have a global install of Stylelint.'
    echo '--> Not creating a global .stylelintrc.json.'
fi

if exists eslint; then
    echo '--> Creating a boilerplate ESLint config at ~/.eslintrc.json...'
    cp ~/.emacs.d/mk/.eslintrc.json.sample ~/.eslintrc.json
    echo '--> Done!'
else
    echo '--> It looks like you do not have a global install of ESLint.'
    echo '--> Not creating a global .eslintrc.json.'
fi

echo '--> Your new emacs config is ready to go!'
