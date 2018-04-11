# My .emacs setup

![web-mode-trifecta](https://raw.githubusercontent.com/d4ncer/.emacs.d/master/screenshots/web-mode-trifecta.png "JS + CSS + MD")

See more [here](SCREENSHOTS.md)

## Should I use it?

I would highly recommend you do not use it as-is. Here are a few reasons to hopefully discourage you:

* I have **no** syntax highlighting
* Not everyone likes Solarized Light as much as I do
* My bindings are haphazard at best, and a chaotic mix between the Emacs & Vim philosophies
* I don't support that one language mode that you can't live without
* I don't use yasnippet

If you're looking for a starting point, check out [Chris Barrett's](https://github.com/chrisbarrett "CB da bomb") [config for tips on getting started](https://github.com/chrisbarrett/.emacs.d#i-want-to-use-your-config "it's really good. you should use it")

## No but look I'm super lazy

Fine, but you were warned.

### Things you need

* Emacs 25+
* [Fira Code](https://github.com/tonsky/FiraCode)
* [Fira Code Symbol](https://github.com/tonsky/FiraCode/files/412440/FiraCode-Regular-Symbol.zip) (this is for ligatures in all `prog-mode` buffers)

### Quick install (recommended)

``` shell
wget -O - https://raw.githubusercontent.com/d4ncer/.emacs.d/master/setup.sh | bash
```

### Manual install

``` shell
mv ~/.emacs.d ~/.emacs.d.bak
git clone git@github.com:d4ncer/.emacs.d.git ~/.emacs.d
```

**Go setup**

Packages to install for Go config:

```bash
go get -u -v github.com/nsf/gocode
go get -u -v github.com/rogpeppe/godef
go get -u -v github.com/kardianos/govendor
go get -u -v golang.org/x/tools/cmd/guru
go get -u -v golang.org/x/tools/cmd/gorename
go get -u -v golang.org/x/tools/cmd/goimports
```

**JS setup**

Tern setup:

```bash
cd lisp/tern
npm install
```

Required binaries:

```bash
npm install -g stylefmt
```

Binaries for global fallback (this is optional, but highly recommended):

```bash
npm install -g eslint flow prettier eslint-config-airbnb eslint-config-prettier \
    eslint-plugin-flowtype eslint-plugin-import eslint-plugin-jsx-a11y \
    eslint-plugin-prettier  eslint-plugin-react
```

If you do use a global `eslint`, you should add a fallback `.eslintrc.json` in
your root folder. A sample:

```js
{
  "extends": ["airbnb", "prettier", "prettier/flowtype", "prettier/react"],
  "env": {
    "browser": true,
    "jest": true,
    "node": true
  },
  "plugins": ["react", "flowtype"],
  "parser": "babel-eslint",
  "rules": {
    "flowtype/define-flow-type": 1,
    "import/no-extraneous-dependencies": ["error", {
      "devDependencies": true,
      "optionalDependencies": false,
      "peerDependencies": false
    }
    ]
  }
}

```

## Packages of note

Most of my work is on the frontend, which is reflected in my setup. Some
packages of note:

* Evil
* Ivy (with `ripgrep`)
* Spacemacs leader keys
* Projectile
* Flycheck
* Magit
* Org
* IBuffer
* Treemacs
* Smartparens

## Language Modes

* Web
  * JS/JSX/HTML/CSS
  * Flow
  * Tern
  * Stylelint
  * Prettier
* Elisp
* Go
* Scala
* Haskell
* YAML
* Markdown
* CoffeeScript
* Dockerfile
* Groovy
* RNC

## Planned Modes

**NB** In order of importance to me. If you're using my setup and want something
supported not on this list, feel free to issue a PR.

* Python
* Java
* OCaml
* TypeScript
* Racket

## Misc notes

* I use [Marked 2](http://marked2app.com/) to preview markdown files. If you
  don't have it, or you want to use something, set `markdown-open-command` to
  point to something else. If you are using Marked 2,
  [here](https://jblevins.org/log/marked-2-command "Running Marked 2 from the
  Command Line") is how you set it up
* If you want to tweak the font settings look in `config/rk-faces/rk-theme-base.el`.
* I put more effort into some modes than others, hence there are some modes
  with considered chordal bindings, and some with none. If you notice this,
  and you'd like to flesh these modes out, please send me a PR!

## Credits

* Heavily ~~copied~~ borrowed & adapted from [Chris Barrett](https://github.com/chrisbarrett)'s
wonderful [setup](https://github.com/chrisbarrett/.emacs.d).
* Thanks to [JH](https://github.com/jackhopner) for the setup script
