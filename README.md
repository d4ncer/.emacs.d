# My Emacs config

## Installation

### Go setup

Packages to install for Go config:

```bash
$ go get -u -v github.com/nsf/gocode
$ go get -u -v github.com/rogpeppe/godef
$ go get -u -v github.com/kardianos/govendor
$ go get -u -v golang.org/x/tools/cmd/guru
$ go get -u -v golang.org/x/tools/cmd/gorename
$ go get -u -v golang.org/x/tools/cmd/goimports
```

### JS setup

Tern setup:

```bash
$ cd lisp/tern
$ npm install
```

Required binaries:

```bash
$ npm intall -g stylefmt
```

Binaries for global fallback (this is optional, but highly recommended):

```bash
$ npm install -g eslint flow prettier eslint-config-airbnb eslint-config-prettier
```

If you do use a global `eslint`, you should add a fallback `.eslintrc.json` in your root folder. A sample:

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

Most of my work is on the frontend, which is reflected in my setup. Some packages of note:

- Evil
- Ivy (with ripgrep)
- Spacemacs leader keys
- Projectile
- Flycheck
- Magit
- Org
- IBuffer
- Neotree
- Smartparens

## Language Modes

- Web (JS, JSX, HTML, CSS). Includes support for Flow, Stylelint, and Prettier.
- Go
- YAML
- RNC
- Markdown
- Elisp
- CoffeeScript

## Planned Modes

**NB** In order of importance to me. If you're using my setup and want something supported not on this list, feel free to issue a PR.

- TypeScript
- Scala
- Java
- Racket
- Haskell

## Credits

Heavily ~~copied~~ borrowed & adapted from Chris Barrett's wonderful [setup](https://github.com/chrisbarrett/.emacs.d).
