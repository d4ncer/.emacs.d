# rnc-mode

rnc-mode is an Emacs major mode for editing XML schemas in
[RELAX-NG](http://relaxng.org/)
[Compact](http://relaxng.org/compact-20021121.html) syntax.

The mode was originally written by David Rosenborg at Pantor Engineering, but
recently canonical URL for the mode stopped working, and the code didn't run
in Emacs 23 and later without some minor modifications.

This repo contains an updated version of `rnc-mode` that works in GNU Emacs 23
and later: I have not tried it on XEmacs. It also has some new features, such
as on-the-fly syntax checking.

## Installation

Put `rnc-mode.el` in your Emacs path and add the following to your Emacs
initialization file:

    (autoload 'rnc-mode "rnc-mode")
    (add-to-list 'auto-mode-alist '("\\.rnc\\'" . rnc-mode))

If you are using Emacs 24 or Emacs 23 with `packages.el` you can install it
with the package manager from the [MELPA](http://melpa.milkbox.net/) package
archive. Put the following in your initialization file:

    (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
    (package-initialize)

## On-the-fly Syntax Checking with Flymake and Jing

You can perform on-the-fly syntax checking of your schema using
[Jing](http://www.thaiopensource.com/relaxng/jing.html) and Flymake. Two
variables control this setting:

- Variable: **rnc-jing-jar-file**

    The value of this variable is a string, the pathname of the JING jar file.
  
- Variable: **rnc-enable-flymake**

    When this variable is `t` flymake is enabled when an RNC file is
    loaded. If it is `nil` then you can control when syntax checking is
    enabled with `M-x flymake-mode`.

For example:

    (setq rnc-enable-flymake t
          rnc-jing-jar-file (expand-file-name "~/src/jing-20091111/bin/jing.jar")

Currently this only works with single-file schema.

## Imenu Support

You can use Imenu to get an alphabetically sorted list of patterns,
namespaces, datatypes, and includes in the file.

- Variable: **rnc-enable-imenu**

    When this variable is `t` an **RNC** menu added to the menu bar containing
    the patterns, namespaces, datatypes, and includes in the current file.

For example,

    (setq rnc-enable-imenu t)

## License

I am __not__ the original author.

Copyright &copy; 2002 Pantor Engineering AB.

Additions (Flymake support) are Copyright &copy; 2012 Thomas R Emerson.

Distributed under the [BSD Revised License](http://opensource.org/licenses/BSD-3-Clause).
