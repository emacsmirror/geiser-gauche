# geiser-gauche

Experimental [Gauche Scheme](http://practical-scheme.net/gauche/) support for [Geiser](https://gitlab.com/jaor/geiser/). 

geiser-gauche is in a verly early stage of its development and mostly untested, so bugs and rough edges are to be expected.

**Table of contents**
- [Description](#description)
- [Requirements](#requirements)
- [Installation and setup](#installation-and-setup)
- [Credits](#credits)
- [License](#license)

## Description

This package adds support for the Gauche Scheme implementation to Geiser, a generic Scheme interaction mode for the GNU Emacs editor. 

Currently the following Geiser features are supported:
+ evaluation of sexps, definitions, regions and whole buffers,
+ loading Scheme files,
+ macroexpansion,
+ symbol and module-name completion,
+ short symbol and module documentation,
+ detailed symbol and module documentation lookup in the Gauche Info manual.

## Requirements

+ Gauche 0.9.6 or later.
+ Geiser 0.11.2 or later.

## Installation and setup

1. Download the [latest release](https://gitlab.com/emacs-geiser/gauche/uploads/8c91ece5799c079b02a11c9c3ea7d55c/geiser-gauche-0.0.2.tar) and install it using Emacs's built-in package manager by invoking the command
```
    M-x package-install-file
```
2. Add Gauche to Geiser's list of active Scheme implementations in your Emacs init file. This can be done, e.g., by adding
```emacs-lisp
(with-eval-after-load 'geiser-impl
  (add-to-list 'geiser-active-implementations 'gauche))
```
or, using `use-package`
```emacs-lisp
(use-package geiser-gauche
  :after geiser
  :init (add-to-list 'geiser-active-implementations 'gauche))
```
Naturally, if the `geiser-active-implementations` variable is already customized in the init file then one can simply add `'gauche` to the set value, e.g. replace 
```emacs-lisp
(setq geiser-active-implementations '(guile chez))
```
with 
```emacs-lisp
(setq geiser-active-implementations '(guile chez gauche))
```
3. If Gauche's `gosh` binary is not in the path for Emacs then the variable
   `geiser-gauche-binary` also has to be set in the init file, e.g., with
```emacs-lisp
(setq geiser-gauche-binary /path/to/gosh)
```

## Credits
Thanks to [Jao](https://gitlab.com/jaor) and his co-developers for creating and maintaining Geiser, and for helping the development of this package. Thanks also to Shiro Kawai and his co-developers for Gauche, the _raison d'être_ of this package.
## License
Copyright (C) 2020 András Simonyi

Authors: András Simonyi

This program is free software; you can redistribute and/or modify it under the terms of the BSD 3-Clause "New" or "Revised" License. You should have received a copy of the license along with this program. If not, see https://spdx.org/licenses/BSD-3-Clause.html.
