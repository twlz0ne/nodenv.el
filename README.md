[![MELPA](https://melpa.org/packages/nodenv-badge.svg)](https://melpa.org/#/nodenv)

## nodenv.el

Emacs integration for nodenv.

## Installation

Copy file `nodenv.el` to directory `~/.emacs.d/site-lisp/nodenv.el/`, for example, and add this to your .emacs to load the mode

```elisp
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/nodenv.el"))
(add-to-list 'exec-path (expand-file-name "~/.nodenv/shims"))
(require 'nodenv)
(add-hook 'js-mode-hook #'nodenv-mode)
```
