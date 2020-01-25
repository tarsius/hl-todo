Highlight TODO and similar keywords (codetags) in comments and strings
-----------------------------------------------------------

To highlight keywords turn on `hl-todo-mode` in individual buffers
or use the the global variant `global-hl-todo-mode`.

This package also provides commands for moving to the next or
previous keyword, to invoke `occur` with a regexp that matches all
known keywords, and to insert a keyword.  If you want to use these
commands, then you should bind them in `hl-todo-mode-map`, e.g.:

```emacs-lisp
(define-key hl-todo-mode-map (kbd "C-c p") 'hl-todo-previous)
(define-key hl-todo-mode-map (kbd "C-c n") 'hl-todo-next)
(define-key hl-todo-mode-map (kbd "C-c o") 'hl-todo-occur)
(define-key hl-todo-mode-map (kbd "C-c i") 'hl-todo-insert)
```

See [this list](https://www.emacswiki.org/emacs/FixmeMode) on the Emacswiki for
other packages that implement the same basic features, but which might
also provide additional features that you might like, but which I
don't deem necessary.

Customizing Tags
================

You can customize the code tags that are recognized by hl-todo by
customizing `hl-todo-keyword-faces` either through the customization
menu or by setting the variable directly in your `.emacs` file for
example:

```emacs-lisp
(setq hl-todo-keyword-faces
      '(("TODO" . "#Ff0000")
        ("FIXME" . "#Ff0000")
        ("DEBUG" . "#A020f0")
        ("GOTCHA" . "#Ff4500")
        ("STUB" . "#1e90ff")))
```