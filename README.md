## Highlight TODO and similar keywords in comments and strings

To highlight keywords turn on `hl-todo-mode` in individual buffers
or use the the global variant `global-hl-todo-mode`.

This package also provides commands for moving to the next or
previous keyword and to invoke `occur` with a regexp that matches
all known keywords.  If you want to use these commands, then you
should bind them in `hl-todo-mode-map`, e.g.:

```lisp
(define-key hl-todo-mode-map (kbd "C-c p") 'hl-todo-previous)
(define-key hl-todo-mode-map (kbd "C-c n") 'hl-todo-next)
(define-key hl-todo-mode-map (kbd "C-c o") 'hl-todo-occur)
```

See [this list](http://emacswiki.org/FixmeMode) on the Emacswiki for
other packages that implement the same basic features, but which might
also provide additional features that you might like, but which I
don't deem necessary.
