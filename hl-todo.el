;;; hl-todo.el --- highlight TODO keywords

;; Copyright (C) 2013-2015  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Homepage: http://github.com/tarsius/hl-todo
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Highlight TODO keywords in comments and strings.  There are
;; many modes like it but this one is mine.  It also happens to
;; be simpler than the following alternatives:

;; - [[http://emacswiki.org/fic-ext-mode.el][fic-ext-mode]]
;; - [[https://github.com/lewang/fic-mode][fic-mode]]
;; - [[http://emacswiki.org/FixmeMode][fixme-mode]]
;; - [[https://github.com/rolandwalker/fixmee][fixmee]]

;; See [[http://emacswiki.org/FixmeMode][this list]] on the Emacswiki for even more alternatives.

;; If you like this you might also like [[https://github.com/tarsius/orglink][orglink]], if you used
;; this, then the above links would not look so ugly.

;;; Code:

(defgroup hl-todo nil
  "Highlight TODO keywords in comments and strings."
  :group 'font-lock-extra-types)

(defface hl-todo
  '((t (:bold t :foreground "#cc9393")))
  "Face used to highlight TODO keywords."
  :group 'hl-todo)

(defcustom hl-todo-activate-in-modes '(emacs-lisp-mode)
  "Major modes in which `hl-todo-mode' should be activated.
This is used by `global-hl-todo-mode'."
  :group 'hl-todo
  :type '(repeat function))

(defvar hl-todo-regexp nil)

(defvar hl-todo-keyword-faces)

(defun hl-todo-set-regexp ()
  "Set the value of `hl-todo-regexp' based on `hl-todo-keyword-faces'."
  (setq hl-todo-regexp
        (concat "\\_<" (regexp-opt (mapcar #'car hl-todo-keyword-faces) t)
                ":?\\_>")))

(defcustom hl-todo-keyword-faces
  '(("HOLD" . "#d0bf8f")
    ("TODO" . "#cc9393")
    ("NEXT" . "#dca3a3")
    ("THEM" . "#dc8cc3")
    ("PROG" . "#7cb8bb")
    ("OKAY" . "#7cb8bb")
    ("DONT" . "#5f7f5f")
    ("FAIL" . "#8c5353")
    ("DONE" . "#afd8af")
    ("NOTE"   . "#d0bf8f")
    ("KLUDGE" . "#d0bf8f")
    ("HACK"   . "#d0bf8f")
    ("FIXME"  . "#cc9393")
    ("XXX"    . "#cc9393")
    ("XXXX"   . "#cc9393")
    ("???"    . "#cc9393"))
  "Faces used to highlight specific TODO keywords."
  :group 'hl-todo
  :type '(repeat (cons (string :tag "Keyword")
                       (choice :tag "Face   "
                               (string :tag "Color")
                               (sexp :tag "Face"))))
  :set (lambda (symbol value)
         (set-default symbol value)
         (hl-todo-set-regexp)))

(defvar hl-todo-keywords
  `(((lambda (_)
       (let (case-fold-search)
         (and (re-search-forward hl-todo-regexp nil t)
              (nth 8 (syntax-ppss))))) ; inside comment or string
     (1 (hl-todo-get-face) t t))))

(defun hl-todo-get-face ()
  (let ((face (cdr (assoc (match-string 1) hl-todo-keyword-faces))))
    (if (stringp face)
        (list :inherit 'hl-todo :foreground face)
      face)))

;;;###autoload
(define-minor-mode hl-todo-mode
  "Highlight TODO tags in comments and strings."
  :lighter ""
  :group 'hl-todo
  (if hl-todo-mode
      (font-lock-add-keywords  nil hl-todo-keywords t)
    (font-lock-remove-keywords nil hl-todo-keywords))
  (when (called-interactively-p 'any)
    (if (fboundp 'font-lock-flush)
        (font-lock-flush)
      (with-no-warnings
        (font-lock-fontify-buffer)))))

;;;###autoload
(define-globalized-minor-mode global-hl-todo-mode
  hl-todo-mode turn-on-hl-todo-mode-if-desired)

(defun turn-on-hl-todo-mode-if-desired ()
  (when (apply #'derived-mode-p hl-todo-activate-in-modes)
    (hl-todo-mode 1)))

(provide 'hl-todo)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; hl-todo.el ends here
