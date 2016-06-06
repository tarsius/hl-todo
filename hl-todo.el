;;; hl-todo.el --- highlight TODO and similar keywords

;; Copyright (C) 2013-2016  Jonas Bernoulli

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

;; Highlight TODO and similar keywords in comments and strings.  To
;; use, turn on `hl-todo-mode' in individual buffers or use the the
;; global variant `global-hl-todo-mode'.

;; See [[http://emacswiki.org/FixmeMode][this list]] on the Emacswiki for other packages that implement
;; the same basic features, but which might also provide additional
;; features that you might like, but which I don't deem necessary.

;;; Code:

(defgroup hl-todo nil
  "Highlight TODO and similar keywords in comments and strings."
  :group 'font-lock-extra-types)

(defface hl-todo
  '((t (:bold t :foreground "#cc9393")))
  "Base face used to highlight TODO and similar keywords.
The faces used to highlight certain keywords are, by default,
created by inheriting this face and using the appropriate
color specified using the option `hl-todo-keyword-faces' as
foreground color."
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
  "Highlight TODO and similar keywords in comments and strings."
  :lighter ""
  :group 'hl-todo
  (if hl-todo-mode
      (progn (hl-todo-set-regexp)
             (font-lock-add-keywords nil hl-todo-keywords t))
    (font-lock-remove-keywords nil hl-todo-keywords))
  (when font-lock-mode
    (if (and (fboundp 'font-lock-flush)
             (fboundp 'font-lock-ensure))
        (save-restriction
          (widen)
          (font-lock-flush)
          (font-lock-ensure))
      (with-no-warnings
        (font-lock-fontify-buffer)))))

;;;###autoload
(define-globalized-minor-mode global-hl-todo-mode
  hl-todo-mode turn-on-hl-todo-mode-if-desired)

(defun turn-on-hl-todo-mode-if-desired ()
  (when (apply #'derived-mode-p hl-todo-activate-in-modes)
    (hl-todo-mode 1)))

;;;###autoload
(defun hl-todo-occur ()
  "Use `occur' to find all TODO or similar keywords.
This actually finds a superset of the highlighted keywords,
because it uses a regexp instead of a more sophisticated
matcher."
  (interactive)
  (occur hl-todo-regexp))

(provide 'hl-todo)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; hl-todo.el ends here
