;;; hl-todo.el --- highlight TODO and similar keywords  -*- lexical-binding: t -*-

;; Copyright (C) 2013-2018  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Homepage: https://github.com/tarsius/hl-todo
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

;; Highlight TODO and similar keywords in comments and strings.

;; You can either turn on `hl-todo-mode' in individual buffers or use
;; the the global variant `global-hl-todo-mode'.  Note that the option
;; `hl-todo-activate-in-modes' controls in what buffers the local mode
;; will be activated if you do the latter.  By default it will only be
;; activated in buffers whose major-mode derives from `prog-mode'.

;; This package also provides commands for moving to the next or
;; previous keyword and to invoke `occur' with a regexp that matches
;; all known keywords.  If you want to use these commands, then you
;; should bind them in `hl-todo-mode-map', e.g.:
;;
;;   (define-key hl-todo-mode-map (kbd "C-c p") 'hl-todo-previous)
;;   (define-key hl-todo-mode-map (kbd "C-c n") 'hl-todo-next)
;;   (define-key hl-todo-mode-map (kbd "C-c o") 'hl-todo-occur)

;; See [[https://www.emacswiki.org/emacs/FixmeMode][this list]] on the Emacswiki for other packages that implement
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

(defcustom hl-todo-activate-in-modes '(prog-mode text-mode)
  "Major-modes in which `hl-todo-mode' should be activated.

This is used by `global-hl-todo-mode', which activates
`hl-todo-mode' in all buffers whose major-mode derived from one
of the modes listed here.

Even though `org-mode' indirectly derives from `text-mode' this
mode is never activated in `org-mode' buffers because that mode
provides its own TODO keyword handling."
  :package-version '(hl-todo . "2.1.0")
  :group 'hl-todo
  :type '(repeat function))

(defcustom hl-todo-text-modes '(text-mode)
  "Major-modes that are considered text-modes.

In buffers whose major-mode derives from one of the modes listed
here TODO keywords are always highlighted even if they are not
located inside a string."
  :package-version '(hl-todo . "2.1.0")
  :group 'hl-todo
  :type '(repeat function))

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
    ("TEMP"   . "#d0bf8f")
    ("FIXME"  . "#cc9393")
    ("XXX"    . "#cc9393")
    ("XXXX"   . "#cc9393")
    ("???"    . "#cc9393"))
  "Faces used to highlight specific TODO keywords."
  :package-version '(hl-todo . "2.0.0")
  :group 'hl-todo
  :type '(repeat (cons (string :tag "Keyword")
                       (choice :tag "Face   "
                               (string :tag "Color")
                               (sexp :tag "Face")))))

(defcustom hl-todo-highlight-punctuation ""
  "String of characters to highlight after keywords.

Each of the characters appearing in this string is highlighted
using the same face as the preceeding keyword when it directly
follows the keyword.

Characters whose syntax class is `w' (which means word),
including alphanumeric characters, cannot be used here."
  :package-version '(hl-todo . "2.0.0")
  :group 'hl-todo
  :type 'string)

(defvar-local hl-todo--regexp nil)
(defvar-local hl-todo--keywords nil)

(defun hl-todo--setup ()
  (setq hl-todo--regexp
        (concat "\\(\\<"
                (regexp-opt (mapcar #'car hl-todo-keyword-faces) t)
                "\\>"
                (and (not (equal hl-todo-highlight-punctuation ""))
                     (concat "[" hl-todo-highlight-punctuation "]*"))
                "\\)"))
  (setq hl-todo--keywords
        `(((lambda (bound) (hl-todo--search nil bound))
           (1 (hl-todo--get-face) t t))))
  (font-lock-add-keywords nil hl-todo--keywords t))

(defvar hl-todo--syntax-table (copy-syntax-table text-mode-syntax-table))

(defun hl-todo--search (&optional regexp bound backward)
  (unless regexp
    (setq regexp hl-todo--regexp))
  (and (let ((case-fold-search nil))
         (with-syntax-table hl-todo--syntax-table
           (funcall (if backward #'re-search-backward #'re-search-forward)
                    regexp bound t)))
       (or (apply #'derived-mode-p hl-todo-text-modes)
           (nth 8 (syntax-ppss)) ; inside comment or string
           (and (or (not bound)
                    (funcall (if backward #'< #'>) bound (point)))
                (hl-todo--search regexp bound backward)))))

(defun hl-todo--get-face ()
  (let ((face (cdr (assoc (match-string 2) hl-todo-keyword-faces))))
    (if (stringp face)
        (list :inherit 'hl-todo :foreground face)
      face)))

(defvar hl-todo-mode-map (make-sparse-keymap)
  "Keymap for `hl-todo-mode'.")

;;;###autoload
(define-minor-mode hl-todo-mode
  "Highlight TODO and similar keywords in comments and strings."
  :lighter ""
  :keymap hl-todo-mode-map
  :group 'hl-todo
  (if hl-todo-mode
      (hl-todo--setup)
    (font-lock-remove-keywords nil hl-todo--keywords))
  (when font-lock-mode
    (save-excursion
      (goto-char (point-min))
      (while (hl-todo--search)
        (save-excursion
	  (font-lock-fontify-region (match-beginning 0) (match-end 0) nil))))))

;;;###autoload
(define-globalized-minor-mode global-hl-todo-mode
  hl-todo-mode hl-todo--turn-on-mode-if-desired)

(defun hl-todo--turn-on-mode-if-desired ()
  (when (and (apply #'derived-mode-p hl-todo-activate-in-modes)
             (not (derived-mode-p 'org-mode)))
    (hl-todo-mode 1)))

;;;###autoload
(defun hl-todo-next (arg)
  "Jump to the next TODO or similar keyword.
The prefix argument ARG specifies how many keywords to move.
A negative argument means move backward that many keywords."
  (interactive "p")
  (if (< arg 0)
      (hl-todo-previous (- arg))
    (while (and (> arg 0)
                (not (eobp))
                (progn
                  (when (let ((case-fold-search nil))
                          (looking-at hl-todo--regexp))
                    (goto-char (match-end 0)))
                  (or (hl-todo--search)
                      (user-error "No more matches"))))
      (cl-decf arg))))

;;;###autoload
(defun hl-todo-previous (arg)
  "Jump to the previous TODO or similar keyword.
The prefix argument ARG specifies how many keywords to move.
A negative argument means move forward that many keywords."
  (interactive "p")
  (if (< arg 0)
      (hl-todo-next (- arg))
    (while (and (> arg 0)
                (not (bobp))
                (let ((start (point)))
                  (hl-todo--search (concat hl-todo--regexp "\\=") nil t)
                  (or (hl-todo--search nil nil t)
                      (progn (goto-char start)
                             (user-error "No more matches")))))
      (goto-char (match-end 0))
      (cl-decf arg))))

;;;###autoload
(defun hl-todo-occur ()
  "Use `occur' to find all TODO or similar keywords.
This actually finds a superset of the highlighted keywords,
because it uses a regexp instead of a more sophisticated
matcher."
  (interactive)
  (occur hl-todo--regexp))

;;; _
(provide 'hl-todo)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; hl-todo.el ends here
