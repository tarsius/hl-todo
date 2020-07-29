;;; hl-todo.el --- highlight TODO and similar keywords  -*- lexical-binding: t -*-

;; Copyright (C) 2013-2020  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Homepage: https://github.com/tarsius/hl-todo
;; Keywords: convenience

;; Package-Requires: ((emacs "25"))

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

;; You can either explicitly turn on `hl-todo-mode' in certain buffers
;; or use the the global variant `global-hl-todo-mode', which enables
;; the local mode based on each buffer's major-mode and the options
;; `hl-todo-include-modes' and `hl-todo-exclude-modes'.  By default
;; `hl-todo-mode' is enabled for all buffers whose major-mode derive
;; from either `prog-mode' or `text-mode', except `org-mode'.

;; This package also provides commands for moving to the next or
;; previous keyword, to invoke `occur' with a regexp that matches all
;; known keywords, and to insert a keyword.  If you want to use these
;; commands, then you should bind them in `hl-todo-mode-map', e.g.:
;;
;;   (define-key hl-todo-mode-map (kbd "C-c p") 'hl-todo-previous)
;;   (define-key hl-todo-mode-map (kbd "C-c n") 'hl-todo-next)
;;   (define-key hl-todo-mode-map (kbd "C-c o") 'hl-todo-occur)
;;   (define-key hl-todo-mode-map (kbd "C-c i") 'hl-todo-insert)

;; See [[https://www.emacswiki.org/emacs/FixmeMode][this list]] on the Emacswiki for other packages that implement
;; the same basic features, but which might also provide additional
;; features that you might like, but which I don't deem necessary.

;;; Code:

(require' cl-lib)

(eval-when-compile
  (require 'subr-x))

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

(define-obsolete-variable-alias 'hl-todo-activate-in-modes
  'hl-todo-include-modes "hl-todo 3.1.0")

(defcustom hl-todo-include-modes '(prog-mode text-mode)
  "Major-modes in which `hl-todo-mode' is activated.

This is used by `global-hl-todo-mode', which activates the local
`hl-todo-mode' in all buffers whose major-mode derive from one
of the modes listed here, but not from one of the modes listed
in `hl-todo-exclude-modes'."
  :package-version '(hl-todo . "2.1.0")
  :group 'hl-todo
  :type '(repeat function))

(defcustom hl-todo-exclude-modes '(org-mode)
  "Major-modes in which `hl-todo-mode' is not activated.

This is used by `global-hl-todo-mode', which activates the local
`hl-todo-mode' in all buffers whose major-mode derived from one
of the modes listed in `hl-todo-include-modes', but not from one
of the modes listed here."
  :package-version '(hl-todo . "3.1.0")
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
    ("XXX+"   . "#cc9393"))
  "An alist mapping keywords to colors/faces used to display them.

Each entry has the form (KEYWORD . COLOR).  KEYWORD is used as
part of a regular expression.  If (regexp-quote KEYWORD) is not
equal to KEYWORD, then it is ignored by `hl-todo-insert-keyword'.
Instead of a color (a string), each COLOR may alternatively be a
face.

The syntax class of the characters at either end has to be `w'
\(which means word) in `hl-todo--syntax-table'.  That syntax
table derives from `text-mode-syntax-table' but uses `w' as the
class of \"?\".

This package, like most of Emacs, does not use POSIX regexp
backtracking.  See info node `(elisp)POSIX Regexp' for why that
matters.  If you have two keywords \"TODO-NOW\" and \"TODO\", then
they must be specified in that order.  Alternatively you could
use \"TODO\\(-NOW\\)?\"."
  :package-version '(hl-todo . "3.0.0")
  :group 'hl-todo
  :type '(repeat (cons (string :tag "Keyword")
                       (choice :tag "Face   "
                               (string :tag "Color")
                               (sexp :tag "Face")))))

(defcustom hl-todo-color-background nil
  "Whether to emphasize keywords using the background color.

If an entry in `hl-todo-keyword-faces' specifies a face, then the
respective keyword is displayed using exactly that face.  In that
case this option is irrelevant.

Otherwise, if an entry specifies only a color, then the `hl-todo'
face controls the appearance of the respective keyword, except
for either the foreground or the background color.  This option
controls which of the two it is."
  :package-version '(hl-todo . "3.1.0")
  :group 'hl-todo
  :type 'boolean)

(defcustom hl-todo-highlight-punctuation ""
  "String of characters to highlight after keywords.

Each of the characters appearing in this string is highlighted
using the same face as the preceding keyword when it directly
follows the keyword.

Characters whose syntax class is `w' (which means word),
including alphanumeric characters, cannot be used here."
  :package-version '(hl-todo . "2.0.0")
  :group 'hl-todo
  :type 'string)

(defvar-local hl-todo--regexp nil)
(defvar-local hl-todo--keywords nil)

(defun hl-todo--regexp ()
  (or hl-todo--regexp (hl-todo--setup-regexp)))

(defun hl-todo--setup-regexp ()
  (when-let ((bomb (assoc "???" hl-todo-keyword-faces)))
    ;; If the user customized this variable before we started to
    ;; treat the strings as regexps, then the string "???" might
    ;; still be present.  We have to remove it because it results
    ;; in the regexp search taking forever.
    (setq hl-todo-keyword-faces (delete bomb hl-todo-keyword-faces)))
  (setq hl-todo--regexp
        (concat "\\(\\<"
                "\\(" (mapconcat #'car hl-todo-keyword-faces "\\|") "\\)"
                "\\>"
                (and (not (equal hl-todo-highlight-punctuation ""))
                     (concat "[" hl-todo-highlight-punctuation "]*"))
                "\\)")))

(defun hl-todo--setup ()
  (hl-todo--setup-regexp)
  (setq hl-todo--keywords
        `(((lambda (bound) (hl-todo--search nil bound))
           (1 (hl-todo--get-face) t t))))
  (font-lock-add-keywords nil hl-todo--keywords t))

(defvar hl-todo--syntax-table (copy-syntax-table text-mode-syntax-table))

(defun hl-todo--search (&optional regexp bound backward)
  (unless regexp
    (setq regexp hl-todo--regexp))
  (cl-block nil
    (while (let ((case-fold-search nil))
             (with-syntax-table hl-todo--syntax-table
               (funcall (if backward #'re-search-backward #'re-search-forward)
                        regexp bound t)))
      (cond ((or (apply #'derived-mode-p hl-todo-text-modes)
                 (hl-todo--inside-comment-or-string-p))
             (cl-return t))
            ((and bound (funcall (if backward #'<= #'>=) (point) bound))
             (cl-return nil))))))

(defun hl-todo--inside-comment-or-string-p ()
  (nth 8 (syntax-ppss)))

(defun hl-todo--get-face ()
  (let ((keyword (match-string 2)))
    (hl-todo--combine-face
      (cdr (cl-find-if (lambda (elt)
                         (string-match-p (format "\\`%s\\'" (car elt))
                                         keyword))
                       hl-todo-keyword-faces)))))

(defun hl-todo--combine-face (face)
  (if (stringp face)
      (list :inherit 'hl-todo
            (if hl-todo-color-background :background :foreground)
            face)
    face))

(defvar hl-todo-mode-map (make-sparse-keymap)
  "Keymap for `hl-todo-mode'.")

;;;###autoload
(define-minor-mode hl-todo-mode
  "Highlight TODO and similar keywords in comments and strings."
  :lighter ""
  :keymap hl-todo-mode-map
  :group 'hl-todo
  (unless (and font-lock-mode font-lock-defaults)
    (user-error "Cannot enable hl-todo-mode without font-lock"))
  (if hl-todo-mode
      (hl-todo--setup)
    (font-lock-remove-keywords nil hl-todo--keywords))
  (save-excursion
    (goto-char (point-min))
    (while (hl-todo--search)
      (save-excursion
        (font-lock-fontify-region (match-beginning 0) (match-end 0) nil)))))

;;;###autoload
(define-globalized-minor-mode global-hl-todo-mode
  hl-todo-mode hl-todo--turn-on-mode-if-desired)

(defun hl-todo--turn-on-mode-if-desired ()
  (when (and (apply #'derived-mode-p hl-todo-include-modes)
             (not (apply #'derived-mode-p hl-todo-exclude-modes)))
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
                          (looking-at (hl-todo--regexp)))
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
                  (hl-todo--search (concat (hl-todo--regexp) "\\=") nil t)
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
matcher.  It also finds occurrences that are not within a
string or comment."
  (interactive)
  (with-syntax-table hl-todo--syntax-table
    (occur (hl-todo--regexp))))

;;;###autoload
(defun hl-todo-insert (keyword)
  "Insert TODO or similar keyword.
If point is not inside a string or comment, then insert a new
comment.  If point is at the end of the line, then insert the
comment there, otherwise insert it as a new line before the
current line."
  (interactive
   (list (completing-read
          "Insert keyword: "
          (cl-mapcan (pcase-lambda (`(,keyword . ,face))
                       (and (equal (regexp-quote keyword) keyword)
                            (list (propertize keyword 'face
                                              (hl-todo--combine-face face)))))
                     hl-todo-keyword-faces))))
  (cond
   ((hl-todo--inside-comment-or-string-p)
    (insert (concat (and (not (memq (char-before) '(?\s ?\t))) " ")
                    keyword
                    (and (not (memq (char-after) '(?\s ?\t ?\n))) " "))))
   ((and (eolp)
         (not (looking-back "^[\s\t]*" (line-beginning-position) t)))
    (insert (concat (and (not (memq (char-before) '(?\s ?\t))) " ")
                    (format "%s %s " comment-start keyword))))
   (t
    (goto-char (line-beginning-position))
    (insert (format "%s %s "
                    (if (derived-mode-p 'lisp-mode 'emacs-lisp-mode)
                        (format "%s%s" comment-start comment-start)
                      comment-start)
                    keyword))
    (unless (looking-at "[\s\t]*$")
      (save-excursion (insert "\n")))
    (indent-region (line-beginning-position) (line-end-position)))))

(define-obsolete-function-alias 'hl-todo-insert-keyword
  'hl-todo-insert "hl-todo 3.0.0")

;;; _
(provide 'hl-todo)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; hl-todo.el ends here
