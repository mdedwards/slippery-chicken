;;; sc-mode.el --- a minor mode for composers        -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ruben Philipp

;; Author: Ruben Philipp <me@rubenphilipp.com>
;; Keywords: lisp, extensions, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides the sc-mode minor mode to Emacs.  It is aimed
;; at composers working with the Common Lisp algorithmic composition
;; software slippery-chicken.
;; This file stores mode-specific bindings to `sc-mode`, and minor-mode
;; definition.

;;; Code:

(require 'slime)
(require 'cl-lib)
(require 'sc-slippery-chicken)

;;;###autoload
(define-minor-mode sc-mode
  "Toggle sc-mode."
  :lighter "slippery-chicken"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c v r") 'sc-validate-rthm-seq)
            (define-key map (kbd "C-c g n") 'sc-get-num-notes)
            map)
  ;;; load helper functions (common lisp)
  ;;; requires slippery chicken to be present in the current lisp environment
  (let* ((cl-expr (concat "(load "
                          "(sc::file-from-sc-dir "
                         "\"src/emacs/utilities.lsp\""
                         "))"))
         (result (slime-eval `(cl:eval
                               (cl:read-from-string ,cl-expr))
                             "slippery-chicken")))
    (if result
        (progn
          (message "Welcome to sc-mode!")
          t)
      (progn
        (error "ERROR: Loading the Common Lisp utilities failed.")
        nil)))
  )


(provide 'sc-mode)
;;; sc-mode.el ends here
