;;; sc-slippery-chicken.el --- a minor mode for composers  -*- lexical-binding: t; -*-

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

;; This file provides functionality specific to Michael Edwards's
;; slippery-chicken. 

;;; Code:
(require 'slime)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* sc-mode/sc-validate-rthm-seq
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-04-20
;;; 
;;; DESCRIPTION
;;; Reads the last expression from a buffer (asserting this is the
;;; rs-list to be validated) and tests if this list can be used to successfully
;;; instantiate a rthm-seq object.
;;;
;;; TODO
;;; - Also include the second return value (the actual condition) into the
;;;   return message. This seems currently not possible as Elisp doesn't
;;;   natively support multiple return values.
;;;   RP  Thu Apr 20 22:26:43 2023
;;;
;;; ARGUMENTS
;;; none
;;; 
;;; OPTIONAL ARGUMENTS
;;; The list to be tested. Must be provided as a string. 
;;; 
;;; RETURN VALUE
;;; Either T, when the test succeeds, or two values:
;;; - NIL.
;;; - The error condition.
;;;
;;; EXAMPLE
[
(sc-validate-rthm-seq "((((3 4) q q q))
                             :pitch-seq-palette (1 2 3))")

;; => message: Valid.
]
;;; 
;;; SYNOPSIS
(defun sc-validate-rthm-seq (&optional rs-list)
  ;;; ****
  "Reads the last expression from a buffer and calls make-rthm-seq"
  (interactive)
  (let* ((rs-list (if rs-list
                      rs-list
                    (slime-last-expression)))
         ;; the Common Lisp expression to
         ;; be evaluated
         (cl-expr (concat "(sc::validate-rthm-seq '"
                          rs-list
                          ")"))
         (result (slime-eval `(cl:eval
                               (cl:read-from-string ,cl-expr))
                             "slippery-chicken")))
    (if result
        (progn
          (message "Valid.")
          t)
      (progn
        (message "INVALID!")
        nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* sc-mode/sc-get-num-notes
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-04-21
;;; 
;;; DESCRIPTION
;;; Returns the number of notes in a rthm-seq list. This can be useful
;;; when construction pitch-seq-palettes.
;;; This is done by:
;;; - Read the last expression from a buffer and test if it is a valid
;;;   rthm-seq list.
;;; - Get and print the num-notes slot of the rthm-seq.
;;;
;;; ARGUMENTS
;;; none
;;; 
;;; OPTIONAL ARGUMENTS
;;; The list to be tested. Must be provided as a string.
;;; 
;;; RETURN VALUE
;;; The number of notes in the rthm-seq-list.
;;;
;;; SYNOPSIS
(defun sc-get-num-notes (&optional rs-list)
  ;;; ****
  "Reads the last expression from a buffer and tries to get the num-notes slot"
  (interactive)
  (let ((rs-list (if rs-list
                     rs-list
                   (slime-last-expression))))
    ;;; test if the list is a valid rthm-seq
    (if (sc-validate-rthm-seq rs-list)
        (let* ((cl-expr (concat "(sc::num-notes
                                  (sc::make-rthm-seq '"
                                rs-list
                                "))"))
               (result (slime-eval `(cl:eval
                                     (cl:read-from-string ,cl-expr))
                                   "slippery-chicken")))
          (if result
              (progn
                (print result)
                result)
            (message "INVALID rthm-seq"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'sc-slippery-chicken)
;;; sc-slippery-chicken.el ends here
