;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* sc/export-symbols.lsp
;;; NAME
;;; export-symbols.lsp
;;;
;;; File:             slippery-chicken.asd
;;;
;;; Version:          1.1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Export all symbols defined in slippery chicken. 
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    March 15th 2024
;;;
;;; $$ Last modified:  16:20:46 Fri Mar 15 2024 CET
;;;
;;; ****
;;; Licence:          Copyright (c) 2010 Michael Edwards
;;;
;;;                   This file is part of slippery-chicken
;;;
;;;                   slippery-chicken is free software; you can redistribute it
;;;                   and/or modify it under the terms of the GNU General
;;;                   Public License as published by the Free Software
;;;                   Foundation; either version 3 of the License, or (at your
;;;                   option) any later version.
;;;
;;;                   slippery-chicken is distributed in the hope that it will
;;;                   be useful, but WITHOUT ANY WARRANTY; without even the
;;;                   implied warranty of MERCHANTABILITY or FITNESS FOR A
;;;                   PARTICULAR PURPOSE.  See the GNU General Public License
;;;                   for more details.
;;;
;;;                   You should have received a copy of the GNU General Public
;;;                   License along with slippery-chicken; if not, write to the
;;;                   Free Software Foundation, Inc., 59 Temple Place, Suite
;;;                   330, Boston, MA 02111-1307 USA
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :slippery-chicken)

;;; MDE Tue Mar 24 11:08:08 2020
;;; This code was originally placed in all.lsp, but needed to be moved as
;;; it is recommended not to place lisp code in the .asd file.
;;; RP  Fri Mar 15 16:20:46 2024
(let ((package (find-package :sc)))
  (do-all-symbols (symb package)
    (when (and (or (find-class symb nil)
                   (fboundp symb))
               ;; globals such as +slippery-chicken-config-data+ won't be
               ;; exported but that's just as it should be 
               (eql (symbol-package symb) package))
      (export symb package))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF export-symbols.lsp
