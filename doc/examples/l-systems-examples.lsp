;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File:             l-systems.lsp
;;;
;;; Class Hierarchy:  None
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Lisp example code to accompany l-systems.html
;;;
;;; Author:           Sean Reed
;;;
;;; Creation date:    22nd August 2012
;;;
;;; $$ Last modified: 22:43:08 Fri May 17 2013 BST
;;;
;;; SVN ID: $Id: l-systems-examples.lsp 3538 2013-05-18 08:29:15Z medward2 $
;;;
;;; ****
;;; Licence:          Copyright (c) 2012 Michael Edwards
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

;;; ============================================================================
;;; l-systems.html examples
;;; ============================================================================

;;; ----------------------------------------------------------------------------
;;;  make-l-for-lookup
;;; ----------------------------------------------------------------------------
(make-l-for-lookup 'l-sys 
                   '((1 ((a))) 
                     (2 ((b)))) 
                   '((1 (1 2)) 
                     (2 (1))))

;;; ----------------------------------------------------------------------------
;;;  do-simple-lookup
;;; ----------------------------------------------------------------------------

(let* ((lfl (make-l-for-lookup 'l-sys
                               '((1 ((a)))
                                 (2 ((b))))
                               '((1 (1 2)) (2 (1))))))
   (do-simple-lookup lfl 1 29))

;;; ----------------------------------------------------------------------------
;;;  do-simple-lookup with flatten
;;; ----------------------------------------------------------------------------

(let* ((lfl (make-l-for-lookup 'l-sys
                               '((1 ((a)))
                                 (2 ((b))))
                               '((1 (1 2)) (2 (1))))))
  (flatten (do-simple-lookup lfl 1 29)))

;;; ----------------------------------------------------------------------------
;;;  get-l-sequence 
;;; ----------------------------------------------------------------------------

(let* ((lfl (make-l-for-lookup 'l-sys
                               '((1 ((a)))
                                 (2 ((b))))
                               '((1 (1 2)) (2 (1))))))
  (get-l-sequence lfl 1 29))

;;; ----------------------------------------------------------------------------
;;;  get-l-sequence with NIL for elements list
;;; ----------------------------------------------------------------------------

(let* ((lfl (make-l-for-lookup 'l-sys
                               nil
                               '((1 (1 2)) (2 (1))))))
  (get-l-sequence lfl 1 29))

;;; ----------------------------------------------------------------------------
;;;  l-systems with fibonacci transitions: do-lookup
;;; ----------------------------------------------------------------------------

(let* ((lfl (make-l-for-lookup 'l-sys-a
                               '((1 ((a) (c)))
                                 (2 ((b))))
                               '((1 (1 2)) (2 (1))))))
   (do-lookup lfl 1 73))

;;; ----------------------------------------------------------------------------
;;;  non-l-system lookup of l-for-lookup object: get-linear-sequence
;;; ----------------------------------------------------------------------------

(let* ((lfl (make-l-for-lookup 'lfl-test
                               nil
                               '((1 (2 3))
                                 (2 (3 1 2))
                                 (3 (1))))))
   (get-linear-sequence lfl 1 23))

;;; ----------------------------------------------------------------------------
;;; EOF
