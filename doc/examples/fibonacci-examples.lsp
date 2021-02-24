;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File:             fibonacci-examples.lsp
;;;
;;; Class Hierarchy:  None
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Lisp-code examples to accompany fibonacci.html
;;;
;;; Author:           Sean Reed
;;;
;;; Creation date:    13th July 2012
;;;
;;; $$ Last modified: 22:42:33 Fri May 17 2013 BST
;;;
;;; SVN ID: $Id: fibonacci-examples.lsp 3538 2013-05-18 08:29:15Z medward2 $
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
;;;  fibonacci.html examples
;;; ============================================================================

;;;  fibonacci-transition; one argument
;;; ----------------------------------------------------------------------------
(fibonacci-transition 50)

;;;  fibonacci-transition; three arguments
;;; ----------------------------------------------------------------------------
(fibonacci-transition 50 's 'e)

;;;  fibonacci-transitions; two integers
;;; ----------------------------------------------------------------------------
(fibonacci-transitions 76 4)

;;;  fibonacci-transitions; one int, one list
;;; ----------------------------------------------------------------------------
(fibonacci-transitions 304 '(s e q h))

;;;  remix-in; defaults
;;; ----------------------------------------------------------------------------
(remix-in '(1 2 3 4 5 6 7 8 9 10 11))

;;;  remix-in; with remix-in-fib-seed argument
;;; ----------------------------------------------------------------------------
(remix-in '(1 2 3 4 5 6 7 8 9 10 11) :remix-in-fib-seed 1)

;;;  fibonacci transition usage example 1
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :title "mini"
         :instrument-palette +slippery-chicken-standard-instrument-palette+
         :ensemble '(((vn (violin :midi-channel 1))))
         :tempo-map '((1 (q 60)))
         :set-palette '((1 ((c4 d4 f4 g4 a4)))
                        (2 ((cs4 ds4 fs4 gs4 as4))))
         :set-map (list (list 1 (fibonacci-transition 17 1 2)))
         :rthm-seq-palette '((1 ((((2 4) q - e s 32 32 -))
                                 :pitch-seq-palette ((1 2 3 4 5)))))
         :rthm-seq-map '((1 ((vn (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;;  fibonacci transition usage example 2
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :title "mini"
         :instrument-palette +slippery-chicken-standard-instrument-palette+
         :ensemble '(((vn (violin :midi-channel 1))))
         :tempo-map '((1 (q 60)))
         :set-palette '((1 ((c4 d4 f4 g4 a4))))
         :set-map '((1 (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)))
         :rthm-seq-palette '((1 ((((2 4) q - e s 32 32 -))
                                 :pitch-seq-palette ((1 2 3 4 5))))
                             (2 ((((2 4) (q) (s) - s s - (s)))
                                 :pitch-seq-palette ((1 2)))))
         :rthm-seq-map (list 
                        (list 1 
                              (list 
                               (list 'vn 
                                     (fibonacci-transition 17 1 2))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))