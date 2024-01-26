;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File:             videos-example-20-a.lsp
;;;
;;; Class Hierarchy:  None
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Lisp example code to accompany video tutorial 20
;;;
;;; Author:           Michael Edwards
;;;
;;; Creation date:    22nd December 2012
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

(let* ((num-bars 37)
       (lfl (make-l-for-lookup 'l-sys
                               '((1 ((a)))
                                 (2 ((b))))
                               '((1 (1 2)) (2 (1)))))
       (l-sys-list (flatten (do-simple-lookup lfl 1 num-bars)))
       (mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :tempo-map '((1 (q 84)))
         :set-palette '((a ((e4 a4 b4 e5 a5 b5 e5)))
                        (b ((e4 fs4 gs4 b4 cs5 e5 b5))))
         :set-limits-high '((vn (0 d5 50 c7 100 g4)))
         :set-limits-low '((vn (0 g3 50 e5 100 c4)))
         :set-map (list (list 1 l-sys-list))
         :rthm-seq-palette '((a ((((4 4) - 32 32 - (e.) (s) - s s s - 
                                   - { 3 ts ts ts } (e) - - (s) 32 32 - (e)))  
                                 :pitch-seq-palette ((1 2 1 4 3 2 3 5 2 3)))) 
                             (b ((((5 8) { 3 - ts ts ts - } s (s) (s) s (s) s 
                                   - s s -))
                                 :pitch-seq-palette ((7 5 2 3 2 2 5 7)))))
         :rthm-seq-map (list 
                        (list 1 
                              (list 
                               (list 'vn l-sys-list)))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF