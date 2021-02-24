;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File:             videos-example-23-all.lsp
;;;
;;; Class Hierarchy:  None
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Lisp example code to accompany video tutorial 23
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; permutations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let* ((perms (permutations 4))
       (mini
        (make-slippery-chicken
         '+mini+
         :tempo-map '((1 (h 60)))
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '((0 ((d4 a4 d5 e5 a5 d6)))
                        (1 ((d4 e4 a4 d5 e5 a5 d6)))
                        (2 ((e4 a4 e5 a5 e6)))
                        (3 ((e4 a4 b4 e5 a5 b5 e5))))
         :set-map `((1 ,(flatten (loop for p in perms collect p))))
         :rthm-seq-palette '((1 ((((4 4) - 32 32 - (e.) (s) - s s s - 
                                   - { 3 ts ts ts } (e) - - (s) 32 32 - (e)))  
                                 :pitch-seq-palette ((1 2 1 4 3 2 3 5 2 3)))))
         :rthm-seq-map `((1 ((vn ,(loop repeat (* 4 (length perms)) 
                                     collect 1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; inefficient-permutations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let* ((perms (inefficient-permutations 4 :max 7 :skip 2 :fix t))
       (mini
        (make-slippery-chicken
         '+mini+
         :tempo-map '((1 (h 60)))
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '((0 ((d4 a4 d5 e5 a5 d6)))
                        (1 ((d4 e4 a4 d5 e5 a5 d6)))
                        (2 ((e4 a4 e5 a5 e6)))
                        (3 ((e4 a4 b4 e5 a5 b5 e5))))
         :set-map `((1 ,(flatten (loop for p in perms collect p))))
         :rthm-seq-palette '((1 ((((4 4) - 32 32 - (e.) (s) - s s s - 
                                   - { 3 ts ts ts } (e) - - (s) 32 32 - (e)))  
                                 :pitch-seq-palette ((1 2 1 4 3 2 3 5 2 3)))))
         :rthm-seq-map `((1 ((vn ,(loop repeat (* 4 (length perms)) 
                                     collect 1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; inefficiently-permutate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let* ((perms (inefficiently-permutate '(0 1 2 3) :max 7 :skip 2 :fix t))
       (mini
        (make-slippery-chicken
         '+mini+
         :tempo-map '((1 (h 60)))
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '((0 ((d4 a4 d5 e5 a5 d6)))
                        (1 ((d4 e4 a4 d5 e5 a5 d6)))
                        (2 ((e4 a4 e5 a5 e6)))
                        (3 ((e4 a4 b4 e5 a5 b5 e5))))
         :set-map `((1 ,(loop for p in perms collect p)))
         :rthm-seq-palette '((1 ((((4 4) - 32 32 - (e.) (s) - s s s - 
                                   - { 3 ts ts ts } (e) - - (s) 32 32 - (e)))  
                                 :pitch-seq-palette ((1 2 1 4 3 2 3 5 2 3)))))
         :rthm-seq-map `((1 ((vn ,(loop repeat (length perms) collect 1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; shuffle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let* ((num-seqs 31)
       (pitch-seqs
        (loop repeat num-seqs 
           collect (shuffle '(1 2 1 4 3 2 3 5 2 3) :fix nil)))
       (mini
        (make-slippery-chicken
         '+mini+
         :tempo-map '((1 (h 60)))
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '((1 ((d4 e4 a4 d5 e5 a5 d6))))
         :set-map `((1 ,(loop repeat num-seqs collect 1)))
         :rthm-seq-palette `((1 ((((4 4) - 32 32 - (e.) (s) - s s s - 
                                   - { 3 ts ts ts } (e) - - (s) 32 32 - (e)))  
                                 :pitch-seq-palette ,pitch-seqs)))
         :rthm-seq-map `((1 ((vn ,(loop repeat num-seqs collect 1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; move-repeats
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let* ((perms (move-repeats
               (inefficiently-permutate '(0 1 2 3) :max 7 :skip 2 :fix t)))
       (mini
        (make-slippery-chicken
         '+mini+
         :tempo-map '((1 (h 60)))
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '((0 ((d4 a4 d5 e5 a5 d6)))
                        (1 ((d4 e4 a4 d5 e5 a5 d6)))
                        (2 ((e4 a4 e5 a5 e6)))
                        (3 ((e4 a4 b4 e5 a5 b5 e5))))
         :set-map `((1 ,(loop for p in perms collect p)))
         :rthm-seq-palette '((1 ((((4 4) - 32 32 - (e.) (s) - s s s - 
                                   - { 3 ts ts ts } (e) - - (s) 32 32 - (e)))  
                                 :pitch-seq-palette ((1 2 1 4 3 2 3 5 2 3)))))
         :rthm-seq-map `((1 ((vn ,(loop repeat (length perms) collect 1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF