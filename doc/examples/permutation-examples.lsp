;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File:             permutations-examples.lsp
;;;
;;; Class Hierarchy:  None
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Lisp example code to accompany permutations.html
;;;
;;; Author:           Sean Reed
;;;
;;; Creation date:    15th November 2012
;;;
;;; $$ Last modified: 22:45:14 Fri May 17 2013 BST
;;;
;;; SVN ID: $Id: permutation-examples.lsp 3538 2013-05-18 08:29:15Z medward2 $
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

;;; ===========================================================================
;;;  PERMUTATIONS
;;; ===========================================================================

;;;  just the function
;;; ---------------------------------------------------------------------------

(permutations 4)

;;;  using the function to generate set-map and rthm-seq-map
;;; ---------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((perms (permutations 4))
       (mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :tempo-map '((1 (q 84)))
         :set-palette '((0 ((d4 a4 d5 e5 a5 d6)))
                        (1 ((d4 e4 a4 d5 e5 a5 d6)))
                        (2 ((e4 a4 e5 a5 e6)))
                        (3 ((e4 a4 b4 e5 a5 b5 e5))))
         :set-limits-high '((vn (0 d5 50 c7 100 g4)))
         :set-limits-low '((vn (0 g3 50 e5 100 c4)))
         :set-map `((1 ,(flatten (loop for p in perms
                                    collect p))))
         :rthm-seq-palette '((0 ((((4 4) - 32 32 - (e.) (s) - s s s - 
                                   - { 3 ts ts ts } (e) - - (s) 32 32 - (e)))  
                                 :pitch-seq-palette ((1 2 1 4 3 2 3 5 2 3)))) 
                             (1 ((((5 8) { 3 - ts ts ts - } s (s) (s) s (s) s 
                                   - s s -))
                                 :pitch-seq-palette ((7 5 2 3 2 2 5 7))))
                             (2 ((((3 4) (s) - s s s - +q +s s (e)))
                                 :pitch-seq-palette ((2 3 7 6))))
                             (3 ((((3 8) (s) - 32 32 - s (e.)))
                                 :pitch-seq-palette ((7 6 3)))))
         :rthm-seq-map `((1 ((vn 
                              ,(flatten (loop for p in perms
                                           collect p))))))))) 
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;; ===========================================================================
;;;  inefficient-permutations
;;; ===========================================================================

;;;  just the function
;;; ---------------------------------------------------------------------------

(inefficient-permutations 4)
(inefficient-permutations 4 :max 7)
(inefficient-permutations 4 :max 7 :skip 2)
(loop repeat 5 collect (inefficient-permutations 4 :max 4 :fix nil))
(loop repeat 5 collect (inefficient-permutations 4 :max 4 :fix t))

;;;  using the function to generate set-map and rthm-seq-map
;;; ---------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((perms (inefficient-permutations 4 :max 11))
       (mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :tempo-map '((1 (q 84)))
         :set-palette '((0 ((d4 a4 d5 e5 a5 d6)))
                        (1 ((d4 e4 a4 d5 e5 a5 d6)))
                        (2 ((e4 a4 e5 a5 e6)))
                        (3 ((e4 a4 b4 e5 a5 b5 e5))))
         :set-limits-high '((vn (0 d5 50 c7 100 g4)))
         :set-limits-low '((vn (0 g3 50 e5 100 c4)))
         :set-map `((1 ,(flatten (loop for p in perms
                                    collect p))))
         :rthm-seq-palette '((0 ((((4 4) - 32 32 - (e.) (s) - s s s - 
                                   - { 3 ts ts ts } (e) - - (s) 32 32 - (e)))  
                                 :pitch-seq-palette ((1 2 1 4 3 2 3 5 2 3)))) 
                             (1 ((((5 8) { 3 - ts ts ts - } s (s) (s) s (s) s 
                                   - s s -))
                                 :pitch-seq-palette ((7 5 2 3 2 2 5 7))))
                             (2 ((((3 4) (s) - s s s - +q +s s (e)))
                                 :pitch-seq-palette ((2 3 7 6))))
                             (3 ((((3 8) (s) - 32 32 - s (e.)))
                                 :pitch-seq-palette ((7 6 3)))))
         :rthm-seq-map `((1 ((vn 
                              ,(flatten (loop for p in perms
                                           collect p))))))))) 
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;; ===========================================================================
;;;  permutate
;;; ===========================================================================

;;;  just the function
;;; ---------------------------------------------------------------------------

(permutate '(a b c d))

;;;  using the function to generate rthm-seq-palette
;;; ---------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((perms (permutate '(e (e) s (e.))))
       (mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :tempo-map '((1 (q 84)))
         :set-palette '((1 ((d4 a4 d5 e5 a5 d6))))
         :set-limits-high '((vn (0 d5 50 c7 100 g4)))
         :set-limits-low '((vn (0 g3 50 e5 100 c4)))
         :set-map (list (list 1 (loop repeat (length perms) collect 1)))
         :rthm-seq-palette (loop for p in perms
                              for rs-id from 0
                              collect `(,rs-id
                                        ((((2 4) ,@(nth rs-id perms)))
                                         :pitch-seq-palette ((1 3)))))
         :rthm-seq-map (list 
                        (list 1 
                              (list 
                               (list 'vn 
                                     (loop for rs from 0 below (length perms)
                                        collect rs)))))))) 
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;; ===========================================================================
;;;  inefficiently-permutate
;;; ===========================================================================

;;;  just the function
;;; ---------------------------------------------------------------------------

(inefficiently-permutate '(a b c d))
(inefficiently-permutate '(a b c d) :sublists t)
(inefficiently-permutate '(a b c d) :max 7)
(inefficiently-permutate '(a b c d) :max 7 :skip 2)
(loop repeat 5 collect (inefficiently-permutate '(a b c d) :max 5 :fix nil))
(loop repeat 5 collect (inefficiently-permutate '(a b c d) :max 5 :fix t))

;;;  using the function to generate rthm-seq-palette
;;; ---------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((perms (inefficiently-permutate '(e (e) s (e.) q s s (e)) 
                                       :max 91 
                                       :sublists t))
       (mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((fl (flute :midi-channel 1))
                      (ob (oboe :midi-channel 2))
                      (bn (bassoon :midi-channel 3))))
         :tempo-map '((1 (q 84)))
         :set-palette '((0 ((fs2 b2 d4 a4 d5 e5 a5 d6)))
                        (1 ((b2 fs3 d4 e4 a4 d5 e5 a5 d6)))
                        (2 ((cs3 fs3 e4 a4 e5 a5 e6)))
                        (3 ((fs2 cs3 e4 a4 b4 e5 a5 b5 e5)))
                        (4 ((d2 a2 e4 fs4 gs4 b4 e5 b5)))
                        (5 ((a2 e3 e4 fs4 gs4 b4 cs5 e5 b5)))
                        (6 ((cs3 fs3 fs4 gs4 a4 cs5 a5 cs6)))
                        (7 ((fs2 cs3 fs4 gs4 a4 b4 cs5 fs5)))
                        (8 ((e2 a2 cs4 fs4 gs4 a4 b4 e5 gs5 b5 e6)))
                        (9 ((d2 a2 fs4 gs4 a4 e5 a5 e6)))
                        (10 ((a2 d2 e4 fs4 a4 e5 a5))))
         :set-limits-high '((ob (0 a5 100 a5))
                            (bn (0 g3 100 g3)))
         :set-limits-low '((fl (0 d5 100 d5)))
         :set-map (list (list 1 (loop for sn from 0 below (length perms) 
                                   collect (mod sn 11)))) ; mod 11 = 11 sets 
         :rthm-seq-palette (loop for p in perms
                              for rs-id from 0
                              collect `(,rs-id
                                        ((((4 4) ,@(nth rs-id perms)))
                                         :pitch-seq-palette ((1 3 2 5 6)))))
         :rthm-seq-map (list 
                        (list 1 
                              (list 
                               (list 'fl 
                                     (loop for rs from 0 below (length perms)
                                        collect rs))
                               (list 'ob 
                                     (loop for rs from 0 below (length perms)
                                        collect 
                                          (mod (1+ rs) (length perms))))
                               (list 'bn
                                     (loop for rs from 0 below (length perms)
                                        collect 
                                          (mod (+ rs 2) (length perms))))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;; ===========================================================================
;;;  shuffle
;;; ===========================================================================

;;;  just the function
;;; ---------------------------------------------------------------------------

(shuffle '(1 2 3 4 5 6 7))
(shuffle '(1 2 3 4 5 6 7) :start 1 :end 5)
(let* ((l '(1 2 3 4 5 6 7)))
  (shuffle l :copy t)
  (print l)
  (shuffle l :copy nil)
  (print l))
(loop repeat 5 collect (shuffle '(1 2 3 4 5 6 7) :fix t))
(loop repeat 5 collect (shuffle '(1 2 3 4 5 6 7) :fix nil))
(loop repeat 3
   collect
     (loop for i below 3 
        collect (shuffle '(1 2 3 4 5 6 7) :fix t :reset (zerop i))))

;;;  using the function to generate set-map and rsm
;;; ---------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((map-shuffles 
        (flatten 
         (loop repeat 11 
            collect 
              (shuffle '(1 2 3 4) :fix nil))))
       (mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :tempo-map '((1 (q 84)))
         :set-palette '((1 ((d4 a4 d5 e5 a5 d6)))
                        (2 ((d4 e4 a4 d5 e5 a5 d6)))
                        (3 ((e4 a4 e5 a5 e6)))
                        (4 ((e4 a4 b4 e5 a5 b5 e5))))
         :set-limits-high '((vn (0 d5 50 c7 100 g4)))
         :set-limits-low '((vn (0 g3 50 e5 100 c4)))
         :set-map (list (list 1 map-shuffles))
         :rthm-seq-palette '((1 ((((4 4) - 32 32 - (e.) (s) - s s s - 
                                   - { 3 ts ts ts } (e) - - (s) 32 32 - (e)))  
                                 :pitch-seq-palette ((1 2 1 4 3 2 3 5 2 3)))) 
                             (2 ((((5 8) { 3 - ts ts ts - } s (s) (s) s (s) s 
                                   - s s -))
                                 :pitch-seq-palette ((7 5 2 3 2 2 5 7))))
                             (3 ((((3 4) (s) - s s s - +q +s s (e)))
                                 :pitch-seq-palette ((2 3 7 6))))
                             (4 ((((3 8) (s) - 32 32 - s (e.)))
                                 :pitch-seq-palette ((7 6 3)))))
         :rthm-seq-map (list 
                        (list 1 
                              (list 
                               (list 'vn map-shuffles))))))) 
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;; ===========================================================================
;;;  multi-shuffle
;;; ===========================================================================

;;;  just the function
;;; ---------------------------------------------------------------------------

(multi-shuffle '(1 2 3 4 5 6 7) 11)
(multi-shuffle '(1 2 3 4 5 6 7) 11 :start 1 :end 5)
(let* ((l '(1 2 3 4 5 6 7)))
  (multi-shuffle l 11 :copy t)
  (print l)
  (multi-shuffle l 11 :copy nil)
  (print l))
(loop repeat 5 collect (multi-shuffle '(1 2 3 4 5 6 7) 11 :fix t))
(loop repeat 5 collect (multi-shuffle '(1 2 3 4 5 6 7) 11 :fix nil))
(loop repeat 3
   collect
     (loop for i below 3 
        collect (multi-shuffle '(1 2 3 4 5 6 7) 11 :fix t :reset (zerop i))))


;;;  using the function to generate set-map and rsm
;;; ---------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((map-shuffles 
        (flatten 
         (loop repeat 11 
            collect 
              (multi-shuffle '(1 2 3 4) 11 :fix nil))))
       (mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :tempo-map '((1 (q 84)))
         :set-palette '((1 ((d4 a4 d5 e5 a5 d6)))
                        (2 ((d4 e4 a4 d5 e5 a5 d6)))
                        (3 ((e4 a4 e5 a5 e6)))
                        (4 ((e4 a4 b4 e5 a5 b5 e5))))
         :set-limits-high '((vn (0 d5 50 c7 100 g4)))
         :set-limits-low '((vn (0 g3 50 e5 100 c4)))
         :set-map (list (list 1 map-shuffles))
         :rthm-seq-palette '((1 ((((4 4) - 32 32 - (e.) (s) - s s s - 
                                   - { 3 ts ts ts } (e) - - (s) 32 32 - (e)))  
                                 :pitch-seq-palette ((1 2 1 4 3 2 3 5 2 3)))) 
                             (2 ((((5 8) { 3 - ts ts ts - } s (s) (s) s (s) s 
                                   - s s -))
                                 :pitch-seq-palette ((7 5 2 3 2 2 5 7))))
                             (3 ((((3 4) (s) - s s s - +q +s s (e)))
                                 :pitch-seq-palette ((2 3 7 6))))
                             (4 ((((3 8) (s) - 32 32 - s (e.)))
                                 :pitch-seq-palette ((7 6 3)))))
         :rthm-seq-map (list 
                        (list 1 
                              (list 
                               (list 'vn map-shuffles))))))) 
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;; ===========================================================================
;;;  multi-shuffle-with-perms
;;; ===========================================================================

;;;  just the function
;;; ---------------------------------------------------------------------------

(multi-shuffle-with-perms '(0 1 2 3 4) 7)

;;;  using the function to generate set-map and rsm
;;; ---------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((map-shuffles 
        (flatten 
         (loop for i from 1 to 11 
            collect 
              (multi-shuffle-with-perms '(1 2 3 4) 11))))
       (mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :tempo-map '((1 (q 84)))
         :set-palette '((1 ((d4 a4 d5 e5 a5 d6)))
                        (2 ((d4 e4 a4 d5 e5 a5 d6)))
                        (3 ((e4 a4 e5 a5 e6)))
                        (4 ((e4 a4 b4 e5 a5 b5 e5))))
         :set-limits-high '((vn (0 d5 50 c7 100 g4)))
         :set-limits-low '((vn (0 g3 50 e5 100 c4)))
         :set-map (list (list 1 map-shuffles))
         :rthm-seq-palette '((1 ((((4 4) - 32 32 - (e.) (s) - s s s - 
                                   - { 3 ts ts ts } (e) - - (s) 32 32 - (e)))  
                                 :pitch-seq-palette ((1 2 1 4 3 2 3 5 2 3)))) 
                             (2 ((((5 8) { 3 - ts ts ts - } s (s) (s) s (s) s 
                                   - s s -))
                                 :pitch-seq-palette ((7 5 2 3 2 2 5 7))))
                             (3 ((((3 4) (s) - s s s - +q +s s (e)))
                                 :pitch-seq-palette ((2 3 7 6))))
                             (4 ((((3 8) (s) - 32 32 - s (e.)))
                                 :pitch-seq-palette ((7 6 3)))))
         :rthm-seq-map (list 
                        (list 1 
                              (list 
                               (list 'vn map-shuffles))))))) 
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;; ===========================================================================
;;;  move-repeats
;;; ===========================================================================

;;;  just the function
;;; ---------------------------------------------------------------------------

(move-repeats '(1 2 3 3 4 5 6 7 8 8 9 10))
(move-repeats '((a b c) (c a b) (c a b) (d e f) (a b c) (g h i)))

;;;  using the function together with inefficiently-permutate
;;;  to generate set-map and rsm
;;; ---------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((map-shuffles 
        (move-repeats (inefficiently-permutate '(1 2 3 4) :max 11)))
       (mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :tempo-map '((1 (q 84)))
         :set-palette '((1 ((d4 a4 d5 e5 a5 d6)))
                        (2 ((d4 e4 a4 d5 e5 a5 d6)))
                        (3 ((e4 a4 e5 a5 e6)))
                        (4 ((e4 a4 b4 e5 a5 b5 e5))))
         :set-limits-high '((vn (0 d5 50 c7 100 g4)))
         :set-limits-low '((vn (0 g3 50 e5 100 c4)))
         :set-map (list (list 1 map-shuffles))
         :rthm-seq-palette '((1 ((((4 4) - 32 32 - (e.) (s) - s s s - 
                                   - { 3 ts ts ts } (e) - - (s) 32 32 - (e)))  
                                 :pitch-seq-palette ((1 2 1 4 3 2 3 5 2 3)))) 
                             (2 ((((5 8) { 3 - ts ts ts - } s (s) (s) s (s) s 
                                   - s s -))
                                 :pitch-seq-palette ((7 5 2 3 2 2 5 7))))
                             (3 ((((3 4) (s) - s s s - +q +s s (e)))
                                 :pitch-seq-palette ((2 3 7 6))))
                             (4 ((((3 8) (s) - 32 32 - s (e.)))
                                 :pitch-seq-palette ((7 6 3)))))
         :rthm-seq-map (list 
                        (list 1 
                              (list 
                               (list 'vn map-shuffles))))))) 
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;; ===========================================================================
;;;  random-rep
;;; ===========================================================================

;;;  just the function
;;; ---------------------------------------------------------------------------

(loop repeat 10 collect (random-rep 5))
(loop repeat 10 collect (random-rep 5 t))

;;;  using the function together with inefficiently-permutate
;;;  to generate set-map and rsm
;;; ---------------------------------------------------------------------------

(in-scale :chromatic)

(let* ((num-seqs 91)
       (mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :tempo-map '((1 (q 84)))
         :set-palette '((0 ((d4 a4 d5 e5 a5 d6)))
                        (1 ((d4 e4 a4 d5 e5 a5 d6)))
                        (2 ((e4 a4 e5 a5 e6)))
                        (3 ((e4 a4 b4 e5 a5 b5 e5))))
         :set-limits-high '((vn (0 d5 50 c7 100 g4)))
         :set-limits-low '((vn (0 g3 50 e5 100 c4)))
         :set-map (list (list 1 (loop repeat num-seqs collect (random-rep 4))))
         :rthm-seq-palette '((0 ((((4 4) - 32 32 - (e.) (s) - s s s - 
                                   - { 3 ts ts ts } (e) - - (s) 32 32 - (e)))  
                                 :pitch-seq-palette ((1 2 1 4 3 2 3 5 2 3)))) 
                             (1 ((((5 8) { 3 - ts ts ts - } s (s) (s) s (s) s 
                                   - s s -))
                                 :pitch-seq-palette ((7 5 2 3 2 2 5 7))))
                             (2 ((((3 4) (s) - s s s - +q +s s (e)))
                                 :pitch-seq-palette ((2 3 7 6))))
                             (3 ((((3 8) (s) - 32 32 - s (e.)))
                                 :pitch-seq-palette ((7 6 3)))))
         :rthm-seq-map (list 
                        (list 1 
                              (list 
                               (list 'vn (loop repeat num-seqs 
                                            collect (random-rep 4))))))))) 
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))
