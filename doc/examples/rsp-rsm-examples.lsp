;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File:             rsp-rsm-examples.lsp
;;;
;;; Class Hierarchy:  None
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Lisp-code examples to accompany rsp-rsm.html
;;;
;;; Author:           Sean Reed
;;;
;;; Creation date:    17th July 2012
;;;
;;; $$ Last modified: 22:49:59 Fri May 17 2013 BST
;;;
;;; SVN ID: $Id: rsp-rsm-examples.lsp 3538 2013-05-18 08:29:15Z medward2 $
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
;;; rsp-rsm.html examples
;;; ============================================================================

;;;  rthm-seq object - measures
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '(((1 ((f4)))) :recurse-simple-data nil)
         :set-map '((1 (1)))
         :rthm-seq-palette '((seq1 ((((2 4) (s) - s e - - e e -)
                                     (q - e s s -)
                                     ((5 8)  - e s s e - q)))))
         :rthm-seq-map '((1 ((vn (seq1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))
  
;;;  rthm-seq object - pitch curves
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c4))))
         :set-map '((1 (1 1 1 1)))
         :rthm-seq-palette 
         '((seq1 ((((2 4) (s) - s e - - e e -)
                   (q - e s s -)
                   ((5 8)  - e s s e - q))
                  :pitch-seq-palette ((5 5 3 3 3 4 3 3 1 1 1 1 1) 
                                      (4 6 6 8 6 7 5 6 7 4 2 2 3)
                                      (3 4 1 1 1 3 3 2 1 1 2 2 3)))))
         :rthm-seq-map '((1 ((vn (seq1 seq1 seq1 seq1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))
  
;;;  rthm-seq object - marks
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c4))))
         :set-map '((1 (1 1 1 1)))
         :rthm-seq-palette 
         '((seq1 ((((2 4) (s) - s e - - e e -)
                   (q - e s s -)
                   ((5 8)  - e s s e - q))
                  :pitch-seq-palette ((5 5 3 3 3 4 3 3 1 1 1 1 1)
                                      (4 6 6 8 6 7 5 6 7 4 2 2 3)
                                      (3 4 1 1 1 3 3 2 1 1 2 2 3))
                  :marks (as 1 te 2 s 3 4 slur 5 6 dim-beg 9 dim-end 13)))) 
         :rthm-seq-map '((1 ((vn (seq1 seq1 seq1 seq1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))
  
;;;  rthm-seq-palette object; printed using cmn-display
;;; ----------------------------------------------------------------------------
(let* ((rsp (make-rsp
             'rsp-test
             '((seq1 ((((2 4) (s) - s e - - e e -)
                       (q - e s s -)
                       ((5 8)  - e s s e - q))
                      :pitch-seq-palette ((5 5 3 3 3 4 3 3 1 1 1 1 1)
                                          (4 6 6 8 6 7 5 6 7 4 2 2 3)
                                          (3 4 1 1 1 3 3 2 1 1 2 2 3)) 
                      :marks (as 1 te 2 s 3 4 slur 5 6 dim-beg 9 
                                 dim-end 13)))
               (seq2 ((((3 4) { 3 - te (te) te - } +q s (e.))
                       ((2 4) (q.) e)
                       (e q e))
                      :pitch-seq-palette ((6 3 5 5 2 2 2) 
                                          (1 3 5 4 5 2 1))
                      :marks (s 1 a 2 slur 2 4 s 5 6 te 7 s 8)))
               (seq3 ((((7 8) - s s s s - +q - +e (s) s - (e)))
                      :pitch-seq-palette ((6 6 3 5 7) 
                                          (5 4 4 2 3) 
                                          (2 2 3 4 2) 
                                          (1 1 2 3 1) 
                                          (2 3 4 2 3))
                      :marks (s 1 2 a 3 slur 3 6 as 7)))))))
  (cmn-display rsp))
  
;;;  rthm-seq-map
;;; ----------------------------------------------------------------------------
(let* ((random-rs (inefficiently-permutate '(q e s (s)) :max 9 :sublists t))
       (mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))
                      (va (viola :midi-channel 2))
                      (vc (cello :midi-channel 3))))
         :set-palette '((1 ((c2 d2 e2 f2 g2 a3 b2
                                c3 d3 e3 f3 g3 a3 b3
                                c4 d4 e4 f4 g4 a4 b4 
                                c5 d5 e5 f5 g5 a5 b5 c6))))
         :set-map '((1 (1 1 1))
                    (2 (1 1 1 1 1))
                    (3 (1 1 1 1 1 1 1)))
         :rthm-seq-palette 
         (loop for rs in random-rs
            for rs-di in '(seq1 seq2 seq3 seq4 seq5 seq6 seq7 seq8 seq9)
            collect (list rs-di
                          (list (list (list '(2 4)
                                            (first rs)
                                            (second rs)
                                            (third rs)
                                            (fourth rs)))
                                ':pitch-seq-palette
                                '((1 2 3)))))
         :rthm-seq-map '((1 ((vn (seq3 seq1 seq2))
                             (va (seq1 seq2 seq3))
                             (vc (seq5 seq3 seq4))))
                         (2 ((vn (seq5 seq1 seq1 seq1 seq5))
                             (va (seq7 seq4 seq2 seq3 seq2))
                             (vc (seq6 seq5 seq3 seq4 seq7))))
                         (3 ((vn (seq9 seq8 seq1 seq5 seq3 seq2 seq1))
                             (va (seq9 seq2 seq4 seq5 seq4 seq1 seq1))
                             (vc (seq9 seq3 seq7 seq5 seq1 seq3 seq1)))))))) 
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))
  
