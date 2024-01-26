;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File:             post-gen-editing-examples.lsp
;;;
;;; Class Hierarchy:  None
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Lisp-code examples to accompany post-gen-editing.html
;;;
;;; Author:           Sean Reed
;;;
;;; Creation date:    17th July 2012
;;;
;;; $$ Last modified: 16:26:35 Mon Oct 20 2014 BST
;;;
;;; SVN ID: $Id: post-gen-editing-examples.lsp 5043 2014-10-20 16:26:46Z medward2 $
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
;;; post-gen-editing.html examples
;;; ============================================================================

;;;  getting first event of a bar
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((2 4) (s) (s) e e e))
                                 :pitch-seq-palette ((1 2 3)))))
         :rthm-seq-map '((1 ((vn (1))))))))
  (data (get-event mini 1 1 'vn)))

;;;  getting first note of same bar
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((2 4) (s) (s) e e e))
                                 :pitch-seq-palette ((1 2 3)))))
         :rthm-seq-map '((1 ((vn (1))))))))
  (data (get-note mini 1 1 'vn)))

;;;  basic usage
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))
                      (va (viola :midi-channel 2))
                      (vc (cello :midi-channel 3))))
         :set-palette '((1 ((c3 d3 e3 f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1 1 1)))
         :rthm-seq-palette '((1 ((((4 4) - e (e) e e - (e) - e e e -))
                                 :pitch-seq-palette ((1 2 3 4 5 6)))))
         :rthm-seq-map '((1 ((vn (1 1 1))
                             (va (1 1 1))
                             (vc (1 1 1))))))))
  (add-mark-to-note mini 2 4 'va 'ppp)
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))
  
;;;  change-pitch
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+sc-object+
         :ensemble '(((va (viola :midi-channel 2))))
         :set-palette '((1 ((c3 d3 e3 f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1 1 1)))
         :rthm-seq-palette '((1 ((((4 4) - e (e) e e - (e) - e e e -))
                                 :pitch-seq-palette ((1 2 3 4 5 6)))))
         :rthm-seq-map '((1 ((va (1 1 1))))))))
  (change-pitch mini 3 2 'va 'c4)
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))
  
;;;  add-mark-to-note
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+sc-object+
         :ensemble '(((va (viola :midi-channel 2))))
         :set-palette '((1 ((c3 d3 e3 f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5)))) 
         :set-map '((1 (1 1 1)))
         :rthm-seq-palette '((1 ((((4 4) - e (e) e e - (e) - e e e -))
                                 :pitch-seq-palette ((1 2 3 4 5 6)))))
         :rthm-seq-map '((1 ((va (1 1 1))))))))
  (add-mark-to-note mini 3 2 'va 'ppp)
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;;  rm-marks-from-note
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+sc-object+
         :ensemble '(((va (viola :midi-channel 2))))
         :set-palette '((1 ((c3 d3 e3 f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5)))) 
         :set-map '((1 (1 1 1)))
         :rthm-seq-palette '((1 ((((4 4) e x 8))
                                 :pitch-seq-palette ((1 2 3 4 5 6 7 8))
                                 :marks (ppp 1 8))))
         :rthm-seq-map '((1 ((va (1 1 1))))))))
  (rm-marks-from-note mini 3 2 'va 'ppp)
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))
  
;;;  trill
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+sc-object+
         :ensemble '(((va (viola :midi-channel 2))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1 1 1)))
         :rthm-seq-palette '((1 ((((4 4) e x 8))
                                 :pitch-seq-palette ((1 2 3 4 5 6 7 8)))))
         :rthm-seq-map '((1 ((va (1 1 1))))))))
  (trill mini 'va 3 2 'd4)
  (write-lp-data-for-all mini))
  
;;;  tie-over-rests
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+sc-object+
         :ensemble '(((va (viola :midi-channel 2))))
         :set-palette '((1 ((c3 d3 e3 f3 g3 a3 b3 c4))))
         :set-map '((1 (1 1 1)))
         :rthm-seq-palette '((1 ((((4 4) e (e) e (e) (e) e e e))
                                 :pitch-seq-palette ((1 2 3 4 5)))))
         :rthm-seq-map '((1 ((va (1 1 1))))))))
  (tie-over-rests mini 1 1 'va :auto-beam 'q :consolidate-notes t)
  (handle-ties mini)
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))
  
;;;  re-bar
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+sc-object+
         :ensemble '(((va (viola :midi-channel 1))))
         :set-palette '((1 ((c3 d3 e3 f3 g3 a3 b3 c4))))
         :set-map (list (list 1 (loop repeat 20 collect 1)))
         :rthm-seq-palette '((1 ((((2 4) e x 4))
                                 :pitch-seq-palette ((1 2 3 4)))))
         :rthm-seq-map (list (list 1 (list (list 'va
                                                 (loop repeat 20 
                                                    collect 1))))))))
  (re-bar mini :min-time-sig '(4 4))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))
  
;;;  move-events
;;; ----------------------------------------------------------------------------
(let* ((mini       
        (make-slippery-chicken
         '+sc-object+
         :ensemble '(((vn (violin :midi-channel 1))
                      (vc (cello :midi-channel 2))))
         :set-palette '((1 ((c3 d3 e3 f3 g3 a3 b3
                                c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map (list (list 1 (loop repeat 5 collect 1)))
         :rthm-seq-palette '((1 ((((4 4) e x 8))
                                 :pitch-seq-palette ((1 2 3 4 5 6 7 8)))))
         :rthm-seq-map (list 
                        (list 1 
                              (list 
                               (list 'vn
                                     (loop repeat 5 collect 1))
                               (list 'vc 
                                     (loop repeat 5 collect nil))))))))
  (move-events mini 'vn 'vc 1 1 3 1)
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;;  double-events
;;; ----------------------------------------------------------------------------  
(let* ((mini       
        (make-slippery-chicken
         '+sc-object+
         :ensemble '(((vn (viola :midi-channel 1))
                      (va (viola :midi-channel 2))
                      (vc (cello :midi-channel 3))))
         :set-palette '((1 ((c3 d3 e3 f3 g3 a3 b3
                                c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map (list (list 1 (loop repeat 5 collect 1)))
         :rthm-seq-palette '((1 ((((4 4) e x 8))
                                 :pitch-seq-palette ((1 2 3 4 5 6 7 8)))))
         :rthm-seq-map (list 
                        (list 1 
                              (list 
                               (list 'vn
                                     (loop repeat 5 collect 1))
                               (list 'va
                                     (loop repeat 5 collect nil))
                               (list 'vc 
                                     (loop repeat 5 collect nil))))))))
  (double-events mini 'vn '(va vc) 1 1 3 1)
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))
  
