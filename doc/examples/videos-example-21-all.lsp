;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File:             videos-example-21-all.lsp
;;;
;;; Class Hierarchy:  None
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Lisp example code to accompany video tutorial 21
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; get-event
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; get-note
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; change-pitch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (change-pitch mini 3 2 'va 'cs4)
  (write-lp-data-for-all mini))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; add-mark-to-note
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (add-mark-to-note mini 3 2 'va 'ppp)
  (write-lp-data-for-all mini))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rm-marks-from-note
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))
                      (va (viola :midi-channel 2))
                      (vc (cello :midi-channel 3))))
         :set-palette '((1 ((c3 d3 e3 f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1 1 1)))
         :rthm-seq-palette '((1 ((((4 4) - e (e) e e - (e) - e e e -))
                                 :pitch-seq-palette ((1 2 3 4 5 6))
                                 :marks (ppp 2))))
         :rthm-seq-map '((1 ((vn (1 1 1))
                             (va (1 1 1))
                             (vc (1 1 1))))))))
  (rm-marks-from-note mini 3 2 'va 'ppp)
  (write-lp-data-for-all mini))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tie
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vn (violin :midi-channel 1))))
        :set-palette '((1 ((c4 d4 e4))))
        :set-map '((1 (1 1 1 1)))
        :rthm-seq-palette '((1 ((((2 4) q s s (s) s))
                                :pitch-seq-palette ((1 1 2 3)))))
        :rthm-seq-map '((1 ((vn (1 1 1 1))))))))
  (tie mini 2 1 'vn)
  (tie mini 3 2 'vn)
  (write-lp-data-for-all mini))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; move-events
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))
                      (va (viola :midi-channel 2))
                      (vc (cello :midi-channel 3))))
         :set-palette '((1 ((c3 d3 e3 f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1 1 1 1)))
         :rthm-seq-palette '((1 ((((2 4) q (e) e))
                                 :pitch-seq-palette ((1 2)))))
         :rthm-seq-map '((1 ((vn (1 1 1 1))
                             (va (nil nil nil nil))
                             (vc (1 1 1 1))))))))
  (move-events mini 'vn 'va 2 1 3 1 :consolidate-rests t :transposition -3)
  (write-lp-data-for-all mini))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; double-events
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))
                      (va (viola :midi-channel 2))
                      (vc (cello :midi-channel 3))))
         :set-palette '((1 ((c3 d3 e3 f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1 1 1 1)))
         :rthm-seq-palette '((1 ((((2 4) q (e) e))
                                 :pitch-seq-palette ((1 2)))))
         :rthm-seq-map '((1 ((vn (1 1 1 1))
                             (va (nil nil nil nil))
                             (vc (nil nil nil nil))))))))
  (double-events mini 'vn '(va vc) 2 1 3 1 
                 :consolidate-rests t :transposition -3)  
  (write-lp-data-for-all mini))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF