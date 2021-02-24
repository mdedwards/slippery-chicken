;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File:             videos-example-08-a.lsp
;;;
;;; Class Hierarchy:  None
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Lisp example code to accompany video tutorial 8
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

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vln (violin :midi-channel 1))
                      (vla (viola :midi-channel 2))
                      (vlc (cello :midi-channel 3))))
         :set-palette '((1 ((c2 d2 e2 f2 g2 a2 b2 c3 d3 e3 f3 g3 a3 b3 c4 d4 e4 
                                f4 g4 a4 b4 c5 d5 e5 f5 g5 a5 b5 c6))))
         :set-limits-high '((vln (0 a4 50 c6 100 c5))
                            (vla (0 d4 50 e5 100 g4))
                            (vlc (0 d3 50 g4 100 d4)))
         :set-limits-low '((vln (0 g3 50 c5 100 c4))
                           (vla (0 d3 50 e4 100 g3))
                           (vlc (0 c2 50 g3 100 d3)))
         :set-map '((1 (1 1 1 1 1 1 1 1 1 1)))
         :rthm-seq-palette '((1 ((((2 4) (s) - e s - - e e -))
                                 :pitch-seq-palette ((1 2 3 4)))))
         :rthm-seq-map '((1 ((vln (1 1 1 1 1 1 1 1 1 1))
                             (vla (1 1 1 1 1 1 1 1 1 1))
                             (vlc (1 1 1 1 1 1 1 1 1 1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF