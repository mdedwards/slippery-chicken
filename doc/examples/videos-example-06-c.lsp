;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File:             videos-example-06-c.lsp
;;;
;;; Class Hierarchy:  None
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Lisp example code to accompany video tutorial 6
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
         :set-palette '((1 ((c2 d2 e2 f2 g2 a2 b2 c3 d3 e3 f3 g3 a3 a3 b3 c4 d4
                                e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1 1 1)))
         :rthm-seq-palette 
         '((1 ((((2 4) (s) - s e - - e e -)
                (q - e s s -)
                ((5 8)  - e s s e - q))
               :pitch-seq-palette ((7 6 7 6 7 6 7 6 7 6 7 6 7)
                                   (3 4 5 4 3 4 5 4 3 4 5 4 3)
                                   (1 2 1 2 1 2 1 2 1 2 1 2 1)))))
         :rthm-seq-map '((1 ((vln (1 1 1))
                             (vla (1 1 1))
                             (vlc (1 1 1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF