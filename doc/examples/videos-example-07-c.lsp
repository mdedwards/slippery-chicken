;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File:             videos-example-07-c.lsp
;;;
;;; Class Hierarchy:  None
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Lisp example code to accompany video tutorial 7
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
         :title "Mini Piece"
         :tempo-map '((1 (q 84)))
         :ensemble '(((cl (b-flat-clarinet :midi-channel 1))
                      (vn (violin :midi-channel 2))
                      (vc (cello :midi-channel 3))))
         :staff-groupings '(1 2)  
         :set-palette '((tonic ((c4 e4 g4 c5)))
                        (subdominant ((c4 f4 a4 c5)))
                        (dominant ((b3 d4 f4 g4 b4 d5))))
         :set-map '((1 (tonic dominant tonic dominant))
                    (2 (dominant dominant dominant tonic))
                    (3 (tonic subdominant dominant tonic)))
         :rthm-seq-palette '((1 ((((2 4) (s) - e s - - e e -))
                                 :pitch-seq-palette ((1 2 3 4)))))
         :rthm-seq-map '((1 ((cl (1 1 1 1))
                             (vn (1 1 1 1))
                             (vc (1 1 1 1))))
                         (2 ((cl (1 1 1 1))
                             (vn (1 1 1 1))
                             (vc (1 1 1 1))))
                         (3 ((cl (1 1 1 1))
                             (vn (1 1 1 1))
                             (vc (1 1 1 1))))))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF