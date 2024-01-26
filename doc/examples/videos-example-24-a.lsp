;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File:             videos-example-24-a.lsp
;;;
;;; Class Hierarchy:  None
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Lisp example code to accompany video tutorial 24
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
         :title "Algorithmic Opera"
         :composer "Joe Green"   
         :ensemble '(((solo (violin :midi-channel 1))
                      (fl (flute :midi-channel 2))
                      (ob (oboe :midi-channel 3))
                      (hn (french-horn :midi-channel 4))
                      (tp (b-flat-trumpet :midi-channel 5))
                      (va (viola :midi-channel 6))
                      (vc (cello :midi-channel 7))))
         :staff-groupings '(1 2 2 2)
         :rehearsal-letters '(2 5 7)
         :set-palette '((1 ((c2 g2 a2 b2 e3 f3 a3 d4 e4 a4 c5 f5 d6 e6 b6))))
         :set-map '((1 (1 1 1 1 1 1 1)))
         :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                 :pitch-seq-palette ((1 2 3 2 1 2 3 2)))))
         :rthm-seq-map '((1 ((solo (1 1 1 1 1 1 1))
                             (fl (1 1 1 1 1 1 1))
                             (ob (1 1 1 1 1 1 1))
                             (hn (1 1 1 1 1 1 1))
                             (tp (1 1 1 1 1 1 1))
                             (va (1 1 1 1 1 1 1))
                             (vc (1 1 1 1 1 1 1))))))))
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF