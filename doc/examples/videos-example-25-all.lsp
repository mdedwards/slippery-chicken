;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File:             videos-example-25-all.lsp
;;;
;;; Class Hierarchy:  None
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Lisp example code to accompany video tutorial 25
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
;;; bar line types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((fl (flute :midi-channel 1))))
         :set-palette '((1 ((f4 g4 a4))))
         :set-map '((1 (1 1 1)))
         :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                 :pitch-seq-palette ((1 1 1 1 1 1 1 1)))))
         :rthm-seq-map '((1 ((fl (1 1 1))))))))
  (change-bar-line-type mini 1 1)
  (change-bar-line-type mini 3 5)
  (cmn-display mini)
  (write-lp-data-for-all mini :base-path "/tmp/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; clefs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vc (cello :midi-channel 1))))
         :tempo-map '((1 (q 72)))
         :set-palette '((1 ((g3 a3 b3 c4 d4 e4 f4 g4))))
         :set-map '((1 (1 1 1)))
         :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                 :pitch-seq-palette (1 2 3 4 5 6 7 8))))
         :avoid-melodic-octaves nil
         :rthm-seq-map '((1 ((vc (1 1 1)))))))) 
  (auto-clefs mini)
  (add-clef mini 'vc 2 2 'tenor)
  (add-clef mini 'vc 3 3 'treble)
  (delete-clefs mini 'vc 3 3)
  (cmn-display mini :auto-clefs nil)
  (write-lp-data-for-all mini :auto-clefs nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; c-scores
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((fl (flute :midi-channel 1))
                      (cl (b-flat-clarinet :midi-channel 2))))
         :set-palette '((1 ((b4 c5 d5 e5 f5 g5 a5 b5 c6 d6 e6 f6 g6))))
         :set-map '((1 (1 1 1 1)))
         :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                 :pitch-seq-palette (1 2 3 4 5 6 7 8))))
         :rthm-seq-map '((1 ((fl (1 1 1 1))
                             (cl (1 1 1 1)))))))) 
  (cmn-display mini :in-c t)
  (write-lp-data-for-all mini :in-c t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; parts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((fl (flute :midi-channel 1))
                      (cl (b-flat-clarinet :midi-channel 2))))
         :set-palette '((1 ((b4 c5 d5 e5 f5 g5 a5 b5 c6 d6 e6 f6 g6))))
         :set-map '((1 (1 1 1 1)))
         :rthm-seq-palette '((1 ((((4 4) - e e e e - - e e e e -))
                                 :pitch-seq-palette (1 2 3 4 5 6 7 8))))
         :rthm-seq-map '((1 ((fl (1 1 1 1))
                             (cl (1 1 1 1)))))))) 
  (cmn-display mini :players '(cl)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF