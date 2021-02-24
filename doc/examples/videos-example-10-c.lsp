;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File:             videos-example-10-c.lsp
;;;
;;; Class Hierarchy:  None
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Lisp example code to accompany video tutorial 10
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

(let* ((remix-series (remix-in (fibonacci-transitions 71 '(1 2 3 4))))
       (num-seqs (length remix-series))
       (mini
        (make-slippery-chicken
         '+mini+
         :tempo-map '((1 (q 126)))
         :ensemble '(((fl (flute :midi-channel 1))
                      (ob (oboe :midi-channel 2))
                      (cl (b-flat-clarinet :midi-channel 3))))
         :set-palette '((1 ((d4 a4 d5 e5 a5 d6)))
                        (2 ((f3 d4 e4 a4 d5 e5 a5 d6)))
                        (3 ((f3 e4 a4 e5 a5 e6)))
                        (4 ((e4 a4 b4 e5 a5 b4 e6))))
         :set-map `((1 ,remix-series))
         :rthm-seq-palette '((1 ((((3 4) - e e - - +e e - - +e e -))
                                 :pitch-seq-palette ((4 3 2 1))))
                             (2 ((((3 4) q - e e - - +e e -))
                                 :pitch-seq-palette ((1 4 3 2))))
                             (3 ((((3 4) q q - e s s -))
                                 :pitch-seq-palette ((3 2 1 4 5))))
                             (4 ((((3 4) - s s e - - +e e - +q))
                                 :pitch-seq-palette ((3 4 2 1))))
                             (5 ((((3 4) - e e - - s s e - +q))
                                 :pitch-seq-palette ((1 1 3 4 2)))))
         :rthm-seq-map 
         `((1 ((fl ,(fibonacci-transitions num-seqs '(1 2 3)))
               (ob ,(fibonacci-transitions num-seqs '(2 3 4)))
               (cl ,(fibonacci-transitions num-seqs '(3 4 5)))))))))
  (print num-seqs)
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF