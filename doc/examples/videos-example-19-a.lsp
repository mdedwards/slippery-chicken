;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File:             videos-example-19-a.lsp
;;;
;;; Class Hierarchy:  None
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Lisp example code to accompany video tutorial 19
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

(let* ((num-seqs 971)
       (orig-palette (make-rsp 'orig 
                               '((1 ((((3 4) - e e - - +e e - - +e e -)) 
                                     :pitch-seq-palette ((4 3 2 1))))
                                 (2 ((((3 4) q - e e - - +e e -))
                                     :pitch-seq-palette ((1 4 3 2))))
                                 (3 ((((3 4) q q - e s s -))
                                     :pitch-seq-palette ((3 2 1 4 5)))))))
       (chopped-palette 
        (chop orig-palette 
              '((1 4) (1 3) (2 4) (1 2) (2 3) (3 4) (1 1) (2 2) (3 3) (4 4)) 
              's))
       (mini
        (make-slippery-chicken
         '+mini+
         :tempo-map '((1 (q 126)))
         :ensemble '(((fl (flute :midi-channel 1))
                      (ob (oboe :midi-channel 2))
                      (cl (b-flat-clarinet :midi-channel 3))))
         :set-palette '((0 ((d4 a4 d5 e5 a5 d6)))
                        (1 ((f3 d4 e4 a4 d5 e5 a5 d6)))
                        (2 ((f3 e4 a4 e5 a5 e6)))
                        (3 ((e4 a4 b4 e5 a5 b4 e6)))
                        (4 ((e4 fs4 gs4 b4 e5 b5)))
                        (5 ((e3 e4 fs4 gs4 b4 cs5 e5 b5)))
                        (6 ((fs3 fs4 gs4 a4 cs5 a5 cs6)))
                        (7 ((fs4 gs4 a4 b4 cs5 fs5)))
                        (8 ((cs3 fs4 gs4 a4 b4 e5 gs5 b5 e6)))
                        (9 ((fs4 gs4 a4 e5 a5 e6)))
                        (10 ((d3 e4 fs4 a4 e5 a5))))
         :set-map `((1 ,(fibonacci-transitions num-seqs 11)))
         :rthm-seq-palette chopped-palette
         :rthm-seq-map 
         `((1 ((fl ,(fibonacci-transitions num-seqs (loop for rs from 1 to 30
                                                       collect (list 1 rs))))
               (ob ,(fibonacci-transitions num-seqs (loop for rs from 1 to 30
                                                       collect (list 2 rs))))
               (cl ,(fibonacci-transitions num-seqs (loop for rs from 1 to 30
                                                       collect 
                                                         (list 3 rs))))))))))
  (re-bar mini :min-time-sig '(4 4))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF