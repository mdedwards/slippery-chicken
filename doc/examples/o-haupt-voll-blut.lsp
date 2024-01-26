;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File:             o-haupt-voll-blut.lsp
;;;
;;; Class Hierarchy:  None
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Proof-of-concept code for tonal composition with sc
;;;
;;; Author:           Sean Reed
;;;
;;; Creation date:    25th September 2012
;;;
;;; $$ Last modified: 22:43:46 Fri May 17 2013 BST
;;;
;;; SVN ID: $Id: o-haupt-voll-blut.lsp 3538 2013-05-18 08:29:15Z medward2 $
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

(in-package :sc)
(in-scale :chromatic)

;; bach matthaeus passion o haupt voll blut und wunden

(let ((o-haupt-voll-blut
       (make-slippery-chicken
        '+o-haupt-voll-blut+
        :title "O Haupt voll Blut und Wunden"
        :composer "J. S. Bach (1865 - 1750)"
        :tempo-map '((1 (q 58)))
        :bars-per-system-map '((1 5) (6 6) (12 6))
        :ensemble '(((fl (flute :midi-channel 1))
                     (ob (oboe :midi-channel 2))
                     (cl (b-flat-clarinet :midi-channel 3))
                     (bn (bassoon :midi-channel 4))))
        :key-sig '(d minor)
        :set-palette '((1 ((c4 d4 f4 a4)))
                       (2 ((f3 g3 a3 bf3 c4 d4 e4 f4 g4 a4 bf4 c5 d5)))
                       (3 ((bf2 c3 f3 c4 d4 e4 f4 g4 a4)))
                       (4 ((cs3 e4 a4 e5)))
                       (5 ((a2 d3 e3 f3 g3 a3 cs4 d4 e4 g4 a4 d5 e5 f5)))
                       (6 ((d3 a3 f4 d5)))
                       (7 ((d3 c4 d4 a4 f5)))
                       (8 ((g3 a3 bf3 c4 f4 g4 bf4 c5 d5 e5)))
                       (9 ((f3 bf3 c4 d4 g4 a4 bf4 f5)))
                       (10 ((f3 ef4 f4 a4 c5)))
                       (11 ((g2 fs3 g3 bf3 d4 fs4 g4 a4 bf4 c5 d5)))
                       (12 ((d3 d4 fs4 a4)))
                       (13 ((b2 d4 g4 f5)))
                       (14 ((c3 g3 a3 b3 c4 d4 e4 f4 g4 c5 e5 f5 g5)))
                       (15 ((c3 f3 g3 g4 a4 b4 c5 d5 e5)))
                       (16 ((f3 c4 e4 f4 a4)))
                       (17 ((c3 g3 a3 bf3 c4 d4 e4 f4 g4 a4 bf4 c5)))
                       (18 ((f3 c4 f4 a4))))
        :set-map '((1 (1 2 3 4 5 6 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17
                       18)))
        :set-limits-high (loop for pl in '(fl ob cl bn) 
                            for n from 1
                            for pch in 
                              '((a4 d5 a4 e5 f5 d5 a4 d5 a4 e5 f5 d5 f5 e5 f5
                                 c5 d5 a4 f5 g5 e5 a4 c5 a4)
                                (f4 a4 f4 a4 a4 f4 f4 a4 f4 a4 a4 f4 a4 bf4 bf4
                                 a4 a4 fs4 g4 c5 c5 f4 f4 f4)
                                (d4 g4 d4 e4 e4 a3 d4 g4 d4 e4 e4 a3 d4 c4 d4
                                 f4 d4 d4 d4 g4 a4 c4 c4 c4)
                                (d4 d4 f3 cs3 a3 d3 d4 d4 f3 cs3 a3 d3 d3 bf3
                                 f3 f3 bf3 d3 b2 c4 g3 f4 c4 f4))
                            collect (list pl
                                     (loop for rs from 1 to (length pch)
                                        collect rs
                                        collect (nth (1- rs) pch))))
        :set-limits-low (loop for pl in '(fl ob cl bn) 
                           for n from 1
                           for pch in 
                             '((a4 a4 g4 e5 d5 d5 a4 a4 g4 e5 d5 d5 f5 c5 f5 c5
                                a4 a4 f5 e5 d5 a4 g4 a4)
                               (f4 d4 e4 a4 g4 f4 f4 d4 e4 a4 g4 f4 a4 f4 g4 a4
                                fs4 fs4 g4 g4 b4 e4 d4 f4)
                               (d4 c4 c4 e4 cs4 a3 d4 c4 c4 e4 cs4 a3 c4 bf3
                                bf3 ef4 d4 d4 d4 c4 g4 c4 g3 c4)
                               (c4 f3 bf2 cs3 a2 d3 c4 f3 bf2 cs3 a2 d3 d3 g3
                                f3 f3 g2 d3 b2 c3 c3 f3 c3 f3)) 
                           collect (list pl
                                         (loop for rs from 1 to (length pch)
                                            collect rs
                                            collect (nth (1- rs) pch))))
        :avoid-melodic-octaves nil
        :avoid-used-notes nil
        :rthm-seq-palette '((1 ((((1 4) q))
                                :pitch-seq-palette ((1))))
                            (2 ((((1 4) - e e -))
                                :pitch-seq-palette ((2 1))))
                            (3 ((((4 4) q q q q))
                                :pitch-seq-palette ((4 3 2 1) (4 3 2 1) 
                                                    (3 2 2 1) (4 3 2 1)
                                                    (4 3 2 1) (3 2 2 1)
                                                    (1 1 1 1) (4 2 3 1)
                                                    (1 2 2 2) (3 2 1 4))))
                            (4 ((((4 4) - e e - q - e e - q))
                                :pitch-seq-palette ((3 4 5 1 2 3) (3 4 5 1 2 3)
                                                    (3 2 3 2 1 2))))
                            (5 ((((4 4) - e e - q q q))
                                :pitch-seq-palette ((2 3 4 5 1) (2 3 4 5 1)
                                                    (3 2 1 2 3) (1 2 3 2 1)
                                                    (1 2 3 4 5) (1 2 3 3 2)))) 
                            (6 ((((3 4) h q))
                                :pitch-seq-palette ((1 2) (1 2) (1 1) (1 1) 
                                                    (1 2))
                                :marks (pause 2)))
                            (7 ((((3 4) q q q))
                                :pitch-seq-palette ((2 1 2) (2 1 1) (1 2 3)
                                                    (2 1 2) (2 1 1) (1 2 3)
                                                    (2 1 2) (2 1 1) (2 3 1))
                                :marks (slur 1 2 pause 3))) 
                            (8 ((((4 4) q q - e e - q))
                                :pitch-seq-palette ((3 3 2 1 2) (3 3 2 1 2)
                                                    (1 2 3 2 1) (4 3 2 1 2)))) 
                            (9 ((((4 4) q q q - e e -))
                                :pitch-seq-palette ((2 2 2 2 1) (2 2 2 2 1)
                                                    (1 2 2 1 2))))
                            (10 ((((4 4) - e e - - e e - q q))
                                 :pitch-seq-palette ((2 3 4 5 6 1))))
                            (11 ((((3 4) h.))
                                 :pitch-seq-palette ((1))
                                 :marks (pause 1)))
                            (12 ((((4 4) q - e e - q q))
                                 :pitch-seq-palette ((2 2 1 1 3) (1 5 4 3 2)
                                                     (2 3 4 5 1))))
                            (13 ((((3 4) q - e e - q))
                                 :pitch-seq-palette ((3 2 1 2) (3 2 1 2))
                                 :marks (slur 1 3 pause 4)))
                            (14 ((((4 4) q q - e s s - - e e -))
                                 :pitch-seq-palette ((3 4 4 3 2 1 4))
                                 :marks (slur 3 5))))
        :rthm-seq-map '((1 ((fl (1 3 6 1 8 11 1 3 6 1 8 11 1 5 6 1 8 11 1 5 6 1
                                   3 11))
                            (ob (1 4 7 1 9 11 1 4 7 1 9 11 1 12 13 1 4 11 1 3 7
                                   2 5 11))
                            (cl (1 5 7 1 3 11 1 5 7 1 3 11 2 9 13 2 3 11 1 5 7
                                   1 14 11))
                            (bn (2 3 7 1 10 11 2 3 7 1 10 11 1 8 6 1 3 11 1 12
                                   7 1 12 11))))))))
  (re-bar o-haupt-voll-blut :start-bar 2 :min-time-sig '(4 4))
  (midi-play o-haupt-voll-blut :midi-file "/tmp/o-haupt-voll-blut.mid")
  (cmn-display o-haupt-voll-blut :file "/tmp/o-haupt-voll-blut.eps")
  (write-lp-data-for-all o-haupt-voll-blut))