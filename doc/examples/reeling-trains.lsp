;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File:             reeling-trains.lsp
;;;
;;; Class Hierarchy:  None
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Lisp example code to accompany trains.html
;;;
;;; Author:           Sean Reed
;;;
;;; Creation date:    1st December 2012
;;;
;;; $$ Last modified: 22:47:17 Fri May 17 2013 BST
;;;
;;; SVN ID: $Id: reeling-trains.lsp 3538 2013-05-18 08:29:15Z medward2 $
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

;; A function to place dynamics on random notes in the score
(defun place-dyns (event)
  (when (equalp (start-time event) 0.0)
    (random-rep 10 t))
  (unless (is-rest event)
    (when (> (random-rep 100) 67)
      (setf (marks event) 
            (list (nth (random-rep 8) '(ppp pp p mp mf f ff fff)))))))

;; A function to place articulations (accent or staccato) on random notes in
;; the score
(defun place-arts (event)
  (when (equalp (start-time event) 0.0)
    (random-rep 10 t))
  (unless (is-rest event)
    (when (> (random-rep 100) 67)
      (setf (marks event) 
            (list (nth (random-rep 5) '(a s)))))))

;; Disabling the chord function on the string instruments (no double-stops) 
(loop for i in '(violin viola cello)
     do
     (set-slot 'chords
               nil
               i
               +slippery-chicken-standard-instrument-palette+))

;; Changing the chord function for the piano instruments
(loop for i in '(piano piano-lh)
   do
     (set-slot 'chord-function
               'chord-fun1
               i
               +slippery-chicken-standard-instrument-palette+))

(let* ((num-seqs 137) ; The number of sequences in the rthm-chain to be made
       ;; Defining the rthm-chain object
       (rch
        (make-rthm-chain
         'test-rch 
         num-seqs
         '((((e) e) ; the 1-beat fragments: 4 in total
            (- s (s) (s) s -)
            ({ 3 (te) - te te - })
            ((e.) s))
           (({ 3 (te) te (te) }) ; what we transition to
            ({ 3 - te (te) te - })
            ({ 3 (te) - te te - })
            ({ 3 (te) (te) te }))
           (((q)) ; the second transition
            (- s e s -)
            ({ 3 te (tq) })
            (s (e.)))
           ((- e e -) ; the third transition
            (- s s s - (s))
            ((32) 32 (e.))
            ((q))))
         '((((q q) ; the 2/4 bars: 4 total
             ((q) q)
             ((q) q)
             ((q) (s) e.))
            (({ 3 te te +te te +te te }) ; what we transition to
             (q - s e. -)
             (q (s) e.)
             (q (s) - s e -))
            ((q - +e e -) ; the second transition
             ((e) e +e e)
             (q - s (e) s -)
             ((s) e. +s e.)))
           ((((e.) s (e) e (s) e.) ; the 3/4 bars: 4 total
             (- e e - (e) e (q))
             (- e. s - - +e e - (q))
             (q (e.) s (q)))
            (({ 3 (te) (te) te +te te +te } (q)) ; what we transition to
             (- e. s - (q) (s) - s e -)
             ({ 3 te te +te } (q) q)
             ({ 3 - te +te te - } (e) e { 3 (te) (te) te }))
            ((q +q +q) ; the second transition
             (q +q (q))
             (- e e - +q (q))
             (s (e.) (q) (q)))))
         ;; The probabilty of rests or sounding notes in each passage
         :activity-curve '(0 5 20 7 30 8 50 3 70 5 80 8 100 10)
         ;; The durations to be used for the inserted rests
         :rests '(s e s q)
         ;; The durations to be used for the inserted rhythms
         :sticking-rthms '(e h e. w s e h s)
         ;; The activity-curve for the sticking rhythms
         :sticking-curve '(0 2 20 3 30 5 50 8 80 10 100 2)
         ;; The number of times each sticking rhythm is repeated
         :sticking-repeats '(3 5 3 5 8)
         ;; The number of consecutive sequences that have the same
         ;; harmonic set
         :harmonic-rthm-curve '(0 1 10 2 30 3 40 1 50 2 80 4 90 5 100 1) 
         ;; The min and max beat duration of bars generated; beat taken from
         ;; the given time signature.
         :split-data '(4 7)
         ;; The IDs for the original two players
         :players '(fl cl))))
  ;; Adding new voices based on the content of the original two, these with
  ;; offsets however to avoid completely identical voices.
  (add-voice rch '(1 fl) 'ob 1)
  (add-voice rch '(1 cl) 'bn 1)
  (add-voice rch '(1 fl) 'pnr 2)
  (add-voice rch '(1 cl) 'pnl 2)
  (add-voice rch '(1 fl) 'vn 3)
  (add-voice rch '(1 fl) 'va 4)
  (add-voice rch '(1 cl) 'vc 3)
  (add-voice rch '(1 cl) 'vb 4)
  ;; Creating automatically generated pitch-seq palettes for the rthm-seqs made 
  (create-psps (palette rch))
  ;; A series of variables for the make-slippery-chicken function, starting
  ;; with the definition of the set-palette
  (let* ((sp '((1 ((c3 cs3 d3 e3 fs3 g3 c4 cs4 e4 c5 gs5 cs6 d6
                       ds6))) 
               (2 ((b2 c3 fs3 gs3 ds4 e4 d5 ds5 cs6)))
               (3 ((af2 f3 fs3 d4 e4 fs4 a4 d5 e5 c6 af6)))
               (4 ((b2 c3 df3 f3 af3 c4 e4 d5 c6)))
               (5 ((bf2 g3 e4 ef5 d6)))
               (6 ((f3 ef4 a4 d5 e5 df6)))
               (7 ((a2 b2 af3 f4 ef5 e5 c6)))
               (8 ((e4 a4 c5 g5 af5 e6)))
               (9 ((e4 df5 bf5)))
               (10 ((b1 a2 g3 e4 df5 a5)))
               (11 ((ef3 bf3 e4 b4 d5 f5 c6 ef6)))
               (12 ((f3 df4 e4 b4 e5 f5 c6 g6)))
               (13 ((e4 b4 df5)))
               (14 ((d4 ef4 bf4)))
               (15 ((df4 bf4 a5 fs6)))
               (16 ((d4 ef4 f4 g4)))
               (17 ((g1 bf1 fs2 af2 f3 fs3 e4)))
               (18 ((ef1 df2 b2 a3 e4 f4 fs4 d5 a5 bf5)))
               (19 ((e4 f4 g4 b4 c5 g5 af5 e6 f6)))
               (20 ((bf3 b3 e4 f4 bf4)))
               (21 ((e3 f3 df4 d4 ds4 f4 df5 d5 b5)))
               (22 ((d3 g3 a3 e4 af4 ef5 df5)))
               (23 ((g4 af4 df5 d5 af5 df6 d6)))
               (24 ((af3 e4 a4 e5 bf5)))
               (25 ((df2 a2 e3 ef4 d5 af5)))
               (26 ((b2 ef3 df4 e4 c5 g5 ef6)))
               (27 ((d1 a1 ef2 c3 a3 f4)))
               (28 ((df3 g3 c4 df4 d4 e4)))
               (29 ((c3 b3 e4 df5 b5)))
               (30 ((ds1 ef2 b2 c3 g3 af3 d4 e4 b4 fs5 d6)))
               (31 ((d1 ef2 a2 ef3 bf3 e4 d5 bf5 g6)))))
         ;; Creating the set-palette using the procession algorithm
         (sm (procession (num-rthm-seqs rch) (length sp)))
         ;; Determining the high and low pitches for each instrument for each
         ;; harmonic set. Every time the same set occurs, the same instruments
         ;; will have the same limits.
         (sls '((fl ((1 gs5 c7) (2 d5 c7) (3 e5 c7) (4 d5 c7) (5 ef5 c7) 
                     (6 e5 c7) (7 ef5 c7) (8 g5 c7) (9 df5 c7) (10 df5 c7) 
                     (11 d5 c7) (12 e5 c7) (13 df5 c7) (14 bf4 c7) (15 a5 c7) 
                     (16 g4 c7) (17 e4 c7) (18 d5 c7) (19 g5 c7) (20 bf4 c7) 
                     (21 df5 c7) (22 ef5 c7) (23 af5 c7) (24 e5 c7)
                     (25 d5 c7) (26 g5 c7) (27 f4 c7) (28 e4 c7) (29 df5 c7) 
                     (30 fs5 c7) (31 d5 c7)))
                (ob ((1 g4 a5) (2 g4 a5) (3 g4 a5) (4 g4 a5) (5 g4 a5) 
                     (6 g4 a5) (7 g4 a5) (8 g4 a5) (9 g4 a5) (10 g4 a5) 
                     (11 g4 a5) (12 g4 a5) (13 g4 a5) (14 g4 a5) (15 g4 a5) 
                     (16 g4 a5) (17 e4 a5) (18 g4 a5) (19 g4 a5) (20 g4 a5) 
                     (21 g4 a5) (22 g4 a5) (23 g4 a5) (24 g4 a5) (25 g4 a5)
                     (26 g4 a5) (27 f4 a5) (28 e4 a5) (29 g4 a5) (30 g4 a5) 
                     (31 g4 a5)))
                (cl ((1 b4 c6) (2 b4 c6) (3 b4 c6) (4 b4 c6) (5 b4 c6) 
                     (6 b4 c6) (7 b4 c6) (8 b4 c6) (9 b4 c6) (10 b4 c6) 
                     (11 b4 c6) (12 b4 c6) (13 b4 c6) (14 bf4 c6) (15 b4 c6) 
                     (16 g4 c6) (17 e4 c6) (18 b4 c6) (19 b4 c6) (20 bf4 c6) 
                     (21 b4 c6) (22 b4 c6) (23 b4 c6) (24 b4 c6) (25 b4 c6)
                     (26 b4 c6) (27 f4 c6) (28 e4 c6) (29 b4 c6) (30 b4 c6) 
                     (31 b4 c6)))
                (bn ((1 a2 d4) (2 a2 d4) (3 a2 d4) (4 a2 d4) (5 a2 d4) 
                     (6 a2 d4) (7 a2 d4) (8 a2 e4) (9 a2 e4) (10 a2 d4) 
                     (11 a2 d4) (12 a2 d4) (13 a2 e4) (14 a2 d4) (15 a2 d4) 
                     (16 a2 d4) (17 a2 d4) (18 a2 d4) (19 a2 e4) (20 a2 d4) 
                     (21 a2 d4) (22 a2 d4) (23 a2 g4) (24 a2 d4) (25 a2 d4)
                     (26 a2 d4) (27 a2 d4) (28 a2 d4) (29 a2 d4) (30 a2 d4) 
                     (31 a2 d4)))
                (vb ((1 c4 f6) (2 c4 f6) (3 c4 f6) (4 c4 f6) (5 c4 f6) 
                     (6 c4 f6) (7 c4 f6) (8 c4 f6) (9 c4 f6) (10 c4 f6) 
                     (11 c4 f6) (12 c4 f6) (13 c4 f6) (14 c4 f6) (15 c4 f6) 
                     (16 c4 f6) (17 c4 f6) (18 c4 f6) (19 c4 f6) (20 c4 f6) 
                     (21 c4 f6) (22 c4 f6) (23 c4 f6) (24 c4 f6) (25 c4 f6) 
                     (26 c4 f6) (27 c4 f6) (28 c4 f6) (29 c4 f6) (30 c4 f6) 
                     (31 c4 f6)))
                (pnr ((1 c4 c8) (2 c4 c8) (3 c4 c8) (4 c4 c8) (5 c4 c8) 
                      (6 c4 c8) (7 c4 c8) (8 g5 c8) (9 df5 c8) (10 c4 c8) 
                      (11 c4 c8) (12 c4 c8) (13 b4 c8) (14 ef4 c8) (15 a5 c8)
                      (16 f4 c8) (17 f3 c8) (18 c4 c8) (19 c5 c8) 
                      (20 e4 c8) (21 df5 c8) (22 c4 c8) (23 af5 c8) (24 a4 c8) 
                      (25 c4 c8) (26 c4 c8) (27 a3 c8) (28 c4 c8) (29 c4 c8) 
                      (30 c4 c8) (31 c4 c8)))
                (pnl ((1 a0 b3) (2 a0 b3) (3 a0 b3) (4 a0 b3) (5 a0 b3) 
                      (6 a0 b3) (7 a0 b3) (8 a0 c5) (9 a0 e4) (10 a0 b3) 
                      (11 a0 b3) (12 a0 b3) (13 a0 e4) (14 a0 d4) 
                      (15 a0 bf4) (16 a0 ef4) (17 a0 af2) (18 a0 b3) 
                      (19 a0 b4) (20 a0 b3) (21 a0 ds4) (22 a0 b3) (23 a0 d5)
                      (24 a0 e4) (25 a0 b3) (26 a0 b3) (27 a0 c3) (28 a0 b3) 
                      (29 a0 b3) (30 a0 b3) (31 a0 b3))) 
                (vn ((1 a4 c7) (2 a4 c7) (3 a4 c7) (4 a4 c7) (5 a4 c7) 
                     (6 a4 c7) (7 a4 c7) (8 a4 c7) (9 a4 c7) (10 a4 c7) 
                     (11 a4 c7) (12 a4 c7) (13 a4 c7) (14 a4 c7) (15 a4 c7) 
                     (16 f4 c7) (17 e4 c7) (18 a4 c7) (19 a4 c7) (20 f4 c7) 
                     (21 a4 c7) (22 a4 c7) (23 a4 c7) (24 a4 c7) (25 a4 c7) 
                     (26 a4 c7) (27 a3 c7) (28 d4 c7) (29 a4 c7) (30 a4 c7) 
                     (31 a4 c7)))
                (va ((1 g3 gs4) (2 g3 gs4) (3 g3 gs4) (4 g3 gs4) (5 g3 gs4) 
                     (6 g3 gs4) (7 g3 gs4) (8 g3 gs4) (9 g3 gs4) 
                     (10 g3 gs4) (11 g3 gs4) (12 g3 gs4) (13 g3 gs4)
                     (14 g3 gs4) (15 g3 gs4) (16 g3 gs4) (17 g3 gs4) 
                     (18 g3 gs4) (19 g3 gs4) (20 g3 gs4) (21 g3 gs4) 
                     (22 g3 gs4) (23 g3 gs4) (24 g3 gs4) (25 g3 gs4) 
                     (26 g3 gs4) (27 g3 gs4) (28 g3 gs4) (29 g3 gs4) 
                     (30 g3 gs4) (31 g3 gs4))) 
                (vc ((1 c2 fs3) (2 c2 fs3) (3 c2 fs3) (4 c2 fs3) (5 c2 fs3) 
                     (6 c2 fs3) (7 c2 fs3) (8 c2 e4) (9 c2 e4) 
                     (10 c2 fs3) (11 c2 fs3) (12 c2 fs3) (13 c2 e4)
                     (14 c2 d4) (15 c2 df4) (16 c2 d4) (17 c2 fs3) 
                     (18 c2 fs3) (19 c2 e4) (20 c2 b3) (21 c2 fs3) 
                     (22 c2 fs3) (23 c2 g4) (24 c2 af3) (25 c2 fs3) 
                     (26 c2 fs3) (27 c2 fs3) (28 c2 fs3) (29 c2 fs3) 
                     (30 c2 fs3) (31 c2 fs3)))))
         ;; Creating a list for just the low set-limits, with consecutive
         ;; integers as x-values so there are equal number of breakpoint pairs
         ;; as sets in the set-palette
         (sll (loop for p in sls
                 collect
                   (list (first p)
                         (loop for s in sm
                            for i from 1
                            collect i
                            collect (second (nth (1- s) (second p)))))))
         ;; Creating a list for just the high set-limits, with consecutive
         ;; integers as x-values so there are equal number of breakpoint pairs
         ;; as sets in the set-palette
         (slh (loop for p in sls
                 collect
                   (list (first p)
                         (loop for s in sm
                            for i from 1
                            collect i
                            collect (third (nth (1- s) (second p)))))))
         ;; Creating the sc object
         (reeling-trains
          (make-slippery-chicken
           '+reeling-trains+
           :title "reeling trains"
           :ensemble '(((fl (flute :midi-channel 1))
                        (ob (oboe :midi-channel 2))
                        (cl (b-flat-clarinet :midi-channel 3))
                        (bn (bassoon :midi-channel 4))
                        (vb (vibraphone :midi-channel 5))
                        (pnr (piano :midi-channel 6))
                        (pnl (piano-lh :midi-channel 7))
                        (vn (violin :midi-channel 8))
                        (va (viola :midi-channel 9))
                        (vc (cello :midi-channel 11))))
           :staff-groupings '(4 1 2 3)
           :set-palette sp
           :set-map `((1 ,sm))
           :set-limits-low sll 
           :set-limits-high slh
           :tempo-map '((1 (q 72)))
           ;; The rs palette and map can be taken directly from the rthm-chain
           ;; object. 
           :rthm-seq-palette (palette rch)
           :rthm-seq-map rch)))
    ;; add random dynamics
    (process-events-by-time reeling-trains #'place-dyns)
    ;; add random articulations
    (process-events-by-time reeling-trains #'place-arts)
    ;; remove repeated dynamics
    (remove-extraneous-dynamics reeling-trains)
    ;; remove specific marks
    (loop for pm in '((1 1 (cl bn vb pnr pnl vc) s) (2 1 vc ppp) 
                      (2 2 (vn vc) s) (2 3 vc ff) (3 1 va p) (3 2 (ob va) s) 
                      (3 2 vc fff) (5 3 ob f) (5 2 vn mf) (6 3 bn p) (8 4 ob s) 
                      (9 1 fl s) (12 1 vn s) (17 1 ob ff) (17 2 fl ppp)
                      (19 3 vc mf) (20 2 cl s) (26 2 vn s) (26 2 va p) 
                      (27 1 bn p) (27 2 va s) (28 3 ob ff) (35 1 vn s)
                      (38 2 bn fff) (42 2 vc f) (44 1 bn s) (47 2 ob mp)
                      (47 3 bn ppp) (51 1 pnr s) (53 1 (fl vb) s) 
                      (53 2 (vb vc) s) (53 2 pnl mf) (54 2 vn s) (58 1 vc pp) 
                      (61 1 bn s) (61 4 (ob pnr va) (mf fff ppp)) (62 1 ob s) 
                      (62 2 va s) (63 2 va s) (65 1 pnr s) (66 1 ob pp)
                      (66 3 ob s) (66 3 fl s) (70 2 fl mf) (72 1 va p) 
                      (73 2 vn s) (74 2 vn fff) (75 3 cl pp) (76 2 ob s)
                      (77 2 ob s) (85 1 bn s) (85 3 vb ff) (85 4 bn s)
                      (85 5 cl p) (85 5 bn a) (85 5 vc a) 
                      (86 3 (bn vc) (pp fff)) (86 4 vc s) (86 5 vb f) 
                      (87 2 ob s) (90 2 vb mf) (90 3 bn ff) (96 2 ob pp) 
                      (97 1 fl pp) (98 4 vn mf) (100 1 ob mf) (101 1 fl p)
                      (101 1 vn s) (101 2 vn p) (102 1 vn s) (103 1 ob s)
                      (103 1 vc mf) (105 1 vn s) (105 1 vb mp) (106 2 va fff)
                      (106 2 vc s) (106 3 vb s) (106 3 vn f) (106 5 vb ff)
                      (106 5 pnl ff) (106 5 vc p) (110 2 vn s) (110 3 va s)
                      (110 3 vn mp) (110 2 va ppp) (111 2 ob fff) 
                      (114 2 vn fff) (114 2 ob mp) (114 3 pnr ppp) 
                      (116 3 vb mp) (118 2 pnr s) (118 2 vn mf) (120 2 va s)
                      (120 2 cl pp) (120 3 pnr mp) (125 3 fl f) (125 3 va p)
                      (125 3 va mp) (131 2 vn mf) (132 1 fl mp) (134 1 va ppp)
                      (137 1 vn s) (137 2 fl fff) (138 2 pnr p) (139 1 va mp)
                      (139 3 vn p) (142 2 vn s) (143 1 vn f) (144 1 pnr ff)
                      (151 3 ob f) (153 1 bn s) (156 1 pnl s) (156 2 bn fff)
                      (156 3 vb pp) (156 3 pnl fff) (156 3 va f) (156 5 vc a)
                      (156 6 vb mf) (158 3 pnr ff) (163 2 fl p) (163 2 vn mp)
                      (163 3 ob fff) (166 1 (fl va) (p mf)) (166 2 ob p) 
                      (167 1 cl f) (169 3 ob mp) (170 2 vn mf) (172 3 pnr s)
                      (172 3 vn pp) (173 1 pnr s) (173 1 vb ff) (173 3 vn mp)
                      (175 4 pnr mf) (175 4 vn ff) (175 4 va p) (176 2 cl s)
                      (178 2 pnr pp) (178 2 va ff))
       do 
         (rm-marks-from-notes reeling-trains 
                              (first pm) 
                              (second pm) 
                              (third pm)
                              (fourth pm)))
    ;; add diminuendo marks
    (loop for pdcr in '((pnr 2 2 4 1) (pnl 2 4 4 1) (vn 2 2 2 3) (bn 5 1 5 2)
                        (bn 14 1 14 2) (fl 17 1 18 1) (ob 26 1 27 2) 
                        (fl 26 2 27 2) (ob 54 1 54 3) (vn 81 1 81 2) 
                        (ob 103 1 103 2) (bn 102 2 103 2) (vb 102 2 103 2) 
                        (va 107 1 107 2) (va 118 1 118 3) (va 169 1 169 3) 
                        (vn 172 2 172 4) (va 176 1 176 2)) 
       do
         (add-mark-to-note reeling-trains
                           (second pdcr)
                           (third pdcr)
                           (first pdcr)
                           'dim-beg)
         (add-mark-to-note reeling-trains
                           (fourth pdcr)
                           (fifth pdcr)
                           (first pdcr)
                           'dim-end))
    ;; add crescendo marks
    (loop for pdcr in '((va 2 2 4 3) (va 10 1 10 3) (cl 14 1 14 2) 
                        (va 17 2 18 1) (va 26 1 27 1) (fl 35 1 35 3) 
                        (ob 48 1 48 3) (vb 53 1 53 3) (pnl 54 1 54 2) 
                        (pnr 57 1 57 3) (fl 65 2 66 2) (bn 67 1 67 2) 
                        (va 66 3 67 1) (fl 77 3 78 1) (va 83 1 83 2) 
                        (fl 93 1 94 1) (va 93 2 94 1) (va 98 3 98 5) 
                        (pnr 106 1 106 3) (ob 110 1 110 3) (fl 142 2 143 2) 
                        (cl 145 1 145 3) (cl 164 1 164 2) (vb 170 1 170 2)
                        (vn 175 3 175 5) (vc 176 1 176 2))
       do
         (add-mark-to-note reeling-trains
                           (second pdcr)
                           (third pdcr)
                           (first pdcr)
                           'cresc-beg)
         (add-mark-to-note reeling-trains
                           (fourth pdcr)
                           (fifth pdcr)
                           (first pdcr)
                           'cresc-end))
    ;; consolidate sounding durations
    (map-over-bars reeling-trains 1 nil nil #'consolidate-notes)
    ;; make the first dynamic forte
    (loop for p in '(fl ob cl bn vb pnr pnl vn va vc)
       do
         (add-mark-to-note reeling-trains 1 1 p 'f))
    ;; auto-slur
    (auto-slur reeling-trains '(fl ob cl bn vb pnr pnl vn va vc) 
               :over-accents nil
               :rm-staccatos nil)
    ;; output
    (midi-play reeling-trains :midi-file "/tmp/reeling-trains.mid")
    (cmn-display reeling-trains 
                 :file "/tmp/reeling-trains.eps"
                 :size 14
                 :auto-clefs nil
                 :in-c t)
    (write-lp-data-for-all reeling-trains
                           :auto-clefs nil
                           :in-c t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF
