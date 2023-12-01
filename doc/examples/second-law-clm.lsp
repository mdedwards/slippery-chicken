;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File:             second-law-clm.lsp
;;;
;;; Class Hierarchy:  None
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          This file contains the data associated with the slippery
;;;                   chicken User Guide tutorial for Intra-Phrasal Looping.
;;;
;;; Author:           Sean Reed
;;;
;;; Creation date:    23rd November 2011
;;;
;;; $$ Last modified:  14:42:31 Fri Dec  1 2023 CET
;;;
;;; SVN ID: $Id: second-law-clm.lsp 3565 2013-05-28 20:21:46Z medward2 $
;;;
;;; ****
;;; Licence:          Copyright (c) 2010 Michael Edwards
;;;
;;;                   This file is part of slippery-chicken
;;;
;;;                   slippery-chicken is free software; you can redistribute it
;;;                   and/or modify it under the terms of the GNU General
;;;                   Public License as published by the Free Software
;;;                   Foundation; either version 2 of the License, or (at your
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

;;; For detailed comments on the code in this file, please see:
;;; doc/manual/second-law-clm.html

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; THIS IS THE SAME CODE AS THE SECOND-LAW.LSP, EXPANDED TO INCLUDE CLM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :sc)

(in-scale :chromatic)

(let* ((sndfiles-dir-1 
        (concatenate 'string 
                     cl-user::+slippery-chicken-home-dir+ 
                     "doc/manual/resources/"))
       (sl-rsp-orig ; defining the source material
        (make-rsp
         'sl-rsp
         '((1 
            ;; A backslash (\) must be used as an escape character when
            ;; indicating a tie to the previous note using a + symbol before a
            ;; numerically represented rhythmic value (as opposed to a letter,
            ;; such as 's or 'e). (This is because Lisp will otherwise read
            ;; "+32" as "positive 32".)
            ((((4 4) - (e.) s - - \+32 32 32 32 (e) - 
               - 32 32 (s) 32 32 32 (32) - - (s) s s (s) - )
              (- (e..) 32 - +q q (q))
              (h. (q)))
             :pitch-seq-palette ((1 2 1 2 3 2 3 3 1 2 5 5 7 6)
                                 (2 1 2 1 2 3 2 2 4 3 2 2 1 2)
                                 (5 3 4 5 4 5 4 5 2 3 6 6 8 7))
             :marks (a 1 s 1 slur 3 5 a 6 slur 6 7 slur 8 10 a 11 s 11 12 
                       a 13))))))
       ;; fragmenting the source material
       (sl-rsp-chopped (chop sl-rsp-orig 
                             '((1 2) (1 1) (2 2)) ; chop points
                             'e)) ; chopping unit
       ;; setting the measure structure
       (num-seqs-list '(53 61 97 79 89 73)))
  ;; adjusting the instrument attributes
  (loop for i in 
       '((flute 13)
         (oboe 7)
         (b-flat-clarinet 9)
         (bassoon 7)
         (french-horn 5)
         (b-flat-trumpet 7)
         (tenor-trombone 5)
         (double-bass 5))
     do
       (set-slot 'largest-fast-leap 
                 (second i)
                 (first i)
                 +slippery-chicken-standard-instrument-palette+))
  ;; This function prints the results of the chop method in easy-to-read form 
  ;; (print-simple sl-rsp-chopped)
  ;; calling slippery-chicken
  (make-slippery-chicken
   '+second-law+
   :title "Second Law"
   :instrument-palette +slippery-chicken-standard-instrument-palette+
   :ensemble '(((fl (flute :midi-channel 1))
                (ob (oboe :midi-channel 2))
                (cl (b-flat-clarinet :midi-channel 3))
                (bn (bassoon :midi-channel 4))
                (hn (french-horn :midi-channel 5))
                (tp (b-flat-trumpet :midi-channel 6))
                (tb (tenor-trombone :midi-channel 7))
                (vno (violin :midi-channel 8))
                (vnt (violin :midi-channel 9))
                (va (viola :midi-channel 12))
                (vc (cello :midi-channel 13))
                (cb (double-bass :midi-channel 14))))
   ;; setting pitch ranges for the scope of this composition
   :set-limits-high '((cl (0 c6 100 c6))
                      (vc (0 a4 100 a4))
                      (cb (0 f3 100 f3)))
   :staff-groupings '(4 3 5)
   :tempo-map '((1 (q 69)))
   :set-palette '((1 ((c3 g3 cs4 e4 fs4 a4 bf4 c5 d5 f5 gf5 af5 ef6)))
                  (2 ((c3 fs3 cs4 e4 g4 a4 b4 c5 df5 f5 g5 af5 ef6)))
                  (3 ((d3 f3 cs4 e4 fs4 a4 b4 c5 d5 e5 fs5 af5 ef6)))
                  (4 ((d3 e3 cs4 ef4 fs4 a4 b4 c5 d5 e5 fs5 af5 d6)))
                  (5 ((d3 e3 g3 a3 c4 ef4 f4 af4 bf4 cs5 fs5 b5 df6)))
                  (6 ((c3 d3 gf3 af3 b3 e4 a4 df5 ef5 g5 bf5 df6)))
                  (7 ((b2 e3 fs3 as3 ef4 g4 a4 d5 f5 af5 c6 df6)))
                  (8 ((af2 b2 ef3 fs3 as3 cs4 e4 g4 a4 d5 f5 bf5 c6 e6 af6))) 
                  (9 ((af2 b2 ef3 fs3 bf3 d4 f4 a4 cs5 e5 g5 c6 f5 af6)))
                  (10 ((af2 c3 ef3 fs3 bf4 d4 f4 a4 cs5 e5 g5 b5 fs6))))
   ;; using a Lisp routine to generate the set-map
   :set-map (loop for section in 
                 '((1 (1 2 3)) 
                   (2 (2 3 4 1))
                   (3 (1 3 5 6 7))
                   (4 (8 9))
                   (5 (5 6 7 9 3))
                   (6 (9 10)))
               collect
                 (list (first section)
                       (fibonacci-transitions 
                        (nth (1- (first section)) num-seqs-list)
                        (second section))))
   :rthm-seq-palette sl-rsp-chopped
   ;; using a Lisp routine to generate the rthm-seq-map
   :rthm-seq-map (loop for section in
                      '((((1 3) fl ob ))
                        (((3 4) fl ob cl))
                        (((5 6 7 8) fl ob cl)
                         ((11 12 13 14) bn tb vc cb))
                        (((9 10 11) hn tp))
                        (((15 16 25 26) fl ob vno vnt)
                         ((9 10 13 14) cl hn va)
                         ((3 1 16 3) tp tb)
                         ((12 13 10 11) bn vc cb))
                        (((1 3 4) fl ob cl bn hn tp tb vno vnt va vc cb)))
                    for section-num from 1
                    collect 
                      (list section-num
                            (loop for ins-group in section 
                               appending
                                 (loop with fts =
                                      (loop for ch in (first ins-group) 
                                         collect
                                           (list 1 ch))
                                    for ins in (rest ins-group) 
                                    collect
                                      (list ins
                                            (fibonacci-transitions
                                             (nth (1- section-num)
                                                  num-seqs-list) 
                                             fts))))))
   :snd-output-dir "/tmp/"
   :sndfile-palette `(((source-sndfile-grp-1
                        ((test-sndfile-2.aiff :start 0.000 :end 0.504)
                         (test-sndfile-2.aiff :start 0.504 :end 0.884)
                         (test-sndfile-2.aiff :start 0.884 :end 1.608)))
                       (source-sndfile-grp-2
                        ((test-sndfile-3.aiff 
                          :start 0.035 :end 0.426 :frequency 664)
                         (test-sndfile-3.aiff 
                          :start 0.426 :end 0.682 :frequency 664)
                         (test-sndfile-3.aiff 
                          :start 0.682 :end 2.200 :frequency 664))))
                      ,(list sndfiles-dir-1))))

;; The re-bar method can be applied outside of the above "let*" scope, since
;; the slippery-chicken class produces a global variable from its first
;; argument

(re-bar +second-law+ :min-time-sig '(4 4) :auto-beam 'q)

;;; the output

;;; midi
(midi-play +second-law+ :midi-file "/tmp/second-law.mid")

;; CMN
(cmn-display +second-law+ :file "/tmp/second-law.eps" :size 12)

;; LP
(write-lp-data-for-all +second-law+ :base-path "/tmp/")

(clm-play +second-law+ 1 nil 'source-sndfile-grp-1
          :check-overwrite nil
          :rev-amt 0.1
          :src-width 5)

(clm-play +second-law+ 1 nil 'source-sndfile-grp-2
          :check-overwrite nil
          :reset-snds-each-rs nil
          :reset-snds-each-player nil
          :rev-amt 0.1
          :src-width 5)

(reaper-play +second-law+ 1 nil 'source-sndfile-grp-1
             :check-overwrite nil :tracks-per-player 2
             :pitch-synchronous t :do-src 'transposition)

(reaper-play +second-law+ 1 nil 'source-sndfile-grp-1
             :check-overwrite nil :tracks-per-player 2
             :pitch-synchronous t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF
