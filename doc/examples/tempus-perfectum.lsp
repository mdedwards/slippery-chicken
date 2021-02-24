;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File:             tempus-perfectum.lsp
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
;;; Creation date:    3rd August 2012
;;;
;;; $$ Last modified: 17:10:18 Fri Jul 20 2012 CEST
;;;
;;; SVN ID: $Id: tempus-perfectum.lsp 3406 2013-01-28 15:13:21Z medward2 $
;;;
;;; ****
;;; Licence:          Copyright (c) 2010 Michael Edwards
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

;;; For detailed comments on the code in this file, please see:
;;; doc/manual/tempus-perfectum.html

(in-package :sc)
(in-scale :chromatic)

(let* ((num-seqs 71)
       (src-width 50)
       (sndfile-dir
        (concatenate 'string
                     cl-user::+slippery-chicken-home-dir+
                     "doc/manual/resources/"))
       (ens '(((ob (oboe :midi-channel 1))
               (cl (b-flat-clarinet :midi-channel 2))
               (bn (bassoon :midi-channel 3))
               (hn (french-horn :midi-channel 4))
               (tp (c-trumpet :midi-channel 5))
               (tb (tenor-trombone :midi-channel 6))
               (pr (piano :midi-channel 7))
               (pl (piano-lh :midi-channel 8))
               (vn (violin :midi-channel 9))
               (va (viola :midi-channel 11))
               (vc (cello :midi-channel 12))
               (c1 (computer :midi-channel 13))
               (c2 (computer :midi-channel 14)))))
       (seqs-rules (loop repeat (length (first ens))
                      with l = '(1 2 3 4 5 6 7 8)
                      for p from 0
                      with po = 1
                      with s = nil
                      do (setf s (list (nth (mod p 8) l)
                                       (nth (mod (+ p po) 8) l)
                                       (nth (mod (+ p (* 2 po)) 8) l)
                                       (nth (mod (+ p (* 3 po)) 8) l)))
                      collect (list (1+ p) s)
                      when (= 8 (first s))
                      do (setf po (1+ po))))
       (seqs (make-l-for-lookup 'l-seqs
                                '((1 ((1)))
                                  (2 ((2)))
                                  (3 ((3)))
                                  (4 ((4)))
                                  (5 ((5)))
                                  (6 ((6)))
                                  (7 ((7)))
                                  (8 ((8))))
                                seqs-rules))
       (rsm-lists 
        (loop for s from 1 to (length (first ens))
           for p in (loop for i in (first ens)
                       collect (first i))
           collect (list p (flatten (do-simple-lookup seqs s num-seqs)))))
       (sp '((1 ((cs2 f2 a2 ds2 c3 cs3 d3 gs3 c4 cs4 fs4 g4 gs4 a4 bf4 b4 d5 ds5
                      e5 f5 fs5 a5)))))
       (harms-list (loop repeat (length (second (first rsm-lists)))
                      collect 1))
       (set-lims-progs
        (loop for pchs in '((g4 ds5) (g4 fs5) (cs4 fs5) (c4 fs5) (d3 e5) 
                            (d3 e5) (d3 b4) (cs3 b4) (cs3 ds5) (cs3 ds5) 
                            (d3 b4) (cs3 a4) (cs3 e5) (d3 e5) (d3 a5) (c4 a5) 
                            (c4 e5) (c4 d5) (c4 ds5) (c4 f5) (g4 e5) (f2 ds5) 
                            (c4 e5) (a2 f5) (cs2 e5) (cs2 fs5))
           for n from 1
           collect (list n pchs)))
       (set-lims-low (loop for s in set-lims-progs 
                        collect (first s)
                        collect (first (second s))))
       (set-lims-high (loop for s in set-lims-progs 
                         collect (first s)
                         collect (second (second s))))
       (tempus-perfectum
        (make-slippery-chicken
         '+tempus-perfectum+
         :title "Tempus Perfectum"
         :ensemble ens
         :staff-groupings '(3 3 2 3 2)
         :tempo-map '((1 (q 112)))
         :set-palette sp
         :set-map (list (list 1 harms-list))
         :set-limits-high `((all ,set-lims-high)
                            (ob (0 a5 100 a5))
                            (bn (0 g4 100 g4))
                            (hn (0 c5 100 c5))
                            (tb (0 g4 100 g4))
                            (va (0 ds5 100 ds5))
                            (vc (0 g4 100 g4)))
         :set-limits-low `((all ,set-lims-low)
                           (ob (0 g4 100 g4))
                           (hn (0 c3 100 c3))
                           (tp (0 c4 100 c4))
                           (tb (0 a2 100 a2))
                           (vn (0 d4 100 d4)))
         :rthm-seq-palette '((1 ((((3 4) - e (s) s - +q { 3 (te) - te te - })
                                  ((h.))
                                  ((h.))
                                  ((h.))
                                  ((h.))
                                  ((h.)))
                                 :pitch-seq-palette ((1 3 2 3))
                                 :marks (ppp 1 pp 2 ppp 3 cresc-beg 3 cresc-end
                                             4 )))
                             (2 ((((3 4) (h.))
                                  ((h.))
                                  (- e.. 32 - { 3 - +te te te } - - +s s - (e))
                                  (- s. 32 +e - - s s - (e) (q))
                                  ((s) - s (s) s - (q) - e e -)
                                  ((h.)))
                                 :pitch-seq-palette ((5 7 9 7 6 7 6 9 5 6 3 1
                                                        2))
                                 :marks (mf 1 s 1 2 p 3 mf 7 s 7 8 ppp 9 s 12
                                            13)))
                             (3 ((((3 4) (h.))
                                  ((q) q { 3 (ts) ts (ts) } e )
                                  (- e s s - +q q)
                                  ({ 3 (ts) - ts ts - } (e) { 3 (ts) te } 
                                     { 3 - +te ts - } q) 
                                  (h.)
                                  (+q - +e s - (s) q))
                                 :pitch-seq-palette ((10 5 7 1 3 4 10 14 8 6 7
                                                         6 10 12 7))
                                 :marks (pp 1 cresc-beg 1 cresc-end 2 f 2 p 3
                                            mf 4 dim-beg 4 dim-end 9 pp 9 mf 11
                                            dim-beg 11 dim-end 15 ppp 15
                                            cresc-beg 15 cresc-end 16 f 16 s
                                            17 mf 18 te 18)))
                             (4 ((((3 4) { 5 - fe fs fs fs - } { 3 te tq } 
                                   - +e e -)
                                  ((h.))
                                  (q (q) (e) e)
                                  ((e) e (h)) 
                                  (- s s - (e) - e (s) s - - e e -)
                                  ((e.) s +h))
                                 :pitch-seq-palette ((9 7 8 6 3 1 9 9 8 8 7 8 3
                                                        8 2 1 (9)))
                                 :marks (mf 1 dim-beg 1 s 4 ppp 6 dim-end 6 mf
                                            8 at 8 cresc-beg 8 cresc-end 11 f
                                            11 s 12 p 13 a 15 s 16 at 17 mf
                                            17)))
                             (5 ((((3 4) (e) q. - +s e. -)
                                  (e (e) (s) - e s - +e (e))
                                  (e (e) (e.) s - +e. s -)
                                  (- +s e. - (e.) s (q)) 
                                  (e (e) (h))
                                  (e (e) (h)))
                                 :pitch-seq-palette ((1 2 (5) 1 3 2 3 2 1 1 1 
                                                        (5)))
                                 :marks (fff 1 dim-beg 1 mf 3 dim-end 3 ppp 4
                                             cresc-beg 8 cresc-end 9 cresc-beg 
                                             11 cresc-end 13 f 15)))
                             (6 ((((3 4) (h.))
                                  ((e) e (h))
                                  ((h.))
                                  ((h.)) 
                                  ((h.))
                                  ((q) e (e) (q)))
                                 :pitch-seq-palette ((1 (1)))
                                 :marks (p 1 f 2)))
                             (7 ((((3 4) e (e) { 3 - ts ts ts - } (e) (q))
                                  ((h.))
                                  ((h.))
                                  ((h.)) 
                                  ((e..) 32 - e e - (q))
                                  ((h.)))
                                 :pitch-seq-palette ((2 5 4 3 1 6 7))
                                 :marks (ppp 1)))
                             (8 ((((3 4) (h.))
                                  ((h.))
                                  ((h) (e..) 32)
                                  (e (e) (h)) 
                                  ((h.))
                                  (h (q)))
                                 :pitch-seq-palette ((4 1 (2)))
                                 :marks (ppp 1 ff 3))))
         :rthm-seq-map (list (list 1 rsm-lists))
         :snd-output-dir "/tmp"
         :sndfile-palette `(((vocal-sounds
                              ((voice-womanKP-18 :frequency 1028)
                               (voice-womanKP-20 :frequency 456)
                               (voice-womanKP-21 :frequency 484)
                               (voice-womanKP-22 :frequency 591)
                               (voice-womanKP-23 :frequency 662)
                               (voice-womanKP-26 :frequency 516)
                               (voice-womanKP-29 :frequency 629)))
                             (mouth-pops-clicks
                              ((mouth_pop_2 :frequency 375)
                               (mouth_pop_2a :frequency 798)
                               (mouthnoises2 :frequency 703))))
                            ,(list sndfile-dir)
                            ("wav")))))
  #+clm (clm-play tempus-perfectum 1 '(c1) 'vocal-sounds
                  :rev-amt 0.07
                  :reset-snds-each-rs nil
                  :pitch-synchronous t
                  :src-width src-width
                  :srate 44100
                  :header-type clm::mus-aiff
                  :data-format clm::mus-bshort
                  :sndfile-extension ".aiff"
                  :check-overwrite nil)
  #+clm (clm-play tempus-perfectum 1 '(c2) 'mouth-pops-clicks
                  :rev-amt 0.07
                  :reset-snds-each-rs nil
                  :pitch-synchronous t
                  :src-width src-width
                  :srate 44100
                  :header-type clm::mus-aiff
                  :data-format clm::mus-bshort
                  :sndfile-extension ".aiff"
                  :check-overwrite nil)
  (setf (staff-name (get-data-data 'pl (ensemble tempus-perfectum))) " ")
  (midi-play tempus-perfectum 
             :midi-file "/tmp/tempus-perfectum.mid"
             :voices '(ob cl bn hn tp tb pr pl vn va vc))
  #+cmn (cmn-display tempus-perfectum
                     :file "/tmp/tempus-perfectum.eps"
                     :size 11 
                     :players '(ob cl bn hn tp tb pr pl vn va vc)
                     :in-c t)
  (write-lp-data-for-all tempus-perfectum
                         :players '(ob cl bn hn tp tb pr pl vn va vc)
                         :in-c t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF tempus-perfectum.lsp
