;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File:             slippery.lsp
;;;
;;; Class Hierarchy:  None
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          This file contains the original data used in the first
;;;                   piece of music which ever used slippery chicken
;;;                   algorithms, "slippery when wet". Written in 1999 to 2000,
;;;                   the software was not then at a stage to generate scores.
;;;                   It was mainly used to organise data and write sound files
;;;                   using pitch and rhythm data from the instrumental
;;;                   score. It's fun (for me at least) to see what happens
;;;                   when we generate the score from the original data but
;;;                   with the now complete pitch selection algorithms, amongst
;;;                   others. Clearly this only partially resembles my
;;;                   original piece but it is nevertheless instructional in
;;;                   that it uses simple data entry (as opposed to more
;;;                   complex algorithms) to provide the raw material.
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    23rd November 2011
;;;
;;; $$ Last modified:  12:06:50 Tue Jan 20 2026 CET
;;;
;;; SVN ID: $Id: slippery.lsp 5627 2016-03-23 14:37:22Z medward2 $
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

(in-package :sc)

(in-scale :quarter-tone)

(make-slippery-chicken 
 '+slippery-when-wet+ ;; creates a global variable out of this symbol
 :composer "Michael Edwards"
 :title "slippery when wet"
 ;; this is the default actually so not necessary to specify but you might want
 ;; to evaluate it to see what's in there (or check instruments.lsp)
 :instrument-palette +slippery-chicken-standard-instrument-palette+
 :tempo-map '((1 (q 48)))
 ;; 3 bars per system/page
 :bars-per-system-map '((1 3))          ; for cmn only
 :instruments-hierarchy '(solo perc fl vln cl vc hn vla)
 ;; remember players are different to instruments and microtones are written on
 ;; a separate channel so that we can have microtonal chords on e.g. strings.
 :ensemble '(((fl ((alto-flute piccolo) ; the flautist doubles
                   :midi-channel 1 :microtones-midi-channel 2))
              ;; e.g. cl is the freely-named player and it plays the two
              ;; clarinets that are defined in our instrument palette (so we
              ;; have to use their names).
              (cl ((b-flat-clarinet bass-clarinet)
                   :midi-channel 3 :microtones-midi-channel 4))
              (hn (french-horn :midi-channel 5 :microtones-midi-channel 6))
              (perc (marimba :midi-channel 7))
              ;; have solo and ensemble violin on same midi channels to save
              ;; sample space in kontakt player (but change them for importing
              ;; into Sibelius/Finale) 
              (solo (violin :midi-channel 11 :microtones-midi-channel 12))
              ;; NB channel 10 reserved for percussion
              (vln (violin :midi-channel 11 :microtones-midi-channel 12))
              (vla (viola :midi-channel 13 :microtones-midi-channel 14))
              (vc (cello :midi-channel 15 :microtones-midi-channel 16))))
 ;; the three wind instruments are grouped with a brace, as are the strings.
 :staff-groupings '(3 1 1 3)
 ;; for demonstration purposes just change instruments about halfway through.
 :instrument-change-map '((1 ((cl ((1 bass-clarinet)))
                              (fl ((1 alto-flute)))))
                          ;; (7 ((cl ((2 b-flat-clarinet)))
                          ;; (fl ((2 piccolo))))))
                          (6 ((cl ((1 b-flat-clarinet)))))
                          (7 ((fl ((2 piccolo))))))
 ;; simple lists of (microtonal) pitches that span the range.
 :set-palette '((1 ((c2 fs2 cs3 gqs3 b3 d4 g4 bf4 e5 af5 dqs6 ef6 a6 f7)))
                (2 ((cs2 gs2 fs3 a3 ef4 f4 aqs4 bqf4 d5 e5 g5 bf5 c6 b6)))
                (3 ((a2 dqf3 f3 b3 df4 fs4 g4 c5 d5 ef5 gqf5 bf5 e6 af6)))
                (4 ((ds2 bqf2 c3 fs3 a3 cs4 e4 gs4 as4 d5 f5 b5 eqf6 g6)))
                (5 ((f2 g2 b2 ef3 aqs3 bf3 cs4 e4 a4 c5 gs5 cqs6 fs6 d7)))
                (6 ((b1 d2 af2 f3 g3 bf3 e4 fs4 c5 fqs5 a5 ef6 aqs6 df7)))
                (7 ((bf1 af2 df3 g3 a3 cqs4 d4 e4 fs4 bqf4 c5 f5 b5 ds6))))
 ;; I've lost the code for the set mapping so just cycle through them.  There
 ;; are twelve sections.  That has to match the rthm-seq-map both in terms of
 ;; number (and id) of sections, and number of rthm-seqs in each section (the
 ;; latter can change from section to section but the two maps have to be
 ;; consistent) 
 :set-map '((1 (1 2 3 4 5 6 7))
            (2 (1 2 3 4 5 6 7))
            (3 (1 2 3 4 5 6 7))
            (4 (1 2 3 4 5 6 7))
            (5 (1 2 3 4 5 6 7))
            (6 (1 2 3 4 5 6 7))
            (7 (1 2 3 4 5 6 7))
            (8 (1 2 3 4 5 6 7))
            (9 (1 2 3 4 5 6 7))
            (10 (1 2 3 4 5 6 7))
            (11 (1 2 3 4 5 6 7))
            (12 (1 2 3 4 5 6 7)))
 ;; each rthm-seq is given an id (here simple integers). Each bar is in a list.
 ;; see rthm-seq-bar.lsp parse-rhythms or doc/manual/rhythms.html for syntax
 ;; details.
 :rthm-seq-palette
 '((1 ((((2 4) (s) e s (e) e)
        ((q) q)
        ({ 5 - +fe f32 f32 (fs) f32 f32 - }
           { 5 (fs) - f32 f32 - (fs) (fe) }))
       ;; there can be as many of these lists as you like
       ;; numbers in () indicate chords
       :pitch-seq-palette (9 9 9 (3) 9 9 (3) 5 9 5)))
   (2 ((((2 4) - 32 32 (32) 32 (32) 32 (32) 32 -
         (32) - 32 (32) 32 - (e))
        ((q) q) (- \+32 32 (s) 32 32 (32) 32 - (s) e 32 (32)))
       :pitch-seq-palette (8 9 8 9 5 9 9 5 6 6 8 6 9 6)))
   (3 ((((2 4) (s) s (e) s (e) s )
        ((h))
        (- e. 32 - (32) (s) - 32 (s) 32 (32) 32 -))
       :pitch-seq-palette (9 9 9 (3) 9 9 (3) 5)))
   (4 ((((2 4) { 3 (te) { 3 (18) 36 } { 3 - 36 36 36 - } }
         - +s+32 - (32) (e))
        ({ 9 (18) - 36 36 36 36 36 36 36 - } (s) e.)
        ({ 3 - +t32 t32 t32 t32 t32 t32 ts { 3 36 18  - } }
           (e) 32 (s.)))
       :pitch-seq-palette (1 2 5 5 5 5 5 5 5 5 4 5 2 1 5 5 5 4 5 2 1)))
   (5 ((((2 4) { 3 (t32) - t32 t32 t32 t32 t32 t32 t32 - (te) } (s) e.)
        (s (e) s (s.) - 32 (s) s -)
        ({ 3 (te) - t32 t32 t32 t32 (ts) ts - }
           { 3 tq - t32 t32 t32 t32 - }))
       :pitch-seq-palette (2 1 5 1 5 1 6 5 1 5 2 5 1 2 6 5 1 5 2 5 1 2)))
   (6 ((((2 4) { 3 (ts) - ts ts } { 3 (ts) ts - (ts) }
         { 3 (ts) - ts (ts) } { 3 ts ts ts - })
        ((e.) s { 3 - +te te te - })
        (- \+32 32 (32) 32 (32) 32 - (s) (q)))
       :pitch-seq-palette (1 2 5 5 5 5 5 5 5 4 5 2 1)))
   (7 ((((2 4) h) (h) (+q - 64 64 64 64 - (e.)))
       :pitch-seq-palette (3 (3) 4 (3) 1 5)))
   (8 ((((2 4) (q) q) (- +e. 32 - (32) (q))
        ({ 3 (te) - ts } { 3 (ts) ts ts - }
           { 3 (ts) - ts (ts) } { 3 ts (ts) ts - }))
       :pitch-seq-palette (10 3 9 3 8 3 7 4)))
   (9 ((((2 4) (s) - s s s - - s s e -)
        ({ 3 - t32 t32 (t32) t32 - (tq) } (q))
        ((e.) s - s (s) 32 32 (32) 32 -))
       :pitch-seq-palette (3 5 8 2 8 9 4 11 8 2 8 9 4 12)))
   (10 ((((2 4) (e) s (s) - 32 (32) 32 32 - (e))
         ({ 3 (te) - t32 t32 t32 t32 - - t32 t32 - (ts) } (q))
         ((e) - 32 32 (32) 32 - (s) s (e)))
        :pitch-seq-palette (4 (5) (5) 3 (6) (6) (5) (5) 6 (5) 8 8 9 8)))
   (11 ((((2 4) { 3 (ts) - ts (ts) } { 3 ts (ts) ts - }
          { 3 (ts) - ts ts } { 3 (ts) ts - (ts) })
         (- 32 (32) 32 32 (32) 32 - (s) (e) - 32 32 (32) 32 -)
         ({ 3 (ts) - te } { 3 +ts - (te) }
            { 3 - ts ts ts } { 3 (ts) ts - (ts) }))
        :pitch-seq-palette (3 8 (3) 9 (3) 8 (7) (7) (7) 4 8 9 9 9 (3) 9 9 (3))))
   (12 ((((2 4) (e.) s (e) { 3 - ts ts ts - })
         ({ 3 (ts) - ts (ts) } { 3 ts (ts) ts - }
            { 3 (ts) - ts ts } { 3 (ts) te - })
         ((e.) s q))
        :pitch-seq-palette (9 3 9 5 10 6 11 8 4 10 2 8)))
   (13 ((((2 4) { 3 - t32 t32 t32 t32 t32 t32 t32 t32 t32 t32 - (ts) }
          (q))
         ({ 3 - ts ts ts } { 3 (ts) ts - (ts) } (e) { 3 - ts ts ts - })
         ((e) - s s - - s s 32 32 (32) 32 -))
        :pitch-seq-palette (4 (5) (5) 3 (6) (6) (7) (7) 4 (9) (9) (3) 8 (3) 9
                              (3) 8 (5) (5) 6 (5) 8 3 8)))
   (14 ((((2 4) (q) (s) - 32 32 - (e))
         ((e) - 32 32 (32) 32 (s) 32 - (32) - 32 32 (32) 32 -)
         ((s) - e+32 - (32) q))
        :pitch-seq-palette (9 3 9 5 10 6 (7) (7) (7) 4 8)))
   (15 ((((2 4) { 3 { 3 (18) - 36 } { 3 18 36 } { 3 18 36 - } }
          { 3 { 3 - 18 36 } te+ts - (ts) })
         ({ 3 (t32) - t32 t32 t32 t32 t32 t32 t32 { 3 36 36 36 - } }
            (s) - e 32 - (32))
         ((s) e.+q))
        :pitch-seq-palette (3 8 (3) 9 (3) 8 (7) (7) 4 (9) (9) 9 3 9 5 10 6 (5)
                              (5) 6 (5))))
   (16 ((((2 4) { 3 { 3 - 36 36 36 } { 3 (36) 36 36 }
          { 3 (36) 36 36 - } } e. 32 (32))
         ((s) - 32 32 (s) s - (s) e.)
         ({ 5 - +fe fe f32 - (f32) } (s) e.))
        :pitch-seq-palette (1 2 5 5 5 5 5 5 5 5 4 5 2 3 2 6)))
   (17 ((((2 4) { 5 - fs fs fs (fs) fs - }
          { 3 (ts) - ts (ts) } { 3 ts ts ts - })
         ({ 3 (ts) - ts (ts) } { 3 te ts - } (s) e.)
         ({ 5 - +fe f32 f32 (fs) f32 f32 - } 
            { 5 (fs) - f32 f32 - (fs) (fe) }))
        :pitch-seq-palette (2 1 5 1 5 1 6 5 1 5 2 5 1 1 5 2 5 1)))
   (18 ((((2 4) (e) e - +e. s -)
         ((e) q.) ({ 5 +fe fs (fe) } q))
        :pitch-seq-palette (1 2 5 5 5)))
   (19 ((((2 4) (h)) ((s) e. +q) (+e. s (q)))
        :pitch-seq-palette (4 5)))
   (20 ((((2 4) q { 3 - +te te te - }) (- +e. s - (q)) (q. e))
        :pitch-seq-palette (3 8 (3) 9 (3) 8)))
   (21 ((((2 4) (e) q.) (+q. s (s))
         ({ 5 (fe) - fe fs - } - +s (e) s -))
        :pitch-seq-palette (9 3 9 5 10))))
 ;; there are twelve sections. 
 :rthm-seq-map
 '((1
    ((solo (1 10 19 13 12 5 21))
     (vln  (2 11 20 15 10 9 8))
     (vla  (3 12 21 17 14 13 10))
     (vc   (4 13 1 16 19 15 14))
     (fl   (5 14 3 18 21 11 16))
     (cl   (6 15 7 2 20 17 19))
     (hn   (7 16 5 4 1 2 12))
     (perc (8 17 9 6 3 4 18))))
   (2
    ((solo (2 11 20 15 10 9 8))
     ;; nil means it sits the sequence out. if it should sit the section out,
     ;; just don't include it
     (vln (nil 12 21 17 14 13 10))
     (vla (nil 13 1 16 19 15 14))
     (vc (nil 14 3 18 21 11 16))
     (fl (nil 15 7 2 20 17 19))
     (cl (nil 16 5 4 1 2 12))
     (hn (nil 17 9 6 3 4 18))
     (perc (nil 11 8 11 7 6 20))))
   (3
    ((solo (3 12 21 nil 14 13 10))
     (vln (nil 13 1 nil 21 17 14))
     (vla (nil 14 3 nil 21 17 16))
     (vc (6 15 7 nil 21 17 19))
     (fl (nil 16 5 nil 1 2 12))
     (cl (nil 17 9 nil 3 4 18))
     (hn (nil 18 8 11 7 6 20))
     (perc (nil 10 19 nil 12 5 21))))
   (4
    ((solo (nil 13 1 16 19 15 14))
     (vln (nil 14 3 18 21 11 16))
     (vla (nil nil 7 2 20 17 19))
     (vc (nil 16 nil 4 1 2 12))
     (fl (8 17 19 nil 3 4 18))
     (cl (9 18 19 11 nil 6 20))
     (hn (nil 10 19 13 12 nil 21))
     (perc (nil 11 20 15 10 9 nil))))
   (5 
    ((solo (nil 14 3 nil 21 11 nil))
     (vln (nil nil 7 nil 20 17 19))
     (vla (8 10 5 nil 1 nil nil))
     (vc (8 10 9 nil nil nil 18))
     (fl (nil 18 8 nil nil 6 20))
     (cl (nil 10 nil nil 12 5 21))
     (hn (2 nil nil nil 10 9 8))
     (perc (nil 12 21 17 14 13 10))))
   (6
    ((solo (nil 15 7 2 20 17 19))
     (vln (nil 16 nil 13 1 2 12))
     (vla (8 nil nil nil 3 nil 18))
     (vc (9 18 nil 13 1 6 nil))
     (fl (nil nil 19 13 1 5 21))
     (cl (nil nil nil 15 10 9 nil))
     (hn (nil 12 nil nil 1 nil nil))
     (perc (nil 13 1 nil 19 nil 14))))
   (7
    ((solo (nil nil 5 4 1 2 12))
     (vln (nil nil 9 17 14 nil nil))
     (vla (9 nil 8 11 nil 6 20))
     (vc (nil 10 nil 17 14 nil nil))
     (fl (nil 11 20 nil nil nil 8))
     (cl (nil 12 nil 17 14 13 10))
     (hn (nil nil nil 16 nil nil nil))
     (perc (nil 14 nil 18 nil 11 nil))))
   (8
    ((solo (8 17 9 6 nil 4 18))
     (vln (9 18 nil nil nil nil 20))
     (vla (3 nil 19 nil nil 5 21))
     (vc (3 nil 19 nil 10 5 nil))
     (fl (3 nil 21 17 nil nil nil))
     (cl (4 nil 19 16 nil nil nil))
     (hn (3 14 19 nil nil nil nil))
     (perc (6 nil 7 nil nil nil nil))))
   (9
    ((solo (9 18 8 11 nil 6 20))
     (vln (6 nil nil nil 12 nil 21))
     (vla (2 11 20 nil 12 nil 21))
     (vc (3 nil 21 nil 12 nil 21))
     (fl (4 nil 1 16 nil nil 21))
     (cl (6 nil 3 nil 21 nil 16))
     (hn (6 nil nil nil 20 17 19))
     (perc (6 nil nil nil 1 nil 21))))
   (10
    ((solo (1 10 19 13 12 5 21))
     (vln (nil nil nil nil nil nil nil))
     (vla (nil nil nil nil nil nil nil))
     (vc (nil nil nil nil nil nil nil))
     (fl (nil nil nil nil nil nil nil))
     (cl (nil nil nil nil nil nil nil))
     (hn (nil nil nil nil nil nil nil))
     (perc (nil nil nil nil nil nil nil))))
   (11
    ((solo (2 11 20 15 10 9 8))
     (vln (3 12 21 17 14 13 10))
     (vla (3 nil nil nil 19 15 14))
     (vc (5 nil nil nil nil nil nil))
     (fl (3 nil nil nil nil nil nil))
     (cl (3 nil nil nil nil nil nil))
     (hn (8 nil nil nil nil nil nil))
     (perc (3 nil nil nil nil nil nil))))
   (12
    ((solo (3 12 21 17 14 13 10))
     (vln (4 13 1 18 14 nil nil))
     (vla (5 14 3 18 14 nil nil))
     (vc (6 14 3 18 14 nil nil))
     (fl (6 nil 5 18 14 2 12))
     (cl (nil nil 3 18 14 nil nil))
     (hn (6 nil nil nil 14 nil nil))
     (perc (6 nil nil nil 12 5 21))))))

;;; Generate our score with CMN
#+cmn
(cmn-display +slippery-when-wet+
             :write-section-info nil
             :file "/tmp/slippery.eps")

;;; Write a MIDI file of the piece (will probably sound awful but hey...)
(midi-play +slippery-when-wet+ :midi-file "/tmp/slippery.mid")

;;; Write Lilypond files to the /tmp directory MDE Mon Apr 16 16:59:13 2012 --
;;; first try Lilypond with less players, for testing purposes.  Ideally we'd
;;; run Lilypond on this too to make sure there are no mistakes.
(write-lp-data-for-all +slippery-when-wet+ 
                       :base-path "/tmp/"
                       :staff-size 14.5
                       :landscape t
                       :line-width 26
                       :auto-clefs nil
                       :paper "a4"
                       :use-custom-markup t
                       :respell-notes t
                       :group-barlines t
                       :page-nums t
                       :all-bar-nums t
                       :barline-thickness 1
                       :players '(vln vc) 
                       :end-bar 30
                       :in-c nil)

;;; Write the Lilypond files for all the players in the /tmp directory.
(write-lp-data-for-all +slippery-when-wet+ 
                       :base-path "/tmp/"
                       :staff-size 14.5
                       :landscape t
                       :line-width 26
                       :auto-clefs nil
                       :paper "a4"
                       :use-custom-markup t
                       :respell-notes t
                       :group-barlines t
                       :page-nums t
                       :all-bar-nums t
                       :barline-thickness 1
                       :in-c nil)

;;; MDE Tue Jan 20 12:00:00 2026, Heidhausen
(auto-add-ornaments +slippery-when-wet+ nil)

(write-xml +slippery-when-wet+)

#|
(lp-display +slippery-when-wet+)

(add-mark-to-note +slippery-when-wet+ 2 1 'fl 'triangle-up)
(add-mark-to-note +slippery-when-wet+ 2 2 'fl 'x-head)
(add-mark-to-note +slippery-when-wet+ 1 '(4 2) 'vn '(rgb (0 1 0)))
(add-mark-to-note +slippery-when-wet+ 1 5 'vn 'flag-head)
(add-mark-to-note +slippery-when-wet+ 1 6 'vn '(rgb (0 0 1)))

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF slippery.lsp
