;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File:             sc-and-clm-examples.lsp
;;;
;;; Class Hierarchy:  None
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Lisp-code examples to accompany sc-and-clm.html
;;;
;;; Author:           Sean Reed
;;;
;;; Creation date:    17th July 2012
;;;
;;; $$ Last modified:  20:49:51 Tue Nov 12 2024 CET
;;;
;;; SVN ID: $Id: sc-and-clm-examples.lsp 3673 2013-06-10 10:56:24Z medward2 $
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
(in-scale :quarter-tone)

;;; ============================================================================
;;; sc-and-clm.html examples
;;; ============================================================================

;;;  setting the output directory
;;; ----------------------------------------------------------------------------
(let* ((mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((2 4) (s) e s e e))
                                 :pitch-seq-palette ((1 2 3 4)))))
         :rthm-seq-map '((1 ((vn (1)))))
         :snd-output-dir "/tmp/")))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))
  
;;;  defining the sndfile-palette
;;; ----------------------------------------------------------------------------
(let* ((sndfile-dir
        (concatenate 'string
                     cl-user::+slippery-chicken-home-dir+
                     "doc/manual/resources/"))
       (mini
        (make-slippery-chicken
         '+mini+
         :ensemble '(((vn (violin :midi-channel 1))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4 c5))))
         :set-map '((1 (1)))
         :rthm-seq-palette '((1 ((((2 4) (s) e s e e))
                                 :pitch-seq-palette ((1 2 3 4)))))
         :rthm-seq-map '((1 ((vn (1)))))
         :snd-output-dir "/tmp/"
         :sndfile-palette `(((developing-group
                              ((test-sndfile-1)
                               (test-sndfile-2)
                               (test-sndfile-3)))
                             (percussive-group
                              ((test-sndfile-4)
                               (test-sndfile-5)
                               (test-sndfile-6))))
                            ,(list sndfile-dir)
                            ("aiff")))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini))

;;;  the call to clm-play; specifying players
;;; ----------------------------------------------------------------------------  
(let* ((sndfile-dir
        (concatenate 'string
                     cl-user::+slippery-chicken-home-dir+
                     "doc/manual/resources/"))
       (mini
        (make-slippery-chicken
         '+sc-object+
         :ensemble '(((player-one (flute :midi-channel 1))
                      (player-two (oboe :midi-channel 2))
                      (player-three (bassoon :midi-channel 3))))
         :set-palette '((1 ((c2 d2 e2 f2 g2 a2 b2
                                c3 d3 e3 f3 g3 a3 b3
                                c4 d4 e4 f4 g4 a4 b4
                                c5 d5 e5 f5 g5 a5 b5 c6))))
         :set-map '((1 (1)))
         :rthm-seq-palette 
         '((1 ((((4 4) h q e s (s)))
               :pitch-seq-palette ((1 2 3 4)))))
         :rthm-seq-map '((1 ((player-one (1))
                             (player-two (1))
                             (player-three (1)))))
         :snd-output-dir "/tmp/"
         :sndfile-palette `(((source-sndfile-grp-1
                              ((test-sndfile-1.aiff)
                               (test-sndfile-2.aiff)
                               (test-sndfile-3.aiff)))
                             (source-sndfile-grp-2
                              ((test-sndfile-4.aiff)
                               (test-sndfile-5.aiff)
                               (test-sndfile-6.aiff))))
                            ,(list sndfile-dir)))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini)
  (clm-play +sc-object+ 1 '(player-one player-two) 'source-sndfile-grp-1
            :check-overwrite nil))

;;;  the call to clm-play; all players via nil
;;; ----------------------------------------------------------------------------
(let* ((sndfile-dir
        (concatenate 'string
                     cl-user::+slippery-chicken-home-dir+
                     "doc/manual/resources/"))
       (mini
        (make-slippery-chicken
         '+sc-object+
         :ensemble '(((player-one (flute :midi-channel 1))
                      (player-two (oboe :midi-channel 2))
                      (player-three (bassoon :midi-channel 3))))
         :set-palette '((1 ((c2 d2 e2 f2 g2 a2 b2
                                c3 d3 e3 f3 g3 a3 b3
                                c4 d4 e4 f4 g4 a4 b4
                                c5 d5 e5 f5 g5 a5 b5 c6))))
         :set-map '((1 (1)))
         :rthm-seq-palette 
         '((1 ((((4 4) h q e s (s)))
               :pitch-seq-palette ((1 2 3 4)))))
         :rthm-seq-map '((1 ((player-one (1))
                             (player-two (1))
                             (player-three (1)))))
         :snd-output-dir "/tmp/"
         :sndfile-palette `(((source-sndfile-grp-1
                              ((test-sndfile-1.aiff)
                               (test-sndfile-2.aiff)
                               (test-sndfile-3.aiff)))
                             (source-sndfile-grp-2
                              ((test-sndfile-4.aiff)
                               (test-sndfile-5.aiff)
                               (test-sndfile-6.aiff))))
                            ,(list sndfile-dir)))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini)
  (clm-play +sc-object+ 1 nil 'source-sndfile-grp-1 :check-overwrite nil))

;;;  :inc-start, :duration-scaler, and :ignore-rests
;;; ----------------------------------------------------------------------------
(let* ((sndfile-dir
        (concatenate 'string
                     cl-user::+slippery-chicken-home-dir+
                     "doc/manual/resources/"))
       (mini
        (make-slippery-chicken
         '+sc-object+
         :ensemble '(((player-one (flute :midi-channel 1))
                      (player-two (oboe :midi-channel 2))
                      (player-three (bassoon :midi-channel 3))))
         :set-palette '((1 ((c2 d2 e2 f2 g2 a2 b2
                                c3 d3 e3 f3 g3 a3 b3
                                c4 d4 e4 f4 g4 a4 b4
                                c5 d5 e5 f5 g5 a5 b5 c6))))
         :set-map '((1 (1 1 1)))
         :rthm-seq-palette '((1 ((((4 4) h q e s (s)))
                                 :pitch-seq-palette ((1 2 3 4)))))
         :rthm-seq-map '((1 ((player-one (1 1 1))
                             (player-two (1 1 1))
                             (player-three (1 1 1)))))
         :snd-output-dir "/tmp/"
         :sndfile-palette `(((source-sndfile-grp-1
                              ((test-sndfile-1.aiff)
                               (test-sndfile-2.aiff)
                               (test-sndfile-3.aiff)))
                             (source-sndfile-grp-2
                              ((test-sndfile-4.aiff)
                               (test-sndfile-5.aiff)
                               (test-sndfile-6.aiff))))
                            ,(list sndfile-dir)))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini)
  (clm-play +sc-object+ 1 'player-one 'source-sndfile-grp-1 
              :inc-start t
              :duration-scaler 1.3
              :check-overwrite nil
              :ignore-rests nil))

;;;  :start, :end, and :duration
;;; ----------------------------------------------------------------------------
(let* ((sndfile-dir
        (concatenate 'string
                     cl-user::+slippery-chicken-home-dir+
                     "doc/manual/resources/"))
       (mini
        (make-slippery-chicken
         '+sc-object+
         :ensemble '(((player-one (flute :midi-channel 1))
                      (player-two (oboe :midi-channel 2))
                      (player-three (bassoon :midi-channel 3))))
         :set-palette '((1 ((c2 d2 e2 f2 g2 a2 b2
                                c3 d3 e3 f3 g3 a3 b3
                                c4 d4 e4 f4 g4 a4 b4
                                c5 d5 e5 f5 g5 a5 b5 c6))))
         :set-map '((1 (1 1 1)))
         :rthm-seq-palette '((1 ((((4 4) h q e s (s)))
                                 :pitch-seq-palette ((1 2 3 4)))))
         :rthm-seq-map '((1 ((player-one (1 1 1))
                             (player-two (1 1 1))
                             (player-three (1 1 1)))))
         :snd-output-dir "/tmp/"
         :sndfile-palette `(((source-sndfile-grp-1 
                              ((test-sndfile-1 :start 0.000 
                                               :end 2.100) 
                               (test-sndfile-1 :start 0.000 
                                               :duration 0.308) 
                               (test-sndfile-1 :start (0 1 000))
                               (test-sndfile-1 :end 1.308)
                               (test-sndfile-1 :duration 1.736))))
                            ,(list sndfile-dir)
                            ("aiff")))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini)
  (clm-play +sc-object+ 1 'player-one 'source-sndfile-grp-1
            :check-overwrite nil))

;;;  base frequency
;;; ----------------------------------------------------------------------------
(let* ((sndfile-dir
        (concatenate 'string
                     cl-user::+slippery-chicken-home-dir+
                     "doc/manual/resources/"))
       (mini
        (make-slippery-chicken
         '+sc-object+
         :ensemble '(((flt (flute :midi-channel 1))
                      (obo (oboe :midi-channel 2))
                      (clr (bassoon :midi-channel 3))))
         :set-palette '((1 ((c2 d2 e2 f2 g2 a2 b2
                                c3 d3 e3 f3 g3 a3 b3
                                c4 d4 e4 f4 g4 a4 b4
                                c5 d5 e5 f5 g5 a5 b5 c6))))
         :set-map '((1 (1 1 1)))
         :rthm-seq-palette '((1 ((((4 4) h q e s (s)))
                                 :pitch-seq-palette ((1 2 3 4)))))
         :rthm-seq-map '((1 ((flt (1 1 1))
                             (obo (1 1 1))
                             (clr (1 1 1)))))
         :snd-output-dir "/tmp/"
         :sndfile-palette `(((percussive-models-group
                              ((test-sndfile-4 :start 0.000 :end 2.100) 
                               (test-sndfile-4 :start 0.000 
                                               :duration 0.308 
                                               :frequency 860) 
                               (test-sndfile-4 :start (0 1 000)
                                               :frequency a5)
                               (test-sndfile-4 :end 1.308
                                               :frequency a7)
                               (test-sndfile-4 :duration 1.736
                                               :frequency b6))))
                            ,(list sndfile-dir)
                            ("aiff")))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini)
  (clm-play +sc-object+ 1 'flt 'percussive-models-group :check-overwrite nil))

;;;  pitch-synchronous src 
;;; ----------------------------------------------------------------------------
(let* ((sndfile-dir
        (concatenate 'string
                     cl-user::+slippery-chicken-home-dir+
                     "doc/manual/resources/"))
       (mini
        (make-slippery-chicken
         '+sc-object+
         :ensemble '(((flt (flute :midi-channel 1))
                      (obo (oboe :midi-channel 2))
                      (bsn (bassoon :midi-channel 3))))
         :set-palette '((1 ((c2 d2 e2 f2 g2 a2 b2
                                c3 d3 e3 f3 g3 a3 b3
                                c4 d4 e4 f4 g4 a4 b4
                                c5 d5 e5 f5 g5 a5 b5 c6))))
         :set-map '((1 (1 1 1)))
         :rthm-seq-palette '((1 ((((4 4) h q e s (s)))
                                 :pitch-seq-palette ((1 2 3 4)))))
         :rthm-seq-map '((1 ((flt (1 1 1))
                             (obo (1 1 1))
                             (bsn (1 1 1)))))
         :snd-output-dir "/tmp/"
         :sndfile-palette `(((source-sndfile-grp-1
                              ((test-sndfile-4 :start 0.000 
                                               :end 2.100
                                               :frequency 860) 
                               (test-sndfile-4 :start 0.000 
                                               :duration 0.308 
                                               :frequency 860) 
                               (test-sndfile-4 :start (0 1 000)
                                               :frequency 860)
                               (test-sndfile-4 :end 1.308
                                               :frequency 860)
                               (test-sndfile-4 :duration 1.736
                                               :frequency 860))))
                            ,(list sndfile-dir)
                            ("aiff")))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini)
  (clm-play +sc-object+ 1 'flt 'source-sndfile-grp-1 :pitch-synchronous t
            :check-overwrite nil)) 

;;;  source sound file groups - multiple calls to clm-play
;;; ----------------------------------------------------------------------------
(let* ((sndfile-dir
        (concatenate 'string
                     cl-user::+slippery-chicken-home-dir+
                     "doc/manual/resources/"))
       (mini
        (make-slippery-chicken
         '+sc-object+
         :ensemble '(((player-one (flute :midi-channel 1))
                      (player-two (oboe :midi-channel 2))
                      (player-three (bassoon :midi-channel 3))))
         :set-palette '((1 ((c2 d2 e2 f2 g2 a2 b2
                                c3 d3 e3 f3 g3 a3 b3
                                c4 d4 e4 f4 g4 a4 b4
                                c5 d5 e5 f5 g5 a5 b5 c6))))
         :set-map '((1 (1 1 1)))
         :rthm-seq-palette '((1 ((((4 4) h q e s (s)))
                                 :pitch-seq-palette ((1 2 3 4))))
                             (2 ((((4 4) q e s (s) h))
                                 :pitch-seq-palette ((1 2 3 4))))
                             (3 ((((4 4) e s (s) h q))
                                 :pitch-seq-palette ((1 2 3 4)))))
         :rthm-seq-map '((1 ((player-one (1 2 3))
                             (player-two (2 3 1))
                             (player-three (3 1 2)))))
         :snd-output-dir "/tmp/"
         :sndfile-palette `(((source-sndfile-grp-1
                              ((test-sndfile-1)
                               (test-sndfile-2)))
                             (source-sndfile-grp-2
                              ((test-sndfile-3)
                               (test-sndfile-4)))
                             (source-sndfile-grp-3
                              ((test-sndfile-5)
                               (test-sndfile-6))))
                            ,(list sndfile-dir)
                            ("aiff")))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini)
  (clm-play +sc-object+ 1 '(player-one player-two) 'source-sndfile-grp-1
            :duration-scaler 0.7
            :check-overwrite nil
            :inc-start nil)
  (clm-play +sc-object+ 1 '(player-one player-two) 'source-sndfile-grp-2
            :duration-scaler 1.3
            :check-overwrite nil
            :inc-start t)
  (clm-play +sc-object+ 1 '(player-three) 'source-sndfile-grp-3
            :check-overwrite nil :ignore-rests nil))

;;;  fibonacci-transitioning between two groups
;;; ----------------------------------------------------------------------------
(let* ((sndfile-dir
        (concatenate 'string
                     cl-user::+slippery-chicken-home-dir+
                     "doc/manual/resources/"))
       (mini
        (make-slippery-chicken
         '+sc-object+
         :ensemble '(((player-one (flute :midi-channel 1))
                      (player-two (oboe :midi-channel 2))
                      (player-three (bassoon :midi-channel 3))))
         :set-palette '((1 ((c2 d2 e2 f2 g2 a2 b2
                                c3 d3 e3 f3 g3 a3 b3
                                c4 d4 e4 f4 g4 a4 b4
                                c5 d5 e5 f5 g5 a5 b5 c6))))
         :set-map '((1 (1 1 1)))
         :rthm-seq-palette '((1 ((((4 4) h q e s (s)))
                                 :pitch-seq-palette ((1 2 3 4))))
                             (2 ((((4 4) q e s (s) h))
                                 :pitch-seq-palette ((1 2 3 4))))
                             (3 ((((4 4) e s (s) h q))
                                 :pitch-seq-palette ((1 2 3 4)))))
         :rthm-seq-map '((1 ((player-one (1 2 3))
                             (player-two (2 3 1))
                             (player-three (3 1 2)))))
         :snd-output-dir "/tmp/"
         :sndfile-palette `(((source-sndfile-grp-1
                              ((test-sndfile-1)
                               (test-sndfile-2)))
                             (source-sndfile-grp-2
                              ((test-sndfile-3)
                               (test-sndfile-4))))
                            ,(list sndfile-dir)
                            ("aiff")))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini)
  (clm-play +sc-object+ 1 '(player-one player-two) 'source-sndfile-grp-1  
            :check-overwrite nil
            :sound-file-palette-ref2 'source-sndfile-grp-2))

;;;  :amplitude and :description
;;; ----------------------------------------------------------------------------
(let* ((sndfile-dir
        (concatenate 'string
                     cl-user::+slippery-chicken-home-dir+
                     "doc/manual/resources/"))
       (mini
        (make-slippery-chicken
         '+sc-object+
         :ensemble '(((fl (flute :midi-channel 1))
                      (ob (oboe :midi-channel 2))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4
                                c5 d5 e5 f5 g5 a5 b5 c6))))
         :set-map '((1 (1 1 1)))
         :rthm-seq-palette '((1 ((((4 4) h q e s (s)))
                                 :pitch-seq-palette ((1 2 3 4)))))
         :rthm-seq-map '((1 ((fl (1 1 1))
                             (ob (1 1 1)))))
         :snd-output-dir "/tmp/"
         :sndfile-palette `(((percussive-models-group
                              ((test-sndfile-4 
                                :description "initial attack"
                                :start 0.000 :end 2.100
                                :frequency 860
                                :amplitude 0.1)
                               (test-sndfile-4
                                :description "snap"
                                :start 0.000 :duration 0.308 
                                :frequency a5
                                :amplitude 1.0))))
                            ,(list sndfile-dir)
                            ("aiff")))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini)
  (clm-play +sc-object+ 1 'fl 'percussive-models-group :check-overwrite nil))

;;;  :num-sections, :from-sequence, :num-sequences, :reset-snds-each-rs, 
;;;  :reset-snds-each-player, :time-scaler, :src-scaler, and :rev-amt
;;; ----------------------------------------------------------------------------
(let* ((sndfile-dir
        (concatenate 'string
                     cl-user::+slippery-chicken-home-dir+
                     "doc/manual/resources/"))
       (mini
        (make-slippery-chicken
         '+sc-object+
         :ensemble '(((fl (flute :midi-channel 1))
                      (ob (oboe :midi-channel 2))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4
                                c5 d5 e5 f5 g5 a5 b5 c6))))
         :set-map '((1 (1 1 1))
                    (2 (1 1 1))
                    (3 (1 1 1)))
         :rthm-seq-palette '((1 ((((4 4) h q e s (s)))
                                 :pitch-seq-palette ((1 2 3 4)))))
         :rthm-seq-map '((1 ((fl (1 1 1))
                             (ob (1 1 1))))
                         (2 ((fl (1 1 1))
                             (ob (1 1 1))))
                         (3 ((fl (1 1 1))
                             (ob (1 1 1)))))
         :snd-output-dir "/tmp/"
         :sndfile-palette `(((source-sndfile-grp-1 
                              ((test-sndfile-1)
                               (test-sndfile-2)
                               (test-sndfile-3)))
                             (source-sndfile-grp-2
                              ((test-sndfile-4)
                               (test-sndfile-5)
                               (test-sndfile-6))))
                            ,(list sndfile-dir)
                            ("aiff")))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini)
  (clm-play +sc-object+ 1 nil 'source-sndfile-grp-1 :num-sections 2
            :check-overwrite nil)
  (clm-play +sc-object+ 1 nil 'source-sndfile-grp-2 :num-sections 1
            :check-overwrite nil
            :from-sequence 2
            :num-sequences 2)
  (clm-play +sc-object+ 2 nil 'source-sndfile-grp-1
            :check-overwrite nil
            :reset-snds-each-rs nil
            :reset-snds-each-player nil)
  (clm-play +sc-object+ 2 nil 'source-sndfile-grp-2 :time-scaler 1.7
            :check-overwrite nil)
  (clm-play +sc-object+ 3 nil 'source-sndfile-grp-1 :src-scaler 1.9
            :check-overwrite nil)
  (clm-play +sc-object+ 3 nil 'source-sndfile-grp-2 :rev-amt 0.1
            :check-overwrite nil))
            
;;;  output format arguments
;;; ----------------------------------------------------------------------------
(let* ((sndfile-dir
        (concatenate 'string
                     cl-user::+slippery-chicken-home-dir+
                     "doc/manual/resources/"))
       (mini
        (make-slippery-chicken
         '+sc-object+
         :ensemble '(((fl (flute :midi-channel 1))
                      (ob (oboe :midi-channel 2))))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4
                                c5 d5 e5 f5 g5 a5 b5 c6))))
         :set-map '((1 (1 1 1)))
         :rthm-seq-palette '((1 ((((4 4) h q e s (s)))
                                 :pitch-seq-palette ((1 2 3 4)))))
         :rthm-seq-map '((1 ((fl (1 1 1))
                             (ob (1 1 1)))))
         :snd-output-dir "/tmp/"
         :sndfile-palette `(((source-sndfile-grp-1 
                              ((test-sndfile-1)
                               (test-sndfile-2)
                               (test-sndfile-3)))
                             (source-sndfile-grp-2
                              ((test-sndfile-4)
                               (test-sndfile-5)
                               (test-sndfile-6))))
                            ,(list sndfile-dir)
                            ("aiff")))))
  (midi-play mini)
  (cmn-display mini)
  (write-lp-data-for-all mini)
  (clm-play +sc-object+ 1 nil 'source-sndfile-grp-1 
            :channels 8
            :check-overwrite nil
            :srate 44100
            :header-type clm::mus-riff
            :data-format clm::mus-bshort
            :sndfile-extension ".aiff"))

;;;  independent tape parts
;;; ----------------------------------------------------------------------------
(let* ((sndfile-dir
        (concatenate 'string
                     cl-user::+slippery-chicken-home-dir+
                     "doc/manual/resources/"))
       (mini
        (make-slippery-chicken
         '+sc-object+
         :ensemble '(((fl (flute :midi-channel 1))
                      (ob (oboe :midi-channel 2))
                      (cl (b-flat-clarinet :midi-channel 3))
                      (cp (computer))))
         :set-limits-high '((cp (0 c6 100 c6)))
         :set-limits-low '((cp (0 f3 100 f3)))
         :set-palette '((1 ((c4 d4 e4 f4 g4 a4 b4
                                c5 d5 e5 f5 g5 a5 b5 c6))))
         :set-map '((1 (1 1 1 1 1 1)))
         :rthm-seq-palette '((1 ((((4 4) h q e s (s)))
                                 :pitch-seq-palette ((1 2 3 4))))
                             (2 ((((4 4) q e s (s) h))
                                 :pitch-seq-palette ((1 2 3 4))))
                             (3 ((((4 4) e s (s) h q))
                                 :pitch-seq-palette ((1 2 3 4)))))
         :rthm-seq-map '((1 ((fl (1 2 3 1 3 2))
                             (ob (2 3 1 3 2 1))
                             (cl (3 1 3 2 1 2))
                             (cp (1 3 2 1 2 3)))))
         :snd-output-dir "/tmp/"
         :sndfile-palette `(((source-sndfile-grp-1 
                              ((test-sndfile-1)
                               (test-sndfile-2)
                               (test-sndfile-3)))
                             (source-sndfile-grp-2
                              ((test-sndfile-4)
                               (test-sndfile-5)
                               (test-sndfile-6))))
                            ,(list sndfile-dir)
                            ("aiff")))))
  (clm-play +sc-object+ 1 'cp 'source-sndfile-grp-1 :check-overwrite nil)
  (midi-play +sc-object+ :voices '(fl ob cl) :midi-file "/tmp/output.mid")
  (cmn-display +sc-object+ :players '(fl ob cl) :file "/tmp/output.eps")
  (write-lp-data-for-all +sc-object+ :players '(fl ob cl) :base-path "/tmp/"))
