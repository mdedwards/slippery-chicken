;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* sndfile/sndfile-ext
;;; NAME 
;;; sndfile-ext
;;;
;;; File:             sndfile-ext.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> sndfile ->
;;;                   sndfile-ext 
;;;
;;; Version:          1.0.1
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Extension of the sndfile class to hold further properties
;;;                   which quantify the character of the sound file, and
;;;                   specifies sound files which can follow the current one.
;;;                   Specifically created to interface with the sndfilenet
;;;                   project in MaxMSP via OSC (see osc.lsp and osc-sc.lsp).
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    16th December 2012, Koh Mak, Thailand
;;;
;;; $$ Last modified: 19:35:02 Sun Dec 16 2012 ICT
;;;
;;; SVN ID: $Id: sclist.lsp 963 2010-04-08 20:58:32Z medward2 $
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

(eval-when (compile)
  (declaim (optimize (speed 3) (safety 1) (space 0) (debug 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :slippery-chicken)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass sndfile-ext (sndfile)
  ;; this should be set for each sound before starting a piece so we can
  ;; include/exclude some sounds from a particular piece/performance.
  ((use :accessor use :type boolean :initarg :use :initform t)
   (loop-it :accessor loop-it :type boolean :initarg :loop-it :initform nil)
   ;; the bit rate of the sound (16, 24...)
   (bitrate :accessor bitrate :type integer :initarg :bitrate :initform -1)
   ;; the sampling rate of the sound file (44100, 48000...)
   (srate :accessor srate :type integer :initarg :srate :initform -1)
   ;; the number of sample frames (one 16 bit sample if mono, two
   ;; if stereo)
   (num-frames :accessor num-frames :type integer :initarg :num-frames
               :initform -1)
   ;; the number of bytes the sound file occupies (including the header)
   (bytes :accessor bytes :type integer :initarg :bytes :initform -1)
   ;; references to the sndfile-ext objects that may follow the current.  This
   ;; should be a list of references into the containing sndfile-palette.  It
   ;; will be turned into a circular-sclist upon init.
   (followers :accessor followers :initarg :followers :initform nil)
   ;; For the following 'characteristics' slots, each of which has an integer
   ;; value, see the textual descriptions for each value in the class slot
   ;; 'characteristics' below.
   ;; 
   ;; the general pitch (tessitura) height of the sound (0=lowest)
   ;; this number represents the octave number (American system,
   ;; octave 4 starts with middle c).  See also frequency slot of sndfile.
   (pitch :accessor pitch :type integer :initarg :pitch :initform -1)
   (pitch-curve :accessor pitch-curve :type integer :initarg :pitch-curve
                :initform -1) 
   ;; the bandwidth or 'vertical' frequency space of the sound,
   ;; unrelated to tessitura (0=very narrow:sinewave, 1=normal
   ;; instrumental tone) 
   ;; cf bandwidth-lookup table
   (bandwidth :accessor bandwidth :type integer :initarg :bandwidth
              :initform -1)
   (bandwidth-curve :accessor bandwidth-curve :type integer :initarg
                    :bandwidth-curve :initform -1)
   ;; how continuous the sound is (0=very sporadic events surrounded by
   ;; silence, 10=held sound or very many events/grains); related to energy but
   ;; not quite the same...  cf continuity-lookup table
   (continuity :accessor continuity :type integer :initarg :continuity
               :initform -1)
   (continuity-curve :accessor continuity-curve :type integer :initarg
                     :continuity-curve :initform -1)
   ;; the 'weight' of the sound. cf weight-lookup table
   (weight :accessor weight :type integer :initarg :weight :initform -1)
   (weight-curve :accessor weight-curve :type integer :initarg :weight-curve
                 :initform -1)
   ;; how static (0) or energetic (10) the sound is on average. cf
   ;; energy-lookup table 
   (energy :accessor energy :type integer :initarg :energy :initform -1)
   (energy-curve :accessor energy-curve :type integer :initarg :energy-curve
                 :initform -1)
   ;; how 'harmonic' the sound is, i.e. pitched (0=white noise,
   ;; 10=clarinet/flute/sine wav).  cf harmonicity-lookup table
   (harmonicity :accessor harmonicity :type integer :initarg :harmonicity
                :initform -1) 
   (harmonicity-curve :accessor harmonicity-curve :type integer :initarg
                      :harmonicity-curve :initform -1)
   ;; the loudness, volume, rms or whatever else you want to call it of the
   ;; sound (1- 10, pppp - ffff) cf volume-lookup table
   (volume :accessor volume :type integer :initarg :volume :initform -1)
   (volume-curve :accessor volume-curve :type integer :initarg :volume-curve
                 :initform -1)
   ;; textual descriptions of the slots above, including their respective curves
   (characteristics 
    :allocation :class :accessor characteristics :initform
    (make-ral
     'sndfile-ext-characeristics
     '((curve
        ((0 "curve: 0: falling")
         (1 "curve: 1: steady")
         (2 "curve: 2: rising")
         (3 "curve: 3: rising then falling")
         (4 "curve: 4: falling then rising")
         (5 "curve: 5: complex")))
       (continuity
        ((0 "continuity: 0: extremely few events, almost all silence")
         (1 "continuity: 1: very few events, much silence")
         (2 "continuity: 2: few events separated by silence")
         (3 "continuity: 3: more events than silence")
         (4 "continuity: 4: significantly more events than silence")
         (5 "continuity: 5: medium dense, quite a lot of events")
         (6 "continuity: 6: quite dense, a lot of events")
         (7 "continuity: 7: dense, a lot of fast moving events")
         (8 "continuity: 8: very dense, a torrent of events")
         (9 "continuity: 9: extremely dense, thick cloud of events")
         (10 "continuity: 10: saturated/held sound")))
       (energy
        ((0 "energy: 0: minimum energy")
         (1 "energy: 1: very low energy")
         (2 "energy: 2: low energy")
         (3 "energy: 3: low tending medium energy")
         (4 "energy: 4: medium tending low energy")
         (5 "energy: 5: medium energy")
         (6 "energy: 6: medium tending high energy")
         (7 "energy: 7: high tending medium energy")
         (8 "energy: 8: high energy")
         (9 "energy: 9: very high energy")
         (10 "energy: 10: maximum energy")))
       (weight
        ((0 "weight 0: minimum")
         (1 "weight: 1: extremely light")
         (2 "weight: 2: very light")
         (3 "weight: 3: light")
         (4 "weight: 4: medium light")
         (5 "weight: 5: medium")
         (6 "weight: 6: medium heavy")
         (7 "weight: 7: heavy")
         (8 "weight: 8: very heavy")
         (9 "weight: 9: extremely heavy")
         (10 "weight: 10: maximum")))
       (harmonicity
        ((0 "harmonicity: 0: white noise")
         (1 "harmonicity: 1: extremely noisy, a little pitch discernible, ~
             e.g. some tam-tam strokes, lightly filtered white noise")
         (2 "harmonicity: 2: very, very noisy, almost all noise but also ~
             a clearly audible pitch content")
         (3 "harmonicity: 3: very noisy, more noise than pitch, ~
             e.g. violin sul pont estremo")
         (4 "harmonicity: 4: more noisy, a little bit more noise than pitch")
         (5 "harmonicity: 5: mixed, pitch and noise mix equally balanced")
         (6 "harmonicity: 6: noisy, a little bit more pitch than noise")
         (7 "harmonicity: 7: quite noisy, more pitch than noise, ~
             e.g. sax flutter tongue or violin poco sul pont")
         (8 "harmonicity: 8: pure note(s), e.g. normal violin tone(s)")
         (9 "harmonicity: 9: very pure note(s), e.g. clarinet, low flute")
         (10 "harmonicity: 10: sine wave(s)")))
       (volume
        ((1 "volume: 1: pppp")
         (2 "volume: 2: ppp")
         (3 "volume: 3: pp")
         (4 "volume: 4: p")
         (5 "volume: 5: mp")
         (6 "volume: 6: mf")
         (7 "volume: 7: f")
         (8 "volume: 8: ff")
         (9 "volume: 9: fff")
         (10 "volume: 10: ffff")))
       (bandwidth
        ((0 "bandwidth: 0: single frequency, e.g. sine wave")
         (1 "bandwidth: 1: single pure note, e.g. clarinet, low flute")
         (2 "bandwidth: 2: single normal note, e.g. violin tone")
         (3 "bandwidth: 3: > single note but extremely narrow range ~
             (< Minor 3rd)")
         (4 "bandwidth: 4: > single note but very narrow range (< Perfect 5th)")
         (5 "bandwidth: 5: > single note but narrow range (< 1 octave)")
         (6 "bandwidth: 6: medium range (< 2 octaves)")
         (7 "bandwidth: 7: wide range (2 - 4 octaves)")
         (8 "bandwidth: 8: very wide range (4 - 6 octaves)")
         (9 "bandwidth: 9: extemely wide range (> 6 octaves)")
         (10 "bandwidth: 10: white noise (all frequencies)"))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((sfe sndfile-ext) &rest initargs)
  (declare (ignore initargs))
  ;; (print 'init-sndfile-ext)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod update :after ((sfe sndfile-ext))
  ;; (print 'update-sfe) (print (path sfe))
  #+clm 
  (when (path sfe)
    (setf (bitrate sfe) (* 8 (clm::sound-datum-size (path sfe)))
          (srate sfe) (clm::sound-srate (path sfe))
          (num-frames sfe) (clm::sound-frames (path sfe))
          (bytes sfe) (clm::sound-length (path sfe)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone ((sfe sndfile-ext))
  (clone-with-new-class sfe 'sndfile-ext))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone-with-new-class :around ((sfe sndfile-ext) new-class)
  (declare (ignore new-class))
  (let ((sf (call-next-method)))
    (setf (slot-value sf 'use) (use sfe)
          (slot-value sf 'pitch) (pitch sfe)
          (slot-value sf 'pitch-curve) (pitch-curve sfe)
          (slot-value sf 'bandwidth) (bandwidth sfe)
          (slot-value sf 'bandwidth-curve) (bandwidth-curve sfe)
          (slot-value sf 'continuity) (continuity sfe)
          (slot-value sf 'continuity-curve) (continuity-curve sfe)
          (slot-value sf 'weight) (weight sfe)
          (slot-value sf 'weight-curve) (weight-curve sfe)
          (slot-value sf 'energy) (energy sfe)
          (slot-value sf 'energy-curve) (energy-curve sfe)
          (slot-value sf 'harmonicity) (harmonicity sfe)
          (slot-value sf 'harmonicity-curve) (harmonicity-curve sfe)
          (slot-value sf 'volume) (volume sfe)
          (slot-value sf 'volume-curve) (volume-curve sfe)
          (slot-value sf 'loop-it) (loop-it sfe)
          (slot-value sf 'bitrate) (bitrate sfe)
          (slot-value sf 'srate) (srate sfe)
          (slot-value sf 'num-frames) (num-frames sfe)
          (slot-value sf 'bytes) (bytes sfe)
          (slot-value sf 'followers) (followers sfe))
    (print 'sndfile-ext-clone-wnc) (print (data sf))
    sf))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object :before ((sfe sndfile-ext) stream)
  (format stream "~%~%SNDFILE-EXT: use: ~a, ~
                    ~%             pitch: ~a, ~
                    ~%             pitch-curve: ~a, ~
                    ~%             bandwidth: ~a, ~
                    ~%             bandwidth-curve: ~a, ~
                    ~%             continuity: ~a, ~
                    ~%             continuity-curve: ~a, ~
                    ~%             weight: ~a, ~
                    ~%             weight-curve: ~a, ~
                    ~%             energy: ~a, ~
                    ~%             energy-curve: ~a, ~
                    ~%             harmonicity: ~a, ~
                    ~%             harmonicity-curve: ~a, ~
                    ~%             volume: ~a, ~
                    ~%             volume-curve: ~a, ~
                    ~%             loop-it: ~a, ~
                    ~%             bitrate: ~a, ~
                    ~%             srate: ~a, ~
                    ~%             num-frames: ~a, ~
                    ~%             bytes: ~a, ~
                    ~%             followers: ~a"
          (use sfe) (pitch sfe) (pitch-curve sfe) (bandwidth sfe)
          (bandwidth-curve sfe) (continuity sfe) (continuity-curve sfe)
          (weight sfe) (weight-curve sfe) (energy sfe) (energy-curve sfe)
          (harmonicity sfe) (harmonicity-curve sfe) (volume sfe)
          (volume-curve sfe) (loop-it sfe) (bitrate sfe) (srate sfe)
          (num-frames sfe) (bytes sfe) (followers sfe)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-next ((sfe sndfile-ext))
  (get-next (followers sfe)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod reset ((sfe sndfile-ext) &optional where)
  (reset (followers sfe) where))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod score ((sfe1 sndfile-ext) (sfe2 sndfile-ext))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod set-characteristic ((sfe sndfile-ext) characteristic value 
                               &optional (on-fail #'error))
  (let* ((is-curve (search "CURVE" (string characteristic)))
         ;; any of the curve slots hold values from the generic 'curve'
         ;; characteristic 
         (val (get-data (list (if is-curve 'curve characteristic) value)
                        (characteristics sfe) nil)))
    (if val
        (setf (slot-value sfe characteristic) value)
        (when on-fail
          (funcall 
           on-fail
           "sndfile-ext::set-characteristic: No such characteristic: ~a ~a"
           characteristic value)
          nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****f* sndfile-ext/make-sndfile-ext
;;; DESCRIPTION
;;; Make a sndfile-ext (extension of sndfile) object which holds the usual
;;; sndfile data as well as a host of others to do with the characteristics of
;;; the sound file.  In addition, the followers slot specifies sound files
;;; which can follow the current one.  The bitrate, srate, num-frames, and
;;; bytes slots will be filled automatically if the path to an existing sound
;;; file is given.
;;; 
;;; ARGUMENTS
;;; See defclass slot descriptions
;;; 
;;; RETURN VALUE
;;; A sndfile-ext object.
;;; 
;;; SYNOPSIS
(defun make-sndfile-ext (path &key id data duration end (start 0.0)
                         (frequency 'c4) (amplitude 1.0)
                         (use t) (pitch -1) (pitch-curve -1) (bandwidth -1)
                         (bandwidth-curve -1) (continuity -1)
                         (continuity-curve -1) (weight -1) (weight-curve -1)
                         (energy -1) (energy-curve -1) (harmonicity -1)
                         (harmonicity-curve -1) (volume -1) (volume-curve -1)
                         (loop-it nil) (bitrate -1) (srate -1) (num-frames -1)
                         (bytes -1) followers)
;;; ****
  (let ((sf (make-sndfile path :id id :data data :duration duration :end end
                          :start start :frequency frequency :amplitude
                          amplitude)))
    ;; (print 'make-sndfile-ext) (print (path sf))
    ;; remember that this goes back to named-object which calls make-instance
    ;; 'sndfile-ext with all slots NIL, so we'll have to call update below.
    (setf sf (clone-with-new-class sf 'sndfile-ext))
    ;; (print 'make-sndfile-ext2) (print (path sf))
    (setf (use sf) use 
          (pitch sf) pitch
          (pitch-curve sf) pitch-curve
          (bandwidth sf) bandwidth
          (bandwidth-curve sf) bandwidth-curve
          (continuity sf) continuity
          (continuity-curve sf) continuity-curve
          (weight sf) weight
          (weight-curve sf) weight-curve
          (energy sf) energy
          (energy-curve sf) energy-curve
          (harmonicity sf) harmonicity 
          (harmonicity-curve sf) harmonicity-curve
          (volume sf) volume
          (volume-curve sf) volume-curve
          (loop-it sf) loop-it
          ;; bear in mind that these data will be changed by the update method
          (bitrate sf) bitrate
          (srate sf) srate
          (num-frames sf) num-frames
          (bytes sf) bytes
          (followers sf) (make-cscl followers :id (format nil "~a-followers"
                                                          (id sf))
                                    :copy nil))
    ;; have to call this here because clone init'ed with all slots NIL
    (update sf)
    sf))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EOF sndfile-ext.lsp
