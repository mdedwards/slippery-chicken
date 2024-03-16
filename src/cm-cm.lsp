;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* sc/cm-cm
;;; NAME 
;;; cm
;;;
;;; File:             cm-cm.lsp
;;;
;;; Class Hierarchy:  none (no classes defined)
;;;
;;; Version:          1.0.12
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Definition of common-music functions etc. that need to be
;;;                   in the cm package 
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    26th March 2020 (abstracted out of cm.lsp)
;;;
;;; $$ Last modified:  14:01:47 Sat Mar 16 2024 CET
;;;
;;; SVN ID: $Id$
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

(in-package :cm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 5/5/06: updated for cm 2.8.0
;;; bend is in semitones

#+cm-2
(defun set-pitch-bend (time channel bend)
  ;; (declare (special midimsg msg))
  ;; (output (new midimsg time time msg 
  ;; (format t "~&set-pitch-bend: time ~a channel ~a bend ~a" time channel bend)
  (new midi-pitch-bend :time time :channel channel
       :bend (if (zerop bend) 0 (round (rescale bend -2 2 -8192 8191)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+cm-2
(defun reset-pitch-bend (time channel)
  (set-pitch-bend time channel 0.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+cm-2
(defun output-midi-note (midi-note pitch-bend time amplitude duration
                         channel)  
  (declare (special midi keynum))
  (list 
   (set-pitch-bend time channel pitch-bend)
   (new midi 
     :time time 
     :keynum midi-note
     ;; CM dict says: "A floating-point logical amplitude 0.0-1.0, or an integer
     ;; velocity 0-127"  
     :amplitude amplitude
     :duration duration  
     :channel channel)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; tempo is a tempo instance

(defun output-midi-tempo-change (time tempo)
  ;; (print tempo)
  (when (< (sc::bpm tempo) 4)
    (warn "cm::output-midi-tempo-change: Tempi < 4 will probably result in ~
           ~%incorrect playback in your MIDI software. (Time: ~a, tempo: ~a)"
          time (sc::bpm tempo)))
  (new midi-tempo-change :time time :usecs (sc::usecs tempo)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun output-midi-time-sig (time num denom &optional (clocks 24))
  ;; (format t "~&output-midi-time-sig: time: ~a, num: ~a, denom: ~a, ~
  ;;         clocks: ~a"
  ;;      time num denom clocks)
  (new midi-time-signature :time time :numerator num
       :denominator denom :clocks clocks))
       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+cm-2
(defun midi-program-change (time channel program)
  ;; (format t "~&midi-program-change: time: ~a, channel: ~a, program: ~a"
     ;;     time channel program)
  (new midi-program-change :time time :channel (1- channel)
       :program (1- program)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Tue Apr 26 15:21:26 2016
#+cm-2
(defun midi-control-change (time channel controller value)
  (new midi-control-change :time time :channel (1- channel)
       :controller controller :value value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 5/5/06: updated for cm 2.8.0

#+cm-2
(defun midi-program-changes (voices &optional (time 0.0))
  (new seq :name 'program-changes :time time
       :subobjects
       (loop for v in voices collect
             (midi-program-change time (first v) (second v)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; voices is the type of list structure returned by
;;; slippery-chicken::get-events-start-time-duration  
;;; midi-setup are the channels and program numbers for the different
;;; instruments.  
;;; start-tempo is an sc tempo object

(defun process-voices (voices midi-file start-tempo midi-setup time-offset
                       &optional force-velocity)
  (events
   (cons (midi-program-changes midi-setup)
         (loop for voice in voices collect
              (new seq :name (gensym) :time 0.0 :subobjects
                   (loop for rs in voice appending
                        (sc::flatten (loop for event in rs 
                                        appending
                                          (sc::output-midi event time-offset 
                                                           force-velocity)))))))
   midi-file :tempo (sc::qtr-bpm start-tempo)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun event-list-to-midi-file (event-list midi-file start-tempo time-offset
                                &optional force-velocity)
  (events
   (new seq :name (gensym) :time 0.0 :subobjects
        (loop for event in (sc::sort-event-list event-list)
           appending
           ;; MDE Fri Nov 30 09:06:39 2018 -- have to flatten because of chords,
           ;; just like process-voices does above when called from midi-play  
             (sc::flatten (sc::output-midi event time-offset 
                                           force-velocity))))
   midi-file :tempo start-tempo))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (cm::parse-midi-file "/Users/medward2/mus/altogether/score/altogether.mid")

;;; ****f* cm/parse-midi-file
;;; DESCRIPTION
;;; Return Common Music MIDI events in the specified file as a list.
;;;
;;; NB: This is a Common Music function and as such must be called with the
;;;     package qualifier cm:: if used within slippery chicken.
;;; 
;;; ARGUMENTS 
;;; - The path (including the file name) to the MIDI file.
;;;
;;; OPTIONAL ARGUMENTS
;;; - An integer or NIL to indicate which track in the specified MIDI file is
;;;   to be accessed. If NIL, all tracks will be accessed. NB: CM (and
;;;   therefore slippery-chicken too) generates some MIDI files by writing each
;;;   channel to a different track, so the "track" would seem synonymous with
;;;   "channel" here. 
;;;
;;; RETURN VALUE  
;;; The CM data for the MIDI events in the specified file, and the number of
;;; events. 
;;;
;;; EXAMPLE
#|
(cm::parse-midi-file "/tmp/multi-ps.mid")

=>
(#i(midi-tempo-change time 0.0 usecs 357142)
 #i(midi-time-signature time 0.0 numerator 2 denominator 4 clocks 24 32nds 8)
 #i(midi time 0.0 keynum 36 duration 0.357142 amplitude 0.09448819 channel 15)
 #i(midi-tempo-change time 0.0 usecs 357142)
 #i(midi-time-signature time 0.0 numerator 2 denominator 4 clocks 24 32nds 8)
 #i(midi-tempo-change time 0.0 usecs 357142)
 #i(midi time 0.178571 keynum 66 duration 0.178571 amplitude 0.09448819 channel 15)
 #i(midi time 0.357142 keynum 68 duration 0.0892855 amplitude 0.09448819 channel 15)
 #i(midi time 0.357142 keynum 40 duration 0.357142 amplitude 0.09448819 channel 15)
 #i(midi time 0.6249985 keynum 66 duration 0.0892855 amplitude 0.09448819 channel 15)
 #i(midi-time-signature time 0.714284 numerator 3 denominator 4 clocks 24 32nds 8)

|#
;;; 
;;; SYNOPSIS
(defun parse-midi-file (file &optional track)
;;; ****
  (let ((midi-stream (parse-midi-file-aux file track))
        (num-events 0))
    (setf num-events (length (subobjects midi-stream)))
    ;; MDE Mon Jun 17 11:47:47 2013 -- not much point printing them when the
    ;; list is printed in pretty much the same way. 
    #|
    (map-subobjects (lambda (n) (format t "~&Event ~A" n)) 
                    midi-stream)
    (format t "~&~a events total" num-events)
    |#
    (values (subobjects midi-stream) num-events)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat May  5 11:48:51 BST 2012: Added robodoc entry

;;; ****f* cm/midi-file-high-low
;;; DATE
;;; 30-Dec-2010
;;;
;;; DESCRIPTION
;;; Print the highest and lowest pitch in a specified MIDI file as a MIDI note
;;; number. 
;;;
;;; NB: This is a Common Music function and as such must be called with the
;;;     package qualifier cm:: if used within slippery chicken.
;;; 
;;; ARGUMENTS 
;;; - The path (including the name) to the MIDI file.
;;;
;;; OPTIONAL ARGUMENTS
;;; - An integer or NIL to indicate which track in the specified MIDI file is
;;;   to be accessed. If NIL, all tracks will be accessed. NB: CM (and
;;;   therefore slippery-chicken too) generates some MIDI files by writing each
;;;   channel to a different track, so the "track" would seem synonymous with
;;;   "channel" here. 
;;; 
;;; RETURN VALUE  
;;; Two integer values (using the values function) that are the highest and
;;; lowest pitches in the specified MIDI file.
;;;
;;; EXAMPLE
#|
(cm::midi-file-high-low "/tmp/multi-ps.mid")

=> 72, 60

|#
;;; 
;;; SYNOPSIS
(defun midi-file-high-low (file &optional track)
;;; ****
  (let ((midi-stream (parse-midi-file-aux file track))
        (low 128)
        (high 0))
    (map-subobjects (lambda (n) 
                      (let ((note (midi-keynum n)))
                        (when (< note low)
                          (setf low note))
                        (when (> note high)
                          (setf high note))))
                    midi-stream :type 'midi)
    (format t "~&high: ~a low: ~a" 
            (sc::midi-to-note high) (sc::midi-to-note low))
    (values high low)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun midi-file-to-events-list (file &optional track)
  (let ((midi-stream (parse-midi-file-aux file track)))
    (subobjects midi-stream)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; btw the time slot is cm::object-time, midi note number cm::midi-keynum
(defun parse-midi-file-aux (file &optional track)
  ;; MDE Sat Nov 25 13:31:12 2017 -- ignore MIDI meta messages
  ;; MDE Wed Jan 30 14:21:24 2019 -- TODO: allow track to be T whereupon we
  ;; would get them all  
  (let ((midi-stream (import-events file :meta-exclude nil)))
    (when track
      (setf midi-stream (nth track midi-stream)))
    midi-stream))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat May  5 11:59:43 BST 2012: Conformed robodoc entry
;;; SAR Mon Jun  4 18:45:06 BST 2012: More conforming

;;; ****f* cm/midi-file-one-note
;;; DESCRIPTION
;; Write all midi notes in the file out to a new one-channel file using the
;; single pitch <note> and channel number <channel>.
;;; 
;;; ARGUMENTS 
;;; - A string that is the file path, including file-name and extension.
;;; - A note-name symbol or MIDI-note integer that is the pitch to write.
;;; - An integer that is the channel to which the output should be written
;;;   (1-based) 
;;;
;;; OPTIONAL ARGUMENTS 
;;; - An integer that is the an existing MIDI channel in the original file. If
;;;   this argument is given, only notes on this channel of the original file
;;;   will be written (1-based).
;;; 
;;; RETURN VALUE  
;;; The path to the new file.
;;; 
;;; EXAMPLE
#|
(cm::midi-file-one-note "/tmp/multi-ps.mid" 'c4 1)
|#
;;; 
;;; SYNOPSIS
(defun midi-file-one-note (file note channel &optional old-channel)
;;; ****
  ;; MDE Sun May  6 16:52:09 2012 -- check
  (when (integerp channel)
    (decf channel))
  ;; MDE Sun May  6 16:50:33 2012 -- test for value first
  (when (integerp old-channel)
    (decf old-channel))
  (let ((midi-stream (import-events file))
        ;; MDE Sat May 19 20:58:00 2012 -- (keynum) would appear to be degree
        ;; in whatever scale we're in, rather than the MIDI note number 
        ;; (degree (if (numberp note) note (keynum note)))
        (degree (if (numberp note) note (sc::note-to-midi note)))
        (new-file (format nil "~a-one-note.mid"
                          (sc::path-minus-extension file)))
        (new '()))
    (map-subobjects (lambda (n) 
                      (when (or (not old-channel)
                                (= old-channel (midi-channel n)))
                        (setf (midi-channel n) channel)
                        (setf (midi-keynum n) degree)
                        (push n new)))
                    midi-stream :type 'midi)
    (if new
        (events (nreverse new) new-file)
        (warn "cm::midi-file-one-note::No events matched/written."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF cm.lsp

