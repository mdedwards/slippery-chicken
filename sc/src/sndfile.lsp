;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* linked-named-object/sndfile
;;; NAME 
;;; sndfile
;;;
;;; File:             sndfile.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> sndfile
;;;
;;; Version:          0.9.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Implementation of the sndfile class that holds
;;;                   information about a sound file as well as specifying
;;;                   desired parameters
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    March 21st 2001
;;;
;;; $$ Last modified: 16:20:22 Wed Mar  7 2012 GMT
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

;;; the name (defaults to the given name of the file) is stored in <id>, the
;;; given file name (perhaps minus path and extension) in <data>.

(defclass sndfile (linked-named-object)
  ((path :accessor path :type string :initarg :path :initform nil)
   ;; the sound's full duration
   (snd-duration :accessor snd-duration :initform nil)
   ;; the duration the user wants
   (duration :accessor duration :initarg :duration :initform nil)
   ;; this shouldn't be given as well as duration
   (end :accessor end :type number :initarg :end :initform nil)
   (channels :accessor channels :initform nil)
   ;; where we want to start in the file (secs).
   (start :accessor start :type number :initarg :start :initform 0.0)
   ;; a simple textual description slot useful for identifying sounds
   (description :accessor description :type string :initarg :description 
                :initform "")
   ;; this is a scaler that will be used in with-sound calls, if wanted.
   (amplitude :accessor amplitude :type number :initarg :amplitude 
              :initform 1.0)
   ;; some sounds have a prominent fundamental which can be used for
   ;; transposing to specific pitches.  Give this here either in the form of a
   ;; real freq or a note, which will then be converted.
   (frequency :accessor frequency :initarg :frequency :initform 'c4)
   ;; when duration is given, we have to update end and vice-versa.  This slot
   ;; tells us whether this was done and so avoids endless back-and-forths when
   ;; calling the setf methods.
   (data-consistent :accessor data-consistent :type boolean :initform nil)
   ;; for some purposes (like incrementing start-time) it's useful to know how
   ;; many times a sound will be used and has been used.
   (will-be-used :accessor will-be-used :type integer :initform 0)
   (has-been-used :accessor has-been-used :type integer :initform 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone ((sf sndfile))
  (clone-with-new-class sf 'sndfile))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone-with-new-class :around ((sf sndfile) new-class)
  (declare (ignore new-class))
  (let ((named-object (call-next-method)))
    (setf (slot-value named-object 'path) (basic-copy-object (path sf))
          (slot-value named-object 'snd-duration ) (snd-duration sf)
          (slot-value named-object 'duration ) (duration sf)
          (slot-value named-object 'end ) (end sf)
          (slot-value named-object 'channels ) (channels sf)   
          (slot-value named-object 'start ) (start sf)         
          (slot-value named-object 'amplitude ) (amplitude sf)         
          (slot-value named-object 'frequency )
          (basic-copy-object (frequency sf))
          (slot-value named-object 'data-consistent ) (data-consistent sf)     
          (slot-value named-object 'will-be-used ) (will-be-used sf)
          (slot-value named-object 'has-been-used ) (has-been-used sf))
    named-object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((sf sndfile) &rest initargs)
  (declare (ignore initargs))
  ;; just in case any of these slots were given as lists of mins, secs, ms
  (setf (slot-value sf 'start) (mins-secs-to-secs (start sf))
        (slot-value sf 'end) (mins-secs-to-secs (end sf))
        (slot-value sf 'duration) (mins-secs-to-secs (duration sf)))
  (when (and (duration sf) (end sf))
    (error "sndfile::initialize-instance: ~
            The Duration and End slots cannot both be specified! ~%~a" 
           sf))
  (update sf))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object :before ((sf sndfile) stream)
  (format stream "~%~%SNDFILE: path: ~a, ~
                    ~%         snd-duration: ~a, channels: ~a, frequency: ~a~
                    ~%         start: ~a, end: ~a, amplitude: ~a, duration ~a~
                    ~%         will-be-used: ~a, has-been-used: ~a~
                    ~%         data-consistent: ~a"
          (path sf) (snd-duration sf) (channels sf) (frequency sf) (start sf)
          (end sf) (amplitude sf) (duration sf) (will-be-used sf)
          (has-been-used sf) (data-consistent sf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* sndfile/stereo
;;; FUNCTION
;;; 
;;; 
;;; ARGUMENTS
;;; 
;;; 
;;; OPTIONAL ARGUMENTS
;;; 
;;; 
;;; RETURN VALUE
;;; 
;;; 
;;; EXAMPLE
#|

|#
;;; SYNOPSIS
(defmethod stereo ((sf sndfile))
;;; ****
  (= 2 (channels sf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* sndfile/reset-usage
;;; FUNCTION
;;; 
;;; 
;;; ARGUMENTS
;;; 
;;; 
;;; OPTIONAL ARGUMENTS
;;; 
;;; 
;;; RETURN VALUE
;;; 
;;; 
;;; EXAMPLE
#|

|#
;;; SYNOPSIS
(defmethod reset-usage ((sf sndfile))
;;; ****
  (setf (will-be-used sf) 0
        (has-been-used sf) 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf path) :after (value (sf sndfile))
  (declare (ignore value))
  (update sf))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf duration) :after (value (sf sndfile))
  (declare (ignore value))
  (setf (slot-value sf 'duration) (mins-secs-to-secs (duration sf)))
  (when (data-consistent sf)
    (setf (data-consistent sf) nil)
    (set-end sf)
    (update sf)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf end) :after (value (sf sndfile))
  (declare (ignore value))
  (setf (slot-value sf 'end) (mins-secs-to-secs (end sf)))
  (when (data-consistent sf)
    (setf (data-consistent sf) nil)
    (set-dur sf)
    (update sf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf start) :after (value (sf sndfile))
  (declare (ignore value))
  (setf (slot-value sf 'start) (mins-secs-to-secs (start sf)))
  (when (data-consistent sf)
    (setf (data-consistent sf) nil)
    (set-dur sf)
    (update sf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf frequency) :after (value (sf sndfile))
  (declare (ignore value))
  (update sf))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod update ((sf sndfile))
  ;; an id will generally only be given when two instances of the same file are
  ;; in the same list in a sndfile-palette, so in the usual case where it's not
  ;; specified set id to simply the given sound file name.
  (unless (id sf)
    (setf (id sf) (data sf)))
  (let ((given-freq (frequency sf)))
    (when (and given-freq (symbolp given-freq))
      (let ((freq (note-to-freq given-freq)))
        (unless freq
          (error "sndfile::update: Couldn't get the frequency for note ~a!!!"
                 given-freq))
        (setf (frequency sf) freq))))
  (let ((path (path sf)))
    (when path
      (unless (and path (probe-file path))
        (error "sndfile::update: ~
                Data slot of sndfile must be set to an existing sound file: ~a"
               path))
      (unless (data sf)
        (setf (data sf) path))
      #+clm
      (setf (snd-duration sf) #+clm(clm::sound-duration path)
            (channels sf) #+clm(clm::sound-chans path))
      (cond ((and (not (end sf)) (duration sf))
             (set-end sf))
            ((and (not (duration sf)) (end sf)) 
             (set-dur sf))
            ((and (not (end sf)) (not (duration sf)) (snd-duration sf))
             (setf (end sf) (snd-duration sf))
             (set-dur sf)))
      (let ((st (start sf))
            (end (end sf)))
        (when (< st 0)
          (error "sndfile::update: start < 0???: ~a" sf))
        (when (and end (<= end st))
          (error "sndfile::update: end <= start???: ~a" sf))
        (when (and (snd-duration sf) (> end (snd-duration sf)))
          (error "sndfile::update: ~
                  Given end point (or duration) is > sound duration: ~a ~%~a"
                 end sf)))
      (setf (data-consistent sf) t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod set-dur ((sf sndfile))
  (let ((end (end sf))
        (start (start sf)))
    (when (and start end)
      (setf (duration sf) (- end start)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod set-end ((sf sndfile))
  (let ((dur (duration sf))
        (start (start sf)))
    (when (and start dur)
      (setf (end sf) (+ start dur)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; If path is a list, then it's come from a sndfile-palette.  The first in the
;;; list is the full path as found by the sndfile-palette and the second is the
;;; list given by the user with the data slots to be used.

;;; ****f* sndfile/make-sndfile
;;; FUNCTION
;;; 
;;; 
;;; ARGUMENTS
;;; 
;;; 
;;; OPTIONAL ARGUMENTS
;;; 
;;; 
;;; RETURN VALUE
;;; 
;;; 
;;; EXAMPLE
#|

|#
;;; SYNOPSIS
(defun make-sndfile (path &key id data duration end (start 0.0) (frequency 'c4)
                          (amplitude 1.0))
;;; ****
  (if (listp path)
      (progn
        (let ((sf (make-instance 'sndfile
                                 :path (first path)
                                 :id (first (second path))))
              (slots (rest (second path))))
          (loop for slot in slots by #'cddr 
                and value in (cdr slots) by #'cddr do
                ;; we have to do this here because (setf (slot-value ... ))
                ;; doesn't call the setf methods...
                (case slot
                      (:duration (setf (duration sf) value))
                      (:end (setf (end sf) value))
                      (:frequency (setf (frequency sf) value))
                      (:start (setf (start sf) value))
                      (t (setf (slot-value sf (rm-package slot)) value))))
          sf))
    (make-instance 'sndfile :id id :data data :path path :duration duration
                   :frequency frequency :end end :start start
                   :amplitude amplitude)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EOF sndfile.lsp
