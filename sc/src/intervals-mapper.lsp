;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* sclist/intervals-mapper
;;; NAME 
;;; intervals-mapper
;;;
;;; File:             intervals-mapper.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> sclist ->
;;;                   intervals-mapper  
;;;
;;; Version:          0.9.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Implementation of a scale object that can initialize its
;;;                   complete pitch list from the interval structure of a
;;;                   given list of notes.  Given the scale, it's a synch to
;;;                   generate note sequences based on note offset patterns
;;;                   e.g.
;;;                   (let ((s (make-intervals-mapper 'c0 '(d e gs as d ef g a 
;;;                                           bf cs d ef gf)))
;;;                         (pat '(-1 2 4  3 6 -2 -1 2 6  7 3 6 2)))
;;;                     (loop for p in pat collect
;;;                          (data (intervals-mapper-note s p 4))))
;;;                   -> (A3 EF4 F4 E4 BF4 F3 A3 EF4 BF4 D5 E4 BF4 EF4)
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    August 3rd 2010 Edinburgh
;;;
;;; $$ Last modified: 16:24:38 Wed Apr 18 2012 BST
;;;
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

(in-package :slippery-chicken)

;;; we pass a list of pitches to initialize; this will become the data slot and
;;; from it we'll extract the intervals stored in steps.
(defclass intervals-mapper (sclist)
  ((steps :accessor steps :type list :initform nil)
   ;; tonic remains a simple note symbol e.g. 'c4
   (tonic ::accessor tonic :type symbol :initarg :tonic :initform 'c4)
   (scale-pitches :accessor scale-pitches :type list :initform nil)
   ;; midi notes of all cs: an octave index of 4 will return middle C (60)
   (cs :accessor cs :type list :allocation :class
       :initform (loop for i from 12 below 127 by 12 collect i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((im intervals-mapper) &rest initargs)
  (declare (ignore initargs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone ((im intervals-mapper))
  (clone-with-new-class im 'intervals-mapper))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* intervals-mapper/get-pitch-symbols
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
(defmethod get-pitch-symbols ((im intervals-mapper))
;;; ****
  (loop for p in (scale-pitches im) collect (data p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object :before ((im intervals-mapper) stream)
  (format stream "~&INTERVALS-MAPPER: steps: ~a, ~&scale-pitches ~
                              (pitch objects): ~a~&tonic: ~a"
          (steps im) (pitch-list-to-symbols (scale-pitches im))
          (tonic im)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone-with-new-class :around ((im intervals-mapper) new-class)
  (declare (ignore new-class))
  (let ((scl (call-next-method)))
    ;; the data list is copied by the named-object class clone method
    (setf (slot-value scl 'steps) (my-copy-list (steps scl))
          (slot-value scl 'scale-pitches) (my-copy-list (scale-pitches scl)))
    scl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod verify-and-store ((im intervals-mapper))
  (get-steps im)
  (get-scale im (tonic im)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; extracts the interval structure of the pitch symbol list passed as data at
;;; init. 
;;; ****m* intervals-mapper/get-steps
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
(defmethod get-steps ((im intervals-mapper))
;;; ****
  (let* ((oct1 (loop for n in (data im) collect
                    (read-from-string (format nil "~a~a" n 1))))
         (steps (loop for n1 in oct1 and n2 in (cdr oct1) 
                   for diff = (- (note-to-midi n2) (note-to-midi n1))
                   collect (if (< diff 0) (+ 12 diff) diff))))
    (setf (steps im) steps)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* intervals-mapper/get-scale
;;; FUNCTION
;;; get-scale: uses the <steps> slot to create a scale starting on the given
;;; note and extending up to MIDI note 127.  The scale will only repeat at
;;; octaves if such is the structure of the notes passed at initialisation.
;;; This is usually only called automatically at initialisation etc.
;;;
;;; ARGUMENTS 
;;; - the intervals-mapper object
;;; - the note to begin the scale on (as a symbol e.g. 'c0) and to store
;;;   in the tonic slot.
;;; 
;;; RETURN VALUE  
;;; a list of the pitch objects in the scale; these are also stored in the
;;; scale-pitches slot. 
;;; 
;;; SYNOPSIS
(defmethod get-scale ((im intervals-mapper) start-note)
;;; ****
  (let* ((midi-start (note-to-midi start-note))
         (steps nil) 
         (midi (loop with current = midi-start while (< current 128)
                  do (unless steps
                       (setf steps (copy-list (steps im))))
                  collect current
                  do (incf current (pop steps))))
         (pitches (loop for m in midi collect (make-pitch (midi-to-note m)))))
    (setf (tonic im) start-note
          (scale-pitches im) pitches)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* intervals-mapper/intervals-mapper-note
;;; FUNCTION
;;; intervals-mapper-note: Get the pitch object (or if <nth> is t, the position
;;; in the list of scale notes across the complete range) for the note with
;;; <degree> (1-based) in <octave>.  As there's no concept of a tonic repeating
;;; at octaves, degree 1 in the given octave is simply the first note >= the C
;;; in this octave.  NB If you pass <tonic> this will change the instance's
;;; tonic and re-calculate scale-pitches accordingly before getting the pitch.
;;; 
;;; ARGUMENTS 
;;; - the intervals-mapper object
;;; - the degree (1-based) of the scale, counting from the first note of the
;;;   scale above or on the C in the given octave.  This can of course be
;;;   negative. 
;;; - the octave we want the pitch from (middle C = octave 4)
;;; - (key :tonic default nil): the new starting note for the scale (usually
;;;   the lowest note the scale should start on).
;;; - (key :nth default nil): return the position of the requested degree in
;;;   the whole scale list, instead of the pitch object.
;;; 
;;; RETURN VALUE  
;;; either the pitch object or its position in the scale list.
;;;
;;; EXAMPLE
;;; 
;;; (let ((s (make-intervals-mapper 'c0 '(d e gs as d ef g a bf cs d ef gf)))
;;;       (pat '(-1 2 4  3 6 -2 -1 2 6  7 3 6 2)))
;;;   (loop for p in pat collect
;;;        (data (intervals-mapper-note s p 4))))
;;;  --> (A3 EF4 F4 E4 BF4 F3 A3 EF4 BF4 D5 E4 BF4 EF4)
;;;
;;; SYNOPSIS
(defmethod intervals-mapper-note ((im intervals-mapper) degree octave
                                  &key tonic nth)
;;; ****
  (when (and tonic (not (eq (tonic im) tonic)))
    (get-scale im tonic))
  (let* ((this-c (nth octave (cs im)))
         (first-note-pos (position-if #'(lambda (x) (>= (midi-note x) this-c))
                                      (scale-pitches im)))
         (pos (+ degree -1 first-note-pos))
         (pitch (when (>= pos 0) (nth pos (scale-pitches im)))))
    (unless pitch 
      (error "~a~&intervals-mapper::intervals-mapper-note: off the scale: ~%~
              degree ~a, octave ~a"
             im degree octave))
    (if nth pos pitch)))
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* intervals-mapper/intervals-mapper-degree
;;; FUNCTION
;;; intervals-mapper-degree: find a given note and return the degree in a given
;;; octave.  Strictly speaking, we're searching for a particular pitch class
;;; e.g. given 'Ds1, find the degree of the first D# in, say, octave 6.
;;; 
;;; ARGUMENTS 
;;; - the intervals-mapper instance
;;; - the pitch whose class we're looking for
;;; - the octave to search
;;; - (optional default nil) whether to return the position in the scale list
;;;   instead of the degree (T or NIL) 
;;; 
;;; RETURN VALUE  
;;; integer: the degree, counting from 1, of the found note.
;;; 
;;; EXAMPLE
;;; 
;;; DATE 14.8.2010
;;; 
;;; SYNOPSIS
(defmethod intervals-mapper-degree ((im intervals-mapper) pitch octave
                                    &optional return-nth)
;;; ****
  (loop for start from (intervals-mapper-note im 1 octave :nth t)
     for degree from 1
     for this-pitch = (nth start (scale-pitches im))
     while this-pitch
     do
       ;; (print (data this-pitch))
     (when (pitch-class-eq pitch this-pitch t)
       ;; (print (data this-pitch))
       ;; (print (data (nth start (scale-pitches im))))
       (return (if return-nth start degree)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****f* intervals-mapper/make-intervals-mapper
;;; FUNCTION
;;; make-intervals-mapper: returns an intervals-mapper object starting on the
;;; given <tonic> and using the interval structure given in notes.  NB The
;;; notes aren't necessarily used, rather their interval structure is simply
;;; mapped onto the starting-note and repeated upwards until we've reached MIDI
;;; note 127.  NB The scale will only repeat at octaves if that is the interval
;;; structure of <notes>
;;; 
;;; ARGUMENTS 
;;; - the starting note, as a pitch symbol (e.g. 'c0)
;;; - a pitch symbol list that provides the interval structure for the scale
;;; 
;;; RETURN VALUE  
;;; an intervals-mapper object
;;; 
;;; EXAMPLE
;;; A non-octave repeating scale:
;;; (make-intervals-mapper 'c0 '(d e gs as d ef g a bf cs d ef gf))
;;; 
;;; SYNOPSIS
(defun make-intervals-mapper (tonic notes)
;;; ****
  (make-instance 'intervals-mapper :data notes :tonic tonic))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EOF intervals-mapper.lsp

