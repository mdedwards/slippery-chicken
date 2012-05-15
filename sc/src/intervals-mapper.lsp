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
;;; Version:          1.0.0-beta1
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

;;; SAR Thu May  3 18:15:56 BST 2012: Added robodoc entry

;;; ****m* intervals-mapper/get-pitch-symbols
;;; FUNCTION
;;; Get the pitches contained in a given intervals-mapper object returned as a
;;; list of note-name symbols.
;;; 
;;; ARGUMENTS
;;; - An intervals-mapper object.
;;; 
;;; RETURN VALUE
;;; A list of note-name pitch symbols.
;;; 
;;; EXAMPLE
#|
(let ((im (make-intervals-mapper 'c0 '(d e gs as d ef g a bf cs d ef gf))))
  (get-pitch-symbols im))

=> (C0 D0 FS0 AF0 C1 CS1 F1 G1 AF1 B1 C2 CS2 E2 FS2 BF2 C3 E3 F3 A3 B3 C4 EF4
    E4 F4 AF4 BF4 D5 E5 AF5 A5 CS6 EF6 E6 G6 AF6 A6 C7 D7 FS7 AF7 C8 CS8 F8 G8
    AF8 B8 C9 CS9 E9 FS9)

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

;;; SAR Thu May  3 18:21:13 BST 2012: Added robodoc entry

;;; ****m* intervals-mapper/get-steps
;;; FUNCTION
;;; Extract the interval structure of the list of note-name pitch symbols
;;; passed as the data to the instance of the intervals-mapper object upon
;;; initialization. The interval structure is returned as a list of semitone
;;; values. 
;;; 
;;; ARGUMENTS
;;; - An intervals-mapper object.
;;; 
;;; RETURN VALUE
;;; A list of integers that are the numbers of semitones between each
;;; consecutive pitch in the original data list.
;;; 
;;; EXAMPLE
#|
(let ((im (make-intervals-mapper 'c0 '(d e gs as d ef g a bf cs d ef gf))))
  (get-steps im))

=> (2 4 2 4 1 4 2 1 3 1 1 3)

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

;;; SAR Thu May  3 18:27:43 BST 2012: Conforming robodoc entry

;;; ****m* intervals-mapper/get-scale
;;; FUNCTION

;;; Create a scale (sequence of pitches) beginning with the specified starting
;;; note and extending up to MIDI note 127 by cycling through the interval
;;; structure of the STEPS slot.
;;;
;;; The scale will only repeat at octaves if the interval structure of the list
;;; of pitches passed at initialisation creates that result. 
;;;
;;; NB: This method is usually only called automatically at initialisation.
;;;
;;; ARGUMENTS 
;;; - An intervals mapper object.
;;; - A note-name pitch symbol (e.g. 'c0) that is the pitch on which to begin
;;;   the scale, and which is to be stored in the TONIC slot.
;;; 
;;; RETURN VALUE  
;;; A list of the pitch objects in the new scale. These are also stored in the
;;; SCALE-PITCHES slot. 
;;;
;;; EXAMPLE
#|
(let ((im (make-intervals-mapper 'c0 '(d e gs as d ef g a bf cs d ef gf))))
  (pitch-list-to-symbols (get-scale im 'd4)))

=> (D4 E4 AF4 BF4 D5 EF5 G5 A5 BF5 CS6 D6 EF6 FS6 AF6 C7 D7 FS7 G7 B7 CS8 D8 F8
    FS8 G8 BF8 C9 E9 FS9)

|#
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

;;; SAR Thu May  3 18:48:38 BST 2012: Conformed robodoc entry

;;; ****m* intervals-mapper/intervals-mapper-note
;;; FUNCTION
;;; Get the pitch object that constitutes the specified scale degree of the
;;; specified octave within an intervals-mapper object; or, if keyword argument
;;; <nth> is set to T, get the position of this pitch object within the full
;;; range of the intervals-mapper object's complete scale.
;;;
;;; As there is no concept of a tonic that repeats at octaves, degree 1 in any
;;; given octave is simply the first note >= the C in that octave.  
;;;
;;; If a note-name pitch symbol is given for the keyword argument <tonic>, the
;;; given intervals-mapper object's TONIC slot will be changed, and its
;;; scale-pitches will be recalculated accordingly before getting the pitch.
;;; 
;;; ARGUMENTS 
;;; - An intervals-mapper object.
;;; - An integer that is the scale degree (1-based) of the desired pitch within
;;;   the specified octave, counting from the first note of the scale above or
;;;   on the C in that octave. If this number is higher than the number of
;;;   pitches in the span of an octave, a pitch from a higher octave will
;;;   accordingly be returned. Similarly, a negative number can also be given
;;;   here to indicate that the pitch is to be collected from that many degrees
;;;   below the specified octave.
;;; - An integer that indicates the octave from which the pitch is to be
;;;   returned (e.g. 4 for the octave starting on middle C, 5 for the octave
;;;   starting on the C above that etc.)
;;;
;;; OPTIONAL ARGUMENTS
;;; keyword arguments
;;; - :tonic. NIL or a note-name pitch symbol that is the new starting note for
;;;   above which the intervals-mapper scale is to be re-mapped before the
;;;   pitch is returned. This will usually be the lowest pitch on which the
;;;   scale should start. If NIL, no changes will be made to the object's
;;;   existing scale pitches before returning the desired pitch. Default = NIL.
;;; - :nth. T or NIL to indicate whether instead of returning the pitch object
;;;   itself, the method should return an integer that is the position of that
;;;   pitch within the full span of the interval-mapper object's complete
;;;   scale. T = return the position. Default = NIL.
;;; 
;;; RETURN VALUE  
;;; A pitch object by default, or its position in the scale list if the :nth
;;; argument is set to T.
;;;
;;; EXAMPLE
#|
;;; Returns a pitch object by default
(let ((im (make-intervals-mapper 'c0 '(d e gs as d ef g a bf cs d ef gf))))
  (intervals-mapper-note im 3 4))

=> 
PITCH: frequency: 329.628, midi-note: 64, midi-channel: 0 
       pitch-bend: 0.0 
       degree: 128, data-consistent: T, white-note: E4
       nearest-chromatic: E4
       src: 1.2599211, src-ref-pitch: C4, score-note: E4 
       qtr-sharp: NIL, qtr-flat: NIL, qtr-tone: NIL,  
       micro-tone: NIL, 
       sharp: NIL, flat: NIL, natural: T, 
       octave: 4, c5ths: 0, no-8ve: E, no-8ve-no-acc: E
       show-accidental: T, white-degree: 30, 
       accidental: N, 
       accidental-in-parentheses: NIL, marks: NIL
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: E4, tag: NIL, 
data: E4

;;; Used with negative degree numbers
(let ((im (make-intervals-mapper 'c0 '(d e gs as d ef g a bf cs d ef gf))))
  (data (intervals-mapper-note im -3 2)))

=> F1

;;; Use with a new tonic and setting nth to T to return the position 
(let ((im (make-intervals-mapper 'c0 '(d e gs as d ef g a bf cs d ef gf))))
  (intervals-mapper-note im 11 5 :tonic 'd1 :nth t))

=> 29

|#
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

;;; SAR Thu May  3 19:19:34 BST 2012: Conforming robodoc entry

;;; ****m* intervals-mapper/intervals-mapper-degree
;;; DATE
;;; 14-Aug-2010
;;;
;;; FUNCTION

;;; Return the scale degree number of a specified pitch class in relation to a
;;; specified octave within the given intervals-mapper object.
;;;
;;; To determine the scale degree number, this method begins at the first pitch
;;; >= C in the specified octave and passes consecutively through each
;;; subsequent pitch in the interval-mapper object's full scale, counting each
;;; step until it first encounters the pitch class of the specified pitch.
;;;
;;; If there are no more instances of the specified pitch class, the method
;;; returns NIL.
;;;
;;; The method takes as its pitch class a pitch object, which includes an
;;; octave indicator. For the purposes of this method, solely the pitch-class
;;; name is extracted from the pitch object.
;;; 
;;; ARGUMENTS 
;;; - An intervals-mapper object.
;;; - An instance of a pitch object whose pitch class is being sought.
;;; - An integer that is the octave in relationship to which the scale degree
;;;   is sought.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indication whether to return the position of the found pitch
;;;   object within the complete scale list of the given intervals-mapper
;;;   object rather than the scale degree. T = return the position. 
;;;   Default = NIL.
;;; 
;;; RETURN VALUE  
;;; Returns an integer that is either the scale degree of the specified pitch
;;; class in relation to the specified octave, counting from 1, or the position
;;; of the found pitch object within the interval-mapper object's complete
;;; scale. 
;;;
;;; Returns NIL if no instances of the specified pitch class are found.
;;; 
;;; EXAMPLE
#|
;;; The desired pitch class BF is found within the specified octave
(let ((im (make-intervals-mapper 'c0 '(d e gs as d ef g a bf cs d ef gf))))
  (intervals-mapper-degree im (make-pitch 'bf4) 4))

=> 6

;;; The desired pitch class B is found outside of the specified octave
(let ((im (make-intervals-mapper 'c0 '(d e gs as d ef g a bf cs d ef gf))))
  (intervals-mapper-degree im (make-pitch 'b4) 5))

=> 20

;;; Return the position instead
(let ((im (make-intervals-mapper 'c0 '(d e gs as d ef g a bf cs d ef gf))))
  (intervals-mapper-degree im (make-pitch 'b4) 5 t))

=> 45

;;; The desired pitch class BF is not present in any of the octaves beginning
;;; including and above the specified octave

(let ((im (make-intervals-mapper 'c0 '(d e gs as d ef g a bf cs d ef gf))))
  (intervals-mapper-degree im (make-pitch 'bf4) 5))

=> NIL


|#
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

;;; SAR Thu May  3 17:24:55 BST 2012: Added robodoc entry

;;; ****f* intervals-mapper/make-intervals-mapper
;;; FUNCTION
;;; Returns an intervals-mapper object starting with the specified pitch
;;; ('tonic') and using the interval structure of the specified list of
;;; pitches, creating a complete pitch list from the interval structure of list
;;; of pitches.
;;;
;;; NB The pitches specified aren't necessarily used in the resulting complete
;;;    pitch list; rather, their interval structure is mapped above the
;;;    specified starting pitch and repeated upwards until the method reaches
;;;    MIDI note 127. 
;;;
;;; NB The scale will only repeat at octaves if that is the interval structure
;;;    of the list of pitches it is passed.
;;; 
;;; ARGUMENTS 
;;; - A note-name pitch symbol that is the starting pitch, (e.g. 'c0).
;;; - A list of note-name pitch symbols that provides the interval structure
;;;   for the resulting scale.
;;; 
;;; RETURN VALUE  
;;; An intervals-mapper object.
;;; 
;;; EXAMPLE
#|
;;; A scale without repeating octaves:
(make-intervals-mapper 'c0 '(d e gs as d ef g a bf cs d ef gf))

=> 
INTERVALS-MAPPER: steps: (2 4 2 4 1 4 2 1 3 1 1 3), 
scale-pitches (pitch objects): (C0 D0 FS0 AF0 C1 CS1 F1 G1 AF1 B1 C2 CS2 E2 FS2
                                BF2 C3 E3 F3 A3 B3 C4 EF4 E4 F4 AF4 BF4 D5 E5
                                AF5 A5 CS6 EF6 E6 G6 AF6 A6 C7 D7 FS7 AF7 C8
                                CS8 F8 G8 AF8 B8 C9 CS9 E9 FS9)
tonic: C0
SCLIST: sclist-length: -1, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: NIL, tag: NIL, 
data: (D E GS AS D EF G A BF CS D EF GF)

|#
;;; 
;;; SYNOPSIS
(defun make-intervals-mapper (tonic notes)
;;; ****
  (make-instance 'intervals-mapper :data notes :tonic tonic))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EOF intervals-mapper.lsp

