;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* sclist/chord
;;; NAME 
;;; chord
;;;
;;; File:             chord.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> sclist -> chord
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Implementation of the chord class that is simply an
;;;                   sclist whose data is a list of pitch instances.   
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    July 28th 2001
;;;
;;; $$ Last modified: 17:38:46 Wed Feb  8 2012 GMT
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

(defclass chord (sclist)
  ;; whether the pitches in the chord are sorted (ascending) at initialization.
  ((auto-sort :accessor auto-sort :type boolean :initarg :auto-sort 
              :initform t)
   ;; 8.2.11 added slot to say whether there are microtones in the chord or not
   (micro-tone :accessor micro-tone :type boolean :initform nil)
   ;; dynamics, accents etc. exactly the code used by cmn.  These will simply
   ;; be copied over to the event when the chord is bound to an event. 
   (marks :accessor marks :type list :initarg :marks 
              :initform nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* chord/delete-marks
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
(defmethod delete-marks ((c chord))
;;; ****
  (setf (marks c) nil)
  (loop for pitch in (data c) do (delete-marks pitch)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((c chord) &rest initargs)
  (declare (ignore initargs))
  (set-micro-tone c)
  (when (auto-sort c)
    (sort-pitches c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod set-micro-tone ((c chord))
  (setf (slot-value c 'micro-tone)
        (loop for p in (data c) do (when (micro-tone p) (return t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf micro-tone) (value (c chord))
  (declare (ignore value))
  (error "chord::(setf micro-tone): micro-tone slot cannot be setf'd; ~
          it is handled automatically according to pitches in chord"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object :before ((c chord) stream)
  (format stream "~%CHORD: auto-sort: ~a, marks: ~a, micro-tone: ~a"
          (auto-sort c) (marks c) (micro-tone c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod verify-and-store :after ((c chord))
  (let ((data (data c)))
    (when (and data (not (typep (first data) 'pitch)))
      (loop for p in data and i from 0 do
            (setf (nth i data) (make-pitch p))))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone ((c chord))
  (clone-with-new-class c 'chord))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone-with-new-class :around ((c chord) new-class)
  (declare (ignore new-class))
  (let ((sclist (call-next-method)))
    (setf (slot-value sclist 'auto-sort) (auto-sort c)
          (slot-value sclist 'micro-tone) (micro-tone c)
          (slot-value sclist 'marks) (my-copy-list (marks c)))
    sclist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Wed Feb 22 17:39:04 GMT 2012: Added robodoc entry

;;; ****m* chord/set-midi-channel
;;; FUNCTION
;;; Set the MIDI channel of the pitch objects in a given chord object to the
;;; specified values. 
;;;
;;; ARGUMENTS 
;;; - A chord object.
;;; - An integer that is to be the MIDI channel for chromatic pitches in the
;;;   given chord object.
;;; - An integer that is to be the MIDI channel for microtonal pitches in the
;;;   given chord object.
;;; 
;;; RETURN VALUE  
;;; Always returns NIL.
;;; 
;;; EXAMPLE
#|
;; Returns NIL
(let ((chrd (make-chord '(c4 e4 gqs4 bqf4 d5 f5)
			  :midi-channel 11
			  :microtones-midi-channel 12)))
  (set-midi-channel chrd 3 4))

=> NIL

;; Print the value of the MIDI slot for each of the pitch objects contained in
;; the chord object before and after setting
(let ((chrd (make-chord '(c4 e4 gqs4 bqf4 d5 f5)
			  :midi-channel 11
			  :microtones-midi-channel 12)))
  (print (loop for p in (data chrd) collect (midi-channel p)))
  (set-midi-channel chrd 3 4)
  (print (loop for p in (data chrd) collect (midi-channel p))))

=>
(11 11 12 12 11 11) 
(3 3 4 4 3 3)

|#
;;;
;;; SYNOPSIS
(defmethod set-midi-channel ((c chord) midi-channel microtones-midi-channel)
;;; ****
  (loop for pitch in (data c) do
       (set-midi-channel pitch midi-channel microtones-midi-channel)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Wed Feb 22 17:52:24 GMT 2012: Deleted MDE's comment here as it is taken
;;; nearly verbatim into the doc below.

;;; SAR Wed Feb 22 17:52:14 GMT 2012: Added robodoc entry

;;; ****m* chord/get-midi-channel
;;; FUNCTION
;;; Get the MIDI channel of the first pitch object contained in a given chord
;;; object. 
;;; 
;;; NB: This method returns only the midi-channel of the first pitch object in
;;;     the chord object's data list.  
;;; 
;;; ARGUMENTS 
;;; - A chord object.
;;; 
;;; RETURN VALUE  
;;; An integer.
;;; 
;;; EXAMPLE
#|
(let ((chrd (make-chord '(c4 e4 gqs4 bqf4 d5 f5)
			:midi-channel 11
			:microtones-midi-channel 12)))
  (get-midi-channel chrd))

=> 11

|#
;;; 
;;; SYNOPSIS
(defmethod get-midi-channel ((c chord))
;;; ****
  (midi-channel (first (data c))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+cm-2
;;; ****m* chord/output-midi-note
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
(defmethod output-midi-note ((c chord) time amplitude duration)
;;; ****
  (loop for p in (data c) collect
        (output-midi-note p time amplitude duration)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ref is 1-based and counts from the lowest note up.

;;; SAR Wed Feb 22 17:56:55 GMT 2012: Added robodoc info

;;; ****m* chord/get-pitch
;;; FUNCTION
;;; Get the pitch object located at the specified index within the given chord
;;; object. The <ref> argument is 1-based.
;;; 
;;; ARGUMENTS 
;;; - A chord object.
;;; - An integer that is the index of the pitch object sought within the data
;;;   list of the given chord object.
;;; 
;;; RETURN VALUE  
;;; A pitch object.
;;; 
;;; EXAMPLE
#|

(let ((chrd (make-chord '(c4 e4 gqs4 bqf4 d5 f5)
			:midi-channel 11
			:microtones-midi-channel 12)))
  (get-pitch chrd 3))

=> 
PITCH: frequency: 403.482, midi-note: 67, midi-channel: 12 
       pitch-bend: 0.5 
       degree: 135, data-consistent: T, white-note: G4
       nearest-chromatic: G4
       src: 1.5422108173370361, src-ref-pitch: C4, score-note: GS4 
       qtr-sharp: 1, qtr-flat: NIL, qtr-tone: 1,  
       micro-tone: T, 
       sharp: NIL, flat: NIL, natural: NIL, 
       octave: 4, c5ths: 0, no-8ve: GQS, no-8ve-no-acc: G
       show-accidental: T, white-degree: 32, 
       accidental: QS, 
       accidental-in-parentheses: NIL, marks: NIL
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: GQS4, tag: NIL, 
data: GQS4

|#
;;; 
;;; SYNOPSIS
(defmethod get-pitch ((c chord) ref)
;;; ****
  (get-nth (1- ref) c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* chord/add-mark
;;; FUNCTION
;;; add-mark:
;;;
;;; 
;;; 
;;; ARGUMENTS 
;;; 
;;; 
;;; RETURN VALUE  
;;; 
;;; 
;;; EXAMPLE
;;; 
;;; 
;;; DATE
;;; 
;;; 
;;; SYNOPSIS
(defmethod add-mark ((c chord) mark &optional warn-rest)
;;; ****
  (declare (ignore warn-rest))
  (push mark (marks c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Wed Feb 22 18:05:57 GMT 2012: Added robodoc entry

;;; ****m* chord/get-pitch-symbols
;;; FUNCTION
;;; Return the data of the pitch objects from a given chord object as a list of
;;; note-name symbols.
;;; 
;;; ARGUMENTS 
;;; - A chord object.
;;; 
;;; RETURN VALUE  
;;; A list of note-name symbols.
;;; 
;;; EXAMPLE
#|
(let ((chrd (make-chord '(c4 e4 gqs4 bqf4 d5 f5)
			:midi-channel 11
			:microtones-midi-channel 12)))
  (get-pitch-symbols chrd))

=> (C4 E4 GQS4 BQF4 D5 F5)
|#
;;; 
;;; 
;;; SYNOPSIS
(defmethod get-pitch-symbols ((c chord))
;;; ****
  (loop for p in (data c) collect (id p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* chord/no-accidental
;;; FUNCTION
;;; don't show any accidentals when writing a score; none in parentheses either
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
(defmethod no-accidental ((c chord))
;;; ****
  (loop for p in (data c) do
        (no-accidental p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Wed Feb 22 18:10:21 GMT 2012: Added robodoc entry

;;; ****m* chord/chord-equal
;;; FUNCTION
;;; Test to see if two chords are equal.
;;;
;;; NB: Two unsorted chord objects that contain the exact same pitch objects in
;;;     a different order will not be considered equal and will return NIL.
;;;
;;; NB: Equality is tested on pitch content only, not on, for example, the
;;;     values of the MIDI slots of those pitch objects etc.
;;; 
;;; ARGUMENTS 
;;; - A first chord object.
;;; - A second chord object.
;;; 
;;; RETURN VALUE  
;;; T or NIL. T if the pitch content of the chords is equal, otherwise NIL.
;;; 
;;; EXAMPLE
#|
;; Two chords are equal
(let ((chrd1 (make-chord '(c4 e4 gqs4 bqf4 d5 f5)
			:midi-channel 11
			:microtones-midi-channel 12))
      (chrd2 (make-chord '(c4 e4 gqs4 bqf4 d5 f5)
			:midi-channel 11
			:microtones-midi-channel 12)))
  (chord-equal chrd1 chrd2))

=> T

;; Chord objects with the same pitch objects in a different order are unequal
(let ((chrd1 (make-chord '(c4 e4 gqs4 bqf4 d5 f5)
			:midi-channel 11
			:microtones-midi-channel 12))
      (chrd2 (make-chord '(e4 c4 gqs4 bqf4 d5 f5)
			:midi-channel 11
			:microtones-midi-channel 12
			:auto-sort nil)))
  (chord-equal chrd1 chrd2))

=> NIL

;; Only the pitch content is compared. Content of other slots is irrelevant. 
(let ((chrd1 (make-chord '(e4 c4 gqs4 bqf4 d5 f5)
			 :midi-channel 11
			 :microtones-midi-channel 12))
      (chrd2 (make-chord '(e4 c4 gqs4 bqf4 d5 f5)
			 :midi-channel 7
			 :microtones-midi-channel 8)))
  (chord-equal chrd1 chrd2))

=> T

|#
;;; 
;;; SYNOPSIS
(defmethod chord-equal ((c1 chord) (c2 chord))
;;; ****
  (equal (get-pitch-symbols c1) (get-pitch-symbols c2)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; usually a chord is auto-sorted so we can return (first (data c)) but check
;;; to make sure. 

;;; SAR Wed Feb 22 18:24:38 GMT 2012: Added robodoc entry

;;; ****m* chord/lowest
;;; FUNCTION
;;; Return the pitch object from the given chord object that has the lowest
;;; pitch data. The method can handle chord objects whose pitches have not been
;;; auto-sorted from low to high.
;;; 
;;; ARGUMENTS 
;;; - A chord object.
;;; 
;;; RETURN VALUE  
;;; A pitch object.
;;; 
;;; EXAMPLE
#|
;; Returns the pitch object of the lowest pitch despite not being sorted
(let ((chrd (make-chord '(e4 c4 gqs4 bqf4 d5 f5)
			:midi-channel 11
			:microtones-midi-channel 12
			:auto-sort nil)))
  (lowest chrd))

=> 
PITCH: frequency: 261.626, midi-note: 60, midi-channel: 11 
       pitch-bend: 0.0 
       degree: 120, data-consistent: T, white-note: C4
       nearest-chromatic: C4
       src: 1.0, src-ref-pitch: C4, score-note: C4 
       qtr-sharp: NIL, qtr-flat: NIL, qtr-tone: NIL,  
       micro-tone: NIL, 
       sharp: NIL, flat: NIL, natural: T, 
       octave: 4, c5ths: 0, no-8ve: C, no-8ve-no-acc: C
       show-accidental: T, white-degree: 28, 
       accidental: N, 
       accidental-in-parentheses: NIL, marks: NIL
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: C4, tag: NIL, 
data: C4

|#
;;; 
;;; SYNOPSIS
(defmethod lowest ((c chord))
;;; ****
  (loop
     with first = (first (data c))
     with result = first
     for p in (rest (data c)) 
     do
     (when (pitch< p first)
       (setf result p))
     finally (return result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Wed Feb 22 18:30:21 GMT 2012: Added robodoc entry

;;; ****m* chord/highest
;;; FUNCTION
;;; Return the pitch object from the given chord object that has the highest
;;; pitch data. 
;;;
;;; NB: As opposed to the "lowest" method, this method cannot handle chord
;;;     objects whose pitches have not been auto-sorted from low to high.
;;; 
;;; ARGUMENTS 
;;; - A chord object.
;;; 
;;; RETURN VALUE  
;;; A pitch object
;;; 
;;; EXAMPLE
#|
;; Returns the last pitch object of a chord object
(let ((chrd (make-chord '(e4 c4 gqs4 bqf4 d5 f5)
			:midi-channel 11
			:microtones-midi-channel 12)))
  (highest chrd))

=> 
PITCH: frequency: 698.456, midi-note: 77, midi-channel: 11 
       pitch-bend: 0.0 
       degree: 154, data-consistent: T, white-note: F5
       nearest-chromatic: F5
       src: 2.669679641723633, src-ref-pitch: C4, score-note: F5 
       qtr-sharp: NIL, qtr-flat: NIL, qtr-tone: NIL,  
       micro-tone: NIL, 
       sharp: NIL, flat: NIL, natural: T, 
       octave: 5, c5ths: 0, no-8ve: F, no-8ve-no-acc: F
       show-accidental: T, white-degree: 38, 
       accidental: N, 
       accidental-in-parentheses: NIL, marks: NIL
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: F5, tag: NIL, 
data: F5

;; Is not capable of returning the highest pitch object from chord objects that
;; have not been auto-sorted
(let ((chrd (make-chord '(e4 c4 gqs4 bqf4 f5 d5)
			:midi-channel 11
			:microtones-midi-channel 12
			:auto-sort nil)))
  (data (highest chrd)))

=> D5

|#
;;; 
;;; SYNOPSIS
(defmethod highest ((c chord))
;;; ****
  (loop
      with first = (first (data c))
      with result = first
      for p in (rest (data c)) 
      do
        (when (pitch> p first)
          (setf result p))
      finally (return result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 
;;; The ignore field is there because of the transpose method in tl-set
;;; Returns a clone of the current chord rather than replacing data

;;; ****m* chord/transpose
;;; FUNCTION
;;;
;;; 
;;; 
;;; ARGUMENTS 
;;; 
;;; 
;;; RETURN VALUE  
;;; 
;;; 
;;; EXAMPLE
;;; 
;;; 
;;; DATE
;;; 
;;; 
;;; SYNOPSIS
(defmethod transpose ((c chord) semitones &key ignore1 ignore2 ignore3)
;;; ****
  (declare (ignore ignore1) (ignore ignore2) (ignore ignore3))
  (let ((result (clone c)))
    (setf (data result)
      (loop 
          for pitch in (data c)
          for new = (transpose pitch semitones)
                    ;; copy over the cmn marks (like special note heads etc.)
          do (setf (marks new) (my-copy-list (marks pitch)))
          collect new))
    ;; 8.2. 11: got to this here too now
    (set-micro-tone result)
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-lp-data ((c chord) &optional ignore1 ignore2)
  (declare (ignore ignore1 ignore2))
  (let ((result '(>))
        (lpchord (loop for pitch in (data c) collect (get-lp-data pitch))))
    (setf result (cons '< (append lpchord result)))
    (list-to-string result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+cmn
(defmethod get-cmn-data ((c chord) &optional (force-natural nil)
                                             ignore1 ignore2 ignore3 ignore4
                                             ignore5 ignore6 ignore7)
  (declare (ignore ignore1 ignore2 ignore3 ignore4 ignore5 ignore6 ignore7))
  (loop for pitch in (data c) collect (get-cmn-data pitch force-natural nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; usually we auto-sort so this shouldn't need to be called directly; it's
;;; called in init

;;; SAR Wed Feb 22 18:43:23 GMT 2012: Added robodoc entry

;;; ****m* chord/sort-pitches
;;; FUNCTION
;;; Sort the pitch objects contained within a given chord object and return
;;; them as a list of pitch objects. 
;;;
;;; As an optional argument, 'ascending or 'descending can be given to indicate
;;; whether to sort from low to high or high to low.
;;; 
;;; ARGUMENTS 
;;; - A chord object. 
;;;
;;; OPTIONAL ARGUMENTS
;;; - The symbol 'ASCENDING or 'DESCENDING to indicate whether to sort the
;;;   given pitch objects from low to high or high to low. 
;;;   Default = 'ASCENDING. 
;;; 
;;; RETURN VALUE  
;;; Returns a list of pitch obects.
;;; 
;;; EXAMPLE
#|
;; Apply the method with no optional argument (defaults to 'ASCENDING) and
;; collect and print the data of the pitch objects in the resulting list
(let ((chrd (make-chord '(d5 c4 gqs4 bqf5 f5 e4)
			:midi-channel 11
			:microtones-midi-channel 12)))
  (print (loop for p in (sort-pitches chrd) collect (data p))))

=> (C4 E4 GQS4 D5 F5 BQF5)

;; Sort from high to low
(let ((chrd (make-chord '(d5 c4 gqs4 bqf5 f5 e4)
			:midi-channel 11
			:microtones-midi-channel 12)))
  (print (loop for p in (sort-pitches chrd 'descending) collect (data p))))

=> (BQF5 F5 D5 GQS4 E4 C4)


|#
;;; 
;;; SYNOPSIS
(defmethod sort-pitches ((c chord) &optional (order 'ascending))
;;; ****
  (unless (or (eq order 'ascending)
              (eq order 'descending))
    (error "chord::sort-pitches: ~
            <order> argument to sort-pitches must be either 'ascending or ~
            'descending: ~a" order))
  (setf (slot-value c 'data)
    (stable-sort (data c)
                 #'(lambda (x y)
                     (if (eq order 'ascending)
                         (< (frequency x) (frequency y))
                       (> (frequency x) (frequency y)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod also-different-inflection ((c chord) (p pitch))
  (loop for cp in (data c) do
        (when (and (not (pitch= cp p))
                   (white-octave cp p)
                   (not (is-octave cp p)))
          (return t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* chord/chord-member
;;; FUNCTION
;;; 
;;; 
;;; ARGUMENTS 
;;; 
;;; 
;;; RETURN VALUE  
;;; 
;;; 
;;; EXAMPLE
;;; 
;;; 
;;; SYNOPSIS
(defmethod chord-member ((c chord) (p pitch) 
                         &optional (enharmonics-are-equal t)
                                   (octaves-are-true nil))
;;; ****
  (or (pitch-member p (data c) enharmonics-are-equal)
      (and octaves-are-true
           (pitch-member p (data c) enharmonics-are-equal #'is-octave))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* chord/common-notes
;;; FUNCTION
;;;
;;; 
;;; 
;;; ARGUMENTS 
;;; 
;;; 
;;; RETURN VALUE  
;;; integer: the number of notes common to the two chords.
;;; 
;;; EXAMPLE
;;; 
;;; 
;;; DATE
;;; 
;;; 
;;; SYNOPSIS
(defmethod common-notes ((c1 chord) (c2 chord)
                         &optional (enharmonics-are-equal t)
                         (octaves-are-true nil))
;;; ****
  (let ((result 0))
    (loop for p in (data c1) do
         (when (chord-member c2 p enharmonics-are-equal octaves-are-true)
           (incf result)))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This doesn't count a natural even if it's been told to display

(defmethod count-accidentals ((c chord) &optional ignore)
  (declare (ignore ignore))
  (loop 
      with count = 0
      for p in (data c)
      do
        ;; (unless (natural p)
        (when (show-accidental p)
          (incf count))
      finally (return count)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This gets the 'desirability' of the spelling of the chord: a bunch b# or
;;; f-flats is very undesirable.  Remember microtones don't figure in this
;;; value. 

(defmethod count-c5ths ((c chord) &optional ignore)
  (declare (ignore ignore))
  (loop 
      for p in (data c) sum (c5ths p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod bad-intervals ((c chord))
  (loop 
      with result = 0
      with ps = (data c)
      for p in ps and i from 0 do
        (loop for j from (1+ i) below (sclist-length c) do
              (when (bad-interval p (nth j ps))
                ;; (format t "~&~a ~a" (id p) (id (nth j ps)))
                (incf result)))
      finally (return result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod bad-adjacent-intervals ((c chord))
  (count-bad-adjacent-intervals (data c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; We only respell notes going up i.e. we don't go back down once we've found
;;; something we don't like; rather we reattempt the whole spelling business
;;; with the enharmonic of the lowest note and see which ends up with the least
;;; accidentals.  
;;; 
;;; good test: (respell-chord (make-chord '(a3 ds4 f4 fs5 c6))) so we get gf5

;;; ****m* chord/respell-chord
;;; FUNCTION
;;;
;;; 
;;; 
;;; ARGUMENTS 
;;; 
;;; 
;;; RETURN VALUE  
;;; 
;;; 
;;; EXAMPLE
;;; 
;;; 
;;; DATE
;;; 
;;; 
;;; SYNOPSIS
(defmethod respell-chord ((c chord) &optional verbose)
;;; ****
  (labels ((rm-enh-aux (pitches)
             (loop 
                 for p in pitches
                 for p-enh = (enharmonic p :warn nil)
                 for i from 0 
                 do
                   (when (and p-enh
                              (pitch-member p-enh pitches nil #'is-octave))
                     (setf (nth i pitches) p-enh)))
             pitches)
           (choose-chord (c1 c2)
             (let* ((c1acc (when c1
                             (count-c5ths c1)))
                    (c2acc (when c2 
                             (count-c5ths c2)))
                    result)
               ;; if the spellings seem to be about as good as each other, then
               ;; get the one with the least accidentals at least...
               (when verbose
                 (format t "~&choose-chord: c1 c5ths ~a c2 c5ths ~a"
                         c1acc c2acc))
               (when (and c1acc c2acc
                          (= c1acc c2acc))
                 (setf c1acc (count-accidentals c1)
                       c2acc (count-accidentals c2))
                 (when verbose
                   (format t "~&choose-chord: c1 accs ~a c2 accs ~a"
                           c1acc c2acc)))
               (when c1
                 (setf result
                   (if (or (not c2)
                           (<= c1acc c2acc))
                       c1
                     c2)))
               ;; OK we got the best by accidentals but let's just make sure
               ;; there's as few nasty intervals in there as possible too.
               (when (and c1 c2)
                 (let ((c1oct (make-chord (transpose-pitch-list-to-octave
                                           (my-copy-list (data c1)) 4)))
                       (c2oct (make-chord (transpose-pitch-list-to-octave
                                           (my-copy-list (data c2)) 4))))
                   (setf c1acc (bad-adjacent-intervals c1oct)
                         c2acc (bad-adjacent-intervals c2oct))
                   (when verbose
                     (format t "~&bad adjacent intervals: ~a ~a" c1acc c2acc))
                   (when (= c1acc c2acc)
                     (setf c1acc (bad-intervals c1oct)
                           c2acc (bad-intervals c2oct)))
                   (when verbose
                     (format t "~&plain bad intervals: ~a ~a" c1acc c2acc))
                   (when (and (/= c1acc c2acc)
                              ;; this makes sure that we only change result if
                              ;; there were some bad intervals, otherwise we
                              ;; stick with the choice by accidentals--removing
                              ;; bad intervals takes precedence over number of
                              ;; accidentals though 
                              (or (not (zerop c1acc))
                                  (not (zerop c2acc))))
                     (setf result
                       (if (< c1acc c2acc)
                           c1
                         c2)))))
               result))
           (rm-enharmonics (chord)
             (let* ((data (data chord))
                    ;; if there are enharmonics then we want to respell first
                    ;; the higher, then the lower and see which looks best
                    ;; if there are 3 of the buggers we're screwed (up to now).
                    (up (make-chord (rm-enh-aux (my-copy-list data))))
                    (down (make-chord (rm-enh-aux (my-copy-list
                                                   (reverse data))))))
               (when verbose
                 (format t "~&rm-enharmonics: up: ~a down: ~a"
                         (get-pitch-symbols up) (get-pitch-symbols down)))
               (setf (data chord) (data (choose-chord up down)))
               chord))
           (attempt (pitch-list)
             ;; this is a little convoluted but it's the easiest way (I think)
             ;; of handling spelling in the sc class (where we're dealing with
             ;; events) and changing spelling is a little involved (see
             ;; enharmonic method of event class for details).
             (let* ((e1 (make-event (first pitch-list) 4))
                    (len (length pitch-list))
                    (e2 (when (> len 1)
                          (make-event (second pitch-list) 4)))
                    (first2 '())
                    (others '())
                    result)
               (when e2
                 (respell e1 e2 nil nil) ; get the first interval right
                 (setf first2 (list (pitch-or-chord e1)
                                    (when e2
                                      (pitch-or-chord e2)))
                       (second pitch-list) (second first2)
                       others 
                       (when (> len 2)
                         (loop 
                             with p1 = (second pitch-list)
                             for p2 in (cddr pitch-list)
                             for pe1 = (make-event p1 4)
                             for pe2 = (make-event p2 4)
                             do
                               (when verbose
                                 (format t "~&attempt: before ~a ~a -- "
                                         (id (pitch-or-chord pe1))
                                         (id (pitch-or-chord pe2))))
                               (respell pe1 pe2 nil t)
                               (when verbose
                                 (format t "after ~a ~a"
                                         (id (pitch-or-chord pe1))
                                         (id (pitch-or-chord pe2))))
                               ;; bit counterintuitive this but works...
                             do (setf p1 (pitch-or-chord pe2))
                             collect p1)))
                 (setf result (make-chord
                               (if others
                                   (append first2 others)
                                 first2)))
                 (when verbose
                   (format t "~&attempt: result: ~a"
                           (get-pitch-symbols result)))
                 result))))
    (let* ((try1 (attempt (data c)))
           (try1first (when try1 
                        (loop for p in (data try1) and i from 0 do
                              (when (or (sharp p)
                                        (flat p))
                                (return i)))))
           (try2 (when try1first
                   (let ((copy (my-copy-list (data try1))))
                     (setf (nth try1first copy) (enharmonic
                                                 (nth try1first copy)))
                     (attempt copy))))
           (result (if try2
                       (choose-chord try1 try2)
                     try1)))
      ;; we can't just return try1/2 as we'd lose data in the other slots then
      (when result
        (setf (data c) (data result)))
      (when verbose
        (format t "~&try1: ~a" (get-pitch-symbols try1))
        (format t "~&try2: ~a" (get-pitch-symbols try2))
        (format t "~&result: ~a"(get-pitch-symbols c)))
      (rm-enharmonics c))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* chord/has-notes
;;; FUNCTION
;;; checks whether there are notes in this chord or not (make-chord
;;; nil) is a valid function call and creates a chord object with no notes.
;;; 
;;; ARGUMENTS 
;;; - the chord object
;;; 
;;; RETURN VALUE  
;;; t if it has notes, nil if it doesn't
;;;
;;; DATE 16.8.10
;;; 
;;; SYNOPSIS
(defmethod has-notes ((c chord))
;;; ****
  (not (zerop (sclist-length c))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; like the pitch class method, to find out the difference in pitch between
;;; two chords, usually the written and the sounding, to find transposition. 
;;; NB this takes pitch bend into consideration
;;; ****m* chord/pitch-
;;; FUNCTION
;;;
;;; 
;;; 
;;; ARGUMENTS 
;;; 
;;; 
;;; RETURN VALUE  
;;; 
;;; 
;;; EXAMPLE
;;; 
;;; 
;;; DATE
;;; 
;;; 
;;; SYNOPSIS
(defmethod pitch- ((c1 chord) (c2 chord))
;;; ****
  (- (midi-note-float (first (data c1))) (midi-note-float (first (data c2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Wed Feb  8 13:24:15 GMT 2012: Added robodoc entry

;;; ****f* chord/make-chord
;;; FUNCTION
;;; Create a chord object from a list of note-name symbols.
;;; 
;;; ARGUMENTS 
;;; - A list of note-name symbols.
;;;
;;; OPTIONAL ARGUMENTS
;;; - keyword argument :id. An element of any type that is to be the ID of the
;;;   chord object created.
;;; - keyword argument :auto-sort. T or NIL to indicate whether the method
;;;   should first sort the individual pitch objects created from low to high
;;;   before returning the new chord object. T = sort. Default = T.
;;; - keyword argument :midi-channel. An integer that is to be the MIDI channel
;;;   value to which all of the chromatic pitch objects in the given chord
;;;   object are to be set for playback. Default = 0.
;;; - keyword argument :microtones-midi-channel. An integer that is to be the
;;;   MIDI channel value to which all of the microtonal pitch objects in the
;;;   given chord object are to be set for playback. Default = 0.
;;; - keyword argument :force-midi-channel. T or NIL to indicate whether to
;;;   force a given value to the MIDI-CHANNEL slot, even if the notes passed to
;;;   the method are already pitch objects with non-zero MIDI-CHANNEL values. 
;;; 
;;; RETURN VALUE  
;;; A chord object.
;;; 
;;; EXAMPLE
#|
;; Simple useage with default values for keyword arguments
(make-chord '(c4 e4 g4 b4 d5 f5))

=>
CHORD: auto-sort: T, marks: NIL, micro-tone: NIL
SCLIST: sclist-length: 6, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: NIL, tag: NIL, 
data: (
PITCH: frequency: 261.626, midi-note: 60, midi-channel: 0 
[...]
data: C4
PITCH: frequency: 329.628, midi-note: 64, midi-channel: 0 
[...]
data: E4
[...]       
PITCH: frequency: 391.995, midi-note: 67, midi-channel: 0 
[...]
data: G4
[...]       
PITCH: frequency: 493.883, midi-note: 71, midi-channel: 0 
[...]
data: B4
[...]       
PITCH: frequency: 587.330, midi-note: 74, midi-channel: 0 
[...]
data: D5
[...]       
PITCH: frequency: 698.456, midi-note: 77, midi-channel: 0 
[...]
data: F5
)

;; By default the pitches are first sorted low to high
(let ((mc (make-chord '(e4 c4 g4 b4 f5 d5))))
  (loop for p in (data mc) collect (data p)))

=> (C4 E4 G4 B4 D5 F5)

;; Setting the :midi-channel and :microtones-midi-channel arguments results in
;; the MIDI-CHANNEL slot of each of the contained pitch objects being set
;; accordingly, depending on whether it is a chromatic or microtonal pitch
(let ((mc (make-chord '(cqs4 e4 gqf4 b4 dqf5 f5) 
		      :midi-channel 11
		      :microtones-midi-channel 12)))
  (loop for p in (data mc) collect (midi-channel p)))

=> (12 11 12 11 12 11)

|#
;;; 
;;; SYNOPSIS
(defun make-chord (note-list &key (id nil) (auto-sort t) (midi-channel 0)
                   (microtones-midi-channel 0) (force-midi-channel nil))
;;; ****
  (let ((chord (make-instance 'chord :id id :data note-list 
                              :auto-sort auto-sort)))
    ;; !!NB by default, ignore given midi-channels if our notes are already
    ;; pitch-objects  
    (when (or force-midi-channel (not (pitch-p (first note-list))))
      (set-midi-channel chord midi-channel microtones-midi-channel))
    chord))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun chord-p (thing)
  (typep thing 'chord))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EOF chord.lsp
