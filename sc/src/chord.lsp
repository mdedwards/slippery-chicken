;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* sclist/chord
;;; NAME 
;;; chord
;;;
;;; File:             chord.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> sclist -> chord
;;;
;;; Version:          1.0.6
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
;;; $$ Last modified: 12:52:03 Wed Jan 13 2016 GMT
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

(in-package :slippery-chicken)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass chord (sclist)
  ;; whether the pitches in the chord are sorted (ascending) at initialization.
  ((auto-sort :accessor auto-sort :type boolean :initarg :auto-sort 
              :initform t)
   ;; MDE Wed Jul 29 13:07:34 2015 -- the dissonance value: see
   ;; calculate-dissonance method
   (dissonance :reader dissonance :writer (setf dissonance) :initform nil)
   ;; MDE Wed Jul 29 13:10:24 2015 -- the spectral centroid of the chord: see
   ;; calculate-spectral-centroid method
   (centroid :reader centroid :writer (setf centroid) :initform nil)
   ;; 8.2.11 added slot to say whether there are microtones in the chord or not
   (micro-tone :accessor micro-tone :type boolean :initform nil)
   ;; dynamics, accents etc. exactly the code used by cmn.  These will simply
   ;; be copied over to the event when the chord is bound to an event.  Not
   ;; used by get-lp-data!  
   (marks :accessor marks :type list :initarg :marks :initform nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Wed Jul 29 13:17:00 2015 -- reader
(defmethod dissonance ((c chord))
  (with-slots ((d dissonance)) c
    (if d d (setf d (calculate-dissonance c)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Wed Jul 29 13:17:17 2015 -- reader
(defmethod centroid ((c chord))
  (with-slots ((cd centroid)) c
    (if cd cd (setf cd (calculate-spectral-centroid c)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SAR Mon Apr 16 16:52:53 BST 2012

;;; ****m* chord/delete-marks
;;; DESCRIPTION
;;; Delete all marks from the MARKS slot of the given chord object.
;;; 
;;; ARGUMENTS
;;; - A chord object.
;;; 
;;; RETURN VALUE
;;; Returns NIL.
;;; 
;;; EXAMPLE
#|
;;; Make a chord object, add two marks, and print the MARKS slot to see them;
;;; apply delete-marks and print the MARKS slot again to see the change
(let ((chrd (make-chord '(cs4 e4 fs4 af4 bf4))))
  (add-mark chrd 'fff)
  (add-mark chrd 'pizz)
  (print (marks chrd))
  (delete-marks chrd)
  (print (marks chrd)))

=>
(PIZZ FFF) 
NIL

|#
;;; SYNOPSIS
(defmethod delete-marks ((c chord))
;;; ****
  (setf (marks c) nil)
  (loop for pitch in (data c) do (delete-marks pitch)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((c chord) &rest initargs)
  (declare (ignore initargs))
  ;; MDE Tue Mar 20 16:11:55 2012 -- remove nils in the pitch list
  (setf (data c) (remove-if #'(lambda (x) (null x)) (data c)))
  (set-micro-tone c)
  (when (auto-sort c)
    (sort-pitches c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod set-micro-tone ((c chord))
  (setf (slot-value c 'micro-tone)
        ;; MDE Tue Mar 20 16:10:09 2012 -- (and p ...
        (loop for p in (data c) do (when (and p (micro-tone p)) (return t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf micro-tone) :before (value (c chord))
  (declare (ignore value c))
  (error "chord::(setf micro-tone): micro-tone slot cannot be setf'd; ~
          it is handled automatically according to pitches in chord"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object :before ((c chord) stream)
  (format stream "~%CHORD: auto-sort: ~a, marks: ~a, micro-tone: ~a, ~
                  ~%centroid: ~a, dissonance: ~a"
          (auto-sort c) (marks c) (micro-tone c) (slot-value c 'centroid)
          (slot-value c 'dissonance)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod verify-and-store :after ((c chord))
  (let ((data (data c)))
    (when (and data (not (typep (first data) 'pitch)))
      (loop for p in data and i from 0 do
           (when p ; MDE Tue Mar 20 16:08:37 2012
             (setf (nth i data) (make-pitch p)))))))
  
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
;;; ****m* chord/chord=
;;; DATE
;;; November 11th 2013
;;;
;;; DESCRIPTION
;;; Test whether the pitch objects of the two chords are pitch=.  Assumes both
;;; chords are sorted by pitch height.  See pitch= in the pitch class for
;;; details of comparing pitches.
;;; 
;;; RETURN VALUE
;;; T or NIL
;;; 
;;; SYNOPSIS
(defmethod chord= ((c1 chord) (c2 chord) &optional enharmonics-are-equal
                   (frequency-tolerance 0.01)) ; (src-tolerance 0.0001))
;;; ****
  (loop with happy = t 
       for p1 in (data c1) for p2 in (data c2) do
       (setf happy (pitch= p1 p2 enharmonics-are-equal frequency-tolerance))
                                        ; src-tolerance))
       while happy
       finally (return happy)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* chord/add-harmonics
;;; DESCRIPTION
;;; Adds pitches to the set which are harmonically related to the existing
;;; pitches. The keywords are the same as for the get-harmonics function.  See
;;; also sc-set method of the same name and get-pitch-list-harmonics. NB This
;;; will automatically sort all pitches from high to low.
;;; 
;;; ARGUMENTS
;;; - a chord object
;;;
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;;  see get-harmonics function
;;; 
;;; RETURN VALUE
;;; the same set object as the first argument but with new pitches added.
;;; 
;;; SYNOPSIS
(defmethod add-harmonics ((c chord) &rest keywords)
;;; ****
  (setf (data c) 
        (append (data c) 
                (apply #'get-pitch-list-harmonics (cons (data c) keywords))))
  c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Wed Feb 22 17:39:04 GMT 2012: Added robodoc entry

;;; ****m* chord/set-midi-channel
;;; DESCRIPTION
;;; Set the MIDI channel of the pitch objects in a given chord object to the
;;; specified values. 
;;;
;;; ARGUMENTS 
;;; - A chord object.
;;; - An integer that is to be the MIDI channel for chromatic pitches in the
;;;   given chord object.
;;; - An integer that is to be the MIDI channel for microtonal pitches in the
;;;   given chord object. NB: See player.lsp/make-player for details on
;;;   microtones in MIDI output.
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
;;; DESCRIPTION
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
;;; SAR Mon Apr 16 15:03:15 BST 2012: Added robodoc entry
;;; ****m* chord/output-midi-note
;;; DESCRIPTION
;;; Generate the MIDI-related data for each pitch in a given chord object. 
;;; 
;;; ARGUMENTS
;;; - A chord object.
;;; - A number that is the start time in seconds of the given chord within the
;;;   output MIDI file.
;;; - A decimal number between 0.0 and 1.0 that is the amplitude of the given
;;;   chord in the output MIDI file.
;;; - A number that is the duration in seconds of the given chord in the output
;;;   MIDI file.
;;; 
;;; RETURN VALUE
;;; The corresponding data in list form.
;;; 
;;; EXAMPLE

#|
;; Generate the MIDI-related data required for a 5-note chord that starts 100
;; seconds into the output MIDI file, with an amplitude of 0.5 and a duration
;; of 13.0 seconds.
(let ((chrd (make-chord '(cs4 e4 fs4 af4 bf4))))
  (output-midi-note chrd 100.0 0.5 13.0))

=> (#i(midi time 100.0 keynum 61 duration 13.0 amplitude 0.5 channel -1)
    #i(midi time 100.0 keynum 64 duration 13.0 amplitude 0.5 channel -1)
    #i(midi time 100.0 keynum 66 duration 13.0 amplitude 0.5 channel -1)
    #i(midi time 100.0 keynum 68 duration 13.0 amplitude 0.5 channel -1)
    #i(midi time 100.0 keynum 70 duration 13.0 amplitude 0.5 channel -1))
|#
;;;
;;; SYNOPSIS
(defmethod output-midi-note ((c chord) time amplitude duration)
;;; ****
  (loop for p in (data c) collect
        (output-midi-note p time amplitude duration)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ref is 1-based and counts from the lowest note up.

;;; SAR Wed Feb 22 17:56:55 GMT 2012: Added robodoc info

;;; ****m* chord/get-pitch
;;; DESCRIPTION
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

;;; ****m* chord/get-highest
;;; DATE
;;; 28/10/14
;;;
;;; DESCRIPTION
;;; Assuming the auto-sort slot is T (which it is by default), return the last
;;; (= highest) pitch in the chord. 
;;; 
;;; ARGUMENTS
;;; - a chord object
;;; 
;;; RETURN VALUE
;;; The highest pitch (object) in the chord.
;;; 
;;; SYNOPSIS
(defmethod get-highest ((c chord))
;;; ****
  (get-pitch c (sclist-length c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SAR Mon Apr 16 16:42:48 BST 2012: Added robodoc entry

;;; ****m* chord/add-mark
;;; DESCRIPTION
;;; Add the specified mark to the MARKS slot of the given chord object.
;;; 
;;; ARGUMENTS 
;;; - A chord object.
;;; - A mark.
;;;
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether to print a warning when attempting to add a
;;;   mark to a rest.
;;; 
;;; RETURN VALUE  
;;; Returns the full contents of the MARKS slot of the given chord object
;;; 
;;; EXAMPLE
#|
;;; Returns the complete contents of the MARKS slot
(let ((chrd (make-chord '(cs4 e4 fs4 af4 bf4))))
  (add-mark chrd 'fff)
  (add-mark chrd 'pizz))

=> (PIZZ FFF)

|#
;;; 
;;; SYNOPSIS
(defmethod add-mark ((c chord) mark &optional warn-rest)
;;; ****
  (declare (ignore warn-rest))
  (push mark (marks c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Wed Feb 22 18:05:57 GMT 2012: Added robodoc entry

;;; ****m* chord/get-pitch-symbols
;;; DESCRIPTION
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
;;; SYNOPSIS
(defmethod get-pitch-symbols ((c chord) &optional ignore)
;;; ****
  (declare (ignore ignore))
  (loop for p in (data c) collect (id p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon Apr 16 14:48:42 BST 2012: Added robodoc entry
;;; SAR Mon Apr 16 14:49:16 BST 2012: MDE's original comment deleted as it was
;;; taken over into the entry nearly verbatim.

;;; ****m* chord/no-accidental
;;; DESCRIPTION
;;; Set the SHOW-ACCIDENTAL slot of all pitch objects within a given chord
;;; object to NIL. This results in no accidentals for the given chord being printed
;;; when written to a score, and also excludes the writing of any accidentals
;;; for that chord in parentheses. 
;;; 
;;; ARGUMENTS
;;; - A chord object.
;;; 
;;; RETURN VALUE
;;; Always returns NIL.
;;; 
;;; EXAMPLE
#|
;;; Make a chord, print the SHOW-ACCIDENTAL slots of the pitch objects it
;;; contains; then call the method and print the same slots again to see the
;;; change.

(let ((chrd (make-chord '(cs4 e4 fs4 af4 bf4))))
  (print (loop for p in (data chrd) collect (show-accidental p)))
  (no-accidental chrd)
  (print (loop for p in (data chrd) collect (show-accidental p))))

=> 
(T T T T T) 
(NIL NIL NIL NIL NIL)

|#
;;; SYNOPSIS
(defmethod no-accidental ((c chord))
;;; ****
  (loop for p in (data c) do
        (no-accidental p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Wed Feb 22 18:10:21 GMT 2012: Added robodoc entry

;;; ****m* chord/chord-equal
;;; DESCRIPTION
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
;;; DESCRIPTION
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
  (lowest-aux c))

;;; we use this so we can have the same algo for highest just with pitch>
(defmethod lowest-aux ((c chord) &optional (test #'pitch<))
  (loop with result = (first (data c))
     for p in (rest (data c)) do
     (when (funcall test p result)
       (setq result p))
     finally (return result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Wed Feb 22 18:30:21 GMT 2012: Added robodoc entry

;;; ****m* chord/highest
;;; DESCRIPTION
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
  (lowest-aux c #'pitch>))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The ignore field is there because of the transpose method in tl-set
;;; Returns a clone of the current chord rather than replacing data

;;; SAR Mon Apr 16 14:39:14 BST 2012: Added robodoc entry

;;; ****m* chord/transpose
;;; DESCRIPTION
;;; Transpose the pitches of a given chord object by a specified number of
;;; semitones. The specified number can be positive or negative, and may
;;; contain a decimal segment for microtonal transposition. If passed a decimal
;;; number, the resulting note-names will be scaled to the nearest degree of
;;; the current tuning. 
;;; 
;;; ARGUMENTS 
;;; - A chord object.  
;;; - A positive or negative integer or decimal number indicating the number of
;;;   semitones by which the pitches of the given chord object are to be
;;;   transposed.
;;; 
;;; RETURN VALUE  
;;; Returns a chord object.
;;; 
;;; EXAMPLE
#|
;; Returns a chord object 
(let ((chrd (make-chord '(c4 e4 g4))))
  (transpose chrd 3))

=>
CHORD: auto-sort: T, marks: NIL, micro-tone: NIL
SCLIST: sclist-length: 3, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: NIL, tag: NIL, 
data: (
[...]
)

;; Accepts positive and negative integers and decimal numbers
(let ((chrd (make-chord '(c4 e4 g4))))
  (pitch-list-to-symbols (data (transpose chrd 3))))

=> (EF4 G4 BF4)

(let ((chrd (make-chord '(c4 e4 g4))))
  (pitch-list-to-symbols (data (transpose chrd -3))))

=> (A3 CS4 E4)

(let ((chrd (make-chord '(c4 e4 g4))))
  (pitch-list-to-symbols (data (transpose chrd -3.17))))

=> (AQF3 CQS4 EQF4)

|#
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
          do (setf (marks new) (my-copy-list (marks pitch))
                   (marks-before new) (my-copy-list (marks-before pitch)))
          collect new))
    ;; 8.2. 11: got to this here too now
    (set-micro-tone result)
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-lp-data ((c chord) &optional ignore1 ignore2 ignore3)
  (declare (ignore ignore1 ignore2 ignore3))
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
;;; DESCRIPTION
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
;;; Returns a list of pitch objects.
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

;;; ****m* chord/artificial-harmonic?
;;; DATE
;;; October 4th 2014
;;; 
;;; DESCRIPTION
;;; Determine whether a chord represents an artificial harmonic of the type
;;; that strings play. An artificial harmonic here is defined as a three note
;;; chord where the second note has a 'flag-head mark (i.e diamond shape) and
;;; the 1st and 3rd are related in frequency by an integer ratio.
;;;
;;; NB What we don't do (yet) is test whether the 2nd note is the correct nodal
;;; point to produce the given pitch.
;;; 
;;; ARGUMENTS
;;; - a chord object
;;; 
;;; OPTIONAL ARGUMENTS
;;; - cents-tolerance: how many cents the top note can deviate from a pure
;;;   partial frequency. E.g. the 7th harmonic is about 31 cents from the
;;;   nearest tempered note.
;;; 
;;; RETURN VALUE
;;; If the chord is an artificial harmonic then the sounding (3rd) note is
;;; returned, otherwise NIL.
;;;
;;; SYNOPSIS
(defmethod artificial-harmonic? ((c chord) &optional (cents-tolerance 31))
;;; ****
  (when (= 3 (sclist-length c))
    (let* ((f1 (frequency (get-nth 0 c)))
           (p2 (get-nth 2 c))
           (multiplier (round (frequency p2) f1))
           (partial (make-pitch (* f1 multiplier))))
      (and (member 'flag-head (marks (get-nth 1 c)))
               ;;   only start with the 8ve+5th harmonic
           (>= multiplier 3)
           (<= (abs (pitch- p2 partial))
               (/ cents-tolerance 100.0))
           p2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Thu Feb 23 19:16:17 GMT 2012: Added robodoc entry

;;; ****m* chord/chord-member
;;; DESCRIPTION
;;; Test whether a specified pitch object is a member of a given chord object. 
;;; 
;;; ARGUMENTS 
;;; - A chord object. 
;;; - A pitch object. This must be a pitch object, not just a note-name symbol,
;;;   but the pitch object can be made with either a note-name symbol or a
;;;   numerical hertz frequency value.
;;;
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether or not the method should consider
;;;   enharmonically equivalent pitches to be equal. T = enharmonics are
;;;   equal. Default = T.
;;; 
;;; RETURN VALUE  
;;; Similar to Lisp's "member" function, this method returns the tail of the
;;; data (list of pitch objects) of the tested chord object starting with the
;;; specified pitch object if that pitch is indeed a member of that list,
;;; otherwise returns NIL.  
;;;
;;; NB: Since the method returns the tail of the given chord (the "rest" of the
;;;     pitches after the given pitch), the result may be different depending
;;;     on whether that chord has been auto-sorted or not.
;;; 
;;; EXAMPLE
#|

;; Returns the tail of pitch objects contained starting with the tested pitch
(let ((chrd (make-chord '(c4 e4 gqs4 a4 d5 f5 bqf5)
                        :midi-channel 11
                        :microtones-midi-channel 12)))
  (pitch-list-to-symbols (chord-member chrd (make-pitch 'a4))))

=> (A4 D5 F5 BQF5)

;; The chord object's default auto-sort feature might appear to affect outcome
(let ((chrd (make-chord '(d5 c4 gqs4 a4 bqf5 f5 e4)
                        :midi-channel 11
                        :microtones-midi-channel 12)))
  (pitch-list-to-symbols (chord-member chrd (make-pitch 'a4))))

=> (A4 D5 F5 BQF5)

;; Returns NIL if the pitch is not present in the tested chord object. This
;; example uses the "pitch-list-to-symbols" function to simplify the
;; pitch-object output.
(let ((chrd (make-chord '(d5 c4 gqs4 a4 bqf5 f5 e4)
                        :midi-channel 11
                        :microtones-midi-channel 12)))
  (pitch-list-to-symbols (chord-member chrd (make-pitch 'b4))))

=> NIL

;; The optional <enharmonics-are-equal> argument is set to NIL by default
(let ((chrd (make-chord '(c4 e4 a4 d5 f5))))
  (pitch-list-to-symbols (chord-member chrd (make-pitch 'ds4))))

=> NIL

;; Setting the optional <enharmonics-are-equal> argument to T
(let ((chrd (make-chord '(c4 ef4 a4 d5 f5))))
  (pitch-list-to-symbols (chord-member chrd (make-pitch 'ds4) t)))

=> (EF4 A4 D5 F5)

;; The optional <octaves-are-true> argument is NIL by default

(let ((chrd (make-chord '(c4 ef4 a4 d5 ef5 f5))))
  (pitch-list-to-symbols (chord-member chrd (make-pitch 'c5))))

=> NIL

;; If optional <octaves-are-true> argument is set to T, any occurrence of the
;; same pitch class in a different octave will be considered part of the chord
;; and return a positive result.
(let ((chrd (make-chord '(c4 ef4 a4 d5 ef5 f5))))
  (pitch-list-to-symbols (chord-member chrd (make-pitch 'c5) nil t)))

=> (C4 EF4 A4 D5 EF5 F5)

|#

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
;;; SAR Mon Apr 16 14:08:02 BST 2012: Added robodoc entry
;;; ****m* chord/common-notes
;;; DESCRIPTION
;;; Return the integer number of pitches common to two chord objects.
;;; 
;;; ARGUMENTS 
;;; - A first chord object.
;;; - A second chord object.
;;;
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether enharmonically equivalent pitches are to be
;;;   considered the same pitch. T = enharmonically equivalent pitches are
;;;   considered the same pitch. Default = T.
;;; - T or NIL to indicate whether the same pitch class in different octaves is
;;;   to be considered the same pitch. T = consider the same pitch class from
;;;   octaves to be the same pitch. Default = NIL.
;;; 
;;; RETURN VALUE  
;;; Returns an integer that is the number of pitches common to the two chords
;;; objects. 
;;; 
;;; EXAMPLE
#|
;; The following two chord objects have 3 pitches in common
(let ((chrd-1 (make-chord '(c4 e4 g4 b4 d5 f5)))
      (chrd-2 (make-chord '(d3 f3 a3 c4 e4 g4))))
  (common-notes chrd-1 chrd-2))

=> 3

;; By default, enharmonically equivalent pitches are considered to be the same
;; pitch 
(let ((chrd-1 (make-chord '(c4 e4 g4 b4 d5 f5)))
      (chrd-2 (make-chord '(d3 f3 a3 c4 ff4 g4))))
  (common-notes chrd-1 chrd-2))

=> 3

;; Setting the first optional argument to NIL causes enharmonically equivalent
;; pitches to be considered separate pitches
(let ((chrd-1 (make-chord '(c4 e4 g4 b4 d5 f5)))
      (chrd-2 (make-chord '(d3 f3 a3 c4 ff4 g4))))
  (common-notes chrd-1 chrd-2 nil))

=> 2

;; By default, the same pitch class in different octaves is considered to be a
;; separate pitch
(let ((chrd-1 (make-chord '(c4 e4 g4 b4 d5 f5)))
      (chrd-2 (make-chord '(d3 f3 a3 ff4 g4 c5))))
  (common-notes chrd-1 chrd-2 t))

=> 2

;; Setting the second optional argument to T causes all pitches of the same
;; pitch class to be considered equal regardless of their octave
(let ((chrd-1 (make-chord '(c4 e4 g4 b4 d5 f5)))
      (chrd-2 (make-chord '(d3 f3 a3 ff4 g4 c5))))
  (common-notes chrd-1 chrd-2 t t))

=> 5

|#
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
;;; MDE Wed Aug 12 13:31:04 2015 -- 
;;; ****m* chord/similarity
;;; DATE
;;; 12th August 2015, Wals, Austria
;;; 
;;; DESCRIPTION
;;; Calculates the similarity of two chord objects on a scale of 0.0-1.0. This
;;; weights equally the number of common notes and the interval structure
;;; between adjacent notes.
;;; 
;;; ARGUMENTS
;;; - the first chord object
;;; - the second chord object
;;;
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether enharmonic notes should be treated as
;;;   equal. Even when NIL, enharmonic equivalents will still score high
;;;   overall, just not perfect.
;;; - T or NIL to indicate whether octave equivalents such as f#1 and f#2
;;;   should result in higher scores.
;;; 
;;; RETURN VALUE
;;; a number between 0.0 and 1.0, with 1.0 being the same exact chord and 0.0
;;; being two chords with no notes and no adjacent intervals in common.
;;; 
;;; 
;;; EXAMPLE
#|

(similarity (make-chord '(c4 e4 g4)) (make-chord '(c4 e4 g4)))
=> 1.0
(similarity (make-chord '(c4 e4 g4)) (make-chord '(df4 f4 af4)))
=> 0.5
(similarity (make-chord '(c1 cs1 f1 b1)) (make-chord '(d1 e1 fs1 gs1 as1)))
=> 0.0
(similarity (make-chord '(cs4 es4 gs4)) (make-chord '(df4 f4 af4)))
=> 1.0
(similarity (make-chord '(cs4 es4 gs4)) (make-chord '(df4 f4 af4)) nil)
=> 0.75

|#
;;; SYNOPSIS
(defmethod similarity ((c1 chord) (c2 chord)
                       &optional (enharmonics-are-equal t)
                         (octaves-are-true nil))

;;; ****
  (let* ((common1 (common-notes c1 c2 enharmonics-are-equal octaves-are-true))
         (common2 (common-notes c1 c2 t t))
         ;; our common notes are the average of common notes with the caller's
         ;; optional arguments and the common notes with both optional
         ;; arguments set to t. If the optional arguments were T anyway,
         ;; there'll be no difference.
         (common (/ (+ common1 common2) 2))
         (c1n (sclist-length c1))
         (c2n (sclist-length c2))
         (cscore (if (zerop common)
                    0.0
                    ;; average common notes ratio for each of the two chords
                    (/ (+ (/ common c1n)
                          (/ common c2n))
                       2.0)))
         (is1 (get-interval-structure c1 t t))
         (is2 (get-interval-structure c2 t t))
         (isi (intersection is1 is2))
         (isscore (float (/ (length isi) (1- (max c1n c2n))))))
    ;; (format t "~%~a ~a: ~a, ~a ~a" is1 is2 isi isscore cscore)
    ;; max score is 1.0 (min 0.0)
    (* 0.5 (+ cscore isscore))))

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

;;; SAR Wed Jun 13 15:26:33 BST 2012: Added robodoc entry

;;; ****m* chord/respell-chord
;;; DESCRIPTION
;;; Respell the pitches of a given chord object to improve interval structure;
;;; i.e., removing augmented intervals etc.
;;;
;;; This method respells pitches from the bottom of the chord upwards. It does
;;; not process the pitches downwards again once pitches has been
;;; changed. Instead, it reattempts the whole respelling with the enharmonic of
;;; the lowest pitch to determine which spelling produces the fewest
;;; accidentals.
;;;
;;; NB: Respelling pitches in a chord is a rather complex process and is by no
;;;     means fool-proof. The process employed here is based on avoiding double
;;;     accidentals; thus, since both FS4 and BS4 have single sharps, the BS4
;;;     won't be changed to C5.
;;; 
;;; ARGUMENTS 
;;; - A chord object.
;;;
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether to print feedback from the process to the
;;;   listener. T = print. Default = NIL.
;;; 
;;; RETURN VALUE  
;;; A chord object. 
;;; 
;;; EXAMPLE
#|
(let ((chrd (make-chord '(a3 ds4 f4 fs5 c6))))
  (pitch-list-to-symbols (data (respell-chord chrd t))))

=> (A3 EF4 F4 GF5 C6)

|#
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
                                           (my-copy-list (data c1)) 4)
                                          ;; MDE Fri Dec  6 10:40:23 2013 
                                          :force-midi-channel nil))
                       (c2oct (make-chord (transpose-pitch-list-to-octave
                                           (my-copy-list (data c2)) 4)
                                          ;; MDE Fri Dec  6 10:40:37 2013 
                                          :force-midi-channel nil)))
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
                    (up (make-chord (rm-enh-aux (my-copy-list data))
                                    ;; MDE Fri Dec  6 10:40:58 2013
                                    :force-midi-channel nil))
                    (down (make-chord (rm-enh-aux (my-copy-list
                                                   (reverse data)))
                                      ;; MDE Fri Dec  6 10:41:06 2013 -- 
                                      :force-midi-channel nil)))
               (when verbose
                 (format t "~&rm-enharmonics: up: ~a down: ~a"
                         (when up (get-pitch-symbols up))
                         (when down (get-pitch-symbols down))))
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
                            ;; bit counter-intuitive this but works...
                            do (setf p1 (pitch-or-chord pe2))
                            collect p1)))
                 (setf result (make-chord
                               (if others
                                   (append first2 others)
                                   first2)
                               ;; MDE Fri Dec  6 10:41:27 2013
                               :force-midi-channel nil))
                 (when verbose
                   (format t "~&attempt: result: ~a"
                           (when result
                             (get-pitch-symbols result))))
                 result))))
    ;; don't try to respell 1/12th tone chords
    (if (micro-but-not-quarter-tone-p c)
        c
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
          ;; we can't just return try1/2 as we'd lose data in the other slots
          ;; then  
          (when result
            (setf (data c) (data result)))
          (when verbose
            (format t "~&try1: ~a" (when try1 (get-pitch-symbols try1)))
            (format t "~&try2: ~a" (when try2 (get-pitch-symbols try2)))
            (format t "~&result: ~a" (when c (get-pitch-symbols c))))
          (rm-enharmonics c)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SAR Mon Apr 16 14:16:13 BST 2012: Added robodoc entry
;;; MDE: original comment:
;;; checks whether there are notes in this chord or not (make-chord
;;; nil) is a valid function call and creates a chord object with no notes.

;;; ****m* chord/has-notes
;;; DATE 
;;; 16-Aug-2010
;;; 
;;; DESCRIPTION
;;; Tests whether a given chord object contains at least one pitch
;;; object. 
;;;
;;; (make-chord nil) is a valid function call and creates a chord object with
;;; no notes. 
;;; 
;;; ARGUMENTS 
;;; - A chord object. 
;;; 
;;; RETURN VALUE  
;;; Returns T if the given chord object contains at least one pitch object,
;;; otherwise returns NIL.
;;;
;;; EXAMPLE
#|
;; Returns T if the given chord object contains at least one pitch object
(let ((chrd (make-chord '(c4))))
  (has-notes chrd))

=> T

(let ((chrd (make-chord '(c4 e4 g4))))
  (has-notes chrd))

=> T

;; Otherwise returns NIL
(let ((chrd (make-chord nil)))
  (has-notes chrd))

=> NIL

|#
;;; SYNOPSIS
(defmethod has-notes ((c chord))
;;; ****
  (not (zerop (sclist-length c))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SAR Mon Apr 16 14:25:06 BST 2012: Added robodoc entry

;;; MDE original comment: like the pitch class method, to find out the
;;; difference in pitch between two chords, usually the written and the
;;; sounding, to find transposition. NB this takes pitch bend into
;;; consideration

;;; ****m* chord/pitch-
;;; DESCRIPTION
;;; Determine the difference between the lowest pitch of two chords. This
;;; method can be used, for example, to compare the written and sounding
;;; versions of a chord to determine transposition. 
;;;
;;; If the lower chord is passed as the first argument, the method will return
;;; a negative number.
;;;
;;; NB: This method takes pitch bend into consideration when calculating. 
;;; 
;;; ARGUMENTS 
;;; - A first chord object.
;;; - A second chord object.
;;; 
;;; RETURN VALUE  
;;; A positive or negative decimal number.
;;; 
;;; EXAMPLE
#|
;; The method measures the distance between the first (lowest) pitches of the
;;; chord only.
(let ((chrd-1 (make-chord '(c4 e4 g4)))
      (chrd-2 (make-chord '(d4 e4 fs4 a4))))
  (pitch- chrd-2 chrd-1))

=> 2.0

;;; Passing the lower chord as the first argument produces a negative result
(let ((chrd-1 (make-chord '(c4 e4 g4)))
      (chrd-2 (make-chord '(d4 e4 fs4 a4))))
  (pitch- chrd-1 chrd-2))

=> -2.0

|#
;;; 
;;; SYNOPSIS
(defmethod pitch- ((c1 chord) (c2 chord))
;;; ****
  (- (midi-note-float (first (data c1))) (midi-note-float (first (data c2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Mon May  5 16:31:06 2014 
(defmethod midi-note-float ((c chord) &optional in-cents)
  (loop for p in (data c) collect (midi-note-float p in-cents)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Tue Apr 17 13:15:31 2012 -- mainly for printing in the clm-play method:
;;; the pitch class has the frequency slot. Mimic this here by returning a list
;;; of freqs.

(defmethod frequency ((c chord))
  (loop for p in (data c) collect (frequency p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Mon Apr 23 13:13:49 2012 
(defmethod enharmonic ((c chord) &key (warn t))
  (loop for p in (data c) collect (enharmonic p :warn warn)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Mon May 14 15:24:07 2012 
(defmethod print-simple ((c chord) &optional (stream t) (separator " "))
  (let ((result ""))
    (loop for p in (data c) do
         (setf result (concatenate 'string result (print-simple p nil)
                                   separator)))
    (format stream "~a" result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Fri Aug 10 16:17:59 2012 -- pitches can be pitch objects or any data
;;; that can be passed to make-pitch, or indeed lists of these, as they will be
;;; flattened. 
;;; e.g.
;;; (add-pitches (make-chord '(c4 e4 g4)) '(bf3 dqf5))
;;; (add-pitches (make-chord '(c4 e4 g4)) 'bf4 'd5)

;;; SAR Thu Oct  4 14:06:35 BST 2012: Added robodoc entry

;;; ****m* chord/add-pitches
;;; DESCRIPTION
;;; Add the specified pitches to the given chord object. 
;;; 
;;; ARGUMENTS
;;; - A chord object.
;;; - The pitches to add to that object. These can be pitch objects or any data
;;;   that can be passed to make-pitch, or indeed lists of these, as they will
;;;   be flattened.
;;; 
;;; RETURN VALUE
;;; - A chord object.
;;; 
;;; EXAMPLE
#|
(let ((ch (make-chord '(c4 e4 g4))))
  (print (get-pitch-symbols ch))
  (add-pitches ch 'bf3)
  (print (get-pitch-symbols ch))
  (add-pitches ch 'af3 'a4 'b4)
  (print (get-pitch-symbols ch))
  (add-pitches ch '(cs5 ds5 fs5))
  (print (get-pitch-symbols ch)))

=>
(C4 E4 G4) 
(BF3 C4 E4 G4) 
(AF3 BF3 C4 E4 G4 A4 B4) 
(AF3 BF3 C4 E4 G4 A4 B4 CS5 DS5 FS5) 


|#
;;; SYNOPSIS
(defmethod add-pitches ((c chord) &rest pitches)
;;; ****
  (setf (data c) 
        (remove-duplicates 
         (append (data c) (init-pitch-list (flatten pitches)))
         :test #'pitch=))
  (initialize-instance c)
  c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Fri Aug 10 16:32:32 2012 -- remove pitches; same criteria apply to
;;; <pitches> as in add-pitches.  No warning/error will be signalled if the
;;; pitches to be removed are not actually in the chord.
;;; e.g.
;;; (print-simple (rm-pitches (make-chord '(c4 e4 g4 bf4 d5)) '(bf4 dqf5)))
;;; (print-simple (rm-pitches (make-chord '(c4 e4 g4 bf4 d5)) '(bf4 e4)))

;;; SAR Thu Oct  4 16:42:21 BST 2012: Added robodoc entry

;;; ****m* chord/rm-pitches
;;; DESCRIPTION
;;; Remove the specified pitches from an existing chord object.
;;; 
;;; ARGUMENTS
;;; - A chord object.
;;; - The pitches to remove from that object. These can be pitch objects or any
;;;   data that can be passed to make-pitch, or indeed lists of these, as they
;;;   will be flattened. NB: No warning/error will be signalled if the pitches
;;;   to be removed are not actually in the chord.
;;; 
;;; RETURN VALUE
;;; - A chord object.
;;; 
;;; EXAMPLE
#|
(let ((ch (make-chord '(af3 bf3 c4 e4 g4 a4 b4 cs5 ds5 fs5))))
  (print (get-pitch-symbols ch))
  (rm-pitches ch 'bf3)
  (print (get-pitch-symbols ch))
  (rm-pitches ch 'af3 'a4 'b4)
  (print (get-pitch-symbols ch))
  (rm-pitches ch '(cs5 ds5 fs5))
  (print (get-pitch-symbols ch)))

=>
(AF3 BF3 C4 E4 G4 A4 B4 CS5 DS5 FS5) 
(AF3 C4 E4 G4 A4 B4 CS5 DS5 FS5) 
(C4 E4 G4 CS5 DS5 FS5) 
(C4 E4 G4) 

|#
;;; SYNOPSIS
(defmethod rm-pitches ((c chord) &rest pitches)
;;; ****
  (setf (data c) (set-difference (data c) (init-pitch-list (flatten pitches))
                                 :test #'pitch=))
  (initialize-instance c)
  c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Sat Nov  2 15:08:33 2013 -- if symbols-only, just compare the pitch
;;; symbols otherwise use pitch= (equal frequencies etc.) 
(defmethod rm-duplicates ((c chord) &optional symbols-only)
  ;; MDE Mon Dec  2 17:40:08 2013 -- sim to rm-octaves below
  ;; (setf (slot-value c 'data) (rm-pitch-duplicates (data c) symbols-only)))
  (setf (data c) (rm-pitch-duplicates (data c) symbols-only)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Thu May  3 10:52:54 2012 
(defmethod rm-octaves ((c chord))
  ;; MDE Mon Dec 2 17:37:36 2013 -- can't remember why we changed slot-value
  ;; instead of data directly but need to do the latter so we update the number
  ;; of pitches (setf (slot-value c 'data) (remove-octaves (data c)))
  (setf (data c) (remove-octaves (data c)))
  c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod rm-marks ((c chord) marks &optional (warn t))
  (rm-marks-aux c marks warn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Thu Jan  9 09:59:43 2014 -- for convenience.
;;; just promote to an sc-set and call the method there.
;;; (get-interval-structure (make-sc-set c :rm-dups rm-dups) in-semitones))
;;; MDE Wed Aug 12 13:35:55 2015 -- moved the code from the sc-set class into
;;; the chord class as the former is now a subclass of chord

;;; ****m* chord/get-interval-structure
;;; DESCRIPTION
;;; Get the distances between each pitch in a given sc-set object and the
;;; lowest (or neighbouring) pitch in that object in DEGREES (which default to
;;; quarter-tones in slippery chicken). This method assumes that the given
;;; sc-set object is sorted from low to high, which is the default action for
;;; sc-set objects.
;;; 
;;; ARGUMENTS
;;; - An sc-set object.
;;;
;;; OPTIONAL
;;; - T or NIL indicating whether to return values in semitones or default of
;;;   degrees. Special case: if this argument is 'frequencies, then the
;;;   interval structure will be returned as frequency differences. T =
;;;   semitones. Default = NIL.
;;; - T or NIL to indicated whether we should return intervals from pitch to
;;;   pitch (T) or from the lowest to each pitch (NIL). Default = NIL.
;;; 
;;; RETURN VALUE
;;; A list of integers.
;;; 
;;; EXAMPLE
#|
;;; Returns the distances in degrees (which are quarter-tones by default
;;; in slippery chicken--use (in-scale :chromatic) at the top of your code to
;;; set to the chromatic scale):

(get-interval-structure (make-chord '(c4 e4 g4)))

=> (8.0 14.0)

;;; Return semitones
(get-interval-structure (make-chord '(c4 e4 g4)) t))

;;; Interval structure not from lowest but from pitch to pitch (ascending)
(get-interval-structure (make-chord '(c4 e4 g4 b4)) t t)
(4.0 3.0 4.0)

=> (4.0 7.0)
|#
;;; SYNOPSIS
(defmethod get-interval-structure ((c chord) &optional in-semitones neighbour)
;;; ****
  (if (zerop (sclist-length c))         ; in case we have a nil chord
      '(0.0)
      (let* ((freqs (eq in-semitones 'frequencies))
             (lowest (if freqs 
                         (frequency (first (data c)))
                         (degree (first (data c)))))
             (last lowest)
             ;; MDE Sat Feb 11 10:44:46 2012
             (dps (degrees-per-semitone)))
        (loop for p in (rest (data c))
           for pd = (if freqs (frequency p) (degree p))
           for diff = (float (- pd last))
           collect
             (if (eq in-semitones t)
                 (/ diff dps)
                 diff)
           do (when neighbour (setq last pd))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Mon Aug 25 18:24:41 2014 
(defmethod micro-but-not-quarter-tone-p ((c chord))
  (some #'(lambda (x) (micro-but-not-quarter-tone-p x))
        (data c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* chord/get-partials-amps
;;; DATE
;;; July 28th 2015, Edinburgh
;;; 
;;; DESCRIPTION
;;; Return a list containing for every pitch in the chord a list of two lists:
;;; first the partial frequency scalars (which will be integers or very close
;;; to integers) then the normalised partial amplitudes.
;;;
;;; Note that if spectrum are given then average is ignored. If spectrum is an
;;; assoc-list then we get the data from it otherwise if it's just a list of
;;; freq-scalers then amps, it's fixed and we use that repeatedly.
;;; 
;;; ARGUMENTS
;;; - the chord object
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :average. Whether to average spectral data over one octave centring around
;;; each of the pictures in the chord. Default = T.
;;; - :spectrum. The spectrum data. Default = (get-sc-config 'default-spectra)
;;;   (see globals.lsp)
;;; 
;;; RETURN VALUE
;;; A list of two element sublist. of numbers.
;;; 
;;; EXAMPLE
#|

(get-partials-amps (make-chord '(c4 e4 g4)))
-->
(((0.99666667 1.9958333 2.9941666 3.9958336 5.002501 6.015 7.0333333 8.049168
   9.086666 10.125 11.174167 12.234166)
  (0.9447856 1.0000001 0.22975834 0.13516104 0.1625952 0.05319352 0.12118169
   2.1978654e-4 0.049031425 0.010756988 0.011732774 0.0))
 ((0.9966666 1.9958333 2.9966667 4.0025005 5.0133333 6.028333 7.0575004
   8.085834 9.136668 10.193334 11.264999 12.346667)
  (0.99999994 0.73724264 0.20146085 0.10752921 0.122650035 0.03613218
   0.06638182 0.0 0.022111647 0.004390597 0.011441806 5.167861e-4))
 ((0.9966666 1.995 3.0 4.005834 5.0208335 6.042501 7.08 8.1225 9.185 10.261666
   11.45 11.405833)
  (1.0 0.48279247 0.1664997 0.066108614 0.07859015 0.027295936 0.035848983 0.0
   0.010107763 0.0027390774 0.0040924386 0.0010194636)))

|#
;;; SYNOPSIS
(defmethod get-partials-amps ((c chord)
                              &key (average t)
                                (spectrum (get-sc-config 'default-spectra)))
;;; ****
  (loop for pitch in (data c)
     for freq = (frequency pitch)
     ;; bear in mind that midi-note is always an integer even when we're
     ;; dealing with microtones so we should be safe as long as we have data
     ;; for all notes
     for midi = (midi-note pitch)
     collect
       (get-spectrum midi spectrum average)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* chord/calculate-dissonance
;;; DATE
;;; 27th July 2015, Edinburgh
;;; 
;;; DESCRIPTION
;;; Calculates the dissonance of a chord as it would appear when played on the
;;; piano (by default; other instruments' spectral data can also be used: see
;;; below). This uses a roughness calculation model by Pantelis N. Vassilakis
;;; as outlined at http://www.acousticslab.org/learnmoresra/moremodel.html. The
;;; model has been perceptually verified. We use spectral data handled by
;;; spectra.lsp to sum the roughness of sine pairs up to the first 12 partials
;;; of each tone, i.e. every partial of every tone is calculated in relation to
;;; all other tones' partials, taking their amplitudes into account.
;;;
;;; NB If notes are above/below the piano range for which we have data then we
;;; use the highest/lowest spectral data available.
;;;
;;; NB This method will be called automatically the first time you access a
;;; chord object's dissonance slot. In that case the default keyword arguments
;;; will be used. If you want to use different arguments or recalculate
;;; dissonance after the chord has been altered, this method can be used at any
;;; time but bear in mind that it does not automatically change the dissonance
;;; slot so use setf for that if necessary. If you merely want to change the
;;; default spectra which are used, then you can do that at startup via
;;; (set-sc-config 'default-spectra ...)
;;; 
;;; ARGUMENTS
;;; - the chord object 
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :num-partials. The number of partials we want to use in our
;;;   calculation. Default = 12.
;;; - :average. T or NIL to indicate whether we want to use the average
;;;   spectrum for an octave with the current note of a chord in the middle.
;;;   Default = T.
;;; - :spectrum. Pass a two-element list of partial frequency scalers (list)
;;;   and amplitudes (list) to use instead of the piano data. The amplitudes
;;;   should be normalised from 0.0 to 1.0 and there should be as many of these
;;;   as frequency scalers and :num-partials. Bear in mind that even using the
;;;   same data for several calls, this will still give different results for
;;;   the same chord type starting on different notes, as the perceptual
;;;   dissonance is based on pitch height as well as interval structure.
;;;   Default = (get-sc-config 'default-spectra) (see globals.lsp)
;;; 
;;; RETURN VALUE
;;; A floating point number representing the dissonance value. Higher values
;;; are more dissonant.
;;; 
;;; SYNOPSIS
(defmethod calculate-dissonance ((c chord)
                                 &key (num-partials 12) (average t)
                                   (spectrum (get-sc-config 'default-spectra)))
;;; ****
  (when (> num-partials 12)
    (warn "chord::calculate-dissonance: using max. of 12 for :num-partials.")
    (setq num-partials 12))
  ;; we're only interested in the ratios of partial amplitudes, rather than
  ;; fixed amplitude values and differences between instruments, hence we
  ;; normalise the numbers we're given or use.
  (let* ((freq-pairs
          (get-all-pairs
           (loop for partials-amps in (get-partials-amps c :spectrum spectrum
                                                         :average average)
              for pitch in (data c)
              appending
                (progn
                  (unless partials-amps
                    (error "chord::calculate-dissonance: can't get ~
                            partial data: ~&~a" c))
                  (loop for partial-num from 1 to num-partials
                     for partial in (first partials-amps)
                     for amp in (second partials-amps)
                     unless (zerop amp)
                     collect (list (* partial (frequency pitch)) amp)))))))
    (loop for pair in freq-pairs
       for n1 = (first pair) for n2 = (second pair)
       sum (sine-pair-roughness (first n1)
                                (second n1)
                                (first n2)
                                (second n2)))))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* chord/calculate-spectral-centroid
;;; DATE
;;; July 28th 2015
;;; 
;;; DESCRIPTION
;;; Calculate the spectral centroid of a chord. This technique is of course
;;; usually applied in digital signal processing to an audio signal via a
;;; Fast Fourier Transform (FFT). It's perhaps a little strange to use this on
;;; pitch data, not least of which because we will not be taking into
;;; account any phase information or interference of possible harmonic partial
;;; interactions, but nevertheless we get a good indication of the "pitch
;;; height" of a chord via this method.
;;; 
;;; NB This will be called automatically the first time you access a chord
;;; object's centroid slot. In that case the default keyword arguments will be
;;; used. If you want to use different arguments or recalculate the centroid
;;; after the chord has been altered, this method can be used at any time but
;;; bear in mind that it does not automatically change the centroid slot so use
;;; setf for that if necessary.
;;; 
;;; ARGUMENTS
;;; - the chord object 
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :num-partials. The number of partials we want to use in our
;;;   calculation. Default = 12.
;;; - :average. T or NIL to indicate whether we want to use the average
;;;   spectrum for an octave with the current note of a chord in the middle.
;;;   Default = T.
;;; - :spectrum. Pass a two-element list of partial frequency scalers (list)
;;;   and amplitudes (list) to use instead of the piano data. The amplitudes
;;;   should be normalised from 0.0 to 1.0 and there should be as many of these
;;;   as frequency scalers and :num-partials. Bear in mind that even using the
;;;   same data for several calls, this will still give different results for
;;;   the same chord type starting on different notes, as the perceptual
;;;   dissonance is based on pitch height as well as interval structure.
;;;   Default = (get-sc-config 'default-spectra) (see globals.lsp)
;;; 
;;; RETURN VALUE
;;; A floating point value representing the frequency in Hertz of the spectral
;;; centroid.  
;;; 
;;; EXAMPLE
#|

(calculate-spectral-centroid (make-chord '(fs3 c4 cs4 d4 ds4 e4)))
 --> 692.3215865354373d0
(calculate-spectral-centroid (make-chord '(fs3 c4 cs4 d4 ds4 e4 g6)))
 --> 772.4924013974714d0

|#
;;; SYNOPSIS
(defmethod calculate-spectral-centroid ((c chord)
                                        &key (num-partials 12)
                                          (average t)
                                          (spectrum
                                           (get-sc-config 'default-spectra)))
;;; ****
  (let ((numerator 0.0)
        (denominator 0.0))
    (loop for partials-amps in (get-partials-amps c :spectrum spectrum
                                                  :average average)
       for pitch in (data c) do
         (unless partials-amps
           (error "chord::calculate-spectral-centroif: can't get partial ~
                   data: ~&~a" c))
         (loop for partial-num from 1 to num-partials
            for partial in (first partials-amps)
            for amp in (second partials-amps) do
              (unless (zerop amp)
                (incf numerator (* amp partial (frequency pitch)))
                (incf denominator amp))))
    (/ numerator denominator)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Wed Feb  8 13:24:15 GMT 2012: Added robodoc entry
;;; MDE Fri Dec  6 09:35:37 2013 -- changed to (force-midi-channel t) from NIL.

;;; ****f* chord/make-chord
;;; DESCRIPTION
;;; Create a chord object from a list of note-name symbols.
;;; 
;;; ARGUMENTS 
;;; - A list of note-name symbols.
;;;
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :id. An element of any type that is to be the ID of the chord object
;;;   created.
;;; - :auto-sort. T or NIL to indicate whether the method should first sort the
;;;   individual pitch objects created from low to high before returning the
;;;   new chord object. T = sort. Default = T.
;;; - :midi-channel. An integer that is to be the MIDI channel value to which
;;;   all of the chromatic pitch objects in the given chord object are to be
;;;   set for playback. Default = 1.
;;; - :microtones-midi-channel. An integer that is to be the MIDI channel value
;;;   to which all of the microtonal pitch objects in the given chord object
;;;   are to be set for playback. Default = 1. NB: See
;;;   player.lsp/make-player for details on microtones in MIDI output.  
;;; - :force-midi-channel. T or NIL to indicate whether to force a given value
;;;   to the MIDI-CHANNEL slot, even if the notes passed to the method are
;;;   already pitch objects with non-zero MIDI-CHANNEL values.
;;; 
;;; RETURN VALUE  
;;; A chord object.
;;; 
;;; EXAMPLE
#|
;; Simple usage with default values for keyword arguments
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
(defun make-chord (note-list &key (id nil) (auto-sort t) (midi-channel 1)
                   (microtones-midi-channel 1) (force-midi-channel t))
;;; ****
  ;; MDE Mon Jun 11 17:59:07 2012 
  ;; (unless (listp note-list)
  ;;    (setf note-list (list note-list)))
  ;; MDE Tue Oct 29 19:13:09 2013 -- remove the above unless and do more
  ;; flexible processing with typecase.
  (let* ((nl (typecase note-list
               ((or pitch symbol) (list note-list))
               ((or sc-set chord) (data note-list))
               (t (if (simple-listp note-list)
                      note-list
                      (error "chord::make-chord: can't make chord from ~a"
                             note-list)))))
         (chord (make-instance 'chord :id id :data nl :auto-sort auto-sort)))
    ;; !!NB by default, ignore given midi-channels if our notes are already
    ;; pitch-objects  
    ;; (print (data chord))
    (when (or force-midi-channel (not (pitch-p (first nl))))
      (set-midi-channel chord midi-channel microtones-midi-channel))
    chord))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun chord-p (thing)
  (typep thing 'chord))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Mon Jul 27 15:14:15 2015 -- chord dissonance calculation methods
;;; See http://www.acousticslab.org/learnmoresra/moremodel.html
(defun sine-pair-roughness (freq1 amp1 freq2 amp2)
  (let* ((fmin (min freq1 freq2))
         (fmax (max freq1 freq2))
         (fmmm (- fmax fmin))
         (amin (min amp1 amp2))
         (amax (max amp1 amp2))
         (s1 0.0207)
         (s2 18.96)
         (s (/ 0.24 (+ s2 (* s1 fmin))))
         (b1 3.5)
         (b2 5.75)
         (x (* amin amax))
         (y (/ (* 2 amin) (+ amin amax)))
         (z (- (exp (- (* b1 s fmmm)))
               (exp (- (* b2 s fmmm))))))
    (* (expt x 0.1) (* 0.5 (expt y 3.11)) z)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; try to iron out the quite radical differences in spectral data for nearby
;;; notes by averaging over a complete octave. Spectrum is a list of freq
;;; scalers and a list of amplitudes. Not to be confused with average spectra,
;;; which is aimed at averaging spectra.
(defun average-spectrum (starting-midi-note
                         &optional (spectrum (get-sc-config 'default-spectra)))
  (let* ((freq-scalers (ml 0.0 12))
         (amp-scalers (ml 0.0 12)))
    (loop for midi from starting-midi-note repeat 12 do
         (loop with data = (data (get-spectrum midi spectrum nil))
                                        ;(get-nearest midi spectrum))
            for scaler in (first data)
            for amp in (second data)
            for i from 0 do
              (incf (nth i freq-scalers) scaler)
              (incf (nth i amp-scalers) amp)))
    ;; we average the frequency scaler but no need to do this with the amp
    ;; scalers as they have to be normalised anyway.
    (list (loop for fs in freq-scalers collect (/ fs 12.0))
          (normalise amp-scalers))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Thu Aug 6 11:14:49 2015 - The spectrum argument can be either a symbol
;;; (whereupon this will be used to look up the spectra stored in
;;; +slippery-chicken-spectra+); an assoc-list or recursive-assoc-list with
;;; partial scaler and amplitude data for as many MIDI notes across the whole
;;; range as possible (see e.g. akoustik-piano-spectra.lsp); or a fixed list of
;;; partial scalers and their respective amplitudes, e.g.
;;; 
;;; ((0.9982489206168257d0 2.014263461548936d0
;;;                        3.0090767036403947d0 4.025891753079929d0
;;;                        5.0391785223504755d0 6.051163469385689d0
;;;                        7.078870767769303d0 8.097071520583727d0 0.0
;;;                        10.04537485275207d0 0.0 12.001576621760098d0)
;;;  (1.0d0 0.9110050427095994d0 0.13036526158661896d0
;;;     0.02384098952910138d0 0.1068663706262658d0
;;;     0.03364125692496988d0 0.06411071328728492d0
;;;     0.010625006807649204d0 0.0 0.0016994831031129103d0 0.0
;;;     0.0018761371417123998d0)))
(defun get-spectrum (midi-note spectrum average)
  (declare (special +slippery-chicken-spectra+))
  (cond ((listp spectrum) spectrum)
        (average
         ;; average spectra over an octave with our desired note in
         ;; the middle?
         (average-spectrum (- midi-note 6) spectrum))
        ((assoc-list-p spectrum) (get-nearest midi-note spectrum))
        ((symbolp spectrum)
         (let ((s (get-data spectrum +slippery-chicken-spectra+)))
           (if s
               (get-nearest midi-note s)
               (error "chord::get-spectrum: can't get ~a from ~
                       +slippery-chicken-spectra+" spectrum))))
        (t (error "chord::get-spectrum: Unkown spectrum arg: ~a" spectrum))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF chord.lsp
