;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* sclist/sc-set
;;; NAME 
;;; player
;;;
;;; File:             sc-set.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> sclist -> sc-set
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Implementation of the sc-set class which holds pitch set
;;;                   information for harmonic and pitch manipulation. 
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    August 10th 2001
;;;
;;; $$ Last modified: 10:33:52 Sat Jan 14 2012 ICT
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

;;; the data slot is the list of pitches.

(defclass sc-set (sclist)
  ;; sort the given pitches from lowest to highest.
  ((auto-sort :accessor auto-sort :type boolean :initarg :auto-sort
              :initform t)
   ;; sometimes it's useful to divide sc-sets into subsets.  These can be given
   ;; here in the form of an assoc-list of pitches which must be part of the
   ;; main set (data slot).  One use might be to create subsets that particular
   ;; instruments can play; these would then be selected in the chord-function
   ;; passed to the instrument object.
   (subsets :accessor subsets :initarg :subsets :initform nil)
   ;; this is similiar to subsets only that the pitches given here don't have
   ;; to be part of the main set.  Can be used, for example, for pitches
   ;; missing from the main set....
   (related-sets :accessor related-sets :initarg :related-sets :initform nil)
   ;; 26/2/07: when choosing pitches for an instrument it's useful to know
   ;; which pitches have already been selected for other
   ;; instruments. sc-make-sequenz calles get-notes for the instrument with a
   ;; given set, and also knows the 'global sequence number' (i.e. irrespective
   ;; or sections and subsections, the current sequence count), so we can store
   ;; the notes used against that instrument for the current count in a
   ;; recursive-assoc-list.
   (used-notes :accessor used-notes :initform nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((s sc-set) &rest initargs)
  (declare (ignore initargs))
  (setf (subsets s) (make-ral (format nil "sc-set-~a-subsets" (id s))
                              (subsets s)))
  (make-ral-pitch-lists (subsets s) (auto-sort s))
  (setf (related-sets s) (make-ral (format nil "sc-set-~a-related-sets" (id s))
                                   (related-sets s)))
  (check-subsets (subsets s) s)
  (setf (used-notes s) (make-ral 'used-notes nil))
  (make-ral-pitch-lists (related-sets s) (auto-sort s)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod verify-and-store :after ((s sc-set))
  (setf (slot-value s 'data) (init-pitch-list (data s) (auto-sort s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone ((s sc-set))
  (clone-with-new-class s 'sc-set))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone-with-new-class :around ((s sc-set) new-class)
  (declare (ignore new-class))
  (let ((sclist (call-next-method)))
    (setf (slot-value sclist 'auto-sort) (auto-sort s)
          (slot-value sclist 'used-notes) (my-copy-list (used-notes s))
          (slot-value sclist 'subsets) (when (subsets s)
                                         (clone (subsets s)))
          (slot-value sclist 'related-sets) (when (related-sets s) 
                                              (clone (related-sets s))))
    sclist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object :before ((s sc-set) stream)
  (format stream "~&SC-SET: auto-sort: ~a, used-notes: ~a~
                  ~%~%**** N.B. All pitches printed as symbols only, ~
                  internally they are all ~%pitch-objects.~%~%"
          (auto-sort s) (used-notes s))
  (format stream "~%    subsets: ")
  (print-ral-of-pitch-lists (subsets s) stream)
  (format stream "~%    related-sets: ")
  (print-ral-of-pitch-lists (related-sets s) stream))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-used-notes ((s sc-set) seq-num &optional instrument)
  (let* ((uns (used-notes s))
         (ups (when uns (get-data (if instrument 
                                      (list seq-num instrument)
                                    seq-num)
                                  uns nil))))
    (when ups
      (if instrument
          (data ups)
        (remove-duplicates 
         (loop for ins in (data (data ups)) appending (data ins)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; MDE:
;;; Move all pitches to <octave> if not nil.

;;; SAR Tue Jan 31 21:21:00 GMT 2012: Delete MDE's original comment here as it
;;; has been taken into the robodoc below nearly verbatim

;;; SAR Tue Jan 31 21:20:49 GMT 2012: Added robodoc info

;;; ****m* sc-set/round-inflections
;;; FUNCTION
;;; Get the microtones of a given sc-set object, rounded to the nearest
;;; chromatic note. 
;;;
;;; This method returns only the rounded microtones, and not any of the pitches
;;; of the original sc-set that are already chromatic.
;;;
;;; By default, this method only gets those microtones that are less than a
;;; quarter-tone. This behavior can be changed by setting the :qtr-tones-also
;;; argument to T. 
;;;
;;; An optional argument allows for all pitches to be moved to a specified
;;; octave, in which case any duplicate pitches are removed.
;;; 
;;; ARGUMENTS
;;; - An sc-set object.
;;; 
;;; OPTIONAL ARGUMENTS

;;; - keyword argument :qtr-tones-also. T or NIL to indicate whether
;;;   quarter-tones are also to be rounded to the nearest chromatic pitch and
;;;   returned. T = round and return. Default = NIL.
;;; - keyword argument :octave. An integer that is the octave designator to
;;;   which all resulting pitches are to be transposed (i.e. the "4" in "C4"
;;;   etc.) 
;;; - keyword argument :remove-duplicates. T or NIL to indicate whether
;;;   any duplicate pitches within an octave that are created by use of the
;;;   :octave keyword argument are to be removed. T = remove
;;;   duplicates. Default =  NIL.
;;; - keyword argument :as-symbols. T or NIL to indicate whether to return the
;;;   results of the method as a list of note-name symbols rather than a list
;;;   of pitch objects. T = return as note-name symbols. Default = NIL. 
;;; - keyword argument :package. The package in which the pitches are to be
;;;   handled. Default = :sc.  
;;; 
;;; RETURN VALUE
;;; A list of pitch objects.
;;; 
;;; EXAMPLE
#|
;; First set the *scale* environment of CM (which is used by slippery chicken)
;; to twelfth-tones
(setf cm::*scale* (cm::find-object 'twelfth-tone))

=> #<tuning "twelfth-tone">

;; By default the method returns a list of pitch objects.
(let ((mscs (make-sc-set '(c4 cts4 css4 cqs4 cssf4 cstf4 cs4))))
  (round-inflections mscs))

=>
(
PITCH: frequency: 261.626, midi-note: 60, midi-channel: 0 
[...]
data: C4
PITCH: frequency: 261.626, midi-note: 60, midi-channel: 0 
[...]
data: C4
[...]
PITCH: frequency: 277.183, midi-note: 61, midi-channel: 0 
[...]
data: CS4
[...]
PITCH: frequency: 277.183, midi-note: 61, midi-channel: 0 
[...]
data: CS4
)

;; Setting the :as-symbols argument to T returns a list of note-name symbols
;; instead 
(let ((mscs (make-sc-set '(c4 cts4 css4 cqs4 cssf4 cstf4 cs4))))
  (round-inflections mscs :as-symbols t))

=> (C4 C4 CS4 CS4)

;; Setting the :qtr-tones-also argument to T returns causes quarter-tones to be
;; rounded and returned as well.
(let ((mscs (make-sc-set '(c4 cts4 css4 cqs4 cssf4 cstf4 cs4))))  
  (round-inflections mscs 
		     :qtr-tones-also T
		     :as-symbols t))

=> (C4 C4 C4 CS4 CS4)

;; Specifying an octave transposes all returned pitches to that octave,
;; removing any duplicates by default
(let ((mscs (make-sc-set '(c2 cts3 css4 cqs5 cssf6 cstf7 cs8))))  
  (round-inflections mscs 
		     :qtr-tones-also T
		     :octave 4
		     :as-symbols t))

=> (C4 CS4)

;; The removal of the duplicates can be turned off by setting the
;; :remove-duplicates argument to NIL 
(let ((mscs (make-sc-set '(c2 cts3 css4 cqs5 cssf6 cstf7 cs8))))  
  (round-inflections mscs 
		     :qtr-tones-also T
		     :octave 4
		     :remove-duplicates NIL
		     :as-symbols t))

=> (C4 C4 C4 CS4 CS4)

|#
;;; SYNOPSIS
(defmethod round-inflections ((s sc-set) 
                              &key
                              qtr-tones-also
                              octave
                              (remove-duplicates t) ;; only if octave!
                              (as-symbols nil)
                              (package :sc))
;;; ****
  (let ((result
         (loop for p in (data s) 
	    when (if qtr-tones-also
		     (micro-tone p)
		     (and (micro-tone p)
			  (not (qtr-tone p))))
	    collect (pitch-round p 
				 ;; if we're going to transpose to an
				 ;; octave get pitch objects and convert to
				 ;; symbols later 
				 :as-symbol (if octave nil as-symbols)
				 :package package))))
    (if octave
        (transpose-pitch-list-to-octave 
         result octave 
         :as-symbols as-symbols :package package 
         :remove-duplicates remove-duplicates)
	result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; just change the micro-tone slot of all pitches to <value>
;;; ****m* sc-set/force-micro-tone
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
(defmethod force-micro-tone ((s sc-set) &optional value)
;;; ****
  (loop for p in (data s) do
       (setf (micro-tone p) value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 
;;; Get the notes from the set that are normal chromatic notes,
;;; i.e. no microtones.  If octave is given, put them all into that octave

;;; ****m* sc-set/get-chromatic
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
(defmethod get-chromatic ((s sc-set) 
                          &key 
                          (octave nil)
                          (remove-duplicates t) ;; only if octave!
                          (as-symbols nil)
                          (package :sc)
                          (invert nil))
;;; **** 
  ;; this will get just the microtones
  (let ((result (loop for p in (data s)
                   unless (if invert
                              (not (micro-tone p))
                              (micro-tone p))
                   collect (clone p))))
    ;; (print result)
    (if octave
        (transpose-pitch-list-to-octave 
         result octave 
         :as-symbols as-symbols :package package 
         :remove-duplicates remove-duplicates)
        result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* sc-set/get-non-chromatic
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
(defmethod get-non-chromatic ((s sc-set) 
                              &key 
                              (octave nil)
                              (as-symbols nil)
                              (package :sc))
;;; ****
  (get-chromatic s :octave octave :as-symbols as-symbols :package package 
                 :invert t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Get the semitone transpositions for the main set.
;;; offset is number of semitones to add.
;;; reference-pitch is the apparent pitch of the sample we're going to
;;; transpose.  

;;; ****m* sc-set/get-semitones
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
(defmethod get-semitones ((s sc-set) &optional 
                                (reference-pitch 'c4)
                                (offset 0))
;;; ****
  (loop for srt in 
        (get-srts-aux (data s) reference-pitch offset)
      collect (srt srt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Get the sampling-rate conversion factors for the main set.
;;; offset is number of semitones to add.
;;; reference-pitch is the apparent pitch of the sample we're going to
;;; transpose.  

;;; ****m* sc-set/get-srts
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
(defmethod get-srts ((s sc-set) &optional 
                                (reference-pitch 'c4)
                                (offset 0))
;;; ****
  (get-srts-aux (data s) reference-pitch offset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* sc-set/subset-get-srts
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
(defmethod subset-get-srts  ((s sc-set) subset &optional 
                                               (reference-pitch 'c4)
                                               (offset 0))
;;; ****
  (get-srts-aux (data (get-data subset (subsets s)))
                reference-pitch offset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Get the interval structure of the set from bottom to top note and add
;;; notes to the top and bottom using the same structure.  Repeat num-stacks
;;; times.  NB we assume the set is sorted.  See also make-stack in the
;;; complete-set class to make a stack from a simple list of pitch symbols.

;;; ****m* sc-set/stack
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
(let ((set (make-sc-set '(c4 e4 g4))))
  (print (stack set 1))
  (stack set 3))
SC-SET: auto-sort: T, used-notes: 
...
data: (F3 AF3 C4 E4 G4 B4 D5)
**************
SC-SET: auto-sort: T, used-notes: 
...
data: (EF2 FS2 BF2 CS3 F3 AF3 C4 E4 G4 B4 D5 FS5 A5 CS6 E6)
|#
;;; SYNOPSIS
(defmethod stack ((s sc-set) &optional (num-stacks 1) id)
;;; ****
  (let* ((distances (get-interval-structure s))
         (degrees (get-degrees s))
         (result degrees)
         chord)
    (loop repeat num-stacks do
          (setf result (stack-aux result distances)))
    (setf result (degrees-to-notes result))
    ;; MDE Sat Jan 14 10:25:25 2012 -- try and get better spellings
    (setf chord (make-chord result :midi-channel 1 :microtones-midi-channel 2))
    (respell-chord chord)
    ;; return a new set, using the given id or if not given, the same id as the
    ;; original set 
    (make-sc-set (data chord) :id (if id id (id s)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Get the degree distances of each note to the bottom note of the set
;;; (assumes set is sorted).

;;; ****m* sc-set/get-interval-structure
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
(defmethod get-interval-structure ((s sc-set))
;;; ****
  (let ((lowest-degree (degree (first (data s)))))
    (loop for i in (rest (data s)) collect
          (- (degree i) lowest-degree))))
     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* sc-set/set-position
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
(defmethod set-position ((p pitch) (s sc-set))
;;; ****
  (position p (data s) :test #'pitch=))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* sc-set/get-degrees
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
(defmethod get-degrees ((s sc-set))
;;; ****
  (loop for p in (data s) collect (degree p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* sc-set/get-freqs
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
(defmethod get-freqs  ((s sc-set))
;;; ****
  (loop for p in (data s) collect (frequency p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* sc-set/get-midi
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
(defmethod get-midi  ((s sc-set))
;;; ****
  (loop for p in (data s) collect (midi-note p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* sc-set/get-semitones-from-middle-note
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
(defmethod get-semitones-from-middle-note ((s sc-set) &optional subset)
;;; ****
  (let* ((notes (if subset (get-data-data subset (subsets s))
                  (data s)))
         (middle (floor (length notes) 2))
         (middle-midi (midi-note-float (nth middle notes))))
    (loop for p in notes collect (- (midi-note-float p) middle-midi))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A new set is made out of the data of the two arguments.  This
;;; means of course that subsets etc. get lost.

;;; ****m* sc-set/add
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
(defmethod add ((s1 sc-set) (s2 sc-set) &optional ignore)
;;; ****
  (declare (ignore ignore))
  (flet ((get-id (object)
           (if (and (linked-named-object-p object)
                    (this object))
               (list-to-string (this object)"-")
             (id object))))
    (let ((notes (append (data s1) (data s2))))
      (make-sc-set notes :id (when (and (id s1) (id s2))
                               (format nil "~a-plus-~a"
                                       (get-id s1) (get-id s2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* sc-set/contains-pitches
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
(defmethod contains-pitches ((s sc-set) pitches)
;;; ****
  (all-members (data s) (init-pitch-list pitches nil) #'pitch=))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* sc-set/create-chord
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
(defmethod create-chord ((s sc-set))
;;; ****
  (make-chord (data s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* sc-set/create-event
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
(defmethod create-event ((s sc-set) rhythm start-time &optional start-time-qtrs)
;;; ****
  (unless start-time-qtrs
    (setf start-time-qtrs start-time))
  (let ((e (make-event (create-chord s) rhythm :start-time start-time)))
    (setf (start-time-qtrs e) start-time-qtrs)
    e))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* sc-set/pitch-symbols
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
(defmethod pitch-symbols ((s sc-set))
;;; ****
  (get-ids-from-pitch-list (data s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-srts-aux (pitch-list &optional 
                                (reference-pitch 'c4)
                                (offset 0))
  (let ((freq (if (numberp reference-pitch) 
                  reference-pitch
                (note-to-freq reference-pitch)))
        (offset-srt (semitones offset)))
    (loop for p in pitch-list do
          (unless (pitch-p p)
            (setf p (make-pitch p)))
        collect (* offset-srt (/ (frequency p) freq)))))
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun stack-aux (degrees distances)
  (let ((lowest (first degrees)) ;; assumes degrees are sorted!
        (highest (first (last degrees)))
        (max (note-to-degree 'b10))
        (result (copy-list degrees)))
    (loop for d in distances 
       for low = (- lowest d)
       for high = (+ highest d)
       do 
       (when (<= high max)
         (push high result))
       (when (> low 0 )
         (push low result)))
    (sort result #'<)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Tue Jan 31 20:05:39 GMT 2012: Added robodoc info

;;; ****f* sc-set/make-sc-set 
;;; FUNCTION 
;;; Create an sc-set object, which holds pitch-set information for harmonic and
;;; pitch manipulation.
;;; 
;;; ARGUMENTS
;;; - A list of note-name symbols that is to be the set (pitch-set) for the
;;;   given sc-set object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - keyword argument :id. A symbol that is to be the ID of the given sc-set
;;;   object.
;;; - keyword argument :subsets. An assoc-list of key/data pairs, in which the
;;;   data is a list of note-name symbols that are a subset of the main
;;;   set. One use for this keyword argument might be to create subsets that
;;;   particular instruments can play; these would then be selected in the
;;;   chord-function passed to the instrument object.
;;; - keyword argument :related-sets. An assoc-list of key/data pairs, similar
;;;   to :subsets, only that the pitches given here do not have to be part of
;;;   the main set. This can be used, for example, for pitches missing from the
;;;   main set.
;;; - keyword argument :auto-sort. T or NIL to indicate whether the specified
;;;   pitches (note-name symbols) are to be automatically  sorted from lowest
;;;   to highest. T = sort. Default = T.
;;; 
;;; RETURN VALUE
;;; An sc-set object.
;;; 
;;; EXAMPLE
#|
;; Simplest usage, with no keyword arguments; returns an sc-set object
(make-sc-set '(d2 cs3 fs3 cs4 e4 c5 af5 ef6))

=> 
SC-SET: auto-sort: T, used-notes: 
RECURSIVE-ASSOC-LIST: recurse-simple-data: T
                      num-data: 0
                      linked: NIL
                      full-ref: NIL
ASSOC-LIST: warn-not-found T
CIRCULAR-SCLIST: current 0
SCLIST: sclist-length: 0, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: USED-NOTES, tag: NIL, 
data: NIL

N.B. All pitches printed as symbols only, internally they are all 
pitch-objects.

    subsets: 
    related-sets: 
SCLIST: sclist-length: 8, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: NIL, tag: NIL, 
data: (D2 CS3 FS3 CS4 E4 C5 AF5 EF6)

;; With keyword arguments
(make-sc-set '(d2 cs3 fs3 cs4 e4 c5 af5 ef6)
	     :id 'scs1
	     :subsets '((violin (e4 c5 af5 ef6))
			(viola (cs4 e4)))
	     :related-sets '((missing (ds2 e2 b3 cs6 d6))))

=> 
SC-SET: auto-sort: T, used-notes: 
RECURSIVE-ASSOC-LIST: recurse-simple-data: T
                      num-data: 0
                      linked: NIL
                      full-ref: NIL
ASSOC-LIST: warn-not-found T
CIRCULAR-SCLIST: current 0
SCLIST: sclist-length: 0, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: USED-NOTES, tag: NIL, 
data: NIL

N.B. All pitches printed as symbols only, internally they are all 
pitch-objects.

    subsets: 
VIOLIN: (E4 C5 AF5 EF6)
VIOLA: (CS4 E4)
    related-sets: 
MISSING: (DS2 E2 B3 CS6 D6)
SCLIST: sclist-length: 8, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: SCS1, tag: NIL, 
data: (D2 CS3 FS3 CS4 E4 C5 AF5 EF6)

|#
;;; SYNOPSIS
(defun make-sc-set (sc-set &key id subsets related-sets (auto-sort t))
;;; ****
  (make-instance 'sc-set :id id :data sc-set :subsets subsets 
                 :related-sets related-sets :auto-sort auto-sort))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-ral-of-pitch-lists (ral stream)
  (unless (is-ral ral)
    (error "sc-set::print-ral-of-pitch-lists: first argument must be a ~
            recursive-association-list: ~a" ral))
  (let ((all-refs (get-all-refs ral)))
    (loop 
        for ref in all-refs
        for no = (get-data ref ral)
        do (format stream "~&~a: ~a"
                   (id no)
                   (get-ids-from-pitch-list (data no))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun init-pitch-list (pitches auto-sort)
  (let ((result (loop for pitch in pitches 
                    do
                      (unless pitch
                        (error "~a~&sc-set::init-pitch-list: pitch is nil!"
                               pitches))
                    collect (make-pitch pitch))))
    (if auto-sort
        (sort (copy-list result)
              #'(lambda (x y) (< (frequency x) (frequency y))))
      result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-ral-pitch-lists (ral auto-sort)
  (loop 
      for ss in (data ral) 
      for pitches = (data ss)
      for i from 0 do
        (if (is-ral pitches)
            (make-ral-pitch-lists pitches auto-sort)
          ;; 4/3/07: could be that we get something like:
          ;; (tc1 ((ds2 e3 a4) "a-tag"))
          ;; so recreate the named-object if so
          (if (simple-listp pitches)
              (setf (data (nth i (data ral)))
                (init-pitch-list pitches auto-sort))
            (setf (nth i (data ral)) 
              (make-named-object (id ss) 
                                 (init-pitch-list (first pitches) auto-sort)
                                 (second pitches)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun check-subsets (subsets sc-set)
  (loop 
      for ss in (data subsets) 
      for pitches = (data ss)
      for i from 0 do
        (if (is-ral pitches)
            (check-subsets pitches sc-set)
          (loop for pitch in (data ss) do
                (unless (pitch-member pitch (data sc-set))
                  (error "sc-set::check-subsets: Note ~a given in subset ~a ~
                            of set ~a is not part of the main set."
                         (id pitch) (id ss) (id sc-set)))))))
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sc-set-p (thing)
  (typep thing 'sc-set))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
;;; EOF sc-set.lsp

