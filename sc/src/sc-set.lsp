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
;;; $$ Last modified: 16:54:21 Tue Feb  7 2012 ICT
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
;;; - keyword argument :octave. NIL or an integer that is the octave designator
;;;   to which all resulting pitches are to be transposed (i.e. the "4" in "C4"
;;;   etc.) Default = NIL.
;;; - keyword argument :remove-duplicates. T or NIL to indicate whether any
;;;   duplicate pitches within an octave that are created by use of the :octave
;;;   keyword argument are to be removed. T = remove duplicates. Default = NIL.
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

;;; SAR Mon Feb  6 13:11:48 GMT 2012: Extended robodoc entry
 
;;; SAR Mon Feb  6 11:58:00 GMT 2012: Added robodoc NB

;;; SAR Sat Feb  4 18:23:23 GMT 2012: Deleted MDE comment, as this is taken
;;; nearly verbatim into the doc below

;;; SAR Sat Feb  4 18:14:25 GMT 2012: Added robodoc entry

;;; ****m* sc-set/force-micro-tone
;;; FUNCTION
;;; Change the value of the MICRO-TONE slot of all pitch objects in a given
;;; sc-set object to the specified <value>.
;;;
;;; NB: Although the MICRO-TONE slot is generally used as a boolean, this
;;;     method allows the user to force-set it to any value.
;;; 
;;; ARGUMENTS
;;; - An sc-set object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - An item of any type that is to be the new value of the MICRO-TONE slot of
;;;   all pitch objects in the given sc-set object. Default = NIL.
;;; 
;;; RETURN VALUE
;;; Always returns NIL.
;;; 
;;; EXAMPLE
#|
;; Create an sc-set object that contains micro-tones and print the MICRO-TONE
;; slot of all of the contained pitch objects to see their values:
(let ((mscs (make-sc-set '(d2 cqs3 fs3 cs4 e4 c5 aqf5 ef6))))
  (loop for p in (data mscs) do (print (micro-tone p))))

=>
NIL 
T 
NIL 
NIL 
NIL 
NIL 
T 
NIL

;; Now apply the force-micro-tone method to the same set using the default
;; value of NIL and print the results

(let ((mscs (make-sc-set '(d2 cqs3 fs3 cs4 e4 c5 aqf5 ef6))))
  (force-micro-tone mscs)
    (loop for p in (data mscs) do (print (micro-tone p))))

=>
NIL 
NIL 
NIL 
NIL 
NIL 
NIL 
NIL 
NIL

;; Using the same sc-set, force all the values to T
(let ((mscs (make-sc-set '(d2 cqs3 fs3 cs4 e4 c5 aqf5 ef6))))
  (force-micro-tone mscs 't)
    (loop for p in (data mscs) do (print (micro-tone p))))

=>
T 
T 
T 
T 
T 
T 
T 
T

|#
;;; SYNOPSIS
(defmethod force-micro-tone ((s sc-set) &optional value)
;;; ****
  (loop for p in (data s) do
       (setf (micro-tone p) value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 

;;; SAR Mon Feb  6 13:28:29 GMT 2012: Extended robodoc entry

;;; SAR Sat Feb  4 18:27:47 GMT 2012: Deleted MDE's comment here as it has been
;;; taken into the doc nearly verbatim.

;;; SAR Sat Feb  4 18:24:46 GMT 2012: Added robodoc entry

;;; ****m* sc-set/get-chromatic 
;;; FUNCTION 
;;; Return those notes of a given sc-set object that are normal chromatic notes
;;; (i.e. no microtones). 
;;;
;;; If a number is given for the <octave> argument, the method will transpose
;;; all returned pitches into the specified octave, in which case any duplicate
;;; pitches are removed.
;;; 
;;; ARGUMENTS
;;; - An sc-set object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - keyword argument :octave. NIL or an integer that is the octave designator
;;;   to which all resulting pitches are to be transposed (i.e. the "4" in "C4"
;;;   etc.) Default = NIL.
;;; - keyword argument :remove-duplicates. T or NIL to indicate whether any
;;;   duplicate pitches within an octave that are created by use of the :octave
;;;   keyword argument are to be removed. T = remove duplicates. Default = NIL.
;;; - keyword argument :as-symbols. T or NIL to indicate whether to return the
;;;   results of the method as a list of note-name symbols rather than a list
;;;   of pitch objects. T = return as note-name symbols. Default = NIL.
;;; - keyword argument :package. The package in which the pitches are to be
;;;   handled. Default = :sc.  
;;; - keyword argument :invert. Get the micro-tone pitches instead.
;;; 
;;; RETURN VALUE
;;; Returns a list of pitch objects by default.
;;;
;;; When the :as-symbols argument is set to T, a list of note-name symbols is
;;; returned instead.
;;; 
;;; EXAMPLE
#|
;;; Returns a list of pitch objects by default
(let ((mscs (make-sc-set '(d2 cqs3 fs3 gqf3 cs4 e4 fqs4 c5 af5 bqf5 d6))))
  (get-chromatic mscs))

=>
(
PITCH: frequency: 73.416, midi-note: 38, midi-channel: 0 
       pitch-bend: 0.0 
       degree: 76, data-consistent: T, white-note: D2
       nearest-chromatic: D2
       src: 0.28061550855636597, src-ref-pitch: C4, score-note: D2 
       qtr-sharp: NIL, qtr-flat: NIL, qtr-tone: NIL,  
       micro-tone: NIL, 
       sharp: NIL, flat: NIL, natural: T, 
       octave: 2, c5ths: 0, no-8ve: D, no-8ve-no-acc: D
       show-accidental: T, white-degree: 15, 
       accidental: N, 
       accidental-in-parentheses: NIL, marks: NIL
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: D2, tag: NIL, 
data: D2
 
PITCH: frequency: 184.997, midi-note: 54, midi-channel: 0 
[...]
)

;; Setting the :as-symbols argument to T returns a list of note-name symbols
;; instead
(let ((mscs (make-sc-set '(d2 cqs3 fs3 gqf3 cs4 e4 fqs4 c5 af5 bqf5 d6))))
  (get-chromatic mscs
                 :as-symbols t))

=> (D2 FS3 CS4 E4 C5 AF5 D6)

;; Giving an integer as the :octave argument transposes all returned  pitches
;; to the specified octave, removing any duplicates by default.
(let ((mscs (make-sc-set '(d2 cqs3 fs3 gqf3 cs4 e4 fqs4 c5 af5 bqf5 d6))))
  (get-chromatic mscs
                 :as-symbols t
                 :octave 4))

=> (FS4 CS4 E4 C4 AF4 D4)

;; Setting the :invert argument to T returns the non-chromatic elements of the
;; given sc-set object instead
(let ((mscs (make-sc-set '(d2 cqs3 fs3 gqf3 cs4 e4 fqs4 c5 af5 bqf5 d6))))
  (get-chromatic mscs
                 :as-symbols t
                 :invert t))

=> (CQS3 GQF3 FQS4 BQF5)

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
    (cond (octave
           (transpose-pitch-list-to-octave 
            result octave 
            :as-symbols as-symbols :package package 
            :remove-duplicates remove-duplicates))
          ;; MDE Sun Feb  5 09:03:06 2012 
          (as-symbols (loop for p in result collect
                           (rm-package (data p) package)))
          (t result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon Feb  6 13:37:14 GMT 2012: Added robodoc entry

;;; ****m* sc-set/get-non-chromatic
;;; FUNCTION
;;; Return those notes of a given sc-set object that are micr-tones (i.e. no
;;; "normal" chromatic notes).
;;; 
;;; If a number is given for the <octave> argument, the method will transpose
;;; all returned pitches into the specified octave, in which case any duplicate
;;; pitches are removed.
;;; 
;;; ARGUMENTS
;;; - An sc-set object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - keyword argument :octave. NIL or an integer that is the octave designator
;;;   to which all resulting pitches are to be transposed (i.e. the "4" in "C4"
;;;   etc.) Default = NIL.
;;; - keyword argument :as-symbols. T or NIL to indicate whether to return the
;;;   results of the method as a list of note-name symbols rather than a list
;;;   of pitch objects. T = return as note-name symbols. Default = NIL.
;;; - keyword argument :package. The package in which the pitches are to be
;;;   handled. Default = :sc.
;;; 
;;; RETURN VALUE
;;; Returns a list of pitch objects by default.
;;;
;;; When the :as-symbols argument is set to T, a list of note-name symbols is
;;; returned instead.
;;; 
;;; EXAMPLE
#|
;; Returns a list of pitch objects by default
(let ((mscs (make-sc-set '(d2 cqs3 fs3 gqf3 cs4 e4 fqs4 c5 af5 bqf5 d6))))
  (get-non-chromatic mscs))

=>
=> (
PITCH: frequency: 134.646, midi-note: 48, midi-channel: 0 
       pitch-bend: 0.5 
       degree: 97, data-consistent: T, white-note: C3
       nearest-chromatic: C3
       src: 0.5146511197090149, src-ref-pitch: C4, score-note: CS3 
       qtr-sharp: 1, qtr-flat: NIL, qtr-tone: 1,  
       micro-tone: T, 
       sharp: NIL, flat: NIL, natural: NIL, 
       octave: 3, c5ths: 0, no-8ve: CQS, no-8ve-no-acc: C
       show-accidental: T, white-degree: 21, 
       accidental: QS, 
       accidental-in-parentheses: NIL, marks: NIL
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: CQS3, tag: NIL, 
data: CQS3
    
PITCH: frequency: 190.418, midi-note: 54, midi-channel: 0 
[...]
)

;; Setting :as-symbols to T returns a list of note-names instead
(let ((mscs (make-sc-set '(d2 cqs3 fs3 gqf3 cs4 e4 fqs4 c5 af5 bqf5 d6))))
  (get-non-chromatic mscs
                     :as-symbols t))

=> (CQS3 GQF3 FQS4 BQF5)

;; Giving an integer as the :octave argument transposes all returned  pitches
;; to the specified octave, removing any duplicates
(let ((mscs (make-sc-set '(d2 cqs3 fs3 gqf3 cs4 e4 fqs4 c5 af5 bqf5 cqs6 d6))))
  (get-non-chromatic mscs
                     :as-symbols t
                     :octave 4))

=> (GQF4 FQS4 BQF4 CQS4)

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

;;; SAR Mon Feb  6 14:06:58 GMT 2012: Deleted MDE's original comment here as it
;;; is taken over nearly verbatim into the robodoc entry below.

;;; SAR Mon Feb  6 14:06:48 GMT 2012: Added robodoc entry

;;; ****m* sc-set/get-srts 
;;; FUNCTION 
;;; Get the sampling-rate conversion factors for the given sc-set object,
;;; whereby 1.0 = unison, 2.0 = one octave higher and 0.5 = one octave lower
;;; etc.
;;;
;;; ARGUMENTS
;;; - An sc-set object.
;;; 
;;; OPTIONAL ARGUMENTS 
;;; - The optional <reference-pitch> is the basis pitch to which the resulting
;;;   factors refer. This will generally be the perceived fundamental pitch of
;;;   the sample (sound file) being modififed ("transposed").
;;; - The optional <offset> argument is the number of semitones to add to the
;;;   pitch of the given set prior to determining the sampling-rate conversion
;;;   factors. 
;;; 
;;; RETURN VALUE
;;; Returns a list of numbers.
;;; 
;;; EXAMPLE
#|
;; Returns a list of factors that are the sampling-rate conversion factor
;; compared to a 'C4 by default:
(let ((mscs (make-sc-set '(d2 fs3 cs4 c5 af5 d6))))
  (get-srts mscs))

=> (0.28061550855636597 0.7071067690849304 1.0594631433486938 2.0
    3.17480206489563 4.4898481369018555)

;; Comparing the same set against a higher reference-pitch will return lower
;; values
(let ((mscs (make-sc-set '(d2 fs3 cs4 c5 af5 d6))))
  (get-srts mscs 'd4))

=> (0.25 0.6299605220704482 0.9438743681693953 1.781797458637491
    2.8284271254540463 4.0)

;; Conversely, comparing the same set against the default reference-pitch but
;; with a postive offset will return higher values

(let ((mscs (make-sc-set '(d2 fs3 cs4 c5 af5 d6))))
  (get-srts mscs 'c4 2))

=> (0.3149802585215549 0.7937005124004939 1.1892071699914617 2.244924096618746
    3.563594828739576 5.039684136344879)

|#
;;; SYNOPSIS
(defmethod get-srts ((s sc-set) &optional 
                                (reference-pitch 'c4)
                                (offset 0))
;;; ****
  (get-srts-aux (data s) reference-pitch offset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon Feb  6 15:29:04 GMT 2012: Addedrobodoc entry

;;; ****m* sc-set/subset-get-srts
;;; FUNCTION
;;; Get the sampling-rate conversion factors for the specified subset of a
;;; given sc-set object, whereby 1.0 = unison, 2.0 = one octave higher and 0.5
;;; = one octave lower etc.
;;; 
;;; ARGUMENTS
;;; - An sc-set object.
;;; - A symbol that is the key of one of the key/data pairs stored in the
;;;   SUBSETS slot of the given sc-set object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - The optional <reference-pitch> is the basis pitch to which the resulting
;;;   factors refer. This will generally be the perceived fundamental pitch of
;;;   the sample (sound file) being modififed ("transposed").
;;; - The optional <offset> argument is the number of semitones to add to the
;;;   pitch of the given set prior to determining the sampling-rate conversion
;;;   factors. 
;;; 
;;; RETURN VALUE
;;; 
;;; 
;;; EXAMPLE
#|
;;; Create an sc-set object with two subsets named 'FL and 'VA, then get the
;;; sampling-rate conversion factors for the 'FL subset
(let ((mscs (make-sc-set '(d2 f2 a2 c3 e3 g3 b3 d4 gf4 bf4 df5 f5 af5 c6)
                         :subsets '((fl (df5 f5 af5 c6))
                                    (va (c3 e3 g3 b3 d4 gf4))))))
  (subset-get-srts mscs 'fl))

=> (2.1189262866973877 2.669679641723633 3.17480206489563 4.0)

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
;;; notes to the top and bottom using the same structure. Repeat num-stacks
;;; times. NB we assume the set is sorted. See also make-stack in the
;;; complete-set class to make a stack from a simple list of pitch symbols.

;;; SAR Mon Feb  6 15:42:53 GMT 2012: Edited robodoc entry

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

;;; SAR Mon Feb  6 16:02:02 GMT 2012: Added robodoc entry and delete MDE
;;; comment here as it was taken into the robodoc nearly verbatim

;;; ****m* sc-set/get-interval-structure
;;; FUNCTION
;;; Get the distances between each pitch in a given sc-set object and the
;;; lowest pitch in that object in DEGREES (which default to quarter-tones in
;;; slippery chicken). (This method assumes that the given sc-set object is
;;; sorted from low to high).
;;; 
;;; ARGUMENTS
;;; - An sc-set object.
;;; 
;;; RETURN VALUE
;;; A list of integers
;;; 
;;; EXAMPLE
#|
;; Returns the distances in degrees (which are set to quarter-tones by default)
(let ((mscs (make-sc-set '(c4 e4 g4))))
  (get-interval-structure mscs))

=> (8 14)

|#
;;; SYNOPSIS
(defmethod get-interval-structure ((s sc-set))
;;; ****
  (let ((lowest-degree (degree (first (data s)))))
    (loop for i in (rest (data s)) collect
          (- (degree i) lowest-degree))))
     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon Feb  6 16:07:24 GMT 2012: Added robodoc entry

;;; ****m* sc-set/set-position
;;; FUNCTION
;;; Get the position (zero-index) of a specified pitch object within a given
;;; sc-set object. 
;;; 
;;; ARGUMENTS
;;; - A pitch object.
;;; - An sc-set object.
;;; 
;;; RETURN VALUE
;;; An integer.
;;; 
;;; EXAMPLE
#|
(let ((mscs (make-sc-set '(d2 f2 a2 c3 e3 g3 b3 d4 gf4 bf4 df5 f5 af5 c6))))
  (set-position (make-pitch 'e3) mscs))

=> 4

|#
;;; SYNOPSIS
(defmethod set-position ((p pitch) (s sc-set))
;;; ****
  (position p (data s) :test #'pitch=))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon Feb  6 16:13:55 GMT 2012: Added robodoc entry

;;; ****m* sc-set/get-degrees
;;; FUNCTION
;;; Return the pitches contained in the given sc-set object as a list of
;;; DEGREES (which default to quarter-tones in slippery chicken).
;;; 
;;; ARGUMENTS
;;; - An sc-set object.
;;; 
;;; RETURN VALUE
;;; A list of integers.
;;; 
;;; EXAMPLE
#|
(let ((mscs (make-sc-set '(d2 f2 a2 c3 e3 g3 b3 d4 gf4 bf4 df5 f5 af5 c6))))
  (get-degrees mscs))

=> (76 82 90 96 104 110 118 124 132 140 146 154 160 168)

|#
;;; SYNOPSIS
(defmethod get-degrees ((s sc-set))
;;; ****
  (loop for p in (data s) collect (degree p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon Feb  6 16:18:45 GMT 2012: Added robodoc entry

;;; ****m* sc-set/get-freqs
;;; FUNCTION
;;; Return the pitches of a given sc-set object as a list of Hz frequencies
;;; 
;;; ARGUMENTS
;;; - An sc-set object.
;;; 
;;; RETURN VALUE
;;; A list of numbers
;;; 
;;; EXAMPLE
#|
(let ((mscs (make-sc-set '(d2 f2 a2 c3 e3 g3 b3 d4 gf4 bf4 df5 f5 af5 c6))))
  (get-freqs mscs))

=> (73.41618871368837 87.30705289160142 109.99999810639679 130.8127784729004
    164.81377633519514 195.99771591817216 246.94163930037348 293.6647548547535
    369.99440456398133 466.1637395092839 554.3652698843016 698.4564231328113
    830.6093584209975 1046.5022277832031)

|#
;;; SYNOPSIS
(defmethod get-freqs ((s sc-set))
;;; ****
  (loop for p in (data s) collect (frequency p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon Feb  6 16:26:18 GMT 2012: Added robodoc entry

;;; ****m* sc-set/get-midi 
;;; FUNCTION
;;; Return the pitches of a given sc-set object as a list of their equivalent
;;; MIDI key numbers.
;;; 
;;; ARGUMENTS
;;; - An sc-set object.
;;; 
;;; RETURN VALUE
;;; A list of numbers
;;; 
;;; EXAMPLE
#|
(let ((mscs (make-sc-set '(d2 f2 a2 c3 e3 g3 b3 d4 gf4 bf4 df5 f5 af5 c6))))
  (get-midi mscs))

=> (38 41 45 48 52 55 59 62 66 70 73 77 80 84)

|#
;;; SYNOPSIS
(defmethod get-midi  ((s sc-set))
;;; ****
  (loop for p in (data s) collect (midi-note p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon Feb  6 16:30:17 GMT 2012: Added robodoc entry

;;; ****m* sc-set/get-semitones-from-middle-note
;;; FUNCTION
;;; Return a list of numbers that are the distances in semitones of each pitch
;;; in a given sc-set object from the middle note of that object. 
;;;
;;; NB: If the given sc-object contains an even number of pitch objects, the
;;;     middle note is determined to be the first note of the second half of
;;;     the set.
;;; 
;;; ARGUMENTS
;;; - An sc-set object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - A symbol that is the key of one of the key/data pairs contained in the
;;;   SUBSETS slot of the given sc-set object.
;;; RETURN VALUE
;;; A list of positive and negative numbers.
;;; 
;;; EXAMPLE
#|
;; With an odd number of items in the sc-set object, the method returns the
;; same number of positive and negative numbers (non-zero)
(let ((mscs (make-sc-set '(d2 f2 a2 c3 e3 g3 b3 d4 gf4 bf4 df5 f5 af5))))
  (get-semitones-from-middle-note mscs))

=> (-21.0 -18.0 -14.0 -11.0 -7.0 -4.0 0.0 3.0 7.0 11.0 14.0 18.0 21.0)

;; With an even number of items in the sc-set object, the middle note is
;; considered to be the first note of the second half of the set
(let ((mscs (make-sc-set '(d2 f2 a2 c3 e3 g3 b3 d4 gf4 bf4 df5 f5 af5 c6))))
  (get-semitones-from-middle-note mscs))

=> (-24.0 -21.0 -17.0 -14.0 -10.0 -7.0 -3.0 0.0 4.0 8.0 11.0 15.0 18.0 22.0)

;; Setting the optional <subset> argument to a symbol that is the key of a
;; given key/data pair in the sc-object's SUBSETS slot applies the method to
;; that subset only
(let ((mscs (make-sc-set '(d2 f2 a2 c3 e3 g3 b3 d4 gf4 bf4 df5 f5 af5 c6)
                         :subsets '((fl (df5 f5 af5 c6))
                                    (va (c3 e3 g3 b3 d4 gf4))))))
  (get-semitones-from-middle-note mscs 'fl))

=> (-7.0 -3.0 0.0 4.0)

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

;;; SAR Mon Feb  6 16:46:22 GMT 2012: Added robodoc info. Deleted MDE's comment
;;; here as it is taken nearly verbatim into the doc below.

;;; ****m* sc-set/add
;;; FUNCTION
;;; Create a new sc-set object from the data of two other specified sc-set
;;; objects.
;;; 
;;; NB: Any subsets contained in the original sc-set objects are lost in the
;;;     process. 
;;;
;;; ARGUMENTS
;;; - A first sc-set object.
;;; - A second sc-set object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; (- optional argument <ignore> is internal only)
;;; 
;;; RETURN VALUE
;;; 
;;; 
;;; EXAMPLE
#|
(let ((mscs1 (make-sc-set '(d2 a2 e3 b3 gf4 df5 af5)))
      (mscs2 (make-sc-set '(f2 c3 g3 d4 bf4 f5 c6))))
  (add mscs1 mscs2))

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

**** N.B. All pitches printed as symbols only, internally they are all 
pitch-objects.


    subsets: 
    related-sets: 
SCLIST: sclist-length: 14, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: NIL, tag: NIL, 
data: (D2 F2 A2 C3 E3 G3 B3 D4 GF4 BF4 DF5 F5 AF5 C6)

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

;;; SAR Mon Feb  6 16:59:16 GMT 2012: Added robodoc info

;;; ****m* sc-set/contains-pitches
;;; FUNCTION
;;; Check to see if a given sc-set object contains pitch objects for all of the
;;; specified note-names. The method returns NIL if any one of the specified
;;; pitches is not found in the given sc-set object.
;;; 
;;; ARGUMENTS
;;; - An sc-set object.
;;; - A list of note-name symbosl. NB: If checking for only one pitch, that
;;;   pitch must be passed as a single-item list.
;;; 
;;; RETURN VALUE
;;; T or NIL.
;;; 
;;; EXAMPLE
#|
;; Returns T when all specified pitches are contained in the given sc-set
;; object 
(let ((mscs (make-sc-set '(d2 f2 a2 c3 e3 g3 b3 d4 gf4 bf4 df5 f5 af5 c6))))
  (contains-pitches mscs '(d2 e3 gf4 af5)))

=> T

;; Returns NIL if any one of the specififed pitches is not contained in the
;; given sc-set object.
(let ((mscs (make-sc-set '(d2 f2 a2 c3 e3 g3 b3 d4 gf4 bf4 df5 f5 af5 c6))))
  (contains-pitches mscs '(d2 e3 gf4 b4 af5)))

=> NIL

|#
;;; SYNOPSIS
(defmethod contains-pitches ((s sc-set) pitches)
;;; ****
  (all-members (data s) (init-pitch-list pitches nil) #'pitch=))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon Feb  6 17:32:38 GMT 2012: Added robodoc entry

;;; ****m* sc-set/create-chord
;;; FUNCTION
;;; Create a chord object from the pitches of the given sc-set object.
;;; 
;;; ARGUMENTS
;;; - An sc-set object.
;;; 
;;; RETURN VALUE
;;; A chord object.
;;; 
;;; EXAMPLE
#|
(let ((mscs (make-sc-set '(d2 c3 d4 df5 c6))))
  (create-chord mscs))

=> 
CHORD: auto-sort: T, marks: NIL, micro-tone: NIL
SCLIST: sclist-length: 5, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: NIL, tag: NIL, 
data: (
PITCH: frequency: 73.416, midi-note: 38, midi-channel: 0 
[...]
)


|#
;;; SYNOPSIS
(defmethod create-chord ((s sc-set))
;;; ****
  (make-chord (data s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon Feb  6 17:42:16 GMT 2012: Added robodoc entry

;;; ****m* sc-set/create-event
;;; FUNCTION
;;; Create an event object (that is a chord) from a given sc-set object,
;;; specifiying a rhythmic value and a start-time (in seconds).
;;; 
;;; ARGUMENTS
;;; - An sc-set object.
;;; - A rhythmic unit, either as a numerical value (32, 16 etc) or a symbol
;;;   that is an alphabetic shorthand ('e, 's etc).
;;; - A number that is the start time in seconds. 
;;; 
;;; OPTIONAL ARGUMENTS
;;; - A number that is the start-time in quarter-notes rather than seconds (see
;;;   event class documentation for more details)
;;; 
;;; RETURN VALUE
;;; An event object.
;;; 
;;; EXAMPLE
#|
;; Create an event from the specified sc-set object that is a quarter-note
;; chord starting at 0.0 seconds
(let ((mscs (make-sc-set '(d2 c3 d4 df5 c6))))
  (create-event mscs 'q 0.0))

=>
EVENT: start-time: 0.000, end-time: NIL, 
       duration-in-tempo: 0.000, 
       compound-duration-in-tempo: 0.000, 
       amplitude: 0.700 
       bar-num: -1, marks-before: NIL, 
       tempo-change: NIL 
       instrument-change: NIL 
       display-tempo: NIL, start-time-qtrs: 0.000, 
       midi-time-sig: NIL, midi-program-changes: NIL, 
       8va: 0
       pitch-or-chord: 
CHORD: auto-sort: T, marks: NIL, micro-tone: NIL
SCLIST: sclist-length: 5, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: NIL, tag: NIL, 
data: (
PITCH: frequency: 73.416, midi-note: 38, midi-channel: 0 
[...]
RHYTHM: value: 4.000, duration: 1.000, rq: 1, is-rest: NIL, 
        score-rthm: 4.0f0, undotted-value: 4, num-flags: 0, num-dots: 0, 
        is-tied-to: NIL, is-tied-from: NIL, compound-duration: 1.000, 
        is-grace-note: NIL, needs-new-note: T, beam: NIL, bracket: NIL, 
        rqq-note: NIL, rqq-info: NIL, marks: NIL, marks-in-part: NIL, 
        letter-value: 4, tuplet-scaler: 1, grace-note-duration: 0.05
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: Q, tag: NIL, 
data: Q


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

;;; SAR Mon Feb  6 18:02:05 GMT 2012: Added robodoc entry

;;; ****m* sc-set/pitch-symbols
;;; FUNCTION
;;; Return the pitches of a given sc-set object as a list of note-name
;;; symbols. 
;;; 
;;; ARGUMENTS
;;; - An sc-set object.
;;; 
;;; RETURN VALUE
;;; A list of note-name symbols.
;;; 
;;; EXAMPLE
#|
(let ((mscs (make-sc-set '(d2 c3 d4 df5 c6))))
  (pitch-symbols mscs))

=> (D2 C3 D4 DF5 C6)

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

