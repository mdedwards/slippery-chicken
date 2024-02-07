;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* sclist/sc-set
;;; NAME 
;;; player
;;;
;;; File:             sc-set.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> sclist -> chord ->
;;;                   sc-set
;;;
;;; Version:          1.0.12
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
;;; $$ Last modified:  17:38:41 Wed Feb  7 2024 CET
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

;;; the data slot is the list of pitches.

(defclass sc-set (chord)
  ;; sort the given pitches from lowest to highest.
  ((auto-sort :accessor auto-sort :type boolean :initarg :auto-sort
              :initform t)
   ;; sometimes it's useful to divide sc-sets into subsets.  These can be given
   ;; here in the form of an assoc-list of pitches which must be part of the
   ;; main set (data slot).  One use might be to create subsets that particular
   ;; instruments can play; these could for instance be selected in the
   ;; chord-function passed to the instrument object.  In any case, if the
   ;; instrument has a subset-id slot, and the current set contains a subset
   ;; with that ID, the pitches the instrument may play are limited to that
   ;; subset.
   (subsets :accessor subsets :initarg :subsets :initform nil)
   ;; this is similar to subsets only that the pitches given here don't have
   ;; to be part of the main set.  Can be used, for example, for pitches
   ;; missing from the main set....
   (related-sets :accessor related-sets :initarg :related-sets :initform nil)
   ;; 26/2/07: when choosing pitches for an instrument it's useful to know
   ;; which pitches have already been selected for other
   ;; instruments. sc-make-sequenz calls get-notes for the instrument with a
   ;; given set, and also knows the 'global sequence number' (i.e. irrespective
   ;; of sections and subsections, the current sequence count), so we can store
   ;; the notes used against that instrument for the current count in a
   ;; recursive-assoc-list.
   (used-notes :accessor used-notes :initform nil)
   ;; MDE Mon May 20 12:50:42 2013 -- warn when removing duplicate pitches?
   (warn-dups :accessor warn-dups :type boolean :initarg :warn-dups :initform t)
   ;; MDE Mon May 20 12:51:51 2013 -- auto-remove duplicate pitches?
   (rm-dups :accessor rm-dups :type boolean :initarg :rm-dups :initform t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((s sc-set) &rest initargs)
  (declare (ignore initargs))
  ;; (print s)
  ;; MDE Sat Oct 26 11:35:23 2013 -- just to trigger the setf method
  (setf (subsets s) (subsets s)
        (related-sets s) (related-sets s))
  (reset s))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Sat Oct 26 12:49:41 2013 
(defmethod verify-and-store :before ((s sc-set))
  (when (chord-p (data s))
    (setf (data s) (data (data s)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod verify-and-store :after ((s sc-set))
  ;; (print (data s))
  (let* ((pl (init-pitch-list (data s) (auto-sort s)))
         ;; MDE Mon May 20 12:52:55 2013 -- 
         (plrd (if (rm-dups s)
                   (remove-duplicates pl :test #'pitch=)
                   pl)))
    (unless (= (length pl) (length plrd))
      (when (warn-dups s)
        ;; (break)
        (warn "sc-set::verify-and-store: found and removed duplicate ~
               pitches in ~&~a"
              (pitch-list-to-symbols pl))))
    (setf (slot-value s 'data) plrd)
    (set-micro-tone s)))

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
          ;; MDE Sun May  8 14:20:47 2016 -- we left out warn-dups and rm-dups
          (slot-value sclist 'warn-dups) (warn-dups s)
          (slot-value sclist 'rm-dups) (rm-dups s)
          (slot-value sclist 'related-sets) (when (related-sets s) 
                                              (clone (related-sets s))))
    sclist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object :before ((s sc-set) stream)
  (format stream "~&SC-SET: auto-sort: ~a, rm-dups: ~a, warn-dups: ~a ~
                  used-notes: ~a, ~
                  ~%~%**** N.B. All pitches printed as symbols only, ~
                  internally they are all ~%pitch-objects.~%~%"
          (auto-sort s) (rm-dups s) (warn-dups s) (used-notes s))
  (format stream "~%    subsets: ")
  (when (subsets s)
    (print-ral-of-pitch-lists (subsets s) stream))
  (format stream "~%    related-sets: ")
  (when (related-sets s)
    (print-ral-of-pitch-lists (related-sets s) stream)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf subsets) :after (value (s sc-set))
  (when (and value (not (assoc-list-p value)))
    (setf (slot-value s 'subsets) 
          (make-ral (format nil "sc-set-~a-subsets" (id s))
                    (subsets s)))
    (make-ral-pitch-lists (subsets s) (auto-sort s))
    (check-subsets (subsets s) s))
  (subsets s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod get-subset (id (s sc-set) &optional (warn t))
  (get-data id (subsets s) warn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf related-sets) :after (value (s sc-set))
  (declare (ignore value))
  (when value
    (setf (slot-value s 'related-sets)
          (make-ral (format nil "sc-set-~a-related-sets" (id s))
                    (related-sets s)))
    (make-ral-pitch-lists (related-sets s) (auto-sort s)))
  (related-sets s))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf data) :after (value (s sc-set))
  (declare (ignore value))
  (check-subsets (subsets s) s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod reset ((s sc-set) &optional ignore1 ignore2)
  (declare (ignore ignore1 ignore2))
  (setf (used-notes s) (make-ral 'used-notes nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* sc-set/add-harmonics
;;; DESCRIPTION
;;; Adds pitches to the set which are harmonically related to the existing
;;; pitches.  The keywords are the same as for the get-harmonics function.  NB
;;; This will automatically sort all pitches from high to low (irrespective of
;;; the auto-sort slot).
;;; 
;;; ARGUMENTS
;;; - an sc-set object
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;;  see get-harmonics function
;;; 
;;; RETURN VALUE
;;; the same set object as the first argument but with new pitches added.
;;; 
;;; EXAMPLE
#|
;;; treat the existing pitches as fundamentals. Note that the existence of E6
;;; twice in the result is not a mistake: though E6 is the nearest pitch, the
;;; frequencies are different.
(let ((s (make-sc-set '(c4 e4) :id 'test)))
  (add-harmonics s :start-partial 3 :max-results 3))
=>
SC-SET: auto-sort: T, rm-dups: T, warn-dups: T used-notes: 
[...]
data: (C4 E4 G5 B5 C6 E6 E6 AF6)

;;; treat the existing pitches as partials and add the fundamentals and
;;; harmonics
(let ((s (make-sc-set '(c4 e4) :id 'test)))
  (add-harmonics s :start-freq-is-partial 3 :max-results 3))
SC-SET: auto-sort: T, rm-dups: T, warn-dups: T used-notes: 
[...]
data: (F2 A2 F3 A3 C4 E4)

|#
;;; SYNOPSIS
(defmethod add-harmonics ((s sc-set) &rest keywords)
;;; ****
  (when (= 1 (length keywords)) ; sequence of & rest causes nested list prob
    (setq keywords (first keywords)))
  (setf (data s)
        (append (data s) 
                (apply #'get-pitch-list-harmonics (cons (data s) keywords))))
  ;; MDE Sat Mar  5 11:59:23 2016 -- don't forget this
  (when (rm-dups s) (rm-duplicates s))
  s)

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

;;; ****m* sc-set/round-inflections
;;; DESCRIPTION
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
;;; keyword arguments:
;;; - :qtr-tones-also. T or NIL to indicate whether quarter-tones are also to
;;;   be rounded to the nearest chromatic pitch and returned. T = round and
;;;   return. Default = NIL.
;;; - :octave. NIL or an integer that is the octave designator to which all
;;;   resulting pitches are to be transposed (i.e. the "4" in "C4" etc.)
;;;   Default = NIL.
;;; - :remove-duplicates. T or NIL to indicate whether any duplicate pitches
;;;   within an octave that are created by use of the :octave keyword argument
;;;   are to be removed. T = remove duplicates. Default = NIL.
;;; - :as-symbols. T or NIL to indicate whether to return the results of the
;;;   method as a list of note-name symbols rather than a list of pitch
;;;   objects. T = return as note-name symbols. Default = NIL.
;;; - :package. The package in which the pitches are to be handled. 
;;;   Default = :sc.
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
;;; MDE Mon Jan 30 19:53:58 2017 -- in the current scale
;;; ****m* sc-set/round-to-nearest
;;; DATE
;;; January 30th 2017
;;; 
;;; DESCRIPTION
;;; Round the (potentially very microtonal) pitches in the set to the nearest
;;; pitch of the current or given scale. Note that this is a destructive
;;; operation.  
;;; 
;;; ARGUMENTS
;;; - the sc-set object
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword argument:
;;; :scale. The scale to use when rounding. (Common Music tuning object or
;;; symbol). If a symbol, then 'chromatic-scale, 'twelfth-tone, or 'quarter-tone
;;; only at present. Default is the current scale as set by (in-scale :...).
;;; 
;;; RETURN VALUE
;;; the modified sc-set object
;;; 
;;; SYNOPSIS
(defmethod round-to-nearest ((s sc-set) &key (scale cm::*scale*))
;;; ****
  (loop for p in (data s) do (round-to-nearest p :scale scale))
  (set-micro-tone s) ; stats
  s)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon Feb  6 13:11:48 GMT 2012: Extended robodoc entry
 
;;; SAR Mon Feb  6 11:58:00 GMT 2012: Added robodoc NB

;;; SAR Sat Feb  4 18:23:23 GMT 2012: Deleted MDE comment, as this is taken
;;; nearly verbatim into the doc below

;;; SAR Sat Feb  4 18:14:25 GMT 2012: Added robodoc entry

;;; ****m* sc-set/force-micro-tone
;;; DESCRIPTION
;;; Change the value of the MICRO-TONE slot of all pitch objects in a given
;;; sc-set object to the specified <value>.
;;; 
;;; NB If the pitches are microtonal and thus have associated pitch-bends and
;;; microtonal frequencies, these will not be changed, i.e. only the micro-tone
;;; slot is changed.
;;;
;;; NB: Although the MICRO-TONE slot is generally used as a boolean, this
;;;     method allows the user to force-set it to any value. 
;;; 
;;; ARGUMENTS
;;; - An sc-set object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - An item of any type that is to be the new value of the MICRO-TONE slot of
;;;   all pitch objects in the given sc-set object (generally T or
;;;   NIL). Default = NIL. 
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
;;; DESCRIPTION
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
;;; keyword arguments:
;;; - :octave. NIL or an integer that is the octave designator to which all
;;;   resulting pitches are to be transposed (i.e. the "4" in "C4" etc.)
;;;   Default = NIL.
;;; - :remove-duplicates. T or NIL to indicate whether any duplicate pitches
;;;   within an octave that are created by use of the :octave keyword argument
;;;   are to be removed. T = remove duplicates. Default = NIL.
;;; - :as-symbols. T or NIL to indicate whether to return the results of the
;;;   method as a list of note-name symbols rather than a list of pitch
;;;   objects. T = return as note-name symbols. Default = NIL.
;;; - :package. The package in which the pitches are to be handled. 
;;;   Default = :sc.
;;; - :invert. Get the micro-tone pitches instead.
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
;;; DESCRIPTION
;;; Return those notes of a given sc-set object that are microtones (i.e. no
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
;;; keyword arguments:
;;; - :octave. NIL or an integer that is the octave designator to which all
;;;   resulting pitches are to be transposed (i.e. the "4" in "C4" etc.)
;;;   Default = NIL.
;;;  :as-symbols. T or NIL to indicate whether to return the results of the
;;;   method as a list of note-name symbols rather than a list of pitch
;;;   objects. T = return as note-name symbols. Default = NIL.
;;; - :package. The package in which the pitches are to be handled. 
;;;   Default = :sc.
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
;;; ****m* sc-set/get-semitones
;;; DESCRIPTION
;;; Get the distances in semitones of each pitch in a given sc-set
;;; object to a static reference pitch. 
;;;
;;; Though this method can be used in other contexts, it was devised
;;; as an aid for transposing audio samples (sound files), and the
;;; reference pitch is therefore generally the perceived fundamental
;;; pitch of the audio sample to be transposed.
;;; 
;;; ARGUMENTS
;;; - An sc-set object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - An optional note-name symbol sets the value of the <reference-pitch>,
;;;   which is the basis pitch to which the resulting number of semitones
;;;   refer. This will generally be the perceived fundamental pitch of the
;;;   sample (sound file) being modified ("transposed").
;;; - The optional <offset> argument takes a number that is the number of
;;;   semitones to add to the pitch of the given set prior to determining its
;;;   distance in semitones from the reference pitch.
;;; 
;;; RETURN VALUE
;;; A list of positive and negative numbers.
;;; 
;;; EXAMPLE
#|
;; Chromatic example
(let ((mscs (make-sc-set '(d2 fs3 cs4 c5 af5 d6))))
  (get-semitones mscs))

=> (-22.0 -6.0 1.0 12.0 20.0 26.0)

;; Quarter-tone example; results can be decimal fractions of semitone
(let ((mscs (make-sc-set '(d2 cqs3 fs3 gqf3 cs4 fqs4 c5 af5 bqf5 cqs6 d6))))
  (get-semitones mscs))

=> (-22.0 -11.5 -6.0 -5.5 1.0 5.5 12.0 20.0 22.5 24.5 26.0)

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
;;; DESCRIPTION
;;; Get the sampling-rate conversion factors for the given sc-set object,
;;; whereby 1.0 = unison, 2.0 = one octave higher and 0.5 = one octave lower
;;; etc.
;;;
;;; ARGUMENTS
;;; - An sc-set object.
;;; 
;;; OPTIONAL ARGUMENTS 
;;; - An optional note-name symbol sets the value of the <reference-pitch>,
;;;   which is the basis pitch to which the resulting factors refer. This will
;;;   generally be the perceived fundamental pitch of the sample (sound file)
;;;   being modified ("transposed").
;;; - The optional <offset> argument takes a number that is the number of
;;;   semitones to add to the pitch of the given set prior to determining the
;;;   sampling-rate conversion factors. 
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
;; with a positive offset will return higher values

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

;;; SAR Mon Feb  6 15:29:04 GMT 2012: Added robodoc entry

;;; ****m* sc-set/subset-get-srts
;;; DESCRIPTION
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
;;;   the sample (sound file) being modified ("transposed").
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
;;; ****m* sc-set/stack
;;; DESCRIPTION
;;; Extend the pitch content of a given sc-set object by adding new pitch
;;; objects which have the same interval structure as the original set. 
;;;
;;; The method analyzes the interval structure of the original set from the
;;; bottom note to the top and adds new sets to the top and bottom of the
;;; original set symmetrically; i.e., with the identical interval structure
;;; above the original set and inverted interval structure below.
;;;
;;; The second argument indicates how many times this procedure is carried
;;; out.
;;;
;;; NB: The method assumes that the pitch content of the original sc-set object
;;;     is sorted from low to high. 
;;;
;;; See also: the make-stack method in the complete-set class to make a stack
;;;           from a simple list of note-name symbols.
;;; 
;;; ARGUMENTS
;;; - An sc-set object.
;;; - An integer that is the number of new sets to be added to each end of the
;;;   original set.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :id. A symbol that will be the ID of the new sc-set object. Default NIL.
;;; - :by-freq. If T then use frequencies when calculating the interval
;;;    structure instead of degrees (semitones if default scale is chromatic).
;;;    In this case the frequencies of pitches will be retained but their
;;;    symbolic value will be rounded to the nearest note in the current scale
;;;    (and pitch bends will be set accordingly). Default NIL. 
;;; - :up. Apply the process upwards in pitch space. Default = T.
;;; - :down. Apply the process downwards in pitch space. Default = T.
;;; - :respell. Respell the chord after the stacking process to (hopefully get
;;;   better spellings. Default = T
;;; 
;;; RETURN VALUE
;;; A new sc-set object. This will have the same tag and id (unless given as a
;;; keyword argument here) slots as the original sc-set but not subsets,
;;; related-sets, used-notes, etc.
;;; 
;;; EXAMPLE
#|
;; Extends the original set with new sets that have the identical interval
;; structure upwards and inverted interval structure downwards. 
(let ((set (make-sc-set '(c4 e4 g4))))
  (stack set 3))
=>
SC-SET: auto-sort: T, used-notes: 
[...]
data: (EF2 GF2 BF2 DF3 F3 AF3 C4 E4 G4 B4 D5 GF5 A5 DF6 E6)

;;; or by calling the make-stack function, which returns a complete-set object
;;; (subclass of tl-set and sc-set).  Called with (in-scale :chromatic):
(make-stack 'test '(430 441 889 270) 1 :by-freq t)
=>
COMPLETE-SET: complete: NIL
[...]
data: (G2 A2 CS4 A4 A4 A5 C6 C6 FS6)

Note the difference of doing :by-freq and by interval:

(let ((set (make-sc-set '(c4 e4 g4))))
        (print (get-pitch-symbols (stack set 1 :by-freq t)))
        (get-pitch-symbols (stack set 1 :by-freq nil)))
-->
(C3 G3 C4 E4 G4 BF4 C5) 
(F3 AF3 C4 E4 G4 B4 D5)

|#
;;; SYNOPSIS
(defmethod stack ((s sc-set) num-stacks &key id by-freq (up t) (down t)
                                             (respell t))
;;; ****
  (let* ((distances (get-interval-structure s (when by-freq 'frequencies)))
         (notes (if by-freq (get-freqs s) (get-degrees s)))
         (result notes)
         chord)
    (loop repeat num-stacks do
             (setq result (stack-aux result distances by-freq up down)))
    (unless by-freq
      (setq result (degrees-to-notes result))
      ;; MDE Sat Jan 14 10:25:25 2012 -- try and get better spellings
      (setf chord (make-chord result :midi-channel 1
                                     :microtones-midi-channel 2))
      ;; if by-freq we want to retain the original freqs, whereas respelling
      ;; would replace these with the freqs of the tempered notes 
      (when respell (respell-chord chord)))
    ;; return a new set, using the given id or if not given, the same id as the
    ;; original set 
    (make-sc-set (if by-freq result (data chord)) :tag (tag s)
                 ;; MDE Tue Aug  4 12:46:23 2020, Heidhausen -- more slots!
                 :auto-sort (auto-sort s) :warn-dups (warn-dups s)
                 :rm-dups (rm-dups s)
                 :subsets (subsets s) :related-sets (related-sets s)
                 :id (if id id (id s)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon Feb  6 16:07:24 GMT 2012: Added robodoc entry

;;; ****m* sc-set/set-position
;;; DESCRIPTION
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
;;; DESCRIPTION
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
;;; DESCRIPTION
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
;;; DESCRIPTION
;;; Return the pitches of a given sc-set object as a list of their equivalent
;;; MIDI note numbers.
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
;;; DESCRIPTION
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
;;; DESCRIPTION
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
;;; ****m* sc-set/has-pitches-in-range
;;; DATE
;;; June 28th 2017, Edinburgh
;;; 
;;; DESCRIPTION
;;; Tests whether a set has pitches within a certain range.
;;; 
;;; ARGUMENTS
;;; - the sc-set object
;;; - the lower pitch of the range (pitch object or symbol)
;;; - the upper pitch of the range (pitch object or symbol)
;;;
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate that only chromatic pitches count, i.e. not
;;;   microtones  
;;; 
;;; RETURN VALUE
;;; T or NIL
;;; 
;;; SYNOPSIS
(defmethod has-pitches-in-range ((s sc-set) lower upper &optional chromatic)
;;; ****  
  (setq lower (make-pitch lower)
        upper (make-pitch upper))
  (loop for p in (data s) do
       (when (and (or (not chromatic) (not (micro-tone p)))
                  (pitch-in-range p lower upper))
         (return t))
       finally (return nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon Feb  6 17:32:38 GMT 2012: Added robodoc entry

;;; ****m* sc-set/create-chord
;;; DESCRIPTION
;;; Create a chord object from the pitches of the given sc-set object. As of
;;; 1.0.6 sc-set is a subset of chord instead of sc-list so this shouldn't be
;;; necessary, but it's still here for legacy code purposes.
;;; 
;;; ARGUMENTS
;;; - An sc-set object.
;;;
;;; OPTIONAL ARGUMENTS
;;; - either a subsets-id or related-sets-id in order to use the respective
;;;   notes instead of all the notes of the set.
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
(defmethod create-chord ((s sc-set) &optional subsets-id related-sets-id)
;;; ****
  (make-chord (cond
                (subsets-id (get-data-data subsets-id (subsets s)))
                (related-sets-id (get-data-data related-sets-id
                                                (related-sets s)))
                (t (data s)))))
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* sc-set/create-event
;;; DESCRIPTION
;;; Create an event object (that is a chord) from a given sc-set object,
;;; specifying a rhythmic value and a start-time (in seconds).
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
;;; - subsets-id, related-sets-id: pass an ID to use just the notes of the
;;;   sub/related set, rather than all the notes of the set.
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
(defmethod create-event ((s sc-set) rhythm start-time
                         &optional start-time-qtrs subsets-id related-sets-id)
;;; ****
  (unless start-time-qtrs
    (setf start-time-qtrs start-time))
  (let ((e (make-event (create-chord s subsets-id related-sets-id)
                       rhythm :start-time start-time)))
    (setf (start-time-qtrs e) start-time-qtrs)
    e))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon Feb  6 18:02:05 GMT 2012: Added robodoc entry

;;; ****m* sc-set/pitch-symbols
;;; DESCRIPTION
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
#|
;;; MDE Sat Oct 26 13:08:28 2013 
;;; MDE Fri Aug 24 15:25:53 2018 -- use chord class method instead
(defmethod print-simple ((s sc-set) &optional ignore (stream t))
  (declare (ignore ignore))
  (print-simple-pitch-list (data s) stream))
|# 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* sc-set/average-pitch
;;; DATE
;;; July 24th 2015, Glenferness
;;;
;;; DESCRIPTION
;;; Calculate the average frequency of all the pitches in the chord and return
;;; this as a new pitch-object. 
;;; 
;;; ARGUMENTS
;;; - the sc-set object
;;; 
;;; RETURN VALUE
;;; a pitch-object
;;; 
;;; EXAMPLE
#|
(average-pitch (make-complete-set '(c4 cs4  d4 e4 ef4 g4 b4)))
=>
PITCH: frequency: 337.015, midi-note: 64, midi-channel: 0 
       pitch-bend: 0.38 
       degree: 64, data-consistent: T, white-note: E4
       nearest-chromatic: E4
       src: 1.2881585030063512d0, src-ref-pitch: C4, score-note: E4 
       qtr-sharp: NIL, qtr-flat: NIL, qtr-tone: NIL,  
       micro-tone: T, 
       sharp: NIL, flat: NIL, natural: T, 
       octave: 4, c5ths: 0, no-8ve: E, no-8ve-no-acc: E
       show-accidental: T, white-degree: 37, 
       accidental: N, 
       accidental-in-parentheses: NIL, marks: NIL, 
       marks-before: NIL
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: E4, tag: NIL, 
data: E4
|#
;;; SYNOPSIS
(defmethod average-pitch ((s sc-set))
;;; ****
  (make-pitch (/ (loop for p in (data s) sum (frequency p)) (sclist-length s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Tue Aug 18 17:12:18 2015 -- 
;;; ****m* sc-set/least-used-octave
;;; DATE
;;; 18th August 2015, Edinburgh
;;; 
;;; DESCRIPTION
;;; Return the octave with the least notes in it. This includes octaves between
;;; the lowest and highest notes inclusive, and may include an octave within
;;; those ranges with no notes at all. 
;;; 
;;; ARGUMENTS
;;; - the sc-set object
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :highest-wins. If two octaves share the least number of notes, return the
;;;    highest when T. Default = T.
;;; - :avoiding. An integer or list of integers. Don't return any octave in
;;;    this list or don't return this single integer argument. Default = NIL.
;;; 
;;; RETURN VALUE
;;; An integer representing the octave with the least notes.
;;; 
;;; EXAMPLE
#|

(least-used-octave (make-sc-set '(cs2 d4 e4 g5)))
--> 3
(least-used-octave (make-sc-set '(cs2 d3 ds4 e4 g5)) :highest-wins nil)
--> 2
(least-used-octave (make-sc-set '(cs2 d3 ds4 e4 g5)))
--> 5

|#
;;; SYNOPSIS
(defmethod least-used-octave ((s sc-set) &key (highest-wins t) avoiding)
;;; ****
  (least-used-octave-aux s (if highest-wins #'<= #'<) avoiding))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* sc-set/most-used-octave
;;; DATE
;;; 18th August 2015, Edinburgh
;;; 
;;; DESCRIPTION
;;; Return the octave with the least notes in it. See least-used-octave for
;;; more details and argument descriptions.
;;; 
;;; SYNOPSIS
(defmethod most-used-octave ((s sc-set) &key (highest-wins t) avoiding)
;;; ****
  (least-used-octave-aux s (if highest-wins #'>= #'>) avoiding))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod least-used-octave-aux ((s sc-set) test avoiding)
  ;; (print (get-pitch-symbols s))
  (unless (listp avoiding) (setq avoiding (list avoiding)))
  ;;     this will hold the count of pitches in each octave starting with -1
  (let ((8vecs (ml 0 20))
        (low8 (octave (lowest s)))
        (hi8 (octave (highest s)))
        result)
    ;; octaves can be as low -1 so 1+ them here
    (loop for p in (data s) do (incf (nth (1+ (octave p)) 8vecs)))
    ;; only include those octaves within the highest and lowest notes (so no
    ;; extremes unless the chord has notes in those extremes)
    (loop for 8ve from -1 for 8vec in 8vecs with num do
         (when (and (or (not num) (funcall test 8vec num))
                    (not (member 8ve avoiding))
                    (>= 8ve low8)
                    (<= 8ve hi8))
           (setq num 8vec
                 result 8ve)))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Wed Jan 13 10:16:28 2016 
(defmethod delete-subsets ((s sc-set) &optional related)
  (setf (subsets s) nil)
  (when related
    (setf (related-sets s) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  MDE Sat Mar  5 14:02:19 2016 
(defmethod wrap :before ((s sc-set) &optional (num-times 1) (transpose t))
  (declare (ignore num-times transpose))
  (when (subsets s)
    (error "sc-set::wrap :before : ~a: can't wrap sets with subsets. ~
            ~%Consider calling the delete-subsets method before wrap."
           (id s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod morph :around ((s1 sc-set) (s2 sc-set) amount)
  (clone-with-new-class (call-next-method) 'sc-set))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* sc-set/subsets-remove
;;; DATE
;;; June 2nd 2017, Edinburgh
;;; 
;;; DESCRIPTION
;;; Remove pitches from all subsets of an sc-set object
;;; 
;;; ARGUMENTS
;;; - the sc-set object
;;; - &rest the pitches to remove, as pitch objects or symbols
;;; 
;;; RETURN VALUE
;;; the sc-set
;;; 
;;; SYNOPSIS
(defmethod subsets-remove ((s sc-set) &rest pitches)
;;; ****
  (when (subsets s)
    (when (listp (first pitches))       ; pitches was already a list
      (setq pitches (first pitches)))
    (setq pitches (loop for p in pitches collect (make-pitch p)))
    ;; MDE Thu Nov  1 11:22:49 2018 -- use nmap-data instead now
    (nmap-data (subsets s)
               #'(lambda (plist)
                   (remove-if
                    #'(lambda (p)
                        (member p pitches :test #'pitch=))
                    plist)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* sc-set/thin
;;; DATE
;;; June 1st 2017, Edinburgh
;;; 
;;; DESCRIPTION
;;; Remove pitches from a set using a deterministic algorithm. From bottom to
;;; top (or vice-versa) pitches will be selected or rejected using the
;;; activity-levels class, to which the strength (1-10) is passed. The pitches
;;; removed are also removed from any subsets.
;;;
;;; By default the set will be reduced by 1/3 of its pitches but it is expected
;;; that either the :remove or :target keyword will be used (see below).
;;; 
;;; ARGUMENTS
;;; - an sc-set object
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :strength. An integer from 1-10 indicating the likelihood of removing
;;;   individual pitches. 1 would be 10%, 10, 100%. Default = 5.
;;; - :remove. An integer indicating how many pitches to remove. Default = NIL
;;; - :target. An integer indicating how many pitches the set should be left
;;;   with. Default = NIL
;;; - :invert. T or NIL to remove pitches starting at the top or bottom. T =
;;;   from top, NIL = from bottom. Default = NIL 
;;; 
;;; RETURN VALUE
;;; The (thinned) sc-set object.
;;; 
;;; EXAMPLE
#|

|#
;;; SYNOPSIS
(defmethod thin ((s sc-set) &key (strength 5) remove target invert)
;;; ****
  ;; (print strength) (print remove) (print target)
  (multiple-value-bind 
        (rmd rm)
      (thin-aux (data s) strength remove target invert)
    (setf (slot-value s 'data) rmd)
    (subsets-remove s rm)
    (verify-and-store s)
    s))

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
;;; MDE Wed Aug 28 17:52:52 2013 -- notes are either degrees (default) or freqs
(defun stack-aux (notes distances &optional freqs up down)
  (let ((lowest (first notes)) ;; assumes notes are sorted!
        (highest (first (last notes)))
        (max (if freqs (note-to-freq 'b10) (note-to-degree 'b10)))
        (result (copy-list notes)))
    (loop for d in distances 
       for low = (- lowest d)
       for high = (+ highest d)
       do 
       (when (and up (<= high max))
         (push high result))
       (when (and down (> low 0 ))
         (push low result)))
    (sort result #'<)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* sc-set/make-sc-set 
;;; DESCRIPTION
;;; Create an sc-set object, which holds pitch-set information for harmonic and
;;; pitch manipulation.
;;; 
;;; ARGUMENTS
;;; - A list of note-name symbols or frequencies that is to be the set
;;;   (pitch-set) for the given sc-set object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :id. A symbol that is to be the ID of the given sc-set object.
;;; - :subsets. An assoc-list of key/data pairs, in which the data is a list of
;;;   note-name symbols that are a subset of the main set. One use for this
;;;   keyword argument might be to create subsets that particular instruments
;;;   can play; these could for instance be selected in the chord-function
;;;   passed to the instrument object.  In any case, if the instrument has a
;;;   subset-id slot, and the current set contains a subset with that ID, the
;;;   pitches the instrument may play are limited to that subset.
;;; - :related-sets. An assoc-list of key/data pairs, similar to :subsets, only
;;;   that the pitches given here do not have to be part of the main set. This
;;;   can be used, for example, for pitches missing from the main set.
;;; - :auto-sort. T or NIL to indicate whether the specified pitches (note-name
;;;   symbols) are to be automatically sorted from lowest to highest. 
;;;   T = sort. Default = T.
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
(defun make-sc-set (sc-set &key id subsets related-sets (auto-sort t) tag
                             (warn-dups t) (rm-dups t))
;;; ****
  (make-instance 'sc-set :id id :data sc-set :subsets subsets :rm-dups rm-dups
                 :warn-dups warn-dups
                 :tag tag :related-sets related-sets :auto-sort auto-sort))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-ral-of-pitch-lists (ral stream)
  (unless (is-ral ral)
    (error "sc-set::print-ral-of-pitch-lists: first argument must be a ~%~
            recursive-association-list:~%~a" ral))
  (let ((all-refs (get-all-refs ral)))
    (loop 
        for ref in all-refs
        for no = (get-data ref ral)
        do (format stream "~&~a: ~a"
                   (id no)
                   (get-ids-from-pitch-list (data no))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Fri Aug 24 14:23:49 2018 -- added midi-channel and microtone-midi-
(defun init-pitch-list (pitches &optional auto-sort midi-channel
                                  microtone-midi-channel)
  (let ((result (loop with p for pitch in pitches do
                     (unless pitch
                       (error "~a~&sc-set::init-pitch-list: pitch is nil!"
                              pitches))
                     (setq p (make-pitch pitch))
                   ;; MDE Fri Aug 24 14:24:12 2018
                     (if (micro-tone p)
                         (if microtone-midi-channel
                             (setf (midi-channel p) microtone-midi-channel)
                             (when midi-channel
                               (setf (midi-channel p) midi-channel)))
                         (when midi-channel
                           (setf (midi-channel p) midi-channel)))
                   collect p)))
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
  ;; (print subsets)
  ;; (print sc-set)
  (when (and subsets (is-ral subsets))
    (loop 
       for ss in (data subsets) 
       for pitches = (data ss)
       for i from 0 do
         ;; (print ss)
         (if (is-ral pitches)
             (check-subsets pitches sc-set)
             (loop for pitch in pitches do
                  (unless (pitch-member pitch (data sc-set))
                    (error "sc-set::check-subsets: Note ~a given in subset ~a~
                            ~%of set ~a is not part of the main set ~a"
                           (id pitch) (id ss) (id sc-set)
                           (pitch-list-to-symbols (data sc-set)))))))))
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sc-set-p (thing)
  (typep thing 'sc-set))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* sc-set/subset-from-harp-salzedo
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-11-28, Essen
;;; 
;;; DESCRIPTION
;;; This method creates a subset from a given sc-set according to a specific
;;; harp-pedaling indicated by a salzedo-list (cf. harp-salzedo-to-tl-set) and
;;; returns either a new sc-set or a list of symbols (when :as-symbols = T)
;;; in order to be further used as either an independent/new sc-set or a
;;; pitch-list to be used in the subset-slot of e.g. a sc-set.
;;;
;;; ARGUMENTS
;;; - A sc-set object which is the actual set to derive the subset from.
;;; - A list, which is a salzedo list with the harp pedalling
;;;   (cf. salzedo-to-set).
;;; 
;;; OPTIONAL ARGUMENTS
;;; - :id. The id of the subset to be generated. When :as-symbols = T, this
;;;   argument will be ignored. 
;;; - :lowest. The lowest pitch to be included in the subset. Default: 'b0
;;; - :highest. The highest pitch to be included in the subset. Default: 'gs7
;;; - :as-symbols. A boolean indicating whether a new sc-list object
;;;   should (NIL) or a list of pitch symbols (NIL) should be returned.
;;;   Default = NIL.
;;; - :subsets. Inherited from make-sc-set. This will be ignored when
;;;   :as-symbols = T.
;;; - :related-sets. Inherited from make-sc-set. This will be ignored when
;;;   :as-symbols = T.
;;; - :auto-sort. Inherited from make-sc-set. This will be ignored when
;;;   :as-symbols = T. Default = T.
;;; 
;;; RETURN VALUE
;;; Either a sc-set or a list of pitch symbols.
;;;
;;; EXAMPLE
#|
(subset-from-harp-salzedo (make-sc-set '(C1 D1 E1 F1 GS1 AF1 AF2 BF2 C3 D3
                                         E3 F3 GS3
                                         AF3 BF3 C4 D4 E4 F4 GS4 AF4 BF4 C5
                                         D5 E5 F5
                                         C6 D6 E6 F6 GS6 AF6 BF6 C7 D7 E7))
                          '(0 1 -1 0 1 0 -1)
                          :as-symbols t)

=>
(D1 E1 AF1 AF2 BF2 D3 E3 AF3 BF3 D4 E4 AF4 BF4 D5 E5 D6 E6 AF6 BF6 D7 E7)
|#
;;; SYNOPSIS
(defmethod subset-from-harp-salzedo ((s sc-set) salzedo
                                     &key
                                       id
                                       (lowest 'b0)
                                       (highest 'gs7)
                                       as-symbols
                                       subsets
                                       related-sets
                                       (auto-sort t))
;;; ****
  (let* ((salzedo-set (harp-salzedo-to-tl-set salzedo
                                              :highest highest
                                              :lowest lowest))
         (salzedo-set-pitches (data salzedo-set))
         (set-pitches (data s))
         (subset (pitch-intersection set-pitches salzedo-set-pitches)))
    (if as-symbols
        (mapcar #'data subset)
        (make-sc-set subset
                     :id id))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF sc-set.lsp

