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
;;; $$ Last modified: 21:15:36 Thu Dec  8 2011 ICT
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
   ;; main set (data slot).
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

;;; Get the microtones rounded to the nearest chromatic note.  As a
;;; default only get those microtones less than a quarter tone.  Move
;;; all pitches to <octave> if not nil.

(defmethod round-inflections ((s sc-set) 
                              &key
                              qtr-tones-also
                              octave
                              (remove-duplicates t) ;; only if octave!
                              (as-symbols nil)
                              (package :sc))
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
(defmethod force-micro-tone ((s sc-set) &optional value)
  (loop for p in (data s) do
       (setf (micro-tone p) value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 
;;; Get the notes from the set that are normal chromatic notes,
;;; i.e. no microtones.  If octave is given, put them all into that octave

(defmethod get-chromatic ((s sc-set) 
                          &key 
                          (octave nil)
                          (remove-duplicates t) ;; only if octave!
                          (as-symbols nil)
                          (package :sc)
                          (invert nil)) ;; this will get just the microtones
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

(defmethod get-non-chromatic ((s sc-set) 
                              &key 
                              (octave nil)
                              (as-symbols nil)
                              (package :sc))
  (get-chromatic s :octave octave :as-symbols as-symbols :package package 
                 :invert t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Get the semitone transpositions for the main set.
;;; offset is number of semitones to add.
;;; reference-pitch is the apparent pitch of the sample we're going to
;;; transpose.  

(defmethod get-semitones ((s sc-set) &optional 
                                (reference-pitch 'c4)
                                (offset 0))
  (loop for srt in 
        (get-srts-aux (data s) reference-pitch offset)
      collect (srt srt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Get the sampling-rate conversion factors for the main set.
;;; offset is number of semitones to add.
;;; reference-pitch is the apparent pitch of the sample we're going to
;;; transpose.  

(defmethod get-srts ((s sc-set) &optional 
                                (reference-pitch 'c4)
                                (offset 0))
  (get-srts-aux (data s) reference-pitch offset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod subset-get-srts  ((s sc-set) subset &optional 
                                               (reference-pitch 'c4)
                                               (offset 0))
  (get-srts-aux (data (get-data subset (subsets s)))
                reference-pitch offset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;  Get the interval structure of the set from bottom to top note and add
;;; notes to the top and bottom using the same structure.  Repeat num-stacks
;;; times.  NB we assume the set is sorted.  See also make-stack in the
;;; complete-set class to make a stack from a simple list of pitch symbols.

(defmethod stack ((s sc-set) &optional (num-stacks 1) id)
  (let* ((distances (get-interval-structure s))
         (degrees (get-degrees s))
         (result degrees))
    (loop repeat num-stacks do
          (setf result (stack-aux result distances)))
    ;; return a new set, using the given id or if not given, the same id as the
    ;; original set 
    (make-sc-set (degrees-to-notes result) :id (if id id (id s)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Get the degree distances of each note to the bottom note of the set
;;; (assumes set is sorted).

(defmethod get-interval-structure ((s sc-set))
  (let ((lowest-degree (degree (first (data s)))))
    (loop for i in (rest (data s)) collect
          (- (degree i) lowest-degree))))
     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod set-position ((p pitch) (s sc-set))
  (position p (data s) :test #'pitch=))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-degrees ((s sc-set))
  (loop for p in (data s) collect (degree p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-freqs  ((s sc-set))
  (loop for p in (data s) collect (frequency p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-midi  ((s sc-set))
  (loop for p in (data s) collect (midi-note p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-semitones-from-middle-note ((s sc-set) &optional subset)
  (let* ((notes (if subset (get-data-data subset (subsets s))
                  (data s)))
         (middle (floor (length notes) 2))
         (middle-midi (midi-note-float (nth middle notes))))
    (loop for p in notes collect (- (midi-note-float p) middle-midi))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A new set is made out of the data of the two arguments.  This
;;; means of course that subsets etc. get lost.

(defmethod add ((s1 sc-set) (s2 sc-set) &optional ignore)
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

(defmethod contains-pitches ((s sc-set) pitches)
  (all-members (data s) (init-pitch-list pitches nil) #'pitch=))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod create-chord ((s sc-set))
  (declare (ignore ignore1) 
           (ignore ignore2))
  (make-chord (data s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod create-event ((s sc-set) rhythm start-time &optional start-time-qtrs)
  (unless start-time-qtrs
    (setf start-time-qtrs start-time))
  (let ((e (make-event (create-chord s) rhythm :start-time start-time)))
    (setf (start-time-qtrs e) start-time-qtrs)
    e))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod pitch-symbols ((s sc-set))
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

(defun make-sc-set (sc-set &key id subsets related-sets (auto-sort t))
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

