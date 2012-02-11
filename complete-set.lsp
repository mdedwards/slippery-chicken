;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* tl-set/complete-set
;;; NAME 
;;; complete-set
;;;
;;; File:             complete-set.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> sclist -> sc-set ->
;;;                   tl-set -> complete-set
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Implementation of the complete-set class which as an
;;;                   extension of the tl-set class allows checking for full
;;;                   sets: ones in which every note of *standard-scale* is
;;;                   present. 
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    10th August 2001
;;;
;;; $$ Last modified: 13:08:08 Sat Feb 11 2012 GMT
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

(defclass complete-set (tl-set)
  ;; this can be given at init (t or 'chromatic) and if the set is not complete
  ;; in either of those senses, a warning is issued.  If this is neither t nor
  ;; 'chromatic at init, then no warning will be issued.  In both cases the
  ;; slot will be set after checking the set.  Apart from t and nil it can also
  ;; be 'chromatic, which means all chromatic equally-tempered notes are
  ;; present in the set.
  ((complete :accessor complete :initarg :complete :initform nil)
   (missing-non-chromatic :accessor missing-non-chromatic :type list
                          :initform '()) 
   (missing-chromatic :accessor missing-chromatic :type list :initform '())
   (num-missing-non-chromatic :accessor num-missing-non-chromatic :type integer
                              :initform 0)
   (num-missing-chromatic :accessor num-missing-chromatic :type integer 
                          :initform 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((cs complete-set) &rest initargs)
  (declare (ignore initargs))
  (unless (member (complete cs) '(t nil chromatic))
    (error "complete-set::initialize-instance: ~
            The complete slot of complete-set may only be T NIL or ~
            'CHROMATIC"))
  (check-complete cs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone ((cs complete-set))
  (clone-with-new-class cs 'complete-set))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone-with-new-class :around ((cs complete-set) new-class)
  (declare (ignore new-class))
  (let ((tl-set (call-next-method)))
    (setf (slot-value tl-set 'complete) (complete cs)
          (slot-value tl-set 'missing-non-chromatic) 
          (copy-list (missing-non-chromatic cs))
          (slot-value tl-set 'missing-chromatic)
          (copy-list (missing-chromatic cs))
          (slot-value tl-set 'num-missing-non-chromatic)
          (num-missing-non-chromatic cs)
          (slot-value tl-set 'num-missing-chromatic)
          (num-missing-chromatic cs))
    tl-set))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object :before ((cs complete-set) stream)
  (format stream "~&COMPLETE-SET: complete: ~a~
                  ~&              num-missing-non-chromatic: ~a~
                  ~&              num-missing-chromatic: ~a~
                  ~&              missing-non-chromatic: ~a~
                  ~&              missing-chromatic: ~a"
          (complete cs) (num-missing-non-chromatic cs)
          (num-missing-chromatic cs) (missing-non-chromatic cs)
          (missing-chromatic cs)))
          

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod check-complete ((cs complete-set) &optional (do-error t))
  (let* ((degs-per-octave (degrees-per-octave))
         (degs (loop for pitch in (data cs)
                   collect (mod (degree pitch) degs-per-octave)))
         (missing-non-chromatic '())
         (missing-chromatic '())
         (step (/ degs-per-octave 12))
         ;; let's see if every note in the standard scale (1/4 tone probably)
         ;; is there...
         (complete (loop for i below degs-per-octave 
                       with ret = t
                       unless (member i degs)
                       do (unless (zerop (mod i step))
                            (push i missing-non-chromatic))
                          (setq ret nil)
                       finally (return ret)))
         (given-complete (complete cs)))
    ;; if not every note is there, then perhaps all the normal chromatic ones?
    (unless complete
      ;; the chromatic tones are 0 2 4 6 8 ... for 1/4 tone scales etc.
      (setf complete
        (when (loop for i below degs-per-octave by step
                  with ret = t
                  unless (member i degs) 
                  do (push i missing-chromatic)
                     (setq ret nil)
                  finally (return ret))
          'chromatic)))
    ;; complete is now t, nil or 'chromatic
    (when (and do-error given-complete 
               (not (or (eq complete given-complete)
                        (eq complete t))))
      (error "complete-set::check-complete: ~
              The complete slot of complete-set ~a was said to be ~a but ~
              the pitch set is not complete.  The complete slot was ~
              determined to be ~a."
             (id cs) given-complete complete))
    (setf (complete cs) complete
          (missing-non-chromatic cs) (notes-from-degrees-no-octave 
                                      missing-non-chromatic)
          (missing-chromatic cs) (notes-from-degrees-no-octave 
                                  missing-chromatic)
          (num-missing-non-chromatic cs) (length missing-non-chromatic)
          (num-missing-chromatic cs) (length missing-chromatic))))
         
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod stack ((cs complete-set) &optional (num-stacks 1) id)
  (declare (ignore num-stacks id))
  (let ((set (call-next-method)))
    (setf set (clone-with-new-class set 'complete-set))
    (check-complete set)
    set))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod missing-non-chromatic-as-pitches ((cs complete-set))
  (missing-to-pitches (missing-non-chromatic cs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod missing-chromatic-as-pitches ((cs complete-set))
  (missing-to-pitches (missing-chromatic cs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+cmn
(defmethod cmn-treble-bass-system ((cs complete-set) 
                                   &optional 
                                   4stave
                                   (text "")
                                   (text-x-offset -0.5)
                                   (text-y-offset 2.0)
                                   (line-break t) 
                                   (font-size 10.0)
                                   include-missing-chromatic
                                   include-missing-non-chromatic
                                   (use-octave-signs t))
  (let* ((main (get-cmn-treble-bass (data cs) nil 4stave))
         (mc (when include-missing-chromatic
               (get-cmn-treble-bass (missing-chromatic-as-pitches cs)
                                    t 4stave)))
         (mnc (when include-missing-non-chromatic
                (get-cmn-treble-bass (missing-non-chromatic-as-pitches cs)
                                     nil 4stave)))
         (treble-clef '())
         (bass-clef '())
         ;; the 15ma clefs
         (quad-treble-clef '())
         (quad-bass-clef '())
         (text-count 0))
    ;; (print main)
    ;; we put the id and tag (here the text arg) a little higher than other
    ;; things so it doesn't overlap subset names etc. 
    (labels ((push-aux (note-list text where &optional (offset-offset 0.0))
               (let ((cmn-ch (cmn::cmn-stemless-chord
                              note-list 
                              :chord-text text
                              :font-size font-size
                              :text-x-offset text-x-offset
                              :text-y-offset 
                              (+ offset-offset text-y-offset)
                              :use-octave-signs use-octave-signs)))
                 ;; (print (cmn::descry cmn-ch))
                 ;; remember push only works when the variable is a list,
                 ;; rather than a variable referring to another variable which
                 ;; is a list. 
                 (case where
                   (1 (push cmn-ch treble-clef))
                   (2 (push cmn-ch bass-clef))
                   (3 (push cmn-ch quad-treble-clef))
                   (4 (push cmn-ch quad-bass-clef)))))
             (push-treble (note-list &optional (text "") (offset-offset 0.0))
               (push-aux note-list text 1
                         ;; raise the text for the next one....
                         (+ (* (mod (incf text-count) 3) 0.5)
                            offset-offset)))
             (push-bass (note-list &optional (text "") (offset-offset 0.0))
               ;; (print note-list)
               (push-aux note-list text 2 offset-offset))
             (push-quad-bass (note-list &optional (text "") (offset-offset
                                                             0.0))
               ;; (print note-list)
               (push-aux note-list text 4 offset-offset))
             (push-quad-treble (note-list &optional 
                                          (text "") (offset-offset 0.0))
               (push-aux note-list text 3 offset-offset))
             (do-subsets (subset)
               (when (data subset)
                 (loop
                    for i below (sclist-length subset) 
                    for ss = (get-nth i subset)
                    for sstb = (get-cmn-treble-bass (data ss) nil 4stave)
                    for label = (format nil "~a ~a" 
                                         (id ss)
                                         (if (tag ss)
                                             (if (listp (tag ss))
                                                 (list-to-string (tag ss))
                                                 (tag ss))
                                             ""))
                    do 
                      ;; (print sstb)
                    (push-treble (first sstb) 
                                 (if 4stave "" label)
                                 ;;0.4)
                                 0)
                    (push-bass (second sstb))
                    (when 4stave
                      (push-quad-treble (third sstb) (if 4stave label ""))
                      (push-quad-bass (fourth sstb)))))))
      (push-treble (first main) (if 4stave "" text) 0.4)
      (push-bass (second main) "" 0.4)
      (when 4stave
        (push-quad-treble (third main) (if 4stave text ""))
        (push-quad-bass (fourth main)))
      (do-subsets (subsets cs))
      (do-subsets (related-sets cs))
      ;; following two don't have to be on quad-* staffs but we have to have
      ;; something in there
      (when include-missing-chromatic
        (push-treble (first mc) (if 4stave "" "Missing"))
        (push-bass (second mc))
        (push-quad-treble nil (if 4stave "Missing" ""))
        (push-quad-bass nil))
      (when include-missing-non-chromatic
        (push-treble (first mnc))
        (push-bass (second mnc))
        (push-quad-treble nil)
        (push-quad-bass nil)))
    (push (cmn::bar) treble-clef)
    (push (cmn::bar) bass-clef)
    (when 4stave
      (push (cmn::bar) quad-treble-clef)
      (push (cmn::bar) quad-bass-clef))
    (when line-break
      (push (cmn::line-break) treble-clef)
      (push (cmn::line-break) bass-clef)
      (when 4stave
        (push (cmn::line-break) quad-treble-clef)
        (push (cmn::line-break) quad-bass-clef)))
    (setf treble-clef (reverse treble-clef)
          bass-clef (reverse bass-clef))
    (when 4stave
      (setf quad-treble-clef (reverse quad-treble-clef)
            quad-bass-clef (reverse quad-bass-clef)))
    ;; (cmn::cmn-treble-bass-system treble-clef bass-clef)
    ;;(format t "~%~a ~a ~a ~a" (length treble-clef) (length bass-clef)
    ;;        (length quad-treble-clef) (length quad-bass-clef))
    ;; (print bass-clef)
    ;; (print treble-clef)
    (list treble-clef bass-clef quad-treble-clef quad-bass-clef)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Feb 11 12:33:15 GMT 2012: Removed MDE's original comment here as it
;;; has now been taken into the doc entry below.

;;; SAR Sat Feb 11 12:33:00 GMT 2012: Extended robodoc entry

;;; SAR Tue Feb  7 10:45:09 GMT 2012: Added robodoc entry

;;; ****f* complete-set/make-complete-set
;;; FUNCTION
;;; Create a complete-set object, which as an extension of the tl-set class
;;; allows checking for full sets: ones in which every note of *standard-scale*
;;; is present.  
;;;
;;; ARGUMENTS
;;; - A set of pitches. This can either take the form of a list of note-name
;;;   symbols or a complete-set, tl-set or sc-set object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - keyword argument :id. An number, symbol or string that is to be the ID of
;;;   the given complete-set object (see doc for sc-set).
;;; - keyword argument :tag. A number, symbol or string that is secondary name,
;;;   description, tag etc. for the given complete-set object. The :tag serves
;;;   for identification but not searching purposes (see doc for
;;;   named-object). 
;;; - keyword argument :subsets. An assoc-list of key/data pairs, in which the
;;;   data is a list of note-name symbols that are a subset of the main
;;;   set. One use for this keyword argument might be to create subsets that
;;;   particular instruments can play; these would then be selected in the
;;;   chord-function passed to the instrument object (see doc for sc-set).
;;; - keyword argument :related-sets. An assoc-list of key/data pairs, similar
;;;   to :subsets, only that the pitches given here do not have to be part of
;;;   the main set. This can be used, for example, for pitches missing from the
;;;   main set (see doc for sc-set).
;;; - keyword argument :auto-sort. T or NIL to indicate whether the specified
;;;   pitches (note-name symbols) are to be automatically sorted from lowest
;;;   to highest. T = sort. Default = T. (see doc for sc-set)
;;; - keyword argument :transposition. A number that is the number of semitones
;;;   by which the pitches of the new complete-set are to be transposed when
;;;   the object is created. Default = 0.  (see doc for tl-set)
;;; - keyword argument :limit-upper. A note-name symbol or a pitch object to
;;;   indicate the highest possible pitch in the given complete-set object to
;;;   be created. (see doc for tl-set)
;;; - keyword argument :limit-lower.  A note-name symbol or a pitch object to
;;;   indicate the lowest possible pitch in the complete-set object to be
;;;   created. (see doc for tl-set) 
;;; - keyword argument :complete. T, NIL, or 'CHROMATIC. This argument can be
;;;   given at init, and if the set is not complete in the sense of T or
;;;   'CHROMATIC (all chromatic, equally-tempered notes are present in the
;;;   set), a warning is printed. If the set is neither T nor 'CHROMATIC at
;;;   init, then no warning will be issued. In both cases the COMPLETE slot of
;;;   the given complete-set object will be set after checking the set.
;;; 
;;; RETURN VALUE
;;; A complete-set object.
;;; 
;;; EXAMPLE
#|
;; Create a complete set using a list of note-name symbols and the default
;; values for the keyword arguments
(make-complete-set '(d2 f2 a2 c3 e3 g3 b3 d4 gf4 bf4 df5 f5 af5 c6))

=>
COMPLETE-SET: complete: NIL
              num-missing-non-chromatic: 12
              num-missing-chromatic: 1
              missing-non-chromatic: (BQS BQF AQS AQF GQS GQF FQS EQS EQF DQS
                                      DQF CQS)
              missing-chromatic: (EF)
TL-SET: transposition: 0
        limit-upper: NIL
        limit-lower: NIL
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
SCLIST: sclist-length: 14, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: NIL, tag: NIL, 
data: (D2 F2 A2 C3 E3 G3 B3 D4 GF4 BF4 DF5 F5 AF5 C6)

;; A new complete-set object can be created from tl-set and sc-set objects
(let ((mcs (make-complete-set 
	    (make-tl-set '(d2 f2 a2 c3 e3 g3 b3 d4 gf4 bf4 df5 f5 af5)))))
  (pitch-symbols mcs))

=> (D2 F2 A2 C3 E3 G3 B3 D4 GF4 BF4 DF5 F5 AF5)

(let ((mcs (make-complete-set 
	    (make-sc-set '(d2 f2 a2 c3 e3 g3 b3 d4 gf4 bf4 df5 f5 af5)))))
  (pitch-symbols mcs))

=> (D2 F2 A2 C3 E3 G3 B3 D4 GF4 BF4 DF5 F5 AF5)

|#
;;; SYNOPSIS
(defun make-complete-set (set &key id tag subsets related-sets 
                          (transposition 0) (auto-sort t)
                          limit-upper limit-lower complete)
;;; ****
  (if (typep set 'sc-set)
      (let ((copy (clone set)))
        (when (or subsets related-sets (not auto-sort) complete)
          (error "complete-set::make-complete-set: When the set argument is a ~
                  complete-set, then the new set will be cloned ~
                  from the given and the arguments id, subsets, ~
                  related-sets, auto-sort, and complete are meaningless"))
        (unless (zerop transposition)
          (transpose copy transposition))
        (when (or limit-upper limit-lower)
          (limit copy :upper limit-upper :lower limit-lower))
        (when id
          (setf (id copy) id))
        copy)
      (make-instance 'complete-set :id id :tag tag :data set :subsets subsets 
                     :related-sets related-sets :auto-sort auto-sort
                     :limit-upper limit-upper :limit-lower limit-lower
                     :transposition transposition
                     :complete complete)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; notes is a simple list of pitch symbols
(defun make-stack (id notes &optional (num-stacks 1))
  (let ((set (make-complete-set notes :id id)))
    (stack set num-stacks)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The missing-chromatic and -non-chromatic are lists of simple note names
;;; without octaves.  Convert them to lists of pitch objects in octave 4.

(defun missing-to-pitches (note-list)
  (loop for n in note-list collect
        (make-pitch (read-from-string (format nil "~a4" n)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun notes-from-degrees-no-octave (degrees-list)
  (loop for degree in degrees-list collect
        (let* ((note-octave (string (degree-to-note degree)))
               (first-digit-pos (loop 
                                    for i from 0 
                                    for char = (elt note-octave i)
                                    when (or (digit-char-p char)
                                             ;; could have negative octave 
                                             ;; numbers 
                                             (eq char #\-))
                                    return i)))
          (read-from-string note-octave :start 0 :end first-digit-pos))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Returns two lists (once for the treble and one for the bass clef) of
;;; cmn-notes (not an instance, rather simply what cmn takes as input for a
;;; note).

#+cmn
(defun get-cmn-treble-bass (pitch-list &optional force-naturals 4stave)
  (if 4stave
      (loop for p in pitch-list
         for cmn-p = (get-cmn-data p force-naturals)
         if (> (midi-note p) 88) ;; gs6
         collect cmn-p into quad-treble
         else if (< (midi-note p) 34) ;; bf1
         collect cmn-p into quad-bass
         else if (< (midi-note p) 60)
         collect cmn-p into bass
         else collect cmn-p into treble
         ;; 26/12/09: don't have duplicated pitches
         finally (return (list (remove-duplicates treble :test #'equal)
                               (remove-duplicates bass :test #'equal)
                               (remove-duplicates quad-treble :test #'equal)
                               (remove-duplicates quad-bass :test #'equal))))
      (loop for p in pitch-list
         for cmn-p = (get-cmn-data p force-naturals)
         if (< (midi-note p) 60)
         collect cmn-p into bass
         else collect cmn-p into treble
         ;; 26/12/09: don't have duplicated pitches
         finally (return (list (remove-duplicates treble :test #'equal)
                               (remove-duplicates bass :test #'equal))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EOF complete-set.lsp

