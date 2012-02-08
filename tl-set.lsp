;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* sc-set/tl-set
;;; NAME 
;;; tl-set
;;;
;;; File:             tl-set.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> sclist -> sc-set 
;;;                   -> tl-set
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Implementation of the tl-set class that extends
;;;                   set to incorporate transposition and limiting to
;;;                   certain pitch ranges.  NB As of yet, once a set is
;;;                   transposed or limited, it can't be retransposed from its
;;;                   original pitches, only from the current set, i.e these
;;;                   methods are destructive!
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    13th August 2001
;;;
;;; $$ Last modified: 08:34:31 Thu Jan 12 2012 ICT
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

(defclass tl-set (sc-set)
  ;; transposition in semitones!  Can include fractions thereof.
  ((transposition :accessor transposition :type number :initarg :transposition
                  :initform 0)
   ;; could be a pitch object, or a note eg 'c4
   (limit-upper :accessor limit-upper :initarg :limit-upper :initform nil)
   (limit-lower :accessor limit-lower :initarg :limit-lower :initform nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((tls tl-set) &rest initargs)
  (declare (ignore initargs))
  (when (or (limit-upper tls)
            (limit-lower tls))
    (limit tls :upper (limit-upper tls) :lower (limit-lower tls)))
  (unless (zerop (transposition tls))
    (transpose tls (transposition tls))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone ((tls tl-set))
  (clone-with-new-class tls 'tl-set))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone-with-new-class :around ((tls tl-set) new-class)
  (declare (ignore new-class))
  (let ((set (call-next-method)))
    (setf (slot-value set 'transposition) (transposition tls)
          (slot-value set 'limit-upper) (when (limit-upper tls) 
                                          ;; we know it's a pitch instance so
                                          ;; clone is safe.
                                          (clone (limit-upper tls)))
          (slot-value set 'limit-lower) (when (limit-lower tls) 
                                          (clone (limit-lower tls))))
    set))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object :before ((tls tl-set) stream)
  (format stream "~&TL-SET: transposition: ~a~
                  ~&        limit-upper: ~a~
                  ~&        limit-lower: ~a"
          (transposition tls) (limit-upper tls) (limit-lower tls)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Wed Feb  8 10:42:33 GMT 2012: Added robodoc entry

;;; ****m* tl-set/stack
;;; FUNCTION
;;; Extend the pitch content of a given sc-set object by adding new pitch
;;; objects which have the same interval structure as the original set. 
;;;
;;; NB: See documentation for the stack method in the sc-set class for usage. 
;;; 
;;; SYNOPSIS
(defmethod stack ((tls tl-set) &optional (num-stacks 1) id)
;;; ****
  (declare (ignore num-stacks id))
  (let ((sc-set (call-next-method)))
    (clone-with-new-class sc-set 'tl-set)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defmethod (setf transposition) :before (value (tls tl-set))
  (declare (ignore value))
  (error "tl-set::(setf transposition): ~
          Don't setf the transposition slot of tl-set, call the transpose ~
          method instead: tl-set id: ~a" (id tls)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf limit-upper) :before (value (tls tl-set))
  (declare (ignore value))
  (error "tl-set::(setf limit): ~
          Don't setf the limit-upper slot of tl-set, call the limit ~
          method instead: tl-set id: ~a" (id tls)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf limit-lower) :before (value (tls tl-set))
  (declare (ignore value))
  (error "tl-set::(setf limit-lower): ~
          Don't setf the limit-lower slot of tl-set, call the limit ~
          method instead: tl-set id: ~a" (id tls)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon Feb  6 21:01:48 GMT 2012: Added robodoc entry

;;; ****m* tl-set/transpose
;;; FUNCTION
;;; Transpose the pitches of a given tl-set by a specified number of
;;; semitones. 
;;;
;;; The contents of the SUBSETS slot are automatically transposed as well, but
;;; the RELATED-SETS slot is left untransposed by default. An optional argument
;;; allows for RELATED-SETS slot to be transposed as well.
;;; 
;;; ARGUMENTS
;;; - A tl-set object.
;;; - A positive or negative integer that is the number of semitones by which
;;;   the pitch content of the given tl-set object is to be transposed.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - keyword argument :do-related-sets. T or NIL to indicate whether to
;;;   tranpsose any contents of the RELATED-SETS slot as well. T = transpose.
;;;   Default = NIL.  
;;; (- additional <ignore> arguments are for internal use only)
;;; 
;;; RETURN VALUE
;;; A tl-set object.
;;; 
;;; EXAMPLE
#|

;; By default the RELATED-SETS are left untransposed
(let ((mtls (make-tl-set '(d2 f2 a2 c3 e3 g3 b3 d4 gf4 bf4 df5 f5 af5 c6)
			 :subsets '((fl (df5 f5 af5))
				    (vla (e3 g3 b3)))
			 :related-sets '((missing (fs2 b5))))))
  (transpose mtls 3))

=> 
TL-SET: transposition: 3
        limit-upper: NIL
        limit-lower: NIL
SC-SET: auto-sort: T, used-notes: 
[...]
    subsets: 
FL: (E5 AF5 B5)
VLA: (G3 BF3 D4)
    related-sets: 
MISSING: (FS2 B5)
SCLIST: sclist-length: 14, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: NIL, tag: NIL, 
data: (F2 AF2 C3 EF3 G3 BF3 D4 F4 A4 CS5 E5 AF5 B5 EF6)

;; Set the <do-related-sets> argument to T for the RELATED-SETS contents to be
;; transposed as well
(let ((mtls (make-tl-set '(d2 f2 a2 c3 e3 g3 b3 d4 gf4 bf4 df5 f5 af5 c6)
			 :subsets '((fl (df5 f5 af5))
				    (vla (e3 g3 b3)))
			 :related-sets '((missing (fs2 b5))))))
  (transpose mtls 3 :do-related-sets t))

=> 
TL-SET: transposition: 3
        limit-upper: NIL
        limit-lower: NIL
SC-SET: auto-sort: T, used-notes: 
[...]
    subsets: 
FL: (E5 AF5 B5)
VLA: (G3 BF3 D4)
    related-sets: 
MISSING: (A2 D6)
SCLIST: sclist-length: 14, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: NIL, tag: NIL, 
data: (F2 AF2 C3 EF3 G3 BF3 D4 F4 A4 CS5 E5 AF5 B5 EF6)

|#
;;; SYNOPSIS
(defmethod transpose ((tls tl-set) semitones 
                      &key do-related-sets
                      ignore1 ignore2)
;;; ****
  (declare (ignore ignore1) (ignore ignore2))
  (setf (slot-value tls 'data) (transpose-pitch-list (data tls) semitones)
        (slot-value tls 'transposition) semitones)
  (transpose-rals (subsets tls) semitones)
  (when do-related-sets
    (transpose-rals (related-sets tls) semitones))
  tls)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 

;;; SAR Mon Feb  6 21:23:51 GMT 2012: Added robodoc entry. Deleted MDE's
;;; comment here as it has been taken into the doc below nearly verbatim. 

;;; ****m* tl-set/limit
;;; FUNCTION
;;; Remove pitch objects from a given tl-set whose pitch content is higher or
;;; lower than the pitches specified. Any pitch objects whose pitch content is
;;; equal to the limit pitches specified will be retained.
;;;
;;; NB: C0 and B10 are the highest and lowest powssible pitches of the
;;;     quarter-tone scale defined in scale.lsp (16.35 and 31608.55 Hz
;;;     respectively).
;;;
;;; NB: The keyword arguments for which the lower and upper limits are to be
;;;     specified are optional arguments, but are required in order for any
;;;     effect to be had.
;;; 
;;; ARGUMENTS
;;; - A tl-set object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - keyword-argument :upper. A note-name symbol that is the upper limit for
;;;   the limiting process.
;;; - keyword argument :lower. A note-name symbol that is the lower limit for
;;;   the limiting process.
;;; - keyword argument :do-related-sets. T or NIL to indicate whether the
;;;   RELATED-SETS slot of the given tl-set object is to be transposed as well
;;;   or left unhandled. T = transpose. Default = NIL.
;;; 
;;; RETURN VALUE
;;; A tl-set object.
;;; 
;;; EXAMPLE
#|
;;; By default the method does not transpose the pitches of the RELATED-SETS
;;; slot 
(let ((mtls (make-tl-set '(d2 f2 a2 c3 e3 g3 b3 d4 gf4 bf4 df5 f5 af5 c6)
			 :subsets '((fl (df5 f5 af5))
				    (vla (e3 g3 b3)))
			 :related-sets '((missing (fs2 b5))))))
  (limit mtls :upper 'df5 :lower 'c3))

=> 
TL-SET: transposition: 0
        limit-upper: 
PITCH: frequency: 554.365, midi-note: 73, midi-channel: 0 
[...]
    subsets: 
FL: (DF5)
VLA: (E3 G3 B3)
    related-sets: 
MISSING: (FS2 B5)
SCLIST: sclist-length: 14, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: NIL, tag: NIL, 
data: (C3 E3 G3 B3 D4 GF4 BF4 DF5)

;; Setting the :do-related-sets argument to T results in any RELATED-SETS pitch
;; content being transposed as well
(let ((mtls (make-tl-set '(d2 f2 a2 c3 e3 g3 b3 d4 gf4 bf4 df5 f5 af5 c6)
			 :subsets '((fl (df5 f5 af5))
				    (vla (e3 g3 b3)))
			 :related-sets '((missing (fs2 b5))))))
  (limit mtls :upper 'c6 :lower 'c3 :do-related-sets t))

=>
[...]
    subsets: 
FL: (DF5 F5 AF5)
VLA: (E3 G3 B3)
    related-sets: 
MISSING: (B5)
SCLIST: sclist-length: 14, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: NIL, tag: NIL, 
data: (C3 E3 G3 B3 D4 GF4 BF4 DF5 F5 AF5 C6)

|#
;;; SYNOPSIS
(defmethod limit ((tls tl-set) &key upper lower do-related-sets)
;;; ****
  (let ((u (limit-get-pitch upper 'b10)) ;; 'b10 and 'c0 are just defaults
        (l (limit-get-pitch lower 'c0)))
    (setf (slot-value tls 'limit-upper) u
          (slot-value tls 'limit-lower) l
          (slot-value tls 'data) (limit-aux (data tls) u l))
    (limit-ral (subsets tls) u l)
    (when do-related-sets
      (limit-ral (related-sets tls) u l)))
  tls)
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; In this case upper and lower are further limits that will be compared to
;;; those of the instrument when limiting the set.  Returns a pitch list
;;; though, not a set object.
;;; 13/1/10: be careful with this: if this function is returning nil, it could
;;; be because the set pitches are microtonal, when you're not expecting that.

;;; SAR Tue Feb  7 10:17:56 GMT 2012: Added robodoc entry

;;; ****m* tl-set/limit-for-instrument
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
(defmethod limit-for-instrument ((tls tl-set) (ins instrument)
                                 &key upper lower do-related-sets)
;;; ****
  ;; if we have upper and/or lower, find out which is
  ;; highest/lowest: these or the instrument's range; then use this to
  ;; limit the set.
  (let* ((ins-set (limit tls
                         :upper (if upper
                                    (pitch-min 
                                     upper
                                     (make-pitch 
                                      (highest-sounding ins)))
                                    (highest-sounding ins))
                         :lower (if lower
                                    (pitch-max 
                                     lower
                                     (make-pitch 
                                      (lowest-sounding ins)))
                                    (lowest-sounding ins))
                         :do-related-sets do-related-sets))
         (set-pitches (if (microtones ins)
                          (data ins-set)
                          ;; 21/2/07:
                          ;; if our instrument can't play microtones just get
                          ;; the normal chromatic notes from the set 
                          (get-chromatic ins-set)))
         (set-pitches-rm (if (missing-notes ins)
                             (remove-pitches set-pitches
                                             (missing-notes ins)
                                             :enharmonics-are-equal t
                                             :return-symbols nil)
                             set-pitches)))
    ;; (print set-pitches)
    set-pitches-rm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon Feb  6 20:29:54 GMT 2012: Added robodoc entry

;;; ****f* tl-set/make-tl-set
;;; FUNCTION
;;; Create a tl-set object, which extends the sc-set class by incorporating
;;; transpostion and limiting to certain pitch ranges. 
;;; 
;;; NB: As of yet, once a set is transposed or limited, it can't be
;;;     retransposed from its original pitches, only from the current set; i.e
;;;     these methods are destructive!
;;; 
;;; ARGUMENTS
;;; - A list of note-name symbols that is to be the set (pitch-set) for the
;;;   given tl-set object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - keyword argument :id. A symbol that is to be the ID of the given tl-set
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
;;; - keyword argument :limit-upper. A note-name symbol or a pitch object to
;;;   indicate the highest possible pitch in the tl-set object to be created. 
;;; - keyword argument :limit-lower.  A note-name symbol or a pitch object to
;;;   indicate the lowest possible pitch in the tl-set object to be created.
;;; - keyword argument :transposition. A number that is the number of semitones
;;;   by which the pitches of the new tl-set are to be transposed when the
;;;   object is created. Default = 0.
;;; - keyword argument :auto-sort. T or NIL to indicate whether the specified
;;;   pitches (note-name symbols) are to be automatically  sorted from lowest
;;;   to highest. T = sort. Default = T.
;;; 
;;; RETURN VALUE
;;; A tl-set object.
;;; 
;;; EXAMPLE
#|
;; Simple usage with default values for keyword arguments
(make-tl-set '(d2 f2 a2 c3 e3 g3 b3 d4 gf4 bf4 df5 f5 af5 c6))

=> 
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

;; Adding subsets and related-sets
(make-tl-set '(d2 f2 a2 c3 e3 g3 b3 d4 gf4 bf4 df5 f5 af5 c6)
	     :subsets '((fl (df5 f5 af5))
			(vla (e3 g3 b3)))
	     :related-sets '((missing (fs2 b5))))
=> 
TL-SET: transposition: 0
[...]
    subsets: 
FL: (DF5 F5 AF5)
VLA: (E3 G3 B3)
    related-sets: 
MISSING: (FS2 B5)
SCLIST: sclist-length: 14, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: NIL, tag: NIL, 
data: (D2 F2 A2 C3 E3 G3 B3 D4 GF4 BF4 DF5 F5 AF5 C6)

;; Limiting the pitch range of the tl-set object, once using a note-name
;; symbol and once using a pitch object
(make-tl-set '(d2 f2 a2 c3 e3 g3 b3 d4 gf4 bf4 df5 f5 af5 c6)
	     :limit-upper 'g5
	     :limit-lower (make-pitch 'd3))

=> 
TL-SET: transposition: 0
        limit-upper: 
PITCH: frequency: 783.991, midi-note: 79, midi-channel: 0 
[...]
        limit-lower: 
PITCH: frequency: 146.832, midi-note: 50, midi-channel: 0 
[...]
data: (E3 G3 B3 D4 GF4 BF4 DF5 F5)

;; Applying a transposition by semitones
(make-tl-set '(d2 f2 a2 c3 e3 g3 b3 d4 gf4 bf4 df5 f5 af5 c6)
	     :transposition 3)

=>
TL-SET: transposition: 3
[...]
data: (F2 AF2 C3 EF3 G3 BF3 D4 F4 A4 CS5 E5 AF5 B5 EF6)

|#
;;; SYNOPSIS
(defun make-tl-set (set &key id subsets related-sets
                             limit-upper limit-lower
                             (transposition 0)
                             (auto-sort t))
;;; ****
  (make-instance 'tl-set :id id :data set :subsets subsets 
                 :related-sets related-sets :auto-sort auto-sort
                 :limit-upper limit-upper :limit-lower limit-lower
                 :transposition transposition))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun limit-get-pitch (p default)
  (if p
      (if (typep p 'pitch)
          p
        (make-pitch p))
    (make-pitch default)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun limit-ral (ral upper lower)
  (loop for i in (data ral) and j from 0 do
        (if (is-ral (data i))
            (limit-ral (data i) upper lower)
          (setf (data (nth j (data ral)))
            (limit-aux (data i) upper lower)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; pitch-list is a list of pitches, lower and upper are both pitch objects.

(defun limit-aux (pitch-list upper lower)
  (unless (and (typep lower 'pitch)
               (typep upper 'pitch))
    (error "tl-set::limit-aux: lower and upper must be pitch objects: ~a ~a"
           lower upper))
  (unless (and (listp pitch-list)
               (loop for p in pitch-list
                   unless (typep p 'pitch) do (return nil)
                   finally (return t)))
    (error "tl-set::limit-aux: ~
            pitch-list must be a simple list of pitch objects: ~a"
           pitch-list))
  (remove-if #'(lambda (x) (or (pitch> x upper)
                               (pitch< x lower)))
             pitch-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun transpose-rals (ral semitones)
  (loop for i in (data ral) and j from 0 do
        (if (is-ral (data i))
            (transpose-rals (data i) semitones)
          (setf (data (nth j (data ral))) 
            (transpose-pitch-list (data i) semitones)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EOF tl-set.lsp
