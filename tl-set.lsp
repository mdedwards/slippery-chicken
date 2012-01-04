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
;;; $$ Last modified: 14:01:17 Wed Jan  4 2012 ICT
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

;;; ****m* tl-set/stack
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

;;; ****m* tl-set/transpose
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
;;; Remove pitches that are higher and lower than those given.  Those equal in
;;; frequency will be retained.
;;; c0 and b10 are the highest and lowest pitches of the quarter-tone scale
;;; defined in scale.lsp (16.35 and 31608.55 Hz respectively) 

;;; ****m* tl-set/limit
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

;;; ****f* tl-set/make-tl-set
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
