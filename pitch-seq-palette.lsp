;;; 02.12.11 SEAN: changed robodoc header to reflect class hierarchy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* palette/pitch-seq-palette
;;; NAME 
;;; pitch-seq-palette
;;;
;;; File:             pitch-seq-palette.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> sclist -> 
;;;                   circular-sclist -> assoc-list -> recursive-assoc-list ->
;;;                   palette -> pitch-seq-palette
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Implementation of the pitch-seq-palette class.
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    19th February 2001
;;;
;;; $$ Last modified: 21:44:59 Wed Oct 19 2011 BST
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

(defclass pitch-seq-palette (palette)
  ((num-notes :accessor num-notes :type integer :initarg :num-notes 
              :initform nil)
   ;; 24/3/07: a list of instruments that are specifically mentioned in the
   ;; pitch-seqs  
   (instruments :accessor instruments :type list :initarg :instruments 
                :initform nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; NB verify-and-store is called automatically from the sclist init and that
;;; concerns the data slot, here we initialize slots particular to this class.

(defmethod initialize-instance :after ((psp pitch-seq-palette) &rest initargs)
  (declare (ignore initargs))
  (let ((nn (num-notes psp)))
    (when nn
      (unless (and (integerp nn) (>= nn 0))
        (error "pitch-seq-palette::initialize-instance: ~
                The num-notes slot of pitch-seq-palette must be set ~
                and must also be an integer >= 0.")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object :before ((psp pitch-seq-palette) stream)
  (format stream "~%PITCH-SEQ-PALETTE: num-notes: ~a, instruments: ~a"
          (num-notes psp) (instruments psp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone ((psp pitch-seq-palette))
  (clone-with-new-class psp 'pitch-seq-palette))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone-with-new-class :around ((psp pitch-seq-palette) new-class)
  (declare (ignore new-class))
  (let ((palette (call-next-method)))
    (setf (slot-value palette 'num-notes) (num-notes psp)
          (slot-value palette 'instruments) (instruments psp))
    palette))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod verify-and-store :after ((psp pitch-seq-palette))
  ;; this will be the case if we're cloning
  (unless (typep (first (data psp)) 'pitch-seq)
    ;; at this point the data is untouched due to the clause in the assoc-list
    ;; method.  If this data is recursive, the error will be picked up
    ;; elsewhere.
    (loop for i in (data psp) and j from 0
          for ps = 
          (progn
            (when (and (listp i)
                       (or (lisp-assoc-listp i) 
                           (lisp-assoc-listp (second i))))
              (error "pitch-seq-palette::verify-and-store: ~
                      Recursive pitch-seq-palettes are not allowed: ~a"
                     i))
            (make-pitch-seq i (format nil "~a-ps-~a" 
                                      (id psp) (1+ j))))
          unless (= (num-notes psp) (sclist-length ps))
          do (error "pitch-seq-palette::verify-and-store: ~%~
                     In pitch-seq ~a from palette ~a:~%~
                    Each pitch sequence must have ~a notes (you have ~a): ~%~a"
                    (id ps) (id psp) (num-notes psp) (sclist-length ps)
                    (data psp))
          do (setf (nth j (data psp)) ps))
    ;; we call this now explicitly from the assoc-list-class because it was
    ;; avoided there. 
    (all-ids-unique psp))
  (setf (instruments psp) 
    (remove-duplicates
     (loop for ps in (data psp) appending (instruments ps))))
  (setf (num-data psp) (r-count-elements psp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod add-inversions ((psp pitch-seq-palette))
  (loop for ps in (data psp) do
        (add (invert ps) psp))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod combine ((psp1 pitch-seq-palette) (psp2 pitch-seq-palette))
  (let ((result (clone psp1)))
    (setf (id result) (combine-ids psp1 psp2)
          (num-notes result) (+ (num-notes psp1) (num-notes psp2)))
    (loop 
        for ps1 in (data psp1) 
        for ps2 = (get-next psp2)
        and i from 0
        do (setf (nth i (data result)) 
             (make-pitch-seq (append (data (clone ps1)) 
                                     (data (clone ps2)))
                             (id ps1))))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 19/3/07: This method looks to see if a ps was named after an
;;; instrument--this means that the string of player is searched for in the ids
;;; of the psp ("flute2-inverted" would match "flute").  If true, then we call
;;; get-next until we get a match, otherwise we just return get-next.  By
;;; implication we can have several ps's for an instrument and they should be
;;; alternated.
;;; 
;;; 24/3/07: changed this so we can specify instruments a pitch-seq applies to
;;; like this:
;;;     ((4 5 6) sdf pno vc rew)
;;; or  (psp4 ((2 4 3) pno vc cl))
;;; this implies that such a ps is __only__ for those instruments!  So a psp
;;; that specifies one of more pitch-seqs with instruments means that only such
;;; pitch-seqs will be used for those instruments and instruments not mentioned
;;; will only use pitch-seqs who have __no__ instruments.

(defmethod get-next-for-ins ((psp pitch-seq-palette) ins)
  (if (member ins (instruments psp))
      (loop for ps = (get-next psp) do
            (when (member ins (instruments ps))
              ;; (format t "~&~a (member): ~a" ins (id ps))
              (return ps)))
    (loop for ps = (get-next psp) do
          (unless (instruments ps)
            ;; (format t "~&~a: ~a" ins (id ps))
            (return ps)))))

#|    
(defmethod get-next-for-ins ((psp pitch-seq-palette) ins)
  (let* ((ids (get-all-refs psp))
         (instr (string ins))
         (instrlen (length instr))
         (doit (loop 
                   for id in ids
                             ;; all ids are returned as a string, even if
                             ;; there's no recursion
                   for idstr = (string (first id))
                   do
                     (when (and (>= (length idstr) instrlen)
                                (string= instr idstr :end2 instrlen))
                       (return t)))))
    (if doit
        (loop 
            for ps = (get-next psp)
            for idstr = (string (id ps))
            for match = (and (>= (length idstr) instrlen)
                             (string= instr idstr :end2 instrlen))
            do
              (when match
                ;; (format t "~&get-next-for-ins: using ~a" (id ps))
                (return ps)))
      (get-next psp))))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod psp-subseq ((psp pitch-seq-palette) start end)
  (let* ((num-notes (- end start))
         (new-psp (make-psp (format nil "~a-post-psp-subseq"
                                    (id psp))
                            num-notes nil)))
    (loop 
        for ps in (data psp) 
        for new-ps = (ps-subseq ps start end)
        do
          (unless (= num-notes (sclist-length new-ps))
            (error "pitch-seq-palette::psp-subseq: ~
                    num-notes: ~a != ~a: ~a ~&start ~a, end ~a"
                   num-notes (sclist-length new-ps) new-ps start end))
          (add new-ps new-psp))
    (setf (instruments new-psp) (copy-list (instruments psp)))
    new-psp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-psp (id num-notes pitch-seqs)
  ;; let's try and catch errors here before sclist complains...
  (loop for i in pitch-seqs unless (listp i)
     ;; todo: I think this is causing an error when we indicate a chord as
     ;; the first note 
      do (error "pitch-seq-palette::make-psp: ~
                 The argument to make-psp should be a list of lists: ~%~a"
                pitch-seqs))
  (make-instance 'pitch-seq-palette :id id :data pitch-seqs
                 :num-notes num-notes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EOF pitch-seq-palette.lsp

