;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* _sc/rthm-seq-palette
;;; NAME 
;;; set-palette
;;;
;;; File:             rthm-seq-palette.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> sclist -> 
;;;                   circular-sclist -> assoc-list -> recursive-assoc-list ->
;;;                   palette -> rthm-seq-palette
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Implementation of the rthm-seq-palette class.
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    19th February 2001
;;;
;;; $$ Last modified: 15:40:12 Sat Feb  5 2011 ICT
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

(defclass rthm-seq-palette (palette)
  ;; whether we've created inversions of the pitch-seqs in pitch-seq-palette
  ((psp-inversions :accessor psp-inversions :type boolean 
                   :initarg :psp-inversions :initform nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object :before ((rsp rthm-seq-palette) stream)
  (format stream "~%RTHM-SEQ-PALETTE: psp-inversions: ~a"
          (psp-inversions rsp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod verify-and-store :after ((rsp rthm-seq-palette))
  (ral-to-rsp rsp (psp-inversions rsp))
  (link-named-objects rsp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone ((rsp rthm-seq-palette))
  (clone-with-new-class rsp 'rthm-seq-palette))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* rthm-seq-palette/create-psps
;;; FUNCTION
;;; create-psps:
;;; 
;;; Automatically create pitch-seq-palettes for each rthm-seq in the
;;; rthm-seq-palette.  The selection function given as argument should be able
;;; to provide a list of numbers (relative notes) for a rthm-seq of any length;
;;; it takes two arguments only: the number of notes needed and the pitch-seq
;;; data lists (see below).  
;;;
;;; As a psp usually has several options for each rthm-seq, it's best when the
;;; selection-fun doesn't always return the same thing given the same number of
;;; notes.  NB This will silently kill any pitch-seq-palettes already supplied
;;; for any rthm-seqs in the palette
;;; 
;;; Note that the default selection function will suffice in lots of cases.
;;; However, you may just want to use different data lists with the default
;;; function.  In that case just pass these via :selection-fun-data.
;;;
;;; DATE 30/3/06
;;;
;;; ARGUMENTS:
;;; - the rth-seq-palette object
;;; - (key :selection-fun default #'create-psps-default) the selection function
;;;   object that will return the pitch-seq numbers. Takes two arguments only:
;;;   the number of notes needed and the pitch-seq data lists.  The function
;;;   also needs to be able to handle being passed nil nil as arguments; in this
;;;   case it should reset, if needs be i.e. it's just a call to init and should
;;;   return nothing. 
;;; - (key :pitch-seqs-per-rthm-seq) the number of pitch-seqs each rthm-seq
;;;   should have.  NB The pitch-seqs given in the selection function will
;;;   simply be cycled through to create the required number.  Default: 3.
;;; - (key :pitch-seqs-per-rthm-seq) the pitch-seq lists to be passed to the
;;;   default selection function. 
;;;   There can be as many pitch-seqs in these lists as desired; the number of
;;;   notes the pitch-seq will provide is the first item of the list; these
;;;   need not be in ascending order.  If you pass t here, the selection
;;;   function will reinitialize its default data and use that.
;;;   At the moment these look like:
;;;             '((1 ((3) (3) (1) (25)))
;;;               (2 ((3 4) (5 2) (25 25) (1 25)))
;;;               (3 ((3 4 3) (5 9 6) (1 2 4) (5 2 2) (6 2 3)))
;;;               (4 ((3 4 3 4) (5 3 6 4) (9 4 5 11) (2 10 4 8)))
;;;               (5 ((5 5 6 5 8) (7 7 7 4 8) (11 8 4 10 2) (7 7 4 9 9)))
;;;               (6 ((4 5 5 3 6 6) (3 8 3 9 3 8) (9 3 9 5 10 6)))
;;;               (7 ((8 8 8 5 9 6 9 ) (9 3 8 4 7 5 4) (3 4 3 5 3 4 3)))
;;;               (8 ((3 3 4 3 3 1 5 4) (10 3 9 3 8 3 7 4) (3 5 8 2 8 9 4 11)))
;;;               (9 ((3 6 4 7 4 7 3 6 7) (10 2 9 2 8 2 7 2 3) 
;;;                   (2 9 3 9 4 9 9 6 11)))
;;;               (10 ((9 9 9 3 9 9 3 5 9 5) (8 9 8 9 5 9 9 5 6 6)))
;;;               (12 ((1 2 5 5 5 5 5 5 5 5 4 5) (2 1 5 1 5 1 6 5 1 5 2 5)))
;;;               (13 ((1 2 5 5 5 5 5 5 5 5 4 5 2) (2 1 5 1 5 1 6 5 1 5 2 5 1)))
;;;               (14 ((1 2 5 5 5 5 5 5 5 5 4 5 2 1) 
;;;                    (2 1 5 1 5 1 6 5 1 5 2 5 1 2)))
;;;               (15 ((1 2 5 5 5 5 5 5 5 5 4 5 2 1 2) 
;;;                    (2 1 5 1 5 1 6 5 1 5 2 5 1 2 6))))))
;;; 
;;; RETURN VALUE: 
;;; always t
;;; 
;;; SYNOPSIS
(defmethod create-psps ((rsp rthm-seq-palette) 
                        &key
                        (selection-fun #'create-psps-default)
                        (selection-fun-data nil)
                        (pitch-seqs-per-rthm-seq 3))
;;; ****
  ;; 3.2.11: got to reinitialize our cscls so make this reset call here;
  ;; create-psps-default or the user-supplied selection-fun needs to be able to
  ;; handle this
  (create-psps-default nil nil)
  (loop with pass-data = t
     for rs in (data rsp) do
       ;; (print pass-data)
       (if (rsp-p (data rs))
           (create-psps (data rs) :selection-fun selection-fun 
                        :pitch-seqs-per-rthm-seq pitch-seqs-per-rthm-seq
                        :selection-fun-data 
                        (when pass-data
                          selection-fun-data))
           (unless (zerop (num-notes rs))
             (let ((psp (loop repeat pitch-seqs-per-rthm-seq collect
                             (funcall selection-fun 
                                      (num-notes rs)
                                      (when pass-data
                                        selection-fun-data)))))
               (setf (pitch-seq-palette rs) psp)
               ;; now turn the lists into a real psp
               (init-psp rs))))
       ;; we only need to pass the pitch-seq data once
       (setf pass-data nil))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* rthm-seq-palette/reset-psps
;;; FUNCTION
;;; reset-psps:
;;;
;;; Call reset for all the pitch-seq-palettes in the rthm-seq objects in the
;;; palette.  This ensures that each rthm-seq starts over again at the first
;;; given pitch-seq.
;;; 
;;; ARGUMENTS:
;;; - the rthm-seq-palette object
;;; 
;;; RETURN VALUE: 
;;; always t
;;; 
;;; SYNOPSIS
(defmethod reset-psps ((rsp rthm-seq-palette))
;;; ****
  (loop 
      with psp
      for rs in (data rsp) do
        (if (rsp-p (data rs))
            (reset-psps (data rs))
          (progn
            (unless (rthm-seq-p rs)
              (error "~a~%rthm-seq-palette::reset-psps: not a rthm-seq!"
                     rs))
            (setf psp (pitch-seq-palette rs))
            (when psp
              (reset psp)))))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod scale ((rsp rthm-seq-palette) scaler
                  &optional ignore1 ignore2 ignore3)
  (declare (ignore ignore1) (ignore ignore2) (ignore ignore3))
  (loop 
      for rs in (data rsp) 
      for i from 0
      do
        (if (rsp-p (data rs))
            (scale (data rs) scaler)
          (progn
            (unless (rthm-seq-p rs)
              (error "~a~%rthm-seq-palette::scale: not a rthm-seq!"
                     rs))
            (setf (nth i (data rsp)) (scale rs scaler)))))
  rsp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+cmn
(defmethod get-cmn-data ((rsp rthm-seq-palette) 
                         &optional ignore1 ignore2 ignore3 ignore4 ignore5 
                                   ignore6 ignore7 ignore8)
  (declare (ignore ignore1 ignore2 ignore3 ignore4 ignore5 ignore6 ignore7
                   ignore8))
  (loop 
      for rs in (data rsp) 
      if (is-ral (data rs))
      append (get-cmn-data (data rs))
      ;; collect (get-cmn-data (data rs)))
      ;; else append (get-cmn-data rs)
      else collect (econs (get-cmn-data rs) 
                          (cmn::line-mark))
           ;;collect (cmn::line-mark)
           ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+cmn
(defmethod cmn-display ((rsp rthm-seq-palette)
                        &key
                        (all-output-in-one-file t)
                        (file "/tmp/cmn.eps")
                        (staff-separation 3)
                        (line-separation 5)
                        (page-nums t)
                        (seqs-per-system 1)
                        (size 15))
  (format t "~&Generating rthm-seqs...")
  (let* ((cmn-data (get-cmn-data rsp))
         (sys
          (loop 
              with voices = (ml nil seqs-per-system)
              for staff in cmn-data 
              for i from 0
              do
                (push staff (nth (mod i seqs-per-system) voices))
              finally 
                (return (loop for v in voices collect 
                              (flatten (nreverse v))))))
         (system (cmn::system cmn::bracket
                  (cmn::engorge
                   (loop for staff in sys collect
                         (cmn::staff cmn::bar (cmn::engorge staff)))))))
    (format t "~&Calling cmn...")
    (cmn::cmn-display 
     system
     :file file 
     ;; :spacing-hook #'cmn::even-spacing
     :auto-bar-nums nil
     :size size 
     :page-nums page-nums
     :line-separation line-separation
     :all-output-in-one-file all-output-in-one-file
     :staff-separation staff-separation)))
     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Calls chop for each seq in the rsp--see rthm-seq-bar::chop-bar for
;;; details of these arguments.  Returns a new rthm-seq-palette with the same
;;; structure as the argument but with a further level of nesting: each
;;; rthm-seq in the argument is replaced by a list of rthm-seqs that are one
;;; 'slice' of the original rthm-seq.

(defmethod chop ((rsp rthm-seq-palette) &optional chop-points 
                                                  (unit 's)
                                                  (number-bars-first t))
  (let ((result (ral-to-rsp (duplicate-structure rsp) nil))
        (refs (get-all-refs rsp)))
    (loop 
        for ref in refs 
        for chops = (chop (get-data ref rsp) chop-points unit
                          number-bars-first) 
        for sub-rsp = (make-rsp (first (last ref))
                                nil)
        do
          (setf (tag sub-rsp) (list-to-string (econs ref "chopped")))
          (loop for rs in chops do (add rs sub-rsp))
          (set-data ref (make-named-object (id sub-rsp) sub-rsp) result))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-multipliers ((rsp rthm-seq-palette) rthm &optional id round)
  (get-multipliers (get-data id rsp) rthm round))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-rsp (id data &key (psp-inversions nil))
  (make-instance 'rthm-seq-palette :id id :data data
                 :psp-inversions psp-inversions))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ral-to-rsp (ral psp-inversions)
  (when (data ral)
    (loop for i in (data ral) and j from 0 do
          (let ((data (data i)))
            (if (is-ral data)
                ;; 22/2/07: to be consistent this shouldn't simply be a ral but
                ;; a named-object whose data is a ral
                (let ((sub-ral (ral-to-rsp data psp-inversions)))
                  ;; (setf (tag sub-ral) (id sub-ral))
                  (setf (nth j (data ral)) ;; (data (nth j (data ral)))
                    (make-named-object (id i) sub-ral)))
              (setf (nth j (data ral)) 
                (if (rthm-seq-p (data i))
                    (progn
                      (when psp-inversions
                        (add-inversions (pitch-seq-palette (data i))))
                      (data i))
                  (make-rthm-seq i :psp-inversions psp-inversions))))))
    (sc-change-class ral 'rthm-seq-palette)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rsp-p (thing)
  (typep thing 'rthm-seq-palette))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((seqs '((1 (((3)) ((3)) ((1)) ((25))))
              (2 ((3 4) (5 2) ((25) 25) (1 25)))
              (3 (((3) 4 (3)) (5 (9) 6) (1 2 4) (5 2 (2)) (6 2 3)))
              (4 ((3 4 3 4) (5 3 6 4) ((9) 4 5 (11)) (2 (10) 4 8)))
              (5 (((5) (5) 6 (5) 8) ((7) (7) (7) 4 8) (11 8 4 10 2) 
                  ((7) (7) 4 (9) (9))))
              (6 ((4 (5) (5) 3 (6) (6)) ((3) 8 (3) 9 (3) 8) (9 3 9 5 10 6)))
              (7 (((8) (8) (8) 5 (9) 6 (9)) (9 3 8 4 7 5 4)
                  ((3) 4 (3) 5 (3) 4 (3))))
              (8 (((3) (3) 4 (3) (3) 1 5 4) (10 3 9 3 8 3 7 4)
                  (3 5 8 2 8 9 4 11)))
              (9 ((3 6 4 (7) 4 (7) 3 6 (7)) (10 (2) 9 (2) 8 (2) 7 (2) 3) 
                  (2 (9) 3 (9) 4 (9) (9) 6 (11))))
              (10 ((9 9 9 (3) 9 9 (3) 5 9 5) (8 9 8 9 5 9 9 5 6 6)))
              (12 ((1 2 5 5 5 5 5 5 5 5 4 5) (2 1 5 1 5 1 6 5 1 5 2 5)))
              (13 ((1 2 5 5 5 5 5 5 5 5 4 5 2) (2 1 5 1 5 1 6 5 1 5 2 5 1)))
              (14 ((1 2 5 5 5 5 5 5 5 5 4 5 2 1) 
                   (2 1 5 1 5 1 6 5 1 5 2 5 1 2)))
              (15 ((1 2 5 5 5 5 5 5 5 5 4 5 2 1 2) 
                   (2 1 5 1 5 1 6 5 1 5 2 5 1 2 6)))))
      (seqs-al nil))
  ;; ****f* rthm-seq-palette/create-psps-default
  ;; FUNCTION
  ;; create-psps-default
  ;;
  ;; Create pitch-sequences for the create-psps method.  This is the callback
  ;; function that is passed by default.  This (and the above lists) was first
  ;; used in I Kill by Proxy.  If data isn't provided for a sequence of a
  ;; certain length, a (recursive!) attempt will be made to make one up from
  ;; two sequences of lesser length.
  ;; 
  ;; ARGUMENTS:
  ;; - number of notes we need a psp for
  ;; - the pitch-seq data (see documentation for create psps method).  Ideally
  ;; this would only be passed the first time the function is called.
  ;; 
  ;; RETURN VALUE: 
  ;; a list of numbers suitable for use in creating a pitch-seq
  ;; 
  ;; SYNOPSIS
  (defun create-psps-default (num-notes data-lists)
    ;; (print data-lists)
    ;; ****
    ;; 3.2.11 need to reinitialize our cscls the first time we call
    ;; create-psps, otherwise the only way to get the same piece each time is
    ;; to reload all sc source files i.e. restart lisp
    (unless (or num-notes data-lists)
      (setf seqs-al nil))
    (when (or data-lists (not seqs-al))
      (let ((dl (if (and data-lists (listp data-lists))
                    data-lists
                    ;; e.g. if data-lists is t, we'll use the default data again
                    seqs)))
        ;; make an assoc-list of circular sc-lists out of the sequences.
        (setf dl
              (loop
                 for l in dl
                 for order = (first l)
                 for pss = (second l)
                 do
                 (loop for ps in pss do
                    ;; check we've the right number of notes.
                      (unless (= order (length ps))
                        (error "kill-get-ps: need ~a elements, got ~a: ~a"
                               order (length ps) ps)))
                 collect
                 (list order (make-cscl pss)))
              seqs-al (make-assoc-list 'create-psps-default dl))))
    (when (and (numberp num-notes) (> num-notes 0))
      (let ((ps (get-data num-notes seqs-al nil)))
        (if ps
            (get-next (data ps))
            (progn
              ;; must avoid infinite recursions....
              (when (= 1 num-notes)
                (error "rthm-seq-palette::create-psps-default: no data for 1!~
                    Avoiding infinite recursion."))
              (let* ((left (floor num-notes 2))
                     (right left))
                (when (oddp num-notes)
                  (incf left))
                (append (create-psps-default left nil) 
                        (create-psps-default right nil)))))))))
        

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF rthm-seq-palette.lsp

