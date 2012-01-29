;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* palette/rthm-seq-palette
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
;;; $$ Last modified: 21:33:00 Fri Jan 13 2012 ICT
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

;;; 02.12.11 SEAN: changed robodoc header to reflect class hierarchy

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

;;; SAR Sat Jan 28 13:16:35 GMT 2012: Edited robodoc info

;;; ****m* rthm-seq-palette/create-psps
;;; DATE 
;;; 30 Mar 2006
;;;
;;; FUNCTION
;;; Automatically create pitch-seq-palette objects for each rthm-seq object in
;;; the given rthm-seq-palette object. 
;;;
;;; The selection function given as an optional keyword argument should be able
;;; to generate a list of numbers (relative note levels) for a rthm-seq of any
;;; length; it takes two arguments only: the number of notes needed and the
;;; pitch-seq data lists (see below).  
;;;
;;; As a pitch-seq-palette usually has several options for each rthm-seq
;;; object, it's best when the selection-fun doesn't always return the same 
;;; thing given the same number of notes. NB: This will silently kill the data
;;; of any pitch-seq-palette objects already supplied for any rthm-seqs in the
;;; palette.  
;;; 
;;; Note that the default selection function will suffice in lots of cases.
;;; However, you may just want to use different data lists with the default
;;; function. In that case just pass these via :selection-fun-data.
;;;
;;; ARGUMENTS 

;;; - A rth-seq-palette object.
;;; - keyword argument :selection-fun. This is a function that will return the
;;;   pitch-seq numbers. It takes two arguments only: 1) the number of notes
;;;   needed, and 2) the pitch-seq data lists. The function also needs to be
;;;   able to handle being passed NIL NIL as arguments. In this case it should
;;;   reset, if needs be; i.e. it's just a call to init and should return
;;;   nothing. Default = #'create-psps-default.
;;; - keyword argument :pitch-seqs-per-rthm-seq. This is an integer that is the
;;;   number of pitch-seqs each rthm-seq should have. NB: The method will
;;;   simply cylcle through the pitch-seqs given in the selection function to
;;;   create the required number.  Default = 3. 
;;; - keyword argument :selection-fun-data. This contains the pitch-seq lists
;;;   to be passed to the default selection function. There can be as many
;;;   pitch-seqs in these lists as desired. The number of notes the pitch-seq
;;;   will provide is the first item of the list. They need not be in ascending
;;;   order. When this argument is passed a value of T, the selection function
;;;   will reinitialize its default data and use that. 
;;;
;;;   At the moment, the default data are:
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
;;; RETURN VALUE  
;;; Always returns T.
;;;
;;; EXAMPLE

#|

;; Create a rthm-seq-palette object that specifies pitch-seq-palettes for each
;; contained rthm-seq object and print the values of the individual
;; pitch-seq-palettes. Then apply the create-psps method using its default
;; values, and print the values of the individual pitch-seq-palettes again to
;; see the change.  
(let ((mrsp (make-rsp 'rsp-test 
		      '((seq1 ((((2 4) q +e. s)
				((s) e (s) q)
				(+e. s { 3 (te) te te } ))
			       :pitch-seq-palette (1 2 3 4 5 6 7)))
			(seq2 ((((3 4) (e.) s { 3 te te te } +q)
				({ 3 +te (te) te } e e (q)))
			       :pitch-seq-palette (2 3 4 5 6 7 8)))
			(seq3 ((((2 4) e e { 3 te te te })
				((5 8) (e) e e e s s))
			       :pitch-seq-palette (3 4 5 6 7 8 9 10 1 2)))))))
  (print (loop for i in (data mrsp)
	    collect 
	      (loop for j in (data (pitch-seq-palette i))
		 collect (data j))))
  (create-psps mrsp)
  (print (loop for i in (data mrsp)
	    collect 
	      (loop for j in (data (pitch-seq-palette i))
		 collect (data j)))))

=>
(((1 2 3 4 5 6 7)) ((2 3 4 5 6 7 8)) ((3 4 5 6 7 8 9 10 1 2))) 

(((8 8 8 5 9 6 9) (9 3 8 4 7 5 4) (3 4 3 5 3 4 3))
 ((8 8 8 5 9 6 9) (9 3 8 4 7 5 4) (3 4 3 5 3 4 3))
 ((9 9 9 3 9 9 3 5 9 5) (8 9 8 9 5 9 9 5 6 6) (9 9 9 3 9 9 3 5 9 5)))

;; Use the :pitch-seqs-per-rthm-seq keyword argument to specify the number of
;; pitch-seq objects to be created for each rthm-seq. This example creates 5
;; instead of the default 3.
(let ((mrsp (make-rsp 'rsp-test 
		      '((seq1 ((((2 4) q +e. s)
				((s) e (s) q)
				(+e. s { 3 (te) te te } ))
			       :pitch-seq-palette (1 2 3 4 5 6 7)))
			(seq2 ((((3 4) (e.) s { 3 te te te } +q)
				({ 3 +te (te) te } e e (q)))
			       :pitch-seq-palette (2 3 4 5 6 7 8)))
			(seq3 ((((2 4) e e { 3 te te te })
				((5 8) (e) e e e s s))
			       :pitch-seq-palette (3 4 5 6 7 8 9 10 1 2)))))))
  (create-psps mrsp :pitch-seqs-per-rthm-seq 5)
  (print (loop for i in (data mrsp)
	    collect 
	      (loop for j in (data (pitch-seq-palette i))
		 collect (data j)))))

=>
(((8 8 8 5 9 6 9) (9 3 8 4 7 5 4) (3 4 3 5 3 4 3) (8 8 8 5 9 6 9)
  (9 3 8 4 7 5 4))
 ((3 4 3 5 3 4 3) (8 8 8 5 9 6 9) (9 3 8 4 7 5 4) (3 4 3 5 3 4 3)
  (8 8 8 5 9 6 9))
 ((9 9 9 3 9 9 3 5 9 5) (8 9 8 9 5 9 9 5 6 6) (9 9 9 3 9 9 3 5 9 5)
  (8 9 8 9 5 9 9 5 6 6) (9 9 9 3 9 9 3 5 9 5)))

|#
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

;;; SAR Sat Jan 28 15:10:10 GMT 2012: Edited robodoc info

;;; ****m* rthm-seq-palette/reset-psps
;;; FUNCTION
;;; Call the reset method (inherited from circular-sclist) for all
;;; pitch-seq-palette objects of all rthm-seq objects in the given
;;; rthm-seq-palette object, resetting their pointers to the head of the
;;; sequence. This ensures that each rthm-seq starts over again at the first
;;; note of the first given pitch-seq.
;;; 
;;; ARGUMENTS 
;;; - A rthm-seq-palette object.
;;; 
;;; RETURN VALUE  
;;; Always returns T.
;;;
;;; EXAMPLE
#|

;; Create a rthm-seq-palette object whose first rthm-seq has three pitch-seq
;; objects in its pitch-seq-palette. Apply the get-next method to the
;; pitch-seq-palette object of the first rthm-seq object twice, then print the
;; data of the next pitch-seq object to show where we are. Apply the reset-psps
;; method and print the data of the next pitch-seq object to show that we've
;; returned to the beginning of the pitch-seq-palette.

(let ((mrsp
       (make-rsp 'rsp-test 
		 '((seq1 ((((2 4) q +e. s)
			   ((s) e (s) q)
			   (+e. s { 3 (te) te te } ))
			  :pitch-seq-palette ((1 2 3 4 5 6 7)
					      (1 3 5 7 2 4 6)
					      (1 4 2 6 3 7 5)
					      (1 5 2 7 3 2 4))))
		   (seq2 ((((3 4) (e.) s { 3 te te te } +q)
			   ({ 3 +te (te) te } e e (q)))
			  :pitch-seq-palette (2 3 4 5 6 7 8)))
		   (seq3 ((((2 4) e e { 3 te te te })
			   ((5 8) (e) e e e s s))
			  :pitch-seq-palette (3 4 5 6 7 8 9 10 1 2)))))))
  (loop repeat 2
       do (get-next (pitch-seq-palette (first (data mrsp)))))
  (print (data (get-next (pitch-seq-palette (first (data mrsp))))))
  (reset-psps mrsp)
  (print (data (get-next (pitch-seq-palette (first (data mrsp)))))))

=>
(1 4 2 6 3 7 5) 
(1 2 3 4 5 6 7)

|#
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

;;; SAR Sat Jan 28 15:42:27 GMT 2012: Added robodoc info

;;; ****m* rthm-seq-palette/scale
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
(defmethod scale ((rsp rthm-seq-palette) scaler
                  &optional ignore1 ignore2 ignore3)
;;; ****
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
                         ;; MDE Fri Jan 13 19:58:53 2012 -- no accidentals!
                         &optional (no-accidentals nil) 
                         ignore2 ignore3 ignore4 ignore5 ignore6 ignore7 ignore8)
  (declare (ignore ignore2 ignore3 ignore4 ignore5 ignore6 ignore7
                   ignore8))
  (loop 
      for rs in (data rsp) 
      if (is-ral (data rs))
     ;; MDE Fri Jan 13 20:01:07 2012 -- no-accidentals!
      append (get-cmn-data (data rs) no-accidentals)
      else collect (econs (get-cmn-data rs no-accidentals) 
                          (cmn::line-mark))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+cmn
;;; ****m* rthm-seq-palette/cmn-display
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
(defmethod cmn-display ((rsp rthm-seq-palette)
                        &key
                        (all-output-in-one-file t)
                        (file "/tmp/cmn.eps")
                        (staff-separation 3)
                        (line-separation 5)
                        (page-nums t)
                        (no-accidentals t)
                        (seqs-per-system 1)
                        (size 15))
;;; ****
  (format t "~&Generating rthm-seqs...")
  ;; MDE Fri Jan 13 20:01:58 2012 -- no accidentals
  (let* ((cmn-data (get-cmn-data rsp no-accidentals)) 
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

;;; ****m* rthm-seq-palette/chop
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
(defmethod chop ((rsp rthm-seq-palette) &optional chop-points 
                                                  (unit 's)
                                                  (number-bars-first t))
;;; ****
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
;;; see rthm-seq method
;;; ****m* rthm-seq-palette/get-multipliers
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
(defmethod get-multipliers ((rsp rthm-seq-palette) rthm &optional id round)
;;; ****
  (get-multipliers (get-data id rsp) rthm round))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Jan 28 11:18:46 GMT 2012: Added robodoc info

;;; ****f* rthm-seq-palette/make-rsp
;;; FUNCTION
;;; Create a rthm-seq-palette object.
;;; 
;;; ARGUMENTS
;;; - A symbol that is to be the ID of the rhtm-seq-palette object created.
;;; - A list containing rhtm-seq data to be made into rthm-seqs. Each item in
;;;   this list is a list of data formatted as it would be when passed to the
;;;   make-rthm-seq function.
;;; 
;;; OPTIONAL ARGUMENTS
;;; T or NIL to indicate whether to automatically generate and store inversions
;;; of the pitch-seq-palette passed to the rthm-seq objects in the
;;; rthm-seq-palette object created. T = generate and store. Default = NIL.
;;; 
;;; RETURN VALUE
;;; A rthm-seq-palette object.
;;; 
;;; EXAMPLE
#|

(make-rsp 'rsp-test '((seq1 ((((2 4) q +e. s)
			      ((s) e (s) q)
			      (+e. s { 3 (te) te te } ))
			     :pitch-seq-palette (1 7 3 4 5 2 6)))
		      (seq2 ((((3 4) (e.) s { 3 te te te } +q)
			      ({ 3 +te (te) te } e e (q)))
			     :pitch-seq-palette (3 1 2 5 1 7 6)))
		      (seq3 ((((2 4) e e { 3 te te te })
			      ((5 8) (e) e e e s s))
			     :pitch-seq-palette (4 4 4 5 4 4 4 5 4 3)))))

=> 
RTHM-SEQ-PALETTE: psp-inversions: NIL
PALETTE: 
RECURSIVE-ASSOC-LIST: recurse-simple-data: T
                      num-data: 3
                      linked: T
                      full-ref: NIL
ASSOC-LIST: warn-not-found T
CIRCULAR-SCLIST: current 0
SCLIST: sclist-length: 3, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: RSP-TEST, tag: NIL, 
data: (
RTHM-SEQ: num-bars: 3
[...]

;; Create two rthm-seq-palette objects, one with :psp-inversions set to NIL and 
;; one with it set to T, and print the DATA of the pitch-seq-palettes of each 
(let ((mrsp1 (make-rsp 'rsp-test 
		       '((seq1 ((((2 4) q +e. s)
				 ((s) e (s) q)
				 (+e. s { 3 (te) te te } ))
				:pitch-seq-palette (1 7 3 4 5 2 6))))
		       :psp-inversions nil))
      (mrsp2 (make-rsp 'rsp-test 
		       '((seq1 ((((2 4) q +e. s)
				 ((s) e (s) q)
				 (+e. s { 3 (te) te te } ))
				:pitch-seq-palette (1 7 3 4 5 2 6))))
		       :psp-inversions t)))
  (print 
   (loop for i in (data (pitch-seq-palette (first (data mrsp1))))
      collect (data i)))
  (print 
   (loop for i in (data (pitch-seq-palette (first (data mrsp2))))
      collect (data i))))

=>
((1 7 3 4 5 2 6)) 
((1 7 3 4 5 2 6) (7 1 5 4 3 6 2))

|#
;;; SYNOPSIS
(defun make-rsp (id data &key (psp-inversions nil))
;;; ****
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF rthm-seq-palette.lsp

