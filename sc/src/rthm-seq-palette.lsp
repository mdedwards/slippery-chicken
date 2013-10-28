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
;;; Version:          1.0.4
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Implementation of the rthm-seq-palette class.
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    19th February 2001
;;;
;;; $$ Last modified: 19:32:09 Mon Oct 28 2013 GMT
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
;;; DESCRIPTION
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
;;;
;;; OPTIONAL ARGUMENTS
;;; keyword arguments
;;; - :selection-fun. This is a function that will return the pitch-seq
;;;   numbers. It takes two arguments only: 1) the number of notes needed, and
;;;   2) the pitch-seq data lists. The function also needs to be able to handle
;;;   being passed NIL NIL as arguments. In this case it should reset, if needs
;;;   be; i.e. it's just a call to init and should return nothing. Default =
;;;   #'create-psps-default.
;;; - :pitch-seqs-per-rthm-seq. This is an integer that is the number of
;;;   pitch-seqs each rthm-seq should have. NB: The method will simply cycle
;;;   through the pitch-seqs given in the selection function to create the
;;;   required number.  Default = 3.
;;; - :selection-fun-data. This contains the pitch-seq lists to be passed to
;;;   the default selection function. There can be as many pitch-seqs in these
;;;   lists as desired. The number of notes the pitch-seq will provide is the
;;;   first item of the list. They need not be in ascending order. When this
;;;   argument is passed a value of T, the selection function will reinitialize
;;;   its default data and use that.
;;; - :reinit. Used internally.  Do not change.
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
;; see the change.  NB You wouldn't normally specify pitch-seq-palettes in your
;; rthm-seq-palette as the whole point of this method is to have them created
;; algorithmically, but they are given here for purposes of comparison.
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
  (print 
   (loop for rs in (data mrsp)
      collect 
        (loop for ps in (data (pitch-seq-palette rs))
           collect (data ps))))
  (create-psps mrsp)
  (print 
   (loop for rs in (data mrsp)
      collect 
        (loop for ps in (data (pitch-seq-palette rs))
                 collect (data ps)))))

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
                                (+e. s { 3 (te) te te } ))))
                        (seq2 ((((3 4) (e.) s { 3 te te te } +q)
                                ({ 3 +te (te) te } e e (q)))))
                        (seq3 ((((2 4) e e { 3 te te te })
                                ((5 8) (e) e e e s s))))))))
  (create-psps mrsp :pitch-seqs-per-rthm-seq 5)
  (loop for rs in (data mrsp)
            collect 
              (loop for ps in (data (pitch-seq-palette rs))
                 collect (data ps))))

=>
(((8 8 8 5 9 6 9) (9 3 8 4 7 5 4) (3 4 3 5 3 4 3) (8 8 8 5 9 6 9)
  (9 3 8 4 7 5 4))
 ((3 4 3 5 3 4 3) (8 8 8 5 9 6 9) (9 3 8 4 7 5 4) (3 4 3 5 3 4 3)
  (8 8 8 5 9 6 9))
 ((9 9 9 3 9 9 3 5 9 5) (8 9 8 9 5 9 9 5 6 6) (9 9 9 3 9 9 3 5 9 5)
  (8 9 8 9 5 9 9 5 6 6) (9 9 9 3 9 9 3 5 9 5)))

;;; Now an example with our own selection-fun creating random pitch-seqs for
;;; demo purposes only: 
(let ((mrsp (make-rsp 'rsp-test 
                      '((seq1 ((((2 4) q +e. s)
                                ((s) e (s) q)
                                (+e. s { 3 (te) te te }))))
                        (seq2 ((((3 4) (e.) s { 3 te te te } +q)
                                ({ 3 +te (te) te } e e (q)))))
                        (seq3 ((((2 4) e e { 3 te te te })
                                ((5 8) (e) e e e s s))))))))
  (create-psps 
   mrsp 
   :selection-fun #'(lambda
                        (num-notes data-lists)
                      ;; NB we're not doing anything with data-lists here
                      (loop repeat num-notes collect (random 10))))
   (loop for rs in (data mrsp)
      collect 
        (loop for ps in (data (pitch-seq-palette rs))
                 collect (data ps))))
=>
(((5 4 3 0 3 8 0) (1 2 8 5 3 7 8) (5 5 7 3 9 1 7))
 ((3 1 5 6 6 7 1) (7 7 8 5 5 2 4) (9 1 3 0 8 7 8))
 ((4 8 6 9 6 6 0 8 1 2) (1 5 5 7 7 2 9 3 1 2) (1 5 6 2 5 3 7 3 4 2)))


|#
;;; 
;;; SYNOPSIS
(defmethod create-psps ((rsp rthm-seq-palette) 
                        &key
                        (selection-fun #'create-psps-default)
                        (selection-fun-data nil)
                        ;; MDE Sat Jul 14 17:20:27 2012 -- 
                        (reinit t)
                        (pitch-seqs-per-rthm-seq 3))
;;; ****
  ;; 3.2.11: got to reinitialize our cscls so make this reset call here;
  ;; create-psps-default or the user-supplied selection-fun needs to be able to
  ;; handle this.  
  ;; MDE Sat Jul 14 17:19:51 2012 -- only reinit once, i.e. not on recursive
  ;; calls.   
  ;; (format t "~&create-psps ~a" (id rsp))
  (when reinit
    (create-psps-default nil nil)) ; see pitch-seq-palette.lsp
  (loop with pass-data = t
     for rs in (data rsp) do
       (if (rsp-p (data rs))
           (create-psps (data rs) :selection-fun selection-fun 
                        :pitch-seqs-per-rthm-seq pitch-seqs-per-rthm-seq
                        :reinit nil
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
;;; DESCRIPTION
;;; Call the reset method (inherited from circular-sclist) for all
;;; pitch-seq-palette objects of all rthm-seq objects in the given
;;; rthm-seq-palette object, resetting their pointers to the head of the
;;; sequence. This ensures that each rthm-seq starts over again at the first
;;; note of the first given pitch-seq.
;;;
;;; NB Since 28/10/13 there's also a reset method for the palette class which
;;; will mean we call the reset method of all pitch-seqs here.  So this method
;;; is now partially obsolete (but still perhaps useful in that in only resets
;;; the psps, not the whole palette).
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

;;; SAR Mon Jan 30 20:19:39 GMT 2012: Added robodoc info

;;; ****m* rthm-seq-palette/scale
;;; DESCRIPTION
;;; Scale the durations of the rhythm objects in a given rthm-seq-palette
;;; object by the specified factor.
;;;
;;; NB: As is evident in the examples below, this method does not replace the
;;;     original data in the rthm-seq-palette object's DATA slot.
;;; 
;;; ARGUMENTS
;;; - A rthm-seq-palette object.
;;; - A real number that is the scaling factor.
;;; 
;;; OPTIONAL ARGUMENTS
;;; (- the three IGNORE arguments are for internal purposes only).
;;; 
;;; RETURN VALUE
;;; Returns a rthm-seq-palette object.
;;; 
;;; EXAMPLE

#|

;; Returns a rthm-seq-palette object
(let ((mrsp
       (make-rsp 'rsp-test 
                 '((seq1 ((((2 4) q +e. s)
                           ((s) e (s) q)
                           (+e. s { 3 (te) te te } ))
                          :pitch-seq-palette ((1 2 3 4 5 6 7)
                                              (1 3 5 7 2 4 6)
                                              (1 4 2 6 3 7 5)
                                              (1 5 2 7 3 2 4))))
                   (seq2 ((((4 4) (e.) s { 3 te te te } +h)
                           ({ 3 +te (te) te } e e (h)))
                          :pitch-seq-palette (2 3 4 5 6 7 8)))
                   (seq3 ((((2 4) e e { 3 te te te })
                           ((4 4) (e) e e e s s (s) s q))
                          :pitch-seq-palette (3 4 5 6 7 8 9 10 1 2 3 7)))))))
   (scale mrsp 2))

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

;; Apply the method and loop through the rthm-seq objects in the
;; rthm-seq-palette object's DATA slot, using the print-simple method to see
;; the changes

(let ((mrsp
       (make-rsp 'rsp-test 
                 '((seq1 ((((2 4) q +e. s)
                           ((s) e (s) q)
                           (+e. s { 3 (te) te te } ))
                          :pitch-seq-palette ((1 2 3 4 5 6 7)
                                              (1 3 5 7 2 4 6)
                                              (1 4 2 6 3 7 5)
                                              (1 5 2 7 3 2 4))))
                   (seq2 ((((4 4) (e.) s { 3 te te te } +h)
                           ({ 3 +te (te) te } e e (h)))
                          :pitch-seq-palette (2 3 4 5 6 7 8)))
                   (seq3 ((((2 4) e e { 3 te te te })
                           ((4 4) (e) e e e s s (s) s q))
                          :pitch-seq-palette (3 4 5 6 7 8 9 10 1 2 3 7)))))))
   (scale mrsp .5)
   (print-simple mrsp))

=>
rthm-seq-palette RSP-TEST
rthm-seq SEQ1
(2 8): note E, note S., note 32, 
(2 8): rest 32, note S, rest 32, note E, 
(2 8): note S., note 32, rest TS, note TS, note TS, 
rthm-seq SEQ2
(4 8): rest S., note 32, note TS, note TS, note TS, note Q, 
(4 8): note TS, rest TS, note TS, note S, note S, rest Q, 
rthm-seq SEQ3
(2 8): note S, note S, note TS, note TS, note TS, 
(4 8): rest S, note S, note S, note S, note 32, note 32, rest 32, note 32, note E,

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
                         ignore2 ignore3 ignore4 ignore5 ignore6 ignore7
                         ignore8)
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

;;; SAR Mon Jan 30 21:15:10 GMT 2012: Added robodoc info

#+cmn
;;; ****m* rthm-seq-palette/cmn-display
;;; DESCRIPTION
;;; Generate printable music notation output (.EPS) of the given
;;; rthm-seq-palette object using the Common Music Notation (CMN)
;;; interface. The method requires at least the name of the given
;;; rthm-seq-palette object to set, but has several additional optional
;;; arguments for customizing output.
;;;
;;; NB: Most of the keyword arguments are CMN attributes and share the same
;;;     name as the CMN feature they effect.
;;; 
;;; ARGUMENTS
;;; - A rthm-seq-palette object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :all-output-in-one-file. T or NIL to indicate whether to write the output
;;;   to a multi-page file or to separate files for each page.  
;;;   T = one multi-page file. Default = T. This is a direct CMN attribute.
;;; - :file. The file path, including the file name, of the file to be
;;;   generated.
;;; - :staff-separation. A number to indicate the amount of white space between
;;;   staves belong to the same system, measured as a factor of the staff
;;;   height. Default = 3. This is a direct CMN attribute.
;;; - :line-separation. A number to indicate the amount of white space between
;;;   lines of music (systems), measured as a factor of the staff
;;;   height. Default = 5. This is a direct CMN attribute.
;;; - :page-nums. T or NIL to indicate whether or not to print page numbers on
;;;   the pages. T = print page numbers. Default = T.
;;; - :no-accidentals. T or NIL to indicate whether or not to supress printing
;;;   accidentals for each and every note (rather than once per bar). 
;;;   T = supress printing all accidentals. Default = NIL.
;;; - :seqs-per-system. An integer indicating the number of rthm-seq objects to
;;;   be printed in one staff system. Default = 1.
;;; - :size. A number to indicate the font size of the CMN output.
;;; - :auto-open. Automatically open the EPS file?.  
;;;    Default = (get-sc-config cmn-display-auto-open)
;;;
;;; RETURN VALUE
;;; T
;;; 
;;; EXAMPLE
#|
;; A typical example with some specified keyword values for file and size
(let ((mrsp
       (make-rsp 'rsp-test 
                 '((seq1 ((((2 4) q +e. s)
                           ((s) e (s) q)
                           (+e. s { 3 (te) te te } ))
                          :pitch-seq-palette ((1 2 3 4 5 6 7)
                                              (1 3 5 7 2 4 6)
                                              (1 4 2 6 3 7 5)
                                              (1 5 2 7 3 2 4))))
                   (seq2 ((((4 4) (e.) s { 3 te te te } +h)
                           ({ 3 +te (te) te } e e (h)))
                          :pitch-seq-palette (2 3 4 5 6 7 8)))
                   (seq3 ((((2 4) e e { 3 te te te })
                           ((4 4) (e) e e e s s (s) s q))
                          :pitch-seq-palette (3 4 5 6 7 8 9 10 1 2 3 7)))))))
  (cmn-display mrsp
               :file "/tmp/rmsp-output.eps"
               :size 10))

|#
;;; SYNOPSIS
(defmethod cmn-display ((rsp rthm-seq-palette)
                        &key
                        (all-output-in-one-file t)
                        (file 
                         (format nil "~a~a.eps" 
                                 (get-sc-config 'default-dir)
                                 (string-downcase (id rsp))))
                        (staff-separation 3)
                        (line-separation 5)
                        (page-nums t)
                        (no-accidentals t)
                        (seqs-per-system 1)
                        (auto-open (get-sc-config 'cmn-display-auto-open))
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
     :staff-separation staff-separation)
    ;; MDE Wed May 29 14:51:27 2013 
    (when auto-open
      (system-open-file file))
    t))
     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Tue Jan 31 13:52:23 GMT 2012: Deleted MDE's comment here as it is taken
;;; nearly verbatim into the robodoc info
;;; SAR Tue Jan 31 13:51:57 GMT 2012: Added robodoc info

;;; ****m* rthm-seq-palette/chop
;;; DESCRIPTION
;;; Applies the chop method to each rthm-seq object in the given
;;; rthm-seq-palette object (see rthm-seq-bar::chop for details). Returns a
;;; new rthm-seq-palette object with the same structure as the argument, but
;;; with a further level of nesting: Each rthm-seq object in the argument is
;;; replaced by a list of rthm-seq objects that are each one "slice" of the
;;; original rthm-seq objects.
;;;
;;; The chop method is the basis for slippery-chicken's feature of
;;; intra-phrasal looping.
;;;
;;; NB: Since the chop method functions by comparing each beat of a given
;;;     rthm-seq-bar object to the specified <chop-points> pattern for
;;;     segmenting that beat, all rthm-seq-bar objects in the given
;;;     rthm-seq-palette object must be evenly divisible by the beat for which
;;;     the pattern is defined. For example, if the <chop-points> argument
;;;     defines a quarter-note, all bars in the given rthm-seq-palette object
;;;     must be evenly divisible by a quarter-note, and a rthm-seq-palette
;;;     consisting of a rthm-seq object with a 2/4, a 3/4 and a 3/8 bar would
;;;     fail at the 3/8 bar with an error.
;;;
;;; NB: The <unit> argument must be a duplet rhythmic value (i.e. 32, 's, 'e
;;;     etc.) and cannot be a tuplet value (i.e. 'te 'fe etc.). 
;;;
;;; NB: In order for the resulting chopped rhythms to be parsable by LilyPond
;;;     and CMN, there can be no tuplets (triplets etc.) among the rhythms to
;;;     be chopped. Such rhythms will result in LilyPond and CMN errors. This
;;;     has only minimal bearing on any MIDI files produced, however, and these
;;;     can potentially be imported into notation software.
;;; 
;;; ARGUMENTS
;;; - A rthm-seq-palette object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - <chop-points>. A list of integer pairs, each of which delineates a
;;;   segment of the beat of the given rthm-seq-bar objects within the given
;;;   rthm-seq-palette object, measured in the rhythmic unit specified by the
;;;   <unit> argument. See the documentation for rthm-seq-bar::chop for more
;;;   details.
;;; - <unit>. The rhythmic duration that serves as the unit of measurement for
;;;   the chop points. Default = 's.
;;; - <number-bars-first>. T or NIL. This argument helps in naming (and
;;;   therefore debugging) the newly-created bars.  If T, the bars in the
;;;   original rthm-seq will be renumbered, starting from 1, and this will be
;;;   reflected in the tag of the new bars.  E.g. if T, a new bar's tag may be
;;;   new-bar-from-rs1-b3-time-range-1.750-to-2.000, if NIL this would be
;;;   new-bar-from-rs1-time-range-1.750-to-2.000. Default = T.
;;; 
;;; RETURN VALUE
;;; A rthm-seq-palette with the same top-level structure of the first argument,
;;; but each ID now referencing a sub-rthm-seq-palette with sequentially
;;; numbered rthm-seqs for each of the chopped results.
;;; 
;;; EXAMPLE
#|
;;; Create a rthm-seq-palette object, chop it with user-defined chop-points and
;;; a <unit> value of 'e, and print-simple the results
(let* ((rsp-orig (make-rsp
                  'sl-rsp
                  '((1 
                     ((((2 4) (e) e (e) e)) 
                      :pitch-seq-palette (1 8)))
                    (2 
                     ((((2 4) (s) e s e. (s))) 
                      :pitch-seq-palette (3 5 7)))
                    (3
                     ((((3 4) q +s e. +q)) 
                      :pitch-seq-palette (1 7))))))
       (rsp-chopped (chop rsp-orig
                          '((1 1) (1 2) (2 2))
                          'e)))
  (print-simple rsp-chopped))

=>
rthm-seq-palette SL-RSP
rthm-seq-palette 1
rthm-seq 1
(1 8): rest 8, 
rthm-seq 2
(1 4): rest E, NIL E, 
rthm-seq 3
(1 8): NIL E, 
rthm-seq 4
(1 8): rest 8, 
rthm-seq 5
(1 4): rest E, NIL E, 
rthm-seq 6
(1 8): NIL E, 
rthm-seq 1
(1 8): rest S, NIL S, 
rthm-seq 2
(1 4): rest S, NIL E, NIL S, 
rthm-seq 3
(1 8): rest S, NIL S, 
rthm-seq 4
(1 8): NIL E, 
rthm-seq 5
(1 4): NIL E., rest S, 
rthm-seq 6
(1 8): rest 8, 
rthm-seq 1
(1 8): NIL E, 
rthm-seq 2
(1 4): NIL Q, 
rthm-seq 3
(1 8): rest 8, 
rthm-seq 4
(1 8): rest S, NIL S, 
rthm-seq 5
(1 4): rest S, NIL E., 
rthm-seq 6
(1 8): rest 8, 
rthm-seq 7
(1 8): rest 8, 
rthm-seq 8
(1 4): rest 4, 
rthm-seq 9
(1 8): rest 8,

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

;;; MDE Mon Feb 20 18:50:26 2012 
(defmethod print-simple ((rsp rthm-seq-palette) &optional (stream t) ignore)
  (declare (ignore ignore))
  (format stream "~&rthm-seq-palette ~a" (id rsp))
  (loop for object in (data rsp) 
     for data = (data object)
     do
       (if (is-ral data)
           (print-simple data)
           (print-simple object)))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* rthm-seq-palette/rsp-subseq
;;; DATE
;;; 23rd October 2013
;;; 
;;; DESCRIPTION
;;; (Recursively) change all the rthm-seq objects in the palette to be a
;;; subsequence of the existing rthm-seqs.
;;; 
;;; ARGUMENTS
;;; - the original rthm-seq-palette object
;;; - the start bar (1-based)
;;; 
;;; OPTIONAL ARGUMENTS
;;; - the end bar (1-based and (unlike Lisp's subseq function) inclusive).  If
;;;   NIL, we'll use the original end bar of each rthm-seq. Default = NIL.
;;; 
;;; RETURN VALUE
;;; The original rthm-seq-palette but with the new rthm-seqs
;;;
;;; SYNOPSIS
(defmethod rsp-subseq ((rsp rthm-seq-palette) start &optional end)
;;; ****
  ;; to avoid the parsing in ral-to-rsp when using the setf method
  (setf (slot-value rsp 'data) 
        (loop for object in (data rsp)
           for data = (data object)
           collect 
           (cond ((is-ral data)
                  (setf (slot-value object 'data) (rsp-subseq data start end))
                  object)
                 ((rthm-seq-p object) 
                  (let ((id (id object))
                        (rs (rs-subseq object start end)))
                    ;; remember: the id of the rthm-seq is changed to reflect
                    ;; the bars it was extracted from, so rename to original
                    (setf (id rs) id)
                    rs))
                 (t (error "rthm-seq-palette::rs-subseq: unexpected object: ~a"
                           object)))))
  rsp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Thu Feb  2 14:30:02 GMT 2012: Added robodoc entry
 
;;; ****m* rthm-seq-palette/get-multipliers
;;; DESCRIPTION
;;; Get a list of factors by which a specified rhythmic unit must be multiplied
;;; in order to create the rhythms of a specified rthm-seq object within the
;;; given rthm-seq-palette object.
;;;
;;; See also rthm-seq method for more information.
;;;
;;; ARGUMENTS
;;; - A rthm-seq object.
;;; - A rhythm unit, either as a number of a CMN shorthand symbol (i.e. 'e)
;;; - A symbol that is the ID of the rthm-seq-object for which the multipliers
;;;   is sought is also a required argument (though it is listed as an optional
;;;   argument for internal reasons).
;;; 
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether to round the results. T = round. 
;;;   Default = NIL. NB: Lisp always rounds to even numbers, meaning x.5 may
;;;   sometimes round up and sometimes round down; thus (round 1.5) => 2, and
;;;   (round 2.5) => 2.
;;; 
;;; RETURN VALUE
;;; A list of numbers.
;;; 
;;; EXAMPLE
#|
;; Returns a list of numbers, by default not rounded
(let ((mrsp
       (make-rsp 'rsp-test 
                 '((seq1 ((((2 4) q +e. s)
                           ((s) e (s) q)
                           (+e. s { 3 (te) te te } ))
                          :pitch-seq-palette ((1 2 3 4 5 6 7)
                                              (1 3 5 7 2 4 6)
                                              (1 4 2 6 3 7 5)
                                              (1 5 2 7 3 2 4))))
                   (seq2 ((((4 4) (e.) s { 3 te te te } +h)
                           ({ 3 +te (te) te } e e (h)))
                          :pitch-seq-palette (2 3 4 5 6 7 8)))
                   (seq3 ((((2 4) e e { 3 te te te })
                           ((4 4) (e) e e e s s (s) s q))
                          :pitch-seq-palette (3 4 5 6 7 8 9 10 1 2 3 7)))))))
  (get-multipliers mrsp 'e 'seq1))

=> (2.0 1.0 1.5 2.0 1.1666666666666665 0.6666666666666666 0.6666666666666666)

;; Setting the option <round> argument to T returns rounded results
(let ((mrsp
       (make-rsp 'rsp-test 
                 '((seq1 ((((2 4) q +e. s)
                           ((s) e (s) q)
                           (+e. s { 3 (te) te te } ))
                          :pitch-seq-palette ((1 2 3 4 5 6 7)
                                              (1 3 5 7 2 4 6)
                                              (1 4 2 6 3 7 5)
                                              (1 5 2 7 3 2 4))))
                   (seq2 ((((4 4) (e.) s { 3 te te te } +h)
                           ({ 3 +te (te) te } e e (h)))
                          :pitch-seq-palette (2 3 4 5 6 7 8)))
                   (seq3 ((((2 4) e e { 3 te te te })
                           ((4 4) (e) e e e s s (s) s q))
                          :pitch-seq-palette (3 4 5 6 7 8 9 10 1 2 3 7)))))))
  (get-multipliers mrsp 'e 'seq1 t))

=> (2 1 2 2 1 1 1)

;; The ID argument is required, even though it's listed as being optional. The
;; method interrupts with an error if no ID is supplied
(let ((mrsp
       (make-rsp 'rsp-test 
                 '((seq1 ((((2 4) q +e. s)
                           ((s) e (s) q)
                           (+e. s { 3 (te) te te } ))
                          :pitch-seq-palette ((1 2 3 4 5 6 7)
                                              (1 3 5 7 2 4 6)
                                              (1 4 2 6 3 7 5)
                                              (1 5 2 7 3 2 4))))
                   (seq2 ((((4 4) (e.) s { 3 te te te } +h)
                           ({ 3 +te (te) te } e e (h)))
                          :pitch-seq-palette (2 3 4 5 6 7 8)))
                   (seq3 ((((2 4) e e { 3 te te te })
                           ((4 4) (e) e e e s s (s) s q))
                          :pitch-seq-palette (3 4 5 6 7 8 9 10 1 2 3 7)))))))
  (get-multipliers mrsp 'e))

=>
rthm-seq-palette::get-multipliers: third argument (rthm-seq ID) is required.
   [Condition of type SIMPLE-ERROR]

;;; Applying the method to the a multiple-bar rthm-seq object may return
;;; different results than applying the method to each of the bars contained
;;; within that rthm-seq object as individual one-bar rthm-seq objects, as the
;;; method measures the distances between attacked notes, regardless of ties
;;; and rests.
(let ((rs1 (make-rthm-seq '(seq1 ((((2 4) q +e. s))
                                  :pitch-seq-palette ((1 2))))))
      (rs2 (make-rthm-seq '(seq2 ((((2 4) (s) e (s) q))
                                  :pitch-seq-palette ((1 2))))))
      (rs3 (make-rthm-seq '(seq3 ((((2 4) +e. s { 3 (te) te te } ))
                                  :pitch-seq-palette ((1 2 3))))))
      (rs4 (make-rthm-seq '(seq4 ((((2 4) q +e. s)
                                   ((s) e (s) q)
                                   (+e. s { 3 (te) te te } ))
                                  :pitch-seq-palette ((1 2 3 4 5 6 7)))))))
  (print (get-multipliers rs1 'e))
  (print (get-multipliers rs2 'e))
  (print (get-multipliers rs3 'e))
  (print (get-multipliers rs4 'e)))

=>
(3.5 0.5) 
(1.5 2.0) 
(1.1666666666666665 0.6666666666666666 0.6666666666666666) 
(3.5 1.0 1.5 3.5 1.1666666666666665 0.6666666666666666 0.6666666666666666)

|#
;;; SYNOPSIS
(defmethod get-multipliers ((rsp rthm-seq-palette) rthm &optional id round)
;;; ****
  (unless id
    (error "rthm-seq-palette::get-multipliers: third argument (rthm-seq ID) ~
            is required."))
  (get-multipliers (get-data id rsp) rthm round))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 14.8.13
(defmethod rsm-from-chopped-rsp ((rsp rthm-seq-palette) num-output-seqs players
                             &key (start 1) stop 
                             (remix-in t) 
                             ;; remix-in args: ;
                             (remix-in-fib-seed 13) (mirror nil) (test #'eql)
                             (replace nil))
  (let* ((rsm-keys (get-keys rsp nil))  ; no warning about top-level keys only ;
         (num-chops (sclist-length (data (first (data rsp)))))
         (fts (fibonacci-transitions num-output-seqs
                                     (loop for i from start to 
                                          (if (integerp stop) stop num-chops)
                                          collect i)))
         (map '()))
    ;; make sure we've got enough chopped sequences for the number of players
    (unless (<= (length players) (length rsm-keys))
      (error "Not enough original sequences (~a) to match the number ~
              of players (~a)" (length rsm-keys) (length players)))
    (when remix-in
      (setf fts (remix-in fts :remix-in-fib-seed remix-in-fib-seed
                          :mirror mirror
                          :test test :replace replace)))
    (setf map
          (list 
           (list 1
                 (loop for p in players 
                    for k in rsm-keys
                    collect
                    (list p (loop for rs in fts collect (list k rs)))))))
    (make-rthm-seq-map 
     (concatenate 'string "from-" (string (id rsp)))
     map :recurse-simple-data nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* rthm-seq-palette/split-into-single-bars
;;; DESCRIPTION
;;; Split every rthm-seq in a palette into as many single-bar rthm-seqs as
;;; there are bars.  This creates an extra level of recursion so that whereas a
;;; rthm-seq that was referenced by e.g. '(long 1 a) beforehand, afterwards
;;; each bar will be reference by '(long 1 a 1), '(long 1 a 2) etc. This is
;;; true even for sequences that only contained one bar before processing.  The
;;; pitch-seq-palettes of the original rthm-seqs will be used to set the
;;; pitch-seqs of the new rthm-seqs.
;;; 
;;; ARGUMENTS
;;; - the rthm-seq-palette object
;;; 
;;; OPTIONAL ARGUMENTS
;;; - whether to clone the palette before processing (T or NIL).  Default = T.
;;; 
;;; RETURN VALUE
;;; A rthm-seq-palette object with extra layers of recursion.
;;; 
;;; SYNOPSIS
(defmethod split-into-single-bars ((rsp rthm-seq-palette) &optional (clone t))
;;; ****
  (when clone
    (setf rsp (clone rsp)))
  (let ((refs (get-all-refs rsp)))
    (loop for ref in refs for rs = (get-data ref rsp) do
         (set-data ref (make-rsp (id rs) (split-into-single-bars rs)) rsp)))
  rsp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Jan 28 11:18:46 GMT 2012: Added robodoc info

;;; ****f* rthm-seq-palette/make-rsp
;;; DESCRIPTION
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

