;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* linked-named-object/rhythm
;;; NAME 
;;; rhythm
;;;
;;; File:             rhythm.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> rhythm
;;;
;;; Version:          1.0.5
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Implementation of the rhythm class for parsing and
;;;                   storing the properties of rhythms.  
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    11th February 2001
;;;
;;; $$ Last modified: 13:50:38 Sun Jun 28 2015 BST
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

;;; NB When rhythms are given as numbers, they can still be dotted by putting a
;;; slash before the dot, e.g. 16/.

(defclass rhythm (linked-named-object)
  ;; the data and id slots of the named-object class stores the given rhythm
  ;; symbol (or number).  value is the duration relative to a whole note,
  ;; e.g. q = 4, for SCORE purposes.
  ((value :accessor value :type number :initform -1)
   ;; the duration in crotchets, or seconds, assuming a default tempo of qtr=60
   ;; 5.8.10 NB this is not related to any tempi applied, rather that is
   ;; reflected in the duration-in-tempo slot of event
   ;; 5.8.10 make duration an init'able field so we can make rhythms without
   ;; letter equivalents
   (duration :accessor duration :type number :initform -1 :initarg :duration)
   ;; the ratio to 1/4 notes for cmn, if cmn package exists, then the ratio
   ;; passable to the rq function
   ;; when compound rhythms are given (e.g. q+s.), the value of all the rhythms
   ;; together; this will only be set for the first of the compound, the others
   ;; will be nil.  When rthm-seqs are made, this slot is calculated.  This is
   ;; still with reference to qtr=60.  
   (compound-duration :accessor compound-duration :type number :initform -1)
   ;; rq is in relation to quarter notes so rq of q is 1, e is 1/2, w is 4 etc.
   (rq :accessor rq :initform nil)
   (undotted-value :accessor undotted-value :type number :initform -1.0)
   (num-dots :accessor num-dots :type integer :initform 0)
   (num-flags :accessor num-flags :type integer :initform 0)
   (score-rthm :accessor score-rthm :initform nil)
   ;; whether this rhythm is a rest i.e. was given in parentheses
   (is-rest :accessor is-rest :type boolean :initarg :is-rest :initform nil)
   ;; whether this is simply a whole bar rest or not.
   (is-whole-bar-rest :accessor is-whole-bar-rest :type boolean :initform nil)
   (is-tied-to :accessor is-tied-to :type boolean :initarg :is-tied-to
               :initform nil)
   ;;; either t, nil or a number indicating curvature (< 0 tie down, > 0 up)
   (is-tied-from :accessor is-tied-from :initarg :is-tied-from
                 :initform nil)
   ;; whether this rhythm is a grace note, symbol g, whose duration will then
   ;; be set to grace-note-duration
   (is-grace-note :accessor is-grace-note :type boolean :initform nil)
   (needs-new-note :accessor needs-new-note :type boolean :initform t)
   ;; if this rhythm starts beaming, beam = 1, if it ends beaming, beam = 0,
   ;; otherwise nil.
   (beam :accessor beam :initarg :beam :initform nil)
   ;; bracket info is stored in a list.  If an element(s) of a list is another
   ;; (2-element) list, this means a bracket starts; the first element of this
   ;; list is the index into the list of current cmn brackets, the second
   ;; element is the tuplet number; if the element(s) is a number, then this is
   ;; the reference to the current cmn bracket that should be closed.  This is
   ;; all a little dirty on my part here, but cmn also requires a
   ;; -beat-subdivision- to set which notes are under the bracket as well, so
   ;; add an element which is a negative number which when made positive is an
   ;; index into the current cmn brackets.  This means we never store 0 as the
   ;; index into the cmn list, rather we start at 1.
   (bracket :accessor bracket :type list :initarg :bracket :initform nil)
   ;; when we give rhythms in the form of a cmn rqq list, then this is one of
   ;; the notes returned by rqq  
   (rqq-note :accessor rqq-note :initarg :rqq-note :initform nil)
   (rqq-info :accessor rqq-info :initarg :rqq-info :initform nil)
   ;; dynamics, accents etc. exactly the code used by cmn.  Used for note or
   ;; chord as a whole (individual pitches of a chord can also have their own
   ;; marks--see pitch.lsp)  
   (marks :accessor marks :type list :initarg :marks 
              :initform nil)
   (marks-in-part :accessor marks-in-part :type list 
                      :initarg :marks-in-part :initform nil)
   ;; MDE Thu Aug 22 18:12:55 2013 -- what's the nth position of this r in the
   ;; bar? 
   (bar-pos :accessor bar-pos :type integer :initform -1)
   ;; 30.1.11 add another couple of slots for lilypond
   (letter-value :accessor letter-value :initform -1)
   ;; this is the overall scaler that a rhythm might have, whether under nested
   ;; tuplets or not, i.e. it's the ratio of the letter-value to the value
   ;; slots. So e.g. if we had a triplet 8th under a nested triplet we'd notate
   ;; them as 8ths although they're 18ths actuall, so closer to
   ;; 16ths--i.e. we'll have one beam, not two, despite the value. This would
   ;; mean a tuplet scaler of 4/9.
   (tuplet-scaler :accessor tuplet-scaler :type rational :initform 1)
   (grace-note-duration :accessor grace-note-duration :initform 0.05
                        :allocation :class)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((i rhythm) &rest initargs)
  (declare (ignore initargs))
  (cond ((data i)
         (parse-rhythm i)
         (unless (id i)
           (setf (id i) (data i))))
        ((duration i) ;; no letter associated with this 
         (setf (compound-duration i) (duration i)
               (rq i) (rationalize (duration i))
               (value i) (/ 4.0 (duration i)))))
  (update-needs-new-note i))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone ((r rhythm))
  (clone-with-new-class r 'rhythm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone-with-new-class :around ((i rhythm) new-class)
  (declare (ignore new-class))
  (let ((named-object (call-next-method)))
    (setf (slot-value named-object 'value) (value i)
          (slot-value named-object 'duration) (duration i)
          (slot-value named-object 'undotted-value) (undotted-value i)
          (slot-value named-object 'num-flags) (num-flags i)
          (slot-value named-object 'num-dots) (num-dots i)
          (slot-value named-object 'score-rthm) (score-rthm i)
          (slot-value named-object 'is-rest) (is-rest i)
          (slot-value named-object 'is-whole-bar-rest) (is-whole-bar-rest i)
          (slot-value named-object 'is-tied-to) (is-tied-to i)
          (slot-value named-object 'is-tied-from) (is-tied-from i)
          (slot-value named-object 'marks) (my-copy-list (marks i))
          (slot-value named-object 'marks-in-part)
          (my-copy-list (marks-in-part i))
          (slot-value named-object 'compound-duration) (compound-duration i)
          (slot-value named-object 'is-grace-note) (is-grace-note i)
          (slot-value named-object 'beam) (beam i)
          (slot-value named-object 'rq) (rq i)
          ;; #+cmn
          (slot-value named-object 'rqq-note) (when (rqq-note i)
                                                ;; should really call cmn::copy
                                                ;; but it doesn't work!  
                                                (rqq-note i))
          (slot-value named-object 'rqq-info) (basic-copy-object (rqq-info i))
          (slot-value named-object 'bracket) (copy-list (bracket i))
          (slot-value named-object 'letter-value) (letter-value i)
          (slot-value named-object 'tuplet-scaler) (tuplet-scaler i)
          (slot-value named-object 'bar-pos) (bar-pos i)
          (slot-value named-object 'needs-new-note) (needs-new-note i))
    named-object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* rhythm/force-rest
;;; DESCRIPTION
;;; Force the given rhythm object to be a rest.
;;; 
;;; ARGUMENTS 
;;; - A rhythm object.
;;; 
;;; RETURN VALUE  
;;; A rhythm object.
;;; 
;;; EXAMPLE
#|
(let ((r (make-rhythm 8)))
  (force-rest r)
  (is-rest r))

=> T
|#
;;; SYNOPSIS
(defmethod force-rest ((r rhythm))
;;; ****
  ;; MDE Thu Aug 22 19:03:09 2013 
  (when (is-grace-note r)
    (error "~a~%rhythm::force-rest: Can't force a grace note to a rest." r))
  (setf (needs-new-note r) nil
        (is-tied-to r) nil
        ;; 24.2.11 need to kill the beam too
        (beam r) nil
        (is-tied-from r) nil
        ;; MDE Fri Jan 23 16:43:44 2015 -- strange this hasn't caused errors
        ;; before now...anyway, compound-duration only applies to tied notes
        (compound-duration r) (duration r)
        (is-rest r) t)
  ;; 22.7.11 (Pula)
  (rm-marks r '(beg-sl end-sl) nil)
  r)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-simple ((r rhythm) &optional written (stream t))
  (format stream "~a~a ~a~a, "
          (if (is-tied-to r) "+" "")
          (cond ((is-rest r) "rest")
                ((and (event-p r) (pitch-or-chord r))
                 (get-pitch-symbol r written))
                (t "note"))
          (if (data r)
              (data r)
              (duration r))
          (if (is-tied-from r) "+" "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object :before ((i rhythm) stream)
  (format stream "~&RHYTHM: value: ~,3f, ~
                            duration: ~,3f, ~
                            rq: ~a, ~
                            is-rest: ~a, ~
                  ~%        is-whole-bar-rest: ~a, ~
                  ~%        score-rthm: ~,3f, ~
                            undotted-value: ~a, ~
                            num-flags: ~a, ~
                            num-dots: ~a, ~
                  ~%        is-tied-to: ~a, ~
                            is-tied-from: ~a, ~
                            compound-duration: ~,3f, ~
                  ~%        is-grace-note: ~a, ~
                            needs-new-note: ~a, ~
                            beam: ~a, ~
                            bracket: ~a, ~
                  ~%        rqq-note: ~a, ~
                            rqq-info: ~a, ~
                            marks: ~a, ~
                            marks-in-part: ~a, ~
                  ~%        letter-value: ~a, ~
                            tuplet-scaler: ~a, ~
                            bar-pos: ~a, ~
                  ~%        grace-note-duration: ~a"
          (value i) (duration i) (rq i) (is-rest i) (is-whole-bar-rest i)
          (score-rthm i) (undotted-value i) 
          (num-flags i) (num-dots i) (is-tied-to i) (is-tied-from i)
          (compound-duration i) (is-grace-note i) (needs-new-note i)
          (beam i) (bracket i) (rqq-note i) (rqq-info i) (marks i)
          (marks-in-part i) (letter-value i) (tuplet-scaler i) (bar-pos i)
          (grace-note-duration i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod change-rhythm ((r rhythm) new-rthm)
  (let ((new (make-rhythm new-rthm)))
    (setf (value r) (value new)
          (duration r) (duration new)
          ;; of course this means we could lose some data....
          (compound-duration r) (compound-duration new)
          (rq r) (rq new)
          (undotted-value r) (undotted-value new)
          (num-dots r) (num-dots new)
          (num-flags r) (num-flags new)
          (data r) (data new)
          (id r) (id new)
          (score-rthm r) (score-rthm new))
    r))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Thu Mar  1 13:24:14 GMT 2012: Edited robodoc entry

;;; ****m* rhythm/scale
;;; DESCRIPTION
;;; Change the value of a rhythm object's duration value by a specified
;;; scaling factor. 
;;; 
;;; ARGUMENTS 
;;; - A rhythm object.
;;; - A scaling factor.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - <clone>. This argument determines whether a new rhythm object is made or
;;;   the duration value of the old object is replaced. When set to T, a new 
;;;   object is made based on the duration value of the original. When set to 
;;;   NIL, the original duration value is replaced (see example). Default = T. 
;;;
;;; RETURN VALUE  
;;; A rhythm object.
;;; 
;;; EXAMPLE
#|
(let ((r (make-rhythm 4)))
  (data (scale r 2)))

=> H

(let ((r (make-rhythm 4)))
  (data (scale r 3)))

=> H.

(let ((r (make-rhythm 4)))
  (data (scale r .5)))

=> E

(let ((r (make-rhythm 4)))
  (dotimes (i 5) 
    (print (value (scale r .5)))))

=>
8.0 
8.0 
8.0 
8.0 
8.0

(let ((r (make-rhythm 4)))
  (dotimes (i 5)
    (print (value (scale r .5 nil)))))

=>
8.0 
16.0 
32.0 
64.0 
128.0

|#
;;; SYNOPSIS
(defmethod scale ((r rhythm) scaler &optional (clone t) ignore1 ignore2)
;;; ****
  (declare (ignore ignore1)
           (ignore ignore2))
  ;; (print scaler)
  (when clone
    (setf r (clone r)))
  (if (or (= 1 scaler) (is-grace-note r))
      r
      (progn
        (setf (value r) (/ (value r) scaler)
              (duration r) (* (duration r) scaler)
              (compound-duration r) (* (compound-duration r) scaler)
              (rq r) (rationalize (* (rq r) scaler))
              (undotted-value r) (/ (undotted-value r) scaler)
              (num-flags r) (rthm-num-flags (undotted-value r)))
        ;; NB score-rthm not handled here!
        ;; handle dots:
        (when (zerop (num-dots r))
          (setf (num-dots r)
                (case scaler
                  (1.5 1)
                  (1.75 2)
                  (1.875 3)
                  (t 0))))
        ;; (when (< (value r) 1.0)
        (when (< (value r) 0.1)
          (warn "~a rhythm::scale: ~
            attempt to scale (~a times) a rhythm above 10x a whole note!"
                r scaler))
        ;; let's see if we can get a new rhythm from this thing all scaled and
        ;; dotted and everything....
        (let ((try (get-rhythm-letter-for-value (value r) nil))) ;; don't warn
          ;; (print 'here)
          (when try
            (setf try (make-rhythm try)
                  (is-rest try) (is-rest r)
                  (is-tied-to try) (is-tied-to r)
                  (is-tied-from try) (is-tied-from r)
                  (needs-new-note try) (needs-new-note r))
            ;; MDE Tue Mar 13 11:09:27 2012 
            (when (= (tuplet-scaler r) (tuplet-scaler try))
              (setf (bracket try) (bracket r))))
          ;; (print 'there)
          (if try
              try
              r)))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod has-flags ((r rhythm))
  (> (num-flags r) 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* rhythm/rhythm-equal
;;; DESCRIPTION
;;; Compares the values of two rhythm objects to determine if they are equal. 
;;;
;;; NB rhythm-equal compares the values only, so rhythms with the same values
;;;    will still be considered equal even if their other attributes (such as
;;;    :is-rest and :is-tied-to etc.) are different.
;;; 
;;; ARGUMENTS 
;;; - A first rhythm object.
;;; - A second rhythm object.
;;; 
;;; RETURN VALUE  
;;; T if the values of the given rhythm objects are equal, else NIL.
;;; 
;;; EXAMPLE
#|
(let ((r1 (make-rhythm 4))
      (r2 (make-rhythm 4)))
  (rhythm-equal r1 r2))

=> T

(let ((r1 (make-rhythm 4))
      (r2 (make-rhythm 8)))
  (rhythm-equal r1 r2))

=> NIL

(let ((r1 (make-rhythm 4 :is-rest T))
      (r2 (make-rhythm 4 :is-rest NIL)))
  (rhythm-equal r1 r2))

=> T

(let ((r1 (make-rhythm 4 :is-tied-to T))
      (r2 (make-rhythm 4 :is-tied-to NIL)))
  (rhythm-equal r1 r2))

=> T

|#
;;; SYNOPSIS
(defmethod rhythm-equal ((r1 rhythm) (r2 rhythm))
;;; ****
  (equal-within-tolerance (value r1) (value r2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* rhythm/rhythm/
;;; DESCRIPTION
;;; Determines the ratio of one rhythm object's duration to that of a second
;;; rhythm object by use of division. 
;;; 
;;; ARGUMENTS 
;;; - A rhythm object.
;;; - A second rhythm object.
;;; 
;;; RETURN VALUE  
;;; A number.
;;; 
;;; EXAMPLE
#|
(let ((r1 (make-rhythm 'q))
      (r2 (make-rhythm 'e)))
  (rhythm/ r1 r2))

=> 2.0

(let ((r1 (make-rhythm 'q))
      (r3 (make-rhythm 's.)))
  (rhythm/ r1 r3))

=> 2.6666667

|#
;;; SYNOPSIS
(defmethod rhythm/ ((r1 rhythm) (r2 rhythm))
;;; ****
  (/ (duration r1) (duration r2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 18.12.11 SAR Added robodoc info

;;; ****m* rhythm/is-multiple
;;; DESCRIPTION
;;; Determines if the value of one rhythm object is a multiple of the value of
;;; a second rhythm object. This is established by dividing the one by the
;;; other and checking to see if the quotient is a whole number.
;;; 
;;; ARGUMENTS 
;;; - A first rhythm object.
;;; - A second rhythm object.
;;; 
;;; RETURN VALUE  
;;; Returns T if true and NIL if not. Always also returns the quotient.
;;; 
;;; EXAMPLE
#|
(let ((r1 (make-rhythm 'q))
      (r2 (make-rhythm 'e)))
  (is-multiple r1 r2))

=> T, 2.0

(let ((r1 (make-rhythm 'q))
      (r2 (make-rhythm 'e.)))
  (is-multiple r1 r2))

=> NIL, 1.3333333333333333

|#
;;; SYNOPSIS
(defmethod is-multiple ((r1 rhythm) (r2 rhythm))
;;; ****
  (whole-num-p (rhythm/ r1 r2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 18.12.11 SAR: Added robodoc info

;;; ****m* rhythm/add-mark
;;; DESCRIPTION
;;; Add an articulation, dynamic, slur or any other mark to a rhythm (also
;;; useful in the event subclass for changing note heads etc.) Multiple marks
;;; can be added separately and consecutively to the same rhythm object. 
;;;
;;; A warning is printed if the same mark is added to the same rhythm object
;;; more than once. 
;;;
;;; NB: This method checks to see if the mark added is a valid mark and will
;;;     warn if it doesn't exist (but it will still add it, in case you have 
;;;     your own processing logic for it).
;;; 
;;; ARGUMENTS 
;;; - A rhythm object.
;;; - A mark.
;;;
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicated whether to issue a warning when trying to add marks
;;;   to a rest. Default = NIL.
;;; 
;;; RETURN VALUE  
;;; Always T.
;;; 
;;; EXAMPLE
#|
(let ((r (make-rhythm 'q)))
  (marks r))

=> NIL

(let ((r (make-rhythm 'q)))
  (add-mark r 'a))

=> T

(let ((r (make-rhythm 'q)))
  (add-mark r 's)
  (marks r))

=> (S)

(let ((r (make-rhythm 'q)))
  (add-mark r 'col-legno)
  (add-mark r 'as)
  (add-mark r 'x-head)
  (marks r))

=> (X-HEAD AS COL-LEGNO)

(let ((r (make-rhythm 'q)))
  (add-mark r 's)
  (add-mark r 's))

=> T
WARNING: rhythm::add-mark: S already present but adding again!: 

(let ((r (make-rhythm 'e :is-rest t)))
  (add-mark r 'at)
  (print (is-rest r))
  (print (marks r)))

=>
T 
(AT)

(let ((r (make-rhythm 'e :is-rest t)))
  (add-mark r 'at t))

=> T
WARNING: 
[...]
rhythm::add-mark: add AT to rest?

|#
;;; SYNOPSIS
(defmethod add-mark ((r rhythm) mark &optional warn-rest)
;;; ****
  (when mark
    (when (and warn-rest (is-rest r))
      (warn "~a~&rhythm::add-mark: add ~a to rest?" r mark))
    ;; 9.4.11 check we haven't already got the mark
    (when (has-mark r mark)
      (warn "~a~&rhythm::add-mark: mark ~a already present but adding again!"
            r mark))
    ;; MDE Fri Dec 23 18:44:20 2011 -- check marks exist now, so as to avoid
    ;; surprises down the line. NB even when a mark doesn't exist in CMN or LP
    ;; the following calls should work as they'll return "" i.e. there should
    ;; be a case for them even if we can't create them.
    (validate-mark mark)
    (push mark (marks r))
    t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 19.12.11 SAR: Added robodoc info
;;; SAR Sat Dec 31 09:10:12 EST 2011: Added DATE back

;;; ****m* rhythm/add-mark-once
;;; DATE
;;; 26 Jul 2011 (Pula)
;;; 
;;; DESCRIPTION
;;; Apply the given mark to the given rhythm object, but do so only if the
;;; given rhythm object does not yet have the mark.
;;; 
;;; ARGUMENTS 
;;; - A rhythm object.
;;; - A mark.
;;;
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether or not to print a warning when attempting to
;;;   apply a mark to a rest.
;;; 
;;; RETURN VALUE  
;;; Returns T if the mark is successfully applied (if the rhythm object did not
;;; already possess the mark), otherwise NIL if the mark was not applied
;;; because the rhythm object already had it.
;;; 
;;; EXAMPLE
#|
(let ((r (make-rhythm 'q)))
  (add-mark-once r 'a))

=> T

(let ((r (make-rhythm 'q)))
  (add-mark-once r 'a)
  (marks r))

=> (A)

(let ((r (make-rhythm 'q)))
  (add-mark-once r 'a)
  (add-mark-once r 'a))

=> NIL

(let ((r (make-rhythm 'q)))
  (add-mark-once r 'a)
  (add-mark-once r 'a)
  (marks r))

=> (A)

|#
;;; SYNOPSIS
(defmethod add-mark-once ((r rhythm) mark &optional warn-rest)
;;; ****
  (when mark
    (unless (has-mark r mark)
      (add-mark r mark warn-rest))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 21/4/10 can't call this add-marks as that method exists and it takes
;;; one argument only.
(defmethod rhythm-add-marks ((r rhythm) marks &optional warn-rest)
  (if (listp marks)
      (loop for mark in marks do
           (add-mark r mark warn-rest))
      (add-mark r marks warn-rest)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod add-mark-in-part ((r rhythm) mark)
  (validate-mark mark)
  (push mark (marks-in-part r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 26.9.11 nothing to do
(defmethod enharmonic ((r rhythm) &key written force-naturals)
  (declare (ignore written force-naturals))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 19.12.11 SAR: Added robodoc info

;;; ****m* rhythm/rm-marks
;;; DESCRIPTION
;;; Remove a specified mark (or a list of specified marks) from the MARKS slot
;;; of a given rhythm object. If the mark specified is not present in the given
;;; rhythm object's MARKS slot, a warning is printed. If some marks of a list
;;; of specified marks are present in the rhythm object's MARKS slot and other
;;; aren't, those that are will be removed and a warning will be printed for
;;; the rest.
;;;
;;; ARGUMENTS 
;;; - A rhythm object.
;;; - A mark or list of marks.
;;;
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether a warning is to be printed if the specified
;;;   mark is not present in the given rhythm object's MARKS slot.
;;; 
;;; RETURN VALUE  
;;; Always returns NIL.
;;; 
;;; EXAMPLE
#|

;; The method itself returns NIL
(let ((r (make-rhythm 'q)))
  (add-mark-once r 'a)
  (rm-marks r 'a))

=> NIL

;; Adding a list of marks to r, then removing only 's
(let ((r (make-rhythm 'q)))
  (loop for m in '(a s pizz col-legno x-head) do 
       (add-mark-once r m))
  (rm-marks r 's)
  (marks r))

=> (X-HEAD COL-LEGNO PIZZ A)

;; Removing a list of marks from r
(let ((r (make-rhythm 'q)))
  (loop for m in '(a s pizz col-legno x-head) do 
       (add-mark-once r m))
  (rm-marks r '(s a))
  (marks r))

=> (X-HEAD COL-LEGNO PIZZ)

;; Attempting to remove a mark that isn't present results in a warning
;; being printed by default
(let ((r (make-rhythm 'q)))
  (loop for m in '(a s pizz col-legno x-head) do 
       (add-mark-once r m))
  (rm-marks r 'zippy))

=> NIL
WARNING: rhythm::rm-marks: no mark ZIPPY in (X-HEAD COL-LEGNO PIZZ S A) 

;; Suppress printing the warning when the specified mark isn't present
(let ((r (make-rhythm 'q)))
  (loop for m in '(a s pizz col-legno x-head) do 
       (add-mark-once r m))
  (rm-marks r 'zippy nil))

=> NIL
|#
;;; SYNOPSIS
(defmethod rm-marks ((r rhythm) marks &optional (warn t))
;;; ****
  ;; MDE Thu Dec 26 15:11:24 2013 -- moved this into aux method so we can call
  ;; for pitch also 
  (rm-marks-aux r marks warn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 19.12.11 SAR: Added robodoc info

;;; ****m* rhythm/replace-mark
;;; DESCRIPTION
;;; Replace a specified mark of a given rhythm object with a second specified
;;; mark. If a rhythm object contains more than one mark, individual marks can
;;; be changed without modifying the remaining marks.
;;; 
;;; ARGUMENTS 
;;; - A rhythm object.
;;; - The mark to be replaced.
;;; - The new mark.
;;;
;;; RETURN VALUE  
;;; Returns the new value of the MARKS slot of the given object.
;;; 
;;; EXAMPLE

#|

;; Make a rhythm object, add the mark 'a, then replace 'a with 's
(let ((r (make-rhythm 'q)))
  (add-mark r 'a)
  (replace-mark r 'a 's))

=> (S)

;; Make a rhythm object, add a list of marks, replace just the 'pizz mark with
;; a 'batt mark

(let ((r (make-rhythm 'q)))
  (loop for m in '(a s pizz col-legno) do (add-mark-once r m))
  (replace-mark r 'pizz 'batt))

=> (COL-LEGNO BATT S A)

|#

;;; SYNOPSIS
(defmethod replace-mark ((r rhythm) what with &optional ignore)
;;; ****
  (declare (ignore ignore))
  (setf (marks r) (substitute with what (marks r))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon Dec 26 20:59:50 EST 2011 Added robodoc info

;;; ****m* rhythm/delete-marks
;;; DESCRIPTION
;;; Delete any marks in the MARKS slot of an event object created within a
;;; rhythm object, replacing the entire list of the MARKS slot with NIL.
;;; 
;;; ARGUMENTS 
;;; - A rhythm object.
;;; 
;;; RETURN VALUE  
;;; Always returns NIL.
;;; 
;;; EXAMPLE 

#|
;; The method returns NIL
(let ((r (make-rhythm (make-event 'c4 'q))))
  (loop for m in '(a s pizz) do (add-mark-once r m))
  (delete-marks r))

=> NIL

;; Create a rhythm object consisting of an event object and print the default
;; contents of the MARKS slot. Set the MARKS slot to contain three marks and
;; print the result. Apply the delete-marks method and print the result.
(let ((r (make-rhythm (make-event 'c4 'q))))
  (print (marks r))
  (loop for m in '(a s pizz) do (add-mark-once r m))
  (print (marks r))
  (delete-marks r) 
  (print (marks r)))

=> 
NIL 
(PIZZ S A) 
NI

|#

;;; SYNOPSIS
(defmethod delete-marks ((r rhythm))
;;; ****
  (setf (marks r) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Mon Sep  1 16:55:24 2014 
(defmethod delete-marks-if ((r rhythm) &optional (fun #'stringp))
  (setf (marks r) (remove-if fun (marks r)))
  (marks r))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod start-beam ((r rhythm) &optional (warn t))
  (when warn
    (check-beam-ok r))
  (setf (beam r) 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod end-beam ((r rhythm) &optional (warn t))
  (when warn
    (check-beam-ok r))
  (setf (beam r) 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 19.12.11 SAR: Added robodoc info

;;; ****m* rhythm/has-mark
;;; DESCRIPTION
;;; Check to see if a given rhythm object possesses a specified mark.
;;; 
;;; ARGUMENTS 
;;; - A rhythm object.
;;; - A mark.
;;; 
;;; RETURN VALUE  
;;; If the specified mark is indeed found in the MARKS slot of the given rhythm
;;; object, the tail of the list of marks contained in that slot is returned;
;;; otherwise NIL is returned.
;;; 
;;; EXAMPLE
#|
;; Add a specific mark and check to see if the rhythm object has it.
(let ((r (make-rhythm 'q)))
  (add-mark r 'a)
  (has-mark r 'a))

=> (A)

;; Check to see if the given rhythm object possess a mark we know it doesn't. 
(let ((r (make-rhythm 'q)))
  (add-mark r 'a)
  (has-mark r 's))

=> NIL

|#
;;; SYNOPSIS
(defmethod has-mark ((r rhythm) mark &optional (test #'equal))
;;; ****
  (has-mark-aux (marks r) mark test))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 22.12.11 SAR: Added robodoc info
;;; SAR Sat Dec 31 09:11:14 EST 2011: Added DATE back

;;; ****m* rhythm/accented-p
;;; DATE
;;; 05 Apr 2011
;;;
;;; DESCRIPTION
;;; Check the MARKS slot of a given rhythm object to determine if it possesses
;;; an accent mark. The rhythm object may also possess other marks as well. 
;;; 
;;; ARGUMENTS 
;;; - A rhythm object.
;;; 
;;; RETURN VALUE  
;;; If the accent mark ('a) is indeed found in the MARKS slot of the given
;;; rhythm object, the tail of the list of marks contained in that slot is
;;; returned; otherwise NIL is returned.
;;; 
;;; EXAMPLE
#|
;; Make a rhythm object, add an accent, and test for the presence of the accent
(let ((r (make-rhythm 'q)))
  (add-mark-once r 'a)
  (accented-p r))

=> (A)

;; Check if an accent mark is among all marks in the MARKS slot
(let ((r (make-rhythm 'q)))
  (add-mark-once r 's)
  (add-mark-once r 'a)
  (accented-p r))

=> (A S)

;; Add an accent and staccato, then remove the accent and test for it
(let ((r (make-rhythm 'q)))
  (add-mark-once r 'a)
  (add-mark-once r 's)
  (rm-marks r 'a)
  (accented-p r))

=> NIL

|#
;;; SYNOPSIS
(defmethod accented-p ((r rhythm))
;;; ****
  (has-mark r 'a))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 22.12.11 SAR: Added robodoc info

;;; ****m* rhythm/begin-slur-p
;;; DESCRIPTION
;;; Check to see if the MARKS slot of a given rhythm object contains a mark for
;;; the beginning of a slur ('beg-sl). The rhythm object may also possess other
;;; marks as well. 
;;; 
;;; ARGUMENTS 
;;; - A rhythm object.
;;; 
;;; RETURN VALUE  
;;; If the 'beg-sl mark is indeed found in the MARKS slot of the given rhythm
;;; object, the tail of the list of marks contained in that slot is returned;
;;; otherwise NIL is returned.
;;; 
;;; EXAMPLE
#|
;; Create a rhythm object, add a 'beg-sl mark and check for it
(let ((r (make-rhythm 'q)))
  (add-mark-once r 'beg-sl)
  (begin-slur-p r))

=> (BEG-SL)

;; Add several marks to a rhythm object and check for 'beg-sl
(let ((r (make-rhythm 'q)))
  (loop for m in '(a s beg-sl) do (add-mark-once r m))
  (begin-slur-p r))

=> (BEG-SL S A)

;; Add a 'beg-sl mark to a rhythm object, then delete it and check for it
(let ((r (make-rhythm 'q)))
  (add-mark-once r 'beg-sl)
  (rm-marks r 'beg-sl)
  (begin-slur-p r))

=> NIL

|#
;;; SYNOPSIS
(defmethod begin-slur-p ((r rhythm))
;;; ****
  (has-mark r 'beg-sl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 22.12.11 SAR: Added robodoc info

;;; ****m* rhythm/end-slur-p
;;; DESCRIPTION
;;; Check to see if the MARKS slot of a given rhythm object contains a mark for
;;; the ending of a slur ('end-sl). The rhythm object may also possess other
;;; marks as well.  
;;; 
;;; ARGUMENTS 
;;; - A rhythm object.
;;; 
;;; RETURN VALUE  
;;; If the 'end-sl mark is indeed found in the MARKS slot of the given rhythm
;;; object, the tail of the list of marks contained in that slot is returned;
;;; otherwise NIL is returned.
;;; 
;;; EXAMPLE
#|
;; Create a rhythm object, add a 'end-sl mark and check for it
(let ((r (make-rhythm 'q)))
  (add-mark-once r 'end-sl)
  (end-slur-p r))

=> (END-SL)

;; Add several marks to a rhythm object and check for 'end-sl
(let ((r (make-rhythm 'q)))
  (loop for m in '(a s end-sl) do (add-mark-once r m))
  (end-slur-p r))

=> (END-SL S A)

;; Add an 'end-sl mark to a rhythm object, then delete it and check for it
(let ((r (make-rhythm 'q)))
  (add-mark-once r 'end-sl)
  (rm-marks r 'end-sl)
  (end-slur-p r))

=> NIL

|#
;;; SYNOPSIS
(defmethod end-slur-p ((r rhythm))
;;; ****
  (has-mark r 'end-sl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Fri May 11 14:29:38 2012 -- corresponding tests for phrase marks
(defmethod begin-phrase-p ((r rhythm))
  (has-mark r 'beg-phrase))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod end-phrase-p ((r rhythm))
  (has-mark r 'end-phrase))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod check-beam-ok ((r rhythm))
  (when (and (zerop (num-flags r))
             (>= (duration r) 1.0))
    (warn "~&~a~&Placing beam on rhythm with no tails!" r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 22.12.11 SAR: Added robodoc info

;;; ****m* rhythm/delete-beam
;;; DESCRIPTION
;;; Removes indication for the start (1) or end (0) of a beam from the BEAM
;;; slot of a given rhythm object, replacing them with NIL.
;;; 
;;; ARGUMENTS 
;;; - A rhythm object.
;;; 
;;; RETURN VALUE  
;;; Always returns NIL.
;;; 
;;; EXAMPLE
#|
;; Manually set the beam of a rhythm object and delete it to see result NIL
(let ((r (make-rhythm 'e)))
  (setf (beam r) 1)
  (delete-beam r))

=> NIL

;; Make a rthm-seq-bar object with beam indications, then check the BEAM slot
;; of each rhythm object in the rthm-seq-bar object.
(let ((rsb (make-rthm-seq-bar '((2 4) - s s e - q))))
  (loop for r in (rhythms rsb) collect (beam r)))

=> (1 NIL 0 NIL)

;; Make a rthm-seq-bar object with beam indications, delete them all, then
;; check the beam slot of each rhythm object in the rthm-seq-bar object.
(let ((rsb (make-rthm-seq-bar '((2 4) - s s e - q))))
  (loop for r in (rhythms rsb) do (delete-beam r))
  (loop for r in (rhythms rsb) collect (beam r)))

=> (NIL NIL NIL NIL)

|#
;;; SYNOPSIS
(defmethod delete-beam ((r rhythm))
;;; ****
  (setf (beam r) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; When we put a 3plet/6plet etc. bracket over rhythms, then 1/8, 1/4 notes
;;; etc. need a dot.  Check for those cases that need the dot and add it if
;;; necessary.

(defmethod dot-for-triplet? ((r rhythm) tuplet)
  (unless (is-grace-note r)
    (when (and (zerop (mod tuplet 3))
               (power-of-2 (value r)))
      (setf (num-dots r) 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 22.12.11 SAR: Added robodoc info

;;; ****m* rhythm/duration-secs
;;; DESCRIPTION
;;; Determine the absolute duration in seconds of a given rhythm object at a
;;; given quarter-note tempo. If no tempo is specified, a tempo of 60 is
;;; assumed.  
;;; 
;;; ARGUMENTS 
;;; - A rhythm object.
;;;
;;; OPTIONAL ARGUMENTS
;;; - A numerical tempo value based on quarter-note beats per minute.
;;; 
;;; RETURN VALUE  
;;; A real number (floating point) representing the absolute duration of the
;;; given rhythm object in seconds.  
;;; 
;;; EXAMPLE
#|
;; Determine the duration in seconds of a quarter note with a default tempo of
;;; quarter = 60
(let ((r (make-rhythm 'q)))
  (duration-secs r))

=> 1.0

;; Specifying a different tempo results in a different duration in seconds 
(let ((r (make-rhythm 'q)))
  (duration-secs r 96))

=> 0.625

|#
;;; SYNOPSIS
(defmethod duration-secs ((r rhythm) &optional (tempo 60))
;;; ****
  (unless (typep tempo 'tempo)
    (setf tempo (make-tempo tempo)))
  (* (qtr-dur tempo) (duration r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod update-needs-new-note ((i rhythm))
  (setf (needs-new-note i) (not (or (is-rest i) (is-tied-to i)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod delete-tuplet-bracket ((r rhythm))
  (setf (bracket r) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The brackets info is a list of 2-element lists where the first element is
;;; the index into the list of brackets created by cmn (i) and the second
;;; number is the tuplet number itself.  

(defmethod add-tuplet-bracket ((r rhythm) bracket-info &optional ignore)
  (declare (ignore ignore))
  ;; it would be illegal (i.e. create errors in cmn) to try and add the same
  ;; bracket twice so avoid this here! 
  (unless (member bracket-info (bracket r) :test #'equal)
    (setf (bracket r) (econs (bracket r) bracket-info))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf is-rest) :after (value (i rhythm))
  (declare (ignore value))
  (update-needs-new-note i))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf is-tied-to) :after (value (i rhythm))
  ;; MDE Sun Nov 18 18:42:54 2012 
  (when (and value (is-rest i))
    (warn "rhythm::(setf is-tied-to): this rhythm is a rest so shouldn't ~
           be tied to: ~%~a" i))
  (update-needs-new-note i))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf is-tied-from) :after (value (i rhythm))
  ;; MDE Sun Nov 18 18:42:54 2012 
  (when (and value (is-rest i))
    (warn "rhythm::(setf is-tied-from): this rhythm is a rest so shouldn't ~
           be tied from: ~%~a" i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Parse rhythm symbols. 'q will return 4 remember to escape dots after and +
;;; signs before normal integers because both +14 and 14. will be evaluated by
;;; lisp as a simple 14: \+14 or 14\. avoids this.

(defmethod parse-rhythm ((i rhythm))
  (parse-rhythm-symbol (data i) :rhythm-object i))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf rq) :after (value (r rhythm))
  ;; make sure the rq is a rational e.g. 15/2 otherwise cmns spacing
  ;; algorithm will go to hell
  (unless (rationalp value)
    (error "rhythm::parse-rhythm: RQ Value not a rational.  ~
            Given rhythm ~a produces RQ ~a"
           r value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 22.12.11 SAR: Added robodoc info

;;; ****m* rhythm/add
;;; DESCRIPTION
;;; Create a new rhythm object with a duration that is equal to the sum of the
;;; duration of two other given rhythm objects. 
;;;
;;; NB: This method only returns a single rhythm rather than a list with
;;;     ties. Thus q+s, for example, returns TQ... 
;;;
;;; If the resulting duration cannot be presented as a single, notatable
;;; rhythm, the DATA slot of the resulting rhythm object is set to NIL, though
;;; the VALUE and DURATION slots are still set with the corresponding numeric
;;; values.  
;;; 
;;; ARGUMENTS 
;;; - A first rhythm object.
;;; - A second rhythm object.
;;;
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether a warning is printed when a rhythm cannot be
;;;   made because the resulting value is 0 or a negative duration. Default = 
;;;   NIL (no warning issued).
;;; 
;;; RETURN VALUE  
;;; A rhythm object. Returns NIL when the object cannot be made.
;;; 
;;; EXAMPLE
#|
;; A quarter plus an eighth makes a dotted quarter
(let ((r1 (make-rhythm 'q))
      (r2 (make-rhythm 'e)))
  (add r1 r2))

=>
RHYTHM: value: 2.6666666666666665, duration: 1.5, rq: 3/2, is-rest: NIL, score-rthm: 4.0f0., 
        undotted-value: 4, num-flags: 0, num-dots: 1, is-tied-to: NIL, 
        is-tied-from: NIL, compound-duration: 1.5, is-grace-note: NIL, 
        needs-new-note: T, beam: NIL, bracket: NIL, rqq-note: NIL, 
        rqq-info: NIL, marks: NIL, marks-in-part: NIL, letter-value: 4, 
        tuplet-scaler: 1, grace-note-duration: 0.05
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: Q., tag: NIL, 
data: Q.

;; A quarter plus a triplet-eighth is presented as a triplet-half
(let ((r1 (make-rhythm 'q))
      (r2 (make-rhythm 'te)))
  (data (add r1 r2)))

=> TH

;; A quarter plus a septuplet-16th cannot be represented as a single, notatable
;; rhythm and therefore produces an object with a VALUE and DURATION but no
;; DATA 
(let ((r1 (make-rhythm 4))
      (r2 (make-rhythm 28)))
  (print (value (add r1 r2)))
  (print (duration (add r1 r2)))
  (print (data (add r1 r2))))

=>
3.5 
1.1428571428571428 
NIL

|#
;;; SYNOPSIS
(defmethod add ((r1 rhythm) (r2 rhythm) &optional warn)
;;; ****
  (arithmetic r1 r2 #'+ warn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 22.12.11 SAR: Added robodoc info

;;; ****m* rhythm/subtract 
;;; DESCRIPTION
;;; Create a new rhythm object with a duration that is equal to the difference
;;; between the duration of two other given rhythm objects.   
;;;
;;; NB: This method only returns a single rhythm rather than a list with
;;;     ties. Thus h - e., for example, returns TQ... 
;;;
;;; If the resulting duration cannot be presented as a single rhythm, the DATA
;;; slot of the resulting rhythm object is set to NIL, though the VALUE and
;;; DURATION slots are still set with the corresponding numeric values. 
;;;
;;; If the resulting duration is equal to or less than 0, NIL is returned and
;;; an optional warning may be printed.
;;; 
;;; ARGUMENTS 
;;; - A first rhythm object.
;;; - A second rhythm object.
;;;
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether a warning is to be printed when the
;;;   resulting duration is less than or equal to 0. Default = 0. 
;;; 
;;; RETURN VALUE  
;;; A rhythm object if the resulting duration is greater than 0, else NIL and
;;; the optional warning.
;;; 
;;; EXAMPLE
#|
;; Make a new rhythm object with a duration equal to one quarter minus one
;; eighth. 
(let ((r1 (make-rhythm 'q))
      (r2 (make-rhythm 'e)))
  (subtract r1 r2))

=> 
RHYTHM: value: 8.0f0, duration: 0.5, rq: 1/2, is-rest: NIL, score-rthm: 8.0f0, 
        undotted-value: 8, num-flags: 1, num-dots: 0, is-tied-to: NIL, 
        is-tied-from: NIL, compound-duration: 0.5, is-grace-note: NIL, 
        needs-new-note: T, beam: NIL, bracket: NIL, rqq-note: NIL, 
        rqq-info: NIL, marks: NIL, marks-in-part: NIL, letter-value: 8, 
        tuplet-scaler: 1, grace-note-duration: 0.05
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: E, tag: NIL, 
data: E 

;; A half minus a dotted eighth is represented as a triplet half
(let ((r1 (make-rhythm 'h))
      (r2 (make-rhythm 'e.)))
  (data (subtract r1 r2)))

=> TQ...

;; If the resulting duration is 0 or less, return NIL, with no warning by
;; default 
(let ((r1 (make-rhythm 'e))
      (r2 (make-rhythm 'q)))
  (subtract r1 r2))

=> NIL

;; Setting the optional argument to t returns a warning when the resulting
;; duration is less than 0
(let ((r1 (make-rhythm 'e))
      (r2 (make-rhythm 'q)))
  (subtract r1 r2 t))

=> NIL
WARNING: rhythm::arithmetic: new duration is -0.5; can't create rhythm

;; Subtracting a septuplet-16th from a quarter results in a duration that
;; cannot be represented as a single rhythm, therefore setting the DATA to NIL
;; while VALUE and DURATION are still set.
(let ((r1 (make-rhythm 4))
      (r2 (make-rhythm 28)))
  (print (value (subtract r1 r2)))
  (print (duration (subtract r1 r2)))
  (print (data (subtract r1 r2))))

=>
4.666666666666666 
0.8571428571428572 
NIL

|#
;;; SYNOPSIS
(defmethod subtract ((r1 rhythm) (r2 rhythm) &optional warn)
;;; ****
  (arithmetic r1 r2 #'- warn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod arithmetic ((r1 rhythm) (r2 rhythm) function warn)
  ;; must use compound-duration so as to take ties into consideration
  (let* ((dur (funcall function (compound-duration r1) (compound-duration r2))))
    (if (> dur 0)
        (make-rhythm dur :duration t)
        (when warn
          ;; warn returns nil
          (warn "rhythm::arithmetic: new duration is ~a; can't create rhythm"
                dur)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 13.12.11 SAR: Added ROBODoc info

;;; ****f* rhythm/make-rhythm
;;; DESCRIPTION
;;; Make a rhythm object.
;;; 
;;; ARGUMENTS 
;;; - A duration either as a numeric representation of a rhythm (subdivision of
;;;   a whole note; 2 = half note, 4 = quarter, 8 = eighth etc), a quoted 
;;;   alphabetic shorthand for a duration (ie, 'h, 'q, 'e etc.), or an absolute 
;;;   duration in seconds.     
;;;
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :is-rest. T or NIL to denote whether the given duration is a rest or
;;;   not. T = rest. Default = NIL.
;;; - :is-tied-to. T or NIL to denote whether the given duration is tied later
;;;   to the next duration in a given rthm-seq-bar/rthm-seq object. T =
;;;   tied. Default = NIL.
;;; - :duration. Indicates whether the duration argument has been given as a
;;;   duration in seconds, not a known rhythm like 'e or 8. T indicates that
;;;   the duration is a duration in seconds. Default = NIL.
;;; - :tempo. Indicates the tempo for the given rhythm and is used only when
;;;    :duration is set to figure out the rhythm type (1/8, 1/4 etc.) from the
;;;    two values. So this is not related to any tempi applied, rather one that
;;;    is reflected in the duration-in-tempo slot of event.
;;; 
;;; RETURN VALUE  
;;; A rhythm object.
;;; 
;;; EXAMPLE
#|
(make-rhythm 16)

=>
RHYTHM: value: 16.0, duration: 0.25, rq: 1/4, is-rest: NIL, score-rthm: 16.0, 
        undotted-value: 16, num-flags: 2, num-dots: 0, is-tied-to: NIL, 
        is-tied-from: NIL, compound-duration: 0.25, is-grace-note: NIL, 
        needs-new-note: T, beam: NIL, bracket: NIL, rqq-note: NIL, 
        rqq-info: NIL, marks: NIL, marks-in-part: NIL, letter-value: 16, 
        tuplet-scaler: 1, grace-note-duration: 0.05
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: 16, tag: NIL, 
data: 16

(make-rhythm 16 :is-rest t :is-tied-to t)

=> 
RHYTHM: value: 16.0, duration: 0.25, rq: 1/4, is-rest: T, score-rthm: 16.0, 
        undotted-value: 16, num-flags: 2, num-dots: 0, is-tied-to: T, 
        is-tied-from: NIL, compound-duration: 0.25, is-grace-note: NIL, 
        needs-new-note: NIL, beam: NIL, bracket: NIL, rqq-note: NIL, 
        rqq-info: NIL, marks: NIL, marks-in-part: NIL, letter-value: 16, 
        tuplet-scaler: 1, grace-note-duration: 0.05
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: 16, tag: NIL, 
data: 16

(make-rhythm .23 :duration t)

=> 
RHYTHM: value: 17.391304, duration: 0.23, rq: 23/100, is-rest: NIL, score-rthm: NIL, 
        undotted-value: -1.0, num-flags: 0, num-dots: 0, is-tied-to: NIL, 
        is-tied-from: NIL, compound-duration: 0.23, is-grace-note: NIL, 
        needs-new-note: T, beam: NIL, bracket: NIL, rqq-note: NIL, 
        rqq-info: NIL, marks: NIL, marks-in-part: NIL, letter-value: -1, 
        tuplet-scaler: 1, grace-note-duration: 0.05
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: NIL, tag: NIL, 
data: NIL

|#
;;; SYNOPSIS
(defun make-rhythm (rthm &key (is-rest nil) (is-tied-to nil) (duration nil)
                    (tempo 60.0))
;;; ****                                
  ;;  (unless rthm                      
  ;;  (error "event::make-rhythm: <rthm> can't be nil")) 
  ;; only if duration t                 
  (cond ((rhythm-p rthm) (clone rthm))
        ((and rthm (not duration))
         (make-instance 'rhythm :data rthm :is-rest is-rest 
                        :is-tied-to is-tied-to))
        ((and (numberp duration) rthm)
         (error "rhythm::make-rhythm: can't process both a <rthm> (~a) and ~
                 <duration> (~a) (duration should be T or NIL)" rthm duration))
        ((and rthm duration)
         (let ((rthm-letter
                (get-rhythm-letter-for-duration 
                 rthm :tempo tempo :warn nil :error-on-fail nil)))
           ;; MDE Mon Mar 19 22:14:20 2012 
           (unless rthm-letter
             (setf rthm-letter (rationalize-if-necessary rthm
                                                         :error-on-fail nil)
                   rthm-letter 
                   (if (and rthm-letter (= 1 (length rthm-letter)))
                       (data (first rthm-letter))
                       nil)))
           (make-instance 'rhythm 
                          :data rthm-letter ;(when rthm-letter rthm-letter)
                          :duration (if rthm-letter -1 rthm)
                          :is-rest is-rest :is-tied-to is-tied-to)))
        (t nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rthm-num-flags (undotted-value)
  (if (< undotted-value 8)
      0
      (- (floor (log undotted-value 2))
         2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set-grace-note-duration (new-duration)
  (unless (numberp new-duration)
    (error "rhythm::set-grace-note-duration: Argument must be a number: ~a" 
           new-duration))
  ;; do we really need this?  can't we set a class variable directly?
  (let ((temp (make-rhythm 'w)))
    (setf (grace-note-duration temp) new-duration)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Returns either a single rhythm instance or a list thereof, depending on
;;; whether <rthm> is compound or not (ie has ties: e+ts).  This time rthm is a
;;; symbol.  Rests are notated by placing them in parentheses
;;; 4.8.10: added &optional inc-compound so we can handle lists of rhythms
;;; outside the context of rthm-seqs 

(defun parse-possibly-compound-rhythm (rhythm &optional inc-compound)
  (if (rhythm-p rhythm)
      rhythm
      (let* ((is-rest (listp rhythm))
             (rthm (if is-rest (first rhythm) rhythm))
             (string (format nil "~a" rthm))
             (first-is-tied-to (char= #\+ (aref string 0)))
             (ties (ties string)))
        (when (and is-rest ties)
          (error "rhythm::parse-possibly-compound-rhythm:~
                Rests cannot be tied: ~a"
                 rhythm))
        (if ties
            (loop for r in ties collect
                 (make-rhythm r :is-rest is-rest :is-tied-to t)
                 into rthms
                 finally 
                 (unless first-is-tied-to 
                   (setf (is-tied-to (first rthms)) nil))
               ;; when a rhythm is tied to, it normally won't have a
               ;; compound-duration but when this happens to be the first note
               ;; of the bar that is tied to, then we need that
               ;; compound-duration for incrementing the last struck note.
               ;; UNLESS EXPLICITLY REQUESTED, DON'T DO THIS HERE ANYMORE.  DO
               ;; IT IN RTHM-SEQ-BAR!!! 
                 (when inc-compound
                   (setf (compound-duration (first rthms))
                         (loop for i in rthms sum (duration i))))
                 (return rthms))
            (make-rhythm rthm :is-rest is-rest)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;; Allow "w+e" syntax:  Split rhythm into separate strings and return them in
;;; a list. 

(defun ties (strg)
  (when (search "+" strg)
    (let ((len (length strg))
          (positions (list -1))) ;; fake position--so we start at char 0.
      (when (= 1 len)
        (error "rhythm::ties: ~a is a rhythm???" strg))
      ;; get the char position of every "+" in the string.
      (loop for i below len if (char= #\+ (aref strg i)) do (push i positions))
      (push len positions) ;; so we loop until the end of the string.
      (setq positions (nreverse positions))
      ;; when we just have a + at the beginning of the string, then we have to 
      ;; adjust
      (when (zerop (second positions))
        (pop positions))
      ;; separate the constituent rhythms and collect them
      (loop for start in positions and end in (cdr positions) collect
         ;; 22.11.11 just leave them as strings otherwise e.g. 16. turns into
         ;; 16 i.e. we lose the dot 
         ;;(read-from-string (subseq strg (+ start 1) end))))))
           (subseq strg (+ start 1) end)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Get the right scaler for the number of dots in the rhythm.
;;; e.g. "w.." => 1.75, "q." => 1.5, "h..." => 1.875.

(defun dots (strg)
  (let* ((num (count-if #'(lambda (x) (char= x #\.)) strg :start 1))
         (scaler (loop for i from 1 to num with result = 1.0 and dot = 0.5 do 
                   (incf result dot) 
                   (setf dot (/ dot 2)) 
                   finally (return result))))
    (list scaler num)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rhythm-p (thing)
  (typep thing 'rhythm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; How many quarter notes does this duration in seconds represent at this
;;; tempo?  

(defun quarters (dur-secs &optional (tempo 60.0))
  (* dur-secs (/ tempo 60.0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Try and get a single rhythm for a duration but if necessary call
;;; rationalize-for-events.
;;; Always returns a list even if only one rhythm is necessary.

(defun rationalize-if-necessary (dur-secs &key (tempo 60.0) (rest t) 
                                 (error-on-fail t)
                                 ;; go for several simple rthms
                                 ;; rather than a single one as
                                 ;; e.g. tuplet with dots 
                                 (keep-it-simple nil))
  ;; (setf dur-secs (decimal-places dur-secs 7))
  (let* ((doddle (get-rhythm-letter-for-duration dur-secs
                                                 :tempo tempo :warn nil))
         (dod-r (if rest
                    (make-rest doddle)
                    (make-rhythm doddle))))
    ;;(format t "rationalize-if-necessary dur-secs:~a,dod-r:~a" dur-secs dod-r)
    (if (and dod-r
             (or (not keep-it-simple)
                 (< (num-dots dod-r) 2)))
        (list dod-r)
        (let ((more (rationalize-for-division dur-secs :rest rest
                                              :tempo tempo)))
          (if more
              more
              ;; MDE Mon Mar 19 17:34:24 2012 -- float errors...
              (if (setf more
                        (let ((ric (round-if-close (/ 1.0 dur-secs) 0.0001)))
                          (when (integerp ric)
                            (/ 1 ric))))
                  (list (let ((r (get-rhythm-letter-for-duration
                                  more :tempo tempo :warn nil)))
                          (unless r
                            (setf r more))
                          (if rest (make-rest r) (make-rhythm r))))
                  (when error-on-fail
                    (error "rhythm::rationalize-if-necessary: ~
                           can't get rhythms for ~a"
                           dur-secs))))))))
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sometimes we get a number like .416666... which we can't turn into a single
;;; rhythm.  But this is simply 5/12 = te+t32 so try and get that out of the
;;; number. 

(defun rationalize-for-division (dur-secs &key (rest t) (tempo 60.0))
  ;; (print 'here)
  ;; converting dur-secs from double-float to short-float irons out some
  ;; rounding errors.
  ;; MDE Tue Mar 20 10:06:58 2012 -- no longer coerce but do round
  (let* ((ratio (rationalize (decimal-places dur-secs 6)))
           ;;(coerce dur-secs 'short-float)))
         (denom (denominator ratio)))
    ;; (print ratio)
    (loop                 ;; try subtracting ratios to see if we get rhythms
       for i from 1 to 3  ;; '(-1 1 -2 2 -3 3) 
       for n1 = (/ i denom)
       for n2 = (- ratio n1)
       for l1 = (get-rhythm-letter-for-duration n1 :warn nil :tempo tempo)
       ;; MDE Mon Mar 19 17:10:24 2012 
       ;; for l2 = (get-rhythm-letter-for-duration n2 :warn nil :tempo tempo)
       for l2 = (unless (zerop n2)
                  (get-rhythm-letter-for-duration n2 :warn nil :tempo tempo))
       do
       (when (and l1 l2)
         (return (if rest
                     (list (make-rest l2) (make-rest l1))
                     (list (make-rhythm l2) (make-rhythm l1))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-rhythm-letter-for-duration (dur-secs
                                       &key (tempo 60.0) 
                                            (warn t) 
                                            (error-on-fail nil))
  (when (zerop dur-secs)
    (error "rhythm::get-rhythm-letter-for-duration: dur-secs is 0!"))
  (let ((result
         (get-rhythm-letter-for-value (* 4.0
                                         (/ 1.0 
                                            (* dur-secs
                                               (/ tempo 60.0))))
                                      warn)))
    (when (and (not result) error-on-fail)
      (error "rhythm::get-rhythm-letter-for-duration: can't get rhythm for ~
              duration ~a seconds" dur-secs))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; e.g. (GET-RHYTHM-LETTER-FOR-VALUE 6) -> TQ
;;;      (GET-RHYTHM-LETTER-FOR-VALUE 2.6666667) -> Q.

(defun get-rhythm-letter-for-value (value &optional (warn t))
  ;; some floating-point operations will have degraded our value so that what
  ;; should be an integer is something like 23.999999999999.  
  (let ((val (round-if-close value 0.0001))
        (1-dot (* value 1.5))
        (2-dots (* value 1.75))
        (3-dots (* value 1.875))
        (tuplet "")
        (letter nil)
        (ok t)
        result
        (num-dots 0))
    (flet 
        ((parse-error 
             (hint)
           (when warn
             (warn "rhythm::get-rhythm-letter-for-value: can't unparse ~a (~a)"
                   value hint))
           (setf ok nil)))
      (unless (whole-num-p value t)
        (loop for d in (list 1-dot 2-dots 3-dots)
           and dots from 1 do
             (when (whole-num-p d t)
               (setf num-dots dots
                     val d)
               (return))))
      ;; MDE Thu May 28 19:55:51 2015 -- both these cases could trigger with a
      ;; value like 60 but in that case we'll catch the problem below  
      (when (zerop (mod val 3)) ;; triplet
        (setf tuplet "t"
              val (* val 2/3)))
      (when (zerop (mod val 5)) ;; quintuplet
        (setf tuplet "f"
              val (* val 4/5)))
      (unless (whole-num-p val t)
        (parse-error "tuplet"))
      (setf letter (case (round val)
                     (1 'w)
                     (2 'h)
                     (4 'q)
                     (8 'e)
                     (16 's)
                     (32 32)
                     (64 64)))
      (unless letter
        (parse-error "letter"))
      (when ok
        ;; MDE Thu May 28 19:48:40 2015 -- this used to have a values call in it
        (setf result (read-from-string 
                      (format nil "~a~a~a" 
                              tuplet letter 
                              (make-string num-dots :initial-element #\.)))))
      ;; MDE Thu May 28 19:55:09 2015 -- now do a sanity check
      (when result
        (unless (equal-within-tolerance value (value (make-rhythm result)))
          (when warn
            (warn "rhythm::get-rhythm-letter-for-value: can't do ~a (got ~a)"
                  value result))
          (setf result nil)))
      result)))
            

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-tied (rthms)
  (when (> (length rthms) 1)
    (setf (is-tied-from (first rthms)) t
          (is-tied-to (first (last rthms))) t)
    (loop for r in (butlast (rest rthms)) do
          (setf (is-tied-from r) t
                (is-tied-to r) t)))
  rthms)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun force-rests (rthms &optional clone)
  (loop for r in rthms collect 
       (force-rest (if clone 
                       (clone r) 
                       r))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 22.12.11 SAR: Added robodoc info

;;; ****f* rhythm/rhythm-list
;;; DESCRIPTION
;;; Create a list of rhythms from symbols, possibly involving ties and not
;;; needing meters etc. (i.e. not as strict as rthm-seq). 
;;;
;;; ARGUMENTS  
;;; - The list of rhythm symbols.
;;;
;;; OPTIONAL ARGUMENTS
;;; - T or NIL indicates whether to create a circular-sclist from the
;;;   result. If NIL, a simple list will be returned (default = NIL). 
;;; 
;;; RETURN VALUE   
;;; A list or circular-sclist of the rhythm objects.
;;; 
;;; EXAMPLE
#|
;; Create a list of rhythm objects
(rhythm-list '(q w+e q. h.+s e.+ts))

=>(
RHYTHM: value: 4.0f0, duration: 1.0
[...]    
RHYTHM: value: 1.0f0, duration: 4.0
[...]    
RHYTHM: value: 8.0f0, duration: 0.5
[...]    
RHYTHM: value: 2.6666666666666665, duration: 1.5
[...]    
RHYTHM: value: 1.3333333333333333, duration: 3.0
[...]    
RHYTHM: value: 16.0f0, duration: 0.25
[...]    
RHYTHM: value: 5.333333333333333, duration: 0.75
[...]    
RHYTHM: value: 24.0f0, duration: 0.16666666666666666
)

;; Collect the data from each of the individual rhythm objects in the list. 
(let ((rl (rhythm-list '(q w+e q. h.+s e.+ts))))
  (print (loop for r in rl collect (data r))))

=> (Q "W" "E" Q. "H." "S" "E." "TS")

;; Set the optional argument to T to create a circular-sclist instead
(rhythm-list '(q w+e q. h.+s e.+ts) t)

=>
CIRCULAR-SCLIST: current 0
SCLIST: sclist-length: 8, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: NIL, tag: NIL, 
data: (
[...]
)

;; Create a circular-sclist and check that it's a circular-sclist using cscl-p 
(let ((rl (rhythm-list '(q w+e q. h.+s e.+ts) t)))
  (cscl-p rl))

=> T

|#
;;; 
;;; SYNOPSIS
(defun rhythm-list (rthms &optional circular)
;;; ****                                ;
  ;; MDE Sat Jun  8 18:51:09 2013 -- need to remove things like { ;
  (let* ((rs (first (parse-rhythms rthms 0)))
         (result
          (loop for r in rs 
            ;; make sure we get compound-duration to reflect ties ;
             for rhythm = (parse-possibly-compound-rhythm r t)
             #|                         ;
             collect 
             ;; if we had something like h+e we can throw away the e as we've
             ;; got its compound-duration in the h 23.3.11: Hmm the above seems
             ;; to be BS so collect everything
             (if (listp rhythm)
               (first rhythm)
               rhythm))))
               |# 
            if (and rhythm (listp rhythm))
              append rhythm
            else if rhythm collect rhythm)))
    ;; 23.3.11 now update is-tied-from to reflect ties....
    (loop for r1 in result and r2 in (cdr result) do
         (when (is-tied-to r2)
           (setf (is-tied-from r1) t)))
    (if circular 
        (make-cscl result)
        result)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rhythms-as-symbols (rthms)
  (loop for r in rthms collect (data r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; MDE Fri Dec 23 22:53:26 2011 
(defun validate-mark (mark)
  ;; MDE Sat Dec 24 12:32:42 2011 -- remember clefs are handled differently
  ;; i.e. not by lp-get-mark and get-cmn-marks  
  (unless (or (cmn-mark-p mark) (clef-list-p mark))
    (when (and (get-sc-config 'warn-no-lp-mark)
               (not (lp-get-mark mark :silent t)))
      (warn "~&rhythm::validate-mark: no LilyPond mark for ~a (but ~
             adding anyway)." mark))
    #+cmn
    (when (and (get-sc-config 'warn-no-cmn-mark)
               (not (cmn::get-cmn-marks mark :silent t)))
      (warn "~&rhythm::validate-mark: no CMN mark for ~a (but adding anyway)."
            mark))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Sat Dec 24 12:34:40 2011 -- marks for clefs are stored as e.g. (clef
;;; treble)  
(defun clef-list-p (thing)
  (and (listp thing)
       (eq (first thing) 'clef)
       (is-clef (second thing))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Fri Dec 23 22:53:26 2011 

(defun cmn-mark-p (thing)
  #+cmn 
  (or (typep thing 'cmn::sundry)
      (typep thing 'cmn::dynamics)
      (typep thing 'cmn::self-acting)
      (typep thing 'cmn::text)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun has-mark-aux (list mark test)
  (member mark list :test test))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; MDE Sat Jun  8 18:23:28 2013 -- (error t) is there so we can test if things
;;; are parseable as rhythms.  This was originally the parse-rhythm method but
;;; I've split things out now.

(defun parse-rhythm-symbol (rthm &key rhythm-object (error t))
  (let* ((num-dots 0)
         (undotted-value 0.0)
         (tuplet-scaler 1)
         (scaler 1.0)
         (value 0.0)
         (letter-value nil)
         (len 0)
         (string (format nil "~a" (rm-package rthm)))
         (dots-scaler 1)
         (rq nil)
         (first-char nil))
    (if (numberp rthm)
        (setf letter-value rthm)
        (progn
          (when (char= #\. (aref string (1- (length string))))
            (let ((dots (dots string)))
              (setq num-dots (second dots)
                    dots-scaler (/ (first dots))))
            ;; backslashes protect the dot in eg 14\.
            (setq string (string-right-trim "\." string)))
          (setf tuplet-scaler (case (aref string 0) 
                                (#\T 3/2)
                                (#\F 5/4)
                                (t 1))
                scaler (* dots-scaler tuplet-scaler)
                len (progn (when (and (/= dots-scaler scaler)
                                      ;; MDE Tue Jul 24 20:57:00 2012 
                                      (> (length string) 1))
                             (setq string (subseq string 1)))
                           (length string))
                first-char (aref string 0)
                letter-value 
                (if (> len 0) ;; i.e. if rthm was a valid argument!
                    (if (digit-char-p first-char)
                        (read-from-string string)
                        ;; string should now only contain one letter
                        (if (= len 1)  
                            (case first-char
                              ;; as a side effect the is-grace-note slot of the
                              ;; rhythm object is set here and a duration of 0
                              ;; is given.
                              (#\G (when rhythm-object
                                     (setf (is-grace-note rhythm-object) t))
                                   0)
                              (#\W 1)
                              (#\H 2)
                              (#\Q 4)
                              (#\E 8)
                              (#\S 16))))))))
    ;; MDE Sat Jun  8 18:20:53 2013 -- only signal an error if we really want
    ;; one
    (if letter-value
        (progn
          (setf value (float (* letter-value scaler))
                undotted-value (* letter-value tuplet-scaler))
          (when rhythm-object
            (setf (undotted-value rhythm-object) undotted-value
                  ;; get the rq for cmn
                  rq (if (is-grace-note rhythm-object)
                         0
                         (* (/ 4 letter-value) (/ tuplet-scaler)
                            (/ (rationalize dots-scaler))))
                  (rq rhythm-object) rq ;; (cmn::rq rq)
                  (value rhythm-object) value
                  ;; set duration to be the duration in seconds when qtr = 60
                  (duration rhythm-object) (if (zerop value) 
                                               0.0 
                                               (/ 4.0 value))
                  (compound-duration rhythm-object) (duration rhythm-object)
                  (num-dots rhythm-object) num-dots
                  ;; 30.1.11 next two for lilypond
                  ;; MDE Wed Jun 24 17:25:41 2015 -- this used to just be
                  ;; letter-value but as we now want to use the slot for
                  ;; Lilypond rhythmic values we need powers of 2.
                  (letter-value rhythm-object) 
                  (if (zerop letter-value) 0 (nearest-power-of-2 letter-value))
                  ;; invert it now
                  (tuplet-scaler rhythm-object) (/ tuplet-scaler) 
                  (num-flags rhythm-object) (rthm-num-flags undotted-value)
                  (score-rthm rhythm-object) 
                  (format nil "~a~a" 
                          (float (* letter-value tuplet-scaler))
                          (make-string num-dots 
                                       :initial-element #\.)))
            ;; MDE Sat Mar 10 17:37:51 2012 -- for lilypond: if we have a plain
            ;; 12 as a rhythm, tuplet-scaler would be 1, because there's no F or
            ;; T in front of a letter value, but we need it to be 3/2 as 12 is a
            ;; te so try and figure it out mathematically
            (when (and (= 1 (tuplet-scaler rhythm-object))
                       ;; MDE Sat Jun 27 16:02:07 2015 -- should be an int not
                       ;; just a number as 70/3 is a number... 
                       ;; (numberp (data rhythm-object)))
                       (integerp (data rhythm-object)))
              (setf (tuplet-scaler rhythm-object) (rationalize
                                                   (/ (nearest-power-of-2
                                                       (value rhythm-object))
                                                      (value rhythm-object))))))
          value)
        (when error
          (error "rhythm::parse-rhythm: ~a is not a valid argument."
                 rthm)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rm-marks-aux (object marks warn)
    (unless (listp marks)
    (setf marks (list marks)))
  (loop for m in marks do
       (if (member m (marks object))
           ;; #'equal so that sub-lists can be removed too
           (setf (marks object) (remove m (marks object) :test #'equal))
           (when warn
             (warn "rhythm::rm-marks: no mark ~a in ~a ~a"
                   m (marks object)
                   (if (event-p object)
                       (format nil " (bar ~a)" (bar-num object))
                       ""))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Thu Jun 25 09:40:21 2015

(defun get-tuplet-ratio (tuplet)
  (flet ((terr () (error "rhythm::get-tuplet-ratio: unhandled tuplet: ~a"
                         tuplet)))
    (typecase tuplet
      (integer (case tuplet
                 (2 3/2)
                 (3 2/3)
                 (4 3/4)
                 (5 4/5)
                 (6 2/3)
                 (7 4/7)
                 (8 3/4)
                 (9 8/9)
                 (10 8/10)
                 (11 8/11)
                 (13 8/13)
                 (t (terr))))
      (rational tuplet) ; i.e we've already got something like 7/5 (= 7:5)
      (t (terr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF rhythm.lsp

