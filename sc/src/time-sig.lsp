;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* linked-named-object/time-sig
;;; NAME 
;;; time-sig
;;;
;;; File:             time-sig.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> sclist -> time-sig
;;;
;;; Version:          1.0.0-beta2
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Implementation of a time-sig class that stores
;;;                   information about time signatures, allows comparison of
;;;                   time signatures etc. 
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    12th February 2001
;;;
;;; $$ Last modified: 15:29:07 Sat Jun  9 2012 BST
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

(eval-when (compile)
  (declaim (optimize (speed 3) (safety 1) (space 0) (debug 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :slippery-chicken)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The time sig is stored as a list in the data slot, then the denom and num
;; are taken from this list and stored separately.

(defclass time-sig (sclist)
  ;; the denominator, or beat type
  ((denom :accessor denom :type integer :initform -1)
   ;; the numerator or number of beats per bar
   (num :accessor num :type integer :initform -1)
   ;; this is in secs which is the same as duration in crotchets (=60) of
   ;; course.  
   (duration :accessor duration :type float :initform -1.0)
   ;; the number of MIDI clocks per beat; there are 24 MIDI clocks in a
   ;; crotchet 
   (midi-clocks :accessor midi-clocks :type integer :initarg :midi-clocks
                :initform 24) 
   ;; how many beats does this time sig have e.g. 4/4=4, 6/8=2
   (num-beats :accessor num-beats :type integer)
   ;; we're going to try and work out whether this is a simple or compound time
   ;; signature...  
   (compound :accessor compound :type boolean :initform nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defmethod initialize-instance :after ((i time-sig) &rest initargs)
;;  (declare (ignore initargs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone ((ts time-sig))
  (clone-with-new-class ts 'time-sig))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone-with-new-class :around ((ts time-sig) new-class)
  (declare (ignore new-class))
  (let ((sclist (call-next-method)))
    (setf (slot-value sclist 'denom) (denom ts)
          (slot-value sclist 'num) (num ts)
          (slot-value sclist 'num-beats) (num-beats ts)
          (slot-value sclist 'compound) (compound ts)
          (slot-value sclist 'midi-clocks) (midi-clocks ts)
          (slot-value sclist 'duration) (duration ts))
    sclist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object :before ((i time-sig) stream)
  (format stream "~%TIME-SIG: num: ~a, denom: ~a, duration: ~a, compound: ~a, ~
                  midi-clocks: ~a, num-beats: ~a"
          (num i) (denom i) (duration i) (compound i) (midi-clocks i)
          (num-beats i))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod verify-and-store :after ((ts time-sig))
  (let ((data (data ts)))
    (when (and data (not (= 2 (sclist-length ts))))
      (error "time-sig::verify-and-store: ~
              The :data slot to time-sig must be a 2-element list: ~a" 
             data))
    (when data
      (let ((num (first data))
            (denom (second data)))
        (unless (and (integer>0 num)
                     (integer>0 denom)
                     ;; for now let's be strict: only normal power of two
                     ;; denominators are allowed.  
                     (zerop (rem (log denom 2) 1)))
          (error "time-sig::verify-and-store: ~
                  Invalid time signature: ~a (numerator must be an integer and~
                  ~%denominator must be a power-of-two integer)" data))
        ;; ok, data was acceptable...
        (setf (num ts) num
              (denom ts) denom
              ;; 26.1.11 make a unique id out of the time-sig
              (id ts) (format nil "~2,'0d~2,'0d" num denom)
              (duration ts) (* num (/ 4.0 denom))
              (compound ts) (is-compound ts)
              (num-beats ts) (if (compound ts) (/ num 3) num))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod rq-duration ((ts time-sig))
  (rationalize (duration ts)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* time-sig/beat-duration

;;; DESCRIPTION
;;; Get the duration in seconds of one beat of the given time-signature at a
;;; tempo of quarter=60.
;;; 
;;; ARGUMENTS
;;; - A time-sig object.
;;; 
;;; RETURN VALUE
;;; A number.
;;; 
;;; EXAMPLE
#|
;; Beat duration in seconds for time-signature 2/4 at quarter=60
(let ((ts (make-time-sig '(2 4))))
  (beat-duration ts))

=> 1.0

;; Beat duration in seconds for 6/8 at quarter=60
(let ((ts (make-time-sig '(6 8))))
  (beat-duration ts))

=> 1.5

|#
;;; SYNOPSIS
(defmethod beat-duration ((ts time-sig))
;;; ****
  (let ((denom-dur (/ 4.0 (denom ts))))
    (if (compound ts)
        (* 3 denom-dur)
      denom-dur)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR 30.12.11 Added robodoc info

;;; ****m* time-sig/get-beat-as-rhythm
;;; DESCRIPTION
;;; Get the beat unit of a given time-sig object and return it as a rhythm.
;;; 
;;; ARGUMENTS
;;; - A time-sig object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether to consider the beat of a compound meter to  
;;;   be the denominator of the time signature (such as 8 for 6/8) or the beat  
;;;   duration derived from the traditionally understood beat of that meter
;;;   (such as Q. for 6/8). NIL = denominator. Default = NIL. 
;;; 
;;; RETURN VALUE
;;; A rhythm object.
;;; 
;;; EXAMPLE
#|
;; Returns a rhythm object
(let ((ts (make-time-sig '(2 4))))
  (get-beat-as-rhythm ts))

=> 
RHYTHM: value: 4.000, duration: 1.000, rq: 1, is-rest: NIL, 
        score-rthm: 4.0f0, undotted-value: 4, num-flags: 0, num-dots: 0, 
        is-tied-to: NIL, is-tied-from: NIL, compound-duration: 1.000, 
        is-grace-note: NIL, needs-new-note: T, beam: NIL, bracket: NIL, 
        rqq-note: NIL, rqq-info: NIL, marks: NIL, marks-in-part: NIL, 
        letter-value: 4, tuplet-scaler: 1, grace-note-duration: 0.05
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: 4, tag: NIL, 
data: 4

;; Default for compound meters is to return the denominator of the time
;; signature 
(let ((ts (make-time-sig '(6 8))))
  (data (get-beat-as-rhythm ts)))

=> 8

;; Setting the optional argument to T returns the compound beat of a compound
;; meter rather than the denominator of the time signature
(let ((ts (make-time-sig '(6 8))))
  (data (get-beat-as-rhythm ts t)))

=> Q.

|#
;;; SYNOPSIS
(defmethod get-beat-as-rhythm ((ts time-sig) &optional handle-compound)
;;; ****
  (let ((result (make-rhythm (denom ts))))
    (if (and handle-compound
             (is-compound ts))
        (scale result 3)
        result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
;;; MDE Sat Dec 24 11:46:35 2011 -- now obsolete

(defmethod score-time-sig ((ts time-sig))
  (format nil "~a ~a" (num ts) (denom ts))) 

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; NB Always returns a new time-sig.  By default we divide the denominator by
;;; the scaler rather than the numerator * the scaler, thus preserving meter
;;; (duple, triple etc.).  If that doesn't give us a 'normal' meter we'll do
;;; numerator * scaler instead.  

;;; SAR Fri Dec 30 17:37:55 EST 2011: Added robodoc info

;;; ****m* time-sig/scale
;;; DESCRIPTION
;;; Scale the value of the given time-sig object by a specified factor.
;;; 
;;; ARGUMENTS
;;; - A time-sig object.
;;; - A number (scaling factor).
;;; 
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether or not to preserve the meter by maintaining 
;;;   the same number of beats as the numerator of the time signature. T = 
;;;   preserve the meter. Default = T. 
;;; 
;;; RETURN VALUE
;;; A time-sig object.
;;; 
;;; EXAMPLE

#|
;; Scaling a (2 4) time-sig object by 3 creates a new time-sig object with a
;; value of 6/4 
(let ((ts (make-time-sig '(2 4))))
  (scale ts 3))

=> 
TIME-SIG: num: 6, denom: 4, duration: 6.0, compound: NIL, midi-clocks: 24, num-beats: 6
SCLIST: sclist-length: 2, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: "0604", tag: NIL, 
data: (6 4)

;; Scaling a (2 4) time-sig object by 2 by default preserves the meter
(let ((ts (make-time-sig '(2 4))))
  (data (scale ts 2)))

=> (2 2)

;; Scaling a (2 4) time-sig object by 2 with the optional argument set to NIL
;; changes the meter and results in a 4/4
(let ((ts (make-time-sig '(2 4))))
  (data (scale ts 2 nil)))

=> (4 4)

;; Halving the value of a time-sig object is achieved using a factor of .5
(let ((ts (make-time-sig '(2 4))))
  (data (scale ts .5)))

=> (2 8)

|#

;;; SYNOPSIS
(defmethod scale ((ts time-sig) scaler
                  &optional (preserve-meter t) ignore1 ignore2)
;;; ****
  (declare (ignore ignore1) (ignore ignore2))
  (let ((denom-scaled (/ (denom ts) scaler))
        (num-scaled (* (num ts) scaler))
        tmp)
    ;; for now we force denominators of 2,4,8... so not allowing e.g. 4/5 a la
    ;; Ferneyhough 
    (cond ((and preserve-meter (power-of-2 denom-scaled))
           ;; got to floor it as we might have a float e.g. 8.0
           (make-time-sig (list (num ts) (floor denom-scaled))))
          ;; OK scale the numerator instead
          ((and (> num-scaled 0) (float-int-p num-scaled))
           (setf num-scaled (floor num-scaled)
                 tmp (denom ts))
           ;; prefer e.g. 2/8 to 1/4
           (when (= num-scaled 1)
             (setf num-scaled 2
                   tmp (* tmp 2)))
           (make-time-sig (list num-scaled tmp)))
          (t (error "~a~&time-sig::scale: using a scaler of ~a, can't create a ~
                     valid new time signature" ts scaler)))))
          
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri Dec 30 17:57:59 EST 2011: Added robodoc info

;;; ****m* time-sig/is-compound
;;; DESCRIPTION
;;; Determine whether the value of a given time-sig object is a compound time
;;; signature. 
;;; 
;;; ARGUMENTS
;;; - A time-sig object.
;;; 
;;; RETURN VALUE
;;; T if the value of the given time-sig object is a compound time signature,
;;; otherwise NIL.
;;; 
;;; EXAMPLE
#|
;; Testing a time-sig object with a 2/4 time signature returns NIL
(let ((ts (make-time-sig '(2 4))))
  (is-compound ts))

=> NIL

;; Testing a time-sig object with a 6/8 time signature returns T
(let ((ts (make-time-sig '(6 8))))
  (is-compound ts))

=> T

|#
;;; SYNOPSIS
(defmethod is-compound ((ts time-sig))
;;; ****
  ;; I know this isn't really true, but only consider time sigs with beat types
  ;; >= 8 to be potentially compound
  ;; TODO: adjust midi-clocks accordingly
  (when (>= (denom ts) 8)
    (zerop (mod (num ts) 3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri Dec 30 18:04:00 EST 2011: Added robodoc info

;;; ****m* time-sig/time-sig-equal
;;; DESCRIPTION
;;; Determine whether the values of two given time-sig objects are the same. If
;;; they are identical in signature, return T; if they are different signatures
;;; but have the same duration (e.g. 2/4, 4/8, 8/16 etc.) return
;;; TIME-SIG-EQUAL-DURATION; otherwise return NIL.
;;; 
;;; ARGUMENTS
;;; - A first time-sig object.
;;; - A second time-sig object.
;;; 
;;; RETURN VALUE
;;; Returns T if the time signatures are identical; returns
;;; TIME-SIG-EQUAL-DURATION if they are different signatures with the same
;;; duration; otherwise NIL.
;;; 
;;; EXAMPLE
#|
;; Two identical signatures return T
(let ((ts1 (make-time-sig '(2 4)))
      (ts2 (make-time-sig '(2 4))))
  (time-sig-equal ts1 ts2))

=> T

;; Two different signatures of the same duration return TIME-SIG-EQUAL-DURATION 
(let ((ts1 (make-time-sig '(2 4)))
      (ts2 (make-time-sig '(4 8))))
  (time-sig-equal ts1 ts2))

=> TIME-SIG-EQUAL-DURATION

;; Two completely different signatures return NIL
(let ((ts1 (make-time-sig '(2 4)))
      (ts2 (make-time-sig '(3 4))))
  (time-sig-equal ts1 ts2))

=> NIL

|#
;;; SYNOPSIS
(defmethod time-sig-equal ((ts1 time-sig) (ts2 time-sig))
;;; ****
  (cond ((and (= (denom ts1) (denom ts2))
              (= (num ts1) (num ts2)))
         t)
        ((= (duration ts1) (duration ts2)) 'time-sig-equal-duration)
        (t nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Fri Dec 30 18:14:43 EST 2011: Added robodoc info

;;; ****m* time-sig/get-whole-bar-rest
;;; DESCRIPTION
;;; Create an event object consisting of a rest equal in duration to one full
;;; bar of the given time-sig object.
;;; 
;;; ARGUMENTS
;;; - A time-sig object.
;;; 
;;; RETURN VALUE
;;; Returns an event object.
;;; 
;;; EXAMPLE
#|
;; Returns an event object
(let ((ts (make-time-sig '(2 4))))
  (get-whole-bar-rest ts))

=> 
EVENT: start-time: NIL, end-time: NIL, 
       duration-in-tempo: 0.0, 
       compound-duration-in-tempo: 0.0, 
       amplitude: 0.7 
       bar-num: -1, marks-before: NIL, 
       tempo-change: NIL 
       instrument-change: NIL 
       display-tempo: NIL, start-time-qtrs: -1, 
       midi-time-sig: NIL, midi-program-changes: NIL, 
       8va: 0
       pitch-or-chord: NIL
       written-pitch-or-chord: NIL
RHYTHM: value: 2.000, duration: 2.000, rq: 2, is-rest: T, 
        score-rthm: 2.0f0, undotted-value: 2, num-flags: 0, num-dots: 0, 
        is-tied-to: NIL, is-tied-from: NIL, compound-duration: 2.000, 
        is-grace-note: NIL, needs-new-note: NIL, beam: NIL, bracket: NIL, 
        rqq-note: NIL, rqq-info: NIL, marks: NIL, marks-in-part: NIL, 
        letter-value: 2, tuplet-scaler: 1, grace-note-duration: 0.05
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: 2, tag: NIL, 
data: 2

;; The rhythmic value of the event object returned is equal to the rhythmic
;; duration of a full bar in the given time signature, the PITCH-OR-CHORD slot
;; is set to NIL, and the IS-REST slot is set to T.
(let* ((ts (make-time-sig '(2 4)))
       (tswbr (get-whole-bar-rest ts)))
  (print (value tswbr))
  (print (pitch-or-chord tswbr))
  (print (is-rest tswbr)))

=>
2.0 
NIL 
T

|#
;;; SYNOPSIS
(defmethod get-whole-bar-rest ((ts time-sig))
;;; ****
  (make-rest (/ (denom ts) (num ts))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; used by slippery-chicken::tie-over-rests to fill a bar
(let ((rthms (loop 
                 for r in '(w.. w. w h.. h. h q.. q. q e.. e. e s. s 32. 32)
                 for e = (make-event nil r)
                 do
                   (setf (needs-new-note e) nil)
                 collect e)))
  (defmethod events-for-full-bar ((ts time-sig) pitch-or-chord
                                  written-pitch-or-chord
                                  &key
                                  (tie-to-first t)
                                  (tie-from-last t)
                                  (tie-in-bar t))
    (let ((result '())
          (bar-dur (duration ts))
          e-clone first last)
      (loop 
          for e in (if (>= bar-dur 8.0) ;; start with w if 8/8 or higher
                       (rest (rest rthms))
                     rthms)
          for e-dur = (duration e) 
          while (> bar-dur 0.0) do
            (loop while (<= e-dur bar-dur) do
              (decf bar-dur e-dur)
              (setf e-clone (clone e) 
                    (pitch-or-chord e-clone) (clone pitch-or-chord))
              (when written-pitch-or-chord
                (setf (written-pitch-or-chord e-clone) 
                  (clone written-pitch-or-chord)))
              ;; (format t "~&acc in ts: ~a" (show-accidental pitch-or-chord))
                 ;; (print e-clone)
              (push e-clone result)))
      (unless (zerop bar-dur)
        (error "time-sig::events-for-full-bar: can't get full bar for ~a"
               (data ts)))
      (setf result (nreverse result)
            first (first result)
            last (first (last result)))
      (when tie-to-first 
        (setf (is-tied-to first) t))
      (when tie-from-last
        (setf (is-tied-from last) t))
      (when tie-in-bar
        (setf (is-tied-from first) t
              (is-tied-to last) t)
        (loop for e in (rest (butlast result)) do
              (setf (is-tied-from e) t)))
      result)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Thu Dec 29 20:21:17 EST 2011

;;; ****f* time-sig/make-time-sig
;;; DESCRIPTION
;;; Create a time-sig object. In addition to the numerator and denominator
;;; values, the object also stores other automatically calculated information,
;;; such as whether the signature is simple or compound, the duration of one
;;; bar of the given time signature in seconds, the number of midi-clocks, etc. 
;;; 
;;; ARGUMENTS
;;; - A two-item list of numbers, the first being the numerator (number of
;;;   beats per measure), the second being the denominator (beat type). 
;;; 
;;; RETURN VALUE
;;; - A time-sig object.
;;; 
;;; EXAMPLE
#|
(make-time-sig '(2 4))

=> 
TIME-SIG: num: 2, denom: 4, duration: 2.0, compound: NIL, midi-clocks: 24, num-beats: 2
SCLIST: sclist-length: 2, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: "0204", tag: NIL, 
data: (2 4)

|#
;;; SYNOPSIS
(defun make-time-sig (ts)
;;; ****
  (cond ((time-sig-p ts) ts)
        ((not ts) (error "make-time-sig: argument can't be nil!"))
        (t (make-instance 'time-sig :data ts))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun time-sig-p (thing)
  (typep thing 'time-sig))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This tries to generate a reasonable time signature from a given duration in
;;; seconds.  Will not do anything sophisticated like decide between simple and
;;; compound times.
;;; 
;;; (make-time-sig-from-duration 2) -> 2/4
;;; (make-time-sig-from-duration 1.75) -> 7/16
;;; 
;;; This will usually result in a numerator >= 2, but tt's not always easy to
;;; decide on the best time-sig, e.g. 2/8 is usually preferable to 1/4, but
;;; 1/16 is better than 2/32.  Hence there's an optional 'prefer' arg that will
;;; substitute less-preferable to more-preferable time-sigs.

(defun make-time-sig-from-duration (dur-secs
                                    &optional 
                                    (tempo 60.0))
  (let* ((quarters (quarters dur-secs tempo)))
    (unless (almost-zero (rem quarters 0.125))
      (error "time-sig::make-time-sig-from-duration: can't make a time ~
              signature from ~a (tempo ~a, quarters ~a)"
             dur-secs tempo quarters))
    (loop ;; this has to get to the return statement, no?!!
        for div in '(2.0 1.0 0.5 0.25 0.125)
        for denom in '(2 4 8 16 32)
        for num = (floor quarters div)
        do
          (when (and (> num 1)
                     (almost-zero (rem quarters div)))
            (return (make-time-sig
                     (get-preferred-time-sig (list num denom))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Use more common time-sigs instead of more arcane.  argument and result is a
;;; list like '(8 8).  If there's no preferred time-sig the argument will be
;;; returned. 
(defun get-preferred-time-sig (given 
                               &optional
                               ;; these need to be given pairwise: the
                               ;; preferred time-sig first, the one it should
                               ;; substitute second 
                               (preferred '(((1 16) (2 32))
                                            ((2 4) (4 8))
                                            ;; MDE Sat Feb 11 11:44:53 2012 
                                            ((4 4) (8 8))
                                            ;; MDE Sat Dec 24 13:00:09 2011
                                            ((1 4) (2 8))
                                            ((1 8) (2 16))
					    ;; SAR Fri Jun 22 13:15:16 BST 2012
					    ((3 8) (6 16))
					    ((5 4) (10 16)))))
  (let ((result (loop
                   for pair in preferred 
                   for yes = (first pair)
                   for no = (second pair)
                   do
                   (when (equal given no)
                     (return yes)))))
    (if result result given)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EOF time-sig.lsp

