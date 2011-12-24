;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* linked-named-object/time-sig
;;; NAME 
;;; time-sig
;;;
;;; File:             time-sig.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> sclist -> time-sig
;;;
;;; Version:          1.0
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
;;; $$ Last modified: 11:48:05 Sat Dec 24 2011 ICT
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
(defmethod beat-duration ((ts time-sig))
;;; ****
  (let ((denom-dur (/ 4.0 (denom ts))))
    (if (compound ts)
        (* 3 denom-dur)
      denom-dur)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* time-sig/get-beat-as-rhythm
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
(defmethod get-beat-as-rhythm ((ts time-sig) &optional (handle-compound))
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

;;; ****m* time-sig/scale
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
          ((and (> num-scaled 0) (float-int-p num-scaled))
           (setf num-scaled (floor num-scaled)
                 tmp (denom ts))
           (when (= num-scaled 1)
             (setf num-scaled 2
                   tmp (* tmp 2)))
           (make-time-sig (list num-scaled tmp)))
          (t (error "~a~&time-sig::scale: using a scaler of ~a, can't create a ~
                     valid new time signature" ts scaler)))))
          
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* time-sig/is-compound
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
(defmethod is-compound ((ts time-sig))
;;; ****
  ;; I know this isn't really true, but only consider time sigs with beat types
  ;; >= 8 to be potentially compound
  ;; TODO: adjust midi-clocks accordingly
  (when (>= (denom ts) 8)
    (zerop (mod (num ts) 3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; if the time-sigs are really the same, return true; if they have the same
;;; duration (2/4, 4/8, 8/16 etc.) return time-sig-equal-duration; otherwise
;;; nil.

;;; ****m* time-sig/time-sig-equal
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
(defmethod time-sig-equal ((ts1 time-sig) (ts2 time-sig))
  (cond ((and (= (denom ts1) (denom ts2))
              (= (num ts1) (num ts2)))
         t)
        ((= (duration ts1) (duration ts2)) 'time-sig-equal-duration)
        (t nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* time-sig/get-whole-bar-rest
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
(defmethod get-whole-bar-rest ((ts time-sig))
  (make-rest (/ (denom ts) (num ts))))
;;; ****

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
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****f* time-sig/make-time-sig
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

(defun get-preferred-time-sig (given &optional
                                     ;; these need to be given pairwise
                                     (preferred '(((1 16) (2 32))
                                                  ((2 4) (4 8))
                                                  ((1 8) (2 16)))))
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

