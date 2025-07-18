;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* linked-named-object/tempo
;;; NAME 
;;; tempo
;;;
;;; File:             tempo.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> tempo
;;;
;;; Version:          1.1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Implementation of the tempo class which holds very simple
;;;                   tempo information, simply the type of beat and the number
;;;                   of beats per minute etc. 
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    March 11th 2001
;;;
;;; $$ Last modified:  15:51:49 Sat May 24 2025 CEST
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

(defclass tempo (linked-named-object)
  ;; beat can be any rhythmic symbol e.g. 4, 2, q, e, q. etc.
  ((beat :accessor beat :initarg :beat :initform 4)
   ;; the value of beat relative to a whole-note, e.g. q = 4
   (beat-value :accessor beat-value)
   ;; beats per minute, i.e. the actual tempo
   (bpm :accessor bpm :type number :initarg :bpm :initform 60.0)
   ;; e.g. "Allegro con moto"
   (description :accessor description :initarg :description :initform nil)
   ;; the quarter note tempo, no matter what beat is given
   (qtr-bpm :accessor qtr-bpm :type number :initform -1)
   ;; the number of microseconds (usecs) per MIDI quarter note
   (usecs :accessor usecs :type integer :initform -1)
   ;; MDE Mon May  5 17:21:12 2014 -- the duration in seconds of the beat
   (beat-dur :accessor beat-dur :type number)
   ;; the duration in seconds of a quarter note at this tempo
   (qtr-dur :accessor qtr-dur :type number)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone ((tpo tempo))
  (clone-with-new-class tpo 'tempo))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone-with-new-class :around ((tpo tempo) new-class)
  (declare (ignore new-class))
  (let ((named-object (call-next-method)))
    (setf (slot-value named-object 'beat) (beat tpo)
          (slot-value named-object 'beat-value) (beat-value tpo)
          (slot-value named-object 'bpm) (bpm tpo)
          (slot-value named-object 'description) (description tpo)
          (slot-value named-object 'qtr-bpm) (qtr-bpm tpo)
          (slot-value named-object 'usecs) (usecs tpo)
          (slot-value named-object 'beat-dur) (beat-dur tpo)
          (slot-value named-object 'qtr-dur) (qtr-dur tpo))
    named-object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((i tempo) &rest initargs)
  (declare (ignore initargs))
  ;; MDE Sat Mar  5 17:50:56 2016 -- split operations out into a separate
  ;; method so that potential subclasses don't end up doing potentially
  ;; destructive stuff
  (init-tempo i))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Sat Mar  5 17:51:35 2016 -- simply what initialize-instance used to do
(defmethod init-tempo ((i tempo))
  (setf (slot-value i 'bpm) (float (bpm i)) ; added 24/8/24 to avoid xml errors
        (beat-value i) (value (make-rhythm (beat i)))
        (beat-dur i) (/ 60.0 (bpm i))
        (qtr-dur i) (* (beat-dur i) (/ (beat-value i) 4.0))
        (qtr-bpm i) (* (bpm i) (/ 4 (beat-value i)))
        (usecs i) (floor (* 1000000 (qtr-dur i)))
        ;; just for the hell set the data slot of the named-object parent to
        ;; the bpm slot 
        (data i) (bpm i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf bpm) :after (value (i tempo))
  ;; MDE Mon May  5 17:29:59 2014 -- re-init the values
  ;; (setf (data i) value))
  (initialize-instance i))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object :before ((i tempo) stream)
  (format stream "~%TEMPO: bpm: ~a, beat: ~a, beat-value: ~a, beat-dur: ~a, ~
                           qtr-dur: ~a ~
                  ~%       qtr-bpm: ~a, usecs: ~a, description: ~a"
          (bpm i) (beat i) (beat-value i) (beat-dur i) (qtr-dur i) (qtr-bpm i)
          (usecs i) (description i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Thu Dec 29 19:18:02 EST 2011: Added robodoc info

;;; ****m* tempo/tempo-equal
;;; DESCRIPTION
;;; Test to determine whether the values of two tempo objects are equal.
;;; 
;;; ARGUMENTS
;;; - A first tempo object.
;;; - A second tempo object.
;;; 
;;; RETURN VALUE
;;; Returns T if the values of the two tempo objects are equal, otherwise NIL. 
;;; 
;;; EXAMPLE
#|
;; Equal
(let ((tt1 (make-tempo 60))
      (tt2 (make-tempo 60)))
  (tempo-equal tt1 tt2))

=> T

;; Not equal
(let ((tt1 (make-tempo 60))
      (tt2 (make-tempo 96)))
  (tempo-equal tt1 tt2))

=> NIL
|#
;;; SYNOPSIS
(defmethod tempo-equal ((t1 tempo) (t2 tempo))
;;; ****
  (cond ((and (= (bpm t1) (bpm t2))
              (= (beat-value t1) (beat-value t2)))
         t)
        ((= (qtr-dur t1) (qtr-dur t2))
         'qtr-dur-equal)
        (t nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod cmn-tempo ((tpo tempo) &optional (dy 0))
  (let ((mm (cmn::mm (round (bpm tpo)) (eval (rm-package (beat tpo) :cmn))
                     cmn::in-parentheses (cmn::dy dy))))
    (if (description tpo)
        (list mm (cmn::sc-cmn-text (description tpo)))
      (list mm))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-lp-data ((tpo tempo) &optional ignore1 ignore2 ignore3)
  (declare (ignore ignore1 ignore2 ignore3))
  ;; MDE Mon Jun 25 17:29:58 2012 -- 
  (let ((bv (round (beat-value tpo))))
    (unless (power-of-2 bv)
      (setf bv
            (case (beat tpo)
              (s. "16.")
              (e. "8.")
              (q. "4.")
              (h. "2.")
              (w. "1.")
              (t (error "tempo::get-lp-data: Can't get Lilypond tempo for ~
                         beat ~a: ~a" (beat tpo) tpo)))))
    (format nil "\\tempo ~a ~a = ~a "
            (if (description tpo)
                (format nil "\"~a\"" (description tpo))
                "")
            bv
            (round (bpm tpo)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Sat Mar 18 10:40:23 2017
(defmethod write-xml ((tpo tempo) &key stream)
  (format stream "~&      <direction directive=\"yes\" placement=\"above\">")
  (when (description tpo)
    (format stream "~&        <direction-type>~
                    ~&          <words default-y=\"29\">~
                    ~&            ~a~
                    ~&          </words>~
                    ~&        </direction-type>~
                    ~&      </direction>~
                    ~&      <direction>"
            (description tpo)))
  ;; MDE Sat Nov 27 10:25:44 2021, Heidhausen -- finally need to handle tempi
  ;; with dotted beats 
  (let ((beat-unit (xml-simple-rhythm (beat-value tpo) nil)) ; try the easy way
        (dots 0)
        rthm)
    (unless beat-unit                   ; must be dotted
      (setq rthm (make-rhythm (beat tpo))
            beat-unit (xml-simple-rhythm (undotted-value rthm))
            dots (num-dots rthm)))
    (when (> dots 1)
      (error "tempo::write-xml: <dots> should be 0 or 1: ~a" dots))
    (format stream "~&        <direction-type>~
                    ~&          <metronome default-y=\"29\">~
                    ~&            <beat-unit>~a</beat-unit>~a~
                    ~&            <per-minute>~a</per-minute>~
                    ~&          </metronome>~
                    ~&        </direction-type>~
                    ~&        <sound tempo=\"~a\"/>~
                    ~&      </direction>"
            beat-unit
            ;; so this is an example where the xml tag just needs stating,
            ;; without a value (I would have guessed a value of 1 or 0 would be
            ;; fine, but if zero dots then don't include <beat-unit-dot>
            (if (zerop dots)
                ""
                (format nil "~%            <beat-unit-dot></beat-unit-dot>"))
            (bpm tpo) (bpm tpo))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Sat May 24 15:48:08 2025, Heidhausen 
(defmethod get-list ((tpo tempo))
  (let ((l (list (beat tpo) (bpm tpo))))
    (if (description tpo)
      (econs l (description tpo))
      l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun usecs-to-bpm (usecs)
  (round (/ 60.0 (/ usecs 1000000.0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Thu Dec 29 19:21:44 EST 2011: Added robodoc info

;;; ****f* tempo/make-tempo
;;; DESCRIPTION
;;; Make a tempo object.
;;; 
;;; ARGUMENTS
;;; - A number indicating beats per minute. If this is >= 10000 we'll treat the
;;; argument as a usecs slot (number of microseconds quarter note) and calcuate
;;; the BPM from that. That gives us a maximum BPM of 5999 before we start
;;; thinking these are usecs (should be fine).
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :beat. Sets the "beat" value of the beats per minute; i.e., 'q (or 4) for
;;;   "quarter = xx bpm" etc. Default = 4.
;;; - :id. Sets the ID of the tempo object.
;;; - :description. A text description (string) of the tempo, such as "Allegro
;;;   con brio" etc.
;;; 
;;; RETURN VALUE
;;; A tempo object.
;;; 
;;; EXAMPLE
#|
;; Default beat is a quarter, thus the following makes a tempo object of
;; quarter=60. 
(make-tempo 60)

=> 
TEMPO: bpm: 60, beat: 4, beat-value: 4.0, qtr-dur: 1.0 
       qtr-bpm: 60.0, usecs: 1000000, description: NIL
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: NIL, tag: NIL, 
data: 60

;; Set the beat using the :beat keyword argument. Thus, the following makes a
;; tempo object of dotted-quarter = 96.
(make-tempo 96 :beat 'q.)

;; Add a text description, which is stored in the tempo object's DESCRIPTION
;; slot. 
(let ((tt (make-tempo 76 :beat 2 :description "Allegretto")))
  (description tt))

=> "Allegretto"

|#
;;; SYNOPSIS
(defun make-tempo (bpm &key (beat 4) id description)
;;; ****
  ;; MDE Tue Jun 28 16:20:35 2016 -- see ARGUMENTS description
  (when (and (numberp bpm) (>= bpm 10000))
    (setq bpm (usecs-to-bpm bpm)))
  ;; MDE Mon Jan 25 10:00:43 2021, Heidhausen -- just return the first arg if
  ;; it's already a tempo object 
  (typecase bpm
    (tempo bpm)
    (list (make-instance 'tempo :bpm (first bpm) :beat (second bpm) 
                         :description (third bpm) :id id))
    (t (make-instance 'tempo :bpm bpm :beat beat :id id 
                      :description description))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tempo-p (thing)
  (typep thing 'tempo))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EOF tempo.lsp
