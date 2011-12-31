;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* sclist/rthm-seq
;;; NAME 
;;; rthm-seq
;;;
;;; File:             rthm-seq.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> sclist -> rthm-seq
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Implementation of the rthm-seq class which holds the bars
;;;                   and rhythms of a sequence (multiple bars).  This will
;;;                   generally be stored in a rthm-seq-palette and referenced
;;;                   later in the rthm-seq-map.
;;;
;;;                   The data used to create such an object will look
;;;                   something like:
;;;
;;;                   (rthm1 ((((2 4) q (q)) 
;;;                             (s x 4 (e) e) 
;;;                             ((3 8) (e) e (e)))
;;;                           :pitch-seq-palette '((psp1 (1 2 1 2 3 2 1)) 
;;;                                                (psp2 (3 2 4 6 1 5 7)) 
;;;                                                (psp3 (2 3 4 1 3 4 5)))))
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    14th February 2001
;;;
;;; $$ Last modified: 17:07:31 Sat Dec 31 2011 ICT
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

;;; The id is the first in the list given to make-rthm-seq, data is the
;;; original data list given.

(defclass rthm-seq (sclist)
  ;; a list of rthm-seq-bar objects
  ((bars :accessor bars :type list :initform nil)
   (pitch-seq-palette :accessor pitch-seq-palette :initarg :pitch-seq-palette
                      :initform nil)
   ;; MDE Mon Dec 12 08:58:00 2011 -- back when we generated input files for
   ;; Leland Smith's Score. 
   ;; markings for score, eg "s 18/f 2;" etc.
   ;; (marks :accessor marks :type string :initarg :marks :initform ";")
   (marks :accessor marks :type list :initarg :marks :initform nil)
   (num-bars :accessor num-bars :type integer :initform 0)
   (num-rhythms :accessor num-rhythms :type integer :initform 0)
   ;; this is the sum of notes-needed from the rthm-seq-bars
   (num-notes :accessor num-notes :type integer :initform 0)
   ;; the number of notes for the score, whether tied or not N.B. a chord
   ;; counts as one note!  
   (num-score-notes :accessor num-score-notes :type integer :initform 0)
   (duration :accessor duration :type float :initform 0.0)
   ;; whether we've created inversions of the pitch-seqs in pitch-seq-palette
   (psp-inversions :accessor psp-inversions :type boolean :initform nil)
   ;; we don't want to increment first notes of a seq more than once!
   (handled-first-note-tie :accessor handled-first-note-tie :type boolean 
                           :initform nil)
   ;; 25.1.11 another id/tag made up of the time signatures of the bars, so if
   ;; we had a 2/4 and a 3/4 bar, this would be "02040304" NB this is only
   ;; created and stored if we call get-time-sigs-tag
   (time-sigs-tag :accessor time-sigs-tag :initform nil)
   (num-rests :accessor num-rests :type integer :initform 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Instead of overriding the verify-and-store method of sclist and replacing
;;; the data slot with the parsed and instantiated bars given to a rthm-seq, I
;;; prefer here to leave data with the rthm-seq in it's original list form and
;;; store the results of processing this in the other slots.  That way I can
;;; check the given rhythms against those stored etc. when debugging.  Wastes a
;;; bit of memory perhaps but what the hell...

(defmethod initialize-instance :after ((rs rthm-seq) &rest initargs)
  (declare (ignore initargs))
  (let* ((data (basic-copy-object (data rs)))
         (bars '()))
    (when data
      (setf bars (loop for bar in (first data) and i from 1
                      for rsb = (make-rthm-seq-bar 
                                 bar (format nil "~a-bar~a" (id rs) i))
                      ;; 2.2.11 make sure rest bars are made here 
                    do (consolidate-rests rsb)
                    collect rsb)
            (bars rs) bars)
      ;; Issue an error when an unnecessary time-sig was given!
      (loop for b1 in bars and b2 in (cdr bars) do
            (when 
                (and (time-sig-equal (get-time-sig b1) (get-time-sig b2))
                     (write-time-sig b2))
              (error "rthm-seq::initialize-instance: ~
                  An unnecessary time signature was given: ~%~a" 
                     data)))
      ;; Get and set the :pitch-seq-palette and any other given slot-value
      ;; pairs.  
      (loop for slot in (cdr data) by #'cddr 
          and value in (cddr data) by #'cddr do
            (setf (slot-value rs (rm-package slot)) value))
      ;; The first bar of a rthm-seq must have a time-sig!
      (unless (time-sig-given (nth 0 bars))
        (error "rthm-seq::initialize-instance: ~
                The first bar of a rthm-seq must have a time signature!: ~a
                ~%First bar: ~a"
               data (nth 0 bars)))
      ;; Collect some handy data.
      (gen-stats rs)
      ;; these come after gen-stats!
      (handle-marks rs)
      (add-marks rs)
      (init-psp rs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; marks are expected like this:
;;; ((s 1 2 5 6) (a 13 14) (p 1) (f 13) (p 15))))
;;; i.e. with sublists, one for each accent
;;; it's easier like this though: (as 1 5 6 t 11 15 16)))
;;; so change the latter into the former

(defmethod handle-marks ((rs rthm-seq))
  (let ((mks (marks rs)))
    (when (and mks (simple-listp mks))
      (setf (slot-value rs 'marks)
            (loop 
               with result = '()
               with temp = '()
               for el in mks do
               (if (numberp el)
                   (push el temp)
                   (progn ;; otherwise it's a symbol like a, t, as etc.
                     (when temp
                       (push (nreverse temp) result))
                     (setf temp '())
                     (push el temp)))
               finally 
               (push (nreverse temp) result)
               (return (nreverse result)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod gen-stats ((rs rthm-seq))
  ;; (print 'rthm-seq-gen-stats)
  (let ((bars (bars rs)))
    (setf (num-bars rs) (length bars)
          (num-rhythms rs) (loop for bar in bars sum (num-rhythms bar))
          (num-notes rs) (loop for bar in bars sum (notes-needed bar))
          (num-score-notes rs) (loop for bar in bars sum
                                     (num-score-notes bar))
          (num-rests rs) (loop for bar in bars sum (num-rests bar))
          (duration rs) (loop for bar in bars sum (bar-duration bar)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod init-psp ((rs rthm-seq))
  (let ((psp (pitch-seq-palette rs)))
    ;; make a one-note psp when none was given
    (unless psp
      ;; 30/3/06: defaults to 3 so we get the middle note out of the harmony.
      (setf psp (make-list (num-notes rs) :initial-element 3)))
    ;; the pitch-seq-palette slot has now been stored but not turned into a
    ;; pitch-seq-palette object
    ;; make-psp expects a list of lists but for the sake of convenience
    ;; let's allow a single pitch-seq to be passed
    (when (atom (first psp))
      (setf psp (list psp)))
    (setf (pitch-seq-palette rs) 
      (make-psp (format nil "rthm-seq-~a-pitch-seq-palette"
                        (id rs))
                (num-notes rs) 
                psp)))
  ;; this is now only called once we have tempo information
  ;; (handle-first-note-ties rs)
  (update-is-tied-from rs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object :before ((i rthm-seq) stream)
  (format stream "~%RTHM-SEQ: num-bars: ~a~
                  ~%          num-rhythms: ~a~
                  ~%          num-notes: ~a~
                  ~%          num-score-notes: ~a~
                  ~%          num-rests: ~a~
                  ~%          duration: ~a~
                  ~%          psp-inversions: ~a~
                  ~%          marks: ~a~
                  ~%          time-sigs-tag: ~a~
                  ~%          handled-first-note-tie: ~a~
                  ~%         (for brevity's sake, slots ~
                  pitch-seq-palette and bars are not printed)"
          (num-bars i) (num-rhythms i) (num-notes i) (num-score-notes i)
          (num-rests i) (duration i) (psp-inversions i) ; (marks i)
          (marks i) (time-sigs-tag i) (handled-first-note-tie i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-simple ((rs rthm-seq) &optional written (stream t))
  (format stream "~&rthm-seq ~a" (id rs))
  (loop for bar in (bars rs) do
       (print-simple bar written stream)))
;;       (format t "~&~a: ~a" 
  ;;             (get-time-sig-as-list bar)
    ;;           (rhythms-as-symbols (rhythms bar)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone ((rs rthm-seq))
  (clone-with-new-class rs 'rthm-seq))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone-with-new-class :around ((rs rthm-seq) new-class)
  (declare (ignore new-class))
  (let ((sclist (call-next-method)))
    (setf (slot-value sclist 'bars) (my-copy-list (bars rs))
          (slot-value sclist 'pitch-seq-palette) 
          (when (pitch-seq-palette rs)
            (clone (pitch-seq-palette rs)))
          ;; (slot-value sclist 'marks) (basic-copy-object (marks rs))
          (slot-value sclist 'marks) (my-copy-list (marks rs))
          (slot-value sclist 'num-bars) (num-bars rs)
          (slot-value sclist 'num-rhythms) (num-rhythms rs)
          (slot-value sclist 'num-notes) (num-notes rs)
          (slot-value sclist 'num-score-notes) (num-score-notes rs)
          (slot-value sclist 'num-rests) (num-rests rs)
          (slot-value sclist 'duration) (duration rs)
          (slot-value sclist 'psp-inversions) (psp-inversions rs)
          (slot-value sclist 'time-sigs-tag) (time-sigs-tag rs)
          (slot-value sclist 'handled-first-note-tie) 
          (handled-first-note-tie rs))
    sclist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Tue Dec 27 19:55:22 EST 2011: Added robodoc info

;;; ****m* rthm-seq/get-nth-non-rest-rhythm
;;; FUNCTION
;;; Get the nth non-rest rhythm object stored in the given rthm-seq object. 
;;; 
;;; ARGUMENTS 
;;; - The zero-based index number indicating which non-rest-rhythm is sought.
;;; - The given rthm-seq object in which to search.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - T or NIL indicating whether to print an error message if the given index 
;;; is greater than the number of non-rest rhythms in given rthm-seq object 
;;; (minus 1 to compensate for the zero-based indexing). (Default = T.)   
;;; 
;;; RETURN VALUE  
;;; A rhythm object.
;;;
;;; Returns NIL if the given index is higher than the highest possible index of
;;; non-rest rhythms in the given rthm-seq-bar object.
;;; 
;;; EXAMPLE
#|
;; The method returns a rhythm object when successful
(let ((rs (make-rthm-seq '((((2 4) q e s s)
                            ((e) q (e))
                            ((3 8) s s e. s))
                           :pitch-seq-palette ((1 2 3 4 1 1 2 3 4))))))
  (get-nth-non-rest-rhythm 4 rs))

=> 
RHYTHM: value: 4.000, duration: 1.000, rq: 1, is-rest: NIL, 
        score-rthm: 4.0f0, undotted-value: 4, num-flags: 0, num-dots: 0, 
        is-tied-to: NIL, is-tied-from: NIL, compound-duration: 1.000, 
        is-grace-note: NIL, needs-new-note: T, beam: NIL, bracket: NIL, 
        rqq-note: NIL, rqq-info: NIL, marks: NIL, marks-in-part: NIL, 
        letter-value: 4, tuplet-scaler: 1, grace-note-duration: 0.05
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: Q, tag: NIL, 
data: Q

;; By default, the method drops into the debugger with an error when the
;; specified index is greater than the number of items in the given rthm-seq 
;; object. 
(let ((rs (make-rthm-seq '((((2 4) q e s s)
                            ((e) q (e))
                            ((3 8) s s e. s))
                           :pitch-seq-palette ((1 2 3 4 1 1 2 3 4))))))
  (get-nth-non-rest-rhythm 11 rs))

=>
rthm-seq::get-nth-non-rest-rhythm: Couldn't get non-rest rhythm with index 11
   [Condition of type SIMPLE-ERROR]

;; This error can be suppressed, simply returning NIL, by setting the optional
;; argument to NIL.
(let ((rs (make-rthm-seq '((((2 4) q e s s)
                            ((e) q (e))
                            ((3 8) s s e. s))
                           :pitch-seq-palette ((1 2 3 4 1 1 2 3 4))))))
  (get-nth-non-rest-rhythm 11 rs nil))

=> NIL

|#
;;; SYNOPSIS
(defmethod get-nth-non-rest-rhythm (index (rs rthm-seq)
                                    &optional (error t))
;;; ****                                ; ;
  (let* ((i index)
         (result
          (loop 
             for bar in (bars rs) 
             for nsn = (num-score-notes bar)
             do
             (if (< i nsn)
                 (return (get-nth-non-rest-rhythm i bar error))
                 (decf i nsn)))))
    (when error
      (unless result
        (error "~a~&rthm-seq::get-nth-non-rest-rhythm: Couldn't get ~
                non-rest rhythm with index ~a"
               rs index)))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Tue Dec 27 20:13:33 EST 2011: Added robodoc info

;;; ****m* rthm-seq/get-nth-attack
;;; FUNCTION
;;; Gets the rhythm object for the nth note in a given rthm-seq object that
;;; needs an attack, i.e. not a rest and not tied. 
;;; 
;;; ARGUMENTS 
;;; - The zero-based index number indicating which attack is sought.
;;; - The given rthm-seq object in which to search.
;;;
;;; OPTIONAL ARGUMENTS
;;; - T or NIL indicating whether to print a warning message if the given index
;;; is greater than the number of attacks (minus one) in the rthm-seq object 
;;; (default = T).    
;;; 
;;; RETURN VALUE  
;;; A rhythm object.
;;; 
;;; EXAMPLE
#|
;; The method returns a rhythm object when successful
(let ((rs (make-rthm-seq '((((2 4) q+e s s)
                            ((e) q (e))
                            ((3 8) s s e. s))
                           :pitch-seq-palette ((1 2 3 4 1 1 2 3))))))
  (get-nth-attack 4 rs))

=> 
RHYTHM: value: 16.000, duration: 0.250, rq: 1/4, is-rest: NIL, 
        score-rthm: 16.0f0, undotted-value: 16, num-flags: 2, num-dots: 0, 
        is-tied-to: NIL, is-tied-from: NIL, compound-duration: 0.250, 
        is-grace-note: NIL, needs-new-note: T, beam: NIL, bracket: NIL, 
        rqq-note: NIL, rqq-info: NIL, marks: NIL, marks-in-part: NIL, 
        letter-value: 16, tuplet-scaler: 1, grace-note-duration: 0.05
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: S, tag: NIL, 
data: S

;; By default, the method drops into the debugger with an error when the
;; specified index is greater than the number of items in the given rthm-seq
;; object. 
(let ((rs (make-rthm-seq '((((2 4) q+e s s)
                            ((e) q (e))
                            ((3 8) s s e. s))
                           :pitch-seq-palette ((1 2 3 4 1 1 2 3))))))
  (get-nth-attack 11 rs))

=>
rthm-seq::get-nth-attack: Couldn't get attack with index 11
   [Condition of type SIMPLE-ERROR]

;; This error can be suppressed, simply returning NIL, by setting the optional
;; argument to NIL.
(let ((rs (make-rthm-seq '((((2 4) q+e s s)
                            ((e) q (e))
                            ((3 8) s s e. s))
                           :pitch-seq-palette ((1 2 3 4 1 1 2 3))))))
  (get-nth-attack 11 rs nil))

=> NIL, 0, NIL 
|#
;;; SYNOPSIS
(defmethod get-nth-attack (index (rs rthm-seq)
                           &optional (error t))
;;; ****                                ;
  (let* ((i index)
         (bar-cnt 0)
         (bar-nth nil)
         (result
          (loop 
             for bar in (bars rs) 
             for bar-count from 0
             for nnn = (notes-needed bar)
             do
             ;; (print nnn)             ;
             (if (< i nnn)
                 (multiple-value-bind
                       (event nth-in-bar)
                     (get-nth-attack i bar error)
                   (setf bar-nth nth-in-bar
                         bar-cnt bar-count)
                   (return event))
                 (decf i nnn)))))
    (when error
      (unless result
        (error "~a~&rthm-seq::get-nth-attack: Couldn't get attack with index ~a"
               rs index)))
    (values result bar-cnt bar-nth)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; NB this does not check that the right rhythms are now in the bar!

;;; SAR Tue Dec 27 20:38:03 EST 2011: Added robodoc info

;;; ****m* rthm-seq/set-nth-attack
;;; FUNCTION
;;; Sets the value of the nth rhythm object of a given rthm-seq object that
;;; needs an attack; i.e., not a rest and not a tied note.
;;;
;;; NB: This method does not check to ensure that the resulting rthm-seq bars
;;; contain the right number of beats.
;;; 
;;; ARGUMENTS 
;;; - A zero-based index number for the attacked note to change.
;;; - An event.
;;; - A rthm-seq object.
;;;
;;; OPTIONAL ARGUMENTS
;;; - T or NIL indicating whether to print a warning message if the given index
;;; is greater than the number of attacks in the rthm-seq object (minus one to
;;; compensate for the zero-based indexing) (default = T).  
;;; 
;;; RETURN VALUE  
;;; - An event object.
;;; 
;;; EXAMPLE
#|
;; The method returns an event object
(let ((rs (make-rthm-seq '((((2 4) q+e s s)
                            ((e) q (e))
                            ((3 8) s s e. s))
                           :pitch-seq-palette ((1 2 3 4 1 1 2 3))))))
  (set-nth-attack 2 (make-event 'c4 'q) rs))

=> 
EVENT: start-time: NIL, end-time: NIL, 
[...]
       pitch-or-chord: 
PITCH: frequency: 261.6255569458008, midi-note: 60, midi-channel: NIL 
[...]
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: C4, tag: NIL, 
data: C4
[...]
       written-pitch-or-chord: NIL
RHYTHM: value: 4.000, duration: 1.000, rq: 1, is-rest: NIL, 
[...]
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: Q, tag: NIL, 
data: Q

;; Create a rthm-seq object, apply set-nth-attack, print the corresponding
;; slots to see the change
(let ((rs (make-rthm-seq '((((2 4) q+e s s)
                            ((e) q (e))
                            ((3 8) s s e. s))
                           :pitch-seq-palette ((1 2 3 4 1 1 2 3))))))
  (set-nth-attack 2 (make-event 'c4 'q) rs)
  (loop for b in (bars rs) 
     collect (loop for r in (rhythms b) collect (data r))))

=> (("Q" "E" S Q) (E Q E) (S S E. S))

;; By default, the method drops into the debugger with an error when the
;; specified index is greater than the number ofitems in the given rthm-seq
;; object. 
(let ((rs (make-rthm-seq '((((2 4) q+e s s)
                            ((e) q (e))
                            ((3 8) s s e. s))
                           :pitch-seq-palette ((1 2 3 4 1 1 2 3))))))
  (set-nth-attack 11 (make-event 'c4 'q) rs))

=> 
rthm-seq::set-nth-attack: Can't set attack 11 as only 8 notes in the rthm-seq
   [Condition of type SIMPLE-ERROR]

;; This error can be suppressed, simply returning NIL, by setting the optional
;; argument to NIL.
(let ((rs (make-rthm-seq '((((2 4) q+e s s)
                            ((e) q (e))
                            ((3 8) s s e. s))
                           :pitch-seq-palette ((1 2 3 4 1 1 2 3))))))
  (set-nth-attack 11 (make-event 'c4 'q) rs nil))

=> NIL

|#
;;; SYNOPSIS
(defmethod set-nth-attack (index (e event) (rs rthm-seq)
                           &optional (error t))
;;; ****
  (when (and error (>= index (num-notes rs)))
    (error "~a~&rthm-seq::set-nth-attack: Can't set attack ~a as only ~a notes ~
             in the rthm-seq" rs index (num-notes rs)))
  (loop 
     for bar in (bars rs) 
     for nnn = (notes-needed bar)
     do
       (if (< index nnn)
           (return (set-nth-attack index e bar error))
           (decf index nnn))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Can't use the sclist method because the bars are stored in the bars slot,
;;; not in the data slot. 

;;; SAR Tue Dec 27 21:24:15 EST 2011: Added robodoc info

;;; ****m* rthm-seq/set-nth-bar
;;; FUNCTION
;;; Change the contents of the nth rthm-seq-bar object in the given rthm-seq. 
;;; 
;;; ARGUMENTS 
;;; - A zero-based index number for the bar to change.
;;; - A rthm-seq-bar object containing the new bar.
;;; - A rthm-seq object. 
;;; 
;;; RETURN VALUE  
;;; A rthm-seq-bar object.
;;; 
;;; EXAMPLE
#|
;; The method returns what is passed to it as the new-bar argument (generally a
;; rthm-seq-bar object.
(let ((rs (make-rthm-seq '((((2 4) q+e s s)
                            ((e) q (e))
                            ((3 8) s s e. s))
                           :pitch-seq-palette ((1 2 3 4 1 1 2 3))))))
  (set-nth-bar 1 (make-rthm-seq-bar '((2 4) (s) e (s) q)) rs))

=> 
RTHM-SEQ-BAR: time-sig: 0 (2 4), time-sig-given: T, bar-num: -1, 
[...]
data: ((2 4) (S) E (S) Q)

;; Create a rthm-seq object, change the second bar (index 1) using the
;; set-nth-bar method, and print the contents of the rhythms data to see the
;; changes. 
(let ((rs (make-rthm-seq '((((2 4) q+e s s)
                            ((e) q (e))
                            ((3 8) s s e. s))
                           :pitch-seq-palette ((1 2 3 4 1 1 2 3))))))
  (set-nth-bar 1 (make-rthm-seq-bar '((2 4) (s) e (s) q)) rs)
  (loop for b in (bars rs)
     collect (loop for r in (rhythms b) collect (data r))))

=> (("Q" "E" S S) (S E S Q) (S S E. S))

|#
;;; SYNOPSIS
(defmethod set-nth-bar (index new-bar (rs rthm-seq))
;;; ****
  (when (and rs (rthm-seq-check-bounds rs index))
    (setf (nth index (bars rs)) new-bar)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
;;; SAR Wed Dec 28 09:51:25 EST 2011: Added robodoc info

;;; ****m* rthm-seq/get-nth-bar
;;; FUNCTION
;;; Get the nth rthm-seq-bar object from a given rthm-seq object.
;;; 
;;; ARGUMENTS 
;;; - A rthm-seq object.
;;; - An index number (zero-based).
;;; 
;;; RETURN VALUE  
;;; Returns a rthm-seq-bar object if successful.
;;;
;;; Returns NIL and prints a warning if the specified index number is greater
;;; than the number of rthm-seq-bar objects (minus one) in the given rthm-seq
;;; object. 
;;; 
;;; EXAMPLE
#|
;;; The method returns a rhtm-seq-bar object when successful
(let ((rs (make-rthm-seq '((((2 4) q+e s s)
                            ((e) q (e))
                            ((3 8) s s e. s))
                           :pitch-seq-palette ((1 2 3 4 1 1 2 3))))))
  (get-nth-bar 1 rs))

=> 
RTHM-SEQ-BAR: time-sig: 0 (2 4), time-sig-given: NIL, bar-num: -1, 
[...]
NAMED-OBJECT: id: "NIL-bar2", tag: NIL, 
data: ((E) Q (E))

;; Returns a warning and prints NIL when the specified index number is greater
;; than the number of rthm-seq-bar objects in the given rthm-seq object
(let ((rs (make-rthm-seq '((((2 4) q+e s s)
                            ((e) q (e))
                            ((3 8) s s e. s))
                           :pitch-seq-palette ((1 2 3 4 1 1 2 3))))))
  (get-nth-bar 11 rs))

=> NIL
WARNING: rthm-seq::rthm-seq-check-bounds: Illegal list reference: 11 

|#
;;; SYNOPSIS
(defmethod get-nth-bar (nth (rs rthm-seq))
;;; ****
  (when (and rs (rthm-seq-check-bounds rs nth))
    (nth nth (bars rs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Wed Dec 28 10:02:34 EST 2011: Added robodoc info

;;; ****m* rthm-seq/get-last-bar
;;; FUNCTION
;;; Get the last rthm-seq-bar object of a given rthm-seq object.
;;; 
;;; ARGUMENTS 
;;; - A rthm-seq object.
;;; 
;;; RETURN VALUE  
;;; A rthm-seq-bar object.
;;; 
;;; EXAMPLE
#|
;;; The method returns a rthm-seq-bar object 
(let ((rs (make-rthm-seq '((((2 4) q+e s s)
                            ((e) q (e))
                            ((3 8) s s e. s))
                           :pitch-seq-palette ((1 2 3 4 1 1 2 3))))))
  (get-last-bar rs))

=> 
RTHM-SEQ-BAR: time-sig: 6 (3 8), time-sig-given: T, bar-num: -1, 
[...]
data: ((3 8) S S E. S)

|#
;;; SYNOPSIS
(defmethod get-last-bar ((rs rthm-seq))
;;; ****
  (get-nth-bar (1- (num-bars rs)) rs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* rthm-seq/get-last-attack
;;; FUNCTION
;;; Gets the rhythm object for the last note that needs an attack (i.e. not a
;;; rest and not a tied note) in a given rthm-seq object.
;;; 
;;; ARGUMENTS 
;;; - A rthm-seq object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - T or NIL indicating whether to print a warning message if the given index
;;; (minus one) is greater than the number of attacks in the rthm-seq object
;;; (default = T). This is a carry-over argument from the get-nth-attack method
;;; called within the get-last-attack method and not likely to be needed for
;;; use with get-last-attack.
;;; 
;;; RETURN VALUE  
;;; A rhythm object.
;;; 
;;; EXAMPLE
#|
;; Returns a rhythm object
(let ((rs (make-rthm-seq '((((2 4) q+e s s)
                            ((e) q (e))
                            ((3 8) s s e. s))
                           :pitch-seq-palette ((1 2 3 4 1 1 2 3))))))
  (get-last-attack rs))

=> 
RHYTHM: value: 16.000, duration: 0.250, rq: 1/4, is-rest: NIL, 
        score-rthm: 16.0f0, undotted-value: 16, num-flags: 2, num-dots: 0, 
        is-tied-to: NIL, is-tied-from: NIL, compound-duration: 0.250, 
        is-grace-note: NIL, needs-new-note: T, beam: NIL, bracket: NIL, 
        rqq-note: NIL, rqq-info: NIL, marks: NIL, marks-in-part: NIL, 
        letter-value: 16, tuplet-scaler: 1, grace-note-duration: 0.05
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: S, tag: NIL, 
data: S

|#
;;; SYNOPSIS
(defmethod get-last-attack ((rs rthm-seq) &optional (warn t))
;;; ****
  (get-last-attack (get-last-bar rs) warn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Wed Dec 28 10:27:21 EST 2011: Added robodoc info

;;; ****m* rthm-seq/get-last-event
;;; FUNCTION
;;; Get the last event object (or rhythm object) of a given rthm-seq-bar
;;; object. 
;;; 
;;; ARGUMENTS 
;;; - A rthm-seq object.
;;; 
;;; RETURN VALUE  
;;; Returns an event (or rhythm) object.
;;; 
;;; EXAMPLE
#|
;; The last event is a rhythm object
(let ((rs (make-rthm-seq '((((2 4) q+e s s)
                            ((e) q (e))
                            ((3 8) s s e. s))
                           :pitch-seq-palette ((1 2 3 4 1 1 2 3))))))
  (get-last-event rs))

=> 
RHYTHM: value: 16.000, duration: 0.250, rq: 1/4, is-rest: NIL, 
        score-rthm: 16.0f0, undotted-value: 16, num-flags: 2, num-dots: 0, 
        is-tied-to: NIL, is-tied-from: NIL, compound-duration: 0.250, 
        is-grace-note: NIL, needs-new-note: T, beam: NIL, bracket: NIL, 
        rqq-note: NIL, rqq-info: NIL, marks: NIL, marks-in-part: NIL, 
        letter-value: 16, tuplet-scaler: 1, grace-note-duration: 0.05
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: S, tag: NIL, 
data: S

;; The last event is an event object
(let ((rs (make-rthm-seq `((((2 4) q+e s s)
                            ((e) q (e))
                            ((3 8) s s e. ,(make-event 'c4 's)))
                           :pitch-seq-palette ((1 2 3 4 1 1 2 3))))))
  (get-last-event rs))

=> 
EVENT: start-time: NIL, end-time: NIL, 
[...]
PITCH: frequency: 261.6255569458008, midi-note: 60, midi-channel: NIL 
[...]
RHYTHM: value: 16.000, duration: 0.250, rq: 1/4, is-rest: NIL, 
[...]
NAMED-OBJECT: id: S, tag: NIL, 
data: S

|#
;;; SYNOPSIS
(defmethod get-last-event ((rs rthm-seq))
;;; ****
  (get-last-event (get-last-bar rs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; We assume here that ties are taken care of within the new bar!

;;; TODO: Test that the pitch-seq-palette splicing actually works; add
;;; inversions if that was in the original 

;;; SAR Wed Dec 28 12:08:30 EST 2011: Added robodoc info

;;; ****m* rthm-seq/insert-bar
;;; FUNCTION
;;; Insert a rthm-seq-bar object into the given rthm-seq object and re-init
;;; it. If there's a pitch-seq/pitch-seq-palette given (list of numbers, or list
;;; of lists), splice this in at the appropriate location.
;;;
;;; NB: This method sets the values of the individual slots but leaves the DATA
;;; slot untouched (for cases in which the user might want to see where the new
;;; data originated from, or otherwise use the old data somehow, such as in a
;;; new rthm-seq object).
;;; 
;;; ARGUMENTS 
;;; - A rthm-seq object.
;;; - A rthm-seq-bar object.
;;; - A bar number (integer). This argument is the bar number of the bar to be
;;; inserted, relative to the rthm-seq and 1-based; e.g., if 3, then it will
;;; come before the present third bar.
;;;
;;; OPTIONAL ARGUMENTS
;;; - A pitch-seq object.
;;; - (three ignore arguments for sc-internal use only)
;;; 
;;; RETURN VALUE  
;;; Returns T if successful.
;;;
;;; Drops into the debugger with an error if the specified bar-number argument
;;; is greater than the number of rthm-seq-bar objects in the given rthm-seq. 
;;; 
;;; EXAMPLE
#|
;; The method returns T when successful
(let ((rs (make-rthm-seq '((((2 4) q+e s s)
                            ((e) q (e))
                            ((3 8) s s e. s))
                           :pitch-seq-palette ((1 2 3 4 1 1 2 3))))))
  (insert-bar rs (make-rthm-seq-bar '((3 4) q. e e s s)) 3))

=> T

;; Create a rthm-seq object with three rthm-seq-bars and print contents of the
;; NUM-BARS slot to confirm that it contains 3 objects. Insert a bar before the
;; third item and print the value of the NUM-BARS slot again to confirm that
;; there are now 4 objects. Use print-simple and get-nth-bar to confirm that
;; the 3rd object (with a zero-based index of 2) is indeed the one inserted.  
(let ((rs (make-rthm-seq '((((2 4) q+e s s)
                            ((e) q (e))
                            ((3 8) s s e. s))
                           :pitch-seq-palette ((1 2 3 4 1 1 2 3))))))
  (print (num-bars rs))
  (insert-bar rs (make-rthm-seq-bar '((3 4) q. e e s s)) 3)
  (print (num-bars rs))
  (print-simple (get-nth-bar 2 rs)))

=>
3 
4 
(3 4): note Q., note E, note E, note S, note S,

;; Attempting to insert a bar with an index number greater than the number of
;; objects currently in the rthm-seq object drops into the debugger with an
;; error 
(let ((rs (make-rthm-seq '((((2 4) q+e s s)
                            ((e) q (e))
                            ((3 8) s s e. s))
                           :pitch-seq-palette ((1 2 3 4 1 1 2 3))))))
  (insert-bar rs (make-rthm-seq-bar '((3 4) q. e e s s)) 11))

=>
rthm-seq::insert-bar: only 3 bars in rthm-seq!
   [Condition of type SIMPLE-ERROR]

;; Inserting a rthm-seq-bar using the optional pitch-seq argument splices the
;; specified value of that argument into the existing pitch-seq-palette
(let ((rs (make-rthm-seq '((((2 4) q+e s s)
                            ((e) q (e))
                            ((3 8) s s e. s))
                           :pitch-seq-palette ((1 2 3 1 1 2 3 4))))))
  (insert-bar rs (make-rthm-seq-bar '((3 4) q. e e s s)) 3 '((1 2 3 4 5)))
  (loop for ps in (data (pitch-seq-palette rs)) collect (data ps)))

=> ((1 2 3 1 1 2 3 4 5 1 2 3 4))

|#
;;; SYNOPSIS

(defmethod insert-bar ((rs rthm-seq) (rsb rthm-seq-bar) bar-num
                       &optional pitch-seq ignore1 ignore2 ignore3)
;;; ****
  ;; these are needed in the piece method.
  (declare (ignore ignore1 ignore2 ignore3))
  (when (> bar-num (num-bars rs))
    (error "rthm-seq::insert-bar: only ~a bars in rthm-seq!" 
           (num-bars rs)))
  (unless pitch-seq
    (setf pitch-seq (ml 1 (notes-needed rsb))))
  ;; (print pitch-seq)
  (let* ((notes-before (loop for bar in (bars rs) and i below (1- bar-num) 
                           sum (notes-needed bar)))
         (psp (pitch-seq-palette rs))
         (num-ps (when psp (sclist-length psp)))
         (new-pss (when pitch-seq
                    (if (simple-listp pitch-seq)
                        (ml pitch-seq num-ps)
                      (progn
                        (unless (= num-ps (length pitch-seq))
                          (error "rthm-seq::insert-bar: need ~a pitch-seqs!" 
                                 num-ps))
                        pitch-seq))))
         (new-psp
          ;; if we've got pitch-seq(s) then splice them in
          (when pitch-seq
            (loop 
                for new-ps in new-pss
                for old-ps = (data (get-next psp))
                collect (splice new-ps old-ps notes-before)))))
    (setf (bars rs) (splice (list rsb) (bars rs) (1- bar-num))
          (pitch-seq-palette rs) new-psp)
    ;; (print (length (bars rs)))
    (gen-stats rs)
    (init-psp rs)
    t))
          

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Can't use the sclist method because the bars are stored in the bars slot
;;; and not the data slot.

(defmethod rthm-seq-check-bounds ((rs rthm-seq) index)
  (let ((ok (and (integerp index) 
                 (>= index 0)
                 (< index (num-bars rs)))))
    (cond (ok t)
          ((bounds-alert rs) 
           (warn "rthm-seq::rthm-seq-check-bounds: ~
                  Illegal list reference: ~a ~a"
                 index rs))
          (t nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* rthm-seq/get-time-sigs
;;; FUNCTION
;;; Return a list of time-sig objects for each of the rthm-seq-bar objects in a
;;; given rthm-seq object. 
;;;
;;; One time signature is returned for each rthm-seq-bar object, even if two or
;;; more consecutive objects have the same time signature. 
;;;
;;; Optionally, this method can return a list of time signatures in list form
;;; (e.g. ((2 4) (3 4)) etc.) rather than a list of time-sig objects.
;;; 
;;; ARGUMENTS 
;;; - A rthm-seq object.
;;;
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether to return the time signatures as time-sig
;;; objects or a list of two-item lists. T = time-sig objects. Default = T.
;;; 
;;; RETURN VALUE  
;;; Returns a list of time-sig objects by default. Optionally a list of time
;;; signatures as two-item lists can be returned instead.
;;; 
;;; EXAMPLE
#|
;; Return a list of time-sig objects, one for each rthm-seq-bar object even if
;; consecutive rthm-seq-bar objects have the same time signature
(let ((rs (make-rthm-seq '((((2 4) q+e s s)
                            ((e) q (e))
                            ((3 8) s s e. s))
                           :pitch-seq-palette ((1 2 3 1 1 2 3 4))))))
  (get-time-sigs rs))

=> (
TIME-SIG: num: 2, denom: 4, duration: 2.0, compound: NIL, midi-clocks: 24, num-beats: 2
SCLIST: sclist-length: 2, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: "0204", tag: NIL, 
data: (2 4)
**************

    
TIME-SIG: num: 2, denom: 4, duration: 2.0, compound: NIL, midi-clocks: 24, num-beats: 2
SCLIST: sclist-length: 2, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: "0204", tag: NIL, 
data: (2 4)
**************

    
TIME-SIG: num: 3, denom: 8, duration: 1.5, compound: T, midi-clocks: 24, num-beats: 1
SCLIST: sclist-length: 2, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: "0308", tag: NIL, 
data: (3 8)
**************
)

;; Return the same as a list of two-item lists instead
(let ((rs (make-rthm-seq '((((2 4) q+e s s)
                            ((e) q (e))
                            ((3 8) s s e. s))
                           :pitch-seq-palette ((1 2 3 1 1 2 3 4))))))
  (get-time-sigs rs t))

=> ((2 4) (2 4) (3 8))

|#
;;; SYNOPSIS
(defmethod get-time-sigs ((rs rthm-seq) &optional as-list)
;;; ****
  (loop for bar in (bars rs) collect 
        (if as-list
            (get-time-sig-as-list bar)
          (get-time-sig bar))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; update the is-tied-from slot of the rhythm objects in the rthm-seq-bar
;; objects in bars. 

(defmethod update-is-tied-from ((rs rthm-seq))
  (let ((is-tied-from nil))
    (loop for bar in (reverse (bars rs)) do
          (loop for rthm in (reverse (rhythms bar)) do
                (when is-tied-from
                  (setf (is-tied-from rthm) t))
                (setf is-tied-from (is-tied-to rthm))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#| 17/7/05: obsolete code as ties are handled now at the piece level

(defmethod handle-first-note-ties ((rs rthm-seq) &optional (warn-ties t))
  (unless (handled-first-note-tie rs)
    (let ((bars (bars rs)))
      (loop for i below (length bars) 
            for rthm1 = (print (first (rhythms (nth i bars))))
            when (and rthm1 (is-tied-to rthm1)) 
            do
            (handle-first-note-tie rs (1- i) (compound-duration rthm1) 
                                   warn-ties))
      (setf (handled-first-note-tie rs) t)
      t)))

(defmethod handle-first-note-tie ((rs rthm-seq) start-bar duration 
                                  &optional (warn-ties t))
  (let ((did-it (loop 
                    for i from start-bar downto 0 
                    when (inc-last-compound-duration (nth i (bars rs))
                                                     duration) 
                    do (return i))))
    (unless did-it
      (when warn-ties
        (warn "rthm-seq::handle-first-note-tie: ~
               Ties to the first note of the first bar ~%of a rthm-seq are ~
               not yet legal (start-bar must be >= 0)! ~%start-bar = ~a, ~
               duration = ~a, (id rs) = ~a ~%~
               If you've added a tie manually to the first note of a sequence,~
               make sure you've done this before any calls to ~
               replace-multi-bar-events."
              start-bar duration (id rs))))))

|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 7.12.11: now obsolete as we no longer use SCORE (sadly...no DOS)
(defmethod get-score-strings ((rs rthm-seq) &key 
                              (notes nil) 
                              (default-note 'e4)
                              (clef 'tr))
  ;; For now we can only write rhythms using the <default-note> 
  (let ((notes-stream (make-string-output-stream))
        (rthms (make-string-output-stream))
        (ties (make-string-output-stream))
        (beams (make-string-output-stream))
        (score-notes 1)
        (note nil)
        (note-count 0))
    ;; when no notes are given we probably want to just display relative
    ;; pitches for the purpose of seeing the rthm-seq so get these from the
    ;; first pitch-seq in the pitch-seq-palette.  This still could result in a
    ;; value of nil when no pitch-seqs were given; in that case we just write
    ;; default-note.
    (unless notes
      (when (pitch-seq-palette rs)
        (setf notes (get-notes (get-nth 0 (pitch-seq-palette rs))
                               nil nil nil nil nil 0 nil))))
    (format notes-stream "~a" clef)
    ;; Loop through the bars
    (loop for bar in (bars rs) do
      (when (write-time-sig bar)
        (format notes-stream "/~a" (score-time-sig (get-time-sig bar))))
      ;; Loop through the rhythms in the bar
      (loop for r in (rhythms bar) for sr = (score-rthm r) do
        (unless (is-rest r)
          (incf note-count))
        (cond ((floatp r) (format rthms "~,3f/" sr))
              ((not r) (error "rthm-seq::get-score-strings: ~
                               score-rthm slot is nil: ~a" r))
              (t (format rthms "~a/" sr)))
        (let ((this-note (cond ((is-whole-bar-rest r) 'rw)
                               ((is-rest r) 'r)
                               ((and note (is-tied-to r)) note)
                               (notes (setf note (pop notes)))
                               (t default-note))))
          (unless this-note
            (break "Note is nil!!! rthm-seq ~a, pitch-seq-palette ~a, ~
                    bar: ~%~a" (id rs) (pitch-seq-palette rs) bar))
          (format notes-stream "/~a" this-note))
        (when (is-tied-to r)
          (format ties "~a ~a/" (1- note-count) note-count)))
      ;; write the bar line
      (format notes-stream "/m")
      ;; Loop through the tuplets in the bar
      (loop for tuplet in (tuplets bar) do
        ;; In score, the tuplets brackets are indicated by start-note
        ;; tuplet-number|end-note with a space between the first two fields but
        ;; none between the last, the second number always being 2 digits wide
        ;; with a zero pad char, e.g. "1 302" a triplet bracket from note 1 to
        ;; 2 
        (format ties "~a~4,1,,,'0f ~a/"
                (first tuplet) 
                (+ score-notes (second tuplet))
                (+ score-notes (third tuplet))))
      ;; Loop through the beams in the bar
      (loop for beam in (beams bar) do
        (format beams "~d ~d/" (+ score-notes (first beam))
                (+ score-notes (second beam))))
      (incf score-notes (num-score-notes bar)))
    ;; End of loop through the bars.
    (format rthms ";")
    (format notes-stream ";")
    (format ties ";")
    (format beams ";")
    (list (get-output-stream-string notes-stream)
          ;; there's one too many slashes in all of these.
          (minus-last-slash (get-output-stream-string rthms))
          (minus-last-slash (get-output-stream-string beams))
          (minus-last-slash (get-output-stream-string ties)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; When ignore-rests is t, the rest duration will be added to the duration of
;;; the note.  Note however that any rests at the beginning of a sequence will
;;; still count as rests, ie no note will be created at time 0.
;;; Returns a list of events objects.
;;; 
;;; 8/5/06: If we're writing a MIDI file and we have a rest bar with a new time
;;; signature we have to get the rest event, hence get-time-sig-changes.

(defmethod get-timings ((rs rthm-seq) time-scaler ignore-rests
                        get-time-sig-changes 
                        &optional (include-rests nil) (ignore-grace-notes nil))
  (loop 
      for bar in (bars rs) 
      for bar-events = 
        (get-timings bar time-scaler ignore-rests get-time-sig-changes
                     include-rests ignore-grace-notes)
        ;; do (format t "~%bar ~a: ~a events ~a struck notes"
        ;; (bar-num bar) (length (rhythms bar)) (notes-needed bar))     
      when bar-events append bar-events))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Wed Dec 28 14:11:09 EST 2011: Added robodoc info

;;; ****m* rthm-seq/combine
;;; FUNCTION
;;; Combine two rthm-seqs into one, updating slots for the new object, which is
;;; a clone.
;;;
;;; NB: The MARKS slot is ignored for now (it is as of yet 
;;;
;;; NB: This method sets the values of the individual slots but leaves the DATA
;;; slot untouched (for cases in which the user might want to see where the new
;;; data originated from, or otherwise use the old data somehow, such as in a
;;; new rthm-seq object).
;;; 
;;; ARGUMENTS 
;;; - A first rthm-seq object.
;;; - A second rthm-seq object.
;;; 
;;; RETURN VALUE  
;;; - A rthm-seq object.
;;; 
;;; EXAMPLE

#|

;; The method returns a rthm-seq object
(let ((rs1 (make-rthm-seq '((((2 4) q+e s s)
                            ((e) q (e))
                            ((3 8) s s e. s))
                           :pitch-seq-palette ((1 2 3 1 1 2 3 4)))))
      (rs2 (make-rthm-seq '((((4 4) h+e (e) { 3 te te te })
                            ((5 8) e e+32 s. +q)
                            ((3 4) (q) q q))
                           :pitch-seq-palette ((1 2 3 4 1 2 3 1 2))))))
  (combine rs1 rs2))

=>
RTHM-SEQ: num-bars: 6
          num-rhythms: 25
          num-notes: 17
          num-score-notes: 21
          num-rests: 4
          duration: 15.0
          psp-inversions: NIL
          marks: NIL
          time-sigs-tag: NIL
          handled-first-note-tie: NIL
         (for brevity's sake, slots pitch-seq-palette and bars are not printed)
SCLIST: sclist-length: 6, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: "NIL-NIL", tag: NIL, 
data: ((((2 4) Q+E S S) ((E) Q (E)) ((3 8) S S E. S)) PITCH-SEQ-PALETTE
       ((1 2 3 1 1 2 3 4))
       (((4 4) H+E (E) { 3 TE TE TE }) ((5 8) E E+32 S. +Q) ((3 4) (Q) Q Q))
       PITCH-SEQ-PALETTE ((1 2 3 4 1 2 3 1 2)))

;; With the same combine call, print the collected contents of the BARS slot
;; and the PITCH-SEQ-PALETTE slot of the new rthm-seq object
(let ((rs1 (make-rthm-seq '((((2 4) q+e s s)
                            ((e) q (e))
                            ((3 8) s s e. s))
                           :pitch-seq-palette ((1 2 3 1 1 2 3 4)))))
      (rs2 (make-rthm-seq '((((4 4) h+e (e) { 3 te te te })
                            ((5 8) e e+32 s. +q)
                            ((3 4) (q) q q))
                           :pitch-seq-palette ((1 2 3 4 1 2 3 1 2))))))
  (print (loop for b in (bars (combine rs1 rs2)) collect (data b)))
  (print (loop for ps in (data (pitch-seq-palette (combine rs1 rs2))) 
            collect (data ps))))

=>
(((2 4) Q+E S S) ((E) Q (E)) ((3 8) S S E. S)
 ((4 4) H+E (E) { 3 TE TE TE }) ((5 8) E E+32 S. +Q) ((3 4) (Q) Q Q)) 
((1 2 3 1 1 2 3 4 1 2 3 4 1 2 3 1 2))

|#

;;; SYNOPSIS

(defmethod combine ((rs1 rthm-seq) (rs2 rthm-seq))
;;; ****
  (let ((result (clone rs1)))
    (incf (num-bars result) (num-bars rs2))
    (incf (num-rhythms result) (num-rhythms rs2))
    (incf (num-notes result) (num-notes rs2))
    (incf (num-score-notes result) (num-score-notes rs2))
    (incf (num-rests result) (num-rests rs2))
    (incf (duration result) (duration rs2))
    (setf (bars result) (append (bars result) (my-copy-list (bars rs2)))
          (pitch-seq-palette result) (combine (pitch-seq-palette result)
                                              (pitch-seq-palette rs2))
          (id result) (combine-ids rs1 rs2)
          (data result) (append (data result) (my-copy-list (data rs2))))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Wed Dec 28 14:47:33 EST 2011: Added robodoc info
;;; MDE Fri Dec 30 18:36:28 2011 -- added optional  psp

;;; ****m* rthm-seq/add-bar
;;; FUNCTION
;;; Add a rthm-seq-bar object to the end of a given rthm-seq object.
;;;
;;; NB: If the rthm-seq-bar object is added without specifying a
;;; pitch-seq-palette, the method automatically adds data to the existing
;;; pitch-seq-palette. 
;;; 
;;; ARGUMENTS 
;;; - A rhtm-seq object.
;;; - A rthm-seq-bar object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - A pitch-seq-palette. 
;;;
;;; RETURN VALUE  
;;; Returns the new value of the DURATION slot of the given rthm-seq object.
;;; 
;;; EXAMPLE
#|
;; Returns the new value of the DURATION slot
(let ((rs (make-rthm-seq '((((2 4) q+e s s)
                            ((e) q (e))
                            ((3 8) s s e. s))
                           :pitch-seq-palette ((1 2 3 1 1 2 3 4))))))
  (add-bar rs (make-rthm-seq-bar '((5 8) e e+32 s. +q))))

=> 10.5

;; Apply the method and print the rhythms objects of the given rthm-seq object
;; to see the changes
(let ((rs (make-rthm-seq '((((2 4) q+e s s)
                            ((e) q (e))
                            ((3 8) s s e. s))
                           :pitch-seq-palette ((1 2 3 1 1 2 3 4))))))
  (add-bar rs (make-rthm-seq-bar '((5 8) e e+32 s. +q)))
  (loop for b in (bars rs)
       collect (loop for r in (rhythms b) collect (data r))))

=> (("Q" "E" S S) (E Q E) (S S E. S) (E "E" "32" S. "Q"))

;; Apply the method and print the DATA slot of the update PITCH-SEQ-PALETTE
;; slot to see the new notes that have been automatically added
(let ((rs (make-rthm-seq '((((2 4) q+e s s)
                            ((e) q (e))
                            ((3 8) s s e. s))
                           :pitch-seq-palette ((1 2 3 1 1 2 3 4))))))
  (add-bar rs (make-rthm-seq-bar '((5 8) e e+32 s. +q)))
  (data (first (data (pitch-seq-palette rs)))))

=> (1 2 3 1 1 2 3 4 3 4 3)

|#
;;; SYNOPSIS
(defmethod add-bar ((rs rthm-seq) (rsb rthm-seq-bar) &optional psp)
;;; ****
  ;; MDE Fri Dec 30 18:36:42 2011 -- check our psp has the right number of
  ;; notes or if we didn't pass one add a made-up one
  (if psp
      (unless (= (num-notes psp) (notes-needed rsb))
        (error "~a~&rthm-seq::add-bar: the pitch-seq-palette needs ~a notes"
               psp (notes-needed rsb)))
      ;; if no psp make one from the default data lists in
      ;; pitch-seq-palette::create-psps-default 
      (setf psp (make-psp 'add-bar-tmp (notes-needed rsb)
                          (get-psps-as-list (notes-needed rsb)
                                            ;; get as many pitch-seqs as there
                                            ;; are in the rthm-seq currently
                                            (num-data (pitch-seq-palette rs))))))
  (setf (pitch-seq-palette rs) (combine (pitch-seq-palette rs) psp)
        (bars rs) (econs (bars rs) rsb))
  ;; MDE Fri Dec 30 18:22:54 2011 -- no need for this as the setf bars method
  ;; calls gen-stats  
  ;; (incf (num-bars rs))
  (incf (num-rhythms rs) (num-rhythms rsb))
  (incf (num-rests rs) (num-rests rsb))
  (incf (num-notes rs) (notes-needed rsb))
  (incf (num-score-notes rs) (num-score-notes rsb))
  (incf (duration rs) (bar-duration rsb)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+cmn
(defmethod get-cmn-data ((rs rthm-seq) 
                         &optional ignore1 ignore2 ignore3 ignore4 ignore5 
                                   ignore6 ignore7 ignore8)
  (declare (ignore ignore1 ignore2 ignore3 ignore4 ignore5 ignore6 ignore7
                   ignore8))
  ;; call the method from the slippery-chicken class to convert the rthm-seq to
  ;; a sequenz so that we can then call the get-cmn-data method of that class. 
  (let ((sequenz (sc-make-sequenz rs nil nil 
                                  (when (pitch-seq-palette rs)
                                    (get-nth 0 (pitch-seq-palette rs)))
                                  nil nil nil nil 
                                  ;; just give any event as the last one from
                                  ;; the previous seq because we're only
                                  ;; displaying the rthm-seq-palette anyway.
                                  ;; If we have notes tied to at the beg of a
                                  ;; seq this might cause tie errors when
                                  ;; calling cmn.
                                  (make-event 'b4 'q) 
                                  nil nil nil nil)))
    ;; put all the bars together...
    (flatten (get-cmn-data sequenz 'show-id-and-tag-only t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Intra-phrasal looping
;;; Calls chop-bar for each rsb in the seq--see rthm-seq-bar::chop-bar for
;;; details.  Returns a list of rthm-seqs each containing just one of the bars
;;; returned by chop-bar.

;;; ****m* rthm-seq/chop
;;; FUNCTION
;;; 
;;; 
;;; ARGUMENTS 
;;; 
;;; 
;;; RETURN VALUE  
;;; 
;;; 
;;; EXAMPLE
#|

|#
;;; SYNOPSIS
(defmethod chop ((rs rthm-seq) &optional chop-points (unit 's)
                 (number-bars-first t))
;;; ****
  (when number-bars-first
    (set-bar-nums rs))
  (loop 
     ;; the rthm-seq-bar needs to know where we are in the pitch-seq so it can
     ;; skip that many notes when pulling out the correct ones for itself.
     with attacks = 0
     with count = 1
     with psp = (pitch-seq-palette rs)
     with result = '()
     for bar in (bars rs) 
     ;; we stored the positions of the start and end notes of the old bar
     ;; that's cannibalised in rthm-seq-bar::new-bar-from-time-range. We use
     ;; these numbers ___plus___ the number of attacked notes in the bars
     ;; previous to the current in order to get a sub-sequence out of the
     ;; pitch-seq-palette and apply it to the new rthm-seq.
     for new-bars = (chop bar chop-points unit (list-to-string (this rs) "-"))
     do
     (loop 
        for bar in new-bars 
        for pse = (parent-start-end bar)
        with rs 
        do
        (setf rs (make-rthm-seq (list count (list (list bar))))
              (tag rs) (id bar))
        (unless (is-rest-bar bar)
          (let* ((start (+ attacks (first pse)))
                 (end (+ attacks (second pse)))
                 (psp-new (psp-subseq psp start end)))
            (setf (pitch-seq-palette rs) psp-new)
            ;; MDE Tue Dec 13 00:04:12 2011 -- check!
            (unless (= (num-notes psp-new) (num-notes rs))
              (error "~a~%~%~a~%~%~a~%rthm-seq::chop: new rthm-seq has ~a ~
                      notes, but psp has ~a"
                     rs (print-simple rs) (pitch-seq-palette rs) (num-notes rs)
                     (num-notes psp-new)))))
        (push rs result)
        (incf count))
     (incf attacks (notes-needed bar))
     finally (return (nreverse result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod set-bar-nums ((rs rthm-seq) &optional (start-bar 1))
  (loop for b in (bars rs) and bar-num from start-bar do
        (setf (bar-num b) bar-num)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* rthm-seq/delete-marks
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
(defmethod delete-marks ((rs rthm-seq))
;;; ****
  ;; MDE Fri Dec 30 12:22:26 2011 -- can't use (setf marks... as that would
  ;; result in a stack overflow 
  (setf (slot-value rs 'marks) nil)
  (loop for bar in (bars rs) do (delete-marks bar)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; MDE Thu Dec 29 11:58:21 2011 
;;; Changing the marks implies deleting the old ones form the marks slot as
;;; well as from the individual rhythm objects 

(defmethod (setf marks) :before (value (rs rthm-seq))
  (declare (ignore value))
  (delete-marks rs))

(defmethod (setf marks) :after (value (rs rthm-seq))
  (declare (ignore value))
  (handle-marks rs)
  (add-marks rs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SAR Thu Dec 29 12:09:16 EST 2011: Removed robodoc info

(defmethod add-marks ((rs rthm-seq))
  (loop for i in (marks rs) do
        ;; when the list is like (a 1 4) it means accent on notes 1 to 4
        ;; (a 1) means accent on note 1
        ;; (a 1 4 6 8) means accents on notes 1, 4, 6 and 8
        ;; if you want an accent on notes 1 and 4, you have to do (a 1) (a 4)
        (if (> (length i) 3)
            (loop for note in (cdr i) with mark = (first i) do
                  (add-marks-aux rs mark note))
          ;; we have a start-end note pair...
          (add-marks-aux rs (first i) (second i) (third i)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 14/4/07: no longer create real cmn marks here, rather just the symbol for
;;; the mark that will be created when get-cmn-data is called. 

(defmethod add-marks-aux ((rs rthm-seq) mark start-note &optional end-note)
  ;; get-nth-non-rest-rhythm is 0-based, we're 1-based here.
  (decf start-note)
  (if end-note
      (decf end-note)
      (setf end-note start-note))
  (when (> end-note (1- (num-score-notes rs)))
    (error "~a~%sequenz::add-marks-aux: ~a notes in seq, but mark on ~a"
           rs (num-score-notes rs) (1+ end-note)))
  ;; cond in case we want to add other special cases later...
  (cond ((eq mark 'slur)
         ;; slurs are a special case...
         (unless (> end-note start-note)
           (error "sequenz::add-marks-aux: slurs must be over ~
                   more than one note: (~a ~a)" 
                  start-note end-note))
         (add-mark (get-nth-non-rest-rhythm start-note rs) 
                   ;; (first (cmn::get-cmn-marks 'begin-slur)))
                   'beg-sl)
         (add-mark (get-nth-non-rest-rhythm end-note rs) 
                   ;;(first (cmn::get-cmn-marks 'end-slur))))
                   'end-sl))
        (t
         ;; get-marks returns a list as some single marks need two
         ;; marks (like accent-staccato) 
         (loop 
            for i from start-note to end-note 
            for event = (get-nth-non-rest-rhythm i rs)
            ;; got to make the marks new each time...
            do
            (add-mark event mark)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Wed Dec 28 21:11:57 EST 2011: Added robodoc info

;;; ****m* rthm-seq/scale
;;; FUNCTION
;;; Scale the durations of the rhythm objects in a given rthm-seq object by the
;;; specified factor. 
;;; 
;;; ARGUMENTS 
;;; - A rthm-seq object.
;;; - A real number.
;;; 
;;; RETURN VALUE  
;;; Returns a rthm-seq object.
;;; 
;;; EXAMPLE
#|
;; The method returns a rthm-seq object.
(let ((rs (make-rthm-seq '((((2 4) q+e s s)
                            ((e) q (e))
                            ((3 8) s s e. s))
                           :pitch-seq-palette ((1 2 3 1 1 2 3 4))))))
  (scale rs 3))

=> 
RTHM-SEQ: num-bars: 3
          num-rhythms: 11
          num-notes: 8
          num-score-notes: 9
          num-rests: 2
          duration: 16.5
          psp-inversions: NIL
          marks: NIL
          time-sigs-tag: NIL
          handled-first-note-tie: NIL
         (for brevity's sake, slots pitch-seq-palette and bars are not printed)
SCLIST: sclist-length: 3, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: NIL, tag: NIL, 
data: ((((2 4) Q+E S S) ((E) Q (E)) ((3 8) S S E. S)) PITCH-SEQ-PALETTE
       ((1 2 3 1 1 2 3 4)))

;; Create a rthm-seq object, scale the durations by 3 times using the scale
;; method, and print-simple the corresponding slots to see the results
(let ((rs (make-rthm-seq '((((2 4) q+e s s)
                            ((e) q (e))
                            ((3 8) s s e. s))
                           :pitch-seq-palette ((1 2 3 1 1 2 3 4))))))
  (print-simple (scale rs 3)))

=>
rthm-seq NIL
(6 4): note H., note Q., note E., note E., 
(6 4): rest Q., note H., rest Q., 
(9 8): note E., note E., note E., note E.,

|#
;;; SYNOPSIS
(defmethod scale ((rs rthm-seq) scaler
                  &optional ignore1 ignore2 ignore3)
;;; ****
  (declare (ignore ignore1) (ignore ignore2) (ignore ignore3))
  (setf (bars rs) (loop for b in (bars rs) collect (scale b scaler)))
  rs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; e.g. (get-multipliers '(e. s q e e) 's) -> (3 1 4 2 2)
;;; ****m* rthm-seq/get-multipliers
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
(defmethod get-multipliers ((rs rthm-seq) rthm &optional round ignore)
;;; ****
  (declare (ignore ignore))
  (let ((durs (loop for bar in (bars rs) with rest-dur = 0.0 with result = '() 
                 appending
                   (loop for r in (rhythms bar) 
                      do
                        (cond ((needs-new-note r)
                             (when result
                               (incf (first result) rest-dur))
                             (push (compound-duration r) result)
                             (setf rest-dur 0.0))
                            ((is-rest r) (incf rest-dur (duration r)))))
                 finally 
                   (incf (first result) rest-dur)
                   (return (nreverse result))))
        (rthm-dur (duration (make-rhythm rthm))))
    (loop for d in durs for m = (/ d rthm-dur) collect
         (if round
             (round m)
             m))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod rhythms-to-events ((rs rthm-seq))
  (setf (bars rs)
        (loop for bar in (bars rs) collect (rhythms-to-events bar)))
  rs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod update-compound-durations ((rs rthm-seq))
  (loop with i = 0
     for r = (get-nth-non-rest-rhythm i rs)
     while (< i (num-score-notes rs))
     do
       (when (is-tied-from r)
         (incf i)
         (loop for rtied = (get-nth-non-rest-rhythm i rs)
            while (is-tied-to rtied)
            do
              (incf (compound-duration r) (duration rtied))
              (incf i)))
       (incf i))
  rs)
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Wed Dec 28 16:35:40 EST 2011: Added robodoc info

;;; ****m* rthm-seq/get-rhythms
;;; FUNCTION
;;; Get the rhythm objects in a given rthm-seq object, contained in a list.
;;; 
;;; ARGUMENTS 
;;; - A rthm-seq object.
;;; 
;;; RETURN VALUE  
;;; A list.
;;; 
;;; EXAMPLE
#|
;; Returns a list of rhythm objects
(let ((rs (make-rthm-seq '((((2 4) q+e s s)
                            ((e) q (e))
                            ((3 8) s s e. s))
                           :pitch-seq-palette ((1 2 3 1 1 2 3 4))))))
  (get-rhythms rs))

=>
(
RHYTHM: value: 4.000, duration: 1.000, rq: 1, is-rest: NIL, 
[...]
RHYTHM: value: 8.000, duration: 0.500, rq: 1/2, is-rest: NIL, 
[...] 
RHYTHM: value: 16.000, duration: 0.250, rq: 1/4, is-rest: NIL, 
[...] 
RHYTHM: value: 16.000, duration: 0.250, rq: 1/4, is-rest: NIL, 
[...] 
RHYTHM: value: 8.000, duration: 0.500, rq: 1/2, is-rest: T, 
[...] 
RHYTHM: value: 4.000, duration: 1.000, rq: 1, is-rest: NIL, 
[...] 
RHYTHM: value: 8.000, duration: 0.500, rq: 1/2, is-rest: T, 
[...] 
RHYTHM: value: 16.000, duration: 0.250, rq: 1/4, is-rest: NIL, 
[...] 
RHYTHM: value: 16.000, duration: 0.250, rq: 1/4, is-rest: NIL, 
[...] 
RHYTHM: value: 5.333, duration: 0.750, rq: 3/4, is-rest: NIL, 
[...]
RHYTHM: value: 16.000, duration: 0.250, rq: 1/4, is-rest: NIL, 
[...]
)

;; Get just the rhythm labels from the same rthm-seq object
(let ((rs (make-rthm-seq '((((2 4) q+e s s)
                            ((e) q (e))
                            ((3 8) s s e. s))
                           :pitch-seq-palette ((1 2 3 1 1 2 3 4))))))
  (loop for r in (get-rhythms rs) collect (data r)))

=> ("Q" "E" S S E Q E S S E. S)

|#
;;; SYNOPSIS
(defmethod get-rhythms ((rs rthm-seq))
;;; ****
  (loop for bar in (bars rs) appending 
       (loop for r in (rhythms bar) collect r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod split-longer-rests ((rs rthm-seq))
  (setf (bars rs) (loop for bar in (bars rs)
                     collect (split-longer-rests bar)))
  rs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 28.1.10
;;; Assuming both rthm-seqs have the same number of beats (duration)
;;; make rs have the same metrical structure as rsmaster.
;;; NB this doesn't attempt to divide up rhythms: if the old rhythms won't fit
;;; as they are into the new meters we'll fail.
;;; if clone, rs will be cloned
(defmethod adopt-meters ((rs rthm-seq) (rsmaster rthm-seq) 
                         &key (clone t) (is-full-error 'warn))
  (unless (= (duration rs) (duration rsmaster))
    (error "adopt-meters: both rthm-seqs must have the same ~
            duration: ~a (~a) vs. ~a (~a) ~&~a~&~a"
           (duration rs) (id rs) (duration rsmaster) (id rsmaster)
           (data rs) (data rsmaster)))
  (let* ((new-bars (loop for bar in (bars rsmaster) collect 
                        (make-rest-bar (clone (get-time-sig bar)) t)))
         (rsret (split-longer-rests (if clone (clone rs) rs)))
         ;; we'll usually adopt the meters of the rthm-seq with the least bars
         ;; so use the bar count from rsmaster
         (bar-num (bar-num (first (bars rsmaster))))
         (nth-seq (nth-seq (first (bars rsmaster))))
         (rthms (get-rhythms rsret)))
    (setf (bars rsret)
          (loop for bar in new-bars for count from 1 with ate = 0 with temp do
               (setf temp (fill-with-rhythms
                           bar (subseq rthms ate)
                           ;;MDE Thu Dec  8 23:55:31 2011 -- changed to key arg
                           ;; :warn nil :is-full-error nil))
                           :warn nil :is-full-error is-full-error))
               (if temp
                   (progn
                     (incf ate temp)
                     (setf (bar-num bar) bar-num ;; (print bar-num)
                           (nth-seq bar) nth-seq)
                     (incf bar-num))
                   (return))
               collect bar)
          (num-bars rsret) (num-bars rsmaster))
    (if (bars rsret)
        (progn
          (gen-stats rsret)
          (update-write-time-sig rsret)
          rsret)
        nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod update-write-time-sig ((rs rthm-seq)
                                 &optional ignore1 ignore2 ignore3)
  (declare (ignore ignore1 ignore2 ignore3))
  (loop with ts-last = (get-time-sig (first (bars rs)))
     for bar in (rest (bars rs))
     for ts-this = (get-time-sig bar)
     do
       (setf (write-time-sig bar)
             (if (time-sig-equal ts-last ts-this)
                 nil
                 t))
       (setf ts-last ts-this))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf bars) :after (value (rs rthm-seq))
  (declare (ignore value))
  (gen-stats rs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-time-sigs-tag ((rs rthm-seq))
  (if (time-sigs-tag rs)
      (time-sigs-tag rs)
      (let* ((tss (loop for bar in (bars rs) collect (id (get-time-sig bar)))))
        (setf (time-sigs-tag rs) (list-to-string tss "-")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;   
;;; SAR Wed Dec 28 19:24:23 EST 2011: Added robodoc info
;;; SAR Sat Dec 31 09:26:36 EST 2011: Put date in DATE block

;;; ****m* rthm-seq/split
;;; DATE
;;; 27 Jan 2011
;;; 
;;; FUNCTION
;;; Splits the rthm-seq-bar objects of a given rthm-seq object into multiple
;;; smaller rthm-seq-bar objects, creating a new rthm-seq object with a greater
;;; number of bars than the original. This will only work if the given
;;; rthm-seq-bar objects can be split into whole beats; e.g., a 4/4 bar will
;;; not be split into 5/8 + 3/8.
;;;
;;; The keyword arguments :min-beats and :max-beats serve as guidelines rather
;;; than strict cut-offs. In some cases, the method may only be able to
;;; effectively split the given rthm-seq-bar by dividing it into segments that
;;; slightly exceed the length stipulated by these arguments (see example
;;; below). 
;;;
;;; Depending on the min-beats/max-beats arguments stipulated by the user or
;;; the rhythmic structure of the given rthm-seq-bar objects, the given
;;; rthm-seq-bar or rthm-seq objects may not be splittable, in which case NIL
;;; is returned. If the keyword argument :warn is set to T, a warning will be
;;; also printed in such cases.
;;;
;;; NB: This method sets the values of the individual slots but leaves the DATA
;;; slot untouched (for cases in which the user might want to see where the new
;;; data originated from, or otherwise use the old data somehow, such as in a
;;; new rthm-seq object).
;;;
;;; ARGUMENTS 
;;; - A rthm-seq object.
;;;
;;; OPTIONAL ARGUMENTS
;;; - keyword argument :min-beats. This argument takes an integer value to
;;; indicate the minimum number of beats in any of the new rthm-seq-bar
;;; objects created. This serves as a guideline only and may occasionally be
;;; exceeded in value by the method. Default value = 2.
;;; - keyword argument :max-beats. This argument takes an integer value to
;;; indicate the maximum number of beats in any of the new rthm-seq-bar objects
;;; created. This serves as a guideline only and may occasionally be exceeded
;;; in value by the method. Default value = 5.
;;; - keyword argument :warn. Indicates whether to print a warning if the
;;; rthm-seq-bar object is unsplittable. Value T = print a warning. Defaults to
;;; NIL. 
;;; 
;;; RETURN VALUE  
;;; A rthm-seq object.
;;; 
;;; EXAMPLE
#|
;; The method returns a new rthm-seq object
(let ((rs (make-rthm-seq '((((4 4) q e s s (e) e e (e))
                            ((3 4) s s e s e s e. s)
                            ((5 4) h q. e e s s))
                           :pitch-seq-palette ((1 2 3 4 5 6 1 2 3 4 5 6 7 8 1 2
                                                3 4 5 6))))))
  (split rs))

=>

RTHM-SEQ: num-bars: 5
          num-rhythms: 22
          num-notes: 20
          num-score-notes: 20
          num-rests: 2
          duration: 12.0
          psp-inversions: NIL
          marks: NIL
          time-sigs-tag: NIL
          handled-first-note-tie: NIL
         (for brevity's sake, slots pitch-seq-palette and bars are not printed)
SCLIST: sclist-length: 3, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: NIL, tag: NIL, 
data: ((((4 4) Q E S S (E) E E (E)) ((3 4) S S E S E S E. S)
        ((5 4) H Q. E E S S))
       PITCH-SEQ-PALETTE ((1 2 3 4 5 6 1 2 3 4 5 6 7 8 1 2 3 4 5 6)))

;; Without setting the :min-beats and :max-beats arguments, the following
;; rthm-seq object is broken down from 3 to 5 rthm-seq-bar objects
(let* ((rs (make-rthm-seq '((((4 4) q e s s (e) e e (e))
                             ((3 4) s s e s e s e. s)
                             ((5 4) h q. e e s s))
                            :pitch-seq-palette ((1 2 3 4 5 6 1 2 3 4 5 6 7 8 1 2
                                                 3 4 5 6)))))
       (rssplt (split rs)))
  (print-simple rssplt))

=>
rthm-seq NIL
(2 4): note Q, note E, note S, note S, 
(2 4): rest E, note E, note E, rest E, 
(3 4): note S, note S, note E, note S, note E, note S, note E., note S, 
(2 4): note H, 
(3 4): note Q., note E, note E, note S, note S,

;; Setting :min-beats to 4 affects the resulting subdivisions to larger bars
(let* ((rs (make-rthm-seq '((((4 4) q e s s (e) e e (e))
                             ((3 4) s s e s e s e. s)
                             ((5 4) h q. e e s s))
                            :pitch-seq-palette ((1 2 3 4 5 6 1 2 3 4 5 6 7 8 1 2
                                                 3 4 5 6)))))
       (rssplt (split rs :min-beats 4)))
  (print-simple rssplt))

=>
rthm-seq NIL
(4 4): note Q, note E, note S, note S, rest E, note E, note E, rest E, 
(3 4): note S, note S, note E, note S, note E, note S, note E., note S, 
(5 4): note H, note Q., note E, note E, note S, note S, 

;; Even though :max-beats is set to 2, an occasional 3/4 bar is constructed
(let* ((rs (make-rthm-seq '((((4 4) q e s s (e) e e (e))
                             ((3 4) s s e s e s e. s)
                             ((5 4) h q. e e s s))
                            :pitch-seq-palette ((1 2 3 4 5 6 1 2 3 4 5 6 7 8 1 2
                                                 3 4 5 6)))))
       (rssplt (split rs :max-beats 2)))
  (print-simple rssplt))

=>
rthm-seq NIL
(2 4): note Q, note E, note S, note S, 
(2 4): rest E, note E, note E, rest E, 
(3 4): note S, note S, note E, note S, note E, note S, note E., note S, 
(2 4): note H, 
(3 4): note Q., note E, note E, note S, note S,

|#
;;; SYNOPSIS
(defmethod split ((rs rthm-seq) 
                  &key (min-beats 2) (max-beats 5) warn (clone t))
;;; ****
  (let ((ret (if clone (clone rs) rs)))
    (setf (bars ret)
          (loop for count from 1
             for bar in (bars ret)
             for split-bars = 
               (progn
                 (unless (and bar (rthm-seq-bar-p bar ))
                   (error "bar ~a is not a rthm-seq-bar:~%~a"
                          count bar))
                 (split bar :min-beats min-beats 
                        :max-beats max-beats :warn warn))
             if split-bars append split-bars
             else do 
             (when warn
               (warn "rthm-seq::split: couldn't split bar ~a" count))
             and collect bar))
    (gen-stats ret)
    (update-write-time-sig ret)
    ret))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****f* rthm-seq/make-rthm-seq
;;; FUNCTION
;;; Creates a rthm-seq object from a list of at least bars and generally also a
;;; list of pitch sequences. 
;;; 
;;; ARGUMENTS 
;;; - A list with the following items:
;;;   - A symbol that will be used as the ID of the seq
;;;   - Another list, containing two items:
;;;     - A list of rthm-seq-bars and
;;;     - A list of pitch-seqs attached to the :pitch-seq-palette accessor
;;;
;;; OPTIONAL ARGUMENTS
;;; - keyword argument :psp-inversions. 
;;; 
;;; RETURN VALUE  
;;; Returns a rthm-seq object.
;;; 
;;; EXAMPLE
#|
;; Make a rthm-seq object with the ID seq1 that contains one 2/4 bar of
;; rhythms and one pitch sequence in the pitch-seq-palette
(make-rthm-seq '(seq1 ((((2 4) q e s s))
                       :pitch-seq-palette ((1 2 3 4)))))

=> 
RTHM-SEQ: num-bars: 1
          num-rhythms: 4
          num-notes: 4
          num-score-notes: 4
          num-rests: 0
          duration: 2.0
          psp-inversions: NIL
          marks: NIL
          time-sigs-tag: NIL
          handled-first-note-tie: NIL
         (for brevity's sake, slots pitch-seq-palette and bars are not printed)
SCLIST: sclist-length: 3, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: SEQ1, tag: NIL, 
data: ((((2 4) Q E S S)) PITCH-SEQ-PALETTE (1 2 3 4))

;; A rthm-seq object with two bars of rhythms and two pitch-seqs in the
;; pitch-seq-palette. There must be as many items in each pitch-seq list as
;; there are rythms in each rthm-seq-bar.
(make-rthm-seq '(seq1 ((((2 4) q e s s)
                        ((e) q (e)))
                       :pitch-seq-palette ((1 2 3 4 5)
                                           (2 4 6 8 10)))))

=> 
RTHM-SEQ: num-bars: 2
          num-rhythms: 7
          num-notes: 5
          num-score-notes: 5
          num-rests: 2
          duration: 4.0
          psp-inversions: NIL
          marks: NIL
          time-sigs-tag: NIL
          handled-first-note-tie: NIL
         (for brevity's sake, slots pitch-seq-palette and bars are not printed)
SCLIST: sclist-length: 3, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: SEQ1, tag: NIL, 
data: ((((2 4) Q E S S) ((E) Q (E))) PITCH-SEQ-PALETTE
       ((1 2 3 4 5) (2 4 6 8 10)))

;; The pitch-seq-palette may be omitted, and time signatures may be changed 
(make-rthm-seq '(seq1 ((((2 4) q e s s)
                        ((e) q (e))
                        ((3 8) s s e. s)))))

=> 
RTHM-SEQ: num-bars: 3
          num-rhythms: 11
          num-notes: 9
          num-score-notes: 9
          num-rests: 2
          duration: 5.5
          psp-inversions: NIL
          marks: NIL
          time-sigs-tag: NIL
          handled-first-note-tie: NIL
         (for brevity's sake, slots pitch-seq-palette and bars are not printed)
SCLIST: sclist-length: 1, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: SEQ1, tag: NIL, 
data: ((((2 4) Q E S S) ((E) Q (E)) ((3 8) S S E. S)))

|#
;;; SYNOPSIS
(defun make-rthm-seq (rs &key (psp-inversions nil))
;;; ****
  ;; if a list then it should be two-elements long, the first the id, the
  ;; second the data.  
  (let ((result
         (cond  
          ((typep rs 'rthm-seq) rs)
          ((listp rs) 
           ;; 4.8.10 if it's just a list of rthms, there's no id, otherwise
           ;; it's a 2-element list: (id (rthms....))  
           (if (and (second rs) (listp (second rs)))
               (make-instance 'rthm-seq :id (first rs) :data (second rs))
               (make-instance 'rthm-seq :id nil :data rs)))
          ;; otherwise it's already a named-object with a list as data...
          ((and (typep rs 'named-object) (listp (data rs)))
           (make-instance 'rthm-seq :id (id rs) 
                          :data (copy-list (data rs))))
          (t (error "rthm-seq::make-rthm-seq: Can't make a rthm-seq from ~a"
                    rs)))))
    (when psp-inversions
      (setf (psp-inversions result) t)
      (add-inversions (pitch-seq-palette result)))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#| 
MDE Mon Dec 12 08:59:36 2011 -- obsolete code from the SCORE days
(defun write-seqs-to-score-file (file rthm-seqs &optional
                                                (left-margin 1.2) 
                                                (right-margin 200)
                                 &key (staff-offset 0))
  (with-open-file
      (stream file
       :direction :output :if-exists :overwrite :if-does-not-exist :create)
    (loop for rs in rthm-seqs
        and staff-num from (+ staff-offset (length rthm-seqs)) by -1 do
          (let ((score-strings (get-score-strings rs)))
            (format stream "IN~a~%~a ~a 1~%~a~%~a~%~a~%~a~%~a~%~%" 
                    staff-num
                    left-margin
                    right-margin
                    (first score-strings) ; notes
                    (second score-strings) ; rhythms
                    (marks rs)          ; marks
                    (third score-strings) ; beams
                    (fourth score-strings)))))) ; ties |#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; todo: should be able to verify-and-store when adding (add) data to an
;;;       assoc-list--fails and causes an rsp to have num-data 0 
;;; 
;;; SAR Tue Dec 27 16:58:57 EST 2011: Added robodoc info

;;; ****f* rthm-seq/make-rthm-seq-from-unit-multipliers
;;; FUNCTION
;;; Given a rhythmic unit, e.g. 32, a list of multipliers (e.g. '(7 9 16)),
;;; and a time signature (e.g. '(4 4)), return a rthm-seq object made up of
;;; bars whose rhythms are multiples of the specified unit by the numbers in
;;; the multipliers list.  
;;;
;;; At this point the unit should be a whole number divisor of the beat in the
;;; time signature, i.e. quintuple eighths won't work in 4/4.
;;;
;;; NB: Setting the auto-beam keyword argument to T can result in errors if
;;; creating durations longer than 1 beat, as auto-beam will call
;;; get-beats. :auto-beam is therefore set to NIL by default.
;;; 
;;; ARGUMENTS 
;;; - A rhythmic duration unit.
;;; - A list of multipliers.
;;; - A time signature.
;;;
;;; OPTIONAL ARGUMENTS
;;; - keyword argument :tuplet. Can be set to T or NIL. Default = NIL.
;;; - keyword argument :tag.  Can be set to T or NIL. Default = NIL.
;;; - keyword argument :auto-beam.  Can be set to T or NIL. Default = NIL. When
;;; T, the method will attempt to automatically set beaming indicators among
;;; the resulting rthm-seq-bar objects. This can result in errors if the
;;; resulting rhythms have a duration of more than 1 beat.
;;; - keyword argument :id. Default = "from-multipliers".
;;; 
;;; RETURN VALUE  
;;; Returns a rthm-seq object.
;;; 
;;; EXAMPLE
#| 
;; Make a rthm-seq object using the rhythmic unit of a 16th-note, rhythms that
;; are 4, 2, 2, 4 and 4 16th-notes long, and a time signature of 2/4; then
;; print-simple the object returned to see the results.
(let ((rs (make-rthm-seq-from-unit-multipliers 's '(4 2 2 4 4) '(2 4))))
  (print-simple rs))

=>
rthm-seq from-multipliers
(2 4): note Q, note E, note E, 
(2 4): note Q, note Q, 

;; Make a rthm-seq object using the rhythmic unit of a 32nd note, combinations
;; of irregular duration, and a time signature of 4/4; then print-simple the
;; returned object to see the results.
(let ((rs (make-rthm-seq-from-unit-multipliers 32 '(7 9 16) '(4 4))))
  (print-simple rs))

=>
rthm-seq from-multipliers
(4 4): note E.., note 32, note Q, note H

;; The print-simple output of the above example disregards the ties. We can
;; check to make sure that there are only three attacked rhythms in the result
;; by reading the values of the IS-TIED-FROM and IS-TIED-TO slots, which show
;; that the 32 is tied to the Q
(let ((rs (make-rthm-seq-from-unit-multipliers 32 '(7 9 16) '(4 4))))
  (loop for b in (bars rs)
       collect (loop for r in (rhythms b) collect (is-tied-from r))
       collect (loop for r in (rhythms b) collect (is-tied-to r))))

=> ((NIL T NIL NIL) (NIL NIL T NIL)) 
|#
;;; SYNOPSIS
(defun make-rthm-seq-from-unit-multipliers (unit multipliers time-sig 
                                            &key
                                            ;; a number for brackets over
                                            ;; each beat.
                                            (tuplet nil)
                                            (tag nil)
                                            (auto-beam nil) ; see above
                                            (id "from-multipliers"))
;;; ****
  ;; (print 'make-rthm-seq-from-unit-multipliers)
  (let* ((tsig (if (time-sig-p time-sig)
                   time-sig
                   (make-time-sig time-sig)))
         (beat (denom tsig))
         (unit-rthm (make-rhythm unit))
         (units-per-beat (/ (value unit-rthm) beat))
         (beats-per-bar (num tsig))
         (units-per-bar (floor (* units-per-beat beats-per-bar)))
         (all
          (loop for m in multipliers 
             ;; 16/1/10: got to handle the case of just 1!
             for temp = (if (= 1 m)
                            (list (make-rhythm unit))
                            (loop for i below m 
                               for r = (make-rhythm unit)
                               do
                               (cond ((zerop i) (setf (is-tied-from r) t))
                                     ((= i (1- m)) (setf (is-tied-to r) t))
                                     (t (setf (is-tied-to r) t)
                                        (setf (is-tied-from r) t)))
                               collect r))
             appending temp))
         (length (length all))
         (rests-needed (mod
                        (- units-per-bar (mod length units-per-bar))
                        units-per-bar))
         (bars '()))
    (setf all (flatten all)
          bars (loop with index = 0
                  with end = units-per-bar
                  for bar = (make-rest-bar tsig nil)
                  while (< index length)
                  do
                  (setf (rhythms bar) (subseq all index 
                                              (min length end)))
                  (when (>= end length) ;; last bar
                    (setf (rhythms bar) 
                          (append (rhythms bar)
                                  (loop for i below rests-needed
                                     collect (make-rest unit)))))
                  (consolidate-notes bar)
                  (consolidate-rests bar)
                  (when auto-beam
                    (auto-beam bar))
                  (when tuplet
                    (auto-put-tuplet-bracket-on-beats bar tuplet))
                  (gen-stats bar)
                  ;; 2/04
                  ;; 17/5/05: now handled at piece level
                  ;; (update-compound-durations bar)
                  (incf index units-per-bar)
                  (incf end units-per-bar)
                  collect bar))
    ;; have to make a 2-element list, the first is the id, the second the bars,
    ;; but the bars have to be in a list themselves....
    (let ((result (make-rthm-seq (list id (list bars)))))
      (when tag
        (setf (tag result) tag))
      result)))
              
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; <fragments> is a list of rhythms with ids

;;; <references> is a list of ids into fragments: these will be collated to
;;; create the rthm-seq.  Each element is a sublist: this will make up a whole
;;; bar according to the <meters> scheme.

;;; NB No pitch-seqs can be passed as yet.

;;; <meters> is a list of the meters (either single numerators: default-beat
;;; will then be the denominator) or num/denum lists

;;; SAR Tue Dec 27 18:54:58 EST 2011
;;; ****f* rthm-seq/make-rthm-seq-from-fragments
;;; DATE
;;; Jan 2010
;;; 
;;; FUNCTION
;;; 
;;; 
;;; ARGUMENTS 
;;; 
;;; 
;;; RETURN VALUE  
;;; 
;;; 
;;; EXAMPLE
#|

|#
;;; SYNOPSIS
(defun make-rthm-seq-from-fragments (id fragments references meters
                                     &optional (default-beat 4))
;;; ****
  (unless (= (length references) (length meters))
    (error "make-rthm-seq-from-fragments: references and meters must be ~
            same length: ~a ~a" references meters))
  (let* ((frag-al (make-assoc-list 'fragments fragments))
         (rs (loop with last-meter = -1
                for bar in references and meter in meters 
                for mtr = (unless (equal meter last-meter)
                            (if (listp meter)
                                meter
                                (list meter default-beat)))
                for rthms =
                (loop for ref in bar appending
                     (copy-list (get-data-data ref frag-al)))
                collect (if mtr 
                            (cons mtr rthms)
                            rthms)
                do
                (setf last-meter meter))))
    (make-instance 'rthm-seq :id id :data (list rs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rthm-seq-p (thing)
  (typep thing 'rthm-seq))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Tue Dec 27 19:04:04 EST 2011: Added robodoc info
;;; SAR Sat Dec 31 09:24:15 EST 2011: Put date in DATE block

;;; ****f* rthm-seq/make-rhythms
;;; DATE
;;; 11 Feb 2010
;;; 
;;; FUNCTION
;;; Initialize a group of rhythms, taking advantage of rthm-seq's ability to
;;; add tuplet and beaming info.
;;; 
;;; ARGUMENTS 
;;; - A list of rhythms equalling one full bar
;;; - The time signature of that bar as a list (e.g (2 4))
;;; 
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indiate whether to divide the resulting list into sublists,
;;; each of which are the equivalent of one beat long. Default = NIL.
;;;
;;; RETURN VALUE  
;;; - A list
;;; 
;;; EXAMPLE
#|
;; Apply the function and test that the result is a list
(let ((rs (make-rhythms '(q e s s) '(2 4))))
  (listp rs))

=> T

;; Apply the function and see that we've created a list with 4 elements
(let ((rs (make-rhythms '(q e s s) '(2 4))))
  (length rs))

=> 4

;; Apply the function with the optional split-into-beats argument set to T and
;; see that we now have two lists, each equalling one beat in combined
;; length. Print the data of the contents.
(let ((rs (make-rhythms '(q e s s) '(2 4) t)))
  (print (length rs))
  (print (loop for b in rs collect (length b)))
  (print (loop for b in rs 
            collect (loop for r in b 
                       collect (data r)))))

=>
2 
(1 3) 
((Q) (E S S))

;; Apply the function using beam indications then print the BEAM slots of the
;; individual rhythm objects contained in the result
(let ((rs (make-rhythms '(q - e s s -) '(2 4))))
  (loop for r in rs collect (beam r)))

=> (NIL 1 NIL 0)

;; Apply the function using tuplet indications then print the BRACKET slots of
;; the individual rhythms objects contained in the result
(let ((rs (make-rhythms '( { 3 te te te } - e s s -) '(2 4))))
  (loop for r in rs collect (bracket r)))

=> (((1 3)) (-1) (1) NIL NIL NIL)

|#
;;; SYNOPSIS
(defun make-rhythms (bar time-sig &optional split-into-beats)
;;; ****
  ;; rthm-seq rather than rthm-seq-bar because the former updates
  ;; tied slots 
  (let* ((rs (make-rthm-seq `(rthm-seq-auto (((,time-sig ,@bar))))))
         ;; nb there is by definition only one bar in this seq
         (bar (first (bars rs))))
    (if split-into-beats
        (get-beats bar)
        (rhythms bar))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EOF rthm-seq.lsp
