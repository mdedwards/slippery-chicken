;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* linked-named-object/bar-holder
;;; NAME 
;;; bar-holder
;;;
;;; File:             bar-holder.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> bar-holder  
;;;
;;; Version:          0.9.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          This class is meant to be subclassed by piece, section
;;;                   and sequence, all of which hold each other or, ultimately
;;;                   a list of bars with relevant rhythms, timings, pitches
;;;                   etc.   
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    16th February 2002
;;;
;;; $$ Last modified: 19:13:44 Mon Apr  9 2012 BST
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

;;; 02.12.11 SEAN: Changed ROBODoc header to reflect class hierarchy

(eval-when (compile)
  (declaim (optimize (speed 3) (safety 1) (space 0) (debug 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :slippery-chicken)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The bars, sections, or sequences are in the data slot.

(defclass bar-holder (linked-named-object)
  ((start-bar :accessor start-bar :initarg :start-bar :type integer
              :initform 0)
   (end-bar :accessor end-bar :initarg :end-bar :type integer :initform 0)
   (start-time :accessor start-time :initarg :start-time
               :initform 0)
   (end-time :accessor end-time :initarg :end-time :initform 0)
   (duration :accessor duration :initform 0)
   ;; the next three slots are the times/durations in crotchets (1/4 notes),
   ;; useful for midi timing.
 
   ;; 02.12.11 SEAN: Added ROBODoc info

   ;;****iS* bar-holder/start-time-qtrs
   ;; FUNCTION
   ;; Class slot to store the start-time of the bar-holder as measured in the
   ;; number of quarter-notes. Useful for MIDI timing.
   ;;
   ;; ARGUMENTS 
   ;; Accessor is start-time-qtrs. Initialized by keyword argument
   ;; :start-time-qtrs. This argument defaults to 0.  
   ;; 
   ;; EXAMPLE
   ;; (setf x (make-instance 'bar-holder :start-time-qtrs 10))
   ;; (start-time-qtrs x)
   ;; => 10 (4 bits, #xA, #o12, #b1010)
   ;;
   ;; SYNOPSIS
   (start-time-qtrs :accessor start-time-qtrs :initarg :start-time-qtrs
                    :initform 0)
   ;; ****
   ;; 02.12.11 SEAN: Added ROBODoc info
   ;;****iS* bar-holder/end-time-qtrs
   ;; FUNCTION
   ;; Class slot to store the end-time of the bar-holder as measured in the
   ;; number of quarter-notes. Useful for MIDI timing.
   ;; 
   ;; ARGUMENTS 
   ;; Accessor is end-time-qtrs. Initialized by keyword argument
   ;; :end-time-qtrs. This argument defaults to 0. 
   ;; 
   ;; EXAMPLE
   ;; (setf x (make-instance 'bar-holder :end-time-qtrs 20))
   ;; (end-time-qtrs x)
   ;; => 20 (5 bits, #x14, #o24, #b10100)
   ;; 
   ;; SYNOPSIS
   (end-time-qtrs :accessor end-time-qtrs :initarg :end-time-qtrs
                  :initform 0)
   ;; ****

   ;; 02.12.11 SEAN: Added ROBODoc info
   ;;****iS* bar-holder/duration-qtrs
   ;; FUNCTION
   ;; Class slot to store the duration of the bar-holder as measured in the
   ;; number of quarter-notes. Useful for MIDI timing.
   ;; 
   ;; ARGUMENTS 
   ;; Accessor is duration-qtrs. This slot has no initialization argument. It
   ;; defaults to 0.  
   ;;
   ;; EXAMPLE
   ;; (setf x (make-instance 'bar-holder))
   ;; (setf (duration-qtrs x) 10)
   ;; => 10 (4 bits, #xA, #o12, #b1010)
   ;; 
   ;; (duration-qtrs x)
   ;; => 10 (4 bits, #xA, #o12, #b1010)
   ;; 
   ;; SYNOPSIS
   (duration-qtrs :accessor duration-qtrs :initform 0)
   ;; ****
   (num-bars :accessor num-bars :type integer :initform 0)
   (num-notes :accessor num-notes :type integer :initform 0)
   (num-score-notes :accessor num-score-notes :type integer :initform 0)
   (num-rests :accessor num-rests :type integer :initform 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone ((bh bar-holder))
  (clone-with-new-class bh 'bar-holder))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone-with-new-class :around ((bh bar-holder) new-class)
  (declare (ignore new-class))
  (let ((ral (call-next-method)))
    (setf (slot-value ral 'start-bar) (start-bar bh) 
          (slot-value ral 'end-bar) (end-bar bh) 
          (slot-value ral 'num-bars) (num-bars bh) 
          (slot-value ral 'start-time) (start-time bh) 
          (slot-value ral 'end-time) (end-time bh) 
          (slot-value ral 'start-time-qtrs) (start-time-qtrs bh) 
          (slot-value ral 'end-time-qtrs) (end-time-qtrs bh) 
          (slot-value ral 'num-notes) (num-notes bh) 
          (slot-value ral 'num-score-notes) (num-score-notes bh) 
          (slot-value ral 'num-rests) (num-rests bh) 
          (slot-value ral 'duration) (duration bh)
          (slot-value ral 'duration-qtrs) (duration-qtrs bh))
    ral))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((bh bar-holder) stream)
  (format stream "~&BAR-HOLDER: ")
  (print-bar-holder-slots bh stream))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-bar-holder-slots ((bh bar-holder) stream)
  (format stream "~&            start-bar: ~a~
                  ~%            end-bar: ~a~
                  ~%            num-bars: ~a~
                  ~%            start-time: ~a~
                  ~%            end-time: ~a~
                  ~%            start-time-qtrs: ~a~
                  ~%            end-time-qtrs: ~a~
                  ~%            num-notes (attacked notes, not tied): ~a~
                  ~%            num-score-notes (tied notes counted ~
                                separately): ~a ~
                  ~%            num-rests: ~a~
                  ~%            duration-qtrs: ~a ~
                  ~%            duration: ~a (~a)~%~%"
          (start-bar bh)
          (end-bar bh)
          (num-bars bh)
          (start-time bh)
          (end-time bh)
          (start-time-qtrs bh)
          (end-time-qtrs bh)
          (num-notes bh)
          (num-score-notes bh)
          (num-rests bh)
          (duration-qtrs bh)
          (duration bh) (secs-to-mins-secs (duration bh))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod statistics ((bh bar-holder) &optional (stream t))
  (print-bar-holder-slots bh stream))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This should only be called for the piece object.  It will produce an error
;;; if called with a player-section or sequenz. Needs to be part of the
;;; bar-holder class though because of the recursive calls with sections etc.

(defmethod update-write-time-sig ((bh bar-holder) 
                                  &optional 
                                  (force nil) ;; to force first bar to t
                                  ;; these two for recursive calls only
                                  (last-bar nil)
                                  (players nil))
  ;; (print 'update-write-time-sig)
  (unless players
    (setf players (players bh)))
  (let ((lb last-bar))
    (loop for thing in (data bh) and i from 0 do
          (setf thing (get-bar-holder thing)
                lb (if (is-section-without-subsections thing)
                       (update-write-time-sig-aux thing (and force (zerop i))
                                                  lb players)
                     (update-write-time-sig thing (and force (zerop i))
                                            lb players))))
    lb))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod is-section-without-subsections ((bh bar-holder))
  (and (typep bh 'section)
       (not (has-subsections bh))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-last-bar ((bh bar-holder))
  (let* ((last (first (last (data bh))))
         (lbh (if last
                  (get-bar-holder last)
                (error "bar-holder::get-last-bar: no last!"))))
    (get-last-bar lbh)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; N.B. Although tempo-map is optional, it is in fact required
(defmethod update-slots ((bh bar-holder)
                         &optional
                         tempo-map
                         (start-time 0.0)
                         (start-time-qtrs 0.0)
                         (start-bar 1)
                         (current-section nil)
                         (nth nil)
                         (warn-ties t))
  (declare (ignore nth))
  (object-is-nil? tempo-map "bar-holder::update-slots" 'tempo-map)
  (let ((bar start-bar)
        (time start-time)
        (time-qtrs start-time-qtrs)
        (loop-update (not (is-section-without-subsections bh))))
    (unless loop-update
      (setf current-section (full-ref bh)))
    (setf (start-bar bh) start-bar
          (start-time bh) start-time
          (num-notes bh) 0
          (num-rests bh) 0
          (num-score-notes bh) 0)
    (loop for thing in (data bh) and i from 1 do ;; sequenzes are 1-based
          (when thing
            (setf thing (get-bar-holder thing))
            (update-slots thing tempo-map time time-qtrs bar current-section i
                         warn-ties) 
            (incf (num-notes bh) (num-notes thing))
            (incf (num-score-notes bh) (num-score-notes thing))
            (incf (num-rests bh) (num-rests thing))
            ;; only sections, subsections and sequenzes should update bar and
            ;; start-time, the player-sections all have the same start-time and
            ;; bar. 
            (when loop-update
              (setf bar (1+ (end-bar thing))
                    time-qtrs (end-time-qtrs thing)
                    time (end-time thing))))))
  (let ((last (if (is-ral bh)
                  (get-last bh)
                (first (last (data bh))))))
    (setf (num-bars bh) (- (end-bar last) (1- start-bar))
          (end-bar bh) (end-bar last)
          (end-time bh) (end-time last)
          (end-time-qtrs bh) (end-time-qtrs last)
          (duration-qtrs bh) (- (end-time-qtrs last) start-time-qtrs)
          (duration bh) (- (end-time last) start-time))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; When num-bars is nil, all bars in the bh from start-bar will be transposed.

;;; 02.12.11 SEAN: Added ROBODoc info
;;; ****m* bar-holder/transpose-bars
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
;;; 
;;; 
;;; DATE
;;; 
;;; 
;;; SYNOPSIS
(defmethod transpose-bars ((bh bar-holder) semitones start-bar num-bars player
                           &key
                           (destructively nil)
                           (print-bar-nums nil)
                           ;; the default functions are the class methods for
                           ;; pitch or chord.
                           (chord-function #'transpose)
                           (pitch-function #'transpose))
;;; ****
  (unless num-bars
    (setf num-bars (- (num-bars bh)
                      (bar-num (get-bar bh start-bar player))
                      -1)))
  (loop for bar-num from start-bar repeat num-bars do 
        (when print-bar-nums
          (format t "~&Transposing bar ~d" bar-num))
      collect
        (transpose (get-bar bh bar-num player)
                   semitones 
                   :destructively destructively
                   :chord-function chord-function
                   :pitch-function pitch-function)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Find bar number bar-num: if this object happens to be a piece or section,
;;; then the bars aren't directly in this object but in the sub-object so call
;;; this method recursively with the sub-object.
;;; N.B. although optional, the player argument is required.  It is optional so
;;; that we can have a sequenz method with the same name which only requires
;;; the bar-num argument. 

(defmethod get-bar ((bh bar-holder) bar-num &optional player)
  (unless player
    (error "bar-holder::get-bar: player argument is required!"))
  (loop 
     for object in (data bh) 
     for bhl = (get-bar-holder object nil) do
     ;; MDE Wed Feb  1 12:41:30 2012 
     (unless bhl
       (error "bar-holder::get-bar: couldn't get bar-holder. ~
                 bar-num = ~a, player = ~a" bar-num player))
     (when (and (>= bar-num (start-bar bhl))
                (<= bar-num (end-bar bhl)))
       ;;(format t "~a ~a" (start-bar bhl) (end-bar bhl))
       (return (get-bar bhl bar-num player)))))
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* bar-holder/get-note
;;; FUNCTION
;;; Return an event from a bar.  bar-num and note-num are 1-based.  note-num
;;; can access the note of a chord like '(2 1) where 2 is the second "note" or
;;; non-rhythm event in the bar, and 1 is the first note in the chord counting
;;; from the bottom.  NB note-num counts tied notes i.e. it's not the attack
;;; number 
;;;
;;; ARGUMENTS 
;;; - the bar-holder object (e.g. piece)
;;; - the bar number (starting from 1) 
;;; - the note number (starting from 1) (see above).
;;; - the player (symbol)
;;;
;;; OPTIONAL ARGUMENTS
;;; - (optional default nil) whether, when accessing a pitch in a chord,
;;;    whether to return the written or sounding pitch.  
;;; 
;;; RETURN VALUE  
;;; An event object, or pitch if accessing a chord.
;;; 
;;; SYNOPSIS
(defmethod get-note ((bh bar-holder) bar-num note-num player &optional written)
;;; ****
  (let* ((bar (get-bar bh bar-num player))
         (wants-chord (listp note-num))
         (nth-note (1- (if wants-chord (first note-num) note-num)))
         (chord-nth (when wants-chord (1- (second note-num))))
         (event
          (cond ((not bar)
                 (warn "bar-holder::get-note: couldn't get bar number ~a ~
                        for player ~a"
                       bar-num player))
                ((>= nth-note (num-score-notes bar))
                 (warn "bar-holder::get-note: couldn't get note number ~a ~
                        from bar ~a for ~a: s~%There are ~a score-notes ~
                        in this bar" 
                       note-num bar-num player (num-score-notes bar)))
                (t (get-nth-non-rest-rhythm nth-note bar))))
         (is-chord (when event (is-chord event))))
    (when event
      (if wants-chord
          (progn
            (unless is-chord
              (error "bar-holder::get-note: requested bar num ~a note num ~a ~
                    for player ~a but that note is not a chord!"
                     bar-num note-num player))
            (unless (= 2 (length note-num))
              (error "bar-holder::get-note: when accessing a chord, note-num ~
                    must be a 2 element list of integers: ~a"
                     note-num))
            (unless (< chord-nth is-chord)
              (warn "bar-holder::get-note requested bar num ~a note num ~a ~
                   for player ~a but chord has only ~a notes"
                    bar-num note-num player is-chord))
            (get-nth chord-nth (if written
                                   (written-pitch-or-chord event)
                                   (pitch-or-chord event))))
        event))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; bar-num and note-num are 1-based

(defmethod get-rest ((bh bar-holder) bar-num rest-num player)
  (let ((bar (get-bar bh bar-num player)))
    (cond ((not bar)
           (warn "bar-holder::get-rest: couldn't get bar number ~a ~
                  for player ~a"
                 bar-num player))
          ((> rest-num (num-rests bar))
           (warn "bar-holder::get-rest: couldn't get rest number ~a: ~
                  only ~a rests in this bar" 
                 rest-num (num-rests bar)))
          (t (get-nth-rest (1- rest-num) bar)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; bar-num and note-num are 1-based

(defmethod get-event ((bh bar-holder) bar-num event-num player)
  (let ((bar (get-bar bh bar-num player)))
    (unless bar
      (warn "bar-holder::get-event: couldn't get bar number ~a ~
             for player ~a"
            bar-num player))
    (get-nth-event (1- event-num) bar)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 30.3.11: turn a rest into a note by supplying a pitch or chord (as objects
;;; or symbols)
(defmethod rest-to-note ((bh bar-holder) bar-num rest-num player new-note
                         &rest marks)
  (setf marks (flatten marks)) ; in case passed as list from sc class
  (let ((rest (get-rest bh bar-num rest-num player)))
    (when rest
      (setf (pitch-or-chord rest) new-note))
    (loop for m in marks do
         (add-mark rest m))
    rest))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; bar-num & note-num are one-based; can't handle chords
(defmethod change-pitch ((bh bar-holder) bar-num note-num player new-pitch)
  (let ((event (get-note bh bar-num note-num player)))
    (when event
      ;; remember the event class setf method will handle different types for
      ;; new-note 
      (setf (pitch-or-chord event) new-pitch)))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* bar-holder/change-notes
;;; FUNCTION
;;; new-notes is a list of lists, each sublist being the notes for each bar in
;;; succession.  e.g. (change-notes bh 'vla 5 '((g3 gs4) nil (nil nil aqf5)))
;;; would change the notes in bars 5 and 7 (for the viola), whereas bar six,
;;; indicated by nil, wouldn't be changed; similarly the first two notes of bar
;;; 7, being nil, also won't be changed, but note 3 will be.  NB tied notes are
;;; counted here.
;;; 
;;; If use-last octave, then notes can be given like '((a3 b g cs4))
;;; i.e. you only give the octave if it changes. 
;;; 
;;; ARGUMENTS 
;;; - the bar-holder object (e.g. piece)
;;; - the sc player (symbol)
;;; - which bar to start at (integer)
;;; - a list of notes in bars (see above)
;;; - (optional default t): whether the last note's octave will
;;;    be used if any notes are specificed without an octave (doesn't work with
;;;    chords).  
;;; 
;;; RETURN VALUE  
;;; always t
;;;
;;; EXAMPLE
;;; (change-notes bh 'vla 5 '((g3 gs4) nil (nil nil aqf5)))
;;; 
;;; SYNOPSIS
(defmethod change-notes ((bh bar-holder) player start-bar new-notes 
                         &optional (use-last-octave t) ignore)
;;; ****
  (declare (ignore ignore))
  (loop for bar in new-notes and bar-num from start-bar do
       (loop for note in bar and note-num from 1 do
            (when note
              ;; 6.7.11: handle the chord case
              (when (and use-last-octave (not (listp note)))
                (multiple-value-bind
                      (n o)
                    (get-note-octave note use-last-octave)
                  (setf note (join-note-octave n o))))
              ;; (format t "~% ~a ~a ~a" bar-num note-num note)
              (change-note bh bar-num note-num player note))))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod add-mark-to-note ((bh bar-holder) bar-num note-num player mark)
  (let ((note (get-note bh bar-num note-num player)))
    (when note
      (add-mark note mark))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 28.2.11 use this to add marks to e.g. rests.  1-based
(defmethod add-mark-to-event ((bh bar-holder) bar-num event-num player
                                  mark)
  (let ((event (get-event bh bar-num event-num player)))
    (when event
      (add-mark event mark))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod add-mark-before-note ((bh bar-holder)
                                       bar-num note-num player mark)
  (let ((note (get-note bh bar-num note-num player)))
    (when note
      (add-mark-before note mark))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* bar-holder/delete-all-marks
;;; FUNCTION
;;; delete-all-marks:
;;;
;;; 
;;; 
;;; ARGUMENTS 
;;; 
;;; 
;;; RETURN VALUE  
;;; 
;;; 
;;; EXAMPLE
;;; 
;;; 
;;; DATE
;;; 
;;; 
;;; SYNOPSIS
(defmethod delete-all-marks ((bh bar-holder) start-bar num-bars player)
;;; ****
  (unless num-bars
    (setf num-bars (- (num-bars bh)
                      (bar-num (get-bar bh start-bar player))
                      -1)))
  (loop 
      for bar-num from start-bar
      for bar = (get-bar bh bar-num player)
      repeat num-bars 
      do 
        (delete-marks bar))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod bh-delete-marks ((bh bar-holder) bar-num note-num player)
  (let ((note (get-note bh bar-num note-num player)))
    (when note
      (setf (marks note) nil)))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod delete-marks-before ((bh bar-holder) bar-num note-num player)
  (let ((note (get-note bh bar-num note-num player)))
    (when note
      (setf (marks-before note) nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; note-num is the note where the tie starts, it is automatically tied to the
;;; next note whether it's in the same bar or the next.

(defmethod tie ((bh bar-holder) bar-num note-num player &optional curvature)
  (let* ((start-note (get-note bh bar-num note-num player))
         (bar (get-bar bh bar-num player))
         ;; bar-num could have been a reference, so get the real bar-num now.
         (real-bar-num (bar-num bar))
         (notes (num-score-notes bar))
         (end-note (if (= note-num notes)
                       (get-note bh (1+ real-bar-num) 1 player)
                     (get-note bh bar-num (1+ note-num) player))))
    (setf (is-tied-from start-note) (if curvature curvature t)
          (is-tied-to end-note) t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-bar-holder (id data &optional (start-bar 0) (start-time 0)
                                          (end-bar 0) (end-time 0))
  (make-instance 'bar-holder :id id :data data :start-bar start-bar
                 :start-time start-time :end-bar end-bar :end-time end-time))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun is-bar-holder (thing)
  (typep thing 'bar-holder))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Sometimes when looping we have a bar-holder object, sometimes a
;;; named-object whose data is a bar-holder.  Determine which we have and
;;; return the bar-holder

(defun get-bar-holder (thing &optional (error t))
  (cond ((is-bar-holder thing) thing)
        ((and (named-object-p thing) (data thing) (is-bar-holder (data thing)))
         (data thing))
        (t (when error
             (error "bar-holder::get-bar-holder: Can't get bar-holder from ~a"
                    thing)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EOF bar-holder.lsp
