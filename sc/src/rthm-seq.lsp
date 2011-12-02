;;; 02.12.11 SEAN: Changed ROBODoc header to reflect class hierarchy
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
;;; $$ Last modified: 11:59:01 Wed Mar 16 2011 GMT
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
   ;; markings for score, eg "s 18/f 2;" etc.
   (marks :accessor marks :type string :initarg :marks :initform ";")
   (cmn-marks :accessor cmn-marks :type list :initarg :cmn-marks :initform nil)
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
   (time-sigs-tag :accessor time-sigs-tag :type string :initform nil)
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
      (handle-cmn-marks rs)
      (add-cmn-marks rs)
      (init-psp rs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; cmn-marks are expected like this:
;;; ((s 1 2 5 6) (a 13 14) (p 1) (f 13) (p 15))))
;;; i.e. with sublists, one for each accent
;;; it's easier like this though: (as 1 5 6 t 11 15 16)))
;;; so change the latter into the former

(defmethod handle-cmn-marks ((rs rthm-seq))
  (let ((mks (cmn-marks rs)))
    (when (and mks (simple-listp mks))
      (setf (cmn-marks rs)
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
                  ~%          cmn-marks: ~a~
                  ~%          time-sigs-tag: ~a~
                  ~%          handled-first-note-tie: ~a~
                  ~%         (for brevity's sake, slots ~
                  pitch-seq-palette and bars are not printed)"
          (num-bars i) (num-rhythms i) (num-notes i) (num-score-notes i)
          (num-rests i) (duration i) (psp-inversions i) (marks i) (cmn-marks i)
          (time-sigs-tag i) (handled-first-note-tie i)))

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
          (slot-value sclist 'marks) (basic-copy-object (marks rs))
          (slot-value sclist 'cmn-marks) (my-copy-list (cmn-marks rs))
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

(defmethod get-nth-non-rest-rhythm (index (rs rthm-seq)
                                    &optional (error t))
  (loop 
      for bar in (bars rs) 
      for nsn = (num-score-notes bar)
      do
        (if (< index nsn)
            (return (get-nth-non-rest-rhythm index bar error))
          (decf index nsn))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-nth-attack (index (rs rthm-seq)
                           &optional (error t))
  (loop 
      for bar in (bars rs) 
      for bar-count from 0
      for nnn = (notes-needed bar)
      do
        (if (< index nnn)
            (multiple-value-bind
                (event nth-in-bar)
                (get-nth-attack index bar error)
              (return (values event bar-count nth-in-bar)))
          (decf index nnn))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; NB this does not check that the right rhythms are now in the bar!

(defmethod set-nth-attack (index (e event) (rs rthm-seq)
                           &optional (error t))
  (loop 
      for bar in (bars rs) 
      for nnn = (notes-needed bar)
      do
        (if (< index (print nnn))
            (return (set-nth-attack index e bar error))
          (decf index nnn))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Can't use the sclist method because the bars are stored in the bars slot,
;;; not in the data slot. 

(defmethod set-nth-bar (index new-bar (rs rthm-seq))
  (when (and rs (rthm-seq-check-bounds rs index))
    (setf (nth index (bars rs)) new-bar)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(defmethod get-nth-bar (nth (rs rthm-seq))
  (when (and rs (rthm-seq-check-bounds rs nth))
    (nth nth (bars rs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-last-bar ((rs rthm-seq))
  (get-nth-bar (1- (num-bars rs)) rs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-last-attack ((rs rthm-seq) &optional (warn t))
  (get-last-attack (get-last-bar rs) warn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-last-event ((rs rthm-seq))
  (get-last-event (get-last-bar rs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Insert a bar in the rthm-seq and re-init it; if there's a
;;; pitch-seq given (list of numbers, or list of lists), splice this
;;; in in the appropriate place.

;;; bar-num is the bar number of the bar to be inserted, relative to
;;; the rthm-seq and 1-based, e.g. if 3, then it will come before the
;;; present third bar.

;;; We assume here that ties are taken care of within the new bar!

;;; TODO: test that the pitch-seq-palette splicing actually works; add
;;; inversions if that was in the original 

(defmethod insert-bar ((rs rthm-seq) (rsb rthm-seq-bar) bar-num
                       &optional pitch-seq ignore1 ignore2 ignore3)
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

(defmethod get-time-sigs ((rs rthm-seq) &optional as-list)
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

;;; Combine two rthm-seqs into one, updating slots for the new object (it is a
;;; clone).
;;; N.B. marks slot is ignored for now (it is as of yet unused)

(defmethod combine ((rs1 rthm-seq) (rs2 rthm-seq))
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

(defmethod add-bar ((rs rthm-seq) (rsb rthm-seq-bar)) ;; &optional psp)
  (setf (bars rs) (econs (bars rs) rsb))
  (incf (num-bars rs))
  (incf (num-rhythms rs) (num-rhythms rsb))
  (incf (num-rests rs) (num-rests rsb))
  (incf (num-notes rs) (notes-needed rsb))
  (incf (num-score-notes rs) (num-score-notes rsb))
  (incf (duration rs) (duration rsb))
  ;; TODO: what about the psp?
  )

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

;;; Calls chop-bar for each rsb in the seq--see rthm-seq-bar::chop-bar for
;;; details.  Returns a list of rthm-seqs each containing just one of the bars
;;; returned by chop-bar.

(defmethod chop ((rs rthm-seq) &optional chop-points 
                                         (unit 's)
                                         (number-bars-first t))
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
                 ;; we stored the positions of the start and end notes of the
                 ;; old bar that's cannibalised in
                 ;; rthm-seq-bar::new-bar-from-time-range. We use these numbers
                 ;; ___plus___ the number of attacked notes in the bars
                 ;; previous to the current in order to get a sub-sequence out
                 ;; of the pitch-seq-palette and apply it to the new rthm-seq.
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
                (setf (pitch-seq-palette rs) 
                  (psp-subseq psp
                              (+ attacks (first pse))
                              (+ attacks (second pse)))))
              (push rs result)
              (incf count))
        (incf attacks (notes-needed bar))
      finally (return (nreverse result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod set-bar-nums ((rs rthm-seq) &optional (start-bar 1))
  (loop for b in (bars rs) and bar-num from start-bar do
        (setf (bar-num b) bar-num)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod add-cmn-marks ((rs rthm-seq))
  (loop for i in (cmn-marks rs) do
        ;; when the list is like (a 1 4) it means accent on notes 1 to 4
        ;; (a 1) means accent on note 1
        ;; (a 1 4 6 8) means accents on notes 1, 4, 6 and 8
        ;; if you want an accent on notes 1 and 4, you have to do (a 1) (a 4)
        (if (> (length i) 3)
            (loop for note in (cdr i) with mark = (first i) do
                  (add-cmn-marks-aux rs mark note))
          ;; we have a start-end note pair...
          (add-cmn-marks-aux rs (first i) (second i) (third i)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 14/4/07: no longer create real cmn marks here, rather just the symbol for
;;; the mark that will be created when get-cmn-data is called. 

#+cmn
(defmethod add-cmn-marks-aux ((rs rthm-seq) mark start-note &optional end-note)
  ;; get-nth-non-rest-rhythm is 0-based, we're 1-based here.
  (decf start-note)
  (if end-note
      (decf end-note)
    (setf end-note start-note))
  (when (> end-note (1- (num-score-notes rs)))
    (error "~a~%sequenz::add-cmn-marks-aux: ~a notes in seq, but mark on ~a"
           rs (num-score-notes rs) (1+ end-note)))
  ;; cond in case we want to add other special cases later...
  (cond ((eq mark 'slur)
         ;; slurs are a special case...
         (unless (> end-note start-note)
           (error "sequenz::add-cmn-marks-aux: slurs must be over ~
                   more than one note: (~a ~a)" 
                  start-note end-note))
         (add-cmn-mark (get-nth-non-rest-rhythm start-note rs) 
                       ;; (first (cmn::get-cmn-marks 'begin-slur)))
                       'beg-sl)
         (add-cmn-mark (get-nth-non-rest-rhythm end-note rs) 
                       ;;(first (cmn::get-cmn-marks 'end-slur))))
                       'end-sl))
        (t
         ;; get-cmn-marks returns a list as some single marks need two
         ;; cmn-marks (like accent-staccato) 
         (loop 
             for i from start-note to end-note 
             for event = (get-nth-non-rest-rhythm i rs)
                         ;; got to make the marks new each time...
                         #|
             for cmn-marks = (cmn::get-cmn-marks mark)
             do (loop for m in cmn-marks do
             (add-cmn-mark event m))))))
             |#
             do
               (add-cmn-mark event mark)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod scale ((rs rthm-seq) scaler
                  &optional ignore1 ignore2 ignore3)
  (declare (ignore ignore1) (ignore ignore2) (ignore ignore3))
  (setf (bars rs) (loop for b in (bars rs) collect (scale b scaler)))
  rs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; e.g. (get-multipliers '(e. s q e e) 's) -> (3 1 4 2 2)
(defmethod get-multipliers ((rs rthm-seq) rthm &optional round ignore)
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

(defmethod get-rhythms ((rs rthm-seq))
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
               (setf temp (fill-with-rhythms bar (subseq rthms ate)
                                             :warn nil :is-full-error nil))
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

;;;  27.1.11.  see rthm-seq-bar class method for caveats.
(defmethod split ((rs rthm-seq) 
                  &key (min-beats 2) (max-beats 5) warn (clone t))
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

(defun make-rthm-seq (rs &key (psp-inversions nil))
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
                    (fourth score-strings)))))) ; ties

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Given a rhythmic unit, e.g. 32nd, and a time sig, return a rthm-seq made up
;;; of bars whose rhythms are multiples of the numbers in the multipliers list
;;; e.g. given a unit of 32nd and multipliers and 4/4 '(7 9 16) we should get a
;;; bar with (e.. 32+q h)
;;; At this point the unit should be a whole number divisor of the beat in the
;;; time-sig, i.e. quintuple eights won't work in 4/4
;;; todo: should be able to verify-and-store when adding (add) data to an
;;;       assoc-list--fails and causes an rsp to have num-data 0 
;;; 
;;; thing to bear in mind with-auto-beam is that auto-beam will call
;;; get-beats and if we've created durations longer than 1

(defun make-rthm-seq-from-unit-multipliers (unit multipliers time-sig 
                                            &key
                                            ;; a number for brackets over
                                            ;; each beat.
                                            (tuplet nil)
                                            (tag nil)
                                            (auto-beam t) ; see above
                                            (id "from-multipliers"))
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

;;; Jan 2010
;;; <fragments> is a list of rhythms with ids
;;; <references> is a list of ids into fragments: these will be collated to
;;; create the rthm-seq.  Each element is a sublist: this will make up a whole
;;; bar according to the <meters> scheme.
;;; NB No pitch-seqs can be passed as yet.
;;; <meters> is a list of the meters (either single numerators: default-beat
;;; will then be the denominator) or num/denum lists
(defun make-rthm-seq-from-fragments (id fragments references meters
                                     &optional (default-beat 4))
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

;;; 11.2.10: sometimes we just want to initialize a bunch of rhythms but take
;;; advantage of rthm-seq's ability to add tuplet/beaming info.  Do this here
;;; with bar being a list of rhythms and time-sig a list (e.g. (2 4)) that
;;; those rhythms add up to
(defun make-rhythms (bar time-sig &optional split-into-beats)
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
