;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* sclist/pitch-seq
;;; NAME 
;;; pitch-seq
;;;
;;; File:             pitch-seq.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> sclist -> 
;;;                   pitch-seq
;;;
;;; Version:          0.9.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Implementation of the pitch-seq class.  This describes
;;;                   the pitch curves for a given rhythmic sequence.  These
;;;                   are normally simple lists of notes indicating pitch
;;;                   height (and later mapped onto sets); chords are indicated
;;;                   by placing a number in parentheses.
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    19th February 2001
;;;
;;; $$ Last modified: 09:44:25 Sat Mar 31 2012 BST
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

;;; 02.12.11 SEAN: Changed robodoc header to reflect class hierarchy

(eval-when (compile)
  (declaim (optimize (speed 3) (safety 1) (space 0) (debug 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :slippery-chicken)

;;; MDE Sun Mar 25 10:39:07 2012 -- These two constants are used in
;;; pitch-seq::get-notes to indicate which lowest number in a pitch-seq would
;;; indicate that we should select the highest or lowest notes possible for the
;;; instrument/set. 
(defparameter +pitch-seq-lowest-equals-prefers-high+ 5)
(defparameter +pitch-seq-lowest-equals-prefers-low+ 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The general note curve (e.g. '(1 2 1 1 4) is stored in the data slot.
(defclass pitch-seq (sclist)
  ((highest :accessor highest :type integer :initform -999)
   (lowest :accessor lowest :type integer :initform -999)
   ;; 5/3/07 we're going to extend the data so that numbers in parentheses
   ;; indicate that a chord should be played.  When the instance is initialized
   ;; though, make a copy of the data list here, then remove parentheses from
   ;; items in the data
   (original-data :accessor original-data :type list :initform nil)
   ;; 24/3/07: whether the user specified an id for the pitch seq or whether it
   ;; was auto-generated from the palette name 
   (user-id :accessor user-id :type boolean :initarg :user-id :initform nil)
   ;; 24/3/07: a list of the instruments that can use this ps.
   (instruments :accessor instruments :type list :initarg :instruments
                :initform nil)
   (notes :accessor notes :type list :initform nil)
   (relative-notes :accessor relative-notes :type list :allocation :class
                   :initform '(d3 e3 f3 g3 a3 b3 c4 d4 e4 f4 g4 a4 b4 c5 d5 e5
                               f5 g5 a5 b5 c6 d6 e6 f6 g6))
   (relative-notes-length :accessor relative-notes-length :type integer 
                           :allocation :class :initform nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object :before ((ps pitch-seq) stream)
  (format stream "~%PITCH-SEQ: notes: ~a~
                  ~%           highest: ~a~
                  ~%           lowest: ~a~
                  ~%           original-data: ~a~
                  ~%           user-id: ~a~
                  ~%           instruments: ~a~
                  ~%           relative-notes: (not printed for sake ~
                               of brevity)~
                  ~%           relative-notes-length: ~a"
          (notes ps) (highest ps) (lowest ps) (original-data ps) (user-id ps) 
          (instruments ps) (relative-notes-length ps)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((ps pitch-seq) &rest initargs)
  (declare (ignore initargs))
  ;; only do this once.
  (unless (relative-notes-length ps)
    (setf (relative-notes ps)
      (loop for n in (relative-notes ps) collect (make-pitch n))
      (relative-notes-length ps) (length (relative-notes ps))))
  (setf (original-data ps) (copy-list (data ps))
        (data ps) (loop for i in (slot-value ps 'data) collect 
                        (if (listp i)
                            (first i)
                          i))
        (highest ps)
        (loop for i in (data ps) unless (integerp i) do
              (error "pitch-seq::initialise-instance: ~
                      Elements of the pitch-seq data list should all be ~
                      integers: ~a"
                     (data ps))
            maximize i)
        (lowest ps) (loop for p in (data ps) minimize p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone ((ps pitch-seq))
  (clone-with-new-class ps 'pitch-seq))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone-with-new-class :around ((ps pitch-seq) new-class)
  (declare (ignore new-class))
  (let ((sclist (call-next-method)))
    (setf (slot-value sclist 'highest) (highest ps)
          (slot-value sclist 'lowest) (lowest ps)
          (slot-value sclist 'user-id) (user-id ps)
          (slot-value sclist 'instruments) (instruments ps)
          (slot-value sclist 'original-data) (original-data ps)
          (slot-value sclist 'notes) (basic-copy-object (notes ps)))
    sclist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Tue Jan  3 19:27:45 EST 2012: Added robodoc info

;;; ****m* pitch-seq/invert
;;; FUNCTION
;;; Invert the pitch sequence contour attached to a given pitch-seq object. The
;;; inversion uses only the same numbers from the original pitch contour list. 
;;; 
;;; ARGUMENTS
;;; - A pitch-seq object.
;;; 
;;; RETURN VALUE
;;; A pitch-seq object.
;;; 
;;; EXAMPLE
#|
(let ((ps (make-pitch-seq '(pseq1 (1 2 1 3 4 7)))))
  (data (invert ps)))

=> (7 4 7 3 2 1)

|#
;;; SYNOPSIS
(defmethod invert ((ps pitch-seq))
;;; ****
  (let* ((sorted (sort (remove-duplicates (data ps))
                       #'<))
         (len-1 (1- (length sorted)))
         (inverted (loop 
                       for p in (data ps)
                       for op in (original-data ps) 
                       for inv = (nth (- len-1 (position p sorted)) sorted)
                       collect
                         (if (listp op)
                             (list inv)
                           inv)))
         (result (make-pitch-seq inverted
                    (when (id ps)
                      (let ((name (format nil "~a-inverted"
                                          (id ps))))
                        (if (stringp (id ps))
                            name
                          (read-from-string name)))))))
    (setf (instruments result) (copy-list (instruments ps)))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Tue Jan  3 19:35:29 EST 2012: Edited robodoc info

;;; ****m* pitch-seq/get-notes
;;; FUNCTION
;;; This gets notes from the sets, limiting the notes used to the range of the
;;; instrument and any other ranges defined in the slippery-chicken class. If
;;; either the instrument or set are missing it just gets the relative pitches
;;; we use to display a pitch sequence.
;;;
;;; limit-high and limit-low are pitch objects. They are extra range definers
;;; that are given to the slippery-chicken object to control the pitch curve of
;;; an instrument over the duration of the whole piece. They always refer to
;;; sounding pitches.  
;;; 
;;; The order of operations for selecting pitches are as follows:
;;;
;;; 1) Limit the set object to the instrument's range
;;; 
;;; 2) Remove the notes that have already been selected for other instruments.
;;;    This is where the slippery-chicken slot :instrument-hierarchy plays an
;;;    important role.  
;;; 
;;; 3) If there is an subset with the same ID as the subset-id slot for this
;;;    instrument, use only those pitches common to that subset and those in
;;;    step 2.
;;; 
;;; 4) If the ratio between the number of pitches now available and the number
;;;    of different numbers in the pitch-seq is less than the slippery-chicken
;;;    slot pitch-seq-index-scaler-min, we will add notes from those used by
;;;    other instruments until we have enough, and the lowest number in the
;;;    pitch-seq will select the lowest pitch in the set that is in the
;;;    instrument's range.  If however we do have enough pitches without adding
;;;    pitches already used by other instruments, then where in the available
;;;    pitches we place our lowest number of the pitch-seq will depend on
;;;    whether you've set the prefers-notes slot of the instrument to be high
;;;    or low.  If high, then the highest number in the pitch-seq will result
;;;    in the highest pitch in the available pitches that is in the
;;;    instrument's range.  If low, then the lowest number in the pitch-seq
;;;    will result in the lowest pitch in the available pitches that is in the
;;;    instrument's range.  If you haven't set this slot, then the range of the
;;;    pitch-seq will correspond to the middle of the available pitches.  There
;;;    are two caveats here if the instrument's prefers-notes slot is NIL: 1)
;;;    if the lowest number in the pitch-seq is 5 or higher, this will have the
;;;    same effect as the prefers-notes slot being high.  Similarly, if the
;;;    lowest number is 1, it will have the same effect as the prefers-notes
;;;    slot being low.  These two numbers (5 and 1) are actually global
;;;    constants: +pitch-seq-lowest-equals-prefers-high+ and
;;;    +pitch-seq-lowest-equals-prefers-low+, as defined above.
;;;
;;;    The question as to how many pitches are enough pitches before adding
;;;    used notes is determined by the pitch-seq-index-scaler-min argument,
;;;    which is by default 0.5 (in the slippery-chicken slot that's usually
;;;    used and passed to this method).  As the pitch-seq notes must be offset
;;;    and scaled before they can be used as indices, there's a minimum scaler
;;;    that's considered acceptable; anything below this would result in more
;;;    notes being added.
;;; 
;;; 5) If at this point, there are no available pitches, the function will
;;;    trigger an error and exit.  This could happen if your set-limits, both
;;;    high and low, took the available pitches outside of the instrument's
;;;    range, for instance.
;;; 
;;; 6) We're now ready to offset and scale then round our pitch-seq numbers in
;;;    order to use them as indices into our pitch list.  If a number is in
;;;    parentheses then this is where the instrument's chord function would be
;;;    called.  As notes are selected, the set marks them as used for the next
;;;    time around.  Also, there's an attempt to avoid melodic octaves on
;;;    adjacent notes, however, if the set is full of octaves this won't be
;;;    possible; in that case a warning will be issued but the octave will be
;;;    used.
;;;
;;;    
;;;
;;; ARGUMENTS 
;;; - A pitch-seq object.
;;; - An instrument object.
;;; - An sc-set object.
;;; - A hint pitch (ignored for now).
;;; - A pitch-object defining the highest possible note.
;;; - A pitch-object defining the lowest possible note.
;;; - The sequence number (for diagnostics).
;;; - The last note of the previous sequence, as a pitch object.
;;; - the lowest scaler we'll accept before adding notes from those used
;;;   i.e. if our pitch-seq needs 6 notes and only 3 are available, there would
;;;   be note repetition but as this would create a scaler of 0.5, that would
;;;   be acceptable
;;; 
;;; RETURN VALUE  
;;; Returns a list of pitch objects.
;;;
;;; EXAMPLE
#|

|#
;;; SYNOPSIS
(defmethod get-notes ((ps pitch-seq) instrument set hint-pitch limit-high
                      limit-low seq-num last-note-previous-seq
                      pitch-seq-index-scaler-min)
;;; ****
  (declare (ignore hint-pitch))
  ;; (print ps)
  ;; (print instrument)
  ;; (print set)
  ;;   (print hint-pitch)
  ;; (print pitch-seq-index-scaler-min)
  (when (data ps) ;; don't do anything for empty seqs!
    (if (or (not instrument) (not set))
        (setf (notes ps) 
              (get-relative-notes ps 
                                  (if instrument
                                      (starting-clef instrument)
                                      'treble)))
        (let* ((highest (highest ps))
               (lowest (lowest ps))
               (do-chords (chords instrument))
               (need (1+ (- highest lowest)))
               (set-pitches-rm (limit-for-instrument (clone set) instrument
                                                     :upper limit-high 
                                                     :lower limit-low
                                                     :do-related-sets t))
               ;; get the notes we've already assigned to other instruments ...
               (used (loop for p in (get-used-notes set seq-num) 
                        when (pitch-member p set-pitches-rm)
                        collect p))
               ;; ... and remove these from the notes we'll select from (in
               ;; order to try and use as many notes from the set as possible
               ;; and avoid repeating notes across instruments)
               (set-pitches-rm-used (remove-pitches set-pitches-rm used
                                                    :enharmonics-are-equal t
                                                    :return-symbols nil))
               (ins-subset (when (subset-id instrument)
                             (get-data-data (subset-id instrument) 
                                            (subsets set))))
               num-set-pitches offset scaler)
          ;; If (subset-id instrument) was set (to limit the pitches for that
          ;; instrument to the subset with this id) then use only pitches in
          ;; subset; also set used to be only those that are in the subset.
          (when ins-subset
            (setf set-pitches-rm-used (pitch-intersection set-pitches-rm-used
                                                          ins-subset)
                  used (pitch-intersection used ins-subset)))
          ;; try to use our pitch curve with only notes not already used but if
          ;; that would result in too few pitches then add notes from used one
          ;; by one until we're happy.  NB By not re-using already-used pitches
          ;; we are effectively deviating from our pitch curve, e.g. p84 of
          ;; cheat sheet where quick runs that should be close become huge fast
          ;; leaps
          (loop with used-cp = (copy-list used) do
               (setf num-set-pitches (length set-pitches-rm-used)
                     offset 
                     (cond 
                       ((>= need num-set-pitches) (- lowest))
                       ;; if the lowest given is >= 5 then always
                       ;; use the top notes
                       ((or (prefers-high instrument)
                            (>= lowest 
                                +pitch-seq-lowest-equals-prefers-high+))
                        (- num-set-pitches need 
                           lowest))
                       ;; if the lowest given is 1 always use the
                       ;; bottom notes
                       ((or (prefers-low instrument)
                            (= lowest
                               +pitch-seq-lowest-equals-prefers-low+))
                        (- lowest))
                       ;; go for the middle
                       (t (- (1- (ceiling (- num-set-pitches need) 2))
                             lowest)))
                     scaler 
                     (if (> need num-set-pitches)
                         (/ num-set-pitches need)
                         1))
             ;; add pitches from those used already to try and get more
             ;; available to fit our pitch curve
               (if (or (not used-cp)
                       (and (> num-set-pitches 1)
                            ;; MDE Tue Mar 27 10:58:36 2012 --
                            ;; pitch-seq-index-scaler-min now comes from the sc
                            ;; slot 
                            (>= scaler pitch-seq-index-scaler-min)))
                   (return)
                   (setf set-pitches-rm-used
                         (init-pitch-list (cons (pop used-cp)
                                                set-pitches-rm-used)
                                          t))))
          ;; (break)
          (unless (> num-set-pitches 0)
            (error "~&pitch-seq::get-notes: For ~a at sequence number ~a: ~
                     no pitches in set!  ~%Perhaps your ~
                     set-limits (high: ~a, low: ~a) are too restrictive?~
                     ~%set = ~a, set minus used: ~a curve = ~a~%~a" 
                   (id instrument) seq-num (when limit-high (id limit-high))
                   (when limit-low (id limit-low)) (pitch-symbols set)
                   (get-ids-from-pitch-list set-pitches-rm-used)
                   (data ps) set))
          (setf (notes ps)
                (loop 
                   ;; remember: the pitch curve is stored in the data slot but
                   ;; this has had parentheses removed from all elements; ()
                   ;; indicate that a chord should happen and these are still
                   ;; intact in the original-data slot.
                   for i in (data ps)
                   for j in (original-data ps)
                   ;; 31/3/10: try rounding instead of floor...
                   for index = (round (* (+ offset i) scaler))
                   for note = (nth (if (= index num-set-pitches)
                                       (1- index)
                                       index)
                                   set-pitches-rm-used)
                   with chord-fun = (when (chords instrument)
                                      (symbol-function
                                       (chord-function instrument)))
                   with used-notes = (used-notes set)
                   with uns-ref = (list seq-num (id instrument))
                   with last = last-note-previous-seq
                   do
                   (unless note
                     (error "~a~&pitch-seq::get-notes: failed to get a note! ~
                             ~%index = ~a, lowest = ~a, highest = ~a, ~
                             num-set-pitches = ~a, offset = ~a, i = ~a, ~
                             scaler = ~a set = ~a"
                            set-pitches-rm-used index lowest highest 
                            num-set-pitches offset i scaler
                            (pitch-symbols set)))
                   (if (and (listp j) do-chords) ;; should be a chord!
                       (progn
                         ;; the chord-function defined should take six
                         ;; arguments: the current number from the curve; the
                         ;; index that this was translated into by the offset
                         ;; and scaler (based on trying to get a best fit for
                         ;; the instrument and set); the pitch-list that we
                         ;; created from the set, taking the instrument's range
                         ;; and other notes already played by other
                         ;; instruments; the pitch-seq object; the instrument
                         ;; object; the set object.  It must return a chord
                         ;; object.
                         (setf note (funcall chord-fun i index 
                                             set-pitches-rm-used ps instrument
                                             set))
                         ;; store the pitches we've used
                         (loop for pitch in 
                              (if (chord-p note)
                                  (data note) 
                                  (list note))
                              do
                              (ral-econs (data pitch) uns-ref used-notes)))
                       ;; it's a single pitch so just update used-notes
                       (ral-econs (data note) uns-ref used-notes))
                   ;; 16/4/07: avoid melodic 8ves where reasonable,
                   ;; i.e. doesn't recheck to see if we've recreated another
                   ;; octave if the available pitches are full of octaves
                   (when (and last
                              (pitch-p last)
                              (pitch-p note)
                              (is-octave note last))
                     (cond 
                       ((> index 0)
                        (setf note (nth (1- index) set-pitches-rm-used)))
                       ((< (1+ index) num-set-pitches)
                        (setf note (nth (1+ index) set-pitches-rm-used)))
                       (t (warn "pitch-seq::get-notes: can't avoid octave; ~
                                 seq-num: ~a, instrument: ~a, pitches: ~a ~
                                 used-pitches: ~a, last: ~a"
                                seq-num (id instrument) 
                                (pitch-list-to-symbols 
                                 set-pitches-rm-used)
                                (pitch-list-to-symbols used)
                                (id last)))))
                     #|
                   (format t "~&set id: ~a, note: ~a, ref: ~a :stored ~a" 
                     (id set) (data note) (list seq-num (id instrument))
                     (get-data (list seq-num (id instrument))
                     (used-notes set)))
                   |#
                (setf last note)
              collect note))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-relative-notes ((ps pitch-seq) clef)
  (let* ((high (loop for rel in (data ps) maximize rel))
         (low (loop for rel in (data ps) minimize rel))
         (range (- high low))
         (rels (relative-notes ps))
         (len-rels (length rels))
         (diff (- len-rels range))
         (rels-lowest (floor diff 2))
         (offset (- rels-lowest low))
         (new-data (loop for rel in (data ps) collect (+ rel offset)))
         (result '()))
    ;; (print clef)
    ;; (print new-data)
    (when (> range len-rels)
      (error "~a~%~%pitch-seq::get-relative-notes: your range is too high ~
              ~% high = ~a low = ~a but there are only ~a relative notes"
             ps high low len-rels))
    (setf result (loop for rel in new-data collect
                       (nth rel rels)))
    (case clef
      (treble result)
      (alto (transpose-pitch-list-force-white-notes result -10))
      (tenor (transpose-pitch-list-force-white-notes result -13))
      (bass (transpose-pitch-list-force-white-notes result -20))
      ;; cmn percussion clef is like treble in terms of note placement
      (percussion result)
      ;; (error "pitch-seq::get-relative-notes: Don't know what ~
      ;; to do with the percussion clef yet"))
      (t (error "pitch-seq::get-relative-notes: Unrecognised clef: ~a"
                clef)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
;;; No longer necessary, taken care of in get-relative-notes. ; ; ;
                   (defmethod relative-int-to-note (int (ps pitch-seq))
(let ((len (relative-notes-length ps)))
(when (or (> int len) (> (highest ps) len))
(error "pitch-seq::relative-int-to-note: ~
              Can only handle relative notes up to ~a at the moment." 
len))
(let* ((ground-zero (floor (- len (highest ps)) 2))
(nth (+ ground-zero (1- int))))
(unless (>= nth 0)
(error "~a~%~%pitch-seq::relative-int-to-note: nth = ~a!" ps nth))
(nth nth (relative-notes ps)))))
                   |#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod ps-subseq ((ps pitch-seq) start end)
  (when (or (< start 0)
            (> end (sclist-length ps)))
    (error "pitch-seq::ps-subseq: ~a pitches but start=~a and end=~a!"
           (sclist-length ps) start end))
  (let ((result (make-pitch-seq (list (id ps) 
                                      (subseq (original-data ps) start end)))))
    (setf (instruments result) (copy-list (instruments ps))
          (id result) (format nil "~a-post-ps-subseq-~a-~a" (id ps) start end))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Tue Jan  3 17:56:59 EST 2012: Added robodoc info

;;; ****f* pitch-seq/make-pitch-seq
;;; FUNCTION
;;; Create a pitch-seq object.
;;;
;;; This function can be either called with one argument, consisting of a
;;; two-item list, in which the first item is the pitch-seq ID and the second
;;; is a list of numbers representing the pitch curve of the intended pitch
;;; sequence; or it can be created with two arguments, the first of which being
;;; the list of numbers representing the pitch curve and the second being the
;;; pitch-seq's ID.
;;; 
;;; ARGUMENTS
;;; - A two-item list, of which the first item is a symbol to be used as the
;;;   object's ID, and the second is a list of integers representing the general 
;;;   contour of the pitch sequence.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - If the optional argument format is used, the first argument is to be only
;;;   a list of numbers representing the general contour of the pitch sequence, 
;;;   and the second is to be a symbol for the pitch-seq object's ID. 
;;; 
;;; RETURN VALUE
;;; - A pitch-seq object.
;;; 
;;; EXAMPLE

#|

;; The first creation option is using one argument that is a two-item list, ; ; ;
;; whereby the first item is a symbol to be used as the pitch-seq object's ID ; ; ;
;; and the second is a list of numbers representing the general contour of the ; ; ;
;; pitch sequence.                      ; ; ;
                   (make-pitch-seq '(pseq1 (1 2 1 1 3)))

                   =>
                   PITCH-SEQ: notes: NIL
                   highest: 3
                   lowest: 1
                   original-data: (1 2 1 1 3)
                   user-id: T
                   instruments: NIL
                   relative-notes: (not printed for sake of brevity)
                   relative-notes-length: 25
                   SCLIST: sclist-length: 5, bounds-alert: T, copy: NIL
                   LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
                   NAMED-OBJECT: id: PSEQ1, tag: NIL, 
                   data: (1 2 1 1 3)

;; The second creation option uses two arguments, the first of which is a list ; ; ;
;; of numbers representing the general contour of the pitch sequence, the ; ; ;
;; second of which is a symbol which will be used as the pith-seq object's ID. ; ; ;
                   (make-pitch-seq '(2 1 1 3 1) 'pseq2)

                   => 
                   PITCH-SEQ: notes: NIL
                   highest: 3
                   lowest: 1
                   original-data: (2 1 1 3 1)
                   user-id: NIL
                   instruments: NIL
                   relative-notes: (not printed for sake of brevity)
                   relative-notes-length: 25
                   SCLIST: sclist-length: 5, bounds-alert: T, copy: NIL
                   LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
                   NAMED-OBJECT: id: PSEQ2, tag: NIL, 
                   data: (2 1 1 3 1)

                   |#
;;; SYNOPSIS
(defun make-pitch-seq (id-data &optional (id nil))
;;; ****
  (let* ((id-data-given (and (listp id-data) 
                             (= 2 (length id-data))
                             (listp (second id-data))))
         (id (cond (id-data-given (first id-data))
                   (id id)
                   (t 
                    (error "pitch-seq::make-pitch-seq: ~
                            Arguments to make-pitch-seq must be either a ~
                            2-element list containing ~%the id and the data, ~
                            or a data list with the id as a second argument: ~
                            ~%~a" id-data))))
         (data (if id-data-given 
                   (second id-data)
                 id-data))
         ;; 24/3/07: can now associate a ps with specific instruments e.g.
         ;; (psp4 ((2 4 3) pno vc cl))
         (instruments (when (and (listp (first data))
                                 (if (listp (second data))
                                     (not (numberp (first (second data))))
                                   (not (numberp (second data)))))
                        (prog1
                            (rest data)
                          (setf data (first data))))))
    (make-instance 'pitch-seq :id id :data data :copy nil 
                   ;; 24/3/07: keep track of whether the id was given by the
                   ;; user or not
                   :user-id id-data-given
                   :instruments instruments)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EOF pitch-seq.lsp
