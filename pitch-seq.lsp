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
;;; Version:          1.0.10
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
;;; $$ Last modified:  14:39:26 Sat Jul 11 2020 CEST
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

;;; The general note curve (e.g. '(1 2 1 1 4) is stored in the data slot.
(defclass pitch-seq (sclist)
  ((highest :accessor highest :type integer :initform -999)
   (lowest :accessor lowest :type integer :initform -999)
   ;; 5/3/07 we're going to extend the data so that numbers in parentheses
   ;; indicate that a chord should be played.  When the instance is initialized
   ;; though, make a copy of the data list here, then remove parentheses from
   ;; items in the data.
   ;; MDE Fri Jul 24 20:40:56 2015 -- despite being able to have 1 or several
   ;; pitch-seqs in a palette, hence different levels of nesting, it is
   ;; possible to have a single note pitch-seq consisting of a chord, e.g.
   ;; :rthm-seq-palette '((1 ((((4 4) w)) :pitch-seq-palette ((((1)))))))
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
                           :allocation :class :initform -1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Mon Oct 28 17:52:31 2013 
(defmethod reset ((ps pitch-seq) &optional ignore1 ignore2)
  (declare (ignore ignore1 ignore2))
  ;; (print 'ps-reset)
  (setf (notes ps) nil))

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
  (unless (> (relative-notes-length ps) 0)
    (setf (relative-notes ps)
      (loop for n in (relative-notes ps) collect (make-pitch n))
      (relative-notes-length ps) (length (relative-notes ps))))
  (setf (original-data ps) (copy-list (data ps))
        (data ps) (loop for i in (slot-value ps 'data) collect 
                        (if (listp i)
                            (first i)
                          i))
        (highest ps)
        (loop for i in (data ps) unless (numberp i) do
              (error "pitch-seq::initialise-instance: ~
                      Elements of the pitch-seq data list should ~%all be ~
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
;;; DESCRIPTION
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
(defmethod invert ((ps pitch-seq) &optional ignore1 ignore2)
;;; ****
  (declare (ignore ignore1 ignore2))
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
;;; SAR Sun Apr 29 16:19:03 BST 2012: Edited robodoc info
;;; MDE Sat Jul 14 18:29:51 2012: avoid-used-notes added

;;; ****m* pitch-seq/get-notes
;;; DESCRIPTION
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
;;; 1) Limit the set object to the instrument's range.
;;; 
;;; 2) Remove the notes that have already been selected for other instruments.
;;;    This is where the slippery-chicken slot :instrument-hierarchy plays an
;;;    important role.  This can be skipped if the <avoid-used-notes> argument
;;;    is nil.
;;; 
;;; 3) If there is a subset with the same ID as the ID slot for the
;;;    player, use only those pitches common to that subset and those in
;;;    step 2. If not, try again using the subset-id of the instrument.
;;; 
;;; 4) If the ratio between the number of pitches now available and the number
;;;    of different numbers in the pitch-seq is less than the slippery-chicken
;;;    slot pitch-seq-index-scaler-min*, add notes from those used by other
;;;    instruments until there are enough; the lowest number in the pitch-seq
;;;    will now select the lowest pitch in the set that is in the instrument's
;;;    range.
;;;
;;;    If however there are enough pitches without adding pitches already used
;;;    by other instruments, then where in the available pitches the lowest
;;;    number of the pitch-seq will be placed depends on whether the
;;;    prefers-notes slot of the instrument has been set to be high or low. If
;;;    high, then the highest number in the pitch-seq will result in the
;;;    highest pitch in the available pitches that is in the instrument's
;;;    range. If low, then the lowest number in the pitch-seq will result in
;;;    the lowest pitch in the available pitches that is in the instrument's
;;;    range. If the user hasn't set this slot, then the range of the pitch-seq
;;;    will correspond to the middle of the available pitches.
;;;
;;;    There are two caveats here if the instrument's prefers-notes slot is
;;;    NIL: 1) If the lowest number in the pitch-seq is 5 or higher, this will
;;;    have the same effect as the prefers-notes slot being high. Similarly, if
;;;    the lowest number is 1, it will have the same effect as the
;;;    prefers-notes slot being low. These two numbers (5 and 1) are actually
;;;    global slippery chicken configuration data: (get-sc-config
;;;    pitch-seq-lowest-equals-prefers-high) and (get-sc-config
;;;    pitch-seq-lowest-equals-prefers-low) so can be set using the
;;;    set-sc-config function.  
;;;
;;;    * The question as to how many pitches are enough pitches before adding
;;;    used notes is determined by the pitch-seq-index-scaler-min argument,
;;;    which is by default 0.5 (in the slippery-chicken slot that's usually
;;;    used and passed to this method). As the pitch-seq notes must be offset
;;;    and scaled before they can be used as indices, there's a minimum scaler
;;;    that's considered acceptable; anything below this would result in more
;;;    notes being added.
;;; 
;;; 5) If at this point there are no available pitches, the function will by
;;;    default trigger an error and exit (see however pitch-seq-no-pitches-error
;;;    in globals.lsp). This could happen if the value of set-limits, both high
;;;    and low, took the available pitches outside of the instrument's range,
;;;    for instance.
;;; 
;;; 6) The pitch-seq numbers are now offset and scaled, then rounded in order
;;;    to use them as indices into the pitch list. If a number is in
;;;    parentheses then this is where the instrument's chord function would be
;;;    called. As notes are selected, the set marks them as used for the next
;;;    time around. Also, there's an attempt to avoid melodic octaves on
;;;    adjacent notes; however, if the set is full of octaves this won't be
;;;    possible; in that case a warning will be issued and the octave will be
;;;    used.
;;;
;;; ARGUMENTS 
;;; - A pitch-seq object.
;;; - An instrument object.
;;; - A player object.
;;; - An sc-set object.
;;; - A hint pitch (ignored for now).
;;; - A pitch-object defining the highest possible note.
;;; - A pitch-object defining the lowest possible note.
;;; - The sequence number (for diagnostics).
;;; - The last note of the previous sequence, as a pitch object.
;;; - The lowest scaler that will be accepted before adding notes from those
;;;   used; i.e., if the pitch-seq needs 6 notes and only 3 are available,
;;;   there would be note repetition, but as this would create a scaler of 0.5,
;;;   that would be acceptable
;;; - Whether to avoid lines jumping an octave in either direction (passed by
;;;   the slippery chicken slot). 
;;; - Whether to remove notes already chosen for other instruments before
;;;   selecting notes for this one.
;;; 
;;; RETURN VALUE  
;;; Returns the list of pitch objects that forms the notes slot of the given
;;; pitch-seq 
;;;
;;; SYNOPSIS
(defmethod get-notes ((ps pitch-seq) instrument player set hint-pitch limit-high
                      limit-low seq-num last-note-previous-seq
                      pitch-seq-index-scaler-min avoid-melodic-octaves
                      avoid-used-notes)
;;; ****
  (declare (ignore hint-pitch))
  ;; (print avoid-used-notes)
  ;; (print (id instrument))
  ;; (when set (print '-------------) (print (pitch-symbols (clone set))))
  ;; (print (get-chromatic set))
  ;; (print (data limit-low)) (print (data limit-high))
  ;; (print instrument) (print set)
  (when (data ps) ;; don't do anything for empty seqs!
    (if (or (not instrument) (not set))
        (setf (notes ps) 
              (get-relative-notes ps (if instrument
                                         (starting-clef instrument)
                                         'treble)))
        (let* ((highest (highest ps))
               (lowest (lowest ps))
               (do-chords (chords instrument))
               (need (1+ (- highest lowest)))
               ;; MDE Mon Jun 22 13:25:56 2015 -- if sets are 'empty' be
               ;; careful that they're not all microtones and the instrument
               ;; cannot play them.  
               (set-pitches-rm (limit-for-instrument (clone set) instrument
                                                     :upper limit-high
                                                     :lower limit-low
                                                     :do-related-sets t))
               ;; get the notes we've already assigned to other instruments ...
               ;; MDE Tue Apr 10 07:57:45 2012 -- remember that the set stores
               ;; all notes used by each instrument for the whole piece using
               ;; the 'global' sequence number (i.e. the used-notes slot is a
               ;; RAL with the top-most IDs being the seq-num, the instrument
               ;; names and their used notes being the next level down), so
               ;; there can be no question of the notes used in a previous
               ;; sequence influencing the choice of notes here.
               ;; MDE Sat Jul 14 18:43:20 2012 -- only when we want to!
               (used (when avoid-used-notes
                       (loop for p in (get-used-notes set seq-num) 
                          when (pitch-member p set-pitches-rm)
                          collect p)))
               ;; ... and remove these from the notes we'll select from (in
               ;; order to try and use as many notes from the set as possible
               ;; and avoid repeating notes across instruments)
               (set-pitches-rm-used (remove-pitches set-pitches-rm used
                                                    :enharmonics-are-equal t
                                                    :return-symbols nil))
               ;; MDE Fri Jul 27 17:33:12 2018 -- updated so that no error is
               ;; issued when there's a subset-id for the ins but not for the
               ;; set. 
               (subset (let* ((sid (subset-id instrument))
                              ;; MDE Wed Aug 29 14:13:14 2018 -- make sure we've
                              ;; got a player object too before doing this 
                              (ps (when (and (player-p player) (subsets set))
                                    (get-data (id player) (subsets set) nil)))
                              (is (when (and (not ps) sid (subsets set))
                                    (get-data sid (subsets set) nil)))
                              ;; MDE Fri Jul 27 19:12:29 2018 -- if there's a
                              ;; subset with the ID of the player, use that,
                              ;; otherwise try for the subset-id of the ins
                              (s (if ps ps is)))
                         (when s (data s))))
               num-set-pitches offset scaler)
          ;; (print (pitch-list-to-symbols set-pitches-rm))
          ;; (print (pitch-symbols (clone set)))
          ;; (print set-pitches-rm)
          ;; If (subset-id instrument) was set (to limit the pitches for that 
          ;; instrument to the subset with this id) then use only pitches in
          ;; subset; also set used to be only those that are in the subset.
          (when subset
            (setf set-pitches-rm-used (pitch-intersection set-pitches-rm-used
                                                          subset)
                  used (pitch-intersection used subset)))
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
                                (get-sc-config 
                                 'pitch-seq-lowest-equals-prefers-high)))
                        ;; (print 'here)
                        (- num-set-pitches need 
                           lowest))
                       ;; if the lowest given is 1 always use the
                       ;; bottom notes
                       ((or (prefers-low instrument)
                            (= lowest
                               (get-sc-config
                                'pitch-seq-lowest-equals-prefers-low)))
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
          #|(format t "~%seq-num: ~a, num-set-pitches: ~a, need: ~a, ~
                     offset: ~a, scaler: ~a"
          seq-num num-set-pitches need offset scaler)|#
          (unless (> num-set-pitches 0)
            ;; MDE Wed Oct 10 16:48:06 2018 -- allow warning instead of error
            (apply
             (cond ((get-sc-config 'pitch-seq-no-pitches-error) #'error)
                   ((get-sc-config 'pitch-seq-no-pitches-warning) #'warn)
                   ;; don't even warn...
                   (t #'(lambda (&rest args))))
             (list
              "~&pitch-seq::get-notes: For ~a at sequence number ~a: ~
               no pitches in set!  ~%Perhaps your ~
               set-limits (high: ~a, low: ~a) are too restrictive or ~
               your ~%set is microtonal and your instrument isn't??~
               ~%set = ~a ~%set for ins: ~a~%set minus used: ~a ~%~
               curve = ~a~%~a~%~a"
             (id instrument) seq-num (when limit-high (id limit-high)) 
             (when limit-low (id limit-low)) (pitch-symbols set)
             (get-ids-from-pitch-list set-pitches-rm)
             (get-ids-from-pitch-list set-pitches-rm-used)
             (data ps) set instrument)))
          ;; (print-simple-pitch-list set-pitches-rm-used)
          (when (> num-set-pitches 0)
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
                     ;;(print used-notes)
                       (unless note
                         (error "~a~&pitch-seq::get-notes: no note! ~
                               ~%index = ~a, lowest = ~a, highest = ~a, ~
                               num-set-pitches = ~a, offset = ~a, i = ~a, ~
                               scaler = ~a set = ~a"
                                set-pitches-rm-used index lowest highest 
                                num-set-pitches offset i scaler
                                (pitch-symbols set)))
                       (if (and (listp j) do-chords) ;; should be a chord!
                           (progn
                             ;; (print 'chord!)
                             ;; the chord-function defined should take six
                             ;; arguments: the current number from the curve;
                             ;; the index that this was translated into by the
                             ;; offset and scaler (based on trying to get a best
                             ;; fit for the instrument and set); the pitch-list
                             ;; that we created from the set, taking the
                             ;; instrument's range and other notes already
                             ;; played by other instruments; the pitch-seq
                             ;; object; the instrument object; the set object.
                             ;; It must return a chord object.
                             (setf note (funcall chord-fun i index
                                               set-pitches-rm-used ps
                                               instrument set))
                           ;; (print note)
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
                     (when (and avoid-melodic-octaves
                                last
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
                   collect note)))
          (when (get-sc-config 'verbose-pitch-selection)
            ;;(print (notes ps))
            (format t "~&**** For ~a at seq-num ~a, with pitch-seq ~a ~
                       ~%and the set ~a, "
                    (id instrument) seq-num (original-data ps) (id set))
            (format t "the following pitches were available for the ~
                       instrument:~%")
            (pitch-list-to-symbols set-pitches-rm-used)
            (print-simple set)
            (format t "~&and the following pitches were chosen:~%")
            (print-simple-pitch-list (notes ps)))))
    (notes ps)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-relative-notes ((ps pitch-seq) clef)
  ;; (print 'get-relative-notes)
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
;;; No longer necessary, taken care of in get-relative-notes. ; ; ; ; ;
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
;;; MDE Fri Feb  2 15:42:53 2018
(defmethod flat-line ((ps pitch-seq))
  (= (highest ps) (lowest ps)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Tue Jan  3 17:56:59 EST 2012: Added robodoc info
;;; MDE Wed Mar 27 19:47:38 2013 -- see also get-next-for-ins method for more
;;; details on named instruments in pitch-seqs.

;;; ****f* pitch-seq/make-pitch-seq
;;; DESCRIPTION
;;; Create a pitch-seq object.
;;;
;;; This function can be either called with one argument, consisting of a
;;; two-item list, in which the first item is the pitch-seq ID and the second
;;; is a list of numbers representing the pitch curve of the intended pitch
;;; sequence; or it can be created with two arguments, the first of which being
;;; the list of numbers representing the pitch curve and the second being the
;;; pitch-seq's ID.
;;;
;;; NB We can assign a pitch-seq exclusively to particular instruments in the
;;; ensemble simply by passing their names as symbols along with the curve
;;; data.  See below for an example.
;;; 
;;; ARGUMENTS
;;; - A two-item list, of which the first item is a symbol to be used as the
;;;   object's ID, and the second is a list of integers representing the general
;;;   contour of the pitch sequence.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - If the optional argument format is used, the first argument is to be 
;;;   a list of numbers representing the general contour of the pitch sequence, 
;;;   and the second is to be a symbol for the pitch-seq object's ID. 
;;; 
;;; RETURN VALUE
;;; - A pitch-seq object.
;;; 
;;; EXAMPLE

#|

;; The first creation option is using one argument that is a two-item list,
;; whereby the first item is a symbol to be used as the pitch-seq object's ID 
;; and the second is a list of numbers representing the general contour of the 
;; pitch sequence.
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

;; The second creation option uses two arguments, the first of which is a list
;; of numbers representing the general contour of the pitch sequence, the 
;; second of which is a symbol which will be used as the pith-seq object's ID. 
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

;; An example assigning a pitch-seq only to specific instruments: ; ;
(make-pitch-seq '((1 2 1 1 3) violin flute) 'ps1))

                |#
;;; SYNOPSIS
(defun make-pitch-seq (id-data &optional (id nil))
;;; ****
  (let* ((id-data-given (and (listp id-data) 
                             (= 2 (length id-data))
                             ;; MDE Thu Apr  5 20:14:36 2012 -- not if there's
                             ;; an ID i.e. the call comes from verify-and-store
                             ;; :after ((psp pitch-seq-palette)) 
                             (not id)
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

(defun pitch-seq-p (thing)
  (typep thing 'pitch-seq))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EOF pitch-seq.lsp
