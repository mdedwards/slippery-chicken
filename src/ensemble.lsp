;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* recursive-assoc-list/ensemble
;;; NAME 
;;; ensemble
;;;
;;; File:             ensemble.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> sclist -> 
;;;                   circular-sclist -> assoc-list -> recursive-assoc-list ->
;;;                   ensemble
;;;
;;; Version:          1.0.12
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Implementation of the ensemble class.
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    4th September 2001
;;;
;;; $$ Last modified:  12:59:04 Wed Dec 30 2020 CET
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

(proclaim '(special +slippery-chicken-standard-instrument-palette+))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; id is the ensemble name, data ist the list of player objects

(defclass ensemble (recursive-assoc-list)
  ;; simple list of instrument names / num instrument pairs that is then used
  ;; to set the score-write-bar-line slot of each instrument in the ensemble.
  ;; All the score-write-bar-line slots are first set to nil, then updated
  ;; according to this list. E.g. '(cello 3 tuba 5 bassoon 6)
  ;; ***** NB when using CMN use the staff-groupings slot of the
  ;; slippery-chicken class instead!.  
  ;; MDE Wed Apr 18 12:27:47 2012 -- now obsolete
  ((bar-line-writers :accessor bar-line-writers :type list 
                     :initarg :bar-line-writers :initform nil)
   ;; MDE Wed Jan 10 19:46:31 2018 -- do this otherwise if we have doubling
   ;; players with two instruments we get errors from auto-midi-channels 
   (recurse-simple-data :initform nil)
   ;; a simple list of the player IDs
   (players :accessor players :type list :initform nil)
   ;; an instrument-palette that contains the instrument objects to be
   ;; cloned. The instruments list contains a simple symbol (id for look-up
   ;; into the instrument-palette) instead of a new instrument
   ;; definition/instantiation.
   (instrument-palette :accessor instrument-palette
                       :initarg :instrument-palette :initform nil))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((e ensemble) &rest initargs)
  (declare (ignore initargs))
  (score-write-bar-line-all-nil e)
  (let ((ip (instrument-palette e)))
    (when (and ip (not (typep ip 'instrument-palette)))
      (error "ensemble::initialize-instance: ~
              the instrument-palette slot must be an ~
              instrument-palette instance: ~a"
             ip)))
  (let ((blws (bar-line-writers e))
        (player nil))
    (loop for ins in blws by #'cddr and num in (cdr blws) by #'cddr do
          (setf player (get-data ins e))
          (unless player
            (error "ensemble::initialize-instance: ~
                    bar-line-writers: reference to instrument ~
                    not in the ensemble: ~a" ins))
         (set-score-write-bar-line player num)))
  ;; MDE Thu Dec 28 18:06:58 2017 
  (auto-midi-channels e)
  (get-players e))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Thu Dec 28 17:56:32 2017 -- called automatically at init
(defmethod auto-midi-channels ((e ensemble) &optional force)
  ;; only do this when channels haven't been explicitly set
  ;; (print 'here)
  (when (or force
            (and (> (num-players e) 1)
                 (every #'(lambda (player)
                            (and (= 1 (midi-channel player))
                                 (= 1 (microtones-midi-channel player))))
                        (data e))))
    (loop
       with chromatic = (= 12 (degrees-per-octave))
       ;; no channel 10 (percussion)
       with chans = (make-cscl (remove 10
                                       (loop for i from 1 to 16 collect i)))
       for player in (data e)
       for chan = (get-next chans)
       do
         (setf (midi-channel player) chan
               ;; MDE Sat Aug 25 16:18:20 2018 -- detect microtonal scale and
               ;; handle accordingly 
               (microtones-midi-channel player)
               (if chromatic chan (setq chan (get-next chans))))
       finally (return chan))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Sat Apr 20 12:34:54 2013 
;;; ****m* ensemble/get-player
;;; DESCRIPTION
;;; Return a player object from an ensemble, if it exists.
;;; 
;;; ARGUMENTS
;;; - An ensemble object.
;;; - The ID of a player.
;;; RETURN VALUE
;;; The player object or NIL if there's no such player.
;;; 
;;; SYNOPSIS
(defmethod get-player ((e ensemble) player)
;;; ****
  (get-data player e nil)) ; no warning

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* ensemble/get-instrument
;;; DATE
;;; November 2nd 2018, Heidhausen
;;; 
;;; DESCRIPTION
;;; Get the instrument object for a player in the ensemble. If the player
;;; doubles, then the optional <ins> argument is required and should be the ID
;;; of the instrument as defined in the ensemble's instrument palette
;;; (e.g. 'flute)  
;;; 
;;; ARGUMENTS
;;; - the ensemble object
;;; - the ID of a player in the ensemble
;;; 
;;; OPTIONAL ARGUMENTS
;;; - the ID of an instrument if the given player doubles
;;; - T or NIL to issue a warning should the instrument not be found.
;;; 
;;; RETURN VALUE
;;; The instrument object.
;;; 
;;; SYNOPSIS
(defmethod get-instrument ((e ensemble) player &optional ins (warn t))
;;; ****
  (let ((plyr (get-player e player)))
    (unless plyr
      (error "ensemble::get-instrument: no player ~a in ensemble: ~a" player e))
    (player-get-instrument plyr ins warn)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* ensemble/get-players
;;; DESCRIPTION
;;; Return the IDs of the players from a given ensemble object.
;;; 
;;; ARGUMENTS
;;; - An ensemble object.
;;; 
;;; RETURN VALUE
;;; - A list of symbols that are the player IDs of the given ensemble object. 
;;; 
;;; EXAMPLE
#|

(let ((ens (make-ensemble 
            'ens
            '((flt ((flute piccolo) :midi-channel 1))
              (clr ((b-flat-clarinet)))
              (tpt ((b-flat-trumpet c-trumpet) :midi-channel 2))
              (vln ((violin))))
            :instrument-palette
            +slippery-chicken-standard-instrument-palette+)))
  (get-players ens))

=> (FLT CLR TPT VLN)

|#
;;; SYNOPSIS
(defmethod get-players ((e ensemble))
;;; ****
  (if (players e)
      (players e)
    (progn
      (link-named-objects e)
      (let* ((current (get-first e))
             (players (remove-duplicates
                       (loop while current 
                           collect (id current)
                           do (setf current (get-data (next current)
                                                      e nil))))))
        ;; if the number of players is not equal to the length of players after
        ;; duplicates are removed, then we must have some players with same
        ;; name, which is illegal
        (unless (= (num-data e) (length players))
          (error "ensemble::get-players: Found duplicate names for players in ~
              ensemble with id ~a" (id e)))
        (setf (players e) players)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod verify-and-store :after ((e ensemble))
  (ral-to-ensemble e (instrument-palette e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Don't print the whole instrument-palette instance as it will probably be
;;; huge.  Print its id only.

(defmethod print-object :before ((e ensemble) stream)
  (let ((ip (instrument-palette e)))
    (format stream "~&ENSEMBLE: bar-line-writers: ~a~
                    ~%          players: ~a~
                    ~%          (id instrument-palette): ~a"
            (bar-line-writers e) (players e) (when ip (id ip)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone ((e ensemble))
  (clone-with-new-class e 'ensemble))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone-with-new-class :around ((e ensemble) new-class)
  (declare (ignore new-class))
  (let ((ral (call-next-method)))
    (setf (slot-value ral 'bar-line-writers) (my-copy-list 
                                              (bar-line-writers e))
          (slot-value ral 'players) (my-copy-list (players e))
          ;; don't clone the instrument-palette, it's almost certain not to be
          ;; garbage-collected 
          (slot-value ral 'instrument-palette) (instrument-palette e))
    ral))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod check-first-bar-ins-for-doubling-players
    ((e ensemble) (icm instrument-change-map) first-section-ref)
  (loop for player in (data e) do
       (setf (first-ins player)
             (if (doubles player)
                 ;; this'll throw an error if it's not set
                 (instrument-for-first-bar icm (id player) first-section-ref)
                 (id (data player))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod score-write-bar-line-all-nil ((e ensemble))
  (score-write-bar-line-all-nil-aux e)
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* ensemble/num-notes
;;; DESCRIPTION
;;; Get the number of attacked notes in a given slippery-chicken object. This
;;; method accesses the ensemble object within the given slippery-chicken
;;; object to perform this task.
;;; 
;;; ARGUMENTS
;;; - An ensemble object.
;;; 
;;; RETURN VALUE
;;; An integer that is the total number of attacked notes in the given
;;; slippery-chicken object.
;;; 
;;; EXAMPLE
#|

(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vn (violin :midi-channel 1))
                     (vc (cello :midi-channel 2))))
        :set-palette '((1 ((f3 g3 a3 b3 c4 d4 e4 f4))))
        :set-map '((1 (1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((2 4) e e e e))
                                :pitch-seq-palette ((1 2 3 4)))))
        :rthm-seq-map '((1 ((vn (1 1 1 1 1))
                            (vc (1 1 1 1 1))))))))
  (num-notes (ensemble mini)))

=> 40
|#
;;; SYNOPSIS
(defmethod num-notes ((e ensemble))
;;; ****
  (num-notes-aux e))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* ensemble/tessitura
;;; DESCRIPTION
;;;
;;; Get the average pitch of a given slippery-chicken object. This method
;;; accesses the ensemble object within the given slippery-chicken object to
;;; perform this task.
;;;
;;; NB: This method processes data in relationship to degrees of the current
;;;     tuning system (scale), which is quarter-tone by default. It is
;;;     therefore possible, when generating a piece using only chromatic
;;;     pitches but within a non-chromatic tuning to get microtonal results.
;;; 
;;; ARGUMENTS
;;; - An ensemble object.
;;; 
;;; RETURN VALUE
;;; An integer that is the average pitch of the given slippery-chicken object
;;; in degrees.
;;; 
;;; EXAMPLE
#|
;;; Change the tuning to chromatic first to get an accurate result:
(in-scale :chromatic)

=> #<tuning "chromatic-scale">

(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vn (violin :midi-channel 1))
                     (vc (cello :midi-channel 2))))
        :set-palette '((1 ((f3 g3 a3 b3 c4 d4 e4 f4))))
        :set-map '((1 (1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((2 4) e e e e))
                                :pitch-seq-palette ((1 2 3 4)))))
        :rthm-seq-map '((1 ((vn (1 1 1 1 1))
                            (vc (1 1 1 1 1))))))))
  (tessitura (ensemble mini)))

=> C4

|#
;;; SYNOPSIS
(defmethod tessitura ((e ensemble))
;;; ****
  (degree-to-note (tessitura-aux e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Wed Apr 18 16:46:58 BST 2012: Added robodoc entry

;;; ****m* ensemble/num-players
;;; DESCRIPTION
;;; Get the number of players in a given ensemble object.
;;; 
;;; ARGUMENTS
;;; - An ensemble object.
;;; 
;;; RETURN VALUE
;;; - An integer.
;;; 
;;; EXAMPLE
#|
(let ((ens (make-ensemble 
            'ens
            '((flt ((flute piccolo) :midi-channel 1))
              (clr ((b-flat-clarinet)))
              (tpt ((b-flat-trumpet c-trumpet) :midi-channel 2))
              (vln ((violin))))
            :instrument-palette
            +slippery-chicken-standard-instrument-palette+)))
  (num-players ens))

=> 4
|#
;;; SYNOPSIS
(defmethod num-players ((e ensemble))
;;; ****
  (num-data e))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* ensemble/players-exist
;;; DESCRIPTION
;;; Produce an error message and drop into the debugger if the specified
;;; player IDs are not found within the given ensemble object.
;;;
;;; ARGUMENTS
;;; - An ensemble object.
;;; - A list of symbols that are the IDs of the players sought.
;;; 
;;; RETURN VALUE
;;; NIL if the specified player ID is present within the given ensemble object,
;;; otherwise drops into the debugger with an error.
;;; 
;;; EXAMPLE
#|
;;; Returns NIL if a player with the specified ID is found in the given
;;; ensemble object.
(let ((ens (make-ensemble 
            'ens
            '((flt ((flute piccolo) :midi-channel 1))
              (clr ((b-flat-clarinet)))
              (tpt ((b-flat-trumpet c-trumpet) :midi-channel 2))
              (vln ((violin))))
            :instrument-palette
            +slippery-chicken-standard-instrument-palette+)))
  (players-exist ens '(vln)))

=> NIL

;; Drops into the debugger with an error if no player with the specified ID is
;; found in the given ensemble object.
(let ((ens (make-ensemble 
            'ens
            '((flt ((flute piccolo) :midi-channel 1))
              (clr ((b-flat-clarinet)))
              (tpt ((b-flat-trumpet c-trumpet) :midi-channel 2))
              (vln ((violin))))
            :instrument-palette
            +slippery-chicken-standard-instrument-palette+)))
  (players-exist ens '(vla)))

=>
ensemble::players-exist: VLA is not a member of the ensemble
   [Condition of type SIMPLE-ERROR]

|#
;;; SYNOPSIS
(defmethod players-exist ((e ensemble) players)
;;; ****
  (let ((e-players (players e)))
    (loop for player in players do
          (unless (member player e-players)
            (error "ensemble::players-exist: ~a is not a member of the ~
                    ensemble" player)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Sat Apr 20 12:16:58 2013 
;;; ****m* ensemble/add-player
;;; DESCRIPTION
;;; Add a player to an existing ensemble. It will be added at the end of the
;;; list. 
;;; 
;;; ARGUMENTS
;;; - The ensemble object.
;;; - The new player, either as a player object or symbol.  If the latter this
;;;   becomes the id of the player we'll create. 
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :instrument. The id of the instrument in the already existing
;;;   instrument-palette. This is required if the player argument is a symbol.
;;;   Default NIL. 
;;; - :instrument-palette. An instrument-palette object.  Default =
;;;   +slippery-chicken-standard-instrument-palette+.
;;; - :midi-channel the midi-channel for the new player
;;; - :microtones-midi-channel the microtones-midi-channel for the new player
;;;
;;; RETURN VALUE
;;; The player object added.
;;; 
;;; SYNOPSIS
(defmethod add-player ((e ensemble) player 
                       &key
                         instrument
                         (instrument-palette
                          +slippery-chicken-standard-instrument-palette+)
                         ;; MDE Tue Jul 14 19:08:15 2020, Heidhausen
                         (midi-channel 1)
                         (microtones-midi-channel -1))
;;; ****
  (let ((player (if (player-p player)
                    player
                    (make-player player instrument-palette instrument
                                 :midi-channel midi-channel
                                 :microtones-midi-channel
                                 microtones-midi-channel))))
    (add player e)
    (setf (players e) (econs (players e) (id player)))
    player))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* ensemble/sort-players
;;; DATE
;;; 27th August 2013
;;;
;;; DESCRIPTION
;;; Return a hierarchical list of players sorted by e.g. how much they're
;;; playing. 
;;; 
;;; ARGUMENTS
;;; - An ensemble object
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :stats-fun. One of the player methods which tots up statistics,
;;;    i.e. total-notes, total-degrees, total-duration, or total-bars
;;; - :ignore. A list of players (symbols) not to count in the sorting.
;;; 
;;; RETURN VALUE
;;; A list of all the players in the ensemble ordered by its statistics.
;;; 
;;; SYNOPSIS
(defmethod sort-players ((e ensemble) &key (stats-fun #'total-duration)
                                        ignore)
;;; ****
  (let* ((all-stats (loop for player in (data e) collect
                         (list (id player) (funcall stats-fun player))))
         (stats (remove-if #'(lambda (x) (member (first x) ignore)) all-stats))
         (sorted (sort stats #'(lambda (l1 l2) (> (second l1) (second l2)))))
         (ids (loop for s in sorted collect (first s)))
         (nums (loop for s in sorted collect (second s))))
    (values ids nums)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* ensemble/balanced-load?
;;; DATE
;;; 28th August 2014
;;; 
;;; DESCRIPTION
;;; Determine whether the playing load is balanced across the players of the
;;; ensemble. By default, if the least active player is playing 80% of the time
;;; that the most active player is playing, we'll return T.
;;; 
;;; ARGUMENTS
;;; - an ensemble instance
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :threshold. A number between 0.0 and 1.0 which represents the lowest
;;;   ratio between the most and least active players.  
;;; - :stats-fun. One of the player methods which tots up statistics,
;;;   i.e. total-notes, total-degrees, total-duration, or total-bars
;;; - :ignore. A list of players (symbols) not to count in the sorting.
;;; 
;;; RETURN VALUE
;;; T or NIL
;;; 
;;; SYNOPSIS
(defmethod balanced-load? ((e ensemble) &key (threshold .8) 
                                             (stats-fun #'total-duration)
                                             ignore)
;;; ****
  (let ((stats (nth-value 1 (sort-players e :stats-fun stats-fun
                                          :ignore ignore))))
    (>= (first (last stats)) (* threshold (first stats)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Thu Nov 1 10:32:07 2018 -- take a list of lists of player IDs and
;;; return an assoc-list where the IDs are the number of players in the combo
;;; and the data is a circular list of the x-player combos
(defmethod organise-combos ((e ensemble) combos)
  (let* ((al (make-assoc-list 'combos nil)))
    (loop for combo in combos
       for len = (length combo)
       for no = (get-data len al nil) 
       do
         (unless (equalp combo (remove-duplicates combo))
           (error "ensemble::organise-combos: duplicate players in ~a" combo))
         (players-exist e combo)      ; check all player IDs are in the ensemble
         (if no
             (push combo (data no))
             (add (list len (list combo)) al)))
    (nmap-data al #'(lambda (l) (make-cscl (reverse l))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Thu Nov  8 17:06:40 2018
;;; ****m* ensemble/lotsa-combos
;;; DATE
;;; November 8th 2018, Heidhausen
;;; 
;;; DESCRIPTION
;;; Get various permutations of the ensemble players, ranging from a single
;;; player up to one less than the number of players in the ensemble. This uses
;;; the shuffle method so is random, but as we use fixed-seed randomness we get
;;; repeatable results.
;;; 
;;; ARGUMENTS
;;; - the ensemble object
;;; 
;;; OPTIONAL ARGUMENTS
;;; - an integer to determine the number of times we permutate for any given
;;; number of players. Note that depending on the number of players in the
;;; ensemble and the number we're looking to put into a combo, we might not
;;; always be able to hit this target (e.g. there are only 6 possible
;;; permutations of three values). Default = 7.
;;; 
;;; RETURN VALUE
;;; a list of lists of player IDs.
;;; 
;;; EXAMPLE
#|
(lotsa-combos (make-ensemble 
              'ens
              '((flt ((flute piccolo)))
                (clr ((b-flat-clarinet)))
                (bsn bassoon)
                (tpt ((b-flat-trumpet c-trumpet))) 
                (trb tenor-trombone)
                (tb tuba)
                (vln ((violin))))))
-->
((CLR FLT TRB BSN TB VLN) (TRB FLT CLR BSN TB TPT) (TB TRB FLT VLN TPT BSN)
 (CLR BSN TPT TB TRB FLT) (FLT TPT VLN BSN TRB TB) (FLT TPT BSN VLN TB CLR)
 (TPT FLT VLN CLR TB TRB) (BSN CLR TB TRB FLT) (TB FLT BSN VLN TPT)
 (FLT TPT BSN CLR TB) (FLT TRB CLR VLN TB) (BSN CLR VLN TPT TRB)
 (BSN TRB FLT TB CLR) (FLT CLR TPT TB VLN) (TRB CLR TPT TB) (FLT VLN TB TRB)
 (TRB VLN TB CLR) (TB VLN TRB BSN) (TPT TRB CLR BSN) (CLR FLT TRB VLN)
 (FLT TPT TRB CLR) (TPT CLR FLT) (TB TRB FLT) (TRB VLN FLT) (TB TPT CLR)
 (FLT BSN TPT) (FLT VLN BSN) (TB TPT TRB) (TPT FLT) (CLR TRB) (FLT CLR)
 (TRB CLR) (BSN CLR) (TRB TB) (BSN TPT) (FLT CLR BSN TPT TRB TB VLN) (FLT)
 (CLR) (BSN) (TPT) (TRB) (TB) (VLN))

(lotsa-combos (make-ensemble 
              'ens
              '((flt ((flute piccolo)))
                (clr ((b-flat-clarinet)))
                (bsn bassoon)
                (tpt ((b-flat-trumpet c-trumpet))) 
                (trb tenor-trombone)
                (tb tuba)
                (vln ((violin))))) 
              10) ; <<--- get more combos
-->
((CLR TPT TB FLT TRB VLN) (BSN TRB VLN TB TPT CLR) (TB CLR FLT TRB BSN TPT)
 (TPT CLR BSN FLT VLN TRB) (VLN BSN TPT CLR TB TRB) (TRB BSN VLN TPT TB CLR)
 (TB TRB CLR VLN FLT BSN) (CLR TPT VLN TB FLT TRB) (TRB BSN VLN CLR FLT TPT)
 (TPT FLT CLR TB TRB VLN) (TPT TRB VLN BSN CLR) (BSN TB VLN TPT FLT)
 (BSN FLT TRB CLR TPT) (BSN TPT FLT CLR TB) (VLN TB TRB TPT FLT)
 (FLT TRB BSN TB VLN) (FLT CLR BSN TB TPT) (TRB FLT VLN TPT BSN)
 (BSN TPT TB TRB FLT) (TPT VLN BSN TRB TB) (BSN VLN TB CLR) (VLN CLR TB TRB)
 (CLR TB TRB FLT) (FLT BSN VLN TPT) (TPT BSN CLR TB) (TRB CLR VLN TB)
 (CLR VLN TPT TRB) (TRB FLT TB CLR) (CLR TPT TB VLN) (TRB CLR TPT TB)
 (VLN TB TRB) (VLN TB CLR) (VLN TRB BSN) (TRB CLR BSN) (FLT TRB VLN)
 (TPT TRB CLR) (TPT CLR FLT) (TB TRB FLT) (TRB VLN FLT) (TB TPT CLR) (VLN BSN)
 (TPT TRB) (TPT FLT) (CLR TRB) (FLT CLR) (TRB CLR) (BSN CLR) (TRB TB) (BSN TPT)
 (FLT CLR BSN TPT TRB TB VLN) (FLT) (CLR) (BSN) (TPT) (TRB) (TB) (VLN))
|#
;;; SYNOPSIS
(defmethod lotsa-combos ((e ensemble) &optional (try 7))
;;; ****
  (random-rep 10 t)
  (let* ((np (num-players e))
         (players (copy-list (players e)))
         ;; first of all the whole ensemble, then single players
         (result (cons (copy-list players)
                       (loop for p in players collect (list p))))
         tmp)
    (loop for i from 2 below np do
         (loop repeat try do
              (setq tmp (nthcdr (- np i)
                                (shuffle players :fix t :copy t :reset nil)))
              (pushnew tmp result :test #'equalp)))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* ensemble/make-ensemble
;;; DESCRIPTION
;;; Make an ensemble object, specifying the players and associated
;;; instruments.  
;;;
;;; NB: If you have an ensemble with a player doubling two instruments, be sure
;;;     to indicate some keyword argument or other as 
;;;     (fl1 ((piccolo violin) :midi-channel 1)) works but 
;;;     (fl1 ((piccolo violin))) thinks that piccolo is a nested ensemble!!!
;;;
;;; ARGUMENTS
;;; - An ID consisting of a symbol, string or number.
;;; - A list of 2-element sublists that define the ensemble. See the above
;;;   comment on adding a keyword argument for doubling players. An existing
;;;   ensemble object can be passed here whereupon it will be cloned and the ID
;;;   will be changed to the (new) given ID if it's not NIll. In this case
;;;   however the keyword arguments will be ignored.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :instrument-palette. An instrument palette object. Default = 
;;;    +slippery-chicken-standard-instrument-palette+
;;; - :bar-line-writers. Obsolete as no longer used.
;;; 
;;; RETURN VALUE
;;; An ensemble object.
;;; 
;;; EXAMPLE
#|
(let ((ens (make-ensemble 
              'ens
              '((flt ((flute piccolo) :midi-channel 1))
                (clr ((b-flat-clarinet))))
              :instrument-palette
              +slippery-chicken-standard-instrument-palette+)))
  (print ens))

=>

ENSEMBLE: bar-line-writers: NIL
          players: (FLT CLR)
          (id instrument-palette): SLIPPERY-CHICKEN-STANDARD-INSTRUMENT-PALETTE
RECURSIVE-ASSOC-LIST: recurse-simple-data: T
                      num-data: 2
                      linked: T
                      full-ref: NIL
ASSOC-LIST: warn-not-found T
CIRCULAR-SCLIST: current 0
SCLIST: sclist-length: 2, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: ENS, tag: NIL, 
data: (
PLAYER: (id instrument-palette): SLIPPERY-CHICKEN-STANDARD-INSTRUMENT-PALETTE 
doubles: T, cmn-staff-args: NIL
LINKED-NAMED-OBJECT: previous: NIL, this: (FLT), next: (CLR)
NAMED-OBJECT: id: FLT, tag: NIL, 
data: 
[...]
data: (
INSTRUMENT: lowest-written: 
[...]
NAMED-OBJECT: id: FLUTE, tag: NIL, 
[...]
INSTRUMENT: lowest-written: 
[...]
NAMED-OBJECT: id: PICCOLO, tag: NIL, 
[...]
PLAYER: (id instrument-palette): SLIPPERY-CHICKEN-STANDARD-INSTRUMENT-PALETTE 
doubles: NIL, cmn-staff-args: NIL
LINKED-NAMED-OBJECT: previous: (FLT), this: (CLR), next: NIL
NAMED-OBJECT: id: CLR, tag: NIL, 
data: 
INSTRUMENT: lowest-written: 
[...]
NAMED-OBJECT: id: B-FLAT-CLARINET, tag: NIL, 
)

|#
;;; SYNOPSIS
(defun make-ensemble (id ensemble
                      &key bar-line-writers
                        (instrument-palette 
                         +slippery-chicken-standard-instrument-palette+))
;;; ****
  ;; MDE Fri Nov  2 19:20:56 2018 -- allow existing ensemble objects to be
  ;; passed 
  (if (ensemble-p ensemble)
      (let ((ens (clone ensemble)))
        (when id (setf (id ens) id))
        ens)
      (make-instance 'ensemble :id id :data ensemble 
                     :bar-line-writers bar-line-writers 
                     :instrument-palette instrument-palette)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun num-notes-aux (ensemble)
  (loop for thing in (data ensemble) sum 
        (cond ((typep thing 'player)
               (total-notes thing))
              ((is-ral (data thing))
               (num-notes-aux (data thing)))
              (t (error "ensemble::num-notes-aux: unexpected object?: ~a"
                        thing)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tessitura-aux (ensemble)
  (/ (loop for thing in (data ensemble) 
        sum (cond ((typep thing 'player)
                   (tessitura-degree thing))
                  ((is-ral (data thing))
                   (tessitura-aux (data thing)))
                  (t (error "ensemble::tessitura-aux: ~
                             wrong data type: ~a"
                            thing))))
     (num-data ensemble)))
                                 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun score-write-bar-line-all-nil-aux (ensemble)
  (loop for thing in (data ensemble) do
        (cond ((typep thing 'player)
               (set-score-write-bar-line thing nil))
              ((is-ral (data thing))
               (score-write-bar-line-all-nil-aux (data thing)))
              (t (error "ensemble::score-write-bar-line-all-nil-aux: ~
                         what's this doing here?: ~a"
                        thing)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ral-to-ensemble (ral ins-palette)
  (when (and ral (data ral))
    (let ((num-players 0))
      (loop for i in (data ral) and j from 0 do
           (let ((data (data i)))
             (if (is-ral data)
                 (multiple-value-bind
                       (ensemble nplayers)
                     (ral-to-ensemble data ins-palette)
                   (incf num-players nplayers)
                   (setf (data (nth j (data ral)))
                         ensemble))
                 (progn
                   (incf num-players)
                   (setf (nth j (data ral))
                         (apply #'make-player (append
                                               (list (id i) 
                                                     ins-palette)
                                               (if (listp data)
                                                   data
                                                   (list data)))))))))
      (values (sc-change-class ral 'ensemble)
              num-players))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ensemble-p (thing)
  (typep thing 'ensemble))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EOF ensemble.lsp

