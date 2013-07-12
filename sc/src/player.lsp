;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* linked-named-object/player
;;; NAME 
;;; player
;;;
;;; File:             player.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> player
;;;
;;; Version:          1.0.5
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Implementation of the player class which holds an
;;;                   instrument or a assoc-list of instruments in it's data
;;;                   slot.    
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    7th September 2001
;;;
;;; $$ Last modified: 16:26:58 Tue Jul  3 2012 BST
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

(defclass player (linked-named-object)
  ;; an instrument-palette that contains the instrument objects to be
  ;; cloned and stored in the data list
  ((instrument-palette :accessor instrument-palette
                       :initarg :instrument-palette :initform nil)
   ;; any additional arguments to the call to cmn::staff, like staff size,
   ;; number of lines etc. Instead of being real cmn function calls, as they
   ;; would be in normal cmn, this is a simple list of pairs: 
   ;; e.g. '(staff-size .8 staff-lines 3)
   (cmn-staff-args :accessor cmn-staff-args :type list :initarg :cmn-staff-args
                   :initform nil)
   ;; which midi-channel this player is on
   (midi-channel :accessor midi-channel :type integer :initarg :midi-channel
                 :initform 1)  
   ;; when an instrument can play microtonal chords we generally need an extra
   ;; channel for the microtones, so that chords can be played.  
   (microtones-midi-channel :accessor microtones-midi-channel :type integer
                            :initarg :microtones-midi-channel :initform -1)
   ;; whether the player plays more than one instrument or not
   (doubles :accessor doubles :type boolean :initform nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((p player) &rest initargs)
  (declare (ignore initargs))
  (let ((data (data p))
        (staff-args (cmn-staff-args p))
        (ip (instrument-palette p)))
    (unless (or (typep data 'instrument)
                (and (typep data 'assoc-list)
                     (typep (first (data data)) 'instrument)))
      (when (and ip (not (typep ip 'instrument-palette)))
        (error "player::initialize-instance: ~
                simple references as instruments must be accompanied ~
                by instrument-palettes!: data: ~a, instrument-palette: ~a"
               (data p) ip)))
    ;; when cmn-staff-args are given, call the functions in the cmn package and
    ;; re-store the results.
    #+cmn
    (when staff-args 
      (unless (evenp (length staff-args))
        (error "player::initialize-instance: ~
                cmn-staff-args must be a list of function,argument pairs: ~
                player: ~a, cmn-staff-args: ~a" 
               (id p) staff-args))
      (setf (cmn-staff-args p)
            (loop 
               for fun in staff-args by #'cddr 
               and arg in (cdr staff-args) by #'cddr collect 
               (funcall (symbol-function (rm-package fun :cmn))
                        (rm-package arg :cmn)))))
    ;; MDE Sun Jan  8 09:05:18 2012 -- one instrument in a list shouldn't make
    ;; a doubling player 
    (setf (data p) (if (and (listp data) (> (length data) 1))
                       ;; he/she plays more than one instrument.
                       (progn
                         (setf (doubles p) t)
                         (make-doublings-al (id p) data ip))
                       ;; copy the instrument as it may be used many times.
                       (when (and data ip)
                         (let ((ins (get-data data ip)))
                           (unless ins
                             (error "player::init: can't find instrument in ~
                                   instrument-palette:~%instrument: ~a~
                                   ~%palette:~%~a" data ip))
                           (clone ins)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Don't print the whole instrument-palette instance as it will probably be
;;; huge.  Print its id only.

(defmethod print-object :before ((p player) stream)
  (let ((ip (instrument-palette p)))
    (format stream "~&PLAYER: (id instrument-palette): ~a ~%doubles: ~a, ~
                    cmn-staff-args: ~a, total-notes: ~a, total-degrees: ~a, ~
                    ~%total-duration: ~a, total-bars: ~a, tessitura: ~a"
            (when ip (id ip)) (doubles p) (cmn-staff-args p)
            ;; MDE Thu Apr 19 13:31:06 2012 -- 
            (total-notes p) (total-degrees p)
            (secs-to-mins-secs (total-duration p))
            (total-bars p) (tessitura-note p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone ((p player))
  (clone-with-new-class p 'player))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone-with-new-class :around ((p player) new-class)
  (declare (ignore new-class))
  (let ((named-object (call-next-method)))
    ;; don't clone the instrument-palette, it's probably huge and almost
    ;; certain not to be garbage-collected
    (setf (slot-value named-object 'instrument-palette) (instrument-palette p)
          (slot-value named-object 'doubles) (doubles p)
          (slot-value named-object 'midi-channel) (midi-channel p)
          (slot-value named-object 'microtones-midi-channel)
          (microtones-midi-channel p)
          (slot-value named-object 'cmn-staff-args) (cmn-staff-args p))
    named-object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Jan  7 18:29:17 EST 2012: Added robodoc info

;;; ****m* player/plays-transposing-instrument
;;; DESCRIPTION
;;; Determine whether a given player object has one or more transposing
;;; instrument objects assigned to it.
;;; 
;;; ARGUMENTS
;;; - A player object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether instruments that transpose at the octave are
;;;   to be considered transposing instruments.  T = instruments that transpose
;;;   at the octave are not considered transposing instruments. Default = T.
;;; 
;;; RETURN VALUE
;;; Returns T if one or more of the instrument objects assigned to the given
;;; player object has a transposition value other than C or a
;;; transposition-semitones value other than 0.
;;; 
;;; EXAMPLE

#|
;; Create a player object using the 'b-flat-clarinet instrument object
;; definition from the default +slippery-chicken-standard-instrument-palette+,
;; then apply the method. 
(let* ((ip +slippery-chicken-standard-instrument-palette+)
       (plr (make-player 'cl ip 'b-flat-clarinet)))
  (plays-transposing-instrument plr))

=> T

;; Create a player object using the 'flute instrument object definition from
;; the default +slippery-chicken-standard-instrument-palette+, then apply the
;; method. 
(let* ((ip +slippery-chicken-standard-instrument-palette+)
       (plr (make-player 'fl ip 'flute)))
  (plays-transposing-instrument plr))

=> NIL

;; Although the intended procedure is to list single instruments as once-off
;; symbols (as in the previous example), single instruments can also be added
;; as a one-item list
(let* ((ip +slippery-chicken-standard-instrument-palette+)
       (plr (make-player 'fl ip '(flute))))
  (doubles plr))

=> NIL

;; Create a player object using a list that consists of the 'flute and
;; 'alto-sax instrument object definitions from the default
;; +slippery-chicken-standard-instrument-palette+, then apply the method to see
;; that it returns T even when only one of the instruments is transposing.
(let* ((ip +slippery-chicken-standard-instrument-palette+)
       (plr (make-player 'fl ip '(flute alto-sax))))
  (plays-transposing-instrument plr))

=> T

;; Setting the optional argument to NIL causes instruments that transpose at
;; the octave to return T.
(let* ((ip +slippery-chicken-standard-instrument-palette+)
       (plr (make-player 'db ip 'double-bass)))
  (plays-transposing-instrument plr))

=> NIL

(let* ((ip +slippery-chicken-standard-instrument-palette+)
       (plr (make-player 'db ip 'double-bass)))
  (plays-transposing-instrument plr nil))

=> T

|#
;;; SYNOPSIS
(defmethod plays-transposing-instrument ((p player) 
                                         &optional (ignore-octaves t) ignore)
;;; ****                                
  (declare (ignore ignore))
  (if (doubles p)
      (loop for ins in (data (data p)) do
           (when (transposing-instrument-p ins ignore-octaves)
             (return t)))
      (transposing-instrument-p (data p) ignore-octaves)))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Jan  7 18:46:02 EST 2012: Added robodoc info

;;; ****m* player/microtonal-chords-p
;;; DESCRIPTION
;;; Determines whether the MICROTONES-MIDI-CHANNEL slot of the given player
;;; object is set to a value greater than 0, indicating that the player and its
;;; instrument are capable of performing microtonal chords.
;;; 
;;; ARGUMENTS
;;; - A player object.
;;; 
;;; RETURN VALUE
;;; Returns T if the value stored in the MICROTONES-MIDI-CHANNEL slot of the
;;; given player object is greater than 0, otherwise returns NIL.
;;; 
;;; EXAMPLE
#|
;; Returns T
(let* ((ip +slippery-chicken-standard-instrument-palette+)
       (plr (make-player 'vln ip 'violin :microtones-midi-channel 2)))
  (microtonal-chords-p plr))

=> T

;; Returns NIL
(let* ((ip +slippery-chicken-standard-instrument-palette+)
       (plr (make-player 'pno ip 'piano)))
  (microtonal-chords-p plr))

=> NIL

|#
;;; SYNOPSIS
(defmethod microtonal-chords-p ((p player))
  (> (microtones-midi-channel p) 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod score-write-bar-line ((p player))
  (let* ((data (data p))
         (ins (if (typep data 'assoc-list)
                  (first (data data))
                data)))
    (score-write-bar-line ins)))
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod set-score-write-bar-line ((p player) int)
  (unless (or (not int) (integerp int))
    (error "player::set-score-write-bar-line: ~
            set-score-write-bar-line: argument must be nil or an integer: ~a" 
           int))
  (let* ((data (data p)))
    (if (typep data 'assoc-list)
        (loop for ins in (data data) do
              (setf (score-write-bar-line ins) int))
      (setf (score-write-bar-line data) int)))
  int)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sun Apr 29 15:37:13 BST 2012: Added robodoc entry

;;; ****m* player/total-notes
;;; DESCRIPTION
;;; Get the total number of notes played by a specified player.
;;; 
;;; ARGUMENTS
;;; - A player object.
;;; 
;;; RETURN VALUE
;;; - An integer that is the number of notes for that player.
;;; 
;;; EXAMPLE
#|

(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vn (violin :midi-channel 1))
                     (va (violin :midi-channel 2))
                     (vc (cello :midi-channel 3))))
        :set-palette '((1 ((gs3 as3 b3 cs4 ds4 e4 fs4 gs4 as4 b4 cs5))))
        :set-map '((1 (1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((2 4) q (e) s (32) 32))
                                :pitch-seq-palette ((1 2 3)))))
        :rthm-seq-map '((1 ((vn (1 1 1 1 1))
                            (va (1 1 1 1 1))
                            (vc (1 1 1 1 1))))))))
  (print (total-notes (get-data 'vc (ensemble mini)))))

=> 15

|#

;;; SYNOPSIS
(defmethod total-notes ((p player))
;;; ****
  (let* ((data (data p)))
    (if (typep data 'assoc-list)
        (loop for ins in (data data) sum
             (total-notes ins))
        (total-notes data))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Wed Aug  8 10:47:10 BST 2012: Added robodoc entry

;;; ****m* player/total-degrees
;;; DESCRIPTION
;;; Return a number that reflects the mean note (tessitura) of a player's
;;; part. This is calculated by incrementing the TOTAL-DEGREES slot of the
;;; corresponding instrument object for each attacked note in the player's part
;;; by the degree of that note (in the scale of the piece), and then dividing
;;; the sum by the total number of notes in the player's part.
;;; 
;;; ARGUMENTS
;;; - A player object.
;;; 
;;; RETURN VALUE
;;; - An integer.
;;; 
;;; EXAMPLE
#|

(in-scale :chromatic)

(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vn (violin :midi-channel 1))
                     (va (violin :midi-channel 2))
                     (vc (cello :midi-channel 3))))
        :set-palette '((1 ((gs3 as3 b3 cs4 ds4 e4 fs4 gs4 as4 b4 cs5))))
        :set-map '((1 (1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((2 4) q (e) s (32) 32))
                                :pitch-seq-palette ((1 2 3)))))
        :rthm-seq-map '((1 ((vn (1 1 1 1 1))
                            (va (1 1 1 1 1))
                            (vc (1 1 1 1 1))))))))
  (total-degrees (first (data (ensemble mini)))))

=> 865

|#
;;; SYNOPSIS
(defmethod total-degrees ((p player))
;;; ****
  (let* ((data (data p)))
    (if (typep data 'assoc-list)
        (loop for ins in (data data) sum
             (total-degrees ins))
        (total-degrees data))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Wed Aug  8 11:14:10 BST 2012: Added robodoc entry

;;; ****m* player/total-bars
;;; DESCRIPTION
;;; Return the number of bars in a specified player object.
;;; 
;;; ARGUMENTS
;;; - A player object.
;;; 
;;; RETURN VALUE
;;; - An integer.
;;; 
;;; EXAMPLE
#|

(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vn (violin :midi-channel 1))
                     (va (violin :midi-channel 2))
                     (vc (cello :midi-channel 3))))
        :set-palette '((1 ((gs3 as3 b3 cs4 ds4 e4 fs4 gs4 as4 b4 cs5))))
        :set-map '((1 (1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((2 4) q (e) s (32) 32))
                                :pitch-seq-palette ((1 2 3)))))
        :rthm-seq-map '((1 ((vn (1 1 1 1 1))
                            (va (1 1 1 1 1))
                            (vc (1 1 1 1 1))))))))
  (total-bars (first (data (ensemble mini)))))

=> 5

|#
;;; SYNOPSIS
(defmethod total-bars ((p player))
;;; ****
  (let* ((data (data p)))
    (if (typep data 'assoc-list)
        (loop for ins in (data data) sum
             (total-bars ins))
        (total-bars data))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sun Apr 29 15:48:52 BST 2012: Started robodoc entry

;;; ****m* player/tessitura-degree
;;; DESCRIPTION
;;; Return a number that represents the average pitch for a specified
;;; instrument over the course of a piece. The number returned will be degrees
;;; in the current scale.
;;; 
;;; ARGUMENTS
;;; - A player object.
;;; 
;;; RETURN VALUE
;;; A number that is the tessitura-degree; i.e., average pitch of the given
;;; instrument for the entirety of the given musical data.
;;; 
;;; EXAMPLE
#|

(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vn (violin :midi-channel 1))
                     (va (violin :midi-channel 2))
                     (vc (cello :midi-channel 3))))
        :set-palette '((1 ((gs3 as3 b3 cs4 ds4 e4 fs4 gs4 as4 b4 cs5))))
        :set-map '((1 (1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((2 4) q (e) s (32) 32))
                                :pitch-seq-palette ((1 2 3)))))
        :rthm-seq-map '((1 ((vn (1 1 1 1 1))
                            (va (1 1 1 1 1))
                            (vc (1 1 1 1 1))))))))
  (tessitura-degree (get-data 'vc (ensemble mini))))

=> 136

|#
;;; SYNOPSIS
(defmethod tessitura-degree ((p player))
;;; ****
  (let* ((data (data p)))
    (if (typep data 'assoc-list)
        (/ (loop for ins in (data data) sum
                (tessitura-degree ins))
           (sclist-length data))
        (tessitura-degree data))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Thu Apr 19 13:32:34 2012 

;;; SAR Wed Aug  8 11:18:08 BST 2012: Added robodoc entry

;;; ****m* player/tessitura-note
;;; DESCRIPTION
;;; Return the value of the TESSITURA-DEGREE slot of a specified player object
;;; as a note-name symbol.
;;; 
;;; ARGUMENTS
;;; - A player object.
;;; 
;;; RETURN VALUE
;;; - A note-name symbol.
;;; 
;;; EXAMPLE
#|
(in-scale :chromatic)

(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vn (violin :midi-channel 1))
                     (va (violin :midi-channel 2))
                     (vc (cello :midi-channel 3))))
        :set-palette '((1 ((gs3 as3 b3 cs4 ds4 e4 fs4 gs4 as4 b4 cs5))))
        :set-map '((1 (1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((2 4) q (e) s (32) 32))
                                :pitch-seq-palette ((1 2 3)))))
        :rthm-seq-map '((1 ((vn (1 1 1 1 1))
                            (va (1 1 1 1 1))
                            (vc (1 1 1 1 1))))))))
  (tessitura-note (first (data (ensemble mini)))))

=> BF3

|#
;;; SYNOPSIS
(defmethod tessitura-note ((p player))
;;; ****
  (let ((td (tessitura-degree p)))
    ;; MDE Thu Apr 19 14:07:40 2012 -- don't give return a note if none have
    ;; been played 
    (unless (zerop td)
      (degree-to-note td))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sun Apr 29 16:04:48 BST 2012: Added robodoc entry

;;; ****m* player/total-duration
;;; DESCRIPTION
;;; Get the total duration of played notes for a given player over the span of
;;; a piece.
;;; 
;;; ARGUMENTS
;;; - A player object.
;;; 
;;; RETURN VALUE
;;; A number that is the total duration in seconds of played notes.
;;; 
;;; EXAMPLE
#|
(let ((mini
       (make-slippery-chicken
        '+mini+
        :ensemble '(((vn (violin :midi-channel 1))
                     (va (violin :midi-channel 2))
                     (vc (cello :midi-channel 3))))
        :set-palette '((1 ((gs3 as3 b3 cs4 ds4 e4 fs4 gs4 as4 b4 cs5))))
        :set-map '((1 (1 1 1 1 1)))
        :rthm-seq-palette '((1 ((((2 4) q (e) s (32) 32))
                                :pitch-seq-palette ((1 2 3))))
                            (2 ((((2 4) (q) e (s) 32 32))
                                :pitch-seq-palette ((1 2 3)))))
        :rthm-seq-map '((1 ((vn (1 1 1 1 1))
                            (va (2 2 2 2 2))
                            (vc (1 2 1 2 1))))))))
  (print (total-duration (get-data 'vn (ensemble mini))))
  (print (total-duration (get-data 'va (ensemble mini))))
  (print (total-duration (get-data 'vc (ensemble mini)))))

=>
6.875 
3.75 
5.625

|#
;;; SYNOPSIS
(defmethod total-duration ((p player))
;;; ****
  (let* ((data (data p)))
    (if (typep data 'assoc-list)
        (loop for ins in (data data) sum
              (total-duration ins))
      (total-duration data))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Thu Jan 12 18:36:07 GMT 2012: Added robodoc info

;;; ****m* player/player-get-instrument
;;; DESCRIPTION
;;; Get the instrument object assigned to a single-instrument player object or
;;; get the specified instrument object assigned to a multiple-instrument
;;; player object.  
;;;
;;; NB: This method will drop into the debugger with an error if no optional
;;;     argument is supplied when applying the method to a multiple-instrument
;;;     player object. It will also print a warning when supplying an optional
;;;     argument to a player object that contains only one instrument object.  
;;; 
;;; ARGUMENTS
;;; - A player object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - Actually a required object for multiple-instrument player objects: The
;;;   symbol that is the ID of the sought-after instrument object, as it
;;;   appears in the instrument-palette with which the player object which
;;;   made. If the given player object consists of only one instrument object,
;;;   this argument is disregarded and a warning is printed.
;;; 
;;; RETURN VALUE
;;; Returns an instrument object.
;;; 
;;; EXAMPLE
#|

;; Returns an instrument object. Needs no optional argument when applied to a
;; player object that contains only one instrument object
(let* ((ip +slippery-chicken-standard-instrument-palette+)
       (plr (make-player 'pno ip 'piano)))
   (player-get-instrument plr))

=>
INSTRUMENT:
[...]
NAMED-OBJECT: id: PIANO, tag: NIL, 
data: NIL

;; Returns the only existing instrument object and prints a warning if using
;; the optional argument when applying to a single-instrument player object 
(let* ((ip +slippery-chicken-standard-instrument-palette+)
       (plr (make-player 'pno ip 'piano)))
  (id (player-get-instrument plr 'piano)))

=> PIANO
WARNING:
   player::player-get-instrument: player PNO has only 1 instrument so optional
argument PIANO is being ignored 

;; Asking for a non-existent instrument obect from a single-instrument player
;; object returns the only existing instrument object instead
(let* ((ip +slippery-chicken-standard-instrument-palette+)
       (plr (make-player 'pno ip 'piano)))
  (id (player-get-instrument plr 'marimba)))

=> PIANO
WARNING:
   player::player-get-instrument: player PNO has only 1 instrument so optional
argument PIANO is being ignored 

;; The ID desired instrument object must be specified when applying the method
;; to a multiple-instrument player object
(let* ((ip +slippery-chicken-standard-instrument-palette+)
       (plr (make-player 'percussion ip '(marimba vibraphone))))
  (id (player-get-instrument plr 'marimba)))

=> MARIMBA

;; Interrupts and drops into the debugger when the optional argument is omitted
;; in applying the method to a multiple-instrument player object
(let* ((ip +slippery-chicken-standard-instrument-palette+)
       (plr (make-player 'percussion ip '(marimba vibraphone))))
   (player-get-instrument plr))

=>
player::player-get-instrument: PERCUSSION doubles so you need to pass the ID of
the instrument you want. 
   [Condition of type SIMPLE-ERROR]

|#
;;; SYNOPSIS
(defmethod player-get-instrument ((p player) &optional ins (warn t))
;;; ****
  (let* ((data (data p)))
    (if (doubles p) ; (typep data 'assoc-list) ; doubles
        (if ins
            (get-data ins data)
            (error "player::player-get-instrument: ~a doubles so you need to ~
                    pass the ID of the instrument you want." (id p)))
        (progn
          (when (and warn ins)
            (warn "player::player-get-instrument: player ~a has only 1 ~
                 instrument so optional argument ~a is being ignored"
                  (id p) ins))
          data))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****f* player/make-player
;;; DESCRIPTION
;;; Create a player object from a specified instrument-palette object and a
;;; specified instrument or list of instruments which that player plays. 
;;;
;;; The player object is separate from the instrument object as on player in an
;;; ensemble may perform more than one instrument ("double"), such as flute and
;;; piccolo, clarinet and bass clarinet, or sax, flute and clarinet.
;;; 
;;; ARGUMENTS
;;; - A symbol which will be the ID of the resulting player object.
;;; - An instrument-palette object.
;;; - A symbol or a list of symbols that are the instruments from the
;;;   specified instrument-palette object that the given player will play, as
;;;   spelled and defined within the instrument-palette object. NB: If only one
;;;   instrument is to be assigned to the given player, it should be stated as
;;;   symbol rather than a list, to avoid errors in the DOUBLES slot.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :midi-channel. An integer that indicates the MIDI channel on which any
;;;   non-microtonal pitch material for this player is to be played
;;;   back. Default = 1.
;;; - :microtones-midi-channel. An integer that indicates the MIDI channel on
;;;   which any microtonal pitch material for this player is to be played
;;;   back. slippery chicken uses this channel to add MIDI pitch-bends via CM
;;;   so that microtonal chords are possible, but due to a current glitch these
;;;   tracks contain no pitch-bend data. A work-around for this is to simply
;;;   open the MIDI file in a sequencer and shift the entire channel by the
;;;   desired pitch-bend value. Default = -1.
;;; - :cmn-staff-args. A list of pairs that indicate any additional arguments
;;;   to the call to cmn::staff for this player, such as staff size, number of
;;;   lines etc. Instead of being real cmn function calls, as they would be in
;;;   normal cmn, this is a simple list of pairs; e.g. '(staff-size .8
;;;   staff-lines 3). Defaults = NIL.
;;;
;;; RETURN VALUE
;;; Returns a player object.
;;; 
;;; EXAMPLE
#|
;; Create a player object with just one instrument object
(let ((ip (make-instrument-palette 
            'inst-pal 
            '((picc (:transposition-semitones 12 :lowest-written d4
                     :highest-written c6)) 
              (flute (:lowest-written c4 :highest-written d7))  
              (clar (:transposition-semitones -2 :lowest-written e3
                     :highest-written c6))  
              (horn (:transposition f :transposition-semitones -7
                     :lowest-written f2 :highest-written c5))    
              (vln (:lowest-written g3 :highest-written c7 :chords t))  
              (vla (:lowest-written c3 :highest-written f6 :chords t))))))
  (make-player 'player-one ip 'flute))

=> 
PLAYER: (id instrument-palette): INST-PAL 
doubles: NIL, cmn-staff-args: NIL
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: PLAYER-ONE, tag: NIL, 
data: 
INSTRUMENT: lowest-written: 
[...]
NAMED-OBJECT: id: FLUTE, tag: NIL, 
data: NIL

;; Create a player object with two instruments, setting the midi channels using
;; the keyword arguments, then print the corresponding slots to see the changes 
(let* ((ip (make-instrument-palette 
            'inst-pal 
            '((picc (:transposition-semitones 12 :lowest-written d4
                     :highest-written c6)) 
              (flute (:lowest-written c4 :highest-written d7))  
              (clar (:transposition-semitones -2 :lowest-written e3
                     :highest-written c6))  
              (horn (:transposition f :transposition-semitones -7
                     :lowest-written f2 :highest-written c5))    
              (vln (:lowest-written g3 :highest-written c7 :chords t))  
              (vla (:lowest-written c3 :highest-written f6 :chords t))))) 
       (plr (make-player 'player-one ip '(flute picc) 
                         :midi-channel 1
                         :microtones-midi-channel 2)))
  (print (loop for i in (data (data plr)) collect (id i)))
  (print (midi-channel plr))
  (print (microtones-midi-channel plr)))

=>
(FLUTE PICC) 
1 
2

;;; With specified cmn-staff-args
(let ((ip (make-instrument-palette 
           'inst-pal 
           '((picc (:transposition-semitones 12 :lowest-written d4
                    :highest-written c6)) 
             (flute (:lowest-written c4 :highest-written d7))  
             (clar (:transposition-semitones -2 :lowest-written e3
                    :highest-written c6))  
             (horn (:transposition f :transposition-semitones -7
                    :lowest-written f2 :highest-written c5))    
             (vln (:lowest-written g3 :highest-written c7 :chords t))  
             (vla (:lowest-written c3 :highest-written f6 :chords t))))))
  (make-player 'player-one ip '(flute picc) 
               :midi-channel 1
               :microtones-midi-channel 2
               :cmn-staff-args '(staff-size .8 staff-lines 3)))

=> 
PLAYER: (id instrument-palette): INST-PAL 
doubles: T, cmn-staff-args: (#<SELF-ACTING {10097B6E73}>
                             #<SELF-ACTING {10097B6EE3}>), total-notes: 0, total-degrees: 0, 
total-duration: 0.000, total-bars: 0, tessitura: NIL
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: PLAYER-ONE, tag: NIL, 
data: 
[...]


|#
;;; SYNOPSIS
(defun make-player (id instrument-palette instruments
                    &key (cmn-staff-args nil)
                         (microtones-midi-channel -1) (midi-channel 1))
;;; ****
  (make-instance 'player :id id :data instruments 
                 :midi-channel midi-channel
                 :microtones-midi-channel microtones-midi-channel
                 :cmn-staff-args cmn-staff-args
                 :instrument-palette instrument-palette))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-doublings-al (player ins-list ins-palette)
  (make-assoc-list (format nil "~a-doublings" player)
                   (loop for ref in ins-list 
                       for ins = (get-data ref ins-palette)
                       do (unless ins
                            (error "player::make-doublings-al: ~
                                    All references to ~
                                    instruments of the instrument-palette ~
                                    in ensemble must be present: ~a"
                                   ref))
                       collect (clone ins))))
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Jan  7 16:08:08 EST 2012: Added player-p function

(defun player-p (thing)
  (typep thing 'player))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EOF player.lsp

