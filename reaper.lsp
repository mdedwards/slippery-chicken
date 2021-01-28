;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* sndfile/reaper-item
;;; ****c* sclist/reaper-track
;;; ****c* sclist/reaper-file
;;; NAME
;;; reaper
;;;
;;; File:             reaper.lsp            
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> sndfile ->
;;;                   reaper-item
;;; Class Hierarchy:  named-object -> linked-named-object -> sclist ->
;;;                   reaper-track
;;; Class Hierarchy:  named-object -> linked-named-object -> sclist ->
;;;                   reaper-file
;;;
;;; Version:          1.0.11
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Class and routine definition for the generation of partial
;;;                   and/or complete reaper files from rhythms, soundfiles.
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    January 21st 2021
;;;
;;; $$ Last modified:  16:36:20 Thu Jan 28 2021 CET
;;;
;;; SVN ID: $Id: sclist.lsp 963 2010-04-08 20:58:32Z medward2 $
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
(in-package :slippery-chicken)

;;; todo: mirror the functionality of clm-play with a reaper-play method in the
;;; sc class  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; a single item/clip/sound object on a track
(defclass reaper-item (sndfile)
  ((istring :accessor istring :type string :allocation :class
            ;; read in the text for an item
            :initform (read-from-file (file-from-sc-dir "src/reaper-item.txt")))
   ;; these are the actual mouse-draggable fades in seconds
   (fade-in :accessor fade-in :type number :initarg :fade-in :initform 0.005)
   (fade-out :accessor fade-out :type number :initarg :fade-out :initform 0.005)
   ;; aka speed
   (play-rate :accessor play-rate :type number :initarg :play-rate
              :initform 1.0)
   ;; no transposition on stretch?
   (preserve-pitch :accessor preserve-pitch :type boolean
                   :initarg :preserve-pitch :initform t)
   ;; the output start-time (in seconds, in the reaper file) NB the input file
   ;; start time is the start slot of the sndfile class
   (start-time :accessor start-time :type number :initarg :start-time
               :initform 0.0)
   ;; the name visible in the reaper item: by default the sndfile name
   (name :accessor name :initarg :name :initform nil)
   ;; the name of the track to put this item on. If items are passed to
   ;; make-reaper-file then before writing they'll be separated into tracks and
   ;; written into those. This can be any string though of course it makes sense
   ;; to put more than one item on a single track
   (track :accessor track :type string :initarg :track :initform "lispy")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; a holder for reaper-items that will all be placed on a single user-named
;;; reaper track. The reaper-items are in the data slot; the ID slot will
;;; determine the track name in reaper (lower case).
(defclass reaper-track (sclist)
  ;; the actual number of channels the track should have. the min-/max- variants
  ;; are for limiting/forcing this
    ((channels :accessor channels :type integer :initarg :channels
               :initform 2)
     ;; a sndfile could have any number of channels of course. usually we'd use
     ;; the maximum number of channels a track's sndfiles have to set the
     ;; channels slot, but let's limit these to something reasonable (or
     ;; e.g. force 4-chan tracks even though all sndfiles are stereo).
     (min-channels :accessor min-channels :type integer :initarg :min-channels
                   :initform 2)
     (max-channels :accessor max-channels :type integer :initarg :max-channels
                   :initform 4)
     ;; the reaper text read in from a file 
     (tstring :accessor tstring :type string :allocation :class
              :initform (read-from-file (file-from-sc-dir
                                         "src/reaper-track.txt")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; the reaper-item objects are in the data slot; the id will be used as a file
;;; name (lower case)
(defclass reaper-file (sclist)
  ;; for the project settings
  ((record-path :accessor record-path :type string :initarg :record-path
                :initform "/tmp/")
   (time-sig :accessor time-sig :initarg :time-sig :initform '(4 4))
   (tempo :accessor tempo :type number :initarg :tempo :initform 60)
   ;; zoom factor for the time line. At the moment this gives me about 80
   ;; seconds overview
   (zoom :accessor zoom :type number :initarg :zoom :initform 20)
   ;; where to place the cursor (e.g. at the end of a sequence of items,
   ;; including rests)
   (cursor :accessor cursor :type number :initarg :cursor :initform 0.0)
   ;; the reaper header text, read in from a file
   (header :accessor header :type string :allocation :class
           :initform (read-from-file (file-from-sc-dir
                                      "src/reaper-header.txt")))
   ;; this will be set when create-tracks is called. It'll be an assoc-list with
   ;; all the reaper-items sorted into data lists associated with the track
   ;; names generated/given when the reaper-items were initialised.
   (tracks :accessor tracks :initform nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod initialize-instance :after ((ri reaper-item) &rest initargs)
  (declare (ignore initargs))
  ;; if there's no name, use the file name (minus dir and extension) as the
  ;; name.
  (when (and (path ri) (stringp (path ri)) (not (name ri)))
    (setf (name ri) (pathname-name (path ri)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod print-object :before ((ri reaper-item) stream)
  (format stream "~%REAPER-ITEM: fade-in: ~a, fade-out: ~a, play-rate: ~a, ~
                  ~%preserve-pitch: ~a, start-time: ~a, name: ~a, track: ~a"
          (fade-in ri) (fade-out ri) (play-rate ri) (preserve-pitch ri)
          (start-time ri) (name ri) (track ri)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod clone ((ri reaper-item))
  (clone-with-new-class ri 'reaper-item))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod clone-with-new-class :around ((ri reaper-item) new-class)
  (declare (ignore new-class))
  (let ((sndfile (call-next-method)))
    (setf (slot-value sndfile 'fade-in) (fade-in ri)
          (slot-value sndfile 'fade-out) (fade-out ri)
          (slot-value sndfile 'preserve-pitch) (preserve-pitch ri)
          (slot-value sndfile 'start-time) (start-time ri)
          (slot-value sndfile 'name) (name ri)
          (slot-value sndfile 'track) (track ri)
          (slot-value sndfile 'play-rate) (play-rate ri))
    sndfile))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod print-object :before ((rt reaper-track) stream)
  (format stream "~%REAPER-TRACK: channels: ~a"
                  (channels rt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod clone ((rt reaper-track))
  (clone-with-new-class rt 'reaper-track))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod clone-with-new-class :around ((rt reaper-track) new-class)
  (declare (ignore new-class))
  (let ((scl (call-next-method)))
    (setf (slot-value scl 'channels) (channels rt))
    scl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod initialize-instance :after ((rf reaper-file) &rest initargs)
  (declare (ignore initargs))
  (setf (tempo rf) (make-tempo (tempo rf))
        (time-sig rf) (make-time-sig (time-sig rf))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod print-object :before ((rf reaper-file) stream)
  (format stream "~%REAPER-FILE: zoom: ~a, cursor: ~a, ~%record-path: ~a, ~
                  ~%tempo: ~a~&time-sig: ~a" (zoom rf) (cursor rf)
                  (record-path rf) (tempo rf) (time-sig rf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod clone ((rf reaper-item))
  (clone-with-new-class rf 'reaper-item))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod clone-with-new-class :around ((rf reaper-file) new-class)
  (declare (ignore new-class))
  (let ((scl (call-next-method)))
    (setf (slot-value scl 'record-path) (strcpy (record-path rf))
          (slot-value scl 'tempo) (clone (tempo rf))
          (slot-value scl 'zoom) (zoom rf)
          (slot-value scl 'cursor) (cursor rf)
          (slot-value scl 'time-sig) (clone (time-sig rf)))
    scl))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; write an item's data to a stream, using the istring slot as a template
(defmethod write-item ((ri reaper-item) stream)
  (format stream (istring ri) (start-time ri) (duration ri) (fade-in ri)
          (fade-out ri) (name ri) (start ri) (play-rate  ri)
          (preserve-pitch ri) (path ri)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; write all items in a track to a reaper stream, using the tstring as template
(defmethod write-track ((rt reaper-track) stream)
  (format stream (tstring rt) (string-downcase (string (id rt))) (channels rt))
  (loop for item in (data rt) do (write-item item stream))
  (format stream "~&  >"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; write the reaper header to a stream using the given slots for project
;;; settings 
(defmethod write-header ((rf reaper-file) stream)
  (format stream (header rf) (cursor rf) (zoom rf) (record-path rf) 
          (bpm (tempo rf)) (num (time-sig rf)) (denom (time-sig rf))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod write-footer ((rf reaper-file) stream)
  (declare (ignore rf)) ; though we might need some slots later, hence method
  (format stream "~%  <EXTENSIONS~%  >~%>~%"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; create the reaper-track instances to hold and later write the instances that
;;; should appear together one one track
;;; all the reaper-items are stored in the data slot, no matter what track they
;;; should be written to. Sort these now.
(defmethod create-tracks ((rf reaper-file)
                          &key (min-channels 2) (max-channels 4))
  ;; items can of course use any name in their track slot, but get all unique
  ;; names here as this will determine how many tracks are written.
  (let* ((track-names (remove-duplicates (mapcar #'track (data rf))
                                         :test #'string=))
         ;; create an assoc-list using the track names as IDs and empty lists
         ;; (to cons into) with the items further down
         (al (make-assoc-list
              'rf-tracks
              (mapcar #'(lambda (x) (list x nil)) track-names))))
    ;; track names can only be strings so error if not
    (unless (every #'stringp track-names)
      (error "reaper-file::create-tracks: The tracks names of the reaper-items ~
              should all be symbols: ~a" track-names))
    ;; put the items in the assoc-list's respective tracks, one after the
    ;; other. NB these are not ordered by time here but probably are elsewhere,
    ;; though that doesn't seem to be necessary for reaper i.e. items can be
    ;; written in any order: they just need their position in seconds setting
    (loop for item in (data rf) do
         (add-to-list-data item (track item) al))
    ;; using the track sound files' number of channels and the two key args, set
    ;; the necessary number of channels for a given track
    (flet ((get-num-channels (sflist)
             (multiple-value-bind
                   (ch-min ch-max)
                 (loop for sf in sflist
                    minimize (channels sf) into min
                    maximize (channels sf) into max
                    finally (return (values min max)))
               (let ((result
                      ;; so even if we've got a bunch of mono files, in reaper
                      ;; we'll have a miniumum of :min-channels
                      (cond ((<= ch-max min-channels) min-channels)
                            ;; even if we've got a sndfile with 16 channels
                            ;; we'll have a reaper track with :max-channels
                            ((>= ch-max max-channels) max-channels)
                            ;; even if all the sound files have more channels
                            ;; than max-channels, use that but warn.
                            ((> ch-min max-channels)
                             (warn "reaper::create-tracks: the least number of ~
                                    channels in the given ~%sound files is ~a ~
                                    which is > :max-channels (~a)"
                                   ch-min max-channels)
                             max-channels)
                            (t max-channels))))
                 ;; reaper always has an even number of channels.
                 (setq result (if (evenp result) result (1+ result)))
                 (when (> result max-channels)
                   (warn "reaper::create-tracks: reaper can only have even ~
                          numbers of channels: setting to ~a" result))
                 result))))
      ;; update the assoc-list of tracks, where the data is the list of
      ;; reaper-items (above).
      (setf (data al)
            (loop for track in (data al) collect
                 (make-instance 'reaper-track :id (id track) :data (data track)
                                :channels (get-num-channels (data track))
                                :min-channels min-channels
                                :max-channels max-channels)))
      ;; (print al)
      ;; Store the assoc-list of tracks in the tracks slot of the rf.
      (setf (tracks rf) al))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; By default the file will be written in slippery-chicken's 'default-dir using
;;; the ID as file name, but :file will override this.
(defmethod write-reaper-file ((rf reaper-file) &key file)
  (let ((outfile (if file
                     file
                     (default-dir-file (format nil "~a.rpp"
                                               (string-downcase (id rf)))))))
    ;; sort the items into tracks
    (create-tracks rf)
    (with-open-file 
        (out outfile
         :direction :output :if-does-not-exist :create
         :if-exists :rename-and-delete)
      (write-header rf out)
      ;; loop through the tracks and write them
      (loop for track in (data (tracks rf)) do (write-track track out))
      (write-footer rf out))
    outfile))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-reaper-file (id reaper-items &rest keyargs &key &allow-other-keys)
  ;; (print reaper-items)
  (apply #'make-instance (append (list 'reaper-file :id id :data reaper-items)
                                 keyargs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; this is just one example of how reaper-items can be made before being passed
;;; to make-reaper-file. Here we pass a single or list of sndfile paths and a
;;; list of rhythms (as symbols or rhythm/event objects) and create as many
;;; reaper-item objects as there are rhythms, using the sndfiles circularly.
(defun make-reaper-items1 (sndfiles rhythms
                           &key (num-tracks 2) (track-base-name "lispy")
                             (fade-in .005) (fade-out .005)
                             (play-rate 1.0)   ; could also be a list (circular)
                             (input-start 0.0) ; ditto
                             ;; NB this could be different from the reaper-file
                             ;; tempo 
                             (tempo 60.0)
                             (preserve-pitch t))
  (let ((sfs (make-cscl (force-list sndfiles)))
        (play-rates (make-cscl (force-list play-rate)))
        (input-starts (make-cscl (force-list input-start)))
        ;; this allows ties (e.g. e+s) and rests (in parens) and will take care
        ;; of the compound duration if there are ties
        (events (rhythm-list rhythms))
        end)
    ;;           convert the rhythms to events, if necessary, before then
    ;;           generating their start-times
    (setq events (mapcar #'(lambda (r) (sc-change-class r 'event)) events)
          end (nth-value 1 (events-update-time events :tempo tempo))
          ;; we won't need rests or tied-to notes--the latter have, though,
          ;; contributed to the initial tied-from rhythm's compound-duration
          events (remove-if #'(lambda (e) (or (is-rest e) (is-tied-to e)))
                            events)
          ;; reaper needs 1 or 0, not T or NIL (clearly)
          preserve-pitch (if preserve-pitch 1 0))
    (values
     (loop for event in events
        for path = (get-next sfs)
        for i from 0
        collect
          (make-instance 'reaper-item
                         :preserve-pitch preserve-pitch
                         :play-rate (get-next play-rates)
                         :fade-out fade-out :fade-in fade-in
                         :start (get-next input-starts)
                         :start-time (start-time event)
                         ;; so the file name and the rhythm is the name
                         :name (format nil "~a-~a"
                                       (pathname-name path) (data event))
                         :track (format nil "~a-~a"
                                        track-base-name (1+ (mod i num-tracks)))
                         :path path
                         :duration (compound-duration-in-tempo event)))
     ;; we return this too so we can e.g. put the cursor at the end of the last
     ;; event: esp. useful if that was a rest
     end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; a couple of old routines. todo: These could/should be updated to write into
;;; reaper files rather to stdout.
;;; 
;;; ****f* utilities/pdivide-reaper-markers
;;; DESCRIPTION
;;; Write to standard out (terminal/interpreter) marker data for a reaper file
;;; generated from calls to the pdivide function.
;;; 
;;; ARGUMENTS
;;; Takes the same arguments as the pdivide function
;;; 
;;; RETURN VALUE
;;; T
;;; 
;;; SYNOPSIS
(defun pdivide-reaper-markers (&rest args)
;;; ****
  (multiple-value-bind
        (times durations generations)
      (apply #'pdivide args)
    (declare (ignore durations))
    (flet ((find-level (time)
             (loop for g in (reverse generations) and i from 1 do
                  (when (member time g
                                :test #'(lambda (x y)
                                          (equal-within-tolerance x y .001)))
                    (return i)))))
      (loop
         ;; hard-coded colours for now: white for level 1, yellow 2, blue 3,
         ;; red 4  
         with colours = '(33554431 33554176 16777471 0)
         for time in (rest times)
         for level = (find-level time)
         for i from 1
         do
           (format t "~&  MARKER ~a ~,3f \"level ~a\" 0 ~a 1"
                   i time level (nth (min 3 (1- level)) colours)))
      t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/pexpand-reaper-markers
;;; DATE
;;; September 4th 2015, Edinburgh
;;; 
;;; DESCRIPTION
;;; Using the pexpand function, we write marker information in a format which
;;; can be read by the Reaper (version 4/5+) DAW software. Though we can think
;;; of the outputs of pexpand to be in beats, seconds, bars, or any arbitrary
;;; scale, the timings of Reaper markers are in seconds, hence the need here
;;; for an initial tempo. In other words, we treat the output of pexpand to be
;;; beat counts; if you would prefer to interpret these as bars, simply divide
;;; the tempo by the number of beats per bar. If there are to be tempo changes
;;; in the mix/piece Reaper itself will update the markers' positions when the
;;; new tempo is inserted in the project window--this is fine when you are
;;; thinking in beats/bars but beware of changing tempo in Reaper if you are
;;; thinking of markers with fixed timings (seconds).
;;;
;;; Copy the output of this into the Reaper file verbatim (no enclosing < >
;;; marks) before the <PROJBAY tag.
;;; 
;;; ARGUMENTS
;;; - the tempo in BPM
;;; - the number of generations: see the pexpand function
;;; - (&rest) the proportions: see the pexpand function
;;; 
;;; RETURN VALUE
;;; Always T
;;; 
;;; EXAMPLE
#|

(pexpand-reaper-markers 144 2 6 3 5 4)
->
  MARKER 1 7.5 "level 4" 0 0 1
  MARKER 2 15.0 "level 4" 0 0 1
  MARKER 3 22.5 "level 4" 0 0 1
  MARKER 4 30.0 "level 4" 0 0 1
  MARKER 5 37.5 "level 4" 0 0 1
...
  MARKER 108 810.0 "level 1" 0 0 1
  MARKER 109 817.5 "level 4" 0 0 1
  MARKER 110 825.0 "level 4" 0 0 1
  MARKER 111 832.5 "level 4" 0 0 1
  MARKER 112 840.0 "level 4" 0 0 1
  MARKER 113 847.5 "level 4" 0 0 1
  MARKER 114 855.0 "level 3" 0 0 1
...
  MARKER 319 2392.5 "level 4" 0 0 1
  MARKER 320 2400.0 "level 3" 0 0 1
  MARKER 321 2407.5 "level 4" 0 0 1
  MARKER 322 2415.0 "level 4" 0 0 1
  MARKER 323 2422.5 "level 4" 0 0 1


Here's where I pasted the data into the .RPP Reaper file:

  <TEMPOENVEX
    ACT 0
    VIS 1 0 1
    LANEHEIGHT 0 0
    ARM 0
    DEFSHAPE 1 -1 -1
  >
  MARKER 1 7.5 "level 4" 0 0 1
  MARKER 2 15 "level 4" 0 0 1
  MARKER 3 22.5 "level 4" 0 0 1
  MARKER 4 30 "level 4" 0 0 1
...
  MARKER 320 2400 "level 3" 0 0 1
  MARKER 321 2407.5 "level 4" 0 0 1
  MARKER 322 2415 "level 4" 0 0 1
  MARKER 323 2422.5 "level 4" 0 0 1
  <PROJBAY
  >
  <TRACK {EBF9837F-BE25-9542-B720-A1862C0DF380}

|#
;;; SYNOPSIS
(defun pexpand-reaper-markers (tempo generations &rest proportions)
;;; ****
  (loop with pexp = (cddr (apply #'pexpand (cons generations proportions)))
     with beat-dur = (/ 60.0 tempo)
     ;; hard-coded colours for now: white for level 1, yellow 2, blue 3, red 4
     with colours = '(33554431 33554176 16777471 0)
     for beat-num in pexp by #'cddr
     for letters in (rest pexp) by #'cddr
     for level = (length letters)
     for i from 1
     do
       (format t "~&  MARKER ~a ~a \"level ~a\" 0 ~a 1"
               i (* beat-dur (1- beat-num)) level (nth (1- level) colours)))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF
