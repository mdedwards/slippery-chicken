;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* sndfile/reaper-item
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
;;; Version:          1.0.12
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
;;; $$ Last modified:  10:52:29 Wed Oct 18 2023 CEST
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

#|
;;; one simple way of algorithmically generating a reaper file:
(let* ((tempo 240)
       (items
         (make-reaper-items1
          (get-sndfiles
           (concatenate 'string
                        cl-user::+slippery-chicken-home-dir+
                        "tests/test-sndfiles-dir-2"))
          '(e (w) (q) q (h) (e) e. (q.) q (w) e (w) e.)
           tempo
          :input-start '(0 .1 .2)
          :play-rate '(1 1.02 1 .98 1.01 1 1.02)
          :preserve-pitch t))
       ;; NB the tempo of the reaper file is independent of the items
       (rf (make-reaper-file 'reaper-test items :tempo tempo)))
  (write-reaper-file rf))

;;; or to write a reaper file just with markers (at times in seconds)
(write-reaper-file (make-reaper-file 'test nil) :markers '(1 2 3.5 7))
|#

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
   ;; start time is the start slot of the sndfile class (in reaper the SOFFS
   ;; line)  
   (start-time :accessor start-time :type number :initarg :start-time
               :initform 0.0)
   ;; the name visible in the reaper item: by default the sndfile name--as this
   ;; might be used for several objects we shouldn't use the named-object ID
   ;; slot, which is generally but not necesssarily unique (e.g. if used in
   ;; assoc-lists)
   (name :accessor name :initarg :name :initform nil)
   ;; the name of the track to put this item on. If items are passed to
   ;; make-reaper-file then before writing they'll be separated into tracks and
   ;; written into those. This can be any string though of course it makes sense
   ;; to put more than one item on a single track
   (track :accessor track :type string :initarg :track
          :initform "reaper-lisp")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; a holder for reaper-items that will all be placed on a single user-named
;;; reaper track. The reaper-items are in the data slot; the ID slot will
;;; determine the track name in reaper (lower case).
;;; ****c* sclist/reaper-track
(defclass reaper-track (sclist)
;;; ****
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
;;; ****c* sclist/reaper-file
(defclass reaper-file (sclist)
;;; ****
  ;; for the project settings
  ((record-path :accessor record-path :type string :initarg :record-path
                :initform "/tmp/")
   (samplerate :accessor samplerate :type integer :initarg :samplerate
               :initform 44100)
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod initialize-instance :after ((ri reaper-item) &rest initargs)
  (declare (ignore initargs))
  ;; if there's no name, use the file name (minus dir and extension) as the
  ;; name.
  ;; trigger the setf method
  (setf (track ri) (track ri))
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
    (setf (slot-value sndfile 'istring) (istring ri)
          (slot-value sndfile 'fade-in) (fade-in ri)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; write an item's data to a stream, using the istring slot as a template
(defmethod write-item ((ri reaper-item) stream)
  (unless (path ri)
    (error "reaper-item::write-item: the path slot is required."))
  ;; start: SOFFS, duration: LENGTH
  (format stream (istring ri) (start-time ri) (duration ri) (fade-in ri)
          (fade-out ri) (name ri) (start ri) (play-rate  ri)
          (preserve-pitch ri)
          (os-format-path (path ri) 
                          (if (get-sc-config 'reaper-files-for-windows)
                              'windows
                              'unix))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod (setf track) :around (track (ri reaper-item))
  (call-next-method
   (typecase track
     (number (format nil "track~a" track))
     (string track)
     (t (error "reaper-item::setf track: track should be a number or string: ~a"
               track)))
      ri))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; write all items in a track to a reaper stream, using the tstring as template
(defmethod write-track ((rt reaper-track) stream)
  (format stream (tstring rt) (string-downcase (string (id rt))) (channels rt))
  (loop for item in (data rt) do (write-item item stream))
  (format stream "~&  >"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; write the reaper header to a stream using the given slots for project
;;; settings 
(defmethod write-header ((rf reaper-file) stream master-channels)
  (format stream (header rf) (cursor rf) (zoom rf) (record-path rf)
          (samplerate rf) (samplerate rf) (bpm (tempo rf)) (num (time-sig rf))
          (denom (time-sig rf)) master-channels))

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
  (when (> min-channels max-channels)
    (setq max-channels min-channels))
  ;; items can of course use any name in their track slot, but get all unique
  ;; names here as this will determine how many tracks are written.
  (let* ((track-names (sort (remove-duplicates (mapcar #'track (data rf))
                                               :test #'string=)
                            #'string<))
         ;; create an assoc-list using the track names as IDs and empty lists
         ;; (to cons into) with the items further down
         (al (make-assoc-list
              'rf-tracks
              (mapcar #'(lambda (x) (list x nil)) track-names))))
    ;; (print track-names)
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
                 (setq result (if (evenp result)
                                  result
                                  (progn
                                    (warn "reaper::create-tracks: reaper ~
                                           can only have even numbers of ~
                                           channels: setting to ~a"
                                          result)
                                    (1+ result))))
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
      (setf (tracks rf) al)
      rf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; By default the file will be written in slippery-chicken's 'default-dir using
;;; the ID as file name, but :file will override this.
(defmethod write-reaper-file ((rf reaper-file)
                              &key file markers
                                   (min-channels 2) (max-channels 4))
  (let ((outfile (if file
                     file
                     (default-dir-file (format nil "~a.rpp"
                                               (string-downcase (id rf)))))))
    ;; sort the items into tracks unless this has already been done
    (unless (tracks rf) 
      (create-tracks rf :min-channels min-channels :max-channels max-channels))
    (with-open-file 
        (out outfile
             :direction :output :if-does-not-exist :create
             :if-exists :rename-and-delete)
      (write-header rf out (max min-channels max-channels))
      ;; MDE Sun Sep 25 17:56:30 2022, Heidhausen -- reaper v6.64 at least
      ;; writes markers before <PROJBAY> (the last entry in our header file) but
      ;; doesn't complain when they come afterwards
      (when markers
        ;; these are either a list of times (in seconds) or a list of sublists
        ;; with data in the order we'd supply to write-reaper-marker
        (loop for m in markers and i from 1 do
                 (if (numberp m)
                     (write-reaper-marker i m "" out)
                     (let* ((len (length m))
                            (args (if (> len 3)
                                      (append (subseq m 0 3) (list out)
                                              (last m))
                                      (econs m out))))
                       (apply #'write-reaper-marker args)))))
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
(defun make-reaper-item (path &rest keyargs &key &allow-other-keys)
  (apply #'make-instance (append (list 'reaper-item :path path)
                                 keyargs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; for make-reaper-items-aux. rhythms could be a rthm-seq too
(defun make-reaper-items-process-rhythms (rhythms tempo
                                          &optional (just-attacks t))
  (when (rthm-seq-p rhythms)
    ;; rthm-seqs don't update their compound-duration slots rather that happens
    ;; in handle-ties at the make-piece level. 
    (setq rhythms (get-rhythm-list rhythms)))
  ;; this allows ties (e.g. e+s) and rests (in parens) and will take care
  ;; of the compound duration if there are ties
  (let ((events (rhythm-list (force-list rhythms)))
        end)
    ;; convert the rhythms to events, if necessary, before then generating their
    ;; start-times
    (setq events (mapcar #'(lambda (r) (sc-change-class r 'event)) events)
          end (nth-value 1 (events-update-time events :tempo tempo)))
    ;; we won't need rests or tied-to notes--the latter have, though,
    ;; contributed to the initial tied-from rhythm's compound-duration
    (when just-attacks
      (setq events (just-attacks events)))
    (values events end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; for use in make-reaper-items1 etc. Here the first two arguments are atoms or
;;; fixed lists: the first list to exit the loop stops the process 
(defun make-reaper-items-aux (sndfiles events end-time
                              &key (num-tracks 2)
                                (track-base-name "reaper-lisp")
                                (fade-in .005) (fade-out .005)
                                ;; could also be a list (circular)
                                (play-rate 1.0)
                                ;; if an event's duration is longer than the
                                ;; sndfile we'll get a warning but by default
                                ;; we'll force its duration
                                (force-duration t)
                                (input-start 0.0) ; ditto
                                (preserve-pitch t))
  ;; reaper needs 1 or 0, not T or NIL (clearly)
  (setq preserve-pitch (if preserve-pitch 1 0))
  (let ((sfs (force-list sndfiles))
        (play-rates (make-cscl (force-list play-rate)))
        (input-starts (make-cscl (force-list input-start))))
    ;; MDE Sat Mar  5 15:33:43 2022, Heidhausen -- if no events are passed we
    ;; just use the duration of the sndfile
    (unless events (setq events (ml nil (length sfs))))
    (values
     (loop for event in events
           for path in sfs
           for i from 0
           for dur = (when event
                       (if (compound-duration-in-tempo event)
                         (compound-duration-in-tempo event)
                         ;; MDE Sat Mar 5 15:25:36 2022, Heidhausen -- in case
                         ;; we've created an event using :duration t
                         (compound-duration event)))
           for ri = (make-instance
                     'reaper-item
                     :preserve-pitch preserve-pitch
                     :play-rate (get-next play-rates)
                     :fade-out fade-out :fade-in fade-in
                     :start (get-next input-starts)
                     :start-time (if event (start-time event) 0.0)
                     ;; so the file name and the rhythm is the
                     ;; item name
                     :name (format nil "~a-~a"
                                   (pathname-name path)
                                   (if event (data event) "?"))
                     :track (format nil "~a-~3,'0d" track-base-name
                                    (1+ (mod i num-tracks)))
                     :path path
                     :duration dur)
           ;; override sndfile::update to allow the duration to be longer than
           ;; the sndfile so we get looping in reaper
           do (when (and dur force-duration (< (duration ri) dur))
                (setf (slot-value ri 'duration) dur))
           collect ri)
     ;; we return this too so we can e.g. put the cursor at the end of the
     ;; last event: esp. useful if that was a rest
     end-time)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; this is just one example of how reaper-items can be made before being passed
;;; to make-reaper-file. Here we pass a single or list of sndfile paths and a
;;; list of rhythms (as symbols or rhythm/event objects) and create as many
;;; reaper-item objects as there are attacked rhythms, using the sndfiles
;;; circularly. NB tempo could be different from the reaper-file tempo.
(defun make-reaper-items1 (sndfiles rhythms tempo
                           &rest keyargs &key &allow-other-keys)
  (multiple-value-bind
        (events end)
      (make-reaper-items-process-rhythms rhythms tempo)
    (let ((sfs (loop with sfs = (make-cscl (force-list sndfiles))
                     repeat (length events)
                     collect (get-next sfs))))
      (apply #'make-reaper-items-aux (append (list sfs events end) keyargs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; similar but this time the length of the sndfiles determines the number of
;;; reaper items and the rhythms are used circularly
(defun make-reaper-items2 (sndfiles rhythms tempo
                           &rest keyargs &key &allow-other-keys)
  (multiple-value-bind
        (events end)
      (make-reaper-items-process-rhythms rhythms tempo nil)
    (let ((all-events (loop with elist = (make-cscl events)
                            with count = 0
                            with end = (length sndfiles)
                            until (= count end)
                            for e = (clone (get-next elist))
                            do (when (needs-new-note e) (incf count))
                            collect e)))
      ;; because we might have circularly used events, we have to update the
      ;; times again
      (events-update-time all-events :tempo tempo)
      (apply #'make-reaper-items-aux
             (append (list sndfiles (just-attacks all-events) end) keyargs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; here we just lay one sndfile after another, with or without a gap i.e. there
;;; are no rhythms/events 
(defun make-reaper-items3 (sndfiles gap ; seconds
                           &rest keyargs &key &allow-other-keys)
  (let ((items (apply #'make-reaper-items-aux
                      ;; cursor will be at 0.0
                      (append (list sndfiles nil 0.0) keyargs)))
        (time 0.0))
    ;; must set start-times by hand now, as we had no rhythms
    (loop for item in items do
      (setf (start-time item) time)
      (incf time (+ gap (duration item))))
    items))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun write-reaper-marker (number time label &optional (stream t) (colour 0))
  (when (symbolp colour)
    (setq colour (case colour
                   (white 33554431)
                   (yellow 33554176)
                   (blue 16777471)
                   (red 0)
                   (t (error "write-reaper-marker: unknown colour: ~a"
                             colour)))))
  (format stream "~&  MARKER ~a ~,3f \"~a\" 0 ~a 1"
                   number time label colour))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* reaper/reaper-layer-sounds
;;; DATE
;;; October 1st 2022
;;; 
;;; DESCRIPTION
;;; Given a list of paths to sound files, create a reaper file that layers/mixes
;;; these together. We create as many mixes as possible given the number of
;;; tracks required, e.g. if there are 20 sound files and <num-tracks> is 4,
;;; then five mixes will be created, one after the other, with a gap of 10
;;; seconds by default. So the files are mixed together in the order of the
;;; first argument, with the longest file in the mix determining the overall
;;; duration and the shorter files beginning after a wait of <indent> multiplied
;;; by the difference between the longest and shorter files'
;;; durations. E.g. with a longest file duration of 60 seconds and a shorter
;;; file of 30 seconds, given :indent 0.5 the shorter file's start time would be
;;; 15 seconds; if its duration was 20 seconds, the start time would be 20.
;;;
;;; In case sound files are passed in alphabetical order, e.g. by
;;; (get-sndfiles...), and assuming that it is undesireable that similarly-named
;;; sound files are placed one after the other, by default the function will
;;; shuffle the order of the mixes (i.e. the groups of 4 or whatever sound files
;;; per mix). NB the 4-groups are shuffled, not the files in the groups.
;;; 
;;; ARGUMENTS
;;; - a list of sound file paths (strings)
;;; - the number of tracks that should be layered/mixed
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; :tempo. The BPM value that should be written to the reaper project, if
;;; desired. Default = 60.
;;; :max-fade. The maximum length in seconds of a fade-in or fade-out. If this
;;; is too long for any given sound file, the :mid-fade multiplied by the sound
;;; file duration will be used. Default = 15 seconds.
;;; :min-fade. The proportion of a sound file's duration that should be used if
;;; :max-fade can't be used for both fade-in and fade-out. This should generally
;;; be < 0.5 but is not enforced. Default = 0.5.
;;; :reaper-file. The path of the reaper-file to be written. If this is NIL then
;;; no file is written and the reaper-file object is returned instead, with all
;;; mixes present. This could be used to add more tracks before writing the
;;; reaper file (see example below). Default = "/tmp/reaper-layer-sounds.rpp"
;;; :min-channels. The minimum number of a channels a reaper track should
;;; have. Usually this will be set to be the maximum number of channels any of
;;; the files on a single track have. But if you have a bunch of stereo files
;;; you want to place on, say, a track which should become 5th order ambisonics,
;;; you can set this to 36. Note that all reaper tracks should have an even
;;; number of channels so however the channel number is achieved it will be
;;; rouned up to the nearest even number if necessary. Default = 2.
;;; :max-channels. This can be set to override the number of channels of any
;;; given sound file, should it have more channels than you want, for some
;;; reason. Default = 4.
;;; :gap. The gap in seconds between the end of the previous mix and the start
;;; of the next. Default = 10 seconds.
;;; :shuffle. Whether to shuffle the mixes before writing. See above for
;;; details. Default = T.
;;; :indent. How far in to start the shorter sounds, as a function of their
;;; duration difference to the longest file (see above). 0.5 would have them
;;; centered bang in the middle. By default the golden mean, 0.618034 (what
;;; else?) i.e. (/ (- (sqrt 5) 1) 2).
;;; 
;;; RETURN VALUE
;;; If :reaper-file is given, then the path will be returned after writing,
;;; otherwise a reaper-file object is returned.
;;; 
;;; EXAMPLE
#|

(let ((overlaps (reaper-overlap-sounds
                 (get-sndfiles "/Volumes/slim500/snd/samples/ambience/rain") 
                 :reaper-file nil))
      (layers (reaper-layer-sounds (get-sndfiles "~/ic/projects/mete/clm") 4
                                   :reaper-file nil)))
  (add (get-first (tracks overlaps)) (tracks layers))
  (write-reaper-file layers :min-channels 36
                     :file "~/ic/projects/mete/reaper/mete-clm-with-rain.rpp"))

|#
;;; SYNOPSIS
(defun reaper-layer-sounds (sndfiles num-tracks
                            &key (tempo 60)
                              (max-fade 15) ; seconds
                              (min-fade .4) ; proportion of duration
                              (reaper-file "/tmp/reaper-layer-sounds.rpp")
                              ;; e.g. if you want all tracks to be 26 channels
                              ;; despite the number of channels in the sound
                              ;; files set this here
                              (min-channels 2)
                              (max-channels 4)
                              ;; gap in seconds between groups
                              (gap 10)
                              ;; shuffle (fixed-seed) the subgroups so that the
                              ;; sound files are not simply processed by
                              ;; directory order?
                              (shuffle t)
                              (indent 0.618034))
;;; ****
  (let* ((subgroups
           (split-into-sub-groups2 sndfiles num-tracks shuffle))
         (time 0.0)
         rf)
    ;; order the files by duration so we can spread by duration
    (setq subgroups (loop for subgrp in subgroups
                          ;; turn the sublist of four sndfile paths into four
                          ;; reaper-items then sort them
                          collect (sort (mapcar #'make-reaper-item subgrp) #'>
                                        :key 'duration)))
    (loop for subgrp in subgroups
          for ri1 = (first subgrp) ; the longest
          for ri1dur = (duration ri1) do
            (setf (start-time ri1) time)
            (loop for ri in subgrp
                  for fade = (min max-fade (* (duration ri) min-fade))
                  for track from 1 do
                    (setf (track ri) track
                          (fade-in ri) fade
                          (fade-out ri) fade)
                    (when (> track 1)
                      (setf (start-time ri)
                            ;; 'indent' shorter sndfiles
                            (+ time (* indent (- ri1dur (duration ri)))))))
            (incf time (+ gap ri1dur)))
    ;; NB the tempo of the reaper file is independent of the items
    (setq rf (make-reaper-file 'layer-sounds (flatten subgroups) :tempo tempo))
    (if reaper-file 
        (write-reaper-file rf :min-channels min-channels
                              :file reaper-file)
        ;; if :reaper-file is nil just return the reaper-file object so that
        ;; e.g. more tracks can be added
        (create-tracks rf :min-channels min-channels
                          :max-channels max-channels))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* reaper/reaper-overlap-sounds
;;; DATE
;;; October 1st 2022.
;;; 
;;; DESCRIPTION
;;; Create a reaper file with one track that overlaps/cross-fades all the given
;;; sound files.
;;; 
;;; ARGUMENTS
;;; - a list of sound files paths (strings) to overlap
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; 
;;; :overlap. The proportion by which to overlap the sound files. Given that
;;; sound files can have any duration and be overlapped in any order, this
;;; proportion is applied to the shortest of any two sound files to be
;;; cross-faded. default = 0.5.
;;; :reaper-file. The path of the reaper-file to be written. If this is NIL then
;;; no file is written and the reaper-file object is returned instead, with all
;;; mixes present. This could be used to add more tracks before writing the
;;; reaper file (see example in reaper-layer-sounds). Default =
;;; "/tmp/reaper-layer-sounds.rpp"
;;; :min-channels. See reaper-layer-sounds. Default = 2.
;;; :max-channels. See reaper-layer-sounds. Default = 4.
;;; :track-name. The string used to name the track in reaper. Default =
;;; "overlaps"
;;; :tempo. The BPM value that should be written to the reaper project, if
;;; desired. Default = 60.
;;; 
;;; RETURN VALUE
;;; If :reaper-file is given, then the path will be returned after writing,
;;; otherwise a reaper-file object is returned. See reaper-layer-sounds for an
;;; example.
;;; 
;;; SYNOPSIS
(defun reaper-overlap-sounds (sndfiles &key (tempo 60) (min-channels 2)
                                         (max-channels 4) (overlap .5)
                                         (track-name "overlaps")
                                         (reaper-file
                                          "/tmp/reaper-layer-sounds.rpp"))
;;; ****
  (let ((items (mapcar #'make-reaper-item sndfiles))
        (time 0.0)
        (overlap-dur 0.0)
        rf)
    (loop for ri1 in items
          for ri2 in (econs (rest items) nil)
          for d1 = (duration ri1)
          for d2 = (if ri2 (duration ri2) 0) do
            (setf (start-time ri1) time
                  (track ri1) track-name
                  (fade-in ri1) overlap-dur)
            ;; the if takes care of whether the first sndfile is longer than
            ;; the 2nd other vice-versa
            (setf overlap-dur (* overlap (if (> d1 d2) d2 d1))
                  (fade-out ri1) overlap-dur)
            (incf time (- d1 overlap-dur)))
    (setq rf (make-reaper-file 'overlap-sounds items :tempo tempo))
    (if reaper-file 
        (write-reaper-file rf :min-channels min-channels
                              :file reaper-file)
        ;; if :reaper-file is nil just return the reaper-file object so that
        ;; e.g. more tracks can be added
        (create-tracks rf :min-channels min-channels
                          :max-channels max-channels))))

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
           (write-reaper-marker i time level t
                                (nth (min 3 (1- level)) colours)))
      ;; (format t "~&  MARKER ~a ~,3f \"level ~a\" 0 ~a 1"
      ;;    i time level (nth (min 3 (1- level)) colours)))
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
        ;; hard-coded colours for now: white for level 1, yellow 2, blue 3,
        ;; red 4
        with colours = '(33554431 33554176 16777471 0)
        for beat-num in pexp by #'cddr
        for letters in (rest pexp) by #'cddr
        for level = (length letters)
        for i from 1
        do
           (write-reaper-marker i (* beat-dur (1- beat-num)) level t
                                (nth (1- level) colours)))
  ;; (format t "~&  MARKER ~a ~a \"level ~a\" 0 ~a 1"
  ;; i (* beat-dur (1- beat-num)) level (nth (1- level) colours)))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF
