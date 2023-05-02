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
;;; $$ Last modified:  21:08:09 Mon Oct  3 2022 CEST
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

;; TODO: init-volume auf 1??

;; load a regex library
;; TODO: what if no quickload?
;; out it in "src/all.lsp"
;;(ql:quickload "cl-ppcre")

;; this is the (partially binary) data for the respective plugin, as it appears
;; in a reaper project file:
(defparameter *iem-stereo-encoder*
  (read-file (file-from-sc-dir "src/iem-stereo-encoder.txt")))
(defparameter *blue-ripple-decoder*
  (read-file (file-from-sc-dir "src/blue-ripple.txt")))

#|
;; only thouch these if you know what you're doing:
(defparameter *spatial-reaper-tempo* 60)
;; if nil - use individual-duration
;; if > 0 - set every env to this duration :duration t
;; if < 0 - end at (abs) of this time :end-times nil
(defparameter *spatial-reaper-duration*  nil)
(defparameter *spatial-reaper-use-starts* nil)
|#

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
;;; Leon Focker:
;;; like sndfile, but with envelopes for elevation and angle
;;; ****c* sndfile/spatial-sndfile
(defclass reaper-envelope (sclist)
  ;; the string that will be printed in the reaper file
  (;;(reaper-automation-data :accessor reaper-automation-data :initform nil)
   (env-type :accessor env-type :initform 'volume :initarg :env-type)
   (parameter-slot :accessor parameter-slot :type integer
		   :initarg :parameter-slot :initform 0)
   (parameter-min :accessor parameter-min :initarg :parameter-min :type number
		  :initform 0)
   (parameter-max :accessor parameter-max :initarg :parameter-max :type number
		  :initform 1)
   (env-string :accessor env-string :type string :initarg :env-string
	      :initform (read-file (file-from-sc-dir "src/reaper-env.txt")))))

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
     (track-volume :accessor track-volume :initarg :track-volume :initform .5
		  :type number)
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
   (sample-rate :accessor sample-rate :type integer :initarg :sample-rate
	       :initform 44100)
   (time-sig :accessor time-sig :initarg :time-sig :initform '(4 4))
   (tempo :accessor tempo :type number :initarg :tempo :initform 60)
   ;; zoom factor for the time line. At the moment this gives me about 80
   ;; seconds overview
   (zoom :accessor zoom :type number :initarg :zoom :initform 20)
   ;; number of channels every track has. If nil, this number is automatically
   ;; decided for each track
   (n-channels :accessor n-channels :initarg :n-channels :initform nil
	       :type integer)
   (master-volume :accessor master-volume :initarg :master-volume :initform .5
		  :type number)
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
;;; write a reaper-envelope object into a string that can be printed into a
;;; reaper file.
;;; start-time and end-time are - obviously - when the envelope will start and
;;; end. TODO: tempo or seconds??
(defun write-reaper-envelope-aux (env-type)
  (case env-type
    ((volume-pre volume-pre-fx vol-pre vol-pre-fx)
     "VOLENV")
    ((volume vol) "VOLENV2")
    ((volume-trim trim-volume vol-trim trim-vol)
     "VOLENV3")
    ((panning-pre panning-pre-fx pan-pre pan-pre-fx)
     "PANENV")
    ((panning pan) "PANENV2")
    ((width-pre with-pre-fx) "WIDTHENV")
    (width "WIDTHENV2")
    (mute "MUTEENV")
    (tempo "TEMPOENVEX")
    ((auxvolume auxvol aux-volume aux-vol)
     "AUXVOLENV")
    ((auxpanning auxpan aux-panning aux-pan)
     "AUXPANENV")
    ((auxmute aux-mute) "AUXMUTEENV")
    (t "PARMENV")))

(defmethod write-reaper-envelope ((env reaper-envelope) stream start-time end-time)
  (format stream (env-string env)
	  (format nil
		  (if (is-parmenv (env-type env)) "~a ~a ~a ~a" "~a ~a ~a")
		  (write-reaper-envelope-aux (env-type env))
		  (if (is-parmenv (env-type env)) (parameter-slot env) "")
		  (or (parameter-min env) "")
		  (or (parameter-max env) ""))
	  (generate-reaper-id)
	  (generate-automation-data env start-time end-time)))

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
  (format stream (tstring rt) (string-downcase (string (id rt))) (track-volume rt)
	  (channels rt))
  (loop for item in (data rt) do (write-item item stream))
  (format stream "~&  >"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; write the reaper header to a stream using the given slots for project
;;; settings 
(defmethod write-header ((rf reaper-file) stream master-channels)
  (format stream (header rf) (cursor rf) (zoom rf) (record-path rf)
	  (sample-rate rf) (sample-rate rf) (bpm (tempo rf)) (num (time-sig rf))
	  (denom (time-sig rf)) master-channels (master-volume rf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod write-footer ((rf reaper-file) stream)
  (declare (ignore rf)) ; though we might need some slots later, hence method
  (format stream "~%  <EXTENSIONS~%  >~%>~%"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; create the reaper-track instances to hold and later write the instances that
;;; should appear together on one track
;;; all the reaper-items are stored in the data slot, no matter what track they
;;; should be written to. Sort these now.
;;; channels is initially nil, when an integer, set the number of channels
;;; of all tracks to this number.
;;; track-volumes can be a number or a list of numbers, that the volumes of the
;;; tracks is set to. If the list is shorter than the umber of tracks created,
;;; it is looped.
(defmethod create-tracks ((rf reaper-file)
                          &key (min-channels 2) (max-channels 4)
			    channels track-volumes)
  ;; make sure channels is multiple of 2
  (when channels (setf channels (if (evenp (round channels))
				    (round channels)
				    (1+ (round channels)))))
  (unless (listp track-volumes)
      (setf track-volumes (list track-volumes)))
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
            (loop for i from 0 and track in (data al) collect
                 (make-instance 'reaper-track :id (id track) :data (data track)
                                :channels (or channels (get-num-channels
							(data track)))
				:track-volume (nth (mod i (length
							   track-volumes))
						   track-volumes)
                                :min-channels min-channels
                                :max-channels max-channels)))
      ;; (print al)
      ;; Store the assoc-list of tracks in the tracks slot of the rf.
      (setf (tracks rf) al)
      rf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; By default the file will be written in slippery-chicken's 'default-dir using
;;; the ID as file name, but :file will override this.
;;; channels is initially nil, when an integer, set the number of channels
;;; of all tracks (also master-track) to this number.
(defmethod write-reaper-file ((rf reaper-file)
                              &key file markers
                                (min-channels 2) (max-channels 4))
  ;; make sure channels is multiple of 2
  (when (n-channels rf)
    (setf (n-channels rf) (if (evenp (n-channels rf))
			      (n-channels rf)
			      (1+ (n-channels rf)))))
  (let ((outfile (if file
                     file
                     (default-dir-file (format nil "~a.rpp"
                                               (string-downcase (id rf)))))))
    ;; sort the items into tracks unless this has already been done
    (unless (tracks rf) 
      (create-tracks rf :min-channels min-channels :max-channels max-channels
		     :channels (n-channels rf)))
    (with-open-file 
        (out outfile
         :direction :output :if-does-not-exist :create
         :if-exists :rename-and-delete)
      (write-header rf out (or (n-channels rf) (max min-channels max-channels)))
      ;; MDE Sun Sep 25 17:56:30 2022, Heidhausen -- reaper v6.64 at least
      ;; writes markers before <PROJBAY> (the last entry in our header file) but
      ;; doesn't complain when they come afterwards
      (when markers
        ;; these are either a list of times (in seconds) or a list of sublists
        ;; with data in the order we'd supply to write-reaper-marker
        (loop for m in markers and i from 1 do
          (if (numberp m)
              (write-reaper-marker i m "" out)
              (apply #'write-reaper-marker m))))
      ;; loop through the tracks and write them
      (loop for track in (data (tracks rf)) do (write-track track out))
      (write-footer rf out))
    outfile))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;; generate string with random numbers in style:
;; "{B1E049E1-FA47-6E73-1EEE-C0817D973E48}"
(defun generate-reaper-id ()
  (format nil "{~8,'0x-~4,'0x-~4,'0x-~4,'0x-~12,'0x}"
	  (random (expt 16 8)) (random (expt 16 4)) (random (expt 16 4))
	  (random (expt 16 4)) (random (expt 16 12))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modulo for envelope curves, using linear interpolation to create
;; intermedeate points.
;; TODO: DOC
(defun env-mod-aux (last-x last-y x y min max)
  (let* ((yfloordiff (- (floor y) (floor last-y)))
	 (pos? (> yfloordiff 0))
	 (ydiff (- y last-y))
	 (xdiff (- x last-x)))
    (loop for i from 0 below (abs yfloordiff)
       for mult = 2 then 1
       for until-crossing = (+ i (if pos? (- 1 (mod last-y 1)) (mod last-y 1)))
       for test = (= 0 until-crossing)
       for new-x = (+ last-x (abs (* (/ until-crossing ydiff) xdiff)))
       unless test collect new-x unless test collect
	 (rescale (if pos? .9999999 0) 0 1 min max #'error #'rationalize)
       collect (+ new-x (* .00001 mult)) collect
	 (rescale (if pos? 0 .9999999) 0 1 min max #'error #'rationalize))))
       
(defun env-mod (envelope &optional (min 0) (max 1))
  (unless (listp envelope)
    (error "envelope in env-mod should be a list but is ~a" envelope))
  (flatten
   (loop for x in envelope by #'cddr and y in (cdr envelope) by #'cddr
      with last-x = (first envelope) with last-y = (second envelope)
      for fy1 = (floor last-y) for fy2 = (floor y)
      when (and (= 0 (mod y 1)) (not (= 0 y))) do (setf y (- y .0000001))
      when (< x last-x)
      do (warn "env-mod encountered am envelope with decreasing x-value")
      unless (= fy1 fy2) collect (env-mod-aux last-x last-y x y min max)
      collect x collect (rescale (mod y 1) 0 1 min max #'error #'rationalize)
      do (setf last-x x last-y y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; parse the data from a reaper-envelope object into reaper automation
;;; data (string)
(defun generate-automation-data (env start-time end-time)
  (let* ((dur (- end-time start-time))
	 (env-ls (env-mod (data env)))
	 (env-len (loop for x in env-ls by #'cddr finally (return x)))
	 ;;(len (rational (* dur (/ *spatial-reaper-tempo* 60))))
	 (min (parameter-min env))
	 (max (parameter-max env))
	 points)
    (setf points
	  (loop for x in env-ls by #'cddr and y in (cdr env-ls) by #'cddr
	     collect
	       (format nil "PT ~a ~a 0 0"
		       (float (+ (* (/ x env-len) dur) start-time))
		       (rescale (mod y 1.0) 0 1 min max #'rational))))
    points))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun is-parmenv (env-type)
  (some #'(lambda (x) (equal env-type x))
	'(volume volume-pre-fx trim-volume pan pan-pre-fx width width-pre-fx
	  mute tempo aux-volume aux-pan aux-mute parmenv parm-env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; env-type can be one of: volume, volume-pre-fx, trim-volume, pan, pan-pre-fx,
;;; width, width-pre-fx, mute, tempo, aux-volume, aux-pan, aux-mute or parmenv.
;;; Any other type will create a parameter-env (parmenv), which is an envelope
;;; for a parameter in a plugin, which is determined by parameter-slot.
(defun make-reaper-envelope (env &key (env-type 'volume)
				   parameter-slot
				   parameter-min
				   parameter-max)
  (setf env-type (intern (string env-type) :sc))
  (unless (is-parmenv env-type)
    (warn "env-type ~a not supported, set to parmenv" env-type)
    (setf env-type 'parmenv))
  (case env-type
    ((panning-pre panning-pre-fx pan-pre pan-pre-fx panning pan auxpanning
		  auxpan aux-panning aux-pan)
     (unless parameter-min (setf parameter-min -1))
     (unless parameter-max (setf parameter-max 1)))
    (t
     (unless parameter-min (setf parameter-min 0))
     (unless parameter-max (setf parameter-max 1))))
  (make-instance 'reaper-envelope
		 :data env
		 ;;:reaper-automation-data (generate-automation-data env)
		 :env-type env-type
		 :parameter-slot parameter-slot
		 :parameter-min parameter-min
		 :parameter-max parameter-max))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; write the neccessary plugin data for reaper to a stream
(defun write-plugin (plugin-binary-string stream)
  (format stream
	  (read-from-file (file-from-sc-dir "src/reaper-fx.txt"))
	  plugin-binary-string
	  (generate-reaper-id)))

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
;; every soundfile gets its own track, start-times are given individually in
;; start-times. Start-times are looped, if list is shorter than sndfiles
(defun make-reaper-items4 (sndfiles &optional (start-times '(0)))
  (let ((items (apply #'make-reaper-items-aux
			      (list sndfiles nil 0.0
				    :num-tracks (length sndfiles)
				    :fade-in 0.0
				    :fade-out 0.0))))
    ;; must set start-times by hand now, as we had no rhythms
    (loop for item in items and i from 0 do
      (setf (start-time item) (nth (mod i (length start-times)) start-times)))
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
        (create-tracks rf :min-channels min-channels :max-channels max-channels))))

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
        ;; hard-coded colours for now: white for level 1, yellow 2, blue 3, red 4
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
;; "POST GENERATIONAL EDITING OF REAPER FILES"
;; SPATIAL REAPER

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editing files on disk:

;; wrapper for the post-generational editing functions below
;; file is the path to the .rpp file you want to edit
;; var can be any symbol, the same symbol must be put in the "string" argument
;; of the editing functions in body
(defmacro edit-file (file var &body body)
  `(let* ((,var (read-file ,file)))
     (setf ,@(loop for i in `,body collect `,var collect i))
     (with-open-file 
	      (out ,file :direction :output :if-exists :rename-and-delete)
	    (princ ,var out))
     (format t "~&succesfully edited ~a" ,file)))

(defun set-master-channels (string &optional (number-of-master-channels 16))
  (let* ((scan (ppcre:create-scanner "MASTER_NCH [0-9]+")))
    (ppcre:regex-replace-all scan string
			     (format nil "MASTER_NCH ~a"
				     number-of-master-channels))))

(defun set-track-channels (string &optional (number-of-track-channels 16))
  (let* ((scan (ppcre:create-scanner "NCHAN [0-9]+")))
    (ppcre:regex-replace-all scan string
			     (format nil "NCHAN ~a"
				     number-of-track-channels))))

(defun set-all-faders (string &optional (set-to .5))
  (let* ((scan (ppcre:create-scanner "AUTOMODE ..{2,6}?VOLPAN [0-9]+?\[.]?[0-9]*"
				     :single-line-mode t)))
    (ppcre:regex-replace-all scan string
			     (format nil "AUTOMODE 0~&    VOLPAN ~a"
				     set-to))))

;;; TODO DOC
;;; track-nr 0 is master
;;; pre nil - insert after other plugins, t - before other plugins
(defun insert-plugin (string plugin-binary-string track-nr &optional (pre nil))
  (unless (stringp string)
    (error "string in insert-plugin must be a string but is: ~a" string))
  (unless (numberp track-nr)
    (error "track-nr in insert-plugin must be a number but is: ~a" track-nr))
  (let* ((master? (>= 0 track-nr))
	 ;; different scans when or master or other track:
	 (place (if master?
		    "MASTER_SEL [0-9]+"
		    ;; TODO: detect whitespaces and newlines instead of .{1,5}?
		    "MAINSEND [0-9]+ [0-9]+"))
	 (fx (if master? "MASTERFXLIST" "FXCHAIN"))
	 (find-fx-chain (ppcre:create-scanner place :single-line-mode t))
	 (fx-exist (ppcre:create-scanner fx :single-line-mode t))
	 (fx-exist? nil)
	 (n 0)
	 track
	 (track-start 0)
	 (track-end 0))
    ;; find the right track
    (ppcre:do-matches (st nd find-fx-chain string)
      (when (or master? (= (1- track-nr) n))
	(setf track (subseq string st nd)
	      track-start st
	      track-end nd))
      (incf n))
    ;; are any effects already on the track?
    (setf fx-exist? (ppcre:scan fx-exist track))
    ;; insert:
    (unless track (warn "no track with number ~a found" track-nr))
    (when track
      (unless fx-exist?
	(setf track
	      (ppcre:regex-replace
	       find-fx-chain
	       track
	       (if master?
		   (format
		    nil
		    "MASTER_SEL 0~&~a"
		    (read-from-file
		     (file-from-sc-dir "src/reaper-masterfxlist.txt")))
		   (format
		    nil
		    "MAINSEND 1 0~&~a"
		    (read-from-file
		     (file-from-sc-dir "src/reaper-trackfxchain.txt")))))))
      (setf track
	    (if (or pre (not fx-exist?))
		(ppcre:regex-replace
		 "DOCKED [0-9]+"
		 track
		 (format
		  nil
		  "DOCKED 0~&~a"
		  (write-plugin plugin-binary-string nil)))
		(ppcre:regex-replace
		 "WAK [0-9]+ [0-9]+"
		 track
		 (format
		  nil
		  "WAK 0 0~&~a"
		  (write-plugin plugin-binary-string nil))))))
    ;; write with new track
    (with-output-to-string (out)
      (write-string string out :end track-start)
      (when track (write-string track out))
      (write-string string out :start track-end))))

;;; TODO DOC
;;; reaper-envelope must be a reaper-envelope object.
;;; TODO: make master trackvisible when env
;;; TODO: function to change name to first item on track
;;; TODO: insert on track by name
;;; TODO: Master-non-parmenvs have different names (mastervol2)
(defun insert-envelope (string reaper-envelope track-nr start-time end-time)
  (unless (stringp string)
    (error "string in insert-envelope must be a string but is: ~a" string))
  (unless (numberp track-nr)
    (error "track-nr in insert-envelope must be a number but is: ~a" track-nr))
  (unless (equal 'reaper-envelope (type-of reaper-envelope))
    (error "reaper-envelope in insert-envelope must be a reaper-envelope object ~
            but is: ~a" reaper-envelope))
  (let* ((master? (>= 0 track-nr))
	 (parmenv? (is-parmenv (env-type reaper-envelope)))
	 (envelope-string (write-reaper-envelope
			   reaper-envelope nil start-time end-time))
	 ;; different scans when master or other track:
	 (place (if master?
		    "MASTER_SEL [0-9]+.*?<PROJBAY"
		    ;; TODO: detect whitespaces and newlines instead of .{1,5}?
		    ;; or detect <Track
		    "<TRACK .*?>.{1,5}?>"))
	 (insert (if parmenv?
		     "WAK [0-9]+ [0-9]+"
		     (if master?
			 "<PROJBAY"
			 "MAINSEND [0-9]+ [0-9]+")))
	 (find-track (ppcre:create-scanner place :single-line-mode t))
	 (find-old (ppcre:create-scanner
		    (format nil (if parmenv? "<~a ~a.*?>" "<~a.*?>")
			    (write-reaper-envelope-aux
			     (env-type reaper-envelope))
			    (parameter-slot reaper-envelope))
		    :single-line-mode t))
	 (insert-here (ppcre:create-scanner insert :single-line-mode t))
	 (n 0)
	 track
	 (track-start 0)
	 (track-end 0))
    ;; find the right track
    (ppcre:do-matches (st nd find-track string)
      (when (or master? (= (1- track-nr) n))
	(setf track (subseq string st nd)
	      track-start st
	      track-end nd))
      (incf n))
    ;; delete old envelope:
    (when track (setf track (ppcre:regex-replace find-old track "")))
    ;; insert new:
    (if (not track)
	(warn "no track with number ~a found" track-nr)
	(setf track
	  (ppcre:regex-replace
	   insert-here
	   track
	   (if parmenv?
	       (format nil "~a~&WAK 0 0" envelope-string)
	       (if master?
		   (format nil "~a~&  <PROJBAY" envelope-string)
		   ;; TODO: how to not hardcode 1 and 0 here?
		   (format nil "MAINSEND 1 0~&~a" envelope-string))))))
    ;; insert new track into project
    (with-output-to-string (out)
      (write-string string out :end track-start)
      (when track (write-string track out))
      (write-string string out :start track-end))))

;; EXAMPLE
#|
(edit-file "/E/test.rpp" any-variable-name
  (set-track-channels any-variable-name 16)
  (set-all-faders any-variable-name .2))
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; todo: what about multichannel input files?
;;;
;;; ****f* reaper/write-spatial-reaper-file
;;; AUTHOR
;;; Leon Focker: leon@leonfocker.de
;;;
;;; DATE
;;; April 30th 2023.
;;; 
;;; DESCRIPTION
;;; Create a reaper file that contains all files to be spatialized, the
;;; neccessary plugins and automation data.
;;; 
;;; ARGUMENTS
;;; - a list of sndfile objects: their angle-env and elevation-env arguments
;;; will be used for spatialization.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;;
;;; :file. The path to the file that shall be generated.
;;; :start-times. A list of start times for each soundfile on each track in
;;; seconds. If the list is shorter than the number of sndfiles, it is looped.
;;; Default = '(0) - this means all files start at 0.
;;; :sample-rate. The sample-rate that should be written to the reaper project.
;;; Default = 48000.
;;; :ambi-order. Should be an integer between 1 and 8, this determines the
;;; number of channel for each track.
;;; :tempo. The BPM value that should be written to the reaper project, if
;;; desired. Default = 60.
;;; :init-volume. Volume multipliers for all faders.
;;; -0dB would be 1, Default = -12dB.
;;; :encoder. The binary data for the plugin that will be inserted into all
;;; tracks except the master track. Default: *iem-stereo-encoder*
;;; :decoder. The binary data for the plugin that will be inserted on the master
;;; track. Default: *blue-ripple-decoder*
;;; :angle-parameter-slot. The automation slot that the angle-env will controll.
;;; This should be changed when other plugins are used than the defaults.
;;; Default = 6, because the IEM stereo encoders 6th automation slot is the
;;; angle-argument.
;;; :elevation-parameter-slot. The automation slot that the elevation-env will
;;; controll. This should be changed when other plugins are used than the
;;; defaults. Default = 7, because the IEM stereo encoders 7th automation slot
;;; is the elevation-argument.
;;; :envs-use-start-times. If nil, all envelopes start at the minimal
;;; start-time. If t, the envelopes use the start time of the respective
;;; sndfile.
;;; :envs-use-end-times. If nil, all envelopes end when every sndfile hast
;;; stopped. If t, end when the respective sndfile stopps.
;;; :envs-duration. If nil, the behavior of envs-use-end-times applies.
;;; If a number, every automation envelope gets this duration -
;;; envs-use-end-times will be ignored in this case.
;;; 
;;; RETURN VALUE
;;; path to the reaper file that was generated.
;;;
;;; EXAMPLE
#|
;;; make three soundfiles into sndfile objects and spatialize them, while 
;;; returning their paths in windows format.
(set-sc-config 'reaper-files-for-windows t)
(write-spatial-reaper-file
 `(,(make-sndfile "/E/pads.wav"
			  :angle-env '(0 0  .5 .5  .8 4  1 3.5)
			  :elevation-env '(0 0  .6 .5  2 .5))
    ,(make-sndfile "/E/synths.wav"
			  :angle-env '(0 0  .5 .5  .8 8  1 3.25)
			  :elevation-env '(0 0.5  1 .5))
    ,(make-sndfile "/E/drums.wav"
			  :angle-env '(0 .5  .5 1  .8 8.5  1 3.75)
			  :elevation-env '(0 0.5  1 .5)))
 :file "/E/spatial.rpp"
 :ambi-order 3
 :envs-use-start-times t
 :envs-use-end-times t)
|#
;;; SYNOPSIS
(defun write-spatial-reaper-file (list-of-sndfiles
				  &key file
				    (start-times '(0))
				    (sample-rate 48000)
				    (ambi-order 3)
				    (tempo 60)
				    (init-volume .2511)
				    (encoder *iem-stereo-encoder*)
				    (decoder *blue-ripple-decoder*)
				    (angle-parameter-slot 6)
				    (elevation-parameter-slot 7)
				    (envs-use-start-times t)
				    (envs-use-end-times t)
				    envs-duration)
  ;; sanity checks:
  (unless (every #'(lambda (x) (equal 'sndfile (type-of x))) list-of-sndfiles)
    (error "list-of-sndfile is indeed not a list of sndfiles but: ~a"
	   list-of-sndfiles))
  (unless (and (integerp ambi-order) (< 0 ambi-order 9))
    (error "ambi-order should be an integer between 1 and 8 but is ~a"
	   ambi-order))
  (unless file (setf file (default-dir-file "spatial.rpp")))
  (let* ((paths (loop for snd in list-of-sndfiles collect (path snd)))
	 (items (make-reaper-items4 paths start-times))
	 (looped-start-times
	    (loop for i from 0 below (length list-of-sndfiles) collect
		 (nth (mod i (length start-times)) start-times)))
	 (min-time (apply #'min start-times))
	 (max-time 0)
	 (channel-nr (expt (1+ ambi-order) 2))
	 (rf (create-tracks
	      (make-reaper-file 'spatial items
				:tempo (or tempo 60)
				:sample-rate sample-rate
				:n-channels channel-nr
				:master-volume init-volume)
	      :channels channel-nr
	      :track-volumes init-volume)))
    ;; when no duration is given and we don't use end-times, look for the file
    ;; that is playing the longest
    (unless (or envs-duration envs-use-end-times)
      (setf max-time (loop for snd in list-of-sndfiles and i from 0
			maximize (+ (nth i looped-start-times)
				    (duration snd)))))
    ;; write the file
    (write-reaper-file rf :file file)
    (format t "~&succesfully wrote ~a" file)
    ;; edit the file
    (edit-file file string
      (insert-plugin string decoder 0 t)
     (loop for i from 1 and snd in list-of-sndfiles
	 with temp-string = string
	 for start = (if envs-use-start-times
			    (nth (1- i) looped-start-times)
			    min-time)
	 for end = (if envs-duration (+ start envs-duration)
			  (if envs-use-end-times (+ start (duration snd))
			      max-time))
	do (setf temp-string (insert-plugin temp-string encoder i))
	  (setf temp-string
		(insert-envelope
		 temp-string
		 (make-reaper-envelope (print (angle-env snd))
				       :env-type 'parmenv
				       :parameter-slot angle-parameter-slot)
		 i
		 start
		 end))
	  (setf temp-string
		(insert-envelope
			  temp-string
			  (make-reaper-envelope (elevation-env snd)
						:env-type 'parmenv
						:parameter-slot elevation-parameter-slot)
			  i
			  start
			  end))
	finally (return temp-string)))
    file))

;;; EOF
