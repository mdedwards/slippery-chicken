;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* linked-named-object/sndfile
;;; NAME 
;;; sndfile
;;;
;;; File:             sndfile.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> sndfile
;;;
;;; Version:          1.0.12
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Implementation of the sndfile class that holds
;;;                   information about a sound file as well as specifying
;;;                   desired parameters
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    March 21st 2001
;;;
;;; $$ Last modified:  15:45:43 Sat Mar 16 2024 CET
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
;;; the name (defaults to the given name of the file) is stored in <id>, the
;;; given file name (perhaps minus path and extension) in <data>.
(defclass sndfile (linked-named-object)
  ((path :accessor path :initarg :path :initform nil)
   ;; the sound's full duration
   (snd-duration :accessor snd-duration :initform nil)
   ;; the duration the user wants
   (duration :accessor duration :initarg :duration :initform nil)
   ;; this shouldn't be given as well as duration
   (end :accessor end :initarg :end :initform nil)
   (channels :accessor channels :initform nil)
   ;; where we want to start in the file (secs).
   (start :accessor start :initarg :start :initform 0.0)
   ;; a simple textual description slot useful for identifying sounds
   (description :accessor description :initarg :description :initform "")
   ;; this is a scaler that will be used in with-sound calls, if wanted.
   (amplitude :accessor amplitude :initarg :amplitude 
              :initform 1.0)
   ;; optional spatialisation data:
   ;; can be a breakpoint-list (env) or a list of envs.
   (angle-env :accessor angle-env :type list :initarg :angle-env
              :initform '(0 0  100 0))
   (elevation-env :accessor elevation-env :type list :initarg :elevation-env
                  :initform '(0 0  100 0))
   (distance-env :accessor distance-env :type list :initarg :distance-env
                 :initform '(0 1  100 1))
   ;; some sounds have a prominent fundamental which can be used for
   ;; transposing to specific pitches.  Give this here either in the form of a
   ;; real freq or a note, which will then be converted.
   ;; MDE Fri Sep 25 13:44:28 2015 -- now allow value of 'detect to indicate we
   ;; want pitch detection (was slowing down make-sfp too much when autoc was
   ;; default).
   ;; MDE Sat Dec 15 14:53:02 2018 -- this can also be a function whereupon it
   ;; will be called with the path slot as argument. The idea is that the
   ;; fundamental can be extracted from the file name.
   (frequency :accessor frequency :initarg :frequency :initform 'c4)
   (centroid :reader centroid :writer (setf centroid) :initform nil)
   ;; when duration is given, we have to update end and vice-versa.  This slot
   ;; tells us whether this was done and so avoids endless back-and-forths when
   ;; calling the setf methods.
   (data-consistent :accessor data-consistent :initform nil)
   ;; for some purposes (like incrementing start-time) it's useful to know how
   ;; many times a sound will be used and has been used.
   (will-be-used :accessor will-be-used :initform 0)
   (has-been-used :accessor has-been-used :initform 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone ((sf sndfile))
  (clone-with-new-class sf 'sndfile))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone-with-new-class :around ((sf sndfile) new-class)
  (declare (ignore new-class))
  (let ((named-object (call-next-method)))
    (setf (slot-value named-object 'path) (basic-copy-object (path sf))
          (slot-value named-object 'snd-duration ) (snd-duration sf)
          (slot-value named-object 'duration ) (duration sf)
          (slot-value named-object 'end ) (end sf)
          (slot-value named-object 'channels ) (channels sf)   
          (slot-value named-object 'start ) (start sf)         
          (slot-value named-object 'amplitude ) (amplitude sf)         
          (slot-value named-object 'frequency )
          (basic-copy-object (frequency sf))
          (slot-value named-object 'data-consistent ) (data-consistent sf)     
          (slot-value named-object 'will-be-used ) (will-be-used sf)
          (slot-value named-object 'has-been-used ) (has-been-used sf)
          (slot-value named-object 'description ) (description sf))
    ;; (print 'sndfile-clone-wnc) (print (data sf))
    named-object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((sf sndfile) &rest initargs)
  (declare (ignore initargs))
  ;; (print 'init-sndfile)
  ;; just in case any of these slots were given as lists of mins, secs, ms
  (setf (slot-value sf 'start) (mins-secs-to-secs (start sf))
        (slot-value sf 'end) (mins-secs-to-secs (end sf))
        (slot-value sf 'duration) (mins-secs-to-secs (duration sf)))
  (when (and (duration sf) (end sf))
    (error "sndfile::initialize-instance: ~
            The Duration and End slots cannot both be specified! ~%~a" 
           sf))
  (update sf))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object :before ((sf sndfile) stream)
  (format stream "~%SNDFILE: path: ~a, ~
                    ~%         snd-duration: ~a, channels: ~a, frequency: ~a~
                    ~%         start: ~a, end: ~a, amplitude: ~a, duration: ~a~
                    ~%         will-be-used: ~a, has-been-used: ~a~
                    ~%         data-consistent: ~a, description: ~a"
          (path sf) (snd-duration sf) (channels sf) (frequency sf) (start sf)
          (end sf) (amplitude sf) (duration sf) (will-be-used sf)
          (has-been-used sf) (data-consistent sf) (description sf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon Apr 16 18:29:55 BST 2012: Added robodoc entry

;;; ****m* sndfile/stereo
;;; DESCRIPTION
;;; Test whether the CHANNELS slot of a given sndfile object is set to 2.
;;; 
;;; ARGUMENTS
;;; - A sndfile object.
;;; 
;;; RETURN VALUE
;;; Returns T if the CHANNELS slot is set to 2, otherwise returns NIL.
;;; 
;;; EXAMPLE
#|
;; The method make-sndfile creates a sndfile object with the CHANNELS slot set
;; to NIL. Make a sndfile object, test to see whether the value of the CHANNELS
;; slot is 2; set the CHANNELS slot to 2 and test again.
(let ((sf-1 (make-sndfile "/path/to/sndfile-1.aiff")))
  (print (stereo sf-1))
  (setf (channels sf-1) 2)
  (print (stereo sf-1)))

=>
NIL 
T

|#
;;; SYNOPSIS
(defmethod stereo ((sf sndfile))
;;; ****
  (when (channels sf) ; MDE Mon Apr 23 18:49:02 2012 -- 
    (= 2 (channels sf))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon Apr 16 18:38:48 BST 2012: Added robodoc entry

;;; ****m* sndfile/reset-usage
;;; DESCRIPTION
;;; Reset the WILL-BE-USED and HAS-BEEN-USED slots of the given sndfile object
;;; to 0. These slots keep track of how many times a sound will be used and has
;;; been used, which is useful for purposes such as incrementing
;;; start-time. These slots are set internally and are not intended to be set by
;;; the user.
;;; 
;;; ARGUMENTS
;;; - A sndfile object.
;;; 
;;; RETURN VALUE
;;; Returns 0.
;;; 
;;; EXAMPLE
#|
;; First set the values of the WILL-BE-USED and HAS-BEEN-USED slots, as these
;; are 0 when a new sndfile object is created using make-sndfile. Set the
;; values, print them; reset both using reset-usage, and print again to see
;; the change.
(let ((sf-1 (make-sndfile "/path/to/sndfile-1.aiff"))))
  (setf (will-be-used sf-1) 11)
  (setf (has-been-used sf-1) 13)
  (print (will-be-used sf-1))
  (print (has-been-used sf-1))
  (reset-usage sf-1)
  (print (will-be-used sf-1))
  (print (has-been-used sf-1)))

=>
11 
13 
0 
0

|#
;;; SYNOPSIS
(defmethod reset-usage ((sf sndfile))
;;; ****
  (setf (will-be-used sf) 0
        (has-been-used sf) 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf path) :after (value (sf sndfile))
  (declare (ignore value))
  (update sf))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DJR Mon 30 Sep 2019 14:56:33 BST
;;; Not usre why this was never here
(defmethod (setf description) :after (value (sf sndfile))
  (setf (slot-value sf 'description) value)
  (update sf))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf duration) :after (value (sf sndfile))
  (declare (ignore value))
  (setf (slot-value sf 'duration) (mins-secs-to-secs (duration sf)))
  (when (data-consistent sf)
    (setf (data-consistent sf) nil)
    (set-end sf)
    (update sf)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf end) :after (value (sf sndfile))
  (declare (ignore value))
  (setf (slot-value sf 'end) (mins-secs-to-secs (end sf)))
  (when (data-consistent sf)
    (setf (data-consistent sf) nil)
    (set-dur sf)
    (update sf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf start) :after (value (sf sndfile))
  (declare (ignore value))
  (setf (slot-value sf 'start) (mins-secs-to-secs (start sf)))
  (when (data-consistent sf)
    (setf (data-consistent sf) nil)
    (set-dur sf)
    (update sf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf frequency) :after (value (sf sndfile))
  (declare (ignore value))
  (update sf))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod update ((sf sndfile) &key ignore)
  (declare (ignore ignore))
  ;; an id will generally only be given when two instances of the same file are
  ;; in the same list in a sndfile-palette, so in the usual case where it's not
  ;; specified set id to simply the given sound file name.
  ;; (print 'update-sf) (print (path sf))
  (unless (id sf)
    (setf (id sf) (data sf)))
  (let ((given-freq (frequency sf)))
    (when (and given-freq (symbolp given-freq) (not (eq given-freq 'detect)))
      (let ((freq (note-to-freq given-freq)))
        (unless freq
          (error "sndfile::update: Couldn't get the frequency for note ~a!!!"
                 given-freq))
        ;; MDE Sun Dec 16 14:37:13 2012 -- set slot value so update isn't
        ;; called again  
        ;; (setf (frequency sf) freq))))
        (setf (slot-value sf 'frequency) freq))))
  (let* ((path (path sf))
	 (sf-info (get-sound-info path)))
    (when path
      (unless (and path (probe-file path))
        (error "sndfile::update: ~
                Data slot of sndfile must be set to an existing sound file:~%~a"
               path))
      (unless (data sf)
        ;; MDE Sun Dec 16 14:37:13 2012 -- set slot value so update isn't
        ;; called again  
        ;; (setf (data sf) path))
        (setf (slot-value sf 'data) path))
      ;; LF 2024-03-17 19:24:43 updated this for #'get-sound-info
      (when sf-info
	(setf (snd-duration sf) (fourth sf-info)
	      (channels sf) (second sf-info)))
      (cond ((and (not (end sf)) (duration sf))
             (set-end sf))
            ((and (not (duration sf)) (end sf)) 
             (set-dur sf))
            ((and (not (end sf)) (not (duration sf)) (snd-duration sf))
             ;; MDE Sun Dec 16 15:02:34 2012 -- slot-value!
             (setf (slot-value sf 'end) (snd-duration sf))
             (set-dur sf)))
      ;; MDE Sat Dec 15 14:50:53 2018 
      (if (functionp (frequency sf))
          (setf (slot-value sf 'frequency) (funcall (frequency sf) (path sf)))
          ;; MDE Mon Sep 7 11:11:09 2015 -- auto detect frequency using Bret
          ;; Battey's CLM autocorrelation instrument. If there's no CLM package
          ;; this will just return the freq for 'c4.
          (when (eq 'detect (frequency sf)) ;(frequency sf))
            (setf (slot-value sf 'frequency)
                  (autoc-get-fundamental
                   path (start sf)      ;(duration sf)))))
                   ;; MDE Tue May 31 10:03:50 2022, Heidhausen -- 200ms is
                   ;; enough for 4 periods of 20Hz
                   .2))))
      ;; MDE Mon Feb 17 15:03:12 2020 -- don't allow freqs of 0 otherwise
      ;; we'll get division-vy-zero errors in clm-play (thanks Dan) 
      (when (equal-within-tolerance (frequency sf) 0.0)
        (warn "sndfile::update: can't have a frequency of zero; setting to 'c4")
        (setf (slot-value sf 'frequency) (note-to-freq 'c4)))
      (let ((st (start sf))
            (end (end sf)))
        (when (< st 0)
          (error "sndfile::update: start < 0???: ~a" sf))
        (when (and end (<= end st))
          (error "sndfile::update: end <= start???: ~a" sf))
        ;;  MDE Thu Jan 28 16:55:48 2021, Heidhausen -- signal a warning only
        ;; and adjust
        (when (and (snd-duration sf) (> end (snd-duration sf)))
          (warn "sndfile::update: ~
                 Given end point (or duration: ~a) ~%  is > sound duration: ~
                 ~a ~%~a"
                end (snd-duration sf) sf)
          (setf (slot-value sf 'end) (snd-duration sf))
          (set-dur sf)))
      (setf (data-consistent sf) t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod set-dur ((sf sndfile))
  (let ((end (end sf))
        (start (start sf)))
    (when (and start end)
      ;; MDE Sun Dec 16 15:00:27 2012 -- use slot-value
      (setf (slot-value sf 'duration) (- end start)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Wed Feb 28 20:27:14 2024, Heidhausen -- updated for reaper-play
(defmethod set-end ((sf sndfile))
  (unless (snd-duration sf)
    (error "sndfile::set-end: can't set end without the duration slot: ~%~a"
           sf))
  (let* ((dur (duration sf))
         (start (start sf))
         (dps most-positive-double-float))
    (when (and start dur)
      (setq dps (+ start dur)))
    ;; MDE Sun Dec 16 15:00:27 2012 -- use slot-value
    (setf (slot-value sf 'end) (if (< dps (snd-duration sf))
                                 dps
                                 (snd-duration sf)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Tue Sep 29 21:43:50 2015 -- for writing sndfile-palette data for
;;; re-reading (so we an avoid redoing costly frequency analysis)
(defmethod get-slots-list ((sf sndfile))
  (list (path sf) :frequency (frequency sf) :id (id sf)
        :duration (snd-duration sf) :end (end sf) :start (start sf)
        :amplitude (amplitude sf) :data (data sf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Thu Oct  1 17:08:29 2015 - used by the sndfile-palette class method.
(defmethod set-frequency-from-filename
    ((sf sndfile)
     &key (name-fun #'akoustik-piano-name))
  (let ((midi (funcall name-fun (pathname-name (path sf)))))
    (when midi
      (setf (frequency sf) (midi-to-freq midi)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* sndfile/centroid
;;; DATE
;;; February 7th 2023, Heidhausen
;;; 
;;; DESCRIPTION
;;; Calculate the average spectral centroid of a sound file, between the start
;;; and end points given. This requires CLM's scentroid.ins to be compiled
;;; and loaded in advance. Note that the averaging is done by FFT analysis every
;;; 10 milliseconds and with an FFT size of 4096 samples. FFT frames that have
;;; an RMS amplitude of < -40dbFS will be ignored.
;;; 
;;; ARGUMENTS
;;; the sndfile object
;;; 
;;; RETURN VALUE
;;; The frequency of the centroid in Hertz.
;;;
;;; SYNOPSIS
(defmethod centroid ((sf sndfile))
;;; ****
  (with-slots ((cd centroid)) sf
    (flet ((warnsc ()
             (warn "sndfile::centroid: the CLM instrument ~
                    scentroid.ins needs to be loaded in order for this ~
                    function to work. Returning 0 as default")
             0))
      ;; if it's already been calculated, just return it
      (if cd
          cd
          #+clm
          (if (fboundp 'clm::scentroid)
              (setf cd
                    (let* ((centroids (clm::scentroid (path sf)
                                                      :beg (start sf)
                                                      :dur (duration sf)))
                           (just-ys (remove-if
                                     ;; don't use spectral frames with rms
                                     ;; values lower than the threshold
                                     ;; (centroid = 0) in the averaging
                                     #'(lambda (x)
                                         (equal-within-tolerance x 0.0))
                                     (loop for y in (rest centroids)
                                           by #'cddr collect y))))
                      (average just-ys)))
              (warnsc))
      #-clm (warnsc)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Mon Apr 16 17:10:01 BST 2012: Added robodoc entry

;;; MDE original comment:
;;; If path is a list, then it's come from a sndfile-palette.  The first in the
;;; list is the full path as found by the sndfile-palette and the second is the
;;; list given by the user with the data slots to be used.

;;; ****f* sndfile/make-sndfile
;;; DESCRIPTION
;;; Create a sndfile object to hold data about an existing sound file,
;;; specifying at least the path and file name of that sound file. Optional
;;; arguments allow for the specification of segments within the given sound
;;; file and its perceived fundamental frequency (for src-based
;;; transposition). 
;;;
;;; If the first argument ("path") is a list, then this sndfile object has been
;;; created from within a sndfile-palette object. In this case, the first item
;;; in the list will be the full path to the sound file, as defined in the
;;; sndfile-palette object; the second item in the list is the list given by
;;; the user containing the data slots to be used, whereby the first item of
;;; that list must be the ID of the object.
;;;
;;; NB: This function creates an object of the class sndfile, which is contains
;;;     data concerning an existing sound file. It does not create an actual
;;;     sound file. 
;;; 
;;; ARGUMENTS
;;; - A path and file name of an existing sound file; or a list as explained
;;;   above. 
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :id. An ID for the sndfile. Will be set automatically if created from
;;;   within a sndfile-palette.  Default nil.
;;; - :data. The given file name, including path and extension, usually set
;;;   automatically to be the given path if nil.  Default nil. 
;;; - :duration. A number in seconds which is the duration of the segment of
;;;   the specified sound file which the user would like to use. This should
;;;   not be specified if :end has been specified. Default nil.
;;; - :end. A number in seconds which is the end time within the source sound
;;;   file for the segment of the file which the user would like to use. This
;;;   should not be specified if :duration has been specified. Default nil.
;;; - :start. A number in seconds which is the start time within the source
;;;   sound file for the segment of the file which the user would like to
;;;   use. Defaults to 0.0.
;;; - :frequency. A number or note-name symbol. This frequency will serve as
;;;   the reference pitch for any src transpositions of this file. This can be
;;;   any value, but will most likely be specified if the source sound file has
;;;   a perceptible fundamental pitch. If given as a number, this number will
;;;   be handled as a frequency in Hertz. Default = 'C4.
;;; - :amplitude. An number that is the amplitude which the user would like to
;;;   designate for this sound file. This number may be of any value, as
;;;   slippery chicken normalizes all sound file events; however, standard
;;;   practice would suggest that this should fall between 0.0 and 1.0.
;;;   Default = 1.0
;;; - :angle-env. used for spatialization, fore example with
;;;   #'write-reaper-ambisonics-file. Can be an env (list of breakpoints) or
;;;   a list of envs. A list of envs can be used to spatialize different channels
;;;   of the soundfile differently. See the examples in
;;;   write-reaper-ambisonics-file. This then represents the azimuth angle in a
;;;   polar coordinate system. 0 and 1 represend 0° and 360° and are assumed to 
;;;   be in the front. 0.5 would thus be 180° and located behind the listener.
;;; - :elevation-env. same as angle-env but represents the elevation angle.
;;;   (the horizontal angle from the x-axis). -.5 represents the lowest point
;;;   at -90°, 0 the level of the listening position and .5 the top (90°).
;;;   This is, so that the span from 0 to 1 can represent the entire 360°.
;;; - :distance-env. same as angle-env but represents the distance from the
;;;   listening position. This is not really relevant for use in ambisonics.
;;;   There the distance is usually 1.
;;;
;;; RETURN VALUE
;;; A sndfile object.
;;; 
;;; EXAMPLE
#|
;; Example specifying the full path, a start and end time, and a base frequency 
(make-sndfile "/path/to/sndfile-1.aiff"
              :start 0.3
              :end 1.1
              :frequency 654)

=> 

SNDFILE: path: /path/to/sndfile-1.aiff, 
         snd-duration: 3.011043, channels: 1, frequency: 654
         start: 0.3, end: 1.1, amplitude: 1.0, duration 0.8
         will-be-used: 0, has-been-used: 0
         data-consistent: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: NIL, tag: NIL, 
data: /path/to/sndfile-1.aiff

;; Example using the sndfile-palette list as the first argument
(make-sndfile '("/path/to/sndfile-1.aiff" 
                (nil :start 0.3 :end 1.1)))

=> 

SNDFILE: path: /path/to/sndfile-1.aiff, 
         snd-duration: 3.011043, channels: 1, frequency: 261.62555
         start: 0.3, end: 1.1, amplitude: 1.0, duration 0.8
         will-be-used: 0, has-been-used: 0
         data-consistent: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: "/Volumes/JIMMY/SlipperyChicken/sndfile-2.aiff", tag: NIL, 
data: /path/to/sndfile-1.aiff

|#
;;; SYNOPSIS
(defun make-sndfile (path &key id data duration end (start 0.0)
                     (frequency nil)
                     (amplitude 1.0)
                     (angle-env '(0 0  100 0))
                     (elevation-env '(0 0  100 0))
                     (distance-env '(0 1  100 1)))
;;; **** 
  (if (and path (listp path))
      (progn
        (let ((sf (make-instance (first path)
                                 ;; MDE Wed Dec 26 11:16:53 2012 -- :class must
                                 ;; come first
                                 ;; :path (first path)
                                 :path (second path)
                                 ;; :id (first (second path))))
                                 :id (first (third path))))
              (slots (rest (third path))))
          (loop for slot in slots by #'cddr 
             and value in (cdr slots) by #'cddr do
             ;; we have to do this here because (setf (slot-value ... ))
             ;; doesn't call the setf methods...
             (case slot
               (:duration (setf (duration sf) value))
               (:end (setf (end sf) value))
               (:frequency (setf (frequency sf) value))
               (:start (setf (start sf) value))
               ;; MDE Wed Dec 26 10:47:45 2012 -- only try and set a slot value
               ;; here if it exists in this class (as opposed to the sndfile-ext
               ;; class) 
               (t (let ((s (rm-package slot)))
                    (when (slot-exists-p sf s)
                      (setf (slot-value sf s) value))))))
          sf))
      (make-instance 'sndfile :id id :data data :path path :duration duration
                     :frequency frequency :end end :start start
                     :amplitude amplitude
                     :angle-env angle-env :elevation-env elevation-env
                     :distance-env distance-env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* sndfile/get-sound-info
;;; AUTHOR
;;; Leon Focker: leon@leonfocker.de
;;;
;;; DATE
;;; March 17th 2024.
;;;
;;; DESCRIPTION
;;; Get duration, srate, number of channels and bits per sample for a
;;; sound (or video) file. This function uses clm, if installed, or ffprobe
;;; as an alternative and fallback method, when clm is not available.
;;; Use (set-sc-config 'ffprobe-command "new/path") to change the default path
;;; for ffprobe.
;;;
;;; NB: CLM seems to get the bitdepth wrong in an awful lot of cases...
;;; is there an alternative?
;;;
;;; ARGUMENTS
;;; - filename. The path to the sound or video file.
;;;
;;; OPTIONAL ARGUMENTS
;;; - ffprobe. Default=nil. When nil, use clm if possible, else ffprobe.
;;;   When t, use ffprobe with the default path to ffprobe. When a string,
;;;   interpret that string as path to the ffprobe executable.
;;;
;;; RETURN VALUE
;;; a list containing duration, srate, number of channels and bits per sample as
;;; numbers
;;;
;;; SYNOPSIS
(defun get-sound-info (filename &optional ffprobe)
;;; ****
  (when (and filename (probe-file filename))
    (let ((clm #+clm t #-clm nil))
      (if (and clm (not ffprobe))
	  ;; this order is important because ffprobe returns results like this
	  (list #+clm(clm::sound-srate filename)
		#+clm(clm::sound-chans filename)
		#+clm(clm::mus-sound-bits-per-sample filename)
		#+clm(clm::sound-duration filename))
	  (string-to-list
	   (shell-to-string (if (stringp ffprobe)
				ffprobe
				(get-sc-config 'ffprobe-command))
			    "-v" "error" "-select_streams" "a:0"
			    "-show_entries" "format=duration"
			    "-show_entries" "stream=sample_rate"
			    "-show_entries" "stream=channels"
			    "-show_entries" "stream=bits_per_sample"
			    "-of" "default=noprint_wrappers=1:nokey=1"
			    filename))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun autoc-get-fundamental (file start duration)
  (flet ((warnac ()
           (warn "sndfile::autoc-get-fundamental: the CLM instrument ~
                  autoc.ins needs to be loaded in order for this function ~
                  to work. Returning 'C4 as default")
           (note-to-freq 'c4)))
    #+clm
    (if (fboundp 'clm::autoc)
        (let* ((penv (clm::autoc file :beg start :post-process t :min-freq 30
                                 :dur duration :db-floor -60))
               (y (loop for y in (cdr penv) by #'cddr collect y))
               (avg (/ (apply #'+ y) (length y))))
          ;; (format t "~%average pitch: ~F~%" avg)
          avg)
        (warnac))
    #-clm (warnac)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sndfile-p (candidate)
  (typep candidate 'sndfile))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF sndfile.lsp
