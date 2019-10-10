;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* palette/sndfilenet
;;; NAME 
;;; sndfile-palette
;;;
;;; File:             sndfilenet.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> sclist -> 
;;;                   circular-sclist -> assoc-list -> recursive-assoc-list ->
;;;                   palette -> sndfile-palette -> sndfilenet
;;;
;;; Version:          1.0.10
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Implementation of the sndfilenet class which extends
;;;                   sndfile-palette to add functionality for loading and
;;;                   playing sounds within MaxMSP.
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    23rd October 2017, Essen
;;;
;;; $$ Last modified:  18:09:33 Sat Nov 18 2017 CET
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :slippery-chicken)
#+sbcl (require "sb-bsd-sockets")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass sndfilenet (sndfile-palette)
  ;; the next sndfile-ext object for the purposes of the OSC sndfilenet
  ;; functionality 
  ((next-sfe :accessor next-sfe :initarg :next-sfe :initform nil)
   ;; whether we'll have the followers slots for the sndfiles i.e. whether
   ;; we're going to call max-play (and hence process-followers) for this
   ;; palette. Set to nil if you don't want to use with max-play, though for
   ;; now, that would imply it's best to use the parent class instead of this.
   (with-followers :accessor with-followers :type boolean
                   :initarg :with-followers :initform t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defmethod clone ((sfn sndfilenet))
  (clone-with-new-class sfn 'sndfilenet))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone-with-new-class :around ((sfn sndfilenet) new-class)
  (declare (ignore new-class))
  (let ((palette (call-next-method)))
    (setf (slot-value palette 'with-followers) (with-followers sfn)
          (slot-value palette 'next-sfe) (when (next-sfe sfn)
                                           (clone (next-sfe sfn))))
    palette))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod print-object :before ((sfn sndfilenet) stream)
  (format stream "~%SNDFILENET:      with-followers: ~a~
                  ~%                 next-sfe: ~a"
          (with-followers sfn) (next-sfe sfn)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod print-for-init ((sfn sndfilenet)
                           &key (stream t) (call 'make-sfn))
  (call-next-method sfn :stream stream :call call))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+clm
(defmethod verify-and-store :after ((sfn sndfilenet))
  (auto-cue-nums sfn)
  (reset sfn)
  ;; MDE Sat Dec 22 20:59:44 2012 (in the sndfile-palette class)
  (when (with-followers sfn)
    (process-followers sfn)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Wed Dec 19 14:29:35 2012 -- for MaxMSP/OSC interface

;;; ****m* sndfilenet/auto-cue-nums
;;; DESCRIPTION
;;; Set the cue-num slot of every sndfile-ext object in the palette to be an
;;; ascending integer starting at 2. NB If a sndfile has it's :use slot set to
;;; NIL it won't be given a cue number.
;;; 
;;; ARGUMENTS
;;; - a sndfilenet object.
;;; 
;;; RETURN VALUE
;;; The cue number of the last sndfile-ext object.
;;; 
;;; SYNOPSIS
(defmethod auto-cue-nums ((sfn sndfilenet))
;;; ****
  ;; to be sure: don't assume we'll always have non-nested data.
  (let ((refs (get-all-refs sfn)) 
        (cue-num 1))
    (loop for ref in refs 
         for snds = (get-data-data ref sfn)
         do
         (loop for snd in snds do
              (when (use snd)
                (setf (cue-num snd) (incf cue-num)))))
    cue-num))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* sndfilenet/osc-send-cue-nums
;;; DESCRIPTION
;;; Send via OSC the cue number of each sound file in a form that a Max sflist~
;;; can process and store (which will include the path and start/stop data: see
;;; sndfile-ext::max-cue for details).
;;; 
;;; E.g. preload 231 triplet-mphonic-001.wav 0. 3300.612305 0 1. 
;;; 
;;; Note that the individual preload data is sent by this method explicitly over
;;; the UDP network. This means you'll need a [udpreceive] somewhere in the max
;;; patch to catch these and that [osc-sc-eval] won't send these out of its
;;; outlet, rather the number of cues will be output there. Also note that the
;;; usual IDs prepended to results from osc-eval are not included in the preload
;;; sent over UDP so a separate parser looking for this data will be needed,
;;; e.g. 
;;;
;;; [udpreceive 8091][fromsymbol][route preload][prepend preload][sflist~ mysfl]
;;; 
;;; ARGUMENTS
;;; - the sndfilenet object.
;;; 
;;; RETURN VALUE
;;; The number of cue numbers sent.  NB This is not the same as the last cue
;;; number as cues start from 2.
;;; 
;;; SYNOPSIS
#+(and darwin sbcl)
(defmethod osc-send-cue-nums ((sfn sndfilenet))
;;; ****
  ;; to be sure: don't assume we'll always have non-nested data.
  (let ((refs (get-all-refs sfn)) 
        (cue-nums 0))
    (loop for ref in refs 
         for snds = (get-data-data ref sfn)
         do
         (loop for snd in snds do
              (when (use snd)
                ;; something like ("preload" 2 "/path/to/snd.wav" 300.0 1100.0)
                (osc-send-list (max-cue snd) nil) ; no warning 
                (incf cue-nums))))
    cue-nums))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Fri Dec 21 09:38:46 2012 
;;; ****m* sndfilenet/reset
;;; DESCRIPTION
;;; Reset the followers' slot circular list to the beginning or to <where>
;;; 
;;; ARGUMENTS
;;; - the sndfilenet object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - an integer to set the point at which to restart.  This can be higher than
;;;   the number of followers as it will wrap.  Default = nil (which equates to
;;;   0 lower down in the class hierarchy).
;;; - whether to issue a warning if <where> is greater than the number of
;;;   followers (i.e. that wrapping will occur).  Default = T.
;;; 
;;; RETURN VALUE
;;; T
;;; 
;;; SYNOPSIS
(defmethod reset ((sfn sndfilenet) &optional where (warn t))
;;; ****
  (let ((refs (get-all-refs sfn)))
    (loop for ref in refs 
       for snds = (get-data-data ref sfn)
       do
       (loop for snd in snds do
            (setf (group-id snd) ref)
            (reset snd where warn))))
  ;; MDE Fri Oct 20 20:11:48 2017
  (setf (next-sfe sfn) (first (data (get-first sfn))))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* sndfilenet/(setf next-sfe)
;;; DATE
;;; October 21st 2017, Essen
;;; 
;;; DESCRIPTION
;;; A setf :after method. Set the next-sfe slot to be a sndfile-ext object. If
;;; it is already, leave it as is. If it's an integer, assume it's a cue number
;;; and get the right object, otherwise get the object with this ID.
;;;
;;; If this is causing problems, use (setf (slot-value sfn 'next-sfe) ... )
;;;
;;; If we're trying to set using a reference/id like '(group1 snd3) then it
;;; will have to be a two-element list.
;;; 
;;; SYNOPSIS
(defmethod (setf next-sfe) :after (value (sfn sndfilenet))
;;; ****
  (let ((sfe (typecase value
               (integer (get-snd-with-cue-num sfn value))
               (list (get-snd (first value) (second value) sfn))
               ((or symbol string)
                (error "sndfilenet::(setf next-sfe): Both group and ~
                        sound file ID are needed: ~a" value))
               (t nil)))) ; so if it's an sfe already, leave it alone
    (when sfe 
      (setf (slot-value sfn 'next-sfe) sfe))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Fri Dec 21 09:38:52 2012
;;; ****m* sndfilenet/get-snd-with-cue-num
;;; DATE
;;; December 21st 2012
;;; 
;;; DESCRIPTION
;;; Return the (first, but generally unique) sndfile object which has the
;;; given cue-num slot.
;;; 
;;; ARGUMENTS
;;; - the sndfilenet object.
;;; - the cue number (integer).
;;; 
;;; RETURN VALUE
;;; The sndfile/sndfile-ext object with the given cue number or NIL if it can't
;;; be found.
;;; 
;;; SYNOPSIS
(defmethod get-snd-with-cue-num ((sfn sndfilenet) cue-num)
;;; ****
  (let ((refs (get-all-refs sfn))
        (result nil))
    (loop for ref in refs 
       for snds = (get-data-data ref sfn)
       do
       (loop for snd in snds do
            (when (= (cue-num snd) cue-num)
              (setf result snd)
              (return))))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* sndfilenet/max-play
;;; DESCRIPTION
;;; This generates the data necessary to play the next sound in the current
;;; sound's followers list. See the sndfile-ext method for details.
;;; 
;;; ARGUMENTS
;;; - The sndfilenet object.
;;; - The fade (in/out) duration in seconds.
;;; - The maximum loop duration in seconds.
;;; - The time to trigger the next file, as a percentage of the current
;;;   sndfile-ext's duration.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - whether to print data to the listener as it is generated. Default = NIL.
;;; 
;;; RETURN VALUE
;;; A list of values returned by the sndfile-ext method.
;;;
;;; SYNOPSIS
(defmethod max-play ((sfn sndfilenet) fade-dur max-loop start-next
                     &optional print)
;;; ****
  (if (next-sfe sfn) ; next-sfe is a sndfile-ext object
      (let* ((current (next-sfe sfn))
             (next (get-next current)))
        ;; (print current)
        (setf (next-sfe sfn) next)
        (when print
          (format t "~&cue ~a (~a): ~a --> ~a"
                  (cue-num current) (id current) (start current) (end current)))
        (max-play current fade-dur max-loop start-next))
      (warn "sndfilenet::max-play: no next-sfe!")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; check that follower references refer to other sndfiles in the palette, then
;;; replace the references with the sndfile object themselves, but only when
;;; their <use> slot is T.  This is perhaps memory intensive but it'll save
;;; some CPU cycles by processing here rather than when max asks for the next
;;; sndfile.
(defmethod process-followers ((sfn sndfilenet) &optional
                                                      (on-fail #'error))
  (let ((refs (get-all-refs sfn))
        (result t))
    (loop for ref in refs 
       for snds = (get-data-data ref sfn)
       do
       (loop for snd in snds with follower with fsnd do
            (if (followers snd)
                (setf (followers snd)
                      (loop for i below (sclist-length (followers snd)) do
                           (setf fsnd nil)
                           (loop for j from i
                              below (sclist-length (followers snd))
                              do
                              (setf follower (get-nth j (followers snd))
                                    fsnd (get-snd-short sfn follower snd))
                              ;;(print follower) (print fsnd)
                              ;; MDE Sat Dec 22 20:36:14 2012 -- got to make
                              ;; sure the user actually wants to use this sound
                              ;; in this piece
                              (when (and fsnd (use fsnd))
                                (return fsnd)))
                           (when (and follower (not fsnd))
                             (setf result nil)
                             (when on-fail
                               (funcall 
                                on-fail "sndfilenet::process-followers: ~
                                         No such sound file: ~a"
                                follower)))
                           when (and fsnd (use fsnd)) collect fsnd))
                (warn "sndfilenet::process-followers: ~a has no followers ~
                       so if triggered will cause max-play to stop."
                      (id snd)))))
    (reset sfn)
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* sndfilenet/auto-followers
;;; DATE
;;; October 20th 2017
;;; 
;;; DESCRIPTION
;;; Automatically generate follower sound files for every sound file in the
;;; palette. These are a mixture of the other files in the same group plus one
;;; from another group, so that we'll move through.
;;; 
;;; ARGUMENTS
;;; - a sndfilenet object
;;; 
;;; RETURN VALUE
;;; T
;;;
;;; SYNOPSIS
(defmethod auto-followers ((sfn sndfilenet))
;;; ****
  (let ((refs (get-all-refs sfn)))
    (loop for ref in refs and next-ref in (wrap-list refs 1)
       for sfs = (reverse (get-data-data ref sfn))
       for nsfs = (make-cscl (reverse (get-data-data next-ref sfn)) :copy nil)
       do 
         (loop for sf in sfs
            for followers = (reverse
                             (cons
                              (get-next nsfs)
                              (remove-with-id sfs (id sf))))
            do 
              (setf (slot-value sf 'followers) (make-cscl followers
                                                          :copy nil)))))
  (setf (with-followers sfn) t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* sndfilenet/analyse-followers
;;; DESCRIPTION
;;; Using the followers slots of each sndfile in the palette, go through each
;;; sndfile in the palette and generate a large number of following sounds,
;;; i.e. emulate max-play. The results of the follow-on process are then
;;; analysed and a warning will be issued if any sndfile seems to dominate
;;; (defined as being present at least twice as many times as its 'fair share',
;;; where 'fair share' would mean an even spread for all the sound files in the
;;; palette).
;;; 
;;; ARGUMENTS
;;; - The sndfilenet object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - integer: How many times to repeat the generation process.  Default = 1000.
;;; - T or NIL for verbose warnings: if T, the emulation list of sound files
;;;   will be printed. Default NIL.
;;; 
;;; RETURN VALUE
;;; T or NIL depending on whether the analysis detects an even spread or not.
;;; 
;;; SYNOPSIS
(defmethod analyse-followers ((sfn sndfilenet)
                              &optional (depth 1000) verbose)
;;; ****
  (loop with ok = t
     with refs = (get-all-refs sfn)
     ;; an equal spread of all sndfiles in the palette would be ideal but let's
     ;; not worry until one of those is played twice as many times as that 
     with threshold = (round (* 2.0 (/ depth (num-snds sfn))))
     for ref in refs
     for snds = (get-data-data ref sfn)
     do
     (loop for snd in snds
        for sndaf = (analyse-followers snd depth)
        for max = (second (first sndaf))
        for this-ok = (<= max threshold)
        do
        (unless this-ok
          (warn "sndfilenet::analyse-followers: (~a ~a) ~%generates ~
                 unbalanced results, e.g. ~a ~%occurs more than ~a times: ~&~a"
                ref (id snd) (first (first sndaf)) threshold
                (if verbose sndaf ""))
          (setf ok nil)))
     finally (return ok)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* sndfilenet/make-sfn
;;; DESCRIPTION
;;; Make a sndfilenet object. This object is a simple palette which checks
;;; to make sure that all of the sound files in a given list exist for each
;;; given ID.
;;;
;;; Sound files are given as as single names, without the path and without the
;;; extension. These can be given using the optional keyword arguments <paths>
;;; and <extensions>.
;;;
;;; NB Although this class is a palette and therefore a subclass of
;;; recursive-assoc-list, the sound lists in this case cannot be nested beyond
;;; a depth of two (as in example below).  
;;; 
;;; ARGUMENTS
;;; - An ID for the palette.
;;; - A list of lists that contains IDs for the names of one or more groups of
;;;   sound files, each paired with a list of one or more names of existing
;;;   sound files. The sound file names themselves can be paired with keywords
;;;   from the sndfile class, such as :start, :end, and :frequency, to define
;;;   and describe segments of a given sound file.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :paths. A list of one or more paths to where the sound files are located.
;;; - :extensions. A list of one or more sound file extensions for the
;;;   specified sound files. The default initialization for this slot of the
;;;   sndfilenet already contains ("wav" "aiff" "aif" "snd"), so this
;;;   argument can often be left unspecified.
;;; - :warn-not-found. T or NIL to indicate whether a warning should be printed
;;;   to the Lisp listener if the specified sound file cannot be found. 
;;;   T = print warning. Default = T.
;;; 
;;; RETURN VALUE
;;; Returns NIL.
;;; 
;;; EXAMPLE
#|
;;; NB won't work unless you change /path/to/ below
(let ((msfn (make-sfn 'sfn-test 
                      '((sndfile-group-1
                         (test-sndfile-1))
                        (sndfile-group-2
                         (test-sndfile-2 test-sndfile-3 
                          (test-sndfile-4 :frequency 261.61)))
                        (sndfile-group-3
                         ((test-sndfile-5 :start 0.006 :end 0.182) 
                          test-sndfile-6)))
                      :paths '("/path/to/sound-files-dir-1/"
                               "/path/to/sound-files-dir-2/")))))

|#
;;; SYNOPSIS
(defun make-sfn (id sfn &key paths (extensions '("wav" "aiff" "aif" "snd"))
                          auto-freq with-followers (warn-not-found t))
;;; ****
  (make-instance 'sndfilenet :id id :data sfn :paths paths
                 :with-followers with-followers :extensions extensions
                 :auto-freq auto-freq
                 :warn-not-found warn-not-found))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sndfilenet-p (thing)
  (typep thing 'sndfilenet))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF sndfilenet.lsp
