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
;;; Version:          1.1.0
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
;;; $$ Last modified:  17:34:58 Sat Jan 30 2021 CET
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
;; MDE Wed Jan  6 15:13:35 2021, Heidhausen -- removed the with-followers slot
;; as followers are now always processed if they're given for any sndfiles
(defclass sndfilenet (sndfile-palette)
  ;; the next sndfile-ext object for the purposes of the OSC sndfilenet
  ;; functionality 
  ((next-sfes :accessor next-sfes :initarg :next-sfes :initform nil)
   (all-refs :accessor all-refs :initform nil :type list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone ((sfn sndfilenet))
  (clone-with-new-class sfn 'sndfilenet))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone-with-new-class :around ((sfn sndfilenet) new-class)
  (declare (ignore new-class))
  (let ((palette (call-next-method)))
    (setf (slot-value palette 'next-sfes) (my-copy-list (next-sfes sfn))
          (slot-value palette 'all-refs) (copy-list (all-refs sfn)))
    palette))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod print-object :before ((sfn sndfilenet) stream)
  (format stream "~%SNDFILENET:      next-sfes (ids): ~a~
                  ~%                 all-refs: ~a"
   (loop for sfe in (next-sfes sfn) collect (id sfe))
   (all-refs sfn)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod print-for-init ((sfn sndfilenet)
                           &key (stream t) (call 'make-sfn))
  (call-next-method sfn :stream stream :call call))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+clm
(defmethod verify-and-store :after ((sfn sndfilenet))
  (auto-cue-nums sfn)
  ;; we can flatten here because we can only have top-level groups
  (setf (slot-value sfn 'all-refs) (flatten (get-all-refs sfn)))
  (reset sfn)
  ;; MDE Sat Dec 22 20:59:44 2012 (in the sndfile-palette class)
  ;; MDE Wed Jan  6 15:11:50 2021, Heidhausen -- always process followers! (but
  ;; no warnings)
  (process-followers sfn nil)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf all-refs) (value (sfn sndfilenet))
  (warn "sndfilenet::(setf all-refs): don't setf all-refs. Ignorning."))

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
  (let ((cue-num 1))
    (loop for ref in (all-refs sfn)
         for snds = (get-data-data ref sfn)
         do
         (loop for snd in snds do
              (when (use snd)
                (setf (cue-num snd) (incf cue-num)))))
    cue-num))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Tue Dec  8 18:40:33 2020, Heidhausen -- this used to be called
;;; osc-send-cue-nums but we no longer send separate lists via OSC rather we
;;; return a list and let osc-eval handle the nested lists

;;; ****m* sndfilenet/preload-cues
;;; DESCRIPTION
;;; Return a list of preload commands for each sound file in a form that a Max
;;; sflist~ can process and store (which will include the path and start/stop
;;; data: see sndfile-ext::max-cue for details).
;;; 
;;; E.g. preload 231 triplet-mphonic-001.wav 0. 3300.612305 0 1. 
;;; 
;;; ARGUMENTS
;;; - the sndfilenet object.
;;; 
;;; RETURN VALUE
;;; The list of preload strings that osc-call can then pass on to the sflist~
;;; 
;;; SYNOPSIS
#+(and darwin sbcl)
(defmethod preload-cues ((sfn sndfilenet))
;;; ****
  ;; to be sure: don't assume we'll always have non-nested data, even though
  ;; that's a requirement of our parent class 
  (let ((cue-nums 0))
    (loop for ref in (all-refs sfn)
       for snds = (get-data-data ref sfn)
       appending
         (loop for snd in snds
            when (use snd)
            collect
            ;; something like ("preload" 2 "/path/to/snd.wav" 300.0 100.0)
              (prog1
                  (max-cue snd)
                (incf cue-nums))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Fri Dec 21 09:38:46 2012 
;;; ****m* sndfilenet/reset
;;; DESCRIPTION
;;; Reset the followers' slot circular list to the beginning or to <where> and
;;; the starting sounds for each group also.
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
(defmethod reset ((sfn sndfilenet) &optional (where 0) (warn t))
;;; ****
  ;; (format t "~&resetting sndfilenet ~a" (id sfn))
  (loop for ref in (all-refs sfn)
     for snds = (get-data-data ref sfn)
     do
       (loop for snd in snds do
            (setf (group-id snd) ref)
          ;; resets the sndfile-ext object's followers slot to <where>
            (reset snd where warn)))
  ;; MDE Fri Oct 20 20:11:48 2017
  ;; (setf (next-sfes sfn) (first (data (get-first sfn))))
  ;; MDE Tue Nov 10 19:27:01 2020, Heidhausen -- next-sfes is now a list of
  ;; sndfile-ext objects that we start with (and then extract the next to play
  ;; from its followers slot). Start with the sfe at position where (or modulo'd
  ;; at least)
  (setf (next-sfes sfn) (loop for ref in (all-refs sfn)
                           for sfe-list = (get-data-data ref sfn)
                           when sfe-list collect
                             (nth (mod where (length sfe-list))
                                  sfe-list)))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
;;; ****m* sndfilenet/(setf next-sfes)
;;; DATE
;;; October 21st 2017, Essen
;;; 
;;; DESCRIPTION
;;; A setf :after method. Set the next-sfes slot to be a sndfile-ext object. If
;;; it is already, leave it as is. If it's an integer, assume it's a cue number
;;; and get the right object, otherwise get the object with this ID.
;;;
;;; If this is causing problems, use (setf (slot-value sfn 'next-sfes) ... )
;;;
;;; If we're trying to set using a reference/id like '(group1 snd3) then it
;;; will have to be a two-element list.
;;; 
;;; SYNOPSIS
(defmethod (setf next-sfes) :after (value (sfn sndfilenet))
;;; ****
  (let ((sfe (typecase value
               (integer (get-snd-with-cue-num sfn value))
               (list (get-snd (first value) (second value) sfn))
               ((or symbol string)
                (error "sndfilenet::(setf next-sfes): Both group and ~
                        sound file ID are needed: ~a" value))
               (t nil)))) ; so if it's an sfe already, leave it alone
    (when sfe 
      (setf (slot-value sfn 'next-sfes) sfe))))
|#
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
  (let ((result nil))
    (loop for ref in (all-refs sfn)
       for snds = (get-data-data ref sfn)
       do
       (loop for snd in snds do
            (when (= (cue-num snd) cue-num)
              (setf result snd)
              (return))))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* sndfilenet/max-play-from-id
;;; DESCRIPTION
;;; This generates the data necessary to play the next sound in the current
;;; sound's followers list. See the sndfile-ext method for details.
;;; 
;;; ARGUMENTS
;;; - The sndfilenet object.
;;; - The group ID to play the next sound from, i.e. the topmost ID to group of
;;;   sounds 
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
(defmethod max-play-from-id ((sfn sndfilenet) id fade-dur max-loop start-next
                             &optional print)
;;; ****
  ;; NB next-sfes is a list of sndfile-ext objects
  (if (next-sfes sfn)                    
      (let* ((id-pos (position id (all-refs sfn))))
        (if id-pos
            (let* ((current (nth id-pos (next-sfes sfn)))
                   (next (get-next current)))
              (setf (nth id-pos (next-sfes sfn)) next)
              (when print
                (format t "~&sndfilenet::max-play: cue ~a (~a): ~a --> ~a"
                        (cue-num current) (id current) (start current)
                        (end current)))
              (max-play current fade-dur max-loop start-next print))
            (warn "max-play-from-id: can't find ~a in ~a" id (id sfn))))
      (warn "sndfilenet::max-play: no next-sfes!")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; check that follower references refer to other sndfiles in the palette, then
;;; replace the references with the sndfile object themselves, but only when
;;; their <use> slot is T.  This is perhaps memory intensive but it'll save
;;; some CPU cycles by processing here rather than when max asks for the next
;;; sndfile.
(defmethod process-followers ((sfn sndfilenet) &optional
                                                 (on-fail #'error))
  (let ((result t))
    (loop for ref in (all-refs sfn)
       for snds = (get-data-data ref sfn)
       do
         (loop for snd in snds with follower with fsnd do
              (if (followers snd)
                  (setf (followers snd) ; convert symbols to sndfile-exts
                        (loop for i below (sclist-length (followers snd)) do
                             (setf fsnd nil)
                             (loop for j from i
                                below (sclist-length (followers snd))
                                do
                                  (setf follower (get-nth j (followers snd))
                                        fsnd (get-snd-short sfn follower snd))
                                ;; (print follower) (print fsnd)
                                ;; MDE Sat Dec 22 20:36:14 2012 -- got to make
                                ;; sure the user actually wants to use this
                                ;; sound in this piece
                                  (when (and fsnd (use fsnd))
                                    (return fsnd)))
                             (when (and follower (not fsnd))
                               (setf result nil)
                               (when on-fail
                                 (funcall 
                                  on-fail "sndfilenet::process-followers: ~
                                           No such sound file (~a)~%in group ~a"
                                  follower ref)))
                           when (and fsnd (use fsnd)) collect fsnd))
                  (when on-fail
                    (funcall on-fail 
                             "sndfilenet::process-followers: ~a ~%  has no ~
                        followers so if triggered will cause max-play to stop."
                     (id snd))))))
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
;;; OPTIONAL ARGUMENTS
;;; - a list of references for which auto-followers should be generated. Default
;;; = NIL = all groups (which applies also if T is passed here).
;;; 
;;; RETURN VALUE
;;; T
;;;
;;; SYNOPSIS
(defmethod auto-followers ((sfn sndfilenet) &optional refs)
;;; ****
  (when (or (eq refs t) (not refs)) (setq refs (all-refs sfn)))
  (let ((al (make-al)))
    ;; MDE Sat Jan 30 12:59:56 2021, Heidhausen -- changed the method of
    ;; selecting followers here as merely reusing the list in original order
    ;; wasn't creating enough variety.
    (flet ((make-followers (list)
             ;;(print 'make-followers) (print (mapcar #'id list))
             (let ((count 0)
                   (cscl (make-cscl list))
                   (len (length list))
                   (result '()))
               ;; use circularly in case not enough elements
               (loop for sf = (get-next cscl) 
                  until (= count (if (> len 6) 6 len)) ; max 6 followers
                  for f = (when (active al 5) sf)
                  do
                    (when (and f (not (member f result :test
                                              #'(lambda (sf1 sf2)
                                                  (string= (path sf1)
                                                           (path sf2))))))
                      (push f result)
                      (incf count)))
               ;; (print (mapcar #'id result))
               result)))
      (loop for ref in refs             ; i.e. for each sndfile group
         for next-ref in (wrap-list refs 1)
         for sfs = (get-data-data ref sfn)
         for nsfs = (make-followers (get-data-data next-ref sfn))
         do
           (if nsfs
               (setq nsfs (make-cscl nsfs :copy nil))
               (error "sndfilenet::auto-followers: couldn't get followers ~
                       for next-ref (~a)" next-ref))
           (loop for sf in sfs
              for followers = (reverse
                               (cons
                                ;; remember that the reference to the snd in
                                ;; another group must be a list using the group
                                ;; id as first element
                                (list next-ref (get-next nsfs))
                                (remove-with-id (make-followers sfs) (id sf))))
              do
                (unless followers
                  (error "sndfilenet::auto-followers: couldn't get any ~
                          followers for ~a" (id sf)))
                (setf (slot-value sf 'followers)
                      ;; just the ids
                      (make-cscl
                       ;; now just get the IDs: if we store the sndfile-ext
                       ;; objects themselves here, then some will have empty
                       ;; :followers as they're only updated later (sometimes
                       ;; I'd really like proper pointers in lisp :/ )
                       (mapcar #'(lambda (x)
                                   ;; 
                                   (if (listp x)
                                       (list (first x) (id (second x)))
                                       (id x)))
                               followers)))))))
  (process-followers sfn)
  t)

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
     ;; an equal spread of all sndfiles in the palette would be ideal but let's
     ;; not worry until one of those is played twice as many times as that 
     with threshold = (round (* 2.0 (/ depth (num-snds sfn))))
     for ref in (all-refs sfn)
     for snds = (get-data-data ref sfn)
     do
     (loop for snd in snds
        for sndaf = (analyse-followers snd depth)
        for max = (second (first sndaf))
        for this-ok = (<= max threshold)
        do
        ;; (print sndaf)
        (unless this-ok
          (warn "sndfilenet::analyse-followers: (~a ~a) ~%generates ~
                 unbalanced results, e.g. ~a ~%occurs ~a/~a times ~
                 (threshold ~a). ~%(Bear in mind that followers of followers ~
                 are followed.) ~&~a"
                ref (id snd) (first (first sndaf)) max depth threshold
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
;;; - :auto-followers. T or NIL to indicate whether the auto-followers method
;;;   should be called once the object is initialised. Note that if this is T,
;;;   then all the followers slots of the sndfile-ext objects will be deleted as
;;;   part of the process. Default = NIL.
;;; - :analyse-followers. T or NIL to indicate whether to call the
;;;   analyse-followers after initialisation (and possibly auto-followers is
;;;   called). Default = NIL.
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
                          auto-freq (warn-not-found t)
                          auto-followers analyse-followers)
;;; ****
  (let ((sfn (make-instance
              'sndfilenet :id id :data sfn :paths paths
              :extensions extensions
              :auto-freq auto-freq
              :warn-not-found warn-not-found)))
    ;; MDE Wed Jan  6 14:27:03 2021, Heidhausen
    (when auto-followers (auto-followers sfn auto-followers))
    (when analyse-followers (analyse-followers sfn))
    sfn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sndfilenet-p (thing)
  (typep thing 'sndfilenet))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF sndfilenet.lsp
