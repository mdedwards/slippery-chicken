;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* palette/sndfile-palette
;;; NAME 
;;; sndfile-palette
;;;
;;; File:             sndfile-palette.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> sclist -> 
;;;                   circular-sclist -> assoc-list -> recursive-assoc-list ->
;;;                   palette -> sndfile-palette
;;;
;;; Version:          1.0.7
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Implementation of the sndfile-palette class, which is a
;;;                   simple palette that checks that all the sound files given
;;;                   in a list for each id exist.  See comments in methods for
;;;                   limitations and special features of this class.
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    18th March 2001
;;;
;;; $$ Last modified: 10:35:19 Thu Feb 25 2016 GMT
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :slippery-chicken)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NB Although this class is a palette and therefore a subclass of
;;; recursive-assoc-list, the sound lists in this case cannot be nested.

(defclass sndfile-palette (palette)
  ;; snds are given as single names, without the path and without the extension
  ;; so give the paths here and the extensions below (when necessary).
  ((paths :accessor paths :type list :initarg :paths :initform nil)
   ;; MDE Fri Jan 4 10:09:06 2013 -- remember: the num-data slot counts the
   ;; number of named objects in the ral but each of these may be a list of
   ;; several sndfiles so num-data and num-snds are not necessarily the same.
   (num-snds :accessor num-snds :type integer :initform -1)
   ;; MDE Fri Sep 25 13:46:28 2015 -- call CLM's autoc for auto detection of
   ;; frequency of each sndfile where the :frequency slot isn't given?
   (auto-freq :accessor auto-freq :type boolean :initarg :auto-freq
              :initform nil)
   ;; the next sndfile-ext object for the purposes of the OSC sndfilenet
   ;; functionality 
   (next :accessor next :initarg :next :initform nil)
   ;; whether we'll have the followers slots for the sndfiles i.e. whether
   ;; we're going to call max-play (and hence process-followers) for this
   ;; palette.  Leave at nil if you don't want to use with max-play.
   (with-followers :accessor with-followers :type boolean
                   :initarg :with-followers :initform nil)
   (extensions :accessor extensions :type list :initarg :extensions 
               :initform '("wav" "aiff" "aif" "snd"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone ((sfp sndfile-palette))
  (clone-with-new-class sfp 'sndfile-palette))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone-with-new-class :around ((sfp sndfile-palette) new-class)
  (declare (ignore new-class))
  (let ((palette (call-next-method)))
    (setf (slot-value palette 'paths) (paths sfp)
          (slot-value palette 'num-snds) (num-snds sfp)
          (slot-value palette 'auto-freq) (auto-freq sfp)
          (slot-value palette 'with-followers) (with-followers sfp)
          (slot-value palette 'next) (when (next sfp)
                                       (clone (next sfp)))
          (slot-value palette 'extensions) (extensions sfp))
    palette))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod print-object :before ((sfp sndfile-palette) stream)
  (format stream "~%SNDFILE-PALETTE: paths: ~a~
                  ~%                 extensions: ~a~
                  ~%                 num-snds: ~a~
                  ~%                 auto-freq: ~a~
                  ~%                 with-followers: ~a~
                  ~%                 next: ~a"
          (paths sfp) (extensions sfp) (num-snds sfp) (auto-freq sfp)
          (with-followers sfp) (next sfp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NB Although this class is a palette and therefore a subclass of
;;; recursive-assoc-list, the sound lists in this case cannot be nested!

#+clm
(defmethod verify-and-store :after ((sfp sndfile-palette))
  (setf (paths sfp) (loop for path in (paths sfp) 
                       collect (trailing-slash path))
        ;; MDE Wed Jun 12 13:49:33 2013 -- to avoid duplicate path errors
        (paths sfp) (remove-duplicates (paths sfp) :test #'string=))
  (loop for sflist in (data sfp) and i from 0 do
       (loop for snd in (data sflist) and j from 0 do 
            (setf (nth j (data (nth i (data sfp))))
                  ;; MDE Fri Oct  5 13:57:51 2012 -- if it's already a sndfile
                  ;; (as when combining palettes) then don't try to re-parse it 
                  (typecase snd
                    (sndfile snd)
                    ;; if a list was given then the first in the list is the
                    ;; sound file (name only or full path) plus any other slots
                    ;; which need to be initialised for this sound.  We still
                    ;; need to find the sound though, hence the funny list arg
                    ;; passed to make-sndfile.
                    (list
                     ;; MDE Sun Dec 16 20:19:30 2012 -- was make-sndfile
                     (make-sndfile-ext (list (find-sndfile sfp (first snd))
                                             snd)))
                    ;; if it wasn't a list, just find the sound and pass this
                    ;; and the given name which also acts as the id per
                    ;; default. 
                    ;; MDE Sun Dec 16 20:19:30 2012 -- was make-sndfile
                    (t (make-sndfile-ext
                        (find-sndfile sfp snd) :id snd
                        ;; MDE Fri Sep 25 13:49:09 2015 
                        :frequency (if (auto-freq sfp) 'detect 'c4)))))))
  (auto-cue-nums sfp)
  (reset sfp)
  ;; MDE Sat Dec 22 20:59:44 2012 
  (when (with-followers sfp)
    (process-followers sfp))
  ;; MDE Fri Jan  4 10:10:22 2013 
  ;; (print 'num-snds)
  (setf (num-snds sfp) (count-snds sfp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Fri Jan  4 10:49:46 2013 -- must explicitly update the num-snds slot
;;; when combining as verify-and-store (which also updates this) will be called
;;; before relinking. 

(defmethod combine :around ((sfp1 sndfile-palette) (sfp2 sndfile-palette))
  (declare (ignore sfp1 sfp2))
  ;; (print 'sfp-combine)
  (let ((result (call-next-method)))
    (setf (num-snds result) (count-snds result))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Thu May  3 12:51:13 BST 2012: Added/edited robodoc entry

;;; MDE's original comment:
;;; find a sound file in any of the directories given in the paths slot, not
;;; necessarily including those in the palette itself!!!  

;;; ****m* sndfile-palette/find-sndfile
;;; DESCRIPTION
;;; Return the full directory path and file name of a specified sound file,
;;; from within the directories given in the PATHS slot.
;;;
;;; ARGUMENTS
;;; - A sndfile-palette object.
;;; - The name of a sound file from within that object. This can be a string or
;;;   a symbol. Unless it is given as a string, it will be handled as a symbol
;;;   and will be converted to lowercase. Inclusion of the file extension is
;;;   optional. 
;;;
;;; RETURN VALUE
;;; Returns the full directory path and file name of the specified sound file
;;; as a string.
;;; 
;;; EXAMPLE
#|
(let ((msfp (make-sfp 'sfp-test 
                      '((sndfile-group-1
                         (test-sndfile-1))
                        (sndfile-group-2
                         (test-sndfile-2 test-sndfile-3 
                          (test-sndfile-4 :frequency 261.61)))
                        (sndfile-group-3
                         ((test-sndfile-5 :start 0.006 :end 0.182) 
                          test-sndfile-6)))
                      :paths
                      '("/path/to/sndfiles-dir-1"
                        "/path/to/sndfiles-dir-2"))))
 (find-sndfile msfp 'test-sndfile-4))

=> "/path/to/sndfiles-dir-2/test-sndfile-4.aiff"

|#
;;; SYNOPSIS
(defmethod find-sndfile ((sfp sndfile-palette) sndfile)
;;; ****
  (let ((files '())
        (full-path "")
        (string (if (stringp sndfile)
                    sndfile
                    (string-downcase (string sndfile)))))
    (if (search "/" string)
        (when (probe-file string)
          (push string files))
        (loop for path in (paths sfp) do
             (if (pathname-type string)
                 (progn
                   (setf full-path (format nil "~a~a"
                                           path string))
                   (when (probe-file full-path)
                     (push full-path files)))
                 (loop for extension in (extensions sfp) do
                      (setf full-path (format nil "~a~a.~a"
                                              path string extension))
                      (when (probe-file full-path)
                        (push full-path files))))))
    (case (length files)
      ;; MDE Mon Apr 9 12:29:26 2012 -- changing from warn to error as this
      ;; is a show-stopper if we call clm-play.
      (0 (error "sndfile-palette::find-sndfile: ~
                 Cannot find sound file '~a'"
                string))
      (1 (first files))
      (t (warn "sndfile-palette::find-sndfile: Sound file '~a' exists in ~
                ~%more than one folder or with more than one extension.  ~
                ~%Please give the full path in your sndfile-palette: ~&~a" 
               string files)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Convenience function as we don't really ever want the named-object as we do
;;; with other palettes.

(defmethod get-snds (id (sfp sndfile-palette))
  (let ((obj (get-data id sfp)))
    (when obj (data obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This class is a little different as we can fake association-list type
;;; functionality by pulling out a sound with a certain id from the sound list.
;;; If the sound list was a proper assoc-list however, we couldn't have the
;;; same sound twice in the list, which would be inconvenient.  Hence we fake
;;; it here.  In this case, we pull out the first sound we see with an id
;;; match.

(defmethod get-snd (id snd-id (sfp sndfile-palette))
  (let* ((list (get-snds id sfp))
         (result (loop for i in list when (id-eq snd-id i) 
                       do (return i))))
    (when (and (warn-not-found sfp) (not result))
      (warn "sndfile-palette::get-snd: ~
             Couldn't find data with id ~a, snd-id ~%~a in sndfile-palette ~%~a"
            id snd-id sfp))
    result))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Wed Dec 19 14:29:35 2012 -- for MaxMSP/OSC interface

;;; ****m* sndfile-palette/auto-cue-nums
;;; DESCRIPTION
;;; Set the cue-num slot of every sndfile-ext object in the palette to be an
;;; ascending integer starting at 2. NB If a sndfile has it's :use slot set to
;;; NIL it won't be given a cue number.
;;; 
;;; ARGUMENTS
;;; - a sndfile-palette object.
;;; 
;;; RETURN VALUE
;;; The cue number of the last sndfile-ext object.
;;; 
;;; SYNOPSIS
(defmethod auto-cue-nums ((sfp sndfile-palette))
;;; ****
  ;; to be sure: don't assume we'll always have non-nested data.
  (let ((refs (get-all-refs sfp)) 
        (cue-num 1))
    (loop for ref in refs 
         for snds = (get-data-data ref sfp)
         do
         (loop for snd in snds do
              (when (use snd)
                (setf (cue-num snd) (incf cue-num)))))
    cue-num))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* sndfile-palette/osc-send-cue-nums
;;; DESCRIPTION
;;; Send via OSC the cue number of each sound file in a form that a Max sflist~
;;; can process and store.
;;; 
;;; ARGUMENTS
;;; - the sndfile-palette object.
;;; 
;;; RETURN VALUE
;;; The number of cue numbers sent.  NB This is not the same as the last cue
;;; number as cues start from 2.
;;; 
;;; SYNOPSIS
#+(and darwin sbcl)
(defmethod osc-send-cue-nums ((sfp sndfile-palette))
;;; ****
  ;; to be sure: don't assume we'll always have non-nested data.
  (let ((refs (get-all-refs sfp)) 
        (cue-nums 0))
    (loop for ref in refs 
         for snds = (get-data-data ref sfp)
         do
         (loop for snd in snds do
              (when (use snd)
                (sb-bsd-sockets::osc-send-list (max-cue snd) nil) ; no warning 
                (incf cue-nums))))
    cue-nums))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Fri Dec 21 09:38:46 2012 

;;; ****m* sndfile-palette/reset
;;; DESCRIPTION
;;; Reset the followers' slot circular list to the beginning or to <where>
;;; 
;;; ARGUMENTS
;;; - the sndfile-palette object.
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
(defmethod reset ((sfp sndfile-palette) &optional where (warn t))
;;; ****
  (let ((refs (get-all-refs sfp)))
    (loop for ref in refs 
       for snds = (get-data-data ref sfp)
       do
       (loop for snd in snds do
            (setf (group-id snd) ref)
            (reset snd where warn))))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Fri Dec 21 09:38:52 2012

;;; ****m* sndfile-palette/get-snd-with-cue-num
;;; DESCRIPTION
;;; Return the (first, but generally unique) sndfile object which has the
;;; given cue-num slot.
;;; 
;;; ARGUMENTS
;;; - the sndfile-palette object.
;;; - the cue number (integer).
;;; 
;;; RETURN VALUE
;;; The sndfile/sndfile-ext object with the given cue number or NIL if it can't
;;; be found.
;;; 
;;; SYNOPSIS
(defmethod get-snd-with-cue-num ((sfp sndfile-palette) cue-num)
;;; ****
  (let ((refs (get-all-refs sfp))
        (result nil))
    (loop for ref in refs 
       for snds = (get-data-data ref sfp)
       do
       (loop for snd in snds do
            (when (= (cue-num snd) cue-num)
              (setf result snd)
              (return))))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****m* sndfile-palette/max-play
;;; DESCRIPTION
;;; This generates the data necessary to play the next sound in the current
;;; sound's followers list.  See the sndfile-ext method for details.
;;; 
;;; ARGUMENTS
;;; - The sndfile-palette object.
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
(defmethod max-play ((sfp sndfile-palette) fade-dur max-loop start-next
                     &optional print)
;;; ****
  (if (next sfp)
    (let* ((current (next sfp))
           (next (get-next current)))
      (setf (next sfp) next)
      (when print
        (format t "~&cue ~a (~a): ~a --> ~a"
                (cue-num current) (id current) (start current) (end current)))
      (max-play current fade-dur max-loop start-next))
    (warn "sndfile-palette::max-play: no next!")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Sat Dec 22 20:42:32 2012 -- if we don't fully reference the group we
;;; assume it's in  the same as the current.
(defmethod get-snd-short ((sfp sndfile-palette) ref (current sndfile-ext))
  (let* ((snd-id ref)
         (next-group (group-id current)))
    (when (listp ref)
      (if (= 1 (length ref))
          (setf snd-id (first ref))
          (setf snd-id (second ref)
                next-group (first ref))))
    (get-snd next-group snd-id sfp)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; check that follower references refer to other sndfiles in the palette, then
;;; replace the references with the sndfile object themselves, but only when
;;; their <use> slot is T.  This is perhaps memory intensive but it'll save
;;; some CPU cycles by processing here rather than when max asks for the next
;;; sndfile.
(defmethod process-followers ((sfp sndfile-palette) &optional
                                                      (on-fail #'error))
  (let ((refs (get-all-refs sfp))
        (result t))
    (loop for ref in refs 
       for snds = (get-data-data ref sfp)
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
                                    fsnd (get-snd-short sfp follower snd))
                              ;; MDE Sat Dec 22 20:36:14 2012 -- got to make
                              ;; sure the user actually wants to use this sound
                              ;; in this piece
                              (when (and fsnd (use fsnd))
                                (return fsnd)))
                           (when (and follower (not fsnd))
                             (setf result nil)
                             (when on-fail
                               (funcall 
                                on-fail "sndfile-palette::process-followers: ~
                                         No such sound file: ~a"
                                follower)))
                           when (and fsnd (use fsnd)) collect fsnd))
                (warn "sndfile-palette::process-followers: ~a has no followers ~
                       so if triggered will cause max-play to stop."
                      (id snd)))))
    (reset sfp)
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* sndfile-palette/analyse-followers
;;; DESCRIPTION

;;; Using the followers slots of each sndfile in the palette, go through each
;;; sndfile in the palette and generate a large number of following sounds,
;;; i.e. emulate the max-play.  The results of the follow-on process are then
;;; analysed and a warning will be issued if any sndfile seems to dominate
;;; (defined as being present at least twice as many times as its 'fair share',
;;; where 'fair share' would mean an even spread for all the sound files in the
;;; palette).
;;; 
;;; ARGUMENTS
;;; - The sndfile-palette object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; How many times to repeat the generation process.  Default = 1000.
;;; 
;;; RETURN VALUE
;;; T or NIL depending on whether the analysis detects an even spread or not.
;;; 
;;; SYNOPSIS
(defmethod analyse-followers ((sfp sndfile-palette) &optional (depth 1000))
;;; ****
  (loop with ok = t
     with refs = (get-all-refs sfp)
     ;; an equal spread of all sndfiles in the palette would be ideal but let's
     ;; not worry until one of those is played twice as many times as that 
     with threshold = (round (* 2.0 (/ depth (num-snds sfp))))
     for ref in refs
     for snds = (get-data-data ref sfp)
     do
     (loop for snd in snds
        for sndaf = (analyse-followers snd depth)
        for max = (second (first sndaf))
        for this-ok = (<= max threshold)
        do
        (unless this-ok
          (warn "sndfile-palette::analyse-followers: (~a ~a) ~%generates ~
                 unbalanced results, e.g. ~a ~%occurs more than ~a times: ~&~a"
                ref (id snd) (first (first sndaf)) threshold sndaf)
          (setf ok nil)))
     finally (return ok)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Thu Jan  3 17:32:32 2013 -- generally used transparently by
;;; e.g. verify-and-store to set the num-snds slot, but can of course be called
;;; by the user if deemed necessary.  However, the combine and other
;;; destructive methods should call this implicitly to set the num-snds slot so
;;; it shouldn't be necessary to call explicitly.
(defmethod count-snds ((sfp sndfile-palette))
  (loop with result = 0
     for ref in (get-all-refs sfp)
     do (incf result (length (get-data-data ref sfp)))
     finally (return result)))
              
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-nearest (freq (sfp sndfile-palette) &rest ids)
  (unless (numberp freq)
    (error "sndfile-palette::get-nearest: freq should be numeric: ~a~%~a"
           freq sfp))
  (unless ids (setq ids (get-all-refs sfp)))
  (get-nearest-by-freq freq
                       (loop for id in ids appending (get-data-data id sfp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod print-for-init ((sfp sndfile-palette) &key (stream t))
  (call-next-method ; assoc-list method
   sfp
   :stream stream
   :data-printer #'(lambda (list stream)
                     (princ "(" stream)
                     (loop for sf in list do
                          (print (get-slots-list sf) stream))
                     (princ ")" stream))
   :call 'make-sfp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* sndfile-palette/set-frequency-from-filename
;;; DATE
;;; October 1st 2015, Edinburgh
;;; 
;;; DESCRIPTION
;;; Set the frequency slot of every sound file object in the palate to a value
;;; extracted from the file name. This is intended but of course not restricted
;;; to palettes created from sample libraries, where, as is often the case, the
;;; file name of each sample contains the pitch of the file. For example, from
;;; the Akoustik Piano sample library we have the file
;;; Stein-R(A1)-V(220)-dB(2446)-T(MF)-M(123)-P(404-03-01).wav. In this case the
;;; A1 in the first parentheses indicate the pitch. By writing and passing a
;;; short parsing function to read the pitch out of such a file name we can
;;; easily process each sound file in the palette. See akoustik-piano-name
;;; below for an example of how to process such file names.
;;;
;;; See also make-sfp-from-folder.
;;; 
;;; ARGUMENTS
;;; - The sound file palette object
;;; 
;;; OPTIONAL ARGUMENTS
;;; Keyword arguments:
;;; - A single group or list of groups (IDs) for the sound files we wish to
;;;   process.
;;; - The functionFor processing a single name. Of course the names must be
;;;   consistent and each sound file must be able to be processed by this
;;;   single function. NB for reasons of similar usage in get-spectra-al (see
;;;   spectra.lsp) this function actually must return the MIDI note number (may
;;;   be a floating point for microtonal applications), which is then converted
;;;   by the sndfile class to a frequency in Hertz.
;;; - The function to be called when the name function cannot determine the
;;;   frequency of the sound file. This could be #'error (the default), #'warn,
;;;   or nil if nothing is to be done on failure.
;;; 
;;; RETURN VALUE
;;; The sndfile-palette object after processing.
;;; 
;;; SYNOPSIS
(defmethod set-frequency-from-filename
    ((sfp sndfile-palette)
     &key groups (name-fun #'akoustik-piano-name) (on-error #'error))
;;; ****
  (setq groups (if groups (force-list groups)
                   (get-all-refs sfp)))
  (loop for ref in groups for group = (get-data-data ref sfp) do
       (loop for sf in group do
            (unless (set-frequency-from-filename sf :name-fun name-fun)
              (when (functionp on-error)
                (funcall on-error "~&Can't set frequency in ~a" sf)))))
  sfp)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun akoustik-piano-name (name)
  (when name
    (let ((zero-octave (char= #\- (elt name 9))))
      ;; samples notes use middle C = C3
      (+ (note-to-midi (read-from-string
                        (subseq name 8 (if zero-octave 11 10))))
         12))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Thu May  3 11:39:21 BST 2012: Added robodoc entry

;;; ****f* sndfile-palette/make-sfp
;;; DESCRIPTION
;;; Make a sndfile-palette object. This object is a simple palette which checks
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
;;;   sndfile-palette already contains ("wav" "aiff" "aif" "snd"), so this
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
(let ((msfp (make-sfp 'sfp-test 
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
(defun make-sfp (id sfp &key paths (extensions '("wav" "aiff" "aif" "snd"))
                          auto-freq with-followers (warn-not-found t))
;;; ****
  (make-instance 'sndfile-palette :id id :data sfp :paths paths
                 :with-followers with-followers :extensions extensions
                 :auto-freq auto-freq
                 :warn-not-found warn-not-found))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Wed Jun 13 17:35:43 BST 2012: Added robodoc entry

;;; ****f* sndfile-palette/make-sfp-from-wavelab-marker-file
;;; DESCRIPTION
;;; Automatically create a sndfile-palette object from the specified wavelab
;;; marker file and the specified sound file (from which the marker file must
;;; have been generated).
;;;
;;; The function will produce a sndfile-palette object with multiple groups,
;;; each of which consists of the number of sound file segments specified using
;;; the :snds-per-group argument (defaults to 8). By default the segments will
;;; be collected into the groups in chronological order. If the optional
;;; :random-every argument is given a value, every nth group will consist of
;;; random segments instead.
;;;
;;; The sound file segments of each group will correspond to the time points
;;; stored in the marker file. 
;;;
;;; The <marker-file> argument can consist of a list of marker files, in which
;;; case these would first be concatenated. 
;;;
;;; NB: Be aware that marker files created on operating systems differing from
;;;     the one on which this function is called might trigger errors due to
;;;     newline character mismatches.
;;; 
;;; ARGUMENTS
;;; - A string that is the name of the marker file, including the directory
;;;   path and extension.
;;; - A string that is the name of the sound file. This can either be a full
;;;   directory path, file name, and extension, or just a base file name. If
;;;   the latter, values for the optional arguments :paths and :extensions must
;;;   also be specified.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :snds-per-group. An integer that is the number of sound file segments to
;;;   include in each group. Default = 8.
;;; - :random-every. An integer to indicate that every nth group is to consist
;;;   of random (rather than chronologically consecutive) sound file segments.
;;;   Default = 999999 (i.e. essentially never) 
;;; - :paths. NIL or a list of strings that are the directory paths to the
;;;   specified sound files. If the sound file is passed with the directory
;;;   path, this must be set to NIL. NB: The paths given here apply only to the
;;;   sound files, not to the marker files. Default = NIL.
;;; - :sampling-rate. An integer that is the sampling rate of the specified
;;;   sound file. Changing this value will alter the start-times determined for
;;;   each sound segment. Default = 44100.
;;; - :extensions. A list of strings that are the extensions to the given sound
;;;   files. If the sound files are passed with their extensions, this must be
;;;   set to NIL. Default = NIL.
;;; - :warn-not-found. T or NIL to indicate whether to print a warning to the
;;;   listener if the specified sound file is not found. T = print a
;;;   warning. Default = NIL.
;;; - :name. The name for the overall sndfile-palette and the base name for
;;;    each group within (these will have a suffix that is an auto-incrementing
;;;    number e.g. 'auto would become 'auto1 'auto2 etc.).  Default = 'auto.
;;; 
;;; RETURN VALUE
;;; A sndfile-palette object.
;;; 
;;; EXAMPLE
#|
(make-sfp-from-wavelab-marker-file 
  "/path/to/24-7.mrk"
 "24-7"
 :snds-per-group 2
 :random-every 3
 :paths '("/path/to/sound-file/directory/")
 :sampling-rate 44100
 :extensions '("wav"))

=>
SNDFILE-PALETTE: paths: (/Volumes/JIMMY/SlipperyChicken/sc/test-suite/)
                 extensions: (wav)
PALETTE: 
RECURSIVE-ASSOC-LIST: recurse-simple-data: T
                      num-data: 8
                      linked: NIL
                      full-ref: NIL
ASSOC-LIST: warn-not-found NIL
CIRCULAR-SCLIST: current 0
SCLIST: sclist-length: 8, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: AUTO, tag: NIL, 
data: (
NAMED-OBJECT: id: "auto1", tag: NIL, 
data: (

SNDFILE: path: /Volumes/JIMMY/SlipperyChicken/sc/test-suite/24-7.wav, 
         snd-duration: 29.652811, channels: 2, frequency: 261.62555
         start: 0.09142857, end: 1.0361905, amplitude: 1.0, duration 0.94476193
         will-be-used: 0, has-been-used: 0
         data-consistent: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: "24-7", tag: NIL, 
data: /Volumes/JIMMY/SlipperyChicken/sc/test-suite/24-7.wav
[...]

|#
;;; SYNOPSIS
(defun make-sfp-from-wavelab-marker-file (marker-file sndfile 
                                          &key
                                          (snds-per-group 8)
                                          (random-every 999999) ;; i.e. never
                                          paths
                                          (sampling-rate 44100)
                                          extensions
                                          ;; MDE Fri Oct  5 14:04:08 2012 
                                          (name 'auto)
                                          warn-not-found)
;;; ****
  ;; do this just to reset the random number generator
  (random-rep 10 t)
  (let* ((snds (parse-wavelab-marker-files-for-sections
                marker-file sndfile :sampling-rate sampling-rate))
         (sndsc (make-cscl snds))
         (num-snds (length snds))
         ;; MDE Tue Jun 19 12:48:44 2012 -- no longer need this var
         ;; (left-over (mod num-snds snds-per-group))
         (sfp
          (loop 
             for count from 1
             with i = 0 
             with sublist
             with group
             with group-num = 0
             with random-group-num = 0
             with do-random
             until (>= i num-snds)
             do
             ;; every so often get a random one!
             (setf do-random (zerop (mod count random-every))
                   sublist 
                   (if do-random
                       (loop for n in
                            (loop repeat snds-per-group collect 
                                 (random-rep num-snds))
                            collect (nth n snds))
                       ;; MDE Tue May 15 14:36:15 2012 -- this fails when
                       ;; there's only a few markers so use a cscl instead.
                       #|
                       (subseq snds (print i)
                               (print (incf i 
                                     (if (zerop left-over)
                                         snds-per-group
                                         (progn
                                           (decf left-over)
                                           (1+ snds-per-group)))))))
                       |#
                       ;; MDE Tue May 15 14:36:15 2012 
                       (loop repeat snds-per-group 
                          do (incf i)
                          collect (get-next sndsc)))
                   sublist (loop for snd in sublist collect
                                (wavelab-section-to-list snd))
                   group (list 
                          (if do-random
                              (format nil "random~a" 
                                      (incf random-group-num))
                              (format nil "~a~a" name (incf group-num)))
                          sublist))
             (when (member nil sublist)
               (error "sndfile-palette::make-sfp-from-wavelab-~
                               marker-file: ~
                               ~% somehow nil got in there as a sound!: ~
                               i =~a (after inc!) ~%~a"
                      i sublist))
             collect group)))
    (make-sfp name sfp 
              :paths paths 
              :extensions extensions 
              :warn-not-found warn-not-found)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; the wavelab-section structure is defined in utilities.lsp

(defun wavelab-section-to-list (wls)
  (list (wavelab-section-sndfile wls)
        :description (wavelab-section-description wls)
        :start (wavelab-section-start wls)
        :end (wavelab-section-end wls)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Thu Jun 14 13:23:14 BST 2012: Added robodoc entry

;;; MDE original comment:
;;; See get-groups below for description of format string in markers.

;;; ****f* sndfile-palette/make-sfp-from-groups-in-wavelab-marker-file
;;; DESCRIPTION
;;; Automatically generate a sndfile-palette object using the specified sound
;;; file from grouping defined in the specified wavelab marker file.
;;;
;;; The <marker-file> argument can be passed as a list of marker files, in
;;; which case these will first be concatenated.
;;; 
;;; ARGUMENTS
;;; - A string that is the name of the marker file, including the directory
;;;   path and extension.
;;; - A string that is the name of the sound file. This can either be a full
;;;   directory path, file name, and extension, or just a base file name. If
;;;   the latter, values for the optional arguments :paths and :extensions must
;;;   also be specified.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - :paths. NIL or a list of strings that are the directory paths to the
;;;   specified sound files. If the sound file is passed with the directory
;;;   path, this must be set to NIL. NB: The paths given here apply only to the
;;;   sound files, not to the marker files. Default = NIL.
;;; - :extensions. A list of strings that are the extensions to the given sound
;;;   files. If the sound files are passed with their extensions, this must be
;;;   set to NIL. Default = NIL.
;;; - :warn-not-found. T or NIL to indicate whether to print a warning to the
;;;   listener if the specified sound file is not found. T = print a
;;;   warning. Default = NIL.
;;; - :sampling-rate. An integer that is the sampling rate of the specified
;;;   sound file. Changing this value will alter the start-times determined for
;;;   each sound segment. Default = 44100.
;;; - :print. T or NIL to indicate whether feedback about the groups found and
;;;   created should be printed to the listener. T = print. Default = T.
;;; 
;;; RETURN VALUE
;;; Returns NIL.
;;; 
;;; EXAMPLE
#|
(make-sfp-from-groups-in-wavelab-marker-file 
 "/path/to/24-7.mrk"
 "24-7"
 :paths '("/path/to/sndfile/directory/")
 :sampling-rate 44100
 :extensions '("wav"))

=>
24 markers read from /path/to/24-7.mrk
Adding tapping: 2.753 -> 4.827
Adding tapping: 5.097 -> 6.581
Adding tapping: 6.763 -> 8.538
Adding splinter: 13.878 -> 15.993
Adding tapping: 16.338 -> 18.261
Adding splinter: 19.403 -> 25.655

tapping: 4 sounds
splinter: 2 sounds

|#
;;; SYNOPSIS
(defun make-sfp-from-groups-in-wavelab-marker-file (marker-file sndfile 
                                                    &key
                                                    paths
                                                    extensions
                                                    warn-not-found
                                                    (sampling-rate 44100)
                                                    (print t))
;;; ****
  (let* ((snds (parse-wavelab-marker-files-for-sections
                marker-file sndfile :sampling-rate sampling-rate))
         (al (make-assoc-list 'from-group-markers nil)))
    (loop 
        for snd in snds
        for groups = (get-groups (wavelab-section-description snd))
        with snd-list
        do
          (when groups
            (setf snd-list (wavelab-section-to-list snd))
            (loop for group in groups do
                  (when print
                    (format t "~&Adding ~a: ~a -> ~a"
                            group
                            (secs-to-mins-secs (wavelab-section-start snd))
                            (secs-to-mins-secs (wavelab-section-end snd))))
                  (if (get-data group al nil)
                      (add-to-list-data snd-list group al)
                    (add (list group (list snd-list)) al)))))
    (setf al (clone-with-new-class al 'sndfile-palette))
    (setf (paths al) paths
          (extensions al) extensions
          (warn-not-found al) warn-not-found)
    (verify-and-store al)
    (when print
      (terpri)
      (terpri)
      (loop for group in (get-all-refs al) do
            (format t "~&~a: ~a sounds" 
                    ;; get-all-refs always returns each ref as a list
                    (first group) (length (data (get-data group al))))))
    al))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  MDE Sat Sep  5 15:08:06 2015
;;; ****f* sndfile-palette/make-sfp-from-folder
;;; DATE
;;; 5th September 2015, Edinburgh
;;; 
;;; DESCRIPTION
;;; Makes a sndfile-palette object from the sound files found in a specific
;;; folder (directory) on the file system. Allows any arbitrary levels of
;;; subfolders with the proviso that they'll be converted to a flat list, using
;;; the subfolders as tags (e.g. dir/subdir1/subdir2 becomes
;;; dir-subdir1-subdir2).
;;;
;;; The folders can contain other files (they'll be ignored). Sound files are
;;; those with extensions .aif .wav .aiff and .snd
;;; 
;;; ARGUMENTS
;;; - the folder path, as a string.
;;;
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :skip. a list of folders to skip i.e. just the last folder name, not the
;;;   complete path. Default = NIL.
;;; - :auto-freq. whether to do automatic frequency detection on the sound
;;;   files. Default = NIL.
;;; - :insist. A single pattern (string) or list of patterns that the sound
;;;   file name must have--just the filename, excluding path/folders and
;;;   extension. If a list then all patterns must be in the file name, not
;;;   just one of them. Default = NIL.
;;; - :resist. Sim. to :insist except this/these are patterns none of which can
;;;   be in the file name. Default = NIL.
;;; 
;;; RETURN VALUE
;;; a sndfile-palette object
;;; 
;;; SYNOPSIS
;;;
;;; 
;;; EXAMPLE
#|
(make-sfp-from-folder "/music/hyperboles/snd/cello/samples/"
                              '("short-percussive" "weird"))
-->
SNDFILE-PALETTE: paths: NIL
                 extensions: (wav aiff aif snd)
                 num-snds: 92
                 with-followers: NIL
                 next: NIL
PALETTE: 
RECURSIVE-ASSOC-LIST: recurse-simple-data: T
                      num-data: 16
                      linked: T
                      full-ref: NIL
ASSOC-LIST: warn-not-found T
CIRCULAR-SCLIST: current 0
SCLIST: sclist-length: 16, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: AUTO, tag: NIL, 
data: (
LINKED-NAMED-OBJECT: previous: NIL, this: (1), next: (10)
NAMED-OBJECT: id: 1, tag: NIL, 
data: (

SNDFILE-EXT: use: T, cue-num: 2, pitch: -1, pitch-curve: -1, bandwidth: -1, 
             bandwidth-curve: -1, continuity: -1, continuity-curve: -1, 
             weight: -1, weight-curve: -1, energy: -1, energy-curve: -1, 
             harmonicity: -1, harmonicity-curve: -1, volume: -1, 
             volume-curve: -1, loop-it: NIL, bitrate: 24, srate: 96000, 
             num-frames: 1719753, bytes: 5159314, group-id: (1)
             followers: NIL

SNDFILE: path: /music/hyperboles/snd/cello/samples/1/g4-III-4-004.aif, 
         snd-duration: 17.914093, channels: 1, frequency: 150.24414
         start: 0.0, end: 17.914093, amplitude: 1.0, duration 17.914093
         will-be-used: 0, has-been-used: 0
         data-consistent: T
...
|#
(defun make-sfp-from-folder (folder &key skip auto-freq insist resist
                                      (default-group 'default-group))
;;; ****
  (let* ((sfs (get-sndfiles folder skip insist resist))
         (groups (get-groups-from-paths sfs folder))
         (pdl (length (trailing-slash folder)))
         sfgroup pos group)
    ;; (print groups)
    ;; MDE Fri Sep 25 15:02:22 2015 -- if we've got sndfiles in the folder
    ;; (i.e. not subfolders) then we have to create a default group 
    (unless groups (setq groups (list default-group)))
    (if sfs
        (progn
          (loop for sf in sfs do
               (setq sfgroup  (get-group-from-file sf pdl)
                     pos (position sfgroup groups
                                   :test #'(lambda (x y)
                                             (if (atom y)
                                                 (eq x y)
                                                 (eq x (first y)))))
                     group  (progn
                              (unless pos (setq group default-group
                                                pos 0))
                              (nth pos groups)))
             ;; (print group) (print pos) (print (nth pos groups))
               (if (atom group)
                   (setf (nth pos groups) (list group (list sf)))
                   (push sf (second (nth pos groups)))))
          ;; (print groups)
          (make-sfp 'auto groups :auto-freq auto-freq))
        (warn "sndfile-palette:make-sfp-from-folder: ~a: no sound files."
              folder))))
         
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* sndfile-palette/kontakt-to-sfp
;;; DATE
;;; October 2nd 2015, Edinburgh
;;; 
;;; DESCRIPTION This function is only available on Mac OSX 64bit Intel machines
;;; and requires the nki executable which since version 1.0.6 comes with
;;; slippery-chicken (in the bin directory).
;;; 
;;; Using the public-domain C programme "nki tool" from the Linux Sampler
;;; project (http://www.linuxsampler.org/nkitool/), convert a Kontakt .nki
;;; sampler file to a sndfile-palette object. This will only include the sound
;;; file and the MIDI note number it is mapped to, but it will mean that the
;;; sndfile objects in the palette include the correct frequency. So using such
;;; a sndfile-palette makes it possible to have clm-play act as a traditional
;;; sampler, though of course much more is possible too.
;;;
;;; The nki file could of course map more than one sample to any MIDI note
;;; (e.g. for handling different velocities). By default all of these could be
;;; included in the sndfile-palette, and the :snd-selector function passed to
;;; clm-play could select the correct file based on velocity, for
;;; instance. However, you may want to filter out some sound files. This is
;;; where the :insist and :resist keyword arguments come in handy (see below).
;;;
;;; NB If you get the error message "zpipe: invalid or incomplete deflate data"
;;; then it's probably because the nki file is in Kontakt 4.2.2 (or higher)
;;; format so can't be converted with this tool. You could try "save as" in
;;; Kontakt 3 or 4 and rerunning this function, as those older programmes will
;;; probably save in the older format (worked for me at least once).
;;; 
;;; ARGUMENTS
;;; - the path to the .nki file to process
;;; - the path to the samples i.e. the sound files which the nki file uses
;;; 
;;; OPTIONAL ARGUMENTS
;;; Keyword arguments:
;;; :insist
;;; - :insist. A single pattern (string) or list of patterns that the sound
;;;   file name must have--just the filename, excluding path/folders and
;;;   extension. If a list then all patterns must be in the file name, not
;;;   just one of them. Default = NIL i.e. accept all.
;;; - :resist. Sim. to :insist except this/these are patterns none of which can
;;;   be in the file name. Default = NIL.
;;; - :group. The group ID that will be assigned to the sound files. Default =
;;;   NIL whereby the name of the nki file will be used.
;;; - :converter. The path to the nki tool executable. Default =
;;;   /path/to/slippery-chicken/bin/nki 
;;; 
;;; RETURN VALUE
;;; 
;;; 
;;; EXAMPLE
#|

|#
;;; SYNOPSIS
#+(and mac-osx X86-64)
(defun kontakt-to-sfp (nki samples-path
                       &key insist resist group
                         (converter
                          (concatenate 'string
                                       cl-user::+slippery-chicken-home-dir+
                                       "bin/nki")))
;;; ****
  (setq insist (force-list insist)
        resist (force-list resist))
  (unless group (setq group (read-from-string (pathname-name nki))))
  (let ((mapping (kontakt-to-coll nki :converter converter))
        (sfp '()))
    (loop for pair in mapping for midi = (first pair)
       for sndfile = (second pair)
       for sfname = (pathname-name sndfile) do
       ;; there may be several files mapped to a single key--velocity
       ;; differences--so we can filter them here.
       ;; (print sndfile) (print midi)
         (when (and (seq-has-all insist sfname)
                    (seq-has-none resist sfname))
           (push (list sndfile :frequency (midi-to-freq midi)) sfp)))
    (make-sfp 'kontakt-to-sfp
              (list (list group (reverse sfp)))
              :paths (list samples-path))))
         
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* sndfile-palette/get-sndfiles
;;; DATE
;;; 5th September 2015, Edinburgh
;;; 
;;; DESCRIPTION
;;; Return a list of the full paths of sound files in the given path. Files
;;; without the extensions aif, wav, aiff, and snd are ignore.
;;; 
;;; ARGUMENTS
;;; The full path to the folder where the sound files are, as a string
;;; 
;;; OPTIONAL ARGUMENTS
;;; - a list of folders to skip i.e. just the last folder name, not the
;;;   complete path
;;; - A single pattern (string) or list of patterns that the sound
;;;   file name must have--just the filename, excluding path/folders and
;;;   extension. If a list then all patterns must be in the file name, not
;;;   just one of them. Default = NIL.
;;; - Sim. to :insist except this/these are patterns none of which can
;;;   be in the file name. Default = NIL.
;;; 
;;; RETURN VALUE
;;; A list of full paths as strings.
;;; 
;;; EXAMPLE
#|
(get-sndfiles "/music/hyperboles/snd/cello/samples/"
              '("short-percussive" "weird"))
-->
("/music/hyperboles/snd/cello/samples/1/g4-III-4-001.aif"
 "/music/hyperboles/snd/cello/samples/1/g4-III-4-002.aif"
 "/music/hyperboles/snd/cello/samples/1/g4-III-4-003.aif"
 "/music/hyperboles/snd/cello/samples/1/g4-III-4-004.aif"
 "/music/hyperboles/snd/cello/samples/10/cs5-I-5-9-13-4-001.aif"
 ... 
|#
;;; SYNOPSIS
(defun get-sndfiles (folder &optional skip insist resist)
  ;;; ****
  (setq insist (force-list insist)
        resist (force-list resist))
  (loop for file in (get-all-files folder skip)
     for name = (pathname-name file) 
     when (and (member (pathname-type file) '("aif" "wav" "aiff" "snd")
                       :test #'string=)
               (seq-has-all insist name)
               (seq-has-none resist name))
     collect file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-groups-from-paths (files parent-dir &optional as-lists)
  (loop with result = '()
     with pdl = (length (trailing-slash parent-dir))
     for file in files
     for group = (get-group-from-file file pdl)
     do
       (pushnew (if as-lists (list group) group) result)
     finally (return (nreverse (remove nil result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-group-from-file (file skip)
  ;; pass the parent-dir instead of the length of the parent-dir, if you like
  (unless (integerp skip)
    (setf skip (length (trailing-slash skip))))
  (let ((pd (parent-dir file)))
    (when (> (length pd) skip)
      (let ((result (subseq pd skip)))
        (read-from-string (substitute #\- #\/ result))))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; groups are indicated in wavelab marker files by the wording
;;; group:g1,g2,g3 etc. The word group can appear anywhere in the
;;; marker description; there can be as many groups indicated as
;;; desired if they are separated by commas; a : must come after
;;; group info if other info is in the description.
;;; e.g. (get-groups "1.3:nice stuff:group:soft, quiet, peaceful:other stuff")
;;; -> ("soft" "quiet" "peaceful")
;;; (get-groups "group:soft") -> ("soft")

(defun get-groups (string)
  (let ((pos (search "group:" string)))
    (when pos
      (let* ((substr (subseq string (+ pos 6))) ;; skip "group:" too
             (next-colon (position #\: substr))
             (groups (if next-colon
                         (subseq substr 0 next-colon)
                       substr))
             (result '()))
        ;; flet can't define recursive functions, labels can
        (labels ((get-groups-aux (string)
                   (when string
                     (multiple-value-bind
                         (group rest)
                         (get-parameter string #\,)
                       (if group
                           (progn
                             ;; (format t "~&get-groups: \"~a\"" group)
                             (push group result)
                             (get-groups-aux rest))
                         ;; NB (get-parameter "three" #\,) -> NIL !!!
                         (push (trim-leading-trailing-whitespace string) 
                               result))))))
          (get-groups-aux groups)
          (nreverse result))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* sndfile-palette/get-nearest-by-freq
;;; DATE
;;; October 1st 2015, Edinburgh
;;; 
;;; DESCRIPTION
;;; Given a frequency and a list of sndfile objects, return the sndfile from
;;; the list whose frequency is closest to the first argument.
;;; 
;;; ARGUMENTS
;;; - a frequency in Hertz (number)
;;; - a simple list of sndfile objects, such as that contained in the data slot
;;;  of a sndfile-palette group.
;;; 
;;; RETURN VALUE
;;; the nearest sndfile object
;;; 
;;; SYNOPSIS
(defun get-nearest-by-freq (freq sflist)
;;; ****
  (let* ((diff most-positive-double-float)
         (cdiff diff)
         it)
    (loop for sf in sflist do
         (when (frequency sf)
           (setq diff (abs (- 1.0 (/ freq (frequency sf)))))
           (when (<= diff cdiff)
             (setq it sf
                   cdiff diff))))
    it))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sndfile-palette-p (thing)
  (typep thing 'sndfile-palette))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF sndfile-palette.lsp


