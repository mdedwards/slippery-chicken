;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;; Version:          1.0.0-beta1
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
;;; $$ Last modified: 16:47:33 Tue May 15 2012 BST
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

;;; 02.12.11 SEAN: changed robodoc header to reflect class hierarchy

(eval-when (compile)
  (declaim (optimize (speed 3) (safety 1) (space 0) (debug 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :slippery-chicken)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass sndfile-palette (palette)
  ;; snds are given as single names, without the path and without the extension
  ;; so give the paths here and the extensions below (when necessary).
  ((paths :accessor paths :type list :initarg :paths :initform nil)
   (extensions :accessor extensions :type list :initarg :extensions 
               :initform '("wav" "aiff" "aif" "snd"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone ((sfp sndfile-palette))
  (clone-with-new-class sfp 'sndfile-palette))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone-with-new-class :around ((sfp sndfile-palette) new-class)
  (declare (ignore new-class))
  (let ((palette (call-next-method)))
    (setf (slot-value palette 'paths) (paths sfp)
          (slot-value palette 'extensions) (extensions sfp))
    palette))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object :before ((sfp sndfile-palette) stream)
  (format stream "~%SNDFILE-PALETTE: paths: ~a~
                  ~%                 extensions: ~a"
          (paths sfp) (extensions sfp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; NB Although this class is a palette and therefore a subclass of
;;; recursive-assoc-list, the sound lists in this case cannot be nested!

#+clm
(defmethod verify-and-store :after ((sfp sndfile-palette))
  (setf (paths sfp) (loop for path in (paths sfp) 
                          collect (trailing-slash path)))
  (loop for sflist in (data sfp) and i from 0 do
        (loop for snd in (data sflist) and j from 0 do 
              (setf (nth j (data (nth i (data sfp))))
                    ;; if a list was given then the first in the list is the
                    ;; sound file (name only or full path) plus any other slots
                    ;; which need to be initialised for this sound.  We still
                    ;; need to find the sound though, hence the funny list arg
                    ;; passed to make-sndfile.
                    (if (listp snd)
                        (make-sndfile (list (find-sndfile sfp (first snd))
                                            snd))
                      ;; if it wasn't a list, just find the sound and pass this
                      ;; and the given name which also acts as the id per
                      ;; default. 
                      (make-sndfile (find-sndfile sfp snd) :id snd))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Thu May  3 12:51:13 BST 2012: Added/edited robodoc entry

;;; MDE's original comment:
;;; find a soundfile in any of the directories given in the paths slot, not
;;; necessarily including those in the palette itself!!!  

;;; ****m* sndfile-palette/find-sndfile
;;; FUNCTION
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
;;; ****                                ;
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
                ~%Please give the full path in your sndfile-palette: ~a" 
               string files)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Convenience function as we don't really ever want the named-object as we do
;;; with other palettes.

(defmethod get-snds (id (sfp sndfile-palette))
  (let ((obj (get-data id sfp)))
    (when obj (data obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This class is a little different as we can fake association-list type
;;; functionality by pulling out a sound with a certain id from the sound
;;; list.  If the sound list was a proper assoc-list however, we couldn't have
;;; the same sound twice in the list, which would be inconvenient.  Hence we
;;; fake it here.
;;; In this case, we pull out the first sound we see with an id match.

(defmethod get-snd (id snd-id (sfp sndfile-palette))
  (let* ((list (get-snds id sfp))
         (result (loop for i in list when (id-eq snd-id i) 
                       do (return i))))
    (when (and (warn-not-found sfp) (not result))
      (warn "sndfile-palette::get-snd: ~
             Couldn't find data with id ~a snd-id ~a in sndfile-palette ~%~a"
            id snd-id sfp))
    result))
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Thu May  3 11:39:21 BST 2012: Added robodoc entry

;;; ****f* sndfile-palette/make-sfp
;;; FUNCTION

;;; Make a sndfile-palette object. This object is a simple palette which checks
;;; to make sure that all of the sound files in a given list exist for each
;;; given ID.
;;;
;;; Sound files are given as as single names, without the path and without the
;;; extension. These can be given using the optional keyword arguments <paths>
;;; and <extensions>.
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
(defun make-sfp (id sfp &key paths extensions (warn-not-found t))
;;; ****
  (if extensions
      (make-instance 'sndfile-palette :id id :data sfp :paths paths
                     :extensions extensions :warn-not-found warn-not-found)
      (make-instance 'sndfile-palette :id id :data sfp :paths paths
                     :warn-not-found warn-not-found)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****f* sndfile-palette/make-sfp-from-wavelab-marker-file
;;; FUNCTION
;;; This creates a sndfile-palette with automatic groups with <snds-per-group>
;;; snds in each auto group and random groups every <random-every>.
;;;
;;; marker-file could be a list of marker files; they would be concatenated.
;;;
;;; NB Beware that marker files created on different operating systems from the
;;; one one which this function is called might trigger errors due to newline
;;; character mismatches.
;;; 
;;; ARGUMENTS
;;; 
;;; 
;;; OPTIONAL ARGUMENTS
;;; 
;;; 
;;; RETURN VALUE
;;; 
;;; 
;;; EXAMPLE
#|

|#
;;; SYNOPSIS
(defun make-sfp-from-wavelab-marker-file (marker-file sndfile 
                                          &key
                                          (snds-per-group 8)
                                          (random-every 999999) ;; i.e. never
                                          paths
                                          (sampling-rate 44100)
                                          extensions
                                          warn-not-found)
;;; ****
  ;; do this just to reset the random number generator
  (random-rep 10 t)
  (let* ((snds (parse-wavelab-marker-files-for-sections
                marker-file sndfile :sampling-rate sampling-rate))
         (sndsc (make-cscl snds))
         (num-snds (length snds))
         (left-over (mod num-snds snds-per-group))
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
                              (format nil "auto~a" (incf group-num)))
                          sublist))
             (when (member nil sublist)
               (error "sndfile-palette::make-sfp-from-wavelab-~
                               marker-file: ~
                               ~% somehow nil got in there as a sound!: ~
                               i =~a (after inc!) ~%~a"
                      i sublist))
             collect group)))
    (make-sfp 'auto sfp 
              :paths paths 
              :extensions extensions 
              :warn-not-found warn-not-found)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; the wavelab-section structure is defined in utilities.lsp

(defun wavelab-section-to-list (wls)
  (list (wavelab-section-sndfile wls)
        :description (wavelab-section-description wls)
        :start (wavelab-section-start wls)
        :end (wavelab-section-end wls)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This creates sfp groups according to the grouping defined in the marker
;;; file. 
;;;
;;; See get-groups below for description of format string in markers.
;;;
;;; marker-file could be a list of marker files; they would be concatenated.

;;; ****f* sndfile-palette/make-sfp-from-groups-in-wavelab-marker-file
;;; FUNCTION
;;; 
;;; 
;;; ARGUMENTS
;;; 
;;; 
;;; OPTIONAL ARGUMENTS
;;; 
;;; 
;;; RETURN VALUE
;;; 
;;; 
;;; EXAMPLE
#|

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sndfile-palette-p (thing)
  (typep thing 'sndfile-palette))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EOF sndfile-palette.lsp


