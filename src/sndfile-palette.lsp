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
;;; Version:          1.1.0
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
;;; $$ Last modified:  17:10:00 Sat Jun 29 2024 CEST
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
   ;; 
   ;; MDE Sat Dec 15 14:58:04 2018 -- this can also be a function whereupon it
   ;; will be called with the path slot of each sndfile as argument. The idea is
   ;; that the fundamental can be extracted from the file name.
   (auto-freq :accessor auto-freq :type boolean :initarg :auto-freq
              :initform nil)
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
          (slot-value palette 'extensions) (extensions sfp))
    palette))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod print-object :before ((sfp sndfile-palette) stream)
  (format stream "~%SNDFILE-PALETTE: paths: ~a~
                  ~%                 extensions: ~a~
                  ~%                 num-snds: ~a~
                  ~%                 auto-freq: ~a"
          (paths sfp) (extensions sfp) (num-snds sfp) (auto-freq sfp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Thu Mar 21 10:52:10 2024, Heidhausen -- verify-and-store used to use
;;; make-sndfile-ext but we need to use videos now hence this helper fun
(defmethod get-media-file ((sfp sndfile-palette) media-data)
  (if (sndfile-p media-data)
    media-data
    (let ((list? (listp media-data))
          make-fun)
      (multiple-value-bind
            (path video?)
          (find-sndfile sfp (if list? (first media-data) media-data))
        (setq make-fun (if video? #'make-vidfile #'make-sndfile-ext))
        ;; if a list was given then the first in the list is the sound file
        ;; (name only or full path) plus any other slots which need to be
        ;; initialised for this sound.  We still need to find the sound though,
        ;; hence the funny list arg passed to make-sndfile.
        (if list?
          (funcall make-fun
                   (cons path
                         ;; MDE Wed Mar 20 16:47:17 2024, Heidhausen -- this
                         ;; would be the case if e.g. we were reading in an sfp
                         ;; that was written with print-for-init
                         (append (unless (member :id media-data)
                                   (list :id (first media-data)))
                                 (rest media-data))))
          ;; if it wasn't a list, just find the sound and pass this and the
          ;; given name which also acts as the id per default.  MDE Sun Dec 16
          ;; 20:19:30 2012 -- was make-sndfile
          (apply make-fun
                 (list path :id media-data
                       ;; MDE Fri Sep 25 13:49:09 2015 
                            :frequency (cond
                                         ((functionp (auto-freq sfp))
                                          (auto-freq sfp))
                                         ((auto-freq sfp) 'detect)
                                         (t 'c4)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NB Although this class is a palette and therefore a subclass of
;;; recursive-assoc-list, the sound lists in this case cannot be nested!

(defmethod verify-and-store :after ((sfp sndfile-palette))
  (setf (paths sfp) (loop for path in (paths sfp) 
                          collect (trailing-slash path))
        ;; MDE Wed Jun 12 13:49:33 2013 -- to avoid duplicate path errors
        (paths sfp) (remove-duplicates (paths sfp) :test #'string=))
  ;; (print (data sfp))
  (loop with sfe
        for sflist in (data sfp)
        for i from 0 do
          (loop for snd in (data sflist) and j from 0 do 
            (setq sfe (get-media-file sfp snd))
            (when sfe
              (setf (nth j (data (nth i (data sfp)))) sfe))))
  ;; MDE Fri Dec 21 17:59:02 2018 -- if we couldn't find a sndfile, remove it
  (loop for sflist in (data sfp) do
    (setf (data sflist) (remove-if-not #'sndfile-p (data sflist))))
  ;; MDE Fri Jan  4 10:10:22 2013 
  (setf (num-snds sfp) (count-snds sfp))
  sfp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Fri Jan  4 10:49:46 2013 -- must explicitly update the num-snds slot
;;; when combining as verify-and-store (which also updates this) will be called
;;; before relinking. 

(defmethod combine :around ((sfp1 sndfile-palette) (sfp2 sndfile-palette))
  (declare (ignore sfp1 sfp2))
  (let ((result (call-next-method)))
    (setf (num-snds result) (count-snds result))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod add :after (named-object (sfp sndfile-palette) &optional ignore)
  (declare (ignore named-object ignore))
  (verify-and-store sfp)
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  (let* ((files '())
         ;; MDE Thu Mar 21 11:11:19 2024, Heidhausen -- 
         (video-extensions (extensions (make-instance 'vidfile)))
         (all-extensions (append (extensions sfp) video-extensions))
         (full-path "")
         (string (if (stringp sndfile)
                   sndfile
                   (string-downcase (string sndfile))))
         result)
    ;; (print all-extensions)
    (if (search "/" string)
      (when (probe-file string)
        (push string files))
      (loop for path in (paths sfp)
            for extension = (pathname-type string)
            do
               ;; MDE Wed Jan  6 12:06:09 2021, Heidhausen -- can't just assume
               ;; that if there's a dot in the filename that it's the extension 
               (if (and extension (member extension all-extensions
                                          :test #'string=))
                 (progn
                   (setq full-path (format nil "~a~a"
                                           path string))
                   (when (probe-file full-path)
                     (push full-path files)))
                 ;; extension not recognised or not given
                 (loop for extension in all-extensions do
                   (setf full-path (format nil "~a~a.~a"
                                           path string extension))
                   (when (probe-file full-path)
                     (push full-path files))))))
    (setq result
          (case (length files)
            ;; MDE Mon Apr 9 12:29:26 2012 -- changing from warn to error as
            ;; this is a show-stopper if we call clm-play.
            (0 (warn "sndfile-palette::find-sndfile: ~
                Cannot find sound file so skipping:~%'~a'"
                     string))
            (1 (first files))
            (t (warn "sndfile-palette::find-sndfile: Sound file '~a' exists in ~
                ~%more than one folder or with more than one extension.  ~
                ~%Please give the full path in your sndfile-palette: ~&~a" 
                     string files))))
    ;; MDE Thu Mar 21 11:22:43 2024, Heidhausen -- second value is whether we've
    ;; got a video file or not. would be nice to capture this in the loop logic
    ;; above but that wouldn't be so easy
    (values result (when result (member (pathname-type result) video-extensions
                                        :test #'string=)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Convenience function as we don't really ever want the named-object as we do
;;; with other palettes. The id here is of the group, not individual files.

(defmethod get-snds (id (sfp sndfile-palette))
  (let ((obj (get-data id sfp)))
    (when obj (data obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This class is a little different as we can fake association-list type
;;; functionality by pulling out a sound with a certain id from the sound list.
;;; If the sound list was a proper assoc-list however, we couldn't have the
;;; same sound twice in the list, which would be inconvenient.  Hence we fake
;;; it here. In this case, we pull out the first sound we see with an id
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
;;; MDE Sat Dec 22 20:42:32 2012 -- if we don't fully reference the group we
;;; assume it's in  the same as the current.
(defmethod get-snd-short ((sfp sndfile-palette) ref (current sndfile-ext))
  ;; (print '*****) (print ref)
  (let* ((snd-id ref)
         (next-group (group-id current)))
    (when (listp ref)
      (if (= 1 (length ref))
          (setf snd-id (first ref))
          (setf snd-id (second ref)
                next-group (first ref))))
    ;; (print next-group) (print snd-id)
    (get-snd next-group snd-id sfp)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Thu Jan  3 17:32:32 2013 -- generally used transparently by
;;; e.g. verify-and-store to set the num-snds slot, but can of course be called
;;; by the user if deemed necessary.  However, the combine and other
;;; destructive methods should call this implicitly to set the num-snds slot so
;;; it shouldn't be necessary to call explicitly.
(defmethod count-snds ((sfp sndfile-palette))
  ;; (print sfp)
  (relink-named-objects sfp)
  (loop with result = 0
     for ref in (get-all-refs sfp)
     do (incf result (length (get-data-data ref sfp)))
     finally (return result)))
              
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Fri Dec 21 14:35:20 2018 -- was &rest ids but now this should be a list
;;; ****m* sndfile-palette/get-nearest
;;; DESCRIPTION
;;; Get the sndfile whose frequency slot is nearest to the first argument.
;;; 
;;; ARGUMENTS
;;; - the frequency we're looking for, in Hertz (a number)
;;; - the sndfile-palette object we'll search
;;; 
;;; OPTIONAL ARGUMENTS
;;; - the IDs of the sndfile groups we'll search. Either a list of ids or a
;;; single id. If NIL then all groups will be searched. Default = NIL.
;;; - if more than one sndfile is very close in frequency, select one at random
;;; (see also the get-nearest-by-freq function)
;;; 
;;; RETURN VALUE
;;; the nearest sndfile object
;;; 
;;; SYNOPSIS
(defmethod get-nearest (freq (sfp sndfile-palette) &optional ids random)
;;; ****
  (setq ids (force-list ids))
  (unless ids (setq ids (get-all-refs sfp)))
  (unless (numberp freq)
    (error "sndfile-palette::get-nearest: freq should be numeric: ~a~%~a"
           freq sfp))
  (get-nearest-by-freq
   freq
   (loop for id in ids appending (get-data-data id sfp))
   random))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod print-for-init ((sfp sndfile-palette)
                           &key (stream t) (call 'make-sfp))
  (call-next-method ; assoc-list method
   sfp
   :stream stream
   :data-printer #'(lambda (list stream)
                     (princ "(" stream)
                     (loop for sf in list do
                          (print (get-slots-list sf) stream))
                     (princ ")" stream))
   :call call))

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
;;; - :groups. A single group or list of groups (IDs) for the sound files we
;;;   wish to process.
;;; - :name-fun.The function for processing a single name. Of course the names
;;;   must be consistent and each sound file must be able to be processed by
;;;   this single function. NB for reasons of similar usage in get-spectra-al
;;;   (see spectra.lsp) this function actually must return the MIDI note number
;;;   (may be a floating point for microtonal applications), which is then
;;;   converted by the sndfile class to a frequency in Hertz.
;;; - :on-error. The function to be called when the name function cannot
;;;   determine the frequency of the sound file. This could be #'error (the
;;;   default), #'warn, or nil if nothing is to be done on failure.
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
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;; - :auto-freq. whether to do automatic frequency detection on the sound
;;;   files. T would mean using a pitch detection routine, or a function can
;;;   also be passed, whereupon it will be called with the path slot as
;;;   argument (the idea is that the fundamental can be extracted from the file
;;;   name). Default = NIL.
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
                          auto-freq (warn-not-found t))
;;; ****
  (make-instance 'sndfile-palette :id id :data sfp :paths paths
                 :extensions extensions
                 :auto-freq auto-freq
                 :warn-not-found warn-not-found))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* sndfile-palette/make-sfp-from-reaper-markers
;;; DATE
;;; July 2nd 2021
;;; 
;;; DESCRIPTION
;;; Create a sndfile-palette object by reading a reaper-file with
;;; specially-named markers. This is specifically aimed at the situation where
;;; you want to use one longer sound file in many segments, rather than lots of
;;; different sound files. We search the reaper file for all markers whose names
;;; begin with clm-play. A marker which begins a sound file segment will then
;;; include the group name after a after a further hyphen e.g. the marker will
;;; be named clm-play-perc or clm-play-string-attack (in these cases the
;;; sndfile-palette will contain groups named perc and string-attack. A marker
;;; with the name clm-play will indicate the end of a sndfile segment, and is
;;; required. All other markers in the reaper file will be ignored.
;;;
;;; (N.B. In all, this is a little different from the funtion which creates
;;; sndfile-palettes from wavelap-marker files but that is a different situation
;;; (marker files, not wavelab files in general, as here with reaper files) and
;;; had a slightly different goal at the time, creating groups in bundles rather
;;; than in names. So we're not reproducing functionality here with reaper files
;;; rather we're providing a slightly different functionality which sorts
;;; current needs quite a few years on.)
;;; 
;;; ARGUMENTS
;;; - the path to the reaper file (string)
;;; - the path to the sound file. This must be provided as the reaper file could
;;;   have marks at any point where there are several sound files
;;;   playing. (string)
;;;
;;; OPTIONAL ARGUMENTS
;;; - the default frequency of the sound files: either a pitch symbol, a
;;;   frequency in Hertz, 'detect for autocorrelation pitch detection, or a
;;;   function to be called to e.g. extract the pitch from the file name or use
;;;   another pitch detection method. Default = 'C4 (middle C)
;;; 
;;; RETURN VALUE
;;; a sndfile-palette object with appropriate groups as read from the reaper
;;; file. 
;;; 
;;; SYNOPSIS
(defun make-sfp-from-reaper-markers (reaper-file sound-file &optional
                                                              (auto-freq 'c4))
;;; ****
  (let ((markers (filter-parameters
                  (get-parameters reaper-file '("MARKER") #\  t)
                  'clm-play))
        (sfp (make-assoc-list 'from-reaper nil)))
    (flet ((saveit (smarker emarker group)
             (unless (get-data group sfp nil)
               (add (list group nil) sfp))
             (add-to-list-data
              (make-sndfile-ext sound-file
                                :id (read-from-string
                                     (pathname-name sound-file))
                                :frequency (if auto-freq auto-freq 'c4)
                                :start (second smarker) :end (second emarker))
              group sfp))
           (group-name (data)
             (let ((marker-name (string (third data))))
               (when (> (length marker-name) 8)
                 (read-from-string (subseq marker-name 9))))))
      (loop for start in markers by #'cddr
         for end in (rest markers) by #'cddr
         for sname = (group-name start)
         for ename = (group-name end)
         do
           (when ename
             (error "make-sfp-from-reaper-markers: end point ~
                     should have a simple 'clm-play' marker. Got clm-play~a"
                    ename))
           (saveit start end sname))
      (setf sfp (change-class sfp 'sndfile-palette)
            (auto-freq sfp) auto-freq)
      (verify-and-store sfp)
      (link-named-objects sfp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
                      :skip '("short-percussive" "weird"))
-->
SNDFILE-PALETTE: paths: NIL
                 extensions: (wav aiff aif snd)
                 num-snds: 92
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
             volume-curve: -1, loop-it: NIL, bit-depth: 24, srate: 96000, 
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
    ;; MDE Fri Sep 25 15:02:22 2015 -- if we've got sndfiles in the folder
    ;; (i.e. not subfolders) then we have to create a default group
    ;; MDE Sun Feb 18 15:19:00 2024, Heidhausen -- in addition, use
    ;; default-group if it's specified and not 'default-group 
    (when (or (not (eq default-group 'default-group))
              (not groups))
      (setq groups (list default-group)))
    (if sfs
        (progn
          (loop for sf in sfs do
               (setq sfgroup (get-group-from-file sf pdl)
                     pos (position sfgroup groups
                                   :test #'(lambda (x y)
                                             (if (atom y)
                                                 (eq x y)
                                                 (eq x (first y)))))
                     group  (progn
                              (unless pos (setq group default-group
                                                pos 0))
                              (nth pos groups)))
               (if (atom group)
                   (setf (nth pos groups) (list group (list sf)))
                   (push sf (second (nth pos groups)))))
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
         (when (and (seq-has-all insist sfname)
                    (seq-has-none resist sfname))
           (push (list sndfile :frequency (midi-to-freq midi)) sfp)))
    ;; (print sfp)
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
;;; without the extensions aif, wav, aiff, and snd are ignored.
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
;;;   just one of them, unless (hack alert!) the first element of the list is
;;;   the symbol or, in which case only one of the patterns must be
;;;   present. Default = NIL.  
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
(defun get-sndfiles (folder &optional skip insist resist force-quotes)
;;; ****
  ;; MDE Wed Apr 15 11:14:59 2020 -- handle pathnames if we've used
  ;; e.g. merge-pathnames  
  (when (typep folder 'pathname)
    (setq folder (agnostic-directory-pathname folder)))
  (setq insist (force-list insist)
        resist (force-list resist))
  (loop for file in (get-all-files folder skip nil force-quotes)
        for name = (pathname-name file)
        when (and (member (pathname-type file) '("aif" "wav" "aiff" "snd"
                                                 "aif\"" "wav\"" "aiff\""
                                                 "snd\"")
                          :test #'string=)
                  ;; MDE Sat Jun 29 17:08:17 2024, Heidhausen -- this is a bit
                  ;; of a hack but I don't want to destroy existing
                  ;; functionality and it is somehow fitting
                  (if (eq (first insist) 'or)
                    (seq-has-some (rest insist) name)
                    (seq-has-all insist name))
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
;;; OPTIONAL ARGUMENTS
;;; - T or NIL to indicate whether a random file should be chosen when there are
;;;   several sndfiles with the same frequency. (NB the function random-rep will
;;;   be used so if you wanted repeatable results call (random-rep 1 t) in order
;;;   to reset the random seed before calling clm-play or whatever context uses
;;;   this function.) Default = NIL.
;;; 
;;; RETURN VALUE
;;; the nearest sndfile object
;;; 
;;; SYNOPSIS
(defun get-nearest-by-freq (freq sflist &optional random)
;;; ****
  (let* ((diff most-positive-double-float)
         (cdiff diff)
         it)
    (loop for sf in sflist do
         (when (frequency sf)
           (setq diff (abs (- 1.0 (/ freq (frequency sf)))))
           ;; MDE Fri Dec 21 14:07:06 2018 -- do the random thing too
           (when (or (<= diff cdiff)
                     (and random
                          (zerop (random-rep 2)) ; 50/50 chance
                          (equal-within-tolerance 0.0 diff 0.001)))
             (setq it sf
                   cdiff diff))))
    it))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun akoustik-piano-name (name)
  (when name
    (let ((zero-octave (char= #\- (elt name 9))))
      ;; samples notes use middle C = C3
      (+ (note-to-midi (read-from-string
                        (subseq name 8 (if zero-octave 11 10))))
         12))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sndfile-palette-p (thing)
  (typep thing 'sndfile-palette))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF sndfile-palette.lsp


