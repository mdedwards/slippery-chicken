;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* sndfile-ext/vidfile
;;; NAME 
;;; sndfile-ext
;;;
;;; File:             vidfile.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> sndfile ->
;;;                   sndfile-ext -> vidfile
;;;
;;; Version:          1.1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Extension of the sndfile-ext class to hold further
;;;                   properties of video files. Essentially, for now--and for
;;;                   the main purpose of allowing video files to be treated
;;;                   like sound files and placed into reaper files via
;;;                   reaper-play--we'll just make a cursory distinction between
;;;                   sound files and video files. This will allow for further
;;;                   functionality in the future, where necessary and relevant
;;;                   for slippery-chicken. See the sndfile-palette class which
;;;                   separates true sound files from video files and allows
;;;                   handling of both. NB clm-play and others won't be able to
;;;                   process the (usually compressed) audio in these files.
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    19th March 2024, Heidhausen, Germany
;;;
;;; $$ Last modified:  12:20:44 Thu Mar 21 2024 CET
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass vidfile (sndfile-ext)
  ((comment :accessor comment :type string :initarg :comment :initform "")
   (date :accessor date :type string :initarg :date :initform "")
   (encoder :accessor encoder :type string :initarg :encoder :initform "")
   (fps :accessor fps :type number :initarg :fps :initform -1)
   (dimensions-x :accessor dimensions-x :type integer :initarg :dimensions-x
                 :initform -1)
   (dimensions-y :accessor dimensions-y :type integer :initarg :dimensions-y
                 :initform -1)
   ;; cannot be set at init rather it's read from the file. This is mainly used
   ;; in reaper to distinguish between <SOURCE WAVE and <SOURCE VIDEO but there
   ;; could also be a video file (i.e. the extension checks and the container
   ;; can hold video streams, but there's not actually a video in there, rather
   ;; just audio or something else). 
   (has-video-codec :accessor has-video-codec :type boolean :initform nil)
   ;; from sndfile class, where it's nil
   (force-ffprobe :initform t)
   ;; we'd prefer via codec_type data from ffprobe but failing that we can guess
   ;; via the file's extension
   (extensions :accessor extensions :type list :initarg :extensions
               :allocation :class
               :initform '("webm" "mkv" "flv" "vob" "ogv" "avi" "mov"
                           "wmv" "mp4" "m4p" "m4v" "mpg" "mpeg" "mpe"
                           "mpv" "m2v"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; placeholder for now but in the future we should read the data from the file
;;; (via ffprobe or sim.) and store in the respective slots.
(defmethod initialize-instance :after ((vf vidfile) &rest initargs)
  (declare (ignore initargs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod update :after ((vf vidfile) &key ignore)
  (declare (ignore ignore))
  (setf (has-video-codec vf) (video-file-p (path vf) nil nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod print-object :before ((vf vidfile) stream)
  (format stream "~%~%VIDFILE: fps: ~a, has-video-codec: ~a, dimensions-x: ~a ~
                  dimensions-y: ~a~%date: ~a~%encoder: ~a~%comment: ~a~
                  ~%extensions: ~a"
          (fps vf) (has-video-codec vf) (dimensions-x vf) (dimensions-y vf)
          (data vf) (encoder vf) (comment vf) (extensions vf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod clone ((vf vidfile))
  (clone-with-new-class vf 'vidfile))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone-with-new-class :around ((vf vidfile) new-class)
  (declare (ignore new-class))
  (let ((sfe (call-next-method)))
    (setf (slot-value sfe 'comment) (basic-copy-object (comment vf))
          (slot-value sfe 'date) (basic-copy-object (date vf))
          (slot-value sfe 'encoder) (basic-copy-object (encoder vf))
          (slot-value sfe 'fps) (basic-copy-object (fps vf))
          (slot-value sfe 'has-video-codec) (has-video-codec vf)
          (slot-value sfe 'dimensions-x) (basic-copy-object (dimensions-x vf))
          (slot-value sfe 'dimensions-y) (basic-copy-object
                                          (dimensions-y vf)))
    sfe))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-vidfile (path &rest keyargs &key &allow-other-keys)
  (when path
    (if (listp path)
      (when (first path)              ; will be NIL if we couldn't find the file
        (apply #'make-instance (append (list 'vidfile :path)
                                       path)))
      (apply #'make-instance (append (list 'vidfile :path path)
                                     keyargs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun video-file-extension-p (path)
  (member (pathname-type path) (extensions (make-vidfile nil))
          :test #'string-equal))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun video-file-codec-p (path)
  (codec-type-p path "video"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; whether a file is a video, not an object (that's below)
(defun video-file-p (path &optional (allow-just-extension t) (warn t))
  (when (and path (probe-file path))
    (or (video-file-codec-p path)
        (progn
          (when warn
            (warn "vidfile::video-file-p: ffprobe command not available or ~
                   failed; ~%trying via file name extension only (~a)." path))
          (when allow-just-extension
            (video-file-extension-p path))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vidfile-p (candidate)
  (typep candidate 'vidfile))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF
