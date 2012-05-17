;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; File:             clm.lsp
;;;
;;; Class Hierarchy:  none: no classes defined
;;;
;;; Version:          1.0.0-beta1
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Definition of functions relating to CLM sound file output.
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    11/5/2012
;;;
;;; $$ Last modified: 20:03:40 Thu May 17 2012 BST
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

;;; SAR Thu May 17 11:07:24 EDT 2012: Added robodoc entry

;;; Use this function to randomly generate the <entry-points> to clm-loops

;;; ****f* clm/random-loop-points

;;; FUNCTION

;;; Return a list of lists of randomly generated entry points (loop markers)
;;; for use with clm-loops-all.
;;;
;;; This function also produces an output text file containing the same list of
;;; lists. This file is in Lisp syntax and can therefore be accessed using
;;; read-from-file. 
;;; 
;;; ARGUMENTS
;;; - A string that is the file name, including directory path and extension,
;;;   of the output file to produce.
;;; - A string that is the sound file for which to generate random entry
;;;   points. 
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :min-points. An integer that is the least number of entry points to
;;;   generate for each list. Default = 5.
;;; - :max-points. An integer that is the greatest number of entry points to
;;;   generate for each list. Default = 13.
;;; - :min-dur. A number that is the shortest duration between two entry
;;;   points. Default = 0.05.
;;; - :num-loop-sets. An integer that is the number of lists of entry points to
;;;   generate. Default = 20.
;;; - :scalers. A list of fractions that are durations relative to the min-dur,
;;;   such that, for example, a min-dur of 0.05 with a scaler of 13/8 would
;;;   result in a scaled duration of 0.08125. The fractions in this list will
;;;   be chosen at random when calculating the duration of the next loop
;;;   segment. Default = '(1/1 2/1 3/2 5/3 8/5 13/8).
;;; 
;;; RETURN VALUE
;;; 
;;; 
;;; EXAMPLE
#|

(random-loop-points 
 "/tmp/outfile" 
 "/path/to/test-sndfile-3.aiff"
 :min-points 3
 :max-points 7
 :min-dur 0.1
 :num-loop-sets 5
 :scalers '(1/1 2/1 3/2 5/3 7/5 11/7 13/11))

=> ((0.789 0.929 1.079) (0.028 0.228 0.368 0.487 0.687) (0.014 0.164 0.321)
    (0.256 0.406 0.524 0.681) (0.069 0.235 0.353 0.472 0.572 0.69))

|#
;;; SYNOPSIS
#+clm
(defun random-loop-points (outfile sndfile 
                           &key 
                           ;; MDE Thu May 17 17:02:15 2012 -- could also be
                           ;; :error or anything else that with-open-file
                           ;; accepts 
                           (if-outfile-exists :overwrite) 
                           ;; the minimum number of time points for an output
                           ;; loop--number of looped sound segments is 1- this
                           (min-points 5)
                           ;; max number of time points--the actual number of
                           ;; points will be randomly chosen between these two
                           ;; numbers. 
                           (max-points 13)
                           ;; minimum duration of a loop segment--this number
                           ;; will actually be used and scaled by scalers
                           (min-dur 0.05)
                           ;; how many sets of loops should be generated
                           (num-loop-sets 20)
                           ;; scalers for the min-dur: these are all
                           ;; proportions relative to min-dur so if we have
                           ;; 13/8 in this list and min-dur of 0.05 then the
                           ;; duration for such a segment would be 0.08125.
                           ;; these will be chosen at random when calculating
                           ;; the next loop segment duration
                           (scalers '(1/1 2/1 3/2 5/3 8/5 13/8)))
;;; ****
  (let* ((snd-dur (clm::sound-duration sndfile))
         (max-scaler (loop for s in scalers maximize s))
         (max-start (- snd-dur (* min-dur (1- max-points) max-scaler)))
         (num-scalers (length scalers)))
    (when (< max-start 0)
      (error "clm::random-loop-points: sound duration (~a) is too short for ~
              a min-dur ~%of ~a and max-points ~a" snd-dur min-dur max-points))
    (with-open-file 
        (out outfile :direction :output :if-does-not-exist :create
             :if-exists if-outfile-exists)
      (format out "(")
      (loop 
         repeat num-loop-sets 
         for num-points = (between min-points max-points)
         for point = (random max-start)
         do
         (format out "~&(")
         (loop 
            repeat num-points
            ;; for scaler = (random-from-list scalers num-scalers)
            ;; for point = start
            do
            (format out "~,3f " point)
            (incf point (* min-dur
                           (random-from-list scalers num-scalers))))
         (format out ")"))
      (format out ")")))
  ;; MDE Thu May 17 17:01:41 2012 -- 
  (read-from-file outfile))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SAR Thu May 10 21:26:12 BST 2012: Added robodoc entry.

;;; ****f* clm/clm-loops
;;; FUNCTION
;;; Generate a sound file from an existing specified sound file by shuffling
;;; and repeating specified segments within the source sound file. 
;;;
;;; This function was first introduced in the composition "breathing Charlie"
;;; (under the name loops): see charlie-loops.lsp in that project for examples.
;;;
;;; The first required argument to the function is the name of the sound file,
;;; including path and extension, looped. This must be a mono file.
;;;
;;; The second required argument (entry-points) is a list of times, in seconds,
;;; where attacks (or something significant) happen in the file. These are used
;;; to create loop start/end points.
;;; 
;;; Be careful when doing shuffles as if, e.g., the transpositions list is more
;;; than 6 elements, shuffling will take a very long time.
;;; 
;;; The entry-points are used randomly so that any segment may start at any
;;; point and transition to any other segment (i.e. skipping intervening
;;; segments, always forwards however). There are always two segments in use at
;;; any time. The function randomly selects which segments are used, then a
;;; transition (see fibonacci-transitions) from repeated segment 1 to repeated
;;; segment 2 is made. Then the next segment is chosen and the process is
;;; repeated (i.e. from previous segment 2 to new segment) until the
;;; max-start-time (in seconds) is achieved.
;;; 
;;; fibonacci-transitions are first shuffled and then made into a circular
;;; list. Then they are expanded to create the transpositions (each number
;;; becomes a series of 1s and 0s--length is the number itself--with a
;;; transition from all 0s to all 1s: e.g. (fibonacci-transition 20) -> (0 0 0
;;; 0 1 0 0 1 0 1 0 1 0 1 0 1 0 1 1 1) This is then used to select one or the
;;; other of the current two segments.
;;;
;;; The sample-rate transpositions are simply randomly permutated and selected.
;;; 
;;; ARGUMENTS
;;; - The name of a sound file, including path and extension.
;;; - A list of numbers that are time in seconds. These serve as the
;;;   "entry-points", i.e. loop markers within the file, and delineate the
;;;   beginning and end of segments that will be shuffled and played back at
;;;   random in the resulting file.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments.
;;; - :max-perms. A number that is the maximum number of permutations generated
;;;   for the transitions. Default = 1000.
;;; - :fibonacci-transitions. A list of numbers that serve as the number of
;;;   steps in each transition from one segment to the next. These numbers will
;;;   be used as the first argument to the call to fibonacci-transition.
;;;   Default = '(34 21 13 8)
;;; - :max-start-time. A number that is the maximum time in second at which a
;;;   segment can start in the resulting sound file. Default = 60.0.
;;; - :output-dir. The directory path for the output file. Default = "/tmp/".
;;; - :srate. The sampling rate. If specified by the user, this will generally
;;;   be a number. By default it takes the CLM global sample-rate, i.e.
;;;   clm::*clm-srate*
;;; - :data-format. The data format of the resulting file. This must be
;;;   preceded by the clm package qualifier. See clm.html for types of data
;;;   formats, such as mus-bshort, mus-l24float etc. 
;;;   Default is the whatever the CLM global clm::*clm-data-format* is set to. 
;;; - :header-type. The header type of the resulting file. This must be
;;;   preceded by the clm package qualifier. See clm.html for possible header
;;;   types, such as mus-riff, mus-aifc etc. By default it takes the CLM global
;;;   clm::*clm-header-type*.
;;; - :sndfile-extension. A string or NIL. If a string, this will be appended
;;;   to the resulting sound file as a file extension. If NIL, the sound file
;;;   extension will automatically be selected based on the header type.  NB:
;;;   This argument does not affect the header type! Default = NIL.
;;; - :channels. An integer that is the number of channels in the resulting
;;;   output. If greater than one, the segments will be automatically panned
;;;   amongst the channels. Default = 1.
;;; - :transpositions. A list of number that are transpositions in
;;;   semitones. These will be shuffled and applied randomly to each
;;;   consecutive segment in the output. Default = '(0).
;;; - :num-shuffles. An integer that will indicate how many times the lists
;;;   passed to fibonacci-transitions and entry-points will be shuffled before
;;;   generating output. Default = - 1.
;;; - :suffix. A string that will be automatically appended to the end of the
;;;   file name. Default = "".
;;; - :src-width. A number that represents the accuracy of the sample-rate
;;;   conversions undertaken for transposition. The higher this number is, the
;;;   more accurate the transposition will be, but the longer it will take to
;;;   process the file. Default = 5.
;;; 
;;; RETURN VALUE
;;; Returns the name of the file generated. 
;;; 
;;; EXAMPLE
#|
;;; A straightforward example with a number of the variables.
(clm-loops "/path/to/sndfile-3.aiff"
           '(0.180 2.164 4.371 7.575 9.4 10.864)
           :fibonacci-transitions '(1 2 3 4 5)
           :max-perms 7
           :output-dir "/tmp/"
           :channels 1
           :transpositions '(1 12 -12)
           :num-shuffles 3
           :src-width 20)

=> "/tmp/sndfile-3-loops-from-00m00.180-.wav"

|#

;;; SYNOPSIS
#+clm
(defun clm-loops (sndfile entry-points &key
                  (max-perms 1000)
                  (fibonacci-transitions '(34 21 13 8))
                  (max-start-time 60.0)
                  (output-dir "/tmp/")
                  (srate clm::*clm-srate*)
                  (data-format clm::*clm-data-format*)
                  ;; MDE Fri May 11 15:33:45 2012 
                  (header-type clm::*clm-header-type*)
                  ;; MDE Fri May 11 15:34:17 2012 -- 
                  (sndfile-extension nil)
                  (channels 1)
                  ;; semitones
                  (transpositions '(0))
                  ;; added 31/7/05 to vary the order of
                  ;; entry points, transpositions and
                  ;; fibonacci-transitions (could be 0!)
                  (num-shuffles 1) 
                  (suffix "")
                  (src-width 5))
;;; ****
  (format t "~&num-shuffles: ~a" num-shuffles)
  ;; MDE Fri May 11 15:34:36 2012 
  (unless sndfile-extension
    (setf sndfile-extension
          (cond                         ; can't use case with clm globals
            ((or (= header-type clm::mus-aiff)
                 (= header-type clm::mus-aifc))
             ".aif")
            ((= header-type clm::mus-riff) ".wav")
            ((= header-type clm::mus-next) ".snd")
            (t (error ".aif")))))
  (let* ((perms (flatten 
                 ;; inefficient-permutations will always return :max results no
                 ;; matter what the first argument
                 (inefficient-permutations (length entry-points)
                                           :max max-perms)))
         (shuffled (multi-shuffle-with-perms entry-points num-shuffles))
         (srcs (loop for i in transpositions collect (semitones i)))
         (transp-perms (make-cscl
                        (multi-shuffle-with-perms
                         (flatten 
                          (permutations (length transpositions)))
                         num-shuffles)))
         (fts (make-cscl 
               (multi-shuffle-with-perms fibonacci-transitions num-shuffles)))
         (transition nil)
         (output-file (format nil "~a~a-loops-from~a~a~a~a" 
                              output-dir (pathname-name sndfile) 
                              (secs-to-mins-secs (first entry-points) 
                                                 :same-width t
                                                 :separator "m")
                              (if (zerop (length suffix)) "" "-")
                              suffix sndfile-extension))
         (start 0.0)
         (end 0.0)
         (start1 0.0)
         (start2 0.0)
         (end1 0.0)
         (end2 0.0)
         (src 0.0)
         (src1 1.0)
         (src2 (nth (get-next transp-perms) srcs))
         (duration 0.0)
         (output-start 0.0)
         (current-perm 0))
    (labels ((get-entry
                 ()
               (let ((this (nth (mod current-perm max-perms) perms))
                     (next (nth (mod (1+ current-perm) max-perms) perms)))
                 (if (= this next)
                     (progn
                       (incf current-perm)
                       (get-entry))
                     (sort (list (nth this shuffled)
                                 (nth next shuffled))
                           #'<))))
             (get-entries ()
               (let ((entry (get-entry)))
                 (setf start1 (first entry)
                       end1 (second entry))
                 ;; so this inc happens only once during selection of the two
                 ;; segments i.e. start1 end1 this time is start2 end2 last
                 ;; time. 
                 (incf current-perm)
                 (setf entry (get-entry)
                       start2 (first entry)
                       end2 (second entry)))))
      (format t "~%Output file will be ~a~%" output-file)
      (clm::with-sound 
          (:scaled-to .99 :play nil :output output-file :channels channels
                      :srate srate
                      :data-format data-format
                      :header-type header-type
                      :statistics t) 
        (loop while (<= output-start max-start-time)
           do
             (get-entries)
             (format t "~%~%seg1 [time (nth entry point)]: ~a (~a) -> ~a (~a)~
                           ~%seg2:                          ~a (~a) -> ~a (~a)"
                     start1 (position start1 entry-points)
                     end1 (position end1 entry-points)
                     start2 (position start2 entry-points)
                     end2 (position end2 entry-points))
             (setf transition (fibonacci-transition (get-next fts))
                   src1 src2
                   src2 (nth (get-next transp-perms) srcs))
             (loop 
                for tr in transition 
                while (<= output-start max-start-time)
                do
                ;; transition is a list of 0s and 1s
                  (if (zerop tr)
                      (setf src src1
                            start start1
                            end end1)
                      (setf src src2
                            start start2
                            end end2))
                  (setf duration (/ (- end start) src))
                  (format t "~%   ~a: src: ~a, dur: ~a, ~a -> ~a" 
                          output-start src duration start end)
                  (clm::samp5 sndfile output-start
                              :printing nil
                              :duration duration
                              :start start
                              :degree (nth (random 5) '(15 30 45 60 75))
                              :srt src
                              :width src-width
                              :amp-env '(0 0 3 1 97 1 100 0))
                ;; 6/10/06: as long as amp-env above doesn't change the *
                ;; 0.94 for duration should ensure an overlap--ok, the next
                ;; duration might be shorter/longer than this so it won't
                ;; perfectly overlap but it will start before this one
                ;; finishes. 
                  (incf output-start (* 0.94 duration))))))))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Thu May 17 12:10:17 EDT 2012: Added robodoc entry

;;; ****f* clm/clm-loops-all
;;; FUNCTION
;;; Similar to clm-loops, but takes a list of lists of entry points (which can
;;; also be generated using the random-loop-points function, for example) and
;;; produces one output sound file for each list of entry points that list
;;; contains. 
;;; 
;;; ARGUMENTS
;;; - A string that is the name of the source sound file including directory
;;;   path and extension.
;;; - A list of lists of numbers that are entry points (loop markers) in the
;;;   specified source sound file.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :max-perms. A number that is the maximum number of permutations generated
;;;   for the transitions. Default = 1000.
;;; - :fibonacci-transitions. A list of numbers that serve as the number of
;;;   steps in each transition from one segment to the next. These numbers will
;;;   be used as the first argument to the call to fibonacci-transition.
;;;   Default = '(34 21 13 8).
;;; - :max-start-time. A number that is the maximum time in seconds at which a
;;;   segment can start in the resulting sound file. Default = 60.0.
;;; - :output-dir. The directory path for the output file. Default = "/tmp/".
;;; - :srate. The sampling rate. If specified by the user, this will generally
;;;   be a number. By default it takes the CLM global sample-rate, i.e.
;;;   clm::*clm-srate*
;;; - :data-format. The data format of the resulting file. This must be
;;;   preceded by the clm package qualifier. See clm.html for types of data
;;;   formats, such as mus-bshort, mus-l24float etc. 
;;;   Default is the whatever the CLM global clm::*clm-data-format* is set to. 
;;; - :header-type. The header type of the resulting file. This must be
;;;   preceded by the clm package qualifier. See clm.html for possible header
;;;   types, such as mus-riff, mus-aifc etc. By default it takes the CLM global
;;;   clm::*clm-header-type*.
;;; - :sndfile-extension. A string or NIL. If a string, this will be appended
;;;   to the resulting sound file as a file extension. If NIL, the sound file
;;;   extension will automatically be selected based on the header type.  NB:
;;;   This argument does not affect the header type! Default = NIL.
;;; - :channels. An integer that is the number of channels in the resulting
;;;   output. If greater than one, the segments will be automatically panned
;;;   amongst the channels. Default = 1.
;;; - :do-shuffles. T or NIL to indicate whether to shuffle the lists passed to
;;;   fibonacci-transitions and entry-points before generating output. 
;;;   T = do shuffles. Default = T.
;;; - :start-after. A number. All loops will be excluded that start before this
;;;   number of seconds. Default = -1.0.
;;; - :stop-after. A number. All loops will be excluded that start after this
;;;   number of seconds. Default =  99999999.0.
;;; - :suffix. A string that will be automatically appended to the end of the
;;;   file name. Default = "".
;;; - :transpositions. A list of number that are transpositions in
;;;   semitones. These will be shuffled and applied randomly to each
;;;   consecutive segment in the output. Default = '(0).
;;; - :transposition-offset. A number that is an additional number of semitones
;;;   to be added to each transposition value before performing the
;;;   transposition. Default = 0.0.
;;; - :src-width. A number that represents the accuracy of the sample-rate
;;;   conversions undertaken for transposition. The higher this number is, the
;;;   more accurate the transposition will be, but the longer it will take to
;;;   process the file. Default = 5.
;;; 
;;; RETURN VALUE
;;; Returns NIL.
;;; 
;;; EXAMPLE
#|

(clm-loops-all
 (concatenate 'string 
              cl-user::+slippery-chicken-home-dir+
              "test-suite/test-sndfiles-dir-1/test-sndfile-3.aiff")
 '((0.794 0.961 1.061 1.161 1.318 1.436 1.536)
   (0.787 0.887 0.987 1.153 1.310 1.510)
   (0.749 0.889 1.056 1.213 1.413)
   (0.311 0.411 0.611 0.729)
   (0.744 0.884 1.002))
 :max-perms 6
 :fibonacci-transitions '(31 8 21 13)
 :output-dir "/tmp/"
 :channels 1
 :transpositions '(1 12 -12)
 :src-width 20)

|#
;;; SYNOPSIS
#+clm
(defun clm-loops-all (sndfile entry-points-list 
                      &key 
                      (max-perms 1000)
                      (fibonacci-transitions '(34 21 13 8))
                      (max-start-time 60.0)
                      (output-dir "/tmp/")
                      (srate clm::*clm-srate*)
                      (data-format clm::*clm-data-format*)
                      ;; MDE Fri May 11 15:33:45 2012 
                      (header-type clm::*clm-header-type*)
                      ;; MDE Fri May 11 15:34:17 2012 -- 
                      (sndfile-extension nil)
                      (channels 1)
                      (do-shuffles t) ;; see clm-loops
                      ;; exclude all those loops who start before this
                      ;; number of seconds. 
                      (start-after -1.0)
                      (stop-after 99999999.0)
                      (suffix "")
                      ;; semitones
                      ;; 6/10/06: using just one list of transpositions passed
                      ;; onto clm-loops created the same tone structure for
                      ;; every file generated (boring).  This list will now be
                      ;; shuffled and 10 versions collected which will then be
                      ;; passed (circularly) one after the other to clm-loops.
                      (transpositions '(0))
                      (transposition-offset 0.0)
                      (src-width 5))
;;; ****
  (let* ((transps-offset (loop for st in transpositions
                            collect (+ transposition-offset st)))
         (transps-shuffled (make-cscl
                            (loop repeat 10 collect
                                 (shuffle transps-offset :reset nil)))))
    (loop for epl in entry-points-list and i from 1 do
         (when (and (> (first epl) start-after)
                    (<= (first epl) stop-after))
           (clm-loops sndfile epl :max-perms max-perms 
                      :fibonacci-transitions fibonacci-transitions
                      :num-shuffles (if do-shuffles
                                        (mod i 7)
                                        0)
                      :max-start-time max-start-time
                      :channels channels
                      :srate srate
                      :suffix suffix
                      :data-format data-format
                      :header-type header-type
                      :sndfile-extension sndfile-extension
                      :output-dir output-dir
                      :transpositions (get-next transps-shuffled)
                      :src-width src-width)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF clm.lsp
