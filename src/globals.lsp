;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* sc/globals.lsp
;;; NAME
;;; globals
;;;
;;; File:             globals.lsp
;;;
;;; Class Hierarchy:  None: no classes defined.
;;;
;;; Version:          1.0.12
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Definition of the user-changeable configuration data and
;;;                   globals for internal programme use. See top of cmn.lsp and
;;;                   osc-sc-bsd.lsp for a few more globals relevant to
;;;                   functionality in their respective packages.  
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    30th May 2013
;;;
;;; $$ Last modified:  10:03:25 Fri Mar 12 2021 CET
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

;;; MDE Mon Jan 18 15:45:32 2016 -- (re)set these to the Lisp defaults so that
;;; printing and reading in Lisp objects (e.g. sndfile-palettes via
;;; print-for-init) works i.e. doesn't fail by printing ... instead of all
;;; objects. 
(setq *print-length* nil
      *print-level* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****P* globals/+slippery-chicken-config-data+
;;; DESCRIPTION
;;; A global to hold various user-settable configuration settings.  Use e.g. 
;;; (set-sc-config 'default-dir "~/Desktop") or (get-sc-config 'default-dir) to
;;; set or query the settings.  
;;; 
;;; SYNOPSIS
(defparameter +slippery-chicken-config-data+
  (make-instance
   'assoc-list
   :id 'slippery-chicken-config-data
   :data
   ;; MDE Sun Mar 25 10:39:07 2012 -- The following two are used in
   ;; pitch-seq::get-notes to indicate which lowest number in a pitch-seq would
   ;; indicate that we should select the highest or lowest notes possible for
   ;; the instrument/set.
   ;; 
   ;; The first is used to indicate the lowest number in a pitch-seq that would
   ;; indicate that the get-notes algorithm should select the highest notes
   ;; possible for the instrument/set.
   '((pitch-seq-lowest-equals-prefers-high 5)
     ;; the lowest number in a pitch-seq that would indicate that the get-notes
     ;; algorithm should select the lowest notes possible for the
     ;; instrument/set.
     (pitch-seq-lowest-equals-prefers-low 1)
     ;; MDE Wed Oct 10 16:35:10 2018 -- until October 2018 an error was
     ;; signalled if the were no pitches in a set for an instrument which should
     ;; be playing. We can now have a rest-sequence generated instead
     (pitch-seq-no-pitches-error t)
     ;; MDE Thu Oct  8 16:27:19 2020, Heidhausen -- issue a warning if we scale
     ;; a rhythm/event to longer than 10xwhole?
     (rhythm-scale-warning t)
     ;; MDE Sat Aug  3 16:39:40 2019 -- should shorten-large-fast-leaps issue a
     ;; warning or not?
     (shorten-large-fast-leaps-warning t)
     ;; MDE Tue Jul 16 11:17:23 2019 -- if NIL don't even warn about no pitches
     ;; being available
     (pitch-seq-no-pitches-warning t)
     ;; Whether to automatically open EPS files generated with CMN via
     ;; cmn-display.  Currently only works with SBCL and CCL on Mac OSX.
     (cmn-display-auto-open #+sc-auto-open T #-sc-auto-open nil)
     ;; Whether to automatically open PDF files generated with via lp-display.
     ;; Currently only works with SBCL and CCL on Mac OSX.
     (lp-display-auto-open #+sc-auto-open T #-sc-auto-open nil)
     ;; Whether to automatically open MIDI files generated with via midi-play.
     ;; Currently only works with SBCL and CCL on Mac OSX.
     (midi-play-auto-open #+sc-auto-open T #-sc-auto-open nil)
     ;; MDE Sat Dec  7 11:56:24 2019 -- if a pitch has a mark included in this
     ;; list, it will be ignored  
     (midi-play-ignore-marks nil)
     ;; The default directory for output of sound files, EPS files, and
     ;; Lilypond files. Don't forget the trailing slash (i.e. "/tmp/" not
     ;; "/tmp").  Bear in mind that on OSX the /tmp directory is emptied upon
     ;; reboot so you shouldn't store any files you'd like to keep in there.
     (default-dir "/tmp/")
     ;; The full path to the lilypond command.  We need to set this if we'll
     ;; call lp-display, i.e. if we want to automatically call Lilypond and
     ;; open the resultant PDF directly from Lisp.  The default should work if
     ;; you have the Lilypond app in your Applications folder on OSX.
     (lilypond-command
      "/Applications/LilyPond.app/Contents/Resources/bin/lilypond")
     ;; The default amplitude for all events that don't have amplitude/dynamic
     ;; set via some means such as marks.
     (default-amplitude 0.7)
     ;; in init-instance of sc class, warn if we can't call sc-init (because it
     ;; will be called explicitly later)
     (warn-no-sc-init t)
     (warn-unused-instruments t)
     ;; whether to warn when there's no CMN mark for a given Lilypond mark
     (warn-no-cmn-mark t)
     ;; sim for Lilypond
     (warn-no-lp-mark t)
     (warn-no-xml-mark t)
     ;; warn if a pitch goes above midi note 119?
     (warn-high-pitch t)
     ;; warn if a pitch goes below octave number -1
     (warn-low-pitch t)
     (warn-fingering t)
     ;; Bar number offsets for CMN
     (cmn-bar-num-dx-for-sc -0.2)
     (cmn-bar-num-dy-for-sc 1.2)
     ;; MDE Wed Feb  8 16:50:31 2017 -- string-chord-selection-fun (used by
     ;; default for selecting chords for violin, viola, cello) will call
     ;; best-string-diad. This tries to find a diad within a certain
     ;; range. These are the min/max
     (best-string-diad-range (7 11))
     (best-string-diad-microtones nil)
     ;; MDE Sat May 10 12:47:25 2014 -- whether to issue warning when we set
     ;; the asco-msgs slot of a rest event (because they will only be written
     ;; to an antescofo~ file if this happens to be a rest in the part we're
     ;; following and we can't know this in advance).
     ;; MDE Tue Dec 29 11:59:33 2020, Heidhausen -- which auxiliary routine to
     ;; use for auto-clefs. For years this was the best-clef-aux function but
     ;; we're now generally using best-clef-aux-new. Some might prefer the old
     ;; routine however, hence this option here.
     (best-clef-aux-fun best-clef-aux-new) 
     (asco-msg-rest-warning t)
     ;; if we've added, say, an antescofo~ label to an event with a rehearal
     ;; letter, we'll get a warning as we can only have one antescofo label per
     ;; NOTE (though it's not an error to have two, the 2nd will be ignored).
     (asco-two-labels-warning t)
     ;; the default spectral data used in the chord class for things such as
     ;; dissonance and spectral centroid calcuation. See spectra.lsp for more
     ;; details about references into +slippery-chicken-spectra+ (such as
     ;; akoustik-piano-spectra) or see the get-spectrum function in chord.lsp
     ;; for a description of the format this data can take. Currently
     ;; slippery-chicken defines three spectra: akoustik-piano-spectra,
     ;; clm-piano-spectra, and violin-ensemble-spectra. Bear in mind that if
     ;; you change this it would be best to start slippery-chicken again (or at
     ;; least regenerated a piece's data) so that chord's/set's
     ;; dissonance/centroid values are recalculated with the new spectra.
     (default-spectra akoustik-piano-spectra)
     ;; MDE Tue Apr 19 12:05:49 2016 -- for debugging purposes, print info
     ;; about the selection of pitches for an instrument given a pitch-seq and
     ;; set etc. (i.e. data passed to pitch-seq's get-notes method). Note that
     ;; chords will be printed as a sublist (i.e. in parentheses).
     (verbose-pitch-selection nil)
     ;; MDE Thu Oct 18 11:46:46 2018 -- since Sept. 18 the slippery-chicken
     ;; method update-slots also calls handle-ties. This will break some older
     ;; pieces so allow that to be skipped if necessary.
     (update-slots-handles-ties t)
     ;; font size for CMN bar numbers
     (cmn-bar-num-size-for-sc 6)
     (diapason 440)
     (autoconvert-eps-to-pdf nil))))
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; placeholder for various instruments' spectra. This will be filled by
;;; spectra.lsp  
(defparameter +slippery-chicken-spectra+
  (make-ral 'slippery-chicken-spectra nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-sc-config (key)
  (declare (special +slippery-chicken-config-data+))
  (get-data-data key +slippery-chicken-config-data+))

(defun set-sc-config (key value)
  (declare (special +slippery-chicken-config-data+))
  ;; MDE Fri Mar 12 09:49:15 2021, Heidhausen -- essentially a trigger for
  ;; reloading our tunings  
  (when (eq key 'diapason)
    (set-diapason value))
  (data (replace-data key value +slippery-chicken-config-data+)))

(defun default-dir-file (name)
  (concatenate 'string (get-sc-config 'default-dir) name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF globals.lsp
