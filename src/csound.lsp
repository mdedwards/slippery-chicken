;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* sc/csound.lsp
;;; NAME
;;; csound
;;;
;;; File:             csound.lsp
;;;
;;; Class Hierarchy:  none: no classes defined
;;;
;;; Version:          1.0.12
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Definition of functions relating to Csound data output.
;;;                   Related to the csound-play method.
;;;
;;; Author:           Ruben Philipp <ruben.philipp@folkwang-uni.de>
;;;
;;; Creation Date:    2023-02-19
;;;
;;; $$ Last modified:  21:35:17 Sun Feb 19 2023 CET
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
;;; ****f* csound/csound-p-fields-simple
;;;
;;; DESCRIPTION
;;; This function is intended to be used in conjunction with
;;; the csound-play method. It returns a list or -- when the given event
;;; is a chord -- a list of lists of p-values.
;;;
;;; The p-fields are allocated as follows:
;;; - p4: frequency
;;; - p5: amplitude
;;;
;;; N.B.: The event-num and cs-instrument arguments are mandatory as they
;;;       are required by the csound-play method, even though they are not
;;;       used in this function.
;;;
;;; ARGUMENTS
;;; - An event object (most likely the event currently processed by
;;;   csound-play).
;;; - A number referring as an index to the position in processing sequence.
;;; - A reference to a Csound-instrument (number or string).
;;;
;;; RETURN VALUE
;;; Returns either a list (when the event is a pitch) or a list of lists
;;; (in case the event contains a chord) with p4- and p5-values (see
;;; above).
;;;
;;; $$ Last modified:  13:59:39 Mon Feb 20 2023 CET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SYNOPSIS
(defun csound-p-fields-simple (event event-num cs-instrument)
  ;;; ****
  (let ((freq (get-frequency event))
        (amplitude (get-amplitude event)))
    (if (listp freq)
        ;; return a list of lists for the chord
        (loop for f in freq
              collect
              ;; limit the float decimal places to 4
              ;; as Csound has issues dealing with big floats
              ;; in scores
              (list (format nil "~,4f" f)
                    amplitude))
        (list (format nil "~,4f" freq)
              amplitude))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF: csound.lsp
