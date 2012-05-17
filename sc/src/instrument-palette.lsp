;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* palette/instrument-palette
;;; NAME 
;;; instrument-palette
;;;
;;; File:             instrument-palette.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> sclist -> 
;;;                   circular-sclist -> assoc-list -> recursive-assoc-list ->
;;;                   palette -> instrument-palette 
;;;
;;; Version:          1.0.0-beta1
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Implementation of the instrument-palette class which
;;;                   intantiates instruments to be used in an ensemble
;;;                   instance.  
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    6th September 2001
;;;
;;; $$ Last modified: 19:10:45 Mon Feb 20 2012 GMT
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

(eval-when (compile)
  (declaim (optimize (speed 3) (safety 1) (space 0) (debug 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :slippery-chicken)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass instrument-palette (palette)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object :before ((ip instrument-palette) stream)
  (format stream "~%INSTRUMENT-PALETTE: "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod verify-and-store :after ((ip instrument-palette))
  (loop for i in (data ip) and j from 0 do
       (if (typep (data i) 'recursive-assoc-list)
           (error "instrument-palette::verify-and-store: ~
                    In order to allow nested ensembles, nested ~
                    instrument palettes have to be avoided: ~a"
                  (id ip))
           (setf (nth j (data ip)) (apply #'make-instrument
                                          (cons (id i) (data i)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clone ((ip instrument-palette))
  (clone-with-new-class ip 'instrument-palette))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Jan  7 14:28:16 EST 2012: Added robodoc info

;;; ****m* instrument-palette/set-prefers-low
;;; DATE
;;; 05 Feb 2011
;;; 
;;; FUNCTION
;;; Set the PREFERS-NOTES slot of a specified instrument object within a given
;;; instrument-palette object to 'LOW. The instrument object is specified
;;; using the ID symbol assigned to it within the instrument-palette object
;;; definition. 
;;; 
;;; NB: The optional argument is actually required, but is listed as optional
;;; because of the attributes of the instrument class method.
;;; 
;;; ARGUMENTS
;;; - An instrument-palette object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - A symbol that is the ID of the instrument object within the
;;;   instrument-palette object definition.
;;; 
;;; RETURN VALUE
;;; Returns the symbol 'LOW.
;;; 
;;; EXAMPLE
#|
;; Define an instrument-palette object, then set the PREFERS-NOTES slot of the
;; instrument object 'piccolo within that instrument-palette object to 'LOW
(let ((ip (make-instrument-palette 'inst-pal 
                                   '((piccolo (:transposition-semitones 12
                                               :lowest-written d4
                                               :highest-written c6))   
                                     (bf-clarinet (:transposition-semitones -2 
                                                   :lowest-written e3 
                                                   :highest-written c6)) 
                                     (horn (:transposition f
                                            :transposition-semitones -7  
                                            :lowest-written f2 
                                            :highest-written c5))   
                                     (violin (:lowest-written g3
                                              :highest-written c7 
                                              :chords t)) 
                                     (viola (:lowest-written c3
                                             :highest-written f6 
                                             :chords t))))))
  (set-prefers-low ip 'piccolo))

=> LOW

|#
;;; SYNOPSIS
(defmethod set-prefers-low ((ip instrument-palette) &optional instrument)
;;; **** 
  (set-prefers-low (get-data instrument ip)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Jan  7 15:36:44 EST 2012 Added robodoc info

;;; ****m* instrument-palette/set-prefers-high
;;; DATE 
;;; 05 Feb 2011
;;;
;;; FUNCTION
;;; Set the PREFERS-NOTES slot of a specified instrument object within a given
;;; instrument-palette object to 'HIGH. The instrument object is specified
;;; using the ID symbol assigned to it within the instrument-palette object
;;; definition. 
;;; 
;;; NB: The optional argument is actually required, but is listed as optional
;;; because of the attributes of the instrument class method.
;;; 
;;; ARGUMENTS
;;; - An instrument-palette object.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - A symbol that is the ID of the instrument object within the
;;;   instrument-palette object definition.
;;; 
;;; RETURN VALUE
;;; Returns the symbol 'HIGH.
;;; 
;;; EXAMPLE
#|

;; Define an instrument-palette object, then set the PREFERS-NOTES slot of the
;; instrument object 'piccolo within that instrument-palette object to 'HIGH 
(let ((ip (make-instrument-palette 'inst-pal 
                                   '((piccolo (:transposition-semitones 12
                                               :lowest-written d4
                                               :highest-written c6))   
                                     (bf-clarinet (:transposition-semitones -2 
                                                   :lowest-written e3 
                                                   :highest-written c6)) 
                                     (horn (:transposition f
                                            :transposition-semitones -7  
                                            :lowest-written f2 
                                            :highest-written c5))   
                                     (violin (:lowest-written g3
                                              :highest-written c7 
                                              :chords t)) 
                                     (viola (:lowest-written c3
                                             :highest-written f6 
                                             :chords t))))))
  (set-prefers-high ip 'piccolo))

=> HIGH

|#
;;; SYNOPSIS
(defmethod set-prefers-high ((ip instrument-palette) &optional instrument)
;;; ****
  (set-prefers-high (get-data instrument ip)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; MDE Mon Feb 20 19:05:54 2012 -- just a helper method...
#|
e.g.
(loop for i in 
     '((flute 13)
       (oboe 7)
       (b-flat-clarinet 9)
       (bassoon 7)
       (french-horn 5)
       (b-flat-trumpet 7)
       (tenor-trombone 5))
     do
     (set-largest-fast-leap 
      +slippery-chicken-standard-instrument-palette+
      (first i)
      (second i)))

|#
(defmethod set-largest-fast-leap ((ip instrument-palette) instrument value)
  (let ((ins (get-data instrument ip)))
    (unless ins
      (error "instrument-palette::set-largest-fast-leap: Couldn't get ~
              ~a from ~a" instrument (id ip)))
    (setf (largest-fast-leap ins) value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Related functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SAR Sat Jan  7 13:43:00 EST 2012: Added robodoc info

;;; ****f* instrument-palette/make-instrument-palette
;;; FUNCTION
;;; Create an instrument-palette object from a list of instrument descriptions
;;; based on the keyword arguments of make-instrument.
;;; 
;;; ARGUMENTS
;;; - A symbol that will serve as the ID for the instrument-palette object. 
;;; - A list of instrument descriptions based on the keyword arguments of
;;;   make-instrument. 
;;; 
;;; OPTIONAL ARGUMENTS
;;; - keyword argument :warn-not-found. T or NIL to indicate whether a warning
;;;   is printed when an index which doesn't exist is used for lookup.
;;;   Default = T. 
;;; 
;;; RETURN VALUE
;;; An instrument palette.
;;; 
;;; EXAMPLE

#|
;; Returns an instrument-palette object
(make-instrument-palette 'inst-pal 
                         '((piccolo (:transposition-semitones 12
                                     :lowest-written d4 :highest-written c6))  
                           (bf-clarinet (:transposition-semitones -2
                                         :lowest-written e3 
                                         :highest-written c6)) 
                           (horn (:transposition f :transposition-semitones -7 
                                  :lowest-written f2 :highest-written c5)) 
                           (violin (:lowest-written g3 :highest-written c7
                                    :chords t)) 
                           (viola (:lowest-written c3 :highest-written f6
                                   :chords t))))

=>
INSTRUMENT-PALETTE: 
PALETTE: 
RECURSIVE-ASSOC-LIST: recurse-simple-data: T
                      num-data: 5
                      linked: NIL
                      full-ref: NIL
ASSOC-LIST: warn-not-found T
CIRCULAR-SCLIST: current 0
SCLIST: sclist-length: 5, bounds-alert: T, copy: T
LINKED-NAMED-OBJECT: previous: NIL, this: NIL, next: NIL
NAMED-OBJECT: id: INST-PAL, tag: NIL, 
data: (
[...]

|#
;;; SYNOPSIS
(defun make-instrument-palette (id ip &key (warn-not-found t))
;;; ****
  (make-instance 'instrument-palette :data ip :id id
                 :warn-not-found warn-not-found))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun instrument-palette-p (thing)
  (typep thing 'instrument-palette))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          
;;; EOF instrument-palette.lsp
