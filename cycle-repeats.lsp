;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* _sc/cycle-repeats
;;; NAME 
;;; rthm-chain
;;;
;;; File:             cycle-repeats.lsp
;;;
;;; Class Hierarchy:  named-object -> linked-named-object -> sclist -> 
;;;                   circular-sclist -> cycle-repeats
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          class used in rthm-chain
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    4th February 2010
;;;
;;; $$ Last modified: 19:28:39 Thu Feb 24 2011 MYT
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

(eval-when (compile)
  (declaim (optimize (speed 3) (safety 1) (space 0) (debug 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :slippery-chicken)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass cycle-repeats (circular-sclist)
  ;; this is something like ((0 5) (1 1) (0 3) (1 1) (0 2) (2 1)) which means
  ;; we'll have 0 five times, then 1 once, then 0 three times etc.  This will
  ;; be unfolded to replace the data list of the cscl.  We actually pass such a
  ;; list to the data slot, copy it over to the folded slot, then store the
  ;; unfolded list in data and let the cscl handle that as usual
  ((folded :accessor folded :type list :initform nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object :before ((cr cycle-repeats) stream)
  (format stream "~&CYCLE-REPEATS: folded: ~a" (folded cr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod verify-and-store :before ((cr cycle-repeats))
  (let ((unfolded 
         (loop for pair in (data cr) with datum with repeats
            do 
            (unless (and (listp pair)
                         (= 2 (length pair)))
              (error "~a~%cycle-repeats::verify-and-store: data ~
                                 must be a list of 2-element sublists"
                     (data cr)))
            (setf datum (first pair)
                  repeats (second pair))
            (unless (integer>0 repeats)
              (error "~a~%cycle-repeats::verify-and-store: second ~
                                 element of each sublist must be an integer>0"
                     (data cr)))
            append (ml datum repeats))))
    (setf (folded cr) (my-copy-list (data cr))
          ;; got to do this or verify-and-store will be called again and signal
          ;; an error.
          (slot-value cr 'data) unfolded)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF cycle-repeats.lsp
