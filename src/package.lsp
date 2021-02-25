;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* sc/package 
;;; NAME package
;;;
;;; File:             package.lsp
;;;
;;; Version:          1.0.12
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          definition of the slippery-chicken package.
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    5.12.00
;;;
;;; $$ Last modified:  11:05:28 Tue Mar 24 2020 CET
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

(in-package :cl-user)

(defpackage :slippery-chicken
  (:use :common-lisp)
  (:nicknames :sc :slimy-poultry)
  (:import-from :cl-user +slippery-chicken-version+))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :slippery-chicken)

(eval-when (compile load #+allegro-cl-lite eval)
  (export '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; we defined these functions in the cm package so import them into sc.
;;; can't import cm::output-midi-note as it's also an SC method name
(import '(cm::parse-midi-file cm::midi-file-high-low cm::midi-file-one-note))

;;; these shadows make sure that sc external symbols with these names will use
;;; the other package's symbols when we're in those packages (so sc's will be
;;; overridden in there; to use them do e.g. sc::transpose).
(in-package :cm)
(shadow '(transpose shuffle lowest between invert and add scale))

(in-package :clm)
(shadow '(add-mark srt statistics scale interpolate))

(in-package :cmn)
(shadow '(add-bar beat-duration transpose end-arrow flatten add-clef stack
          double cmn-display get-rest tie count-notes scale trill lowest
          highest))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF package.lsp
