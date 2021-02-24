;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File:             notenames-scales-examples.lsp
;;;
;;; Class Hierarchy:  None
;;;
;;; Version:          1.0
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Lisp-code examples to accompany notenames-scales.html
;;;
;;; Author:           Sean Reed
;;;
;;; Creation date:    13th July 2012
;;;
;;; $$ Last modified: 22:43:24 Fri May 17 2013 BST
;;;
;;; SVN ID: $Id: notenames-scales-examples.lsp 3538 2013-05-18 08:29:15Z medward2 $
;;;
;;; ****
;;; Licence:          Copyright (c) 2012 Michael Edwards
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

;;; ============================================================================
;;; notenames-scales.html examples
;;; ============================================================================

;;; Making pitch objects within different tuning scales
;;; ----------------------------------------------------------------------------

;;; chromatic-scale
;;; ----------------------------------------------------------------------------
(in-scale :chromatic-scale)

(make-pitch 'bf3)

(make-pitch 'cs4)

;;; quarter-tone
;;; ----------------------------------------------------------------------------
(in-scale :quarter-tone)

(make-pitch 'bqf3)

(make-pitch 'fqs4)

;;; twelfth-tone
;;; ----------------------------------------------------------------------------
(in-scale :twelfth-tone)

(make-pitch 'btf3)

(make-pitch 'bsf3)

(make-pitch 'cts4)

(make-pitch 'css4)