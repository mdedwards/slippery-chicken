;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* sc/cm-load
;;; NAME 
;;; cm-load
;;;
;;; File:             cm-load.lsp
;;;
;;; Class Hierarchy:  none (no classes defined)
;;;
;;; Version:          0.92
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Definition of the common-music quarter-tone scale and 
;;;                   twelfth-tone scale which should be loaded and not
;;;                   compiled.  The quarter tone scale is our default
;;;                   No public interface envisaged (so no robodoc entries).
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    7th February 2003
;;;
;;; $$ Last modified: 18:12:03 Wed Jan  4 2012 ICT
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

;;; 02.12.11 SEAN: Changed robodoc header

(in-package :cm)

;;; NB if any new tunings are added here, their microtone accidentals will have
;;; to be added to the case test in pitch.lsp::set-white-note in order to get
;;; the nearest chromatic note.

(new tuning
     :name 'quarter-tone
     :octaves '(-1 10)
     ;; I never use 3/4 flat/sharp--no need.
     :cents '((50 c 
                  (cn :accidental n) 
                  (bs :accidental s :octave -1))
              (50 (cqs :accidental qs))
              (50 (cs :accidental s) 
                  (df :accidental f))
              (50 (dqf :accidental qf))
              (50 d 
                  (dn :accidental  n))
              (50 (dqs :accidental qs))
              (50 (ef :accidental f)
                  (ds :accidental s))
              (50 (eqf :accidental qf))
              (50 e 
                  (en :accidental n)
                  (ff :accidental f))
              (50 (eqs :accidental qs)
                  (fqf :accidental qf))
              (50 f 
                  (fn :accidental n) 
                  (es :accidental s))
              (50 (fqs :accidental qs))
              (50 (fs :accidental s)
                  (gf :accidental f))
              (50 (gqf :accidental qf))
              (50 g 
                  (gn :accidental n))
              (50 (gqs :accidental qs))
              (50 (af :accidental f) 
                  (gs :accidental s))
              (50 (aqf :accidental qf))
              (50 a 
                  (an :accidental n)) 
              (50 (aqs :accidental qs))
              (50 (bf :accidental f) 
                  (as :accidental s))
              (50 (bqf :accidental qf))
              (50 b 
                  (bn :accidental n)
                  (cf :accidental f :octave +1))
              (50 (bqs :accidental qs) 
                  (cqf :accidental qf :octave +1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(new tuning
     :name 'twelfth-tone
     :octaves '(-1 10)
     :cents '((100/6 c
                     (cn :accidental n)
                     (bs :accidental s :octave -1))
              (100/6 (cts :accidental ts))
              (100/6 (css :accidental ss))
              (100/6 (cqs :accidental qs))
              (100/6 (cssf :accidental ssf) 
               ;; flat with 1/6th down can't be notated yet so leave out...
               ;; (dfsf :accidental fsf)
               )
              (100/6 (cstf :accidental stf)
               ;; flat with 1/12th down can't be notated yet so leave out...
               ;; (dftf :accidental ftf)
               )
              (100/6 (cs :accidental s) 
                     (df :accidental f))
              (100/6 (csts :accidental sts)
                     (dfts :accidental fts)) 
              (100/6 (csss :accidental sss)
                     (dfss :accidental fss))
              (100/6 (dqf :accidental qf))
              (100/6 (dsf :accidental sf))
              (100/6 (dtf :accidental tf))

              (100/6 d
                     (dn :accidental  n))
              (100/6 (dts :accidental ts))
              (100/6 (dss :accidental ss))
              (100/6 (dqs :accidental qs))
              (100/6 (dssf :accidental ssf))
              (100/6 (dstf :accidental stf))
              (100/6 (ds :accidental s) 
                     (ef :accidental f))
              (100/6 (dsts :accidental sts)
                     (efts :accidental fts))
              (100/6 (dsss :accidental sss)
                     (efss :accidental fss))
              (100/6 (eqf :accidental qf))
              (100/6 (esf :accidental sf))
              (100/6 (etf :accidental tf))
              
              (100/6 e
                     (en :accidental  n))
              (100/6 (ets :accidental ts))
              (100/6 (ess :accidental ss))
              (100/6 (eqs :accidental qs)
                     (fqf :accidental qf))
              (100/6 (fsf :accidental sf))
              (100/6 (ftf :accidental tf))
              
              (100/6 f
                     (fn :accidental  n)
                     (es :accidental  n))
              (100/6 (fts :accidental ts))
              (100/6 (fss :accidental ss))
              (100/6 (fqs :accidental qs))
              (100/6 (fssf :accidental ssf) 
               ;; (gfsf :accidental fsf)
               )
              (100/6 (fstf :accidental stf) 
               ;; (gftf :accidental ftf)
               )
              (100/6 (fs :accidental s) 
                     (gf :accidental f))
              (100/6 (fsts :accidental sts)
                     (gfts :accidental fts))
              (100/6 (fsss :accidental sss)
                     (gfss :accidental fss))
              (100/6 (gqf :accidental qf))
              (100/6 (gsf :accidental sf))
              (100/6 (gtf :accidental tf))
 
              (100/6 g
                     (gn :accidental  n))
              (100/6 (gts :accidental ts))
              (100/6 (gss :accidental ss))
              (100/6 (gqs :accidental qs))
              (100/6 (gssf :accidental ssf) 
               ;; (afsf :accidental fsf)
               )
              (100/6 (gstf :accidental stf) 
               ;; (aftf :accidental ftf)
               )
              (100/6 (gs :accidental s) 
                     (af :accidental f))
              (100/6 (gsts :accidental sts)
                     (afts :accidental fts))
              (100/6 (gsss :accidental sss)
                     (afss :accidental fss))
              (100/6 (aqf :accidental qf))
              (100/6 (asf :accidental sf))
              (100/6 (atf :accidental tf))

              (100/6 a
                     (an :accidental  n))
              (100/6 (ats :accidental ts))
              (100/6 (ass :accidental ss))
              (100/6 (aqs :accidental qs))
              (100/6 (assf :accidental ssf) 
               ;; (bfsf :accidental fsf)
               )
              (100/6 (astf :accidental stf) 
               ;; (bftf :accidental ftf)
               )
              (100/6 (as :accidental s) 
                     (bf :accidental f))
              (100/6 (asts :accidental sts)
                     (bfts :accidental fts))
              (100/6 (asss :accidental sss)
                     (bfss :accidental fss))
              (100/6 (bqf :accidental qf))
              (100/6 (bsf :accidental sf))
              (100/6 (btf :accidental tf))

              (100/6 b
                     (bn :accidental  n))
              (100/6 (bts :accidental ts))
              (100/6 (bss :accidental ss))
              (100/6 (bqs :accidental qs)
                     (cqf :accidental qf :octave 1))
              (100/6 (csf :accidental sf :octave 1))
              (100/6 (ctf :accidental tf :octave 1))
              ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf *scale* (find-object 'quarter-tone))
;; (setf *scale* (find-object 'chromatic-scale))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EOF cm-load.lsp

