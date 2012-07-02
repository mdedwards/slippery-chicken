;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* sc/lilypond
;;;
;;; File:             lilypond.lsp
;;;
;;; Class Hierarchy:  none, no classes defined
;;;
;;; Version:          1.0.0-beta2
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Some helper functions for creating Lilypond files for
;;;                   notation. Note that most Lilypond functionality is in the
;;;                   slippery-chicken, rthm-seq-bar, event, pitch, and chord
;;;                   classes.
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    30th January 2011
;;;
;;; $$ Last modified: 13:28:08 Mon Jul  2 2012 BST
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

(in-package :slippery-chicken)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; To use the markup commands which refer to graphics files, you will need to
;;; copy the graphics in doc/media/lilypond-graphics.zip 
;;; into the directory containing the lilypond source files you want
;;; to process.  
;;;
;;; num-flags is the number of quaver tails on a note; used to
;;; generate the right number of tremolo strokes (which are always measured in
;;; lp) 

;;; ****f* lilypond/lp-get-mark
;;; DESCRIPTION
;;; lp-get-mark:
;;; Translation function for Lilypond marks (dynamics, accents, etc.).  Not
;;; generally called by the user but the list of symbols that can be used will
;;; be useful.  If <silent> then non-existing marks will not produce
;;; warnings/errors (but we'll return nil). 
(defun lp-get-mark (mark &key (num-flags 0) silent)
  (flet ((no-lp-mark (mark)
           (when silent
             (warn "lilypond:lp-get-mark: Sorry but ~a is not yet available ~
                    for sc->Lilypond; ignoring" mark))
           ""))
    (when mark
      (typecase mark
        (symbol
         (case mark
;;; SYNOPSIS
           (a "-> ") ; accent
           (lhp "-+ ")
           ;; see p229 of lilypond.pdf: need to define this command in file
           (bartok "^\\snapPizzicato ") 
           (pizz "^\"pizz.\" ")
           (ord "^\"ord.\" ")
           (pizzp "^\"(pizz.)\" ")
           (clb "^\"clb\"")
           (cl "^\"cl\" ")
           (col-legno "^\"col legno\" ")
           (clt "^\"clt\" ")
           (arco "^\"arco\" ")
           (batt "^\"batt.\" ")
           (spe "^\"spe\" ")
           (sp "^\"sul pont.\" ")
           (mv "^\"molto vib.\" ")
           (sv "^\"senza vib.\" ")
           (poco-crini "^\"poco crini\" ")
           (s "-. ")
           (nail (no-lp-mark 'nail))
           (stopped (no-lp-mark 'stopped))
           (as "->-. ")
           (at "->-- ")
           (ts "-_ ")
           (te "-- ")
           ;; so unmeasured is implicit
           (t3 (format nil ":~a " (* 32 (expt 2 num-flags))))
           (flag "\\flageolet ")
           (niente "^\markup { niente } ")
           (pppp "\\pppp ")
           (ppp "\\ppp ")
           (pp "\\pp ")
           (p "\\p ")
           (mp "\\mp ")
           (mf "\\mf ")
           (f "\\f ")
           (ff "\\ff ")
           (fff "\\fff ")
           (ffff "\\ffff ")
           (sfz "\\sfz ")
           (downbow "\\downbow ")
           (upbow "\\upbow ")
           (open "\\open ")
           (0 "\\open ")
           (1 "-1 ")
           (2 "-2 ")
           (3 "-3 ")
           (4 "-4 ")
           (I "^\\markup { \\teeny \"I\" } ")
           (II "^\\markup { \\teeny \"II\" } ")
           (III "^\\markup { \\teeny \"III\" } ")
           (IV "^\\markup { \\teeny \"IV\" } ")
           (beg-sl "( ")
           (end-sl  ") ")
           ;; MDE Fri Apr  6 21:57:59 2012 -- apparently LP can't have nested
           ;; slurs but it does have phrase marks:  
           (beg-phrase "\\( ")
           (end-phrase "\\) ")
           ;; there's no start gliss / end gliss in lilypond
           (beg-gliss "\\glissando ")
           (end-gliss "")
           ;; 13.4.11
           (beg-8va "\\ottava #1 ")
           (end-8va "\\ottava #0 ")
           (beg-8vb "\\ottava #-1 ")
           (end-8vb "\\ottava #0 ")
           ;; NB note heads should be added via (add-mark-before ... so if
           ;; adding new, add the mark symbol to the move-elements call in
           ;; event::get-lp-data 
           (circled-x "\\once \\override NoteHead #'style = #'xcircle ")
           (x-head "\\once \\override NoteHead #'style = #'cross ")
           (triangle "\\once \\override NoteHead #'style = #'triangle ")
           (triangle-up "\\once \\override NoteHead #'style = #'do ")
           ;; (mensural "\\once \\override NoteHead #'style = #'slash ")
           ;;(flag-head "\\once \\override NoteHead #'style = #'harmonic-mixed
           ;;")  
           ;; MDE Mon Apr 30 20:46:06 2012 -- see event::get-lp-data for how
           ;; this is handled 
           (flag-head "\\harmonic ")
           ;; MDE Mon Apr 30 20:46:31 2012 -- flag-heads by default don't
           ;; display dots so we need to add-mark-before to get these to
           ;; display or turn them off again
           (flag-dots-on "\\set harmonicDots = ##t ")
           (flag-dots-off "\\set harmonicDots = ##f ")
           (airy-head (no-lp-mark 'airy-head))
           (none (no-lp-mark 'none))
           (trill-f (no-lp-mark 'trill-f))
           (trill-n (no-lp-mark 'trill-n))
           (trill-s (no-lp-mark 'trill-s))
           (beg-trill-a "\\pitchedTrill ") ; must be before note
           ;; we'll also need e.g. (trill-note g5) to give the note in ()
           (end-trill-a "\\stopTrillSpan ") ; after note
           (square (no-lp-mark 'square))
           (slash (no-lp-mark 'slash))
           (arrow-up (no-lp-mark 'arrow-up))
           (arrow-down (no-lp-mark 'arrow-down))
           (cresc-beg "\\< ")
           (cresc-end "\\! ")
           (dim-beg "\\> ")
           (dim-end "\\! ")
           (<< "<< ")
           (>> ">> ")
           ;; NB this override has to come exactly before the note/dynamic it
           ;; applies to 
           (hairpin0 "\\once \\override Hairpin #'circled-tip = ##t ")
           ;; (dim0-beg "\\once \\override Hairpin #'circled-tip = ##t \\> ")
           (pause "\\fermata ")
           (short-pause
            "^\\markup { \\musicglyph #\"scripts.ushortfermata\" } ")
           ;; MDE Thu Apr  5 16:17:11 2012 -- these need the graphics files in
           ;; lilypond-graphics.zip to be in the same directory as the
           ;; generated lilypond files  
           (aeolian-light "^\\aeolianLight ") 
           (aeolian-dark "^\\aeolianDark ")
           ;; this one uses the graphic for close bracket
           (bracket-end "^\\bracketEnd ")
           (mphonic "^\\mphonic ")
           (mphonic-arr "^\\mphonicArr ")
           (mphonic-cons "^\\mphonicCons ")
           (mphonic-diss "^\\mphonicDiss ")
           (mphonic-cluster "^\\mphonicCluster ")
           (sing "^\\sing ")
           (sing-arr "^\\singArr ")
           (arrow-up-down "^\\arrowUpDown ")
           ;;; end lilypond-graphics.zip files
           ;; these must have been set up with the event::add-arrow method
           (start-arrow "\\startTextSpan ")
           (end-arrow "\\stopTextSpan ")
           (harm "^\\flageolet ")
           ;; 2.3.11
           ;; write sost. pedal as text (usually held for long time so brackets
           ;; not a good idea)
           (ped "\\sustainOn ")
           (ped^ "\\sustainOff\\sustainOn ")
           (ped-up "\\sustainOff ")
           (uc "\\unaCorda ")
           (tc "\\treCorde ")
;;; ****
           (t (unless silent
                (error "lilypond::lp-get-mark: unrecognised mark: ~a" mark)))))
        (integer
         (when (or (> mark 5) (< mark 0))
           (warning "lilypond::lp-get-mark: adding fingering ~a, hope your ~
                     musicians have more than 4 fingers and a thumb!." mark))
         (format nil "^\\markup{\\finger ~a}" mark))
        ;; 25.6.11 a 2 element list will generate a 'transition arrow' with the
        ;; first element as the starting text and the second as end text.  The
        ;; elements will be converted to lowercase strings unless they're
        ;; already strings
        (list 
         (case (first mark)
           (arrow 
            (let ((current (second mark))
                  (target (third mark)))
              (unless (stringp current)
                (setf current (string-downcase current)))
              (unless (stringp target)
                (setf target (string-downcase target)))
              (format 
               nil
               "~%\\override TextSpanner #'bound-padding = #1.0 ~
            ~%\\override TextSpanner #'style = #'line ~%~
            \\override TextSpanner #'(bound-details right arrow) = ##t ~%~
            \\override TextSpanner #'(bound-details left text) = #\"~a\" ~%~
            \\override TextSpanner #'(bound-details right text) = #\"~a\"~%~
            \\override TextSpanner #'(bound-details right padding) = #0.6 ~%~
            \\override TextSpanner #'(bound-details right ~
                stencil-align-dir-y) = #CENTER ~%~
            \\override TextSpanner #'(bound-details left ~
                stencil-align-dir-y) = #CENTER~%" current target)))
           (trill-note
            (format nil "\\startTrillSpan ~a "
                    (get-lp-data (make-pitch (second mark)))))
           ;; 3/11/11 sometimes we just want to insert text as given, e.g. with
           ;; funny markup code
           (text (second mark))
           ;; MDE Sat Jun 30 12:06:08 2012 -- key signatures 
           ;; e.g. '(key fs major), but note that they will appear _after_ the
           ;; note they're attached to.
           (key (string-downcase
                 (format nil "\\key ~a \\~a " (second mark) (third mark))))
           (t (unless silent
                (error "lilypond::lp-get-mark: unrecognised mark as list: ~a"
                       mark)))))
        ;; 27.5.11: use expicit \markup command instead of ^
        ;; and here's a quick hack: put all strings up or down according to
        ;; whether there's a ^ or _ as first char, or if neither, it's up (^)
        (string (let* ((char1 (elt mark 0))
                       (up (char= char1 #\^))
                       (down (char= char1 #\_))
                       (mk (if (or up down)
                               (subseq mark 1)
                               mark))
                       (char1out (if down #\_ #\^)))
                  (format nil "~c\\markup { ~a } " char1out mk)))
        ;; if it's a list then it's a bunch of arguments to sc-cmn-text
        ;; otherwise it might be a mark (e.g. text) already
        ;; ignore cmn stuff but warn
        (t (unless silent
             (warn "lilypond::get-lp-mark: unknown mark: ~a" mark)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lp-set-instrument (ins &optional short) ; ins is a string
  (let ((name (lp-flat-sign ins)))
    (format nil "\\set Staff.~a = \\markup { ~a }"
            (if short "shortInstrumentName" "instrumentName")
            name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; NB The word flat is automatically replaced to the flat sign in CMN
(defun lp-flat-sign (name) ; string
  ;; MDE Thu Mar  1 09:59:24 2012 -- look for the hyphen version first
  (let ((handle-flat (string-replace "-flat" "\\flat" name)))
    ;; MDE Mon Feb 20 20:01:15 2012 -- maybe we've got -flat?
    (unless handle-flat
      (setf handle-flat (string-replace "flat" "\\flat" name)))
    (if handle-flat handle-flat name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 24.7.11 (Pula): format multi-line text

(defun lp-ml-text (&rest strings)
  (format nil "~&\\column {~{~&  \\line{~a}~^~}~&}~%" strings))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EOF lilypond.lsp
