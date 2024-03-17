;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* sc/lilypond
;;;
;;; File:             lilypond.lsp
;;;
;;; Class Hierarchy:  none, no classes defined
;;;
;;; Version:          1.1.0
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
;;; $$ Last modified:  15:11:39 Sat Mar 16 2024 CET
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
;;; Translation function for LilyPond marks (dynamics, accents, etc.).  Not
;;; generally called by the user but the list of symbols that can be used will
;;; be useful.  If <silent> then non-existing marks will not produce
;;; warnings/errors (but we'll return nil). 
(defun lp-get-mark (mark &key (num-flags 0)
                           ;; MDE Thu Sep 15 10:50:47 2016 -- default to the
                           ;; config setting 
                           (silent (not (get-sc-config 'warn-no-lp-mark))))
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
           (a "-> ")                    ; accent
           (lhp "-+ ")
           ;; see p229 of lilypond.pdf: need to define this command in file
           ;; this is done for us in lilypond.ly, which will be included if we
           ;; call write-lp-data-for-all with :use-custom-markup T 
           (bartok "^\\snappizzicato ") 
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
           (st "^\"sul tasto\" ")
           (mv "^\"molto vib.\" ")
           (sv "^\"senza vib.\" ")
           (poco-crini "^\"poco crini\" ")
           (s "-. ")
           (ss "-! ") ; MDE Thu Dec 2 15:16:27 2021, Heidhausen
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
           ;; MDE Sat Aug 11 15:51:16 2012 -- dynamics in parentheses
           (ffff-p "\\parenFFFF ")
           (fff-p "\\parenFFF ")
           (ff-p "\\parenFF ")
           (f-p "\\parenF ")
           (mf-p "\\parenMF ")
           (mp-p "\\parenMP ")
           (p-p "\\parenP ")
           (pp-p "\\parenPP ")
           (ppp-p "\\parenPPP ")
           (pppp-p "\\parenPPPP ")
           (sfz "\\sfz ")
           (downbow "\\downbow ")
           (upbow "\\upbow ")
           (open "\\open ")
           (I "^\\markup { \\teeny \"I\" } ")
           (II "^\\markup { \\teeny \"II\" } ")
           (III "^\\markup { \\teeny \"III\" } ")
           (IV "^\\markup { \\teeny \"IV\" } ")
           ;; MDE Thu Dec 26 14:14:34 2013 -- guitar string numbers
           (c1 "\\1 ")
           (c2 "\\2 ")
           (c3 "\\3 ")
           (c4 "\\4 ")
           (c5 "\\5 ")
           (c6 "\\6 ")
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
           ;; MDE Mon Jan 18 11:43:43 2016
           (beg-15ma "\\ottava #2 ")
           (end-15ma "\\ottava #0 ")
           (beg-15mb "\\ottava #-2 ")
           (end-15mb "\\ottava #0 ")
           ;; NB note heads used to be added via (add-mark-before) but can now
           ;; be added by add-mark and friends (16.6.20) so if adding new heads
           ;; , make sure to add the mark symbol to the move-elements call in
           ;; event::separate-marks-before
           ;; 
           ;; (circled-x "\\once \\override NoteHead #'style = #'xcircle ")
           (circled-x "\\tweak #'style #'xcircle ")
           ;; (x-head "\\once \\override NoteHead #'style = #'cross ")
           (x-head "\\xNote ")
           ;; (triangle "\\once \\override NoteHead #'style = #'triangle ")
           (triangle "\\tweak #'style #'triangle ")
           ;; (triangle-up "\\once \\override NoteHead #'style = #'do ")
           (triangle-up "\\tweak #'style #'do ")
           (airy-head (no-lp-mark 'airy-head)) 
           ;; this has to be added to the event _before_ the one which needs to
           ;; start with these noteheads.
           (improvOn "\\improvisationOn ")
           (improvOff "\\improvisationOff ")
           ;; MDE Sat Nov  9 20:21:19 2013 -- in CMN it's :breath-in: a
           ;; triangle on its side (pointing left)
           ;; (wedge "\\once \\override NoteHead #'style = #'fa ")
           (wedge "\\tweak #'style #'fa ")
           ;; (square "\\once \\override NoteHead #'style = #'la ")
           (square "\\tweak #'style #'la ")
           ;; (mensural "\\once \\override NoteHead #'style = #'slash ")
           ;;(flag-head "\\once \\override NoteHead #'style = #'harmonic-mixed
           ;;")  
           ;; MDE Mon Apr 30 20:46:06 2012 -- see event::get-lp-data for how
           ;; this is handled 
           (flag-head "\\harmonic ") ; diamond
           ;; MDE Mon Apr 30 20:46:31 2012 -- flag-heads by default don't
           ;; display dots so we need to add-mark-before to get these to
           ;; display or turn them off again
           (flag-dots-on "\\set harmonicDots = ##t ")
           (flag-dots-off "\\set harmonicDots = ##f ")
           ;; circle head but stem extends through it like a vertical slash
           (none (no-lp-mark 'none))
           (trill-f (no-lp-mark 'trill-f))
           (trill-n (no-lp-mark 'trill-n))
           (trill-s (no-lp-mark 'trill-s))
           (beg-trill-a "\\pitchedTrill ") ; must be before note
           ;; we'll also need e.g. (trill-note g5) to give the note in ()
           (end-trill-a "\\stopTrillSpan ") ; after note
           ;; (no-lp-mark 'square))
           (slash (no-lp-mark 'slash))
           ;; MDE Sat Dec 28 11:37:22 2013 -- up and down arrows on arpeggio
           ;; lines will need more complex treatment (need a note-before mark
           ;; :/ ) 
           (arp "\\arpeggio ")
           (arrow-up (no-lp-mark 'arrow-up))
           (arrow-down (no-lp-mark 'arrow-down))
           (cresc-beg "\\< ")
           (cresc-end "\\! ")
           (dim-beg "\\> ")
           (dim-end "\\! ")
           (<< "<< ")
           (>> ">> ")
           ;; NB this override has to come exactly before the note/dynamic it
           ;; applies to. MDE Tue Jun 16 23:38:57 2020, Heidhausen -- again this
           ;; override should be OK (rather than tweak) as it's below the staff
           ;; rather than a property of a note (in a chord)
           ;; (hairpin0 "\\once \\override Hairpin #'circled-tip = ##t ")
           ;; MDE Thu May 11 15:26:00 2023, Heidhausen -- version 2.21 changes
           (hairpin0 "\\once \\override Hairpin.circled-tip = ##t ")
           ;; (dim0-beg "\\once \\override Hairpin #'circled-tip = ##t \\> ")
           (pause "\\fermata ")
           (long-pause "\\longfermata ")
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
           (high-sine "^\\high-sine ")
           (noise "^\\noise ")
           (focus "^\\focus ")
           (balance "^\\balance ")
           (alternate "^\\alternate ")
           (sing-arr "^\\singArr ")
           (arrow-up-down "^\\arrowUpDown ")
           ;; end lilypond-graphics.zip files
           ;; these must have been set up with the event::add-arrow method
           (start-arrow "\\startTextSpan ")
           (end-arrow "\\stopTextSpan ")
           (harm "^\\flageolet ")
           ;; 2.3.11
           ;; write sost. pedal as text (usually held for long time so brackets
           ;; not a good idea)
           ;; MDE Wed May 25 12:40:57 2016 -- update: do use the LP
           ;; sost. brackets after all
           (sost "\\sostenutoOn ")
           (sost-up "\\sostenutoOff ")
           (sost^ "\\sostenutoOff\\sostenutoOn ")
           (ped "\\sustainOn ")
           (ped^ "\\sustainOff\\sustainOn ")
           (ped-up "\\sustainOff ")
           (uc "\\unaCorda ")
           (tc "\\treCorde ")
;;; ****
           ;; MDE Fri May  7 12:46:09 2021, Heidhausen -- ignore cmn marks
           (t (unless silent
                (error "lilypond::lp-get-mark: unrecognised mark: ~a" mark)))))
        (integer
         (case mark
           (0 "\\open ")
           (1 "-1 ")
           (2 "-2 ")
           (3 "-3 ")
           (4 "-4 ")
           (5 "-5 ")
           (t (warn "lilypond::lp-get-mark: adding fingering ~a, hope your ~
                        ~%musicians have more than 4 fingers and a thumb!."
                       mark)
              (format nil "^\\markup{\\finger ~a}" mark))))
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
               ;; MDE Tue Jun 16 23:37:43 2020, Heidhausen -- leave arrows with
               ;; override rather than tweak as these are above the staff and
               ;; not linked (yet?) to single notes in a chord
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
           ;; MDE Fri Feb 17 16:01:04 2017 -- for getting lines to go to the
           ;; right places in gliss chords; see http://tinyurl.com/jopurj7
           (gliss-map (format nil "\\set glissandoMap = #'~a " (second mark)))
           ;; MDE Sat Mar  4 11:30:35 2017 -- subito
           (sub (string-downcase
                 (format nil "_\\markup { \\dynamic ~a \\italic sub. } "
                         (second mark))))
           (trill-note
            (format nil "\\startTrillSpan ~a "
                    (get-lp-data (make-pitch (second mark)))))
           ;; 3/11/11 sometimes we just want to insert text as given, e.g. with
           ;; funny markup code. For instance, we can make a notehead small and
           ;; in parentheses with (add-mark-before <pitch> '(text
           ;; "\\parenthesize \\tweak font-size -2"))))
           (text (concatenate 'string (second mark) " "))
           ;; the staff name will be the lower case version of your player name
           ;; with all - and _ characters removed. If in any doubt, look at the
           ;; "music = { " block in your Lilypond -def.ly file
           (staff (format nil "\\change Staff = \"~a\"" (second mark)))
           (rgb  ; list of three rgb values between 0.0 and 1.0
            ;; MDE Thu Dec 10 16:13:35 2020, Heidhausen -- if there's a 4th
            ;; element (e.g. t) in the rgb list, use override (e.g. to colour a
            ;; whole chord) otherwise just tweak (for individual chord note
            ;; colours)
            (let* ((rgb (second mark))
                   (r (first rgb))
                   (g (second rgb))
                   (b (third rgb))
                   (override (fourth rgb)))
              (format 
               nil
               (if override
                   "\\once \\override NoteHead.color = #(rgb-color ~a ~a ~a) ~
                  \\once \\override Beam.color = #(rgb-color ~a ~a ~a) ~
                  \\once \\override Accidental.color = #(rgb-color ~a ~a ~a) ~
                  \\once \\override Flag.color = #(rgb-color ~a ~a ~a) ~
                  \\once \\override Stem.color = #(rgb-color ~a ~a ~a) "
                   ;; MDE Tue Jun 16 23:33:15 2020, Heidhausen -- use tweak
                   ;; instead and set individual properties rather tweaking the
                   ;; whole note as that causes things like text to change color
                   ;; too, I believe. NB if you set the rgb of a note in a
                   ;; chord, whether the beam, flag etc. are that colour depends
                   ;; on which note those are attached to.
                   "\\tweak NoteHead.color #(rgb-color ~a ~a ~a) 
                  \\tweak Beam.color #(rgb-color ~a ~a ~a) ~
                  \\tweak Accidental.color #(rgb-color ~a ~a ~a) ~
                  \\tweak Flag.color #(rgb-color ~a ~a ~a) ~
                  \\tweak Stem.color  #(rgb-color ~a ~a ~a) ")
               r g b r g b r g b r g b r g b)))
           ;; MDE Sat Jun 30 12:06:08 2012 -- key signatures 
           ;; e.g. '(key fs major), but note that they will appear _after_ the
           ;; note they're attached to.
           (key (get-lp-key-sig (second mark) (third mark)))
           ;;; A salzedo mark for harp pedalling indications
           ;;; E.g.: '(salzedo (1 1 0 -1 1 0 -1))
           ;;; RP  Tue Dec 19 19:01:57 2023
           (salzedo (lp-salzedo-mark (second mark)))
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
        ;; MDE Fri May  7 12:46:09 2021, Heidhausen -- ignore cmn marks
        (t (unless (or silent (typep mark 'cmn::sundry))
             (warn "lilypond::get-lp-mark: unknown mark: ~a" mark)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-lp-key-sig (ton art)
  (string-downcase
   (format nil "\\key ~a \\~a " ton art)))

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
;;; MDE Thu Jun 25 09:47:59 2015 -- we format a Lilypond string for a tuplet up
;;; to the opening { When explicit ratios are given, we make sure they're
;;; printed in the bracket in the score like 5:4, otherwise an integer is
;;; placed in the bracket without qualification.

(defun get-lp-tuplet (tuplet)
  (let ((tup (get-tuplet-ratio tuplet)))
    (typecase tuplet
      (integer (format nil "\\times ~a { " tup))
      ;; This code was in the body of event::get-lp-data:
      ;; MDE Thu Jun 4 11:16:07 2015 -- now allow explicit ratios that will
      ;; show up in brackets as e.g. 5:4 (expressed as 4/5 in the rthm-seq-bar
      ;; list of rhythms) instead of just 5
      (rational
       ;; MDE Tue Jun 16 23:37:05 2020, Heidhausen -- shouldn't need tweak here
       ;; as this won't need to work on separate notes in a chord
       (format nil "\\once \\override TupletNumber.text = ~
                    #tuplet-number::calc-fraction-text \\times ~a { " tup)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* lilypond/salzedo-to-ly-string
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-12-19
;;; 
;;; DESCRIPTION
;;; This method converts a salzedo list to a string to be used with LilyPond's
;;; \harp-pedal markup. Cf.
;;; https://lilypond.org/doc/v2.23/Documentation/notation/harp
;;;
;;; N.B.: Steinberg's Dorico notation software uses the same syntax for entering
;;; harp pedal diagrams. 
;;;
;;; ARGUMENTS
;;; A salzedo list (cf. harp-salzedo-to-tl-set). 
;;; 
;;; RETURN VALUE
;;; The LilyPond salzedo-string. 
;;;
;;; EXAMPLE
#|
(salzedo-to-ly-string '(0 1 0 1 -1 1 0))
=>
"-v-|v^v-"
|#
;;; SYNOPSIS
(defun salzedo-to-ly-string (salzedo)
;;; ****  
  ;;; sanity checks
  (unless (salzedo-p salzedo)
    (error "lilypond::salzedo-to-ly-string: The given list ~a is not a valid ~
            salzedo-list." salzedo))
  (let ((translation (loop for ped in salzedo
                           for i from 0
                           for trans = (case ped
                                         (-1 "^")
                                         (0 "-")
                                         (1 "v"))
                           append
                           (if (eq i 2)
                               (list trans "|")
                               (list trans))
                           )))
    (format nil "~{~a~}" translation)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* lilypond/lp-salzedo-mark
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-12-19
;;; 
;;; DESCRIPTION
;;; Returns the LilyPond markup for a harp salzedo pedal-mark given as a salzedo
;;; list (cf. harp-salzedo-to-tl-set).
;;;
;;; ARGUMENTS
;;; - The salzedo list. E.g. '(0 -1 1 0 1 -1 1) for d, cf, bs, e, fs, gf, as
;;;
;;; EXAMPLE
#|
(lp-salzedo-mark '(1 -1 1 0 1 0 1))
;; => "_\\markup { \\harp-pedal \"v^v|-v-v\" }"
|#
;;; SYNOPSIS
(defun lp-salzedo-mark (salzedo)
;;; ****
  (unless (salzedo-p salzedo)
    (error "lilypond::lp-salzedo-mark: ~a is not of type salzedo list" salzedo))
  (let ((lp-salzedo (salzedo-to-ly-string salzedo)))
    (format nil "_\\markup { \\harp-pedal \"~a\" }" lp-salzedo)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; EOF lilypond.lsp
