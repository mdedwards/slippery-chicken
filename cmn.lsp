;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* sc/cmn
;;; NAME 
;;; cmn
;;;
;;; File:             cmn.lsp
;;;
;;; Class Hierarchy:  None: no classes defined.
;;;
;;; Version:          1.0.0-beta
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Interface from complete-set to Bill's CMN package for
;;;                   displaying of sets in musical notation.
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    11th February 2002
;;;
;;; $$ Last modified: 20:48:46 Mon Apr 30 2012 BST
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

(eval-when (compile)
  (declaim (optimize (speed 3) (safety 1) (space 0) (debug 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cmn)

(declaim (special *cmn-units*))
(setf *cmn-units* :cm)

;;; We're not going to have more than 20 nested brackets applied to a single
;;; note are we? :=) 
(defparameter *cmn-open-brackets-for-sc* (make-list 20))

(defparameter *cmn-grace-notes-for-sc* nil)

(defparameter *cmn-bar-num-dx-for-sc* -0.2)
(defparameter *cmn-bar-num-dy-for-sc* 1.2)
(defparameter *cmn-bar-num-size-for-sc* 6)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; called automatically if placed as value of page-hook dimensions of current
;; page are in score variables page-width, page-height,
;; left/right/header/footer-margin (in inches), "page" arg is the current
;; page number

(defun sc-page-number (score page &optional (ignore))
  (declare (ignore ignore))
  (show score (text (format nil "~D" page)  (font-scaler .5))
        :matrix (list 1 0 0 1
                      (* (scr-size score)
                         (in-inches (+ (right-margin score)
                                       (* .5 (page-width score)) -.5)))
                      (* (scr-size score) 
                         (in-inches (* .75 (footer-margin score)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 27/5/07: tempo is now a list, first element is the result of a call to
;;; cmn::mm, the (optional) second element is a textual description.
;;; 16.3.11 ins-change is nil or a string
(defun cmn-rest (rq dots flags brackets bar-num cmn-marks time tempo ins-change)
  (apply #'rest 
         (append
          (list (rq rq)
                (cmn-bar-number bar-num)
                (dots dots)
                (flags flags)
                (dy 0)
                time
                (first tempo)
                (second tempo))
          (get-all-cmn-marks cmn-marks)
          (when ins-change
            (list (new-staff-name ins-change)
                  (sc-cmn-text ins-change)))
          (cmn-tuplet-brackets brackets))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cmn-bar-line (bar-num &optional (write-bar-nums t) type 
                     rehearsal-letter)
  (let ((rhl (when rehearsal-letter 
               ;; (print rehearsal-letter)
               ;; (print write-bar-nums)
               (text (format nil "~D" rehearsal-letter)
                     (font-name "Verdana-Bold")
                     (font-size 14)
                     (dx -0.2)
                     (dy 1.7))))
        #|
        ;; it seems we can't add these except at the top of a group of staves! ;
        (rehearsal-letter rehearsal-letter 
        (dy 0.5)
        (rehearsal-frame-white-space 0.15)
        (frame :box))))
  |#
        ;; 10/3/07: seems to me this first is never used...
        ;; 2/4/07: it is now since changing 2nd arg of rsb::get-cmn-data to T
        (bnum (when (and bar-num 
                         write-bar-nums
                         (not (zerop bar-num)))
                ;; (zerop (mod bar-num 5)))
                #|
                ;; same as rehearsal-letters! ;
  (measure-number bar-num
                ;; (dy 1.5)             ;
                (font-name "Verdana-Bold")
                (font-size *cmn-bar-num-size-for-sc*)
                ;; (frame :box)         ;
                ))))
                |#
                (cmn-bar-number bar-num))))
    (case type
      (0 (bar (height 1.0) bnum rhl))
      (1 (interior-double-bar (height 1.0) bnum rhl))
      (2 (double-bar (height 1.0) bnum rhl))
      ;; MDE Wed Mar 21 07:44:10 2012 -- added repeat barlines
      (3 (begin-repeat-bar (height 1.0) bnum rhl))
      (4 (begin-and-end-repeat-bar (height 1.0) bnum rhl))
      (5 (end-repeat-bar (height 1.0) bnum rhl)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cmn-bar-number (bar-num)
  ;; rehearsal numbers go haywire when attached to notes or rests so use normal
  ;; text instead... 
  ;; (print bar-num)
  (when bar-num
    (text (format nil "~D" bar-num)
          (font-name "Verdana-Bold")
          (font-size *cmn-bar-num-size-for-sc*)
          (dx *cmn-bar-num-dx-for-sc*)
          (dy *cmn-bar-num-dy-for-sc*) ;; (frame :box)
          )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-all-cmn-marks (marks)
  ;; 13.4.11 we've pushed marks in so reverse them here
  ;; (print marks)
  (loop for mark in (reverse marks) ;appending
       for cmn-marks = (get-cmn-marks mark)
       when (and cmn-marks (listp cmn-marks))
       append cmn-marks))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-cmn-marks (mark &key
                      silent
                      (dx 0.0)
                      (dy 0.0)
                      text
                      (text-dy 0.0)
                      (text-dx 0.7) 
                      (text-font "Times-Italic")
                      (text-font-size 9))
  ;; (declare (special accent staccato tenuto))
  ;; (print text)
  (flet ((no-cmn-mark (mark)
           (unless silent
             (warn "cmn:get-cmn-marks: Sorry but ~a is not yet available ~
                    for cmn output; ignoring" mark))
           ;; ""))
           nil))
    (when mark
      (setf mark (sc::rm-package mark :cmn))
      (let ((the-text (when text (sc-cmn-text text :dy text-dy :dx text-dx 
                                              :font text-font
                                              :font-size text-font-size))))
        (typecase mark
          (symbol
           (case mark
             (a (list (accent (dx dx) (dy dy) the-text)))
             (lhp (list (left-hand-pizzicato (dx dx) (dy dy) the-text)))
             (bartok (list (bartok-pizzicato (dx dx) (dy dy) the-text)))
             (s (list (staccato (dx dx) (dy dy) the-text)))
             (nail (list (fingernail (dx dx) (dy dy) the-text)))
             (stopped (list (stopped-note (dx dx) (dy dy) the-text)))
             (as (list (accent (dx dx) (dy dy) the-text) staccato))
             (at (list (accent (dx dx) (dy dy) the-text) tenuto))
             (ts (list (tenuto (dx dx) (dy dy) the-text) staccato))
             (te (list (tenuto (dx dx) (dy dy) the-text)))
             (t3 (list (tremolo (tremolo-slashes 3) 
                                (dx dx) (dy dy) the-text)))
             (pppp (list (pppp (dx dx) (dy dy) the-text)))
             (ppp (list (ppp (dx dx) (dy dy) the-text)))
             (pp (list (pp (dx dx) (dy dy) the-text)))
             (p (list (p (dx dx) (dy dy) the-text)))
             (mp (list (mp (dx dx) (dy dy) the-text)))
             (mf (list (mf (dx dx) (dy dy) the-text)))
             (f (list (f (dx dx) (dy dy) the-text)))
             (ff (list (ff (dx dx) (dy dy) the-text)))
             (fff (list (fff (dx dx) (dy dy) the-text))) 
             (ffff (list (ffff (dx dx) (dy dy) the-text))) 
             (pppp-p (list (pppp in-parentheses (dx dx) (dy dy) the-text)))
             (ppp-p (list (ppp in-parentheses (dx dx) (dy dy) the-text)))
             (pp-p (list (pp in-parentheses (dx dx) (dy dy) the-text)))
             (p-p (list (p in-parentheses (dx dx) (dy dy) the-text)))
             (mp-p (list (mp in-parentheses (dx dx) (dy dy) the-text)))
             (mf-p (list (mf in-parentheses (dx dx) (dy dy) the-text)))
             (f-p (list (f in-parentheses (dx dx) (dy dy) the-text)))
             (ff-p (list (ff in-parentheses (dx dx) (dy dy) the-text)))
             (fff-p (list (fff in-parentheses (dx dx) (dy dy) the-text)))
             (ffff-p (list (ffff in-parentheses (dx dx) (dy dy) the-text)))
             ;; NB adding a gliss to a chord not possible in CMN--try adding to
             ;; a note in a chord instead
             (beg-gliss (list begin-glissando))
             (end-gliss (list end-glissando))
             ;; 13.4.11
             (beg-8va (list begin-octave-up))
             (end-8va (list end-octave-up))
             (beg-8vb (list begin-octave-down))
             (end-8vb (list end-octave-down))
             (I (list (fingering "I" (dx dx) (dy dy) the-text)))
             (II (list (fingering "II" (dx dx) (dy dy) the-text)))
             (III (list (fingering "III" (dx dx) (dy dy) the-text)))
             (IV (list (fingering "IV" (dx dx) (dy dy) the-text)))
             (I-II (list (fingering "I" "II" (dx dx) (dy dy) the-text)))
             (I-II-III (list (fingering "I" "II" "III" (dx dx) (dy dy)
                                        the-text)))
             (II-III (list (fingering "II" "III" (dx dx) (dy dy) the-text)))
             (III-IV (list (fingering "III" "IV" (dx dx) (dy dy) the-text)))
             ;; in case of our slurs overlapping cmn auto-slurs (grace
             ;; notes) try the setf form.
             ;; (setf *sc-slur-tag* (begin-slur))))
             ;; N.B. no text for slurs!
             (beg-sl (list begin-slur))
             (end-sl (list end-slur)) ;; (end-slur *sc-slur-tag*)))
             ;; MDE Fri Apr  6 21:57:59 2012 -- apparently LP can't have nested
             ;; slurs but it does have phrase marks, but CMN can have nested
             ;; slurs and doesn't have a phrase mark, so use slurs instead
             (beg-phrase (list begin-slur))
             (end-phrase (list end-slur)) ;; (end-slur *sc-slur-tag*)))
             (airy-head (list (note-head :airy-head)))
             (flag-head (list (note-head :artificial-harmonic)))
             ;; MDE Mon Apr 30 20:48:18 2012 -- 
             (flag-dots-on (no-cmn-mark 'flag-dots-on))
             (flag-dots-off (no-cmn-mark 'flag-dots-off))
             (none (list (note-head :none)))
             (circled-x (list (note-head :circled-x)))
             ;; (bartok-pizz (list (bartok-pizzicato)))
             (trill-f (list (trill (ornament-sign flat))))
             (trill-n (list (trill (ornament-sign natural))))
             (trill-s (list (trill (ornament-sign sharp))))
             ;; 23.9.11 trill with aux note in ()
             (beg-trill-a (no-cmn-mark 'beg-trill-a))
             (end-trill-a (no-cmn-mark 'end-trill-a))
             (x-head (list (note-head :x)))
             (square (list (note-head :square)))
             (slash (list (note-head :slash)))
             (triangle (list (note-head :triangle)))
             (arrow-up (list (note-head :arrow-up)))
             (arrow-down (list (note-head :arrow-down)))
             (cresc-beg (list (begin-crescendo)))
             (cresc-end (list (end-crescendo)))
             (dim-beg (list (begin-diminuendo)))
             (dim-end (list (end-diminuendo)))
             ;; 23.8.11: little circles at start/end of dynamics
             (hairpin0 (no-cmn-mark 'hairpin0))
             (<< (no-cmn-mark '<<))
             (>> (no-cmn-mark '>>))
             ;; (dim0-beg (no-cmn-mark 'dim0-beg) 
             ;;       (list (begin-diminuendo))) ; just do a normal cresc
             ;; 28.2.11 hard to believe I didn't have these already.  
             (pause (list (fermata)))
             (pizz (list (sc-cmn-text "pizz")))
             (ord (list (sc-cmn-text "ord.")))
             (niente (list (sc-cmn-text "niente")))
             (pizzp (list (sc-cmn-text "(pizz)")))
             (clb (list (sc-cmn-text "clb")))
             (clt (list (sc-cmn-text "clt")))
             (cl (list (sc-cmn-text "cl")))
             (col-legno (list (sc-cmn-text "col legno")))
             (arco (list (sc-cmn-text "arco")))
             (batt (list (sc-cmn-text "batt")))
             (spe (list (sc-cmn-text "spe")))
             (sp (list (sc-cmn-text "sul pont.")))
             (sv (list (sc-cmn-text "senza vib")))
             (mv (list (sc-cmn-text "molto vib")))
             (poco-crini (list (sc-cmn-text "poco crini")))
             (harm (list (natural-harmonic)))
             ;; 20.6.11 for the flute: see lilypond.lsp for the actual symbols
             ;; we want; these here are just gap-fillers 23.7.11 (Pula): just
             ;; to keep lilypond targetted scores happy
             (triangle-up (list (note-head :triangle)))
             (bracket-end (no-cmn-mark 'bracket-end))
             (aeolian-light (list (natural-harmonic)))
             (aeolian-dark (list (thumb)))
             (sing (no-cmn-mark 'sing))
             (mphonic (no-cmn-mark 'mphonic))
             (mphonic-cons (no-cmn-mark 'mphonic-cons))
             (mphonic-diss (no-cmn-mark 'mphonic-diss))
             (mphonic-cluster (no-cmn-mark 'mphonic-cluster))
             (sing-arr (no-cmn-mark 'sing-arr))
             (mphonic-arr (no-cmn-mark 'mphonic-arr))
             (arrow-end (no-cmn-mark 'arrow-end))
             (arrow-up-down (no-cmn-mark 'arrow-up-down))
             (start-arrow (no-cmn-mark 'start-arrow))
             (end-arrow (no-cmn-mark 'end-arrow))
             (open (list (open-note)))
             ;; there is no short pause in CMN so just use pause i.e. //
             (short-pause (list (pause))) ;(list (martellato (scale 2 2))))
             ;; 2.3.11
             (ped (list (pedal-)))
             (ped^ (list (-pedal-)))
             (ped-up (list (-pedal)))
             (uc (list (sc-cmn-text "una corda")))
             (tc (list (sc-cmn-text "tre corde")))
             (t (unless silent
                  (error "cmn::get-cmn-marks: unrecognised mark: ~a" mark)))))
          (string (list (sc-cmn-text mark)))
          (number
           (when (or (> mark 5) (< mark 0))
             (warning "cmn::get-cmn-marks: adding fingering ~a, hope your ~
                       musicians have more than 4 fingers and a thumb!." mark))
           (case mark
             (0 (list (fingering 0 (dx dx) (dy dy) the-text)))
             (1 (list (fingering 1 (dx dx) (dy dy) the-text)))
             (2 (list (fingering 2 (dx dx) (dy dy) the-text)))
             (3 (list (fingering 3 (dx dx) (dy dy) the-text)))
             (4 (list (fingering 4 (dx dx) (dy dy) the-text)))
             (5 (list (fingering 5 (dx dx) (dy dy) the-text)))))
          ;; if it's a list then it's usually a bunch of arguments to
          ;; sc-cmn-text 
          (list ;; (print (symbolp (print (first mark))))
           (case (sc::rm-package (first mark) :cmn)
             (arrow (no-cmn-mark 'arrow))           ; ignore lilypond arrow
             (trill-note (no-cmn-mark 'trill-note)) ; ignore lilypond trill
             ;; this is also for lilypond markup code so ignore
             (text (no-cmn-mark (format nil "lilypond-markup: ~a" mark)))
             (t (list (apply #'sc-cmn-text mark)))))
          ;; otherwise it might be a cmn-mark (e.g. text) already
          (t (list mark)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Marks should not consist of slurs!  Actually, cmn has a problem
;;; with dynamics for grace notes too! 
(defun cmn-grace-note (note marks)
  ;; (print 'clm-grace-note)
  ;; (print note)
  ;; (when marks (print 'marks))
  (push note *cmn-grace-notes-for-sc*)
  (loop for m in marks do
        (loop for cmn-mark in (get-cmn-marks m) do
              (push cmn-mark *cmn-grace-notes-for-sc*)))
  ;; the 1/8th note rhythm makes sure only 1 beam is placed over multiple grace
  ;; notes.
  ;; (push e *cmn-grace-notes-for-sc*)
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Despite the name, this handles chords too when given as a list of already
;;; created cmn notes (as prepared by chord::get-cmn-data).
;;; 
;;; 27/5/07: tempo is now a list, first element is the result of a call to
;;; cmn::mm, the (optional) second element is a textual description.
;;; 
;;; 16.3.11 ins-change is nil or a string
(defun cmn-note (note rq dots flags beam brackets tied-to tied-from bar-num
                 marks time tempo ins-change)
  ;; (format t "~%~a ~a ~a ~a ~a ~a ~a ~a ~a ~a "
  ;;     note rq dots flags beam brackets tied-to tied-from bar-num marks)
  (let ((note-symbol (symbolp note))
        (chord (listp note)))
    ;; 21.9.11 as far as I can tell cmn can't handle glissandi on chords so
    ;; remove them
    (when chord
      (when (or (member 'sc::end-gliss marks)
                (member 'sc::beg-gliss marks))
        (warn "cmn::cmn-note: glissandi lines on chords not possible in CMN.")
        (setf marks (remove 'sc::beg-gliss (remove 'sc::end-gliss marks)))))
    (prog1
        ;; if we just have a note symbol, call the note function, otherwise we
        ;; have a cmn note already and want to apply marks etc. to it so
        ;; call ur-note (or chord if that's what we have)
        (apply (if note-symbol #'note 
                   (if chord
                       #'chord
                       #'ur-note))
               (append
                (list (if note-symbol
                          (eval (sc::rm-package note :cmn))
                          (if chord
                              (apply #'notes note)
                              note))
                      (when rq
                        (rq rq))
                      (when dots
                        (dots dots))
                      (when flags
                        (flags flags))
                      (cmn-bar-number bar-num)
                      ;; dynamics so: (pp (dy -.2))
                      (when tied-to
                        end-tie)
                      (when tied-from
                        (if (numberp tied-from)
                            (begin-tie (tie-direction (if (> tied-from 0)
                                                          :up
                                                          :down))
                                       (tie-curvature (abs tied-from)))
                            begin-tie))
                      (when beam
                        (if (zerop beam)
                            end-beam
                            begin-beam))
                      time
                      ;; no-beam-break
                      (first tempo)
                      (second tempo))
                (get-all-cmn-marks marks)
                (when ins-change
                  (list (new-staff-name ins-change)
                        (sc-cmn-text ins-change)))
                (cmn-tuplet-brackets brackets)
                (list (when *cmn-grace-notes-for-sc*
                        ;; (print *cmn-grace-notes-for-sc*)
                        (apply #'grace-note 
                               (reverse *cmn-grace-notes-for-sc*))
                        ;; (print 'didit)
                        ))))
      (setf *cmn-grace-notes-for-sc* nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sc-cmn-text (text &key (dy 1.0) (dx 0.0) (font "Verdana")
                              (font-size 8))
  (unless text
    (error "cmn::sc-cmn-text: text is nil!"))
  (text text (dy dy) (dx dx) (font-name font) (font-size font-size)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cmn-tuplet-brackets (brackets)
  (declare (special *cmn-open-brackets-for-sc*))
  (when brackets
    (loop for bracket in brackets with dy = -0.3 collect
          (cond ((listp bracket)      
                 ;; open bracket
                 (setf (nth (first bracket) *cmn-open-brackets-for-sc*)
                   (beat-subdivision- (subdivision (second bracket)) 
                                      ;; (bracketed :up)
                                      (when (third bracket)
                                        (dx (third bracket)))
                                      (if (fourth bracket)
                                          (dy (+ (incf dy 0.5)
                                                 (fourth bracket)))
                                        (dy (incf dy 0.5)))
                                      (when (fifth bracket)
                                        (dx0 (fifth bracket)))
                                      (when (sixth bracket)
                                        (dy0 (sixth bracket)))
                                      (if (seventh bracket)
                                          (dx1 (seventh bracket))
                                        (dx1 .2))
                                      ;; 23/1/10: in SBCL (eighth ...) is
                                      ;; causing some confusion so use nth form
                                      ;; instead 
                                      (when (nth 7 bracket)
                                        (dy1 (nth 7 bracket)))
                                      )))
                ((sc::integer<0 bracket)
                 ;; under bracket
                 (-beat-subdivision- (nth (abs bracket) 
                                          *cmn-open-brackets-for-sc*)))
                ((sc::integer>=0 bracket)
                 ;; close bracket
                 (-beat-subdivision (nth bracket *cmn-open-brackets-for-sc*)))
                (t (error "cmn::cmn-tuplet-brackets: what's this? : ~a"
                          bracket))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; If the note-list is empty, then we return a chord with an invisible c4 so
;;; that alignment is correct between the staves.
;;;
;;; Returns a list no matter whether it's a single chord or not.

(defun cmn-stemless-chord (note-list &key
                                     (chord-text "") 
                                     (text-x-offset 2.0)
                                     (text-y-offset -0.5)
                                     (font-size 10.0)
                                     (rq 4)
                                     (use-octave-signs t))
  (declare (special invisible))
  (when (numberp rq)
    (setf rq (rq rq)))
  (let ((start-octave-sign nil)
        (end-octave-sign nil))
    (when (and note-list use-octave-signs)
      (when (chord-needs-8ve-up note-list)
        ;; (octave-up (dy 1) (dx .3))
        (setf start-octave-sign (begin-octave-up (dy 1) (dx .3)) 
              end-octave-sign (end-octave-up (dy 1) (dx .3)) 
              note-list (sc::transpose-chord note-list -12 :cmn)))
      (when (chord-needs-8ve-down note-list)
        (setf start-octave-sign (begin-octave-down (dx .3))
              end-octave-sign end-octave-down
              note-list (sc::transpose-chord note-list 12 :cmn))))
    (chord 
     start-octave-sign
     (if note-list
         (engorge 
          (loop for n in note-list collect
                (note (eval n) rq (head-quarters 1) (dots 0))))
       (note (c4 invisible) rq))
     no-stem (flags 0)
     end-octave-sign
     (text chord-text 
           (dx text-x-offset)
           (y #'(lambda (mark note score &optional justifying)
                  (declare (ignore justifying score mark))
                  (+ (staff-y0 note) text-y-offset)))
           (font-size font-size)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This doesn't handle a case where 15ma would be better, just the 8ve
;;; Also assumes treble clef!
(defun chord-needs-8ve-up (note-list)
  ;; this assumes the chord is sorted from lowest to highest, which is the case
  ;; if it came from an sc-set 
  ;; remember the pitch is something like (d4 natural)
  (let ((lowest (sc::make-pitch (first (first note-list))))
        (highest (sc::make-pitch (first (first (last note-list)))))
        (low (sc::make-pitch 'g4))
        (high (sc::make-pitch 'c7))) 
    (if (and (sc::pitch> highest high)
             (sc::pitch> lowest low))
        t
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Assumes bass clef!
(defun chord-needs-8ve-down (note-list)
  ;; this assumes the chord is sorted from lowest to highest, which is the case
  ;; if it came from an sc-set 
  (let ((lowest (sc::make-pitch (first (first note-list))))
        (highest (sc::make-pitch (first (first (last note-list)))))
        (low (sc::make-pitch 'f1))
        (high (sc::make-pitch 'g3))) 
    (if (and (sc::pitch< lowest low)
             (sc::pitch< highest high))
        t
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cmn-system (staves staff-names starting-clefs other-staff-args)
  (declare (special bracket bar treble bass))
  (system bracket 
          (engorge
           (loop 
               for stave in staves 
               and staff-name in staff-names
               and clef in starting-clefs 
               and other-args in other-staff-args
               collect
                 (apply #'staff 
                        (append
                         (cons (staff-name staff-name)
                               other-args)
                         (list bar
                               (cmn-get-clef clef)
                               (engorge stave))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-x-empty-bars (x)
  (loop for i below x
      collect whole-measure-rest
      collect bar))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cmn-get-clef (clef)
  (case (sc::rm-package clef :cmn)
    (treble treble)
    (alto alto)
    (tenor tenor)
    (bass bass)
    (percussion percussion)
    (double-treble double-treble)
    (double-bass double-bass)
    (t (error "cmn::cmn-get-clef: Unrecognised clef: ~a" clef))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cmn-treble-bass-system (treble-clef bass-clef)
  (declare (special bracket bar treble bass))
  (setf treble-clef (append treble-clef (list (bar) (line-mark)))
        bass-clef (append bass-clef (list (bar) (line-mark))))
  (system bracket 
          (staff bar treble (engorge treble-clef))
          (staff bar bass (engorge bass-clef))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The quad clefs are Bill's 15ma versions.
(defun cmn-treble-bass-quad-system (treble-clef bass-clef quad-treble-clef
                                    quad-bass-clef)
  (declare (special bracket bar treble bass quad-treble quad-bass))
  (setf treble-clef (append treble-clef (list (bar) (line-mark)))
        bass-clef (append bass-clef (list (bar) (line-mark)))
        quad-treble-clef (append quad-treble-clef (list (bar) (line-mark)))
        quad-bass-clef (append quad-bass-clef (list (bar) (line-mark))))
  (system bracket 
          (staff bar quad-treble (engorge quad-treble-clef))
          (staff bar treble (engorge treble-clef))
          (staff bar bass (engorge bass-clef))
          (staff bar quad-bass (engorge quad-bass-clef))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cmn-display (system-list 
                    &key
                    (file "/tmp/cmn.eps")
                    (page-height 29.7)
                    (page-width 21.0)
                    (size 20)
                    (staff-separation 3)
                    (all-output-in-one-file t)
                    (one-line-per-page nil)
                    (page-nums t)
                    (auto-bar-nums :by-line)
                    (automatic-line-breaks nil)
                    (automatic-octave-signs nil)
                    (spacing-hook nil)
                    ;; MDE Fri Apr  6 13:31:50 2012 
                    title
                    ;; if nil will auto-number bars
                    (start-bar-numbering nil)
                    ;; separation for groups in a score line
                    (group-separation 2)
                    ;; although there's a system-separation parameter for
                    ;; cmn, this is the one that actually seems to do that
                    ;; job... 
                    (line-separation 3))
  (unless (listp system-list)
    (setf system-list (list system-list)))
  (cmn (output-file file) 
       (page-height page-height)
       (page-width page-width)
       (automatic-beat-subdivision-numbers nil)
       (full-last-line nil)
       (automatic-line-breaks automatic-line-breaks)
       (first-measure-number start-bar-numbering)
       (staff-name-font "Verdana")
       (staff-name-font-scaler .6)
       (default-font '("Verdana" "Verdana-Italic" "Verdana-Bold" 
                       "Verdana-BoldItalic"))
       (slur-curvature .35)
       (slur-thickness .05)
       (tie-curvature .25)
       (tie-thickness .06)
       (spacing-hook spacing-hook)
       (automatic-beams nil)
       (title title)
       (automatic-ties nil)
       (automatic-bars nil)
       (redundant-accidentals t)
       (automatic-naturals nil)
       (automatic-page-numbers page-nums)
       (automatic-measure-numbers auto-bar-nums)
       ;; (automatic-measure-numbers (if auto-bar-nums 5 nil))
       (all-output-in-one-file all-output-in-one-file)
       (one-line-per-page one-line-per-page)
       (always-show-staff-names nil)
       (use-abbreviated-staff-names nil)
       (size size)
       ;; (page-hook #'sc-page-number)
       (system-separation group-separation)
       (dynamics-size 1.0)
       (line-separation line-separation)
       (staff-separation staff-separation)
       (automatic-octave-signs automatic-octave-signs)
       (title-separation 3)
       ;; (ideal-stem-length 0.3)
       (always-show-staff-names t)
       (use-abbreviated-staff-names t)
       (beam-spacing .26) ;; default .26775
       (beam-width .14) ;; default 0.14175001
       (engorge
        (loop for sys in system-list collect
              (system-engorge (list sys))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF cmn.lsp
