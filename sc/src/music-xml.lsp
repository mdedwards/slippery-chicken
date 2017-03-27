;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* sc/music-xml
;;;
;;; File:             music-xml.lsp
;;;
;;; Class Hierarchy:  none, no classes defined
;;;
;;; Version:          1.0.7
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          Some helper functions for creating music-xml files for
;;;                   notation. Note that most music-xml functionality is in the
;;;                   slippery-chicken, rthm-seq-bar, event, pitch, and chord
;;;                   classes.
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    March 20th 2017, Edinburgh
;;;
;;; $$ Last modified:  18:37:41 Mon Mar 27 2017 BST
;;;
;;; SVN ID: $Id: music-xml.lsp 6147 2017-03-17 16:48:09Z medward2 $
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

;;; todo:
;;; check instrument changes are happening in slippery-when-wet
;;; check harmonic signs are being added in mieko
;;; see further inline todos below

(in-package :slippery-chicken)

;;; requirements for a decent test: start/stop/start+stop repeat signs; grace
;;; notes; microtones; lots of marks (all?);

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun xml-placement (placement)
  (if placement
      (format nil " placement=\"~a\""   ; <-- note leading space
              (case placement (a "above") (b "below")
                    (t (error "xml-placement: placement ~
                               should be nil, 'a, or 'b: ~a"
                              placement))))
      ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; placement: a="above", b="below"
(defun xml-mark-aux (tag stream mark placement &optional data)
  (unless (and mark (not (equal mark "")))
    (error "music-xml::xml-mark-aux: no mark for ~a" tag))
  (format stream "~&        <notations>")
  (format stream "~&          <~a>~
                  ~&            <~a~a~a~
                  ~&          </~a>"    
          tag mark (xml-placement placement)
          ;; if the articulation/technical needs data then we have an opening
          ;; and closing mark tag with data inbetween, otherwise it's just an
          ;; opening and closing tag
          (if data
              (format nil ">~a</~a>" data mark)
              " />")
          tag)
  (format stream "~&        </notations>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun xml-articulation (stream art placement &optional data)
  (xml-mark-aux "articulations" stream art placement data))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; e.g. t "slur" "number=\"1\" type=\"start\""
(defun xml-notation-with-args (stream tag args &optional placement)
  (format stream "~&        <notations>")
  (format stream "~&          <~a ~a~a />" ; tag, args, placement
          tag args (xml-placement placement))
  (format stream "~&        </notations>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun xml-ornament (stream orn placement &optional data)
  (xml-mark-aux "ornaments" stream orn placement data))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun xml-beg-trill (stream)
  (format stream "~&        <notations> ~
                  ~&          <ornaments> ~
                  ~&            <trill-mark default-y=\"28\" /> ~
                  ~&            <wavy-line default-y=\"28\" number=\"1\" ~
                                 type=\"start\" /> ~
                  ~&          </ornaments> ~
                  ~&        </notations>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun xml-end-trill (stream)
  (xml-ornament stream "wavy-line number=\"1\" type=\"stop\"" 'a))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun xml-notehead (stream head)
  (format stream "~&        <notehead>~a</notehead>" head))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun xml-technical (stream tech placement &optional data)
  (xml-mark-aux "technical" stream tech placement data))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun xml-direction (stream tag &optional content tag-options (placement 'a))
  (format stream "~&      <direction~a>"
          (xml-placement placement))
  (xml-direction-type stream tag content tag-options)
  (format stream "~&      </direction>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun xml-rehearsal (stream letter)
  (xml-direction stream "rehearsal" letter
                 "default-x=\"-5\" default-y=\"40\" font-weight=\"bold\""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; for strings connected to notes, although according to
;;; https://usermanuals.musicxml.com/MusicXML/Content/EL-MusicXML-direction.htm
;;; "A direction is a musical indication that is not attached to a specific
;;; note."
(defun xml-direction-type (stream tag &optional content tag-options)
  ;; (unless tag-options (setq tag-options "")) ; allow nil
  (setq tag-options (if tag-options
                        (concatenate 'string " " tag-options) ; leading space
                        ""))
  (format stream "~&        <direction-type>")
  (if content
      (format stream "~&          <~a~a>~a</~a>" tag tag-options content tag)
      (format stream "~&          <~a~a />" tag tag-options))
  (format stream "~&        </direction-type>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun xml-words (stream words &optional tag-options (placement 'a))
  (xml-direction stream "words" words tag-options placement))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun xml-dynamic (stream dyn)
  ;; (xml-mark-aux "dynamics" stream dyn nil))
  (xml-direction stream "dynamics" (format nil "<~a />" dyn) nil 'b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun xml-subito-dynamic (stream dyn)
  (format stream "~&        <direction~a>" (xml-placement 'b))
  (xml-direction-type stream "words" "subito"
                      "font-style=\"italic\" halign=\"right\"")
  (xml-direction-type stream "dynamics"
                      (string-downcase (format nil "<~a />" dyn))
                      nil)
  (format stream "~&        </direction>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Mon Mar 27 13:09:21 2017 -- text no longer needed/possible due to
;;; required tag order 
(defun xml-pause (stream &optional text)
  (xml-notation-with-args stream "fermata"
                          "default-x=\"-5\" default-y=\"10\" type=\"upright\"")
  (when text
    (xml-words stream text)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* music-xml/xml-get-mark
;;; DESCRIPTION
;;; xml-get-mark:
;;; Translation function for music-xml marks (dynamics, accents, etc.). Not
;;; generally called by the user but the list of symbols that can be used will
;;; be useful. If <silent> then non-existing marks will not produce
;;; warnings/errors (but we'll return nil). 
(defun xml-get-mark (mark stream &key (num-flags 0)
                                   ;; default to the config setting
                                   (silent (not (get-sc-config
                                                 'warn-no-xml-mark))))
  (declare (ignore num-flags))
  (flet ((no-xml-mark (mark)
           (when silent
             (warn "music-xml:xml-get-mark: Sorry but ~a is not yet available ~
                    for sc->Music-Xml; ignoring" mark))
           nil))
    (let ((xml-mark
           (when mark
             (typecase mark
               (symbol
                (case mark
;;; SYNOPSIS
                  ;; if returns a string then xml-articulation will be called,
                  ;; otherwise (dir ...) (tech ...)
                  ;; also possible: (art "accent" "below") or a list of
                  ;; strings, in which case there'll be 2+ xml-articulation
                  (a "accent")       ; accent
                  (s "staccato")
                  (as '("accent" "staccato"))
                  (at '("accent" "tenuto"))
                  (ts "detached-legato")
                  (te "tenuto")
                  ;; tech means xml-technical will be called
                  (lhp '(tech "stopped" a))
                  (bartok '(tech "snap-pizzicato" a))
                  ;; todo: could also add the <sound> tag here to change
                  ;; midi-programme (see MozartTrio.xml for e.g.)
                  ;; dir means xml-direction will be called
                  (pizz '(wds "pizz." a))
                  (ord '(wds "ord."))
                  (pizzp '(wds "(pizz.)"))
                  (clb '(wds "clb"))
                  (cl '(wds "cl"))
                  (col-legno '(wds "col legno"))
                  (clt '(wds "clt"))
                  (arco '(wds "arco"))
                  (batt '(wds "batt"))
                  (spe '(wds "spe"))
                  (sp '(wds "sul pont."))
                  (st '(wds "sul tasto"))
                  (mv '(wds "molto vib"))
                  (sv '(wds "senza vib"))
                  (poco-crini '(wds "poco crini"))
                  (nail '(tech "fingernails" a))
                  (stopped (no-xml-mark 'stopped))
                  ;; so unmeasured tremolo is implicit
                  (t3 '(orn "tremolo" nil 3))
                  (flag '(tech "harmonic" a))
                  (niente '(wds "niente"))
                  (pppp  '(dyn "pppp"))
                  (ppp  '(dyn "ppp"))
                  (pp  '(dyn "pp"))
                  (p  '(dyn "p"))
                  (mp  '(dyn "mp"))
                  (mf  '(dyn "mf"))
                  (f  '(dyn "f"))
                  (ff  '(dyn "ff"))
                  (fff  '(dyn "fff"))
                  (ffff  '(dyn "ffff"))
                  ;; todo: find out how to put dynamics in parentheses; for now
                  ;; just put in the dynamic without
                  (pppp-p '(dyn "pppp"))
                  (ppp-p '(dyn "ppp"))
                  (pp-p '(dyn "pp"))
                  (p-p '(dyn "p"))
                  (mp-p '(dyn "mp"))
                  (mf-p '(dyn "mf"))
                  (f-p '(dyn "f"))
                  (ff-p '(dyn "ff"))
                  (fff-p '(dyn "fff"))
                  (ffff-p '(dyn "ffff"))
                  (sfz '(dyn "sfz"))
                  (downbow '(tech "down-bow" a))
                  (upbow '(tech "up-bow" a))
                  (open '(tech "fingering" a 0))
                  (I '(wds "I"))
                  (II '(wds "II"))
                  (III '(wds "III"))
                  (IV '(wds "IV"))
                  ;; guitar string numbers; todo: find a way of circling them
                  (c1 "1")
                  (c2 "2")
                  (c3 "3")
                  (c4 "4")
                  (c5 "5")
                  (c6 "6")
                  ;; music xml doesn't seem to have separate slur and phrase
                  ;; marks so we'll simply use the slur tag with number=1 for
                  ;; slurs and number=2 for phrases
                  (beg-sl '(not "slur" "number=\"1\" type=\"start\""))
                  (end-sl '(not "slur" "number=\"1\" type=\"stop\""))
                  (beg-phrase '(not "slur" "number=\"2\" type=\"start\""))
                  (end-phrase '(not "slur" "number=\"2\" type=\"stop\""))
                  (beg-gliss '(not "glissando" "number=\"1\" type=\"start\" line-type=\"solid\""))
                  (end-gliss '(not "glissando" "number=\"1\" type=\"stop\" line-type=\"solid\""))
                  (beg-8va '(dir "octave-shift"
                             ;; it really is down, probably because what's
                             ;; notated is an 8ve lower than sounding
                             "dash-length=\"7.5\" size=\"8\" space-length=\"7.5\" type=\"down\""))
                  (end-8va '(dir "octave-shift" "size=\"8\" type=\"stop\""))
                  (beg-8vb '(dir "octave-shift"
                             "dash-length=\"7.5\" size=\"-8\" space-length=\"7.5\" type=\"up\""))
                  (end-8vb '(dir "octave-shift" "size=\"-8\" type=\"stop\""))
                  (beg-15ma '(dir "octave-shift"
                              "dash-length=\"7.5\" size=\"15\" space-length=\"7.5\" type=\"down\""))
                  (end-15ma '(dir "octave-shift" "size=\"15\" type=\"stop\""))
                  (beg-15mb '(dir "octave-shift"
                              "dash-length=\"7.5\" size=\"-15\" space-length=\"7.5\" type=\"up\""))
                  (end-15mb '(dir "octave-shift" "size=\"-15\" type=\"stop\""))
                  ;; note heads
                  (circled-x '(hd "circle-x"))
                  (x-head '(hd "x"))
                  (triangle '(hd "triangle"))
                  (triangle-up '(hd "inverted-triangle"))
                  (airy-head (no-xml-mark 'airy-head))
                  (none '(hd "none"))
                  ;; the On and Off versions were for lilypond; here we only
                  ;; need the On version (or preferably, just improv but for
                  ;; compatibility...) 
                  (improvOn '(hd "slash"))
                  (improvOff nil)
                  (wedge '(hd "fa"))
                  (square '(hd "la"))
                  ;; todo: look at xml doc for harmonic and sort out a way of
                  ;; representing artificial harms in xml
                  (flag-head '(hd "diamond"))
                  (flag-dots-on (no-xml-mark 'flag-dots-on))
                  (flag-dots-off (no-xml-mark 'flag-dots-off))
                  (trill-f (no-xml-mark 'trill-f))
                  (trill-n (no-xml-mark 'trill-n))
                  (trill-s (no-xml-mark 'trill-s))
                  (beg-trill-a (xml-beg-trill stream))
                  (end-trill-a (xml-end-trill stream))
                  (slash (no-xml-mark 'slash))
                  (arp '(not "arpeggiate" "default-x=\"-12\"  number=\"1\""))
                  (arrow-up (no-xml-mark 'arrow-up))
                  (arrow-down (no-xml-mark 'arrow-down))
                  (cresc-beg '(dir "wedge" nil
                               "spread=\"0\" type=\"crescendo\"" b))
                  (cresc-end '(dir "wedge" nil
                               "spread=\"15\" type=\"stop\"" b))
                  (dim-beg '(dir "wedge" nil
                             "spread=\"15\" type=\"diminuendo\"" b))
                  (dim-end '(dir "wedge" nil
                             "spread=\"0\" type=\"stop\"" b))
                  (<< (no-xml-mark '<<))
                  (>> (no-xml-mark '>>))
                  ;; we could faff around with lexical variables and manipulate
                  ;; the niente property of "wedge" but as this would be
                  ;; post-gen anyway we can do it in the notation software
                  (hairpin0 (no-xml-mark 'hairpin0))
                  (pause (xml-pause stream))
                  ;; seems there is a fermata-shape element but damned if I can
                  ;; find out where it could go and be interpreted correctly.
                  ;; MDE Mon Mar 27 13:09:50 2017 -- see xml-pause above
                  (long-pause (xml-pause stream)); "lunga"))
                  (short-pause (xml-pause stream)); "breva"))
                  (aeolian-light (no-xml-mark 'aeolianLight))
                  (aeolian-dark (no-xml-mark 'aeolianDark))
                  ;; this one uses the graphic for close bracket
                  (bracket-end (no-xml-mark 'bracketEnd))
                  (mphonic (no-xml-mark 'mphonic))
                  (mphonic-arr (no-xml-mark 'mphoniArr))
                  (mphonic-cons (no-xml-mark 'mphonicCons))
                  (mphonic-diss (no-xml-mark 'mphonicDiss))
                  (mphonic-cluster (no-xml-mark 'mphonicCluster))
                  (sing (no-xml-mark 'sing))
                  (high-sine (no-xml-mark 'high-sine))
                  (noise (no-xml-mark 'noise))
                  (focus (no-xml-mark 'focus))
                  (balance (no-xml-mark 'balance))
                  (alternate (no-xml-mark 'alternate))
                  (sing-arr (no-xml-mark 'singArr))
                  (arrow-up-down (no-xml-mark 'arrowUpDown))
                  ;; no need to fiddle with these: can do in notation software,
                  ;; no? 
                  (start-arrow (no-xml-mark 'start-arrow))
                  (end-arrow (no-xml-mark 'end-arrow))
                  (harm '(tech "harmonic" a))
                  (sost '(wds "sost."))
                  (sost-up '(wds "sost.*"))
                  (sost^ (no-xml-mark 'sost^)) ;"\\sostenutoOff\\sostenutoOn ")
                  (ped '(dir "pedal" nil "line=\"yes\" type=\"start\""))
                  (ped^ '(dir "pedal" nil "line=\"yes\" type=\"change\""))
                  (ped-up '(dir "pedal" nil "line=\"yes\" type=\"stop\""))
                  (uc '(wds "una corda" nil b))
                  (tc '(wds "tre corde" nil b))
;;; ****
                  (t (unless silent
                       (error "music-xml::xml-get-mark: unrecognised mark: ~a"
                              mark)))))
               (integer
                (progn
                  (when (or (< mark 0) (> mark 5))
                    (warning "music-xml::xml-get-mark: adding fingering ~a, ~
                              hope your musicians have more than 4 fingers ~
                              and a thumb!."
                             mark)
                    ;; todo: is there a special mark for an open string?
                    '(tech "fingering" a mark))))
               ;; 25.6.11 a 2 element list will generate a 'transition arrow'
               ;; with the first element as the starting text and the second as
               ;; end text.  The elements will be converted to lowercase
               ;; strings unless they're already strings
               (list 
                (case (first mark)
                  (clef (xml-clef (second mark) stream t))
                  (arrow (no-xml-mark 'arrow))
                  (gliss-map (no-xml-mark 'gliss-map))
                  (sub (xml-subito-dynamic stream (second mark)))
                  ;; implement maybe later...
                  (trill-note (no-xml-mark 'trill-note))
                  (text (no-xml-mark 'text))
                  (staff (no-xml-mark 'staff))
                  (rgb (no-xml-mark 'rgb))
                  ;; todo: implement key sigs!
                  (key (no-xml-mark 'key))
                  ;; (get-xml-key-sig (second mark) (third mark)))
                  (t (unless silent
                       (error "music-xml::xml-get-mark: unrecognised mark as ~
                               list: ~a" mark)))))
               ;; 27.5.11: use expicit \markup command instead of ^ and here's
               ;; a quick hack: put all strings up or down according to whether
               ;; there's a ^ or _ as first char, or if neither, it's up (^)
               (string (let* ((char1 (elt mark 0))
                              (up (char= char1 #\^))
                              (down (char= char1 #\_))
                              (mk (if (or up down)
                                      (subseq mark 1)
                                      mark)))
                         (xml-words stream mk nil (if down 'b 'a))))
               ;; if it's a list then it's a bunch of arguments to sc-cmn-text
               ;; otherwise it might be a mark (e.g. text) already
               ;; ignore cmn stuff but warn
               (t (unless silent
                    (warn "music-xml::get-xml-mark: unknown mark: ~a"
                          mark)))))))
      (when xml-mark
        (if (listp xml-mark)
            (if (stringp (first xml-mark))
                (loop for s in xml-mark do
                     (xml-articulation stream s nil))
                (apply 
                 (case (first xml-mark)
                   (art #'xml-articulation)
                   (orn #'xml-ornament)
                   (tech #'xml-technical)
                   (not #'xml-notation-with-args)
                   (hd #'xml-notehead)
                   (dyn #'xml-dynamic)
                   (dir #'xml-direction)
                   (wds #'xml-words))
                 (cons stream (rest xml-mark))))
            (xml-articulation stream xml-mark nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun xml-simple-rhythm (num)
  (let ((pos (log num 2)))
    (unless (float-int-p pos)
      (error "rhythm::xml-simple-rhythm: argument must be a power of 2 ~
              between 1 and 128: ~a" num))
    (nth (floor pos) '("whole" "half" "quarter" "eighth" "16th" "32nd" "64th"
                       "128th"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun xml-barline (type stream &optional (location "right")
                                  (repeat-direction "backward"))
  (unless (zerop type) ; <barline> not needed in regular bars
    (format stream "~&        <barline location=\"~a\">~
                    ~&          <bar-style>~a</bar-style>"
            location
            (case type
              (1 "light-light")
              (2 "light-heavy")
              (3 "heavy-light")         ; begin repeat
              (4 "heavy-light")         ; begin & end repeat
              (5 "light-heavy")         ; end repeat
              (t (error "rthm-seq-bar::xml-barline: unhandled barline: ~a"
                        type))))
    (when (member type '(3 4 5))
      (format stream "~&          <repeat direction=\"~a\" ~
                                     winged=\"none\" />"
              repeat-direction))
    (format stream "~&        </barline>")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun xml-clef (clef stream &optional write-attributes)
  (when write-attributes
      (format stream "~&      <attributes>"))
  (format stream "~&        <clef><sign>~a</sign>"
          (case clef (treble 'g) (bass 'f) (tenor 'c) (alto 'c) (treble-8vb 'g)
                (double-treble 'g) (double-bass 'f) (percussion "percussion")))
  (unless (eq clef 'percussion)
    (format stream "~&          <line>~a</line>"
            (case clef (treble 2) (bass 4) (tenor 4) (alto 3) (treble-8vb 2)
                  (double-treble 2) (double-bass 4))))
  (when (member clef '(double-treble double-bass treble-8vb))
    (format stream "~&          <clef-octave-change>~a</clef-octave-change>"
            (case clef (double-treble 8) (double-bass -1) (treble-8vb -1))))
  (format stream "~&        </clef>")
  (when write-attributes
    (format stream "~&      </attributes>")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; https://usermanuals.musicxml.com/MusicXML/Content/EL-MusicXML-scaling.htm
;;; The tenths type is a number representing tenths of interline staff space
;;; (positive or negative). Both integer and decimal values are allowed, such
;;; as 5 for a half space and 2.5 for a quarter space. Interline space is
;;; measured from the middle of a staff line. Distances in a MusicXML file are
;;; measured in tenths of staff space. Tenths are then scaled to millimeters
;;; within the scaling element, used in the defaults element at the start of a
;;; score. Individual staves can apply a scaling factor to adjust staff size.
(defun xml-mm2tenths (mm &optional (staff-height 7) (tenths 40))
  (floor (* tenths (/ mm staff-height))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MDE Sat Mar 18 13:41:17 2017
(defun xml-flat-sign (name) ; string
  ;; look for the hyphen version first. we assume we're inside <display-text>
  ;; so close first then reopen after 
  (let* ((xml (format nil "</display-text><accidental-text>flat~
                           </accidental-text><display-text>"))
         (handle-flat (string-replace "-flat" xml name)))
    ;; maybe we've got flat?
    (unless handle-flat
      (setf handle-flat (string-replace "flat" xml name)))
    (if handle-flat handle-flat name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun xml-tuplet (actual normal bracket-number stream
                   ;;        this should be the tuplet-type e.g. "eighth" or nil
                   &optional write-actual-normal)
  (let* ((show-number (if (member (list actual normal)
                                  '((5 4) (3 2) (6 4))
                                  :test #'equal)
                          "actual" "both")))
    (if actual                          ; if arg1=nil then it's a stop tuplet
        (progn
          (format stream "~&          <tuplet type=\"start\" bracket=\"yes\" ~
                                       number=\"~a\" show-number=\"~a\"~a"
                  bracket-number show-number
                  (if write-actual-normal ">" " />"))
          (when write-actual-normal
            (format stream "~&          <tuplet-actual>~
                            ~&            <tuplet-number>~a</tuplet-number>~
                            ~&            <tuplet-type>~a</tuplet-type>~
                            ~&          </tuplet-actual>~
                            ~&          <tuplet-normal>~
                            ~&            <tuplet-number>~a</tuplet-number>~
                            ~&            <tuplet-type>~a</tuplet-type>~
                            ~&          </tuplet-normal>~
                            ~&          </tuplet>"
                    actual write-actual-normal normal write-actual-normal)))
        (format stream "~&          <tuplet type=\"stop\" number=\"~a\" />"
                bracket-number))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun xml-write-marks (list stream)
  (loop for m in list do (xml-get-mark m stream)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF music-xml.lsp
