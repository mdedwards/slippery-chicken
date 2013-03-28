;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* sc/package 
;;; NAME package
;;;
;;; File:             package.lsp
;;;
;;; Version:          1.0.1
;;;
;;; Project:          slippery chicken (algorithmic composition)
;;;
;;; Purpose:          definition of the slippery-chicken package and export of
;;;                   its symbols 
;;;
;;; Author:           Michael Edwards: m@michael-edwards.org
;;;
;;; Creation date:    5.12.00
;;;
;;; $$ Last modified: 12:16:07 Thu Mar 28 2013 GMT
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

;;; 02.12.11 SEAN: changed robodoc header (deleted underscore)

(eval-when (compile)
  (declaim (optimize (speed 3) (safety 1) (space 0) (debug 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage :slippery-chicken
  (:use :common-lisp)
  (:nicknames :sc :slimy-poultry)
  (:import-from :cl-user +slippery-chicken-version+))

(in-package :slippery-chicken)

(eval-when (compile load #+allegro-cl-lite eval)
  (export '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :slippery-chicken)

;;; The CLM package conflicts with add-mark srt statistics scale interpolate so
;;; don't export these.  If the user wants to use them outside the sc package
;;; then they must explicitly give the package e.g. sc::add-mark
(export 
 '( 
   ;; globals
   +pitch-seq-lowest-equals-prefers-low+
   +pitch-seq-lowest-equals-prefers-high+
   +slippery-chicken-standard-instrument-palette+

   ;; functions (copy/pasted from robo_functions.html) 
   all-members almost-zero amplitude-to-dynamic assoc-list-id-list

   between

   cello-chord-selection-fun check-num-sequences chord-fun-aux chord-fun1
   chord-fun2 clm-loops clm-loops-all combine-into-symbol count-elements
   create-psps-default

   db2amp decimal-places default-chord-function degree-to-note
   degrees-per-octave degrees-per-semitone degrees-to-notes
   dynamic-to-amplitude

   econs env-plus env-symmetrical equal-within-tolerance event-p

   factor fibonacci fibonacci-start-at-2 fibonacci-transition
   fibonacci-transitions flatten force-length freq-to-degree freq-to-note

   get-harmonics get-pitch-bend get-sublist-indices get-sublist-lengths
   guitar-chord-selection-fun

   hailstone hash-least-used hz2ms

   in-octave in-scale inefficient-permutations inefficiently-permutate
   invert-pitch-list is-dynamic

   lisp-assoc-listp list-to-string logarithmic-steps lp-get-mark

   make-al make-assoc-list make-change-data make-chord make-complete-set
   make-cscl make-ensemble make-event make-events make-events2 make-instrument
   make-instrument-change-map make-instrument-palette make-intervals-mapper
   make-l-for-lookup make-pitch make-pitch-seq make-player make-popcorn
   make-psp make-punctuation-events make-ral make-re make-rest make-rest-bar
   make-rhythm make-rhythms make-rsp make-rthm-chain make-rthm-seq
   make-rthm-seq-bar make-rthm-seq-from-fragments
   make-rthm-seq-from-unit-multipliers make-rthm-seq-map make-sc-map
   make-sc-set make-sclist make-set-palette make-sfp
   make-sfp-from-groups-in-wavelab-marker-file
   make-sfp-from-wavelab-marker-file make-simple-change-map
   make-slippery-chicken make-sndfile make-sndfile-ext make-tempo make-time-sig
   make-tl-set middle midi-file-high-low midi-file-one-note midi-to-degree
   midi-to-freq midi-to-note mins-secs-to-secs move-elements move-repeats
   move-to-end multi-shuffle multi-shuffle-with-perms

   nconc-sublists nearest-power-of-2 note-to-degree note-to-freq note-to-midi

   octave-freqs osc-call osc-send-list

   parse-audacity-label-file-for-loops parse-midi-file
   parse-wavelab-marker-file-for-loops partial-freqs permutate permutations
   piano-chord-fun pitch-intersection pitch-list-to-symbols pitch-member
   power-of-2 print-simple-pitch-list procession pts2cm

   random-amount random-from-list random-loop-points random-rep randomise
   read-from-file recursive-set-palette-from-ring-mod reflect-list remix-in
   remove-all remove-elements remove-more remove-octaves remove-pitches
   repeat-env replace-elements reverse-env rhythm-list ring-mod ring-mod-bass
   round-if-close rsm-count-notes

   scale-env secs-to-mins-secs semitones set-palette-from-ring-mod
   set-palette-p setf-last shuffle sort-event-list sort-pitch-list
   sort-symbol-list splice split-groups split-into-sub-groups
   split-into-sub-groups2 split-into-sub-groups3 string-chord-selection-fun
   string-replace swap-elements

   transpose-pitch-list transpose-pitch-list-to-octave

   viola-chord-selection-fun violin-chord-selection-fun

   wavelab-to-audacity-marker-file wrap-events-list wrap-list

   ;; methods (copy/pasted from robo_methods.html) 
   
   accented-p active add add add add
   add-arrow add-arrow-to-events add-bar add-clef add-clef add-empty-parcel
   add-event-to-bar add-inversions 
   add-mark-all-players add-mark-before-note add-mark-once add-mark-to-event
   add-mark-to-note add-marks-sh add-marks-to-note add-marks-to-notes
   add-pitches add-pitches-to-chord add-repeats add-repeats-simple
   add-to-list-data add-to-list-data-force add-trill add-tuplet-bracket-to-bar
   add-tuplet-brackets-to-beats add-voice all-rests?  amp2db analyse-followers
   at-start auto-accidentals auto-beam auto-beam auto-clefs auto-cue-nums
   auto-put-tuplet-bracket-on-beats auto-set-written auto-slur auto-tuplets

   beat-duration begin-slur-p

   change-bar-line-type change-pitch change-pitches change-pitches
   change-time-sig check-beams check-beams check-ties check-time-sigs
   check-tuplets check-tuplets chop chop chop chord-equal chord-member clm-play
   clone cm-get-data cmn-display cmn-display cmn-display combine combine
   combine common-notes consolidate-notes consolidate-rests
   consolidate-rests-max contains-pitches copy-bars count-notes create-chord
   create-event create-psps

   degree- delete-all-marks delete-bars delete-beam delete-beams delete-clefs
   delete-clefs delete-events delete-marks delete-marks delete-marks
   delete-marks delete-marks delete-nth-in-map delete-rehearsal-letter
   delete-sequenzes delete-slur delete-tuplets delete-written delete-written
   do-lookup do-simple-lookup double double-events duration-secs

   end-arrow end-slur-p end-trill enharmonic enharmonic enharmonic
   enharmonic-spellings enharmonics event-distance

   fill-with-rhythms find-nearest find-note find-rehearsal-letters
   find-sets-with-pitches find-sndfile fit-to-length flat-p
   force-artificial-harmonic force-artificial-harmonics force-micro-tone
   force-micro-tone force-rest force-rest force-rest-bar force-rest-bars

   gen-max-coll-file gen-midi-chord-seq gen-midi-chord-seq
   get-all-data-from-palette get-all-players get-all-refs get-all-section-refs
   get-amplitude get-bar get-bar get-bar get-bar-from-ref get-bar-num-from-ref
   get-beat-as-rhythm get-change-data get-chromatic get-clef get-clef
   get-current-instrument-for-player get-data get-data get-data-data
   get-data-from-palette get-degree get-degrees get-dynamic get-dynamics
   get-event get-events-from-to get-first get-first get-first
   get-first-for-player get-first-ref get-freqs
   get-instrument-for-player-at-bar get-interval-structure get-it get-kernel
   get-keys get-l-sequence get-last get-last get-last get-last get-last-attack
   get-last-attack get-last-bar get-last-event get-last-event get-last-ref
   get-linear-sequence get-map-refs get-midi get-midi-channel get-midi-channel
   get-multipliers get-multipliers get-next get-next get-non-chromatic get-note
   get-note get-notes get-nth get-nth-attack get-nth-attack get-nth-bar
   get-nth-event get-nth-from-map get-nth-from-palette get-nth-non-rest-rhythm
   get-nth-non-rest-rhythm get-nth-rest get-nth-rhythm get-nth-sequenz
   get-num-sections get-pitch get-pitch-symbol get-pitch-symbols
   get-pitch-symbols get-pitch-symbols get-player get-players get-position
   get-previous get-rest get-rhythm-symbols get-rhythms get-scale get-section
   get-section-refs get-semitones get-semitones-from-middle-note get-sequenz
   get-sequenz-from-bar-num get-sequenz-from-section get-snd-with-cue-num
   get-srts get-starting-ins get-steps get-tempo get-time-sig get-time-sig
   get-time-sig-as-list get-time-sig-ral get-time-sigs get-transposition-at-bar
   get-whole-bar-rest

   has-mark has-mark-before has-notes has-subsections heat highest highest

   in-range inc-duration insert-bar insert-bar intervals-mapper-degree
   intervals-mapper-note invert is-chord is-compound is-multiple
   is-single-pitch

   limit limit-for-instrument link-named-objects lowest lowest

   map-data map-over-bars max-cue max-play max-play microtonal-chords-p midi-
   midi-play move-clef move-events

   natural-p next-event no-accidental no-accidental note-add-bracket-offset
   note= num-bars num-notes num-notes num-players num-seqs num-sequenzes

   on-it osc-send-cue-nums output-midi output-midi-note

   parcel-data pitch- pitch- pitch- pitch-class-eq pitch-in-range pitch-inc
   pitch-max pitch-min pitch-round pitch-symbols pitch< pitch<= pitch= pitch>
   pitch>= player-doubles player-get-instrument players players-exist
   plays-transposing-instrument plot prefers-high prefers-low
   process-events-by-time proximity

   r-count-elements ral-econs re-bar re-bar rebar rebar recursivep
   relink-named-objects remove-dynamics remove-extraneous-dynamics
   replace-events replace-mark replace-mark replace-multi-bar-events
   replace-tempo-map reset reset reset reset reset reset-8va reset-psps
   reset-usage respell-bar respell-bars respell-chord respell-notes
   respell-notes-for-player rest-to-note rhythm-equal rhythm/ rm-marks
   rm-marks-from-note rm-marks-from-notes rm-pitches rm-pitches-from-chord
   rm-slurs round-inflections rthm-chain-gen

   sc-delete-beams sc-delete-marks sc-delete-marks-before
   sc-delete-marks-from-event sc-force-rest sc-move-dynamic sc-nthcdr
   sc-remove-dynamic sc-remove-dynamics sc-subseq sclist-econs
   sclist-remove-elements scm-get-data set-8va set-amplitudes
   set-cautionary-accidental set-characteristic set-data set-data set-dynamics
   set-map-refs set-midi-channel set-midi-channel set-midi-channel
   set-midi-channel set-midi-time-sig set-nth set-nth-attack set-nth-attack
   set-nth-bar set-nth-of-data set-position set-prefers-high set-prefers-high
   set-prefers-low set-prefers-low set-rehearsal-letter set-slot set-written
   set-written sharp-p shorten-large-fast-leaps sort-pitches split split split
   stack stack stereo subset-get-srts subtract

   tempo-equal tessitura tessitura-degree tessitura-note tie
   tie-all-last-notes-over-rests tie-over-all-rests tie-over-rest-bars
   tie-over-rests time-sig-equal time-sig-equal total-bars total-degrees
   total-duration total-notes transpose transpose transpose transpose transpose
   transpose-bars transpose-events transpose-to-octave transposing-instrument-p
   trill

   unset-cautionary-accidental update-slots

   write-lp-data-for-all))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF package.lsp
