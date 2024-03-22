# slippery chicken

This is slippery-chicken, a Common Lisp and CLOS package for
algorithmic composition.

slippery chicken outputs sound files, [reaper](https://reaper.fm) files, MIDI files, and score 
files in PDF, Lilypond, and MusicXML formats. It also outputs
scores in [antescofo~](https://antescofo-doc.ircam.fr) format for use with Arshia Cont's score
following external for Max/MSP.

The source code is available here in github. For more details 
about installation see the [install page](install.md).

Extensive documentation can be found at https://www.michael-edwards.org/sc

Since March 2024 slippery-chicken can be installed via Common Lisp's ASDF mechanism. See the [install page](install.md)
for details. For the old method of installing via script or from source, 
see this [wiki page](https://github.com/mdedwards/slippery-chicken/wiki/how-to-install-slippery-chicken-'by-hand')

## hello (slippery) world

<img width="1000" alt="sc-noodles" src="doc/media/sc-noodles.png">

Here's a small example of how to work with slippery chicken. It's a demo rather than a fully-nuanced piece of music. 
It keeps code length short but illustrates some of the power of the algorithmic approach. Think of slippery chicken
as a big (non-artificially) intelligent sequencer or music compiler, but bear in mind that the `make-slippery-chicken` approach
is not the only way of cooking something up (e.g. see [this wiki page](https://github.com/mdedwards/slippery-chicken/wiki/How-can-I-'roll-my-own'-slippery-chicken%3F)
for another approach)

```
(in-package :sc)

(let* ((num-seqs 55) ; change this number for a longer/shorter piece
       ;; we will have only 4 rhythm sequences (similar to phrases)
       ;; use the procession algorithm to move between these
       (proc (procession num-seqs '(1 2 3 4)))
       (hello 
         (make-slippery-chicken
          '+hello-slippery+ 
          :composer "ME"
          :title "hello slippery chicken noodles"
          :year "2024"
          :title "16ths-a-go-go"
          :tempo-map '((1 (q 144 "fast")))
          ;; just three instruments on different MIDI channels. Symbols such as
          ;; vln name the players who will play the instruments (they could play
          ;; more than one)
          :ensemble '(((vln (violin :midi-channel 1))
                       (perc (vibraphone :midi-channel 4))
                       (bsn (bassoon :midi-channel 9))))
          ;; our rhythm sequences (aka phrases). We reference these in the
          ;; rthm-seq-map below to sequence the piece. Rhythms are notated as
          ;; simple symbols like s for 16th, e for eighth, q. for dotted quarter
          ;; etc. Rhythms can be repeated with e.g. s x 4 (four 16ths). Melodic
          ;; curves are indicated by the :pitch-seq-palette. See 
          ;; https://michael-edwards.org/sc/manual/pitches.html#curves
          :rthm-seq-palette '((1 ((((5 8) - s x 4 -  - s e s - (e)))
                                  :pitch-seq-palette ((1 2 5 4 1 3 6)
                                                      (5 3 2 6 4 1 2))))
                              (2 ((((5 8) - s s s - (s)  (e) - s x 4 -))
                                  :pitch-seq-palette ((1 3 2 6 4 2 3))))
                              (3 ((((5 8) (e) - e s s - - s e s -))
                                  :pitch-seq-palette ((1 4 2 3 6 3))))
                              (4 ((((5 8) - s e s -  e (e) - s s -))
                                  :pitch-seq-palette ((4 1 2 5 2 3)))))
          ;; our 'harmonic' material. We'll also use 4 chords or sets. Pitches
          ;; are notated as symbols using note names from c to b, the octave
          ;; number (where 4 is the middle octave), and modifiers s and f for
          ;; sharp and flat. E.g. cs4 is middle c#
          :set-palette '((1 ((cs2 e2 d3 bf3 fs4 g4 gs4 a5)))
                         (2 ((c2 f2 ef3 b3 fs4 gs4 as4 g5)))
                         (3 ((e2 bf2 ef3 c3 bf3 fs4 cs5 g5 b5)))
                         (4 ((d2 b2 g3 f4 df5 af5 ef6))))
          ;; we can shape the range or tessitura of our instruments using
          ;; breakpoint curves. Here all three instruments will start off low
          ;; but move to a high point 75% of the way through before moving back
          ;; to low again by the end.
          ;; https://michael-edwards.org/sc/manual/nouveau-reich.html#interpl
          :set-limits-high '((vln (0 c5 75 c7 100 c5))
                             (perc (0 a4 75 f6 100 a4))
                             (bsn (0 g3 75 a4 100 g3)))
          :set-limits-low '((vln (0 c4 75 g5 100 c4))
                            (perc (0 f3 75 g4 100 f3))
                            (bsn (0 bf1 75 g3 100 bf1)))
          ;; here we just splice our procession list of IDs into both 'maps' to
          ;; determine the order of the harmonic sets and the rhythm sequences.
          :set-map `((1 ,proc))
          :rthm-seq-map `((1 ((vln ,proc)
                              ;; wrap the lists around on themselves to get
                              ;; instant counterpoint
                              (perc ,(wrap-list proc 4))
                              (bsn ,(wrap-list proc 7))))))))
  ;; now the piece is in memory and ready to 'render' but let's do a little
  ;; post-generation editing first. Fast repeated notes are a cinch on the vibes
  ;; and violin but much harder on the bassoon so let's change repeated notes in
  ;; that part to neighbouring tones in the current set.
  (rm-repeated-pitches hello 'bsn)
  ;; marks can be part of rthm-sequence-palettes or added post-generation. They
  ;; can be symbols like 'a' for accent or any arbitrary text, as here
  (add-mark-to-note hello 1 1 'vln "hello")
  (add-mark-to-note hello 1 4 'vln "slippery")
  (add-mark-to-note hello 2 1 'vln "chicken")
  ;; render a score via lilypond or if your system can't handle calls to the
  ;; shell, just write the lilypond formatted text files and render in lilypond
  ;; separately (for that use write-lp-data-for-all)
  (lp-display hello)
  ;; write a music-xml file for import into Doric, Musescore, Sibelius etc.
  (write-xml hello)
  ;; write a midi file. All of these files can be specified with a path or, as
  ;; here, written to the default directory (usually /tmp but user-settable via
  ;; e.g. (set-sc-config 'default-dir "~/music/")
  (midi-play hello))
```
Via Lilypond this produces a [lovely score](doc/media/_hello-slippery-chicken-noodles-score.pdf).
We could listen to the piece with instrument approximations using Dorico or something but I quite
like the MIDI file render made with [pianoteq 7's](https://www.modartt.com/pianoteq_overview)
elektric piano *MKII Flower Power*: [listen](https://github.com/mdedwards/slippery-chicken/raw/quicklisp/doc/media/sc-noodles.mp3)

Happy lisping, 
  Michael Edwards (m@michael-edwards.org)
