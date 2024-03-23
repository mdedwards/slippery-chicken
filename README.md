# slippery chicken

This is slippery-chicken, a Common Lisp and CLOS package for algorithmic
composition.

slippery chicken outputs sound files, [reaper](https://reaper.fm) files, MIDI
files, and score files in PDF, Lilypond, and MusicXML formats. It also outputs
scores in [antescofo~](https://antescofo-doc.ircam.fr) format for use with
Arshia Cont's score following external for Max/MSP.

The source code is available here in github. For more details about installation
see the [install page](install.md).

Extensive documentation can be found at https://www.michael-edwards.org/sc You
may also be interested in [a
paper](https://michael-edwards.org/sc/media/sc-paper-long.pdf) I wrote some
years ago.

Since March 2024 slippery-chicken can be installed via Common Lisp's ASDF
mechanism. See the [install page](install.md) for details. For the old method of
installing via script or from source, see this [wiki
page](https://github.com/mdedwards/slippery-chicken/wiki/how-to-install-slippery-chicken-'by-hand')

## hello (slippery) world

<img width="1000" alt="sc-noodles" src="doc/media/sc-noodles.png">

Here's a small example of how to work with slippery chicken. It's a demo rather
than a fully-nuanced piece of music and shouldn't be seen as indicative of
slippery-chicken's aesthetic (which doesn't exist).  It keeps code length short
(at least if you take out the extensive comments) but illustrates some of the
power of the algorithmic approach. Think of slippery chicken as a big
(non-artificially) intelligent sequencer or music compiler, but bear in mind
that the `make-slippery-chicken` approach is not the only way of cooking
something up (e.g. see [this wiki
page](https://github.com/mdedwards/slippery-chicken/wiki/How-can-I-'roll-my-own'-slippery-chicken%3F)
for another approach).

> [!WARNING]
> Copy/pasting from a webpage into a Lisp interpreter often causes problems
> because the characters are not all standard ANSI. The code below can be found
> in [this repo](doc/examples/hello-slippery.lsp).

```lisp
(in-package :sc)

(let* ((num-seqs 55)             ; change this number for a longer/shorter piece
       ;; we will have only 4 rhythm sequences (similar to phrases: see below)
       ;; use slippery chicken's procession algorithm to move between these
       (proc (procession num-seqs '(1 2 3 4)))
       (hello 
         (make-slippery-chicken
          '+hello-slippery+ 
          :composer "ME"
          :title "hello slippery chicken noodles"
          :year "2024"
          :tempo-map '((1 (q 144 "fast")))
          ;; just three instruments on different MIDI channels. You can have as
          ;; many instruments/players as you like--there are no known limits
          ;; (famous last words). Symbols such as 'vln' name the players who
          ;; will play the instruments (they could play more than one)
          :ensemble '(((vln (violin :midi-channel 1))
                       (perc (vibraphone :midi-channel 4))
                       (bsn (bassoon :midi-channel 9))))
          ;; our rhythm sequences (aka phrases). We reference these in the
          ;; rthm-seq-map below to sequence the piece. Rhythms are notated as
          ;; simple symbols like s for 16th, e for eighth, q. for dotted quarter
          ;; etc. Nested tuplets (complex rhythms) and RQQ notation also
          ;; possible. Rhythms can be repeated with e.g. s x 4 (four
          ;; 16ths). Melodic curves are indicated by the :pitch-seq-palette. See
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
          ;; our 'harmonic' material. We'll also use 4 chords or sets so that we
          ;; can use the procession algorithm to sequence these also. Pitches
          ;; are notated as symbols using note names from c to b, the octave
          ;; number (where 4 is the middle octave), and modifiers s and f for
          ;; sharp and flat (microtones also possible). E.g. cs4 is middle c#,
          ;; dqf2 is the lowest d quarter-flat on the cello.
          :set-palette '((1 ((d2 f2 ef3 b3 g4 af4 a4 bf5)))
                         (2 ((b1 e2 d3 bf3 f4 g4 a4 fs5)))
                         (3 ((ef2 a2 d3 b2 a3 f4 c5 fs5 bf5)))
                         (4 ((cs2 bf2 fs3 e4 c5 g5 d6))))
          ;; we can shape the range or tessitura of our instruments using
          ;; breakpoint curves. Here all three instruments will start off low
          ;; but move to a high point 75% of the way through before moving back
          ;; to low again by the end. Instruments will only be assigned pitches
          ;; from the sets that are in their range, but they will also be
          ;; limited by set-limits.
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
                              ;; 'instant counterpoint'
                              (perc ,(wrap-list proc 4))
                              (bsn ,(wrap-list proc 7))))))))
  ;; now the piece is in memory and ready to render but let's do a little
  ;; post-generation editing first. Fast repeated notes are a cinch on the vibes
  ;; and violin but much harder on the bassoon so let's change repeated notes in
  ;; that part to neighbouring tones in the current set.
  (rm-repeated-pitches hello 'bsn)
  ;; marks can be part of rthm-sequence-palettes or added post-generation. They
  ;; can be symbols like 'a' for accent or any arbitrary text, as here.
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
  ;; here, written to the default directory (by default /tmp but user-settable
  ;; via e.g. (set-sc-config 'default-dir "~/music/")
  (midi-play hello))
```

Via Lilypond [(or CMN)](https://ccrma.stanford.edu/software/cmn/) we can
directly produce a
[score](doc/media/_hello-slippery-chicken-noodles-score.pdf). We could listen
to the piece with instrument approximations using Dorico or something but I
quite like the funky MIDI file render made with [pianoteq
7's](https://www.modartt.com/pianoteq_overview) elektric piano *MKII Flower
Power*:
[listen](https://github.com/mdedwards/slippery-chicken/raw/quicklisp/doc/media/sc-noodles.mp3)

Happy lisping,   
  Michael Edwards (m@michael-edwards.org)

## related Common Lisp packages
[Common Lisp Music](https://ccrma.stanford.edu/software/clm/)  
[Common Music Notation](https://ccrma.stanford.edu/software/cmn/)  
[Common Music](https://github.com/ormf/cm)