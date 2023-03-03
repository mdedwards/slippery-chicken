;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FILE:     mini-synth.orc
;;
;; PURPOSE:  A minimal Csound-orchestra for testing purposes.
;;
;; AUTHOR:   Ruben Philipp <ruben.philipp@folkwang-uni.de>
;; CREATED:  2023-03-02
;; $$ Last modified:  21:45:22 Thu Mar  2 2023 CET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

sr = 44100
ksmps = 32
nchnls = 2
0dbfs = 1.0


;; ftables
giSine ftgen 0, 0, 8192, 10, 1
giSquare ftgen 0, 0, 8192, 10, 1, 0, 0.3, 0, 0.2, 0, 0.14, 0, 0.111


instr 1
  
  istart = p2
  idur = p3
  ifreq = p4
  iamp = p5

  asin poscil3 iamp, ifreq, giSine

  ;; MIXING

  ares = linseg:k(0.0, .05, 1.0, idur-.1, 1.0, .05, 0.0)*asin*0.12

  ;; OUTS

  aoutl, aoutr pan2 ares, .75
  
  outs aoutl, aoutr


endin

instr 2

  istart = p2
  idur = p3
  ifreq = p4
  iamp = p5

  asin poscil3 iamp, ifreq, giSquare

  ;; MIXING

  ares = linseg:k(0.0, .05, 1.0, idur-.1, 1.0, .05, 0.0)*asin*0.12

  ;; OUTS

  aoutl, aoutr pan2 ares, .35
  
  outs aoutl, aoutr

endin