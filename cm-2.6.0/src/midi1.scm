;;; **********************************************************************
;;; Copyright (C) 2002 Heinrich Taube (taube@uiuc.edu) 
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; **********************************************************************

;;; $Name: rel-2_6_0 $
;;; $Revision: 1.17 $
;;; $Date: 2005/01/01 18:52:27 $

;;;
;;; MIDI MESSAGE TYPES
;;; 
;;; MIDI messages types are determined by their status byte. A status byte
;;; in a stream of messages is defined as a byte whose MSB is set.  
;;;
;;; NOTE: The following bitfields comply with the MIDI standard.
;;;       The actual constants, however, have their nibbles swapped.)
;;;
;;; NOTE: Some message names differ from the official specification as
;;;       published by the MMA (cf. http://www.midi.org/table1.htm).  In
;;;       particular, the following message types should read (=>):
;;;
;;;         Key Pressure	=> Polyphonic Key Pressure
;;;         Pitch Bend		=> Pitch Wheel Change
;;;         Sysex		=> System Exclusive
;;;         Song Position	=> Song Position Pointer
;;;         Eox (unused)	=> End of Exclusive
;;;         System Reset	=> Reset
;;;
;;;
;;;                          status-byte     data1-byte   data2-byte
;;;    -------------------------------------------------------------------
;;;
;;;    [channel messages]
;;;      note-off            1000 ----       key           velocity
;;;      note-on             1001 ----       key           velocity
;;;      key-pressure        1010 ----       key           pressure
;;;      control-change      1011 ----       controller    value
;;;      program-change      1100 ----       program       -
;;;      channel-pressure    1101 ----       pressure      -
;;;      pitch-bend          1110 ----       lsb           msb


;;; 
;;; Byte specifications and offsets (Encoded MIDI Messages)

(define +enc-route-byte+		(byte 10 22))
(define +enc-lower-status-byte+	(byte  4 18))
(define +enc-upper-status-byte+	(byte  4 14))
(define +enc-swapped-status-byte+ 	(byte  8 14))
(define +enc-logical-channel-byte+	(byte 14 18))
(define +enc-opcode-byte+		+enc-upper-status-byte+)
(define +enc-data-1-byte+		(byte  7  7))
(define +enc-data-2-byte+		(byte  7  0))

(define +enc-route-offs+		-22)
(define +enc-lower-status-offs+    -18)
(define +enc-upper-status-offs+    -10) ; keep byte-aligned
(define +enc-swapped-status-offs+ 	-14) ; not byte-aligned
(define +enc-logical-channel-offs+ +enc-lower-status-offs+)
(define +enc-opcode-offs+          +enc-upper-status-offs+)
(define +enc-data-1-offs+          -7)
(define +enc-data-2-offs+          0)

;;; AND with this mask to turn an encoded note on message into a note off
;;; message with zero velocity. 

(define +enc-note-off-mask+         #xfffe3f80)
(define +ml-note-off-opcode+                       #b1000)
(define +ml-note-on-opcode+                        #b1001)
(define +ml-key-pressure-opcode+                   #b1010)
(define +ml-control-change-opcode+                 #b1011)
(define +ml-program-change-opcode+                 #b1100)
(define +ml-channel-pressure-opcode+               #b1101)
(define +ml-pitch-bend-opcode+                     #b1110)

(define +ml-default-note-on-velocity+  64)
(define +ml-default-note-off-velocity+ 64)

;;;    [channel mode messages]
;;;      (these are actually just control changes with special mode
;;;      selecting controller numbers)

;;;
;;;    [system common messages]
;;;      sysex               1111 0000       <variable length>
;;;      mtc-quarter-frame   1111 0001       SMPTE bits    -    (*)
;;;      song-position       1111 0010       lsb           msb  (**)
;;;      song-select         1111 0011       song number   -
;;;      <undefined>         1111 0100       -             -
;;;      cable-select        1111 0101       -             -    (***)
;;;      tune-request        1111 0110       -             -
;;;      eox                 1111 0111       -             -

(define +ml-msg-sysex-type+                        #b00001111)
(define +ml-msg-mtc-quarter-frame-type+            #b00011111)
(define +ml-msg-song-position-type+                #b00101111)
(define +ml-msg-song-select-type+                  #b00111111)
(define +ml-msg-cable-select-type+                 #b01011111)
(define +ml-msg-tune-request-type+                 #b01101111)
(define +ml-msg-eox-type+                          #b01111111)

;;; (*):   transmits the full SMPTE code in 8 messages (= 2 frames)
;;; (**):  this is a 14-bit counter where 1 count = 6 timing clocks.
;;; (***): though officially undefined, some MIDI interfaces use this
;;;        message to control cable access; a single data byte that follows
;;;        designates the cable number on which subsequent MIDI messages are
;;;        routed. 
;;; 
;;;    [system realtime messages]
;;;      timing-clock        1111 1000       (24 ticks per quarter)
;;;      timing-tick         1111 1001       (1 tick = 10 milliseconds)
;;;      start               1111 1010 
;;;      continue            1111 1011                 
;;;      stop                1111 1100              
;;;      <undefined>         1111 1101                  
;;;      active sensing      1111 1110       (sent every 300ms or more often)
;;;      system reset        1111 1111        

(define +ml-msg-timing-clock-type+                 #b10001111)
(define +ml-msg-timing-tick-type+                  #b10011111)
(define +ml-msg-start-type+                        #b10101111)
(define +ml-msg-continue-type+                     #b10111111)
(define +ml-msg-stop-type+                         #b11001111)
(define +ml-msg-active-sensing-type+               #b11101111)
(define +ml-msg-system-reset-type+                 #b11111111)

;;;
;;; NOTE: Meta messages are of variable length and have the syntax 
;;;
;;;          #xFF <type> <length> {<data>}*
;;;       
;;;       However, since #xFF collides with the system reset realtime
;;;       message, we substitute a #x00 status byte to flag a meta message
;;;       and treat them like sysex otherwise (i.e., attach the FULL meta
;;;       message as a string, including the initial #xff byte.
;;; 
;;;       MIDI file types are NOT swapped, since they don't travel through
;;;       the C interface.
;;;       
;;;   [meta messages]
;;;     eot                  0000 0000       0010 1111     <length>    ...
;;;     tempo-change         0000 0000       0101 0001     <length>    ...
;;;     time-signature       0000 0000       0101 1000     <length>    ...

(define +ml-meta-type+                             #b00000000)
(define +ml-file-meta-marker+                      #xff)

(define +ml-file-sequence-number-opcode+           #x00)

(define +ml-file-text-event-opcode+                #x01)
(define +ml-file-copyright-note-opcode+            #x02)
(define +ml-file-sequence/track-name-opcode+       #x03)
(define +ml-file-instrument-name-opcode+           #x04)
(define +ml-file-lyric-opcode+                     #x05)
(define +ml-file-marker-opcode+                    #x06)
(define +ml-file-cue-point-opcode+                 #x07)

(define +ml-file-midi-channel-opcode+              #x20)
(define +ml-file-midi-port-opcode+                 #x21)
(define +ml-file-eot-opcode+                       #x2f)

(define +ml-file-tempo-change-opcode+              #x51)
(define +ml-file-smpte-offset-opcode+              #x54)
(define +ml-file-time-signature-opcode+            #x58)
(define +ml-file-key-signature-opcode+             #x59)
(define +ml-file-sequencer-event-opcode+           #x7f)

;;;
;;; basic accessors and type predicates
;;;

(define (midimsg-data1 message)
  (ldb +enc-data-1-byte+ message))

(define (midimsg-data2 message)
  (ldb +enc-data-2-byte+ message))

(define (midi-channel-message-p message)
  (< 0 (ldb +enc-opcode-byte+ message) #xf))

(define (midi-system-message-p message)
  (= (ldb +enc-upper-status-byte+ message) #xf))

(define (midi-meta-message-p message)
  (= (ldb +enc-swapped-status-byte+ message) +ml-meta-type+))

;;;
;;; Provide a quick way of looking up the number of bytes (including the
;;; status byte) of a message.
;;;
;;;          -1 = <undefined>
;;;           0 = variable length
;;;   1,2, or 3 = length

(define +channel-message-sizes+
  #(3                                   ; note-off
    3                                   ; note-on
    3                                   ; key-pressure
    3                                   ; control-change
    2                                   ; program-change
    2                                   ; channel-pressure
    3                                   ; pitch-bend
    ))

(define +system-message-sizes+ 
  #(0                                   ; sysex
    2                                   ; mtc-quarter-frame
    3                                   ; song-position
    2                                   ; song-select
    -1                                  ; <undefined>
    2                                   ; cable-select
    1                                   ; tune-request
    1                                   ; eox
    1                                   ; timing-clock
    1                                   ; timing-tick
    1                                   ; start
    1                                   ; continue
    1                                   ; stop
    -1                                  ; <undefined>
    1                                   ; active-sensing
    1                                   ; system-reset
    ))

(define +ml-channel-msg-type-strings+
  #("Note-Off" "Note-On" "Key-Pressure" "Control-Change" "Program-Change"
    "Channel-Pressure" "Pitch-Bend"))

(define +ml-msg-type-strings+
  #("Sysex" "MTC Quarter Frame" "Song-Position" "Song-Select"  "Undefined"
    "Cable-Select" "Tune-Request" "Eox" "Timing-Clock" "Timing-Tick"
    "Start" "Continue" "Stop" "Undefined" "Active-Sensing" "System-Reset"))

(define +ml-meta-msg-type-strings+
  #((0 . "Sequence Number") (1 . "Text Event") (2 . "Copyright Note")
    (3 . "Sequence/Track Name") (4 . "Instrument Name") (5 . "Lyric")
    (6 . "Marker") (7 . "Cue Point") (#x20 . "MIDI Channel")
    (#x21 . "MIDI Port") (#x2f . "End of Track") (#x51 . "Tempo Change")
    (#x54 . "SMPTE Offset") (#x58 . "Time Signature")
    (#x59 . "Key Signature") (#x7f . "Sequencer Event")))

(define (get-meta-msg-type-string type)
  (let ((res (find type +ml-meta-msg-type-strings+
                   :test (function =) :key (function car))))
  (if res (cdr res) "Unknown Meta Event")))

;;;
;;; low-level utilities used by mf.lisp and midi.lisp
;;;

(define (midimsg-logical-channel m)
  (ldb +enc-logical-channel-byte+ m))

(define (midimsg-route m)
  (ldb +enc-route-byte+ m))

(define (midimsg-opcode m)
  (ldb +enc-opcode-byte+ m))

(define (midimsg-upper-status m)    ; private
  (ldb +enc-upper-status-byte+ m))

(define (midimsg-lower-status m)    ; private
  (ldb +enc-lower-status-byte+ m))

(define (midimsg-status m)
  (ldb +enc-swapped-status-byte+ m))

(define (midimsg-size m)            ; private
  (if (midi-channel-message-p m) 
    (vector-ref +channel-message-sizes+
                (logand (ash m +enc-swapped-status-offs+) #b111))
    (vector-ref +system-message-sizes+
                (logand (ash m +enc-lower-status-offs+) #b1111))))

(define (channel-note-hash m) 
   ;; hash value chan&key 
   (logior (ash (ldb +enc-logical-channel-byte+ m) 8)
           (midimsg-data1 m)))

(define (%midi-encode-channel-message bytes size)
  (if (= size 3)
    (make-channel-message (ash (logand bytes #xf00000) -20)
                          (ash (logand bytes #x0f0000) -16)    
                          (ash (logand bytes #x007f00) -8)
                          (logand bytes #x7f))
    (if (= size 2)
      (make-channel-message (ash (logand bytes #xf000) -12)
                            (ash (logand bytes #x0f00) -8)       
                            (logand bytes #x7f))
      (err "Size ~s cannot be a channel message." size))))

(define *midi-open* #f)
(define *midi-time* -1)

(defmacro define-message-set! (accessor bytespec)
  ;; defined in level1 because bqoute translation hopeless!
  (make-midi-message-set! accessor bytespec))


;;; ==========================================================================
;;;
;;; Channel (and Channel Mode) Messages
;;;

(define (make-channel-message opcode channel data1 . args)
  (let ((data2 (if (null? args) 0 (car args))))
    (dpb channel +enc-logical-channel-byte+
         (dpb opcode +enc-opcode-byte+
              (dpb data1 +enc-data-1-byte+
                   (dpb data2 +enc-data-2-byte+ 0))))))

(define (channel-message-p message)
  (midi-channel-message-p message))

(define (channel-message-channel message)
  (ldb +enc-logical-channel-byte+ message))

(define (channel-message-opcode message)
  (ldb +enc-opcode-byte+ message))

(define (channel-message-data1 message)
  (ldb +enc-data-1-byte+ message))

(define (channel-message-data2 message)
  (ldb +enc-data-2-byte+ message))

(define-message-set! channel-message-channel
    +enc-logical-channel-byte+)
(define-message-set! channel-message-opcode  +enc-opcode-byte+)
(define-message-set! channel-message-data1   +enc-data-1-byte+)
(define-message-set! channel-message-data2   +enc-data-2-byte+)


;;;
;;; :note-off

(define (make-note-off channel key velocity)
  (make-channel-message +ml-note-off-opcode+ channel key velocity))

(define (note-off-p message)
  (= (ldb +enc-opcode-byte+ message) +ml-note-off-opcode+))

(define (note-off-channel message)
  (ldb +enc-logical-channel-byte+ message))

(define (note-off-key message)
  (ldb +enc-data-1-byte+ message))

(define (note-off-velocity message)
  (ldb +enc-data-2-byte+ message))

(define-message-set! note-off-channel  +enc-logical-channel-byte+)
(define-message-set! note-off-key      +enc-data-1-byte+)
(define-message-set! note-off-velocity +enc-data-2-byte+)


;;;
;;; :note-on

(define (make-note-on channel key velocity)
  (make-channel-message +ml-note-on-opcode+ channel key velocity))

(define (note-on-p message)
  (= (ldb +enc-opcode-byte+ message) +ml-note-on-opcode+))

(define (note-on-channel message)
  (ldb +enc-logical-channel-byte+ message))

(define (note-on-key message)
  (ldb +enc-data-1-byte+ message))

(define (note-on-velocity message)
  (ldb +enc-data-2-byte+ message))

(define-message-set! note-on-channel  +enc-logical-channel-byte+)
(define-message-set! note-on-key      +enc-data-1-byte+)
(define-message-set! note-on-velocity +enc-data-2-byte+)


;;;
;;; :key-pressure

(define (make-key-pressure channel key pressure)
  (make-channel-message +ml-key-pressure-opcode+ channel key pressure))

(define (key-pressure-p message)
  (= (ldb +enc-opcode-byte+ message) +ml-key-pressure-opcode+))

(define (key-pressure-channel message)
  (ldb +enc-logical-channel-byte+ message))

(define (key-pressure-key message)
  (ldb +enc-data-1-byte+ message))

(define (key-pressure-pressure message)
  (ldb +enc-data-2-byte+ message))

(define-message-set! key-pressure-channel  +enc-logical-channel-byte+)
(define-message-set! key-pressure-key      +enc-data-1-byte+)
(define-message-set! key-pressure-pressure +enc-data-2-byte+)


;;;
;;; :control-change

(define (make-control-change channel controller value)
  (make-channel-message +ml-control-change-opcode+
                        channel controller value))

(define (control-change-p message)
  (= (ldb +enc-opcode-byte+ message) +ml-control-change-opcode+))

(define (control-change-channel message)
  (ldb +enc-logical-channel-byte+ message))

(define (control-change-controller message)
  (ldb +enc-data-1-byte+ message))

(define (control-change-value message)
  (ldb +enc-data-2-byte+ message))

(define-message-set! control-change-channel
    +enc-logical-channel-byte+)

(define-message-set! control-change-controller +enc-data-1-byte+)

(define-message-set! control-change-value  +enc-data-2-byte+)


;;;
;;; :program-change
;;;

(define (make-program-change channel program)
  (make-channel-message +ml-program-change-opcode+ channel program))

(define (program-change-p message)
  (= (ldb +enc-opcode-byte+ message) +ml-program-change-opcode+))

(define (program-change-channel message)
  (ldb +enc-logical-channel-byte+ message))

(define (program-change-program message)
  (ldb +enc-data-1-byte+ message))

(define-message-set! program-change-channel
    +enc-logical-channel-byte+)
(define-message-set! program-change-program +enc-data-1-byte+)


;;;
;;; :channel-pressure

(define (make-channel-pressure channel pressure)
  (make-channel-message +ml-channel-pressure-opcode+ channel pressure))

(define (channel-pressure-p message)
  (= (ldb +enc-opcode-byte+ message) +ml-channel-pressure-opcode+))

(define (channel-pressure-channel message)
  (ldb +enc-logical-channel-byte+ message))

(define (channel-pressure-pressure message)
  (ldb +enc-data-1-byte+ message))

(define-message-set! channel-pressure-channel
    +enc-logical-channel-byte+)

(define-message-set! channel-pressure-pressure +enc-data-1-byte+)


;;;
;;; :pitch-bend

(define (make-pitch-bend channel value . args)
  (let ((width (if (null? args) 2 (car args))))
    (let ((bend (inexact->exact
		 (floor
		  (rescale value (- width) width 0 16383)))))
      (make-channel-message +ml-pitch-bend-opcode+ channel
                            (ldb (byte 7 0) bend)
                            (ldb (byte 7 7) bend)))))

(define (pitch-bend-p message)
  (= (ldb +enc-opcode-byte+ message) +ml-pitch-bend-opcode+))

(define (pitch-bend-channel message)
  (ldb +enc-logical-channel-byte+ message))

(define (pitch-bend-lsb message)
  (ldb +enc-data-1-byte+ message))

(define (pitch-bend-msb message)
  (ldb +enc-data-2-byte+ message))

(define-message-set! pitch-bend-channel +enc-logical-channel-byte+)
(define-message-set! pitch-bend-lsb     +enc-data-1-byte+)
(define-message-set! pitch-bend-msb     +enc-data-2-byte+)

;;; ======================================================================
;;;
;;; System (Common and Real-Time) Messages
;;;

(define (make-system-message type route . args)
  (with-args (args &optional (data1 0) (data2 0))
    (dpb route +enc-route-byte+
         (dpb type +enc-swapped-status-byte+
              (dpb data1 +enc-data-1-byte+
                   (dpb data2 +enc-data-2-byte+ 0))))))

(define (system-message-p message)
  (midi-system-message-p message))

(define (system-message-route message)
  (ldb +enc-route-byte+ message))

(define (system-message-status message)
  (ldb +enc-swapped-status-byte+ message))

(define (system-message-data1 message)
  (ldb +enc-data-1-byte+ message))

(define (system-message-data2 message)
  (ldb +enc-data-2-byte+ message))

(define-message-set! system-message-route +enc-route-byte+)
(define-message-set! system-message-status +enc-swapped-status-byte+)
(define-message-set! system-message-data1  +enc-data-1-byte+)
(define-message-set! system-message-data2  +enc-data-2-byte+)

;;;
;;; :sysex
;;;
;;; if data is pair apply make-sysex-data, else it has to be an array.

(define (make-sysex-data . args)
  (let ((len 2)
        (i 0) ; gets pre-incremented during stuffing
        (msg #() ))
    (letrec ((incflen 
              (lambda (args)
                (dolist (a args)
                  (cond ((char? a)
                         (incf len))
                        ((and (integer? a)
                              (<= 0 i #b11111111))
                         (incf len))
                        ;; add 1 for trailing '\0'
                        ((string? a)
                         (incf len (+ (string-length a) 1)))
                        ((pair? a)
                         (incflen a))
                        (else
                         (err "~s not char, byte or string."
                              a))))))
             (stuff 
              (lambda (byte)
                (incf i)
                (vector-set! msg i byte)))
             (stuffdata 
              (lambda (args)
                (dolist (a args)
                  (cond ((char? a)
                         (stuff (char->integer a)))
                        ((string? a)
                         (loop for i below (string-length a)
                               for c = (string-ref a i)
                               do (stuff (char->integer c))
                               finally (stuff 0)))
                        ((and (integer? a)
                              (<= 0 i #b11111111))
                         (stuff a))
                        ((pair? a)
                         (stuffdata a))
                        (else
                         (err "~s not char, byte or string." a)))))))
            (incflen args)
            ;; allocate data
            (set! msg (make-vector len 0))
            ;; stuff data
            (stuffdata args)
            ;; add tags
            (vector-set! msg 0 #xF0)
            (vector-set! msg (- len 1) #xF7)
            msg)))

(define (make-sysex route data)
  (values (make-system-message +ml-msg-sysex-type+ route)
          (if (pair? data)
            (apply (function make-sysex-data) data)
            (if (vector? data) data
                (err "~s is not a pair or a vector." data)))))

(define (sysex-p message)
  (= (ldb +enc-swapped-status-byte+ message) +ml-msg-sysex-type+))

(define (sysex-route message)
  (ldb +enc-route-byte+ message))

(define-message-set! sysex-route +enc-route-byte+)

;;;
;;; :mtc-quarter-frame

(define (make-mtc-quarter-frame route tag nibble)
  (make-system-message +ml-msg-mtc-quarter-frame-type+ route
		       (logior (ash tag 4) nibble)))

(define (mtc-quarter-frame-p message)
  (= (ldb +enc-swapped-status-byte+ message)
     +ml-msg-mtc-quarter-frame-type+))

(define (mtc-quarter-frame-route message)
  (ldb +enc-route-byte+ message))

(define (mtc-quarter-frame-tag message)
  (logand (ldb +enc-data-1-byte+ message) #b1110000))

(define (mtc-quarter-frame-nibble message)
  (logand (ldb +enc-data-1-byte+ message) #b1111))

(define-message-set! mtc-quarter-frame-route +enc-route-byte+)


;;;
;;; :song-position

(define (make-song-position route lsb msb)
  (make-system-message +ml-msg-song-position-type+ route lsb msb))

(define (song-position-p message)
  (= (ldb +enc-swapped-status-byte+ message)
     +ml-msg-song-position-type+))

(define (song-position-route message)
  (ldb +enc-route-byte+ message))

(define (song-position-lsb message)
  (ldb +enc-data-1-byte+ message))

(define (song-position-msb message)
  (ldb +enc-data-2-byte+ message))

(define-message-set! song-position-route +enc-route-byte+)
(define-message-set! song-position-lsb +enc-data-1-byte+)
(define-message-set! song-position-msb +enc-data-2-byte+)

;;;
;;; :song-select

(define (make-song-select route song)
  (make-system-message +ml-msg-song-select-type+ route song))

(define (song-select-p message)
  (= (ldb +enc-swapped-status-byte+ message)
     +ml-msg-song-select-type+))

(define (song-select-route message)
  (ldb +enc-route-byte+ message))

(define (song-select-song message)
  (ldb +enc-data-1-byte+ message))

(define-message-set! song-select-route +enc-route-byte+)
(define-message-set! song-select-song +enc-data-1-byte+)


;;;
;;; :cable-select

(define (make-cable-select route cable)
  (make-system-message +ml-msg-cable-select-type+ route cable))

(define (cable-select-p message)
  (= (ldb +enc-swapped-status-byte+ message)
     +ml-msg-cable-select-type+))

(define (cable-select-route message)
  (ldb +enc-route-byte+ message))

(define (cable-select-cable message)
  (ldb +enc-data-1-byte+ message))

(define-message-set! cable-select-route +enc-route-byte+)
(define-message-set! cable-select-cable +enc-data-1-byte+)


;;;
;;; :tune-request

(define (make-tune-request route)
  (make-system-message +ml-msg-tune-request-type+ route))

(define (tune-request-p message)
  (= (ldb +enc-swapped-status-byte+ message) +ml-msg-tune-request-type+))

(define (tune-request-route message)
  (ldb +enc-route-byte+ message))

(define-message-set! tune-request-route +enc-route-byte+)


;;;
;;; :eox (unused, but who cares...)

(define (make-eox route)
  (make-system-message +ml-msg-eox-type+ route))

(define (eox-p message)
  (= (ldb +enc-swapped-status-byte+ message) +ml-msg-eox-type+))

(define (eox-route message)
  (ldb +enc-route-byte+ message))

(define-message-set! eox-route +enc-route-byte+)


;;;
;;; :timing-clock

(define (make-timing-clock route)
  (make-system-message +ml-msg-timing-clock-type+ route))

(define (timing-clock-p message)
  (= (ldb +enc-swapped-status-byte+ message) +ml-msg-timing-clock-type+))

(define (timing-clock-route message)
  (ldb +enc-route-byte+ message))

(define-message-set! timing-clock-route +enc-route-byte+)


;;;
;;; :timing-tick

(define (make-timing-tick route)
  (make-system-message +ml-msg-timing-tick-type+ route))

(define (timing-tick-p message)
  (= (ldb +enc-swapped-status-byte+ message) +ml-msg-timing-tick-type+))

(define (timing-tick-route message)
  (ldb +enc-route-byte+ message))

(define-message-set! timing-tick-route +enc-route-byte+)


;;;
;;; :start

(define (make-start route)
  (make-system-message +ml-msg-start-type+ route))

(define (start-p message)
  (= (ldb +enc-swapped-status-byte+ message) +ml-msg-start-type+))

(define (start-route message)
  (ldb +enc-route-byte+ message))

(define-message-set! start-route +enc-route-byte+)


;;;
;;; :continue

(define (make-continue route)
  (make-system-message +ml-msg-continue-type+ route))

(define (continue-p message)
  (= (ldb +enc-swapped-status-byte+ message) +ml-msg-continue-type+))

(define (continue-route message)
  (ldb +enc-route-byte+ message))

(define-message-set! continue-route +enc-route-byte+)


;;;
;;; :stop

(define (make-stop route)
  (make-system-message +ml-msg-stop-type+ route))

(define (stop-p message)
  (= (ldb +enc-swapped-status-byte+ message) +ml-msg-stop-type+))

(define (stop-route message)
  (ldb +enc-route-byte+ message))

(define-message-set! stop-route +enc-route-byte+)


;;;
;;; :active-sensing

(define (make-active-sensing route)
  (make-system-message +ml-msg-active-sensing-type+ route))

(define (active-sensing-p message)
  (= (ldb +enc-swapped-status-byte+ message)
     +ml-msg-active-sensing-type+))

(define (active-sensing-route message)
  (ldb +enc-route-byte+ message))

(define-message-set! active-sensing-route +enc-route-byte+)


;;;
;;; :system-reset

(define (make-system-reset route)
  (make-system-message +ml-msg-system-reset-type+ route))

(define (system-reset-p message)
  (= (ldb +enc-swapped-status-byte+ message)
     +ml-msg-system-reset-type+))

(define (system-reset-route message)
  (ldb +enc-route-byte+ message))

(define-message-set! system-reset-route +enc-route-byte+)



;;; ==========================================================================
;;;
;;; MIDI File Meta Messages
;;;

;;;
;;; meta messages are of the form:  
;;;
;;;   +ml-meta-type+ <type>           are cm-encoded in a fixnum
;;;   <length> <data>                 array
;;;
;;; the <length> field is of variable-length and prepended to the message.
;;;
;;; Since meta messages are for MIDI files only, we omit the route field.

(define (make-meta-message type . data-bytes)
  (values (dpb +ml-meta-type+ +enc-swapped-status-byte+
               (dpb type +enc-data-1-byte+ 0))
          ;; 
          (let ((l (length data-bytes))
                (v #f) (d #f))
            (set! v (if (< l #x80) 1
                        (if (< l #x4000) 2
                            (if (< l #x200000) 3
                                (if (< l #x10000000) 4
                                    (err "Illegal length: ~s" l))))))
            (set! d (make-vector (+ v l) 0 ))
            (do ((i 0 (+ 1 i))
                 (offs (* (- v 1) 7) (- offs 7)))
                ((not (< i v)) #f)
              (vector-set! d i
                           (if (= offs 0)
                             (ldb (byte 7 offs) l)
                             (logior (ldb (byte 7 offs) l) #x80))))
            (do ((i v (+ i 1))
                 (b data-bytes (cdr b)))
                ((null? b) #f)
              (vector-set! d i (car b)))
            d)))

(define (meta-message-p message)
  (midi-meta-message-p message))

(define (meta-message-type message)
  (ldb +enc-data-1-byte+ message))

(define-message-set! meta-message-type +enc-data-1-byte+)


;;;
;;; :sequence-number

(define (make-sequence-number num)
  (when (>= num #x10000)
    (err "~s too big for a sequence number." num))
  (make-meta-message +ml-file-sequence-number-opcode+
                     (ldb (byte 8 8) num) (ldb (byte 8 0) num)))

(define (sequence-number-p message)
  (and (midi-meta-message-p message)
       (= (ldb +enc-data-1-byte+ message)
	  +ml-file-sequence-number-opcode+)))

;;(define (sequence-number-type message)
;;  (ldb +enc-data-1-byte+ message))

;;(define-message-set! sequence-number-type +enc-data-1-byte+)


;;;
;;; :text-event
;;;
;;; The constructor (but NOT the predicate) is the workhorse for all
;;; other text events. 

(define (make-text-event string . args)
  (with-args (args &optional (type +ml-file-text-event-opcode+))
    (apply (function make-meta-message) type
           (loop for i below (string-length string)
                 collect (char->integer (string-ref string i))))))


(define (text-event-p message)
    (and (midi-meta-message-p message)
	 (= (ldb +enc-data-1-byte+ message)
	    +ml-file-text-event-opcode+)))

;;(define (text-event-type message)
;;  (ldb +enc-data-1-byte+ message))

;;(define-message-set! text-event-type +enc-data-1-byte+)

;;;
;;; Utilities for all text meta events:

(define +text-meta-event-types+
  (list +ml-file-text-event-opcode+
        +ml-file-copyright-note-opcode+
        +ml-file-sequence/track-name-opcode+
        +ml-file-instrument-name-opcode+
        +ml-file-lyric-opcode+
        +ml-file-marker-opcode+
        +ml-file-cue-point-opcode+))

(define (text-meta-event-p message)
  (and (midi-meta-message-p message)
       (find (ldb +enc-data-1-byte+ message) +text-meta-event-types+)))

(define (text-meta-event-data-to-string data)
  (let ((len (vector-ref data 0)))
    (when (= (vector-ref data len) 0)
      (set! len (max (- len 1) 0)))
    (loop with str = (make-string len)
          for i below len
          do (string-set! str i (integer->char (vector-ref data (+ i 1))))
          finally (return str))))

;(text-meta-event-data-to-string #(3 102 111 111))       => "foo"
;(text-meta-event-data-to-string #(3 102 111 111 0))     => "foo"
;(text-meta-event-data-to-string #(4 102 111 111 0))     => "foo"
;(text-meta-event-data-to-string #(0))                   => ""

;;;
;;; :copyright-note

(define (make-copyright-note string)
  (make-text-event string +ml-file-copyright-note-opcode+))

(define (copyright-note-p message)
  (and (midi-meta-message-p message)
       (= (ldb +enc-data-1-byte+ message)
	  +ml-file-copyright-note-opcode+)))

;;(define (copyright-note-type message)
;;  (ldb +enc-data-1-byte+ message))

;;(define-message-set! copyright-note-type +enc-data-1-byte+)


;;;
;;; :sequence/track-name

(define (make-sequence/track-name string)
  (make-text-event string +ml-file-sequence/track-name-opcode+))

(define (sequence/track-name-p message)
  (and (midi-meta-message-p message)
       (= (ldb +enc-data-1-byte+ message)
	  +ml-file-sequence/track-name-opcode+)))

;;(define (sequence/track-name-type message)
;;  (ldb +enc-data-1-byte+ message))

;;(define-message-set! sequence/track-name-type +enc-data-1-byte+)


;;;
;;; :instrument-name

(define (make-instrument-name string)
  (make-text-event string +ml-file-instrument-name-opcode+))

(define (instrument-name-p message)
  (and (midi-meta-message-p message)
       (= (ldb +enc-data-1-byte+ message)
	  +ml-file-instrument-name-opcode+)))

;;(define (instrument-name-type message)
;;  (ldb +enc-data-1-byte+ message))

;;(define-message-set! instrument-name-type +enc-data-1-byte+)


;;;
;;; :lyric

(define (make-lyric string)
  (make-text-event string +ml-file-lyric-opcode+))

(define (lyric-p message)
  (and (midi-meta-message-p message)
       (= (ldb +enc-data-1-byte+ message)
	  +ml-file-lyric-opcode+)))

;;(define (lyric-type message)
;;  (ldb +enc-data-1-byte+ message))

;;(define-message-set! lyric-type +enc-data-1-byte+)


;;;
;;; :marker

(define (make-marker string)
  (make-text-event string +ml-file-marker-opcode+))

(define (marker-p message)
  (and (midi-meta-message-p message)
       (= (ldb +enc-data-1-byte+ message)
	  +ml-file-marker-opcode+)))

;;(define (marker-type message)
;;  (ldb +enc-data-1-byte+ message))

;;(define-message-set! marker-type +enc-data-1-byte+)


;;;
;;; :cue-point

(define (make-cue-point string)
  (make-text-event string +ml-file-cue-point-opcode+))

(define (cue-point-p message)
  (and (midi-meta-message-p message)
       (= (ldb +enc-data-1-byte+ message)
	  +ml-file-cue-point-opcode+)))

;;(define (cue-point-type message)
;;  (ldb +enc-data-1-byte+ message))

;;(define-message-set! cue-point-type +enc-data-1-byte+)


;;;
;;; :midi-channel

(define (make-midi-channel channel)
  (make-meta-message +ml-file-midi-channel-opcode+ channel))

(define (midi-channel-p message)
  (and (midi-meta-message-p message)
       (= (ldb +enc-data-1-byte+ message)
	  +ml-file-midi-channel-opcode+)))


;;;
;;; :midi-port

(define (make-midi-port port)
  (make-meta-message +ml-file-midi-port-opcode+ port))

(define (midi-port-p message)
  (and (midi-meta-message-p message)
       (= (ldb +enc-data-1-byte+ message)
	  +ml-file-midi-port-opcode+)))


;;;
;;; :eot

(define (make-eot )
  (make-meta-message +ml-file-eot-opcode+))

(define (eot-p message)
  (and (midi-meta-message-p message)
       (= (ldb +enc-data-1-byte+ message)
	  +ml-file-eot-opcode+)))

;;(define (eot-type message)
;;  (ldb +enc-data-1-byte+ message))

;;(define-message-set! eot-type +enc-data-1-byte+)


;;;
;;; :tempo-change
;;;
;;; Tempo is in microseconds per MIDI quarter note

(define (make-tempo-change usecs-per-beat)
  (apply (function make-meta-message)
         +ml-file-tempo-change-opcode+
          (loop for pos from 16 by 8 downto 0
                collect (ldb (byte 8 pos) usecs-per-beat))))

(define (tempo-change-p message)
  (and (midi-meta-message-p message)
       (= (ldb +enc-data-1-byte+ message)
	  +ml-file-tempo-change-opcode+)))

;;(define (tempo-change-type message)
;;  (ldb +enc-data-1-byte+ message))

;;(define-message-set! tempo-change-type +enc-data-1-byte+)


;;;
;;; :smpte-offset

(define (make-smpte-offset hours mins secs frames fractional-frames)
  (make-meta-message +ml-file-smpte-offset-opcode+
                     hours mins secs frames fractional-frames))

(define (smpte-offset-p message)
  (and (midi-meta-message-p message)
       (= (ldb +enc-data-1-byte+ message)
	  +ml-file-smpte-offset-opcode+)))

;;(define (smpte-offset-type message)
;;  (ldb +enc-data-1-byte+ message))

;;(define-message-set! smpte-offset-type +enc-data-1-byte+)


;;;
;;; :time-signature

(define (make-time-signature numerator denominator . args)
  (with-args (args &optional (clocks 24) (32nds 8))
    (multiple-value-bind (f r) (clfloor (log2 denominator ))
      (unless (zero? r)
        (err "Time signature denominator ~s is not a power of 2." 
             denominator))
      (make-meta-message +ml-file-time-signature-opcode+
		         numerator f clocks 32nds))))

(define (time-signature-p message)
  (and (midi-meta-message-p message)
       (= (ldb +enc-data-1-byte+ message)
	  +ml-file-time-signature-opcode+)))

;;(define (time-signature-type message)
;;  (ldb +enc-data-1-byte+ message))

;;(define-message-set! time-signature-type +enc-data-1-byte+)

;;;
;;; :key-signature

(define (make-key-signature key . args)
  (with-args (args &optional (mode ':major) )
    (let ((sf #f))
      (set! mode (case mode
                   ((:major major 0) 0)
                   ((:minor minor 1) 1)
                   (else (err "key signature mode not :major or :minor"))))
      (cond
        ((number? key)
         (unless (<= -7 key 7)
           (err "Key signature must be between -7 (b) and 7 (#)."))
         (set! sf key))
        (else
         (err "~s is not a number or symbol." key)))
      (set! sf (if (< sf 0) (+ sf 256) sf))
      (make-meta-message +ml-file-key-signature-opcode+ sf mode))))

(define (key-signature-p message)
  (and (midi-meta-message-p message)
       (= (ldb +enc-data-1-byte+ message)
	  +ml-file-key-signature-opcode+)))

;;(define (key-signature-type message)
;;  (ldb +enc-data-1-byte+ message))

;;(define-message-set! key-signature-type +enc-data-1-byte+)


;;;
;;; :sequencer-event

(define (make-sequencer-event . data)
  (apply (function make-meta-message)
         +ml-file-sequencer-event-opcode+ data))

(define (sequencer-event-p message)
  (and (midi-meta-message-p message)
       (= (ldb +enc-data-1-byte+ message)
	  +ml-file-sequencer-event-opcode+)))

;;(define (sequencer-event-type message)
;;  (ldb +enc-data-1-byte+ message))

;;(define-message-set! sequencer-event-type +enc-data-1-byte+)


;;;
;;; make-midimsg provides a general interface to message construction
;;; commented out, not really neede
;
;(define (make-midimsg type . args)
;  (apply (case type
;	   ;; channel messages
;	   ((:channel-message channel-message) 
;            (function make-channel-message))
;	   ((:note-off note-off) 
;            (function make-note-off))
;	   ((:note-on note-on) 
;            (function make-note-on))
;	   ((:key-pressure key-pressure)
;            (function make-key-pressure))
;	   ((:control-change control-change) 
;            (function make-control-change))
;	   ((:program-change program-change) 
;            (function make-program-change))
;	   ((:channel-pressure channel-pressure)
;            (function make-channel-pressure))
;	   ((:pitch-bend pitch-bend)
;            (function make-pitch-bend))
;	   ;; system messages
;	   ((:system-message system-message) 
;            (function make-system-message))
;	   ((:sysex sysex)
;            (function make-sysex))
;	   ((:mtc-quarter-frame mtc-quarter-frame)
;            (function make-mtc-quarter-frame))
;	   ((:song-position song-position)
;            (function make-song-position))
;	   ((:song-select song-select) 
;            (function make-song-select))
;	   ((:cable-select cable-select)
;            (function make-cable-select))
;	   ((:tune-request tune-request) 
;            (function make-tune-request))
;	   ((:eox eox) 
;            (function make-eox))
;	   ((:timing-clock timing-clock) 
;            (function make-timing-clock))
;	   ((:timing-tick timing-tick)
;            (function make-timing-tick))
;	   ((:start start) 
;            (function make-start))
;	   ((:continue continue) 
;            (function make-continue))
;	   ((:stop stop) 
;            (function make-stop))
;	   ((:active-sensing active-sensing) 
;            (function make-active-sensing))
;	   ((:system-reset system-reset) 
;            (function make-system-reset))
;	   ;; meta messages
;	   ((:meta-message meta-message) 
;            (function make-meta-message))
;	   ((:sequence-number sequence-number) 
;            (function make-sequence-number))
;	   ((:text-event text-event) 
;            (function make-text-event))
;	   ((:copyright-note copyright-note) 
;            (function make-copyright-note))
;	   ((:sequence/track-name sequence/track-name)
;	    (function make-sequence/track-name))
;	   ((:instrument-name instrument-name) 
;            (function make-instrument-name))
;	   ((:lyric lyric) 
;            (function make-lyric))
;	   ((:marker marker) 
;            (function make-marker))
;	   ((:cue-point cue-point) 
;            (function make-cue-point))
;	   ((:midi-channel midi-channel) 
;            (function make-midi-channel))
;	   ((:midi-port midi-port) 
;            (function make-midi-port))
;	   ((:eot eot) 
;            (function make-eot))
;	   ((:tempo-change tempo-change) 
;            (function make-tempo-change))
;	   ((:smpte-offset smpte-offset) 
;            (function make-smpte-offset))
;	   ((:time-signature time-signature) 
;            (function make-time-signature))
;	   ((:key-signature key-signature) 
;            (function make-key-signature))
;	   ((:sequencer-event sequencer-event) 
;            (function make-sequencer-event)))
;	 args))
;
;(defmacro midimsg-case (message . body)
;  (let ((msgtst
;         (lambda (type msg)
;           (unless (string? type)
;             (set! type (symbol->string type)))
;           (cond ((string-ci=? type "NOTE-OFF")
;                  `(= (ldb +enc-opcode-byte+ ,msg)
;                      +ml-note-off-opcode+))
;                 ((string-ci=? type "NOTE-ON")
;                  `(= (ldb +enc-opcode-byte+ ,msg) 
;                      +ml-note-on-opcode+))
;                 ((string-ci=? type "KEY-DOWN")
;                  `(and (= (ldb +enc-opcode-byte+ ,msg)
;                           +ml-note-on-opcode+)
;                        (> (ldb +enc-data-2-byte+ ,msg) 0)))
;                 ((string-ci=? type "KEY-UP")
;                  `(or (= (ldb +enc-opcode-byte+ ,msg)
;                          +ml-note-off-opcode+)
;                       (and (= (ldb +enc-opcode-byte+ ,msg)
;                               +ml-note-on-opcode+)
;                            (= (ldb +enc-data-2-byte+ ,msg)
;                               0))))
;                 ((string-ci=? type "KEY-PRESSURE")
;                  `(= (ldb +enc-opcode-byte+ ,msg)
;                      +ml-key-pressure-opcode+))
;                 ((string-ci=? type "PITCH-BEND")
;                  `(= (ldb +enc-opcode-byte+ ,msg) 
;                      +ml-pitch-bend-opcode+))
;                 ((string-ci=? type "CONTROL-CHANGE")
;                  `(= (ldb +enc-opcode-byte+ ,msg)
;                      +ml-control-change-opcode+))
;                 ((string-ci=? type "PROGRAM-CHANGE")
;                  `(= (ldb +enc-opcode-byte+ ,msg)
;                      +ml-program-change-opcode+))
;                 ((string-ci=? type "CHANNEL-PRESSURE")
;                  `(= (ldb +enc-opcode-byte+ ,msg)
;                      +ml-channel-pressure-opcode+))
;                 ((string-ci=? type "SYSEX")
;                  `(sysex-p ,msg))
;                 ((string-ci=? type "EOX")
;                  `(eox-p ,msg))
;                 ((or (string-ci=? type "#T")
;                      (string-ci=? type "ELSE"))
;                  #t)
;                 (else
;                  (err "~A is not a midi message type."
;                                type))))))
;    (let ((msg (gensym)))
;      `(let ((,msg ,message))
;         (cond
;          ,@ (map (lambda (form)
;                    `(,(if (pair? (car form))
;                         `(or ,@ (map (lambda (f) (msgtst f msg))
;                                      (car form)))
;                         (msgtst (car form) msg))
;                      ,@ (cdr form)))
;                  body))))))


;;;
;;;
;;;

(define (midi-print-channel-message stream gm? size type channel
				    data1 data2
                                    time-string)
  (let ((name (vector-ref +ml-channel-msg-type-strings+ 
                          (logand type #b111))))
    (format stream "~a~a" name time-string)
    (when gm?
      (let ((p (gm-percussion-channel-p channel)))
        (if p (set! channel "P"))
        (cond ((= type #b1100)  ; program change
               (set! data1 (gm-patch-name data1)))
              ((= type #b1011)  ; control change
               (set! data1 (midi-controller-name data1)))
              ((or (= type #b1010)  ; key pressure
                   (= type #b1000)  ; note on
                   (= type #b1001)) ; note on
               (if p
                 (set! data1 (gm-drum-kit-name data1)))))))
    (format stream " ~a" channel)
    (if (= size 3)
      (format stream " ~s ~s" data1 data2)
      (format stream " ~s" data1))))

;;;
;;; MIDI Message Printing
;;;

;;; Set this to T to enable verbose GM MIDI Message printing.

(define *midi-gm-mode* #t)

;;; Print <max> bytes of (<length> bytes of) data pointed to by <data>.
;;; Line has 16 message bytes, using 70 bytes of storage (including the 
;;; trailing newline.
;
; 1         2         3         4         5         6        
; 123456789012345678901234567890123456789012345678901234567890123456789
;------------------------------------------------------------------------
;system exclusive:   0  11 104        25407
; | 000000:  f000 0104 0001 0605 042f 666f 6f2e 6169   ........./foo.ai
; | 000010:  6666 0063 6861 6e67 6520 7072 6573 6574   ff.change preset
; |          [... (204 Bytes remaining)]
; | 000020:  00f7                                      ..              
;------------------------------------------------------------------------

(define (%print-sysex-aux stream data bytes rest indent)
  (let ((offs 0) 
        (toprint bytes)
        (n 0) 
        (oldn #f)
        (blank #f))
    (loop while (> toprint 0)
          do 
          ;; print lines
          (when indent (format stream indent))
          ;; offset 
          ;;(format stream "~6,'0d:  " offs)
          (format stream (format-integer offs 6 #\0))
          (format stream ":")

          ;; bytes
          (set! oldn n)               ; cache n
          (loop for i below 16 
                while (< n bytes)
                do (begin
                    ;;(format stream "~2,'0x~:[~; ~]"
                    ;;        (vector-ref data n) (odd? i))
                    (format stream
                            (format-integer (vector-ref data n) 2 #\0))
                    (if (odd? i) (format stream " "))
                    (incf n))
                finally (set! blank (- 16 i)))
          ;; padding 
          ;;(format stream (format nil "~~~d@t" 
          ;;                       (- (+ (* blank 3) 2)
          ;;                          (floor (/ blank 2)))))
          (dotimes (i (- (+ (* blank 3) 2)
                        (floor (/ blank 2))))
            (format stream " "))

          ;; chars
          (set! n oldn)               ; restore n
          (loop for i below 16
                while (< n  bytes)
                for b = (vector-ref data n)
                do (begin
                    ;;(format stream "~:[.~*~;~c~]"
                    ;;        (< 31 b 127) (integer->char b))
                    (if (< 31 b 127)
                      (format stream (make-string 1 (integer->char b)))
                      (format stream "."))
                    (incf n))
                finally (decf toprint i))
          (format stream "~%")
          (incf offs 16))
    
    ;; print remark, if necessary
    (when (> rest 0)
      (format stream "         [... (~s Bytes remaining)]~%" rest))
    (values)))

;(print-sysex-data #t #f 1 #(0 1 2 3 23 91 92 95 91 66) 10)
;(print-sysex-data #t #f 8 (make-vector 256 61) #f)

(define (print-sysex-data stream string lines data length)
  ;; accept only :stdout and :stderr, to be comptible with the C version
  (let ((bytes 0)
        (rest 0))
    (unless length
      (set! length (vector-length data)))
    (set! lines (if (number? lines) lines 0))
    (set! bytes (if (and (> lines 0)
                         (< (* lines 16) length))
                  (* (- lines 1) 16)  ; reserve last line for remark
                  length))
    (set! rest (- length bytes))
    ;; if we print to both a string and stream,
    ;; do string first, then print it
    (if string
      (err "string output not supported")
      (when stream
        (%print-sysex-aux stream data bytes rest #f)))))

(define (midi-print-message msg time . args)
  (with-args (args &key data length
                   (stream #t)
                   (time-format #t)
                   (time-string "")
                   (gm *midi-gm-mode*)
                   (delimit #t))
    (when (and time time-format)
      (format stream "~a " (format-integer time 8 #\space)))
    (when delimit (format stream "#<" ))
    (cond ((midi-channel-message-p msg)
           (let* ((size (midimsg-size msg))
                  (op (channel-message-opcode msg))
                  (chan (channel-message-channel msg))
                  (data1 (channel-message-data1 msg))
                  (data2 (channel-message-data2 msg)))
             
             (midi-print-channel-message stream gm size op chan 
                                         data1 data2 time-string)))
          ((midi-system-message-p msg)
           (let ((name (vector-ref +ml-msg-type-strings+
                                   (ldb +enc-lower-status-byte+ msg)))
                 (route (ldb +enc-route-byte+ msg))
                 (size (midimsg-size msg)))
             (format stream "~a~a ~s" name time-string route)
             (cond ((= size 3)
                    (format stream " ~s ~s"
                            (midimsg-data1 msg)
                            (midimsg-data2 msg)))
                   ((= size 2)
                    (format stream " ~s" (midimsg-data1 msg)))
                   (t (when delimit 
                        (format stream ">")
                        (set! delimit #f))
                      (when (sysex-p msg)
                        (format stream "~%")
                        (print-sysex-data #t #f 0 data length))))))
          ((midi-meta-message-p msg)
           (cond ((or (midi-channel-p msg)
                      (midi-port-p msg))
                  (format stream "~a~a ~s"
                          (get-meta-msg-type-string
                           (ldb +enc-data-1-byte+ msg))
                          time-string
                          (if data (vector-ref data 1) "?")))
                 ((tempo-change-p msg)
                  (format stream "~a~a ~s ms"
                          (get-meta-msg-type-string
                           (ldb +enc-data-1-byte+ msg))
                          time-string
                          (/ (+ (ash (vector-ref data 1) 16)
                                (ash (vector-ref data 2)  8)
                                (vector-ref data 3))
                             1000)))
                 ((time-signature-p msg)
                  (format stream "~a~a ~s/~s (~s clocks, ~s 32nds)"
                          (get-meta-msg-type-string
                           (ldb +enc-data-1-byte+ msg))
                          time-string
                          (vector-ref data 1) 
                          (expt 2 (vector-ref data 2))
                          (vector-ref data 3)
                          (vector-ref data 4)))
                 ((key-signature-p msg)
                  (let ((a (vector-ref data 1))
                        (s #f))
                    (set! a (or (and (logbit? a #x80) 
                                     (- a #x100)) a))
                    
                    (set! s (if (> (abs a) 1) "s" ""))
                    (format stream "~a~a ~a ~a~a, ~a"
                            (get-meta-msg-type-string
                             (ldb +enc-data-1-byte+ msg))
                            time-string
                            (if (zero? a) "no" (abs a))
                            (case (signum a)
                              ((-1) "flat")
                              (( 0) "accidentals")
                              (( 1) "sharp"))
                            s 
                            (if (zero? (vector-ref data 2)) 
                              "major" "minor"))))
                 (else
                  (format stream "~a~a"
                          (get-meta-msg-type-string
                           (ldb +enc-data-1-byte+ msg))
                          time-string)
                  (if data
                    (format stream " ~s"
                            (if (text-meta-event-p msg)
                              (text-meta-event-data-to-string data)
                              data))
                    (format stream ">")))))
          (else
           (format stream "Bogus Midi Message~a]" time-string)))
    (when delimit (format stream ">"))
    msg))

;;;
;;; MIDI Constants and Utilities
;;;

;;;
;;; deflabel defines a constant and inserts its namestring in a vector at
;;; the given offset.

(define %deflabelvar% (gensym))

(defmacro deflabel (sym val vector str pos)
  `(begin
    (define ,sym ,val)
    ,(if str
	 `(vector-set! ,vector ,pos ,str)
	 `(begin
	   (set! %deflabelvar% (format #f "~a" ',sym))
	   (do ((i 0 (+ i 1))
		(e (string-length %deflabelvar%)))
	       ((not (< i e)) #f)
	     (if (char=? (string-ref %deflabelvar% i) #\space)
	       (string-set! %deflabelvar% i #\-)))
	   (vector-set! ,vector ,pos
	    (string-capitalize!
	     (substring %deflabelvar%
			1 (- (string-length %deflabelvar%)
			     1))))))))


;;; ======================================================================
;;;
;;; Controller Numbers

(define +midi-controller-strings+
  (make-vector 128 ""))

(defmacro defcontroller (sym val . str)
  `(deflabel ,sym ,val +midi-controller-strings+
    ,(if (null? str) #f (car str))
    ,val))

;;;
;;; Utilities

(define (midi-opcode-name vec code lb ub delta)
  (cond ((and (exact? code)
	      (<= lb code ub))
	 (vector-ref vec (+ code delta)))
	((string? code) code)
	(else
	 (err "MIDI opcode ~s not string or int ~s-~s."
              code lb ub))))

(define (midi-controller-name c)
  (midi-opcode-name +midi-controller-strings+ c 0 127 0))


; (midi-controller-name 71)
; (midi-controller-name "unset")

;;;
;;; There are 128 possible controller numbers (ie, 0 to 127).  Some numbers
;;; are defined for specific purposes. Others are undefined, and reserved
;;; for future use. 
;;; 
;;; Naming according to the MIDI Manufacturers Association (cf. 
;;; http://www.midi.org/table3.html) with the exception 
;;; of "Fine" controllers (32-51, 98 and 100).

;;;
;;; Controllers 0-31 should be used for coarse adjustments

;;; Specific Continuous Controllers 0-16,383 (Coarse, MSB of 14 bits)

(defcontroller +Bank-Select+		  0) ; no-op unless followed by a
					     ; program change
(defcontroller +Modulation-Wheel+	  1) ; 0 is no modulation effect
(defcontroller +Breath-Control+		  2) ; 0 is minimum breath pressure
(defcontroller +Foot-Controller+	  4) ; 0 is minimum effect
(defcontroller +Portamento-Time+	  5) ; 0 is slowest rate
(defcontroller +Data-Entry+		  6) ; value of a previously set
					     ; registered or non-registered
					     ; parameter.  0 is minimum effect
(defcontroller +Channel-Volume+		  7) ; formerly "main volume"
(define +Volume+			  7) ; (alternative name)
(defcontroller +Balance+		  8) ; balance before pan. 0 is left
(defcontroller +Pan+			 10) ; 0 is left
(defcontroller +Expression-Controller+	 11) ; percentage of volume. 0 is off
(defcontroller +Effect-Control-1+	 12) ; 0 is minimum effect
(defcontroller +Effect-Control-2+	 13) ; 0 is minimum effect
(defcontroller +General-Purpose-Controller-1+	 16
  "General-Purpose Controller 1")
(defcontroller +General-Purpose-Controller-2+	 17
  "General-Purpose Controller 2")
(defcontroller +General-Purpose-Controller-3+	 18
  "General-Purpose Controller 3")
(defcontroller +General-Purpose-Controller-4+	 19
  "General-Purpose Controller 4")

;;;
;;; Controllers 32-63 should be used for fine adjustments

;;; Specific Continuous Controllers 0-16,383 (Fine, LSB of 14 bits)
;;;
;;; Some devices do not implement these fine adjust counterparts to the
;;; coarse specific continuous controllers above.  However, devices that
;;; implement 14-bit resolution are required to deal with either the coarse
;;; or fine controller message being sent without its counterpart following.

(defcontroller +Bank-Select-Fine+		 32 "Bank Select (Fine)")
(defcontroller +Modulation-Wheel-Fine+		 33 "Modulation Wheel (Fine)")
(defcontroller +Breath-Control-Fine+		 34 "Breath Control (Fine)")
(defcontroller +Foot-Controller-Fine+		 36 "Foot Controller (Fine)")
(defcontroller +Portamento-Time-Fine+		 37 "Portamento Time (Fine)")
(defcontroller +Data-Entry-Fine+		 38 "Data Entry (Fine)")
(defcontroller +Channel-Volume-Fine+		 39 "Channel Volume (Fine)")
(define +Volume-Fine+			 39) ; (alternative name)
(defcontroller +Balance-Fine+			 40 "Balance (Fine)")
(defcontroller +Pan-Fine+			 42 "Pan (Fine)")
(defcontroller +Expression-Controller-Fine+	 43
  "Expression Controller (Fine)")
(defcontroller +Effect-Control-1-Fine+		 44 "Effect Control 1 (Fine)")
(defcontroller +Effect-Control-2-Fine+		 45 "Effect Control 2 (Fine)")
(defcontroller +General-Purpose-Controller-1-Fine+ 48 
  "General-Purpose Controller 1 (Fine)")
(defcontroller +General-Purpose-Controller-2-Fine+ 49 
  "General-Purpose Controller 1 (Fine)")
(defcontroller +General-Purpose-Controller-3-Fine+ 50 
  "General-Purpose Controller 1 (Fine)")
(defcontroller +General-Purpose-Controller-4-Fine+ 51 
  "General-Purpose Controller 1 (Fine)")

;;; Common Switches 0(-63): on/(64-)127: off

(defcontroller +Hold-1+			 64) ; when on, also postpones any 
					     ; All-Notes-Off controller
					     ; message on the same channel
(define +Sustain+			 64) ; (alternative name)
(define +Damper-Pedal+		 64) ; (alternative name)
(defcontroller +Portamento+		 65)
(defcontroller +Sostenuto+		 66) ; only sustain sounding notes
					     ; when on, also postpones any 
					     ; All-Notes-Off controller
					     ; message on the same channel
					     ; for the notes held 
(defcontroller +Soft-Pedal+		 67) 
(defcontroller +Legato-Footswitch+	 68)
(defcontroller +Hold-2+			 69) ; lengthen release times

;;; Sound Controllers 0-127.  0 is always minimum setting

(defcontroller +Sound-Control-1+	 70) ; anything, really
(define +Sound-Variation+		 70) ; (alternative name)
(defcontroller +Sound-Control-2+	 71) ; VCF envelope brightness control
(define +Sound-Timbre+		 71) ; (alternative name)
(defcontroller +Sound-Control-3+	 72)
(define +Sound-Release-Time+	 72) ; (alternative name)
(defcontroller +Sound-Control-4+	 73)
(define +Sound-Attack-Time+	 73) ; (alternative name)
(defcontroller +Sound-Control-5+	 74) ; VCF cutoff brightness control
(define +Sound-Brightness+		 74) ; (alternative name)
(defcontroller +Sound-Control-6+	 75)
(defcontroller +Sound-Control-7+	 76)
(defcontroller +Sound-Control-8+	 77)
(defcontroller +Sound-Control-9+	 78)
(defcontroller +Sound-Control-10+	 79)

;;; Additional (Coarse) Controllers 0-127
(defcontroller +General-Purpose-Controller-5+	 80
  "General-Purpose Controller 5")
(defcontroller +General-Purpose-Controller-6+	 81
  "General-Purpose Controller 6")
(defcontroller +General-Purpose-Controller-7+	 82
  "General-Purpose Controller 7")
(defcontroller +General-Purpose-Controller-8+	 83
  "General-Purpose Controller 8")

(defcontroller +Portamento-Control+	 84) ; uses source note

;;; Level Controllers 0-127
(defcontroller +Effects-1-Depth+	 91)
(define +Effects-Level+		 91) ; (alternative name)
(defcontroller +Effects-2-Depth+	 92)
(define +Tremolo-Level+		 92) ; (alternative name)
(defcontroller +Effects-3-Depth+	 93)
(define +Chorus-Level+		 93) ; (alternative name)
(defcontroller +Effects-4-Depth+	 94)
(define +Detune-Level+		 94) ; (alternative name)
(defcontroller +Effects-5-Depth+	 95)
(define +Phasor-Level+		 95) ; (alternative name)

;;; Data Entry Step Controllers (without Value Byte)
(defcontroller +Data-Entry-+1+		 96) ; increment a previously set
					     ; registered or non-registered
					     ; parameter
(define +Data-Entry-Increment+	 96) ; (alternative name)
(defcontroller +Data-Entry--1+		 97 "Data Entry -1") 
					     ; decrement a previously set
					     ; registered or non-registered
					     ; parameter
(define +Data-Entry-Decrement+	 97) ; (alternative name)

;;; Parameter Number Selection 0-16,383 (LSB or MSB of 14 bits)
(defcontroller +Non-Registered-Parameter-Number-Fine+	 98
  "Non-Registered Parameter Number (Fine)")
(defcontroller +Non-Registered-Parameter-Number+	 99
  "Non-Registered Parameter Number")
(defcontroller +Registered-Parameter-Number-Fine+	100
  "Registered Parameter Number (Fine)")
(defcontroller +Registered-Parameter-Number+		101) ; (coarse)

;;; Channel Mode Messages (without Value Byte, unless stated otherwise)
;;; Note: the 4 omni/poly messages must be received on the device's Base
;;; Channel.
(defcontroller +All-Sound-Off+		 120)
(defcontroller +Reset-All-Controllers+	 121)
(defcontroller +Local-Control+		 122) ; 0(-63): on/(64-)127: off
(defcontroller +All-Notes-Off+		 123)
(defcontroller +Omni-Mode-Off+		 124) ; + all notes off
(defcontroller +Omni-Mode-On+		 125) ; + all notes off
(defcontroller +Poly-Mode-On/Off+	 126) ; value equals the number of
					      ; channels, or zero if the
					      ; number of channels equals
					      ; the number of voices in the
					      ; receiver. + all notes off if 
					      ; value > 1 ("Mono Off")
(defcontroller +Poly-Mode-On+		 127) ; + all notes off

;;;
;;; Registered Parameter Numbers

(define +RPN-Pitch-Bend-Sensitivity+   '(#x00 #x00))
(define +RPN-Fine-Tuning+              '(#x00 #x01))
(define +RPN-Coarse-Tuning+            '(#x00 #x02))
(define +RPN-Reset+	            '(#x3f #xff)) ; clear current RPN


;;; ======================================================================
;;; ======================================================================
;;;
;;; General MIDI Definitions
;;;

(define +gm-patch-strings+
  (make-vector 128 '""))

;;; drum sounds are defined from 35-81, so we shift them back -32 to 3-49

(define +gm-drum-kit-strings+
  (make-vector 50 '""))

(defmacro defgmpatch (sym val . str)
  `(deflabel ,sym ,val +gm-patch-strings+ 
    ,(if (null? str) #f (car str))
    ,val))

(defmacro defgmdrum (sym val . str)
  `(deflabel ,sym ,val +gm-drum-kit-strings+
    ,(if (null? str) #f (car str))
    ,(- val 32)))

;;;
;;; Utilities

(define (gm-patch-name patch)
  (midi-opcode-name +gm-patch-strings+ patch 0 127 0))

(define (gm-drum-kit-name key)
  (midi-opcode-name +gm-drum-kit-strings+ key 35 81 -32))

; (gm-patch-name 71)		=> "Clarinet"
; (gm-patch-name "unset")	=> "unset"
; (gm-drum-kit-name 60)		=> "Hi Bongo"

;;;
;;; General MIDI Instrument Patches

;;; Piano
(defgmpatch +Acoustic-Grand-Piano+	  0)
(defgmpatch +Bright-Acoustic-Piano+	  1)
(defgmpatch +Electric-Grand-Piano+	  2)
(defgmpatch +Honky-Tonk-Piano+		  3)
(defgmpatch +Electric-Piano-1+		  4)
(defgmpatch +Electric-Piano-2+		  5)
(defgmpatch +Harpsichord+		  6)
(defgmpatch +Clavi+			  7)

;;; Chromatic Percussion
(defgmpatch +Celesta+			  8)
(defgmpatch +Glockenspiel+		  9)
(defgmpatch +Music-Box+			 10)
(defgmpatch +Vibraphone+		 11)
(defgmpatch +Marimba+			 12)
(defgmpatch +Xylophone+			 13)
(defgmpatch +Tubular-Bells+		 14)
(defgmpatch +Dulcimer+			 15)

;;; Organ
(defgmpatch +Drawbar-Organ+		 16)
(defgmpatch +Percussive-Organ+		 17)
(defgmpatch +Rock-Organ+		 18)
(defgmpatch +Church-Organ+		 19)
(defgmpatch +Reed-Organ+		 20)
(defgmpatch +Accordion+			 21)
(defgmpatch +Harmonica+			 22)
(defgmpatch +Tango-Accordion+		 23)

;;; Guitar
(defgmpatch +Acoustic-Guitar-Nylon+	 24)
(defgmpatch +Acoustic-Guitar-Steel+	 25)
(defgmpatch +Electric-Guitar-Jazz+	 26)
(defgmpatch +Electric-Guitar-Clean+	 27)
(defgmpatch +Electric-Guitar-Muted+	 28)
(defgmpatch +Overdriven-Guitar+		 29)
(defgmpatch +Distortion-Guitar+		 30)
(defgmpatch +Guitar-Harmonics+		 31)

;;; Bass
(defgmpatch +Acoustic-Bass+		 32)
(defgmpatch +Electric-Bass-Finger+	 33)
(defgmpatch +Electric-Bass-Pick+	 34)
(defgmpatch +Fretless-Bass+		 35)
(defgmpatch +Slap-Bass-1+		 36)
(defgmpatch +Slap-Bass-2+		 37)
(defgmpatch +Synth-Bass-1+		 38)
(defgmpatch +Synth-Bass-2+		 39)

;;; Solo Strings
(defgmpatch +Violin+			 40)
(defgmpatch +Viola+			 41)
(defgmpatch +Cello+			 42)
(defgmpatch +Contrabass+		 43)
(defgmpatch +Tremolo-Strings+		 44)
(defgmpatch +Pizzicato-Strings+	 45)
(defgmpatch +Orchestral-Strings+	 46)
(defgmpatch +Timpani+			 47)

;;; Ensemble
(defgmpatch +String-Ensemble-1+	 48)
(defgmpatch +String-Ensemble-2+	 49)
(defgmpatch +Synthstrings-1+		 50)
(defgmpatch +Synthstrings-2+		 51)
(defgmpatch +Choir-Aahs+		 52)
(defgmpatch +Voice-Oohs+		 53)
(defgmpatch +Synth-Voice+		 54)
(defgmpatch +Orchestra-Hit+		 55)

;;; Brass
(defgmpatch +Trumpet+			 56)
(defgmpatch +Trombone+			 57)
(defgmpatch +Tuba+			 58)
(defgmpatch +Muted-Trumpet+		 59)
(defgmpatch +French-Horn+		 60)
(defgmpatch +Brass-Section+		 61)
(defgmpatch +Synthbrass-1+		 62)
(defgmpatch +Synthbrass-2+		 63)

;;; Reed
(defgmpatch +Soprano-Sax+		 64)
(defgmpatch +Alto-Sax+			 65)
(defgmpatch +Tenor-Sax+		 66)
(defgmpatch +Baritone-Sax+		 67)
(defgmpatch +Oboe+			 68)
(defgmpatch +English-Horn+		 69)
(defgmpatch +Bassoon+			 70)
(defgmpatch +Clarinet+			 71)

;;; Pipe
(defgmpatch +Piccolo+			 72)
(defgmpatch +Flute+			 73)
(defgmpatch +Recorder+			 74)
(defgmpatch +Pan-Flute+			 75)
(defgmpatch +Blown-Bottle+		 76)
(defgmpatch +Skakuhachi+		 77)
(defgmpatch +Whistle+			 78)
(defgmpatch +Ocarina+			 79)

;;; Synth banks are multiply defined for their nicknames.
;;; Only the default name is defgmpatch'ed

;;; Synth Lead
(defgmpatch +Lead-1-Square+		 80 "Lead 1 (Square)")
(define +Lead-1+			 80)
(define +Square-Lead+		 80)
(define +Square+			 80)
(defgmpatch +Lead-2-Sawtooth+		 81 "Lead 2 (Sawtooth)")
(define +Lead-2+			 81)
(define +Sawtooth-Lead+		 81)
(define +Sawtooth+			 81)
(defgmpatch +Lead-3-Calliope+		 82 "Lead 3 (Calliope)")
(define +Lead-3+			 82)
(define +Calliope-Lead+		 82)
(define +Calliope+			 82)
(defgmpatch +Lead-4-Chiff+		 83 "Lead 4 (Chiff)")
(define +Lead-4+			 83)
(define +Chiff-Lead+		 83)
(define +Chiff+			 83)
(defgmpatch +Lead-5-Charang+		 84 "Lead 5 (Charang)")
(define +Lead-5+			 84)
(define +Charang-Lead+		 84)
(define +Charang+			 84)
(defgmpatch +Lead-6-Voice+		 85 "Lead 6 (Voice)")
(define +Lead-6+			 85)
(define +Voice-Lead+		 85)
(define +Voice+			 85)
(defgmpatch +Lead-7-Fifths+		 86 "Lead 7 (Fifths)")
(define +Lead-7+			 86)
(define +Fifths-Lead+		 86)
(define +Fifths+			 86)
(defgmpatch +Lead-8-Bass+Lead+		 87 "Lead 8 (Bass+Lead)")
(define +Lead-8+			 87)
(define +Bass+Lead-Lead+		 87)
(define +Bass+Lead+		 87)

;;; Synth Pad
(defgmpatch +Pad-1-New-Age+		 88 "Pad 1 (New Age)")
(define +Pad-1+			 88)
(define +New-Age-Pad+		 88)
(define +New-Age+			 88)
(defgmpatch +Pad-2-Warm+		 89 "Pad 2 (Warm)")
(define +Pad-2+			 89)
(define +Warm-Pad+			 89)
(define +Warm+			 89)
(defgmpatch +Pad-3-Polysynth+		 90 "Pad 3 (Polysynth)")
(define +Pad-3+			 90)
(define +Polysynth-Pad+		 90)
(define +Polysynth+		 90)
(defgmpatch +Pad-4-Choir+		 91 "Pad 4 (Choir)")
(define +Pad-4+			 91)
(define +Choir-Pad+		 91)
(define +Choir+			 91)
(defgmpatch +Pad-5-Bowed+		 92 "Pad 5 (Bowed)")
(define +Pad-5+			 92)
(define +Bowed-Pad+		 92)
(define +Bowed+			 92)
(defgmpatch +Pad-6-Metallic+		 93 "Pad 6 (Metallic)")
(define +Pad-6+			 93)
(define +Metallic-Pad+		 93)
(define +Metallic+			 93)
(defgmpatch +Pad-7-Halo+		 94 "Pad 7 (Halo)")
(define +Pad-7+			 94)
(define +Halo-Pad+			 94)
(define +Halo+			 94)
(defgmpatch +Pad-8-Sweep+		 95 "Pad 8 (Sweep)")
(define +Pad-8+			 95)
(define +Sweep-Pad+		 95)
(define +Sweep+			 95)

;;; Synth Effects
(defgmpatch +Fx-1-Rain+			 96 "FX 1 (Rain)")
(define +Fx-1+			 96)
(define +Rain-Fx+			 96)
(define +Rain+			 96)
(defgmpatch +Fx-2-Soundtrack+		 97 "FX 2 (Soundtrack)")
(define +Fx-2+			 97)
(define +Soundtrack-Fx+		 97)
(define +Soundtrack+		 97)
(defgmpatch +Fx-3-Crystal+		 98 "FX 3 (Crystal)")
(define +Fx-3+			 98)
(define +Crystal-Fx+		 98)
(define +Crystal+			 98)
(defgmpatch +Fx-4-Atmosphere+		 99 "FX 4 (Atmosphere)")
(define +Fx-4+			 99)
(define +Atmosphere-Fx+		 99)
(define +Atmosphere+		 99)
(defgmpatch +Fx-5-Brightness+		100 "FX 5 (Brightness)")
(define +Fx-5+			100)
(define +Brightness-Fx+		100)
(define +Brightness+		100)
(defgmpatch +Fx-6-Goblins+		101 "FX 6 (Goblins)")
(define +Fx-6+			101)
(define +Goblins-Fx+		101)
(define +Goblins+			101)
(defgmpatch +Fx-7-Echoes+		102 "FX 7 (Echoes)")
(define +Fx-7+			102)
(define +Echoes-Fx+		102)
(define +Echoes+			102)
(defgmpatch +Fx-8-Sci-Fi+		103 "FX 8 (Sci-Fi)")
(define +Fx-8+			103)
(define +Sci-Fi-Fx+		103)
(define +Sci-Fi+			103)

;;; Ethnic
(defgmpatch +Sitar+			104)
(defgmpatch +Banjo+			105)
(defgmpatch +Shamisen+			106)
(defgmpatch +Koto+			107)
(defgmpatch +Kalimba+			108)
(defgmpatch +Bagpipe+			109)
(defgmpatch +Fiddle+			110)
(defgmpatch +Shanai+			111)

;;; Percussive
(defgmpatch +Tinkle-Bell+		112)
(defgmpatch +Agogo+			113)
(defgmpatch +Steel-Drums+		114)
(defgmpatch +Woodblock+			115)
(defgmpatch +Taiko-Drum+		116)
(defgmpatch +Melodic-Tom+		117)
(defgmpatch +Synth-Drum+		118)
(defgmpatch +Reverse-Cymbal+		119)

;;; Sound Effects
(defgmpatch +Guitar-Fret-Noise+		120)
(defgmpatch +Breath-Noise+		121)
(defgmpatch +Seashore+			122)
(defgmpatch +Bird-Tweet+		123)
(defgmpatch +Telephone-Ring+		124)
(defgmpatch +Helicopter+		125)
(defgmpatch +Applause+			126)
(defgmpatch +Gunshot+			127)

;;;
;;; General MIDI Drum Kit

;;; Drum sounds are by convention received on channel 9, but could be any
;;; sequence of logical channels.

(define *gm-percussion-channels*	#(9))

(define (gm-percussion-channel-p chan)
  (do ((i 0 (+ i 1))
       (f #f)
       (e (vector-length *gm-percussion-channels*)))
      ((or f (not (< i e))) f)
    (set! f (= (vector-ref *gm-percussion-channels* i)
	       chan))))

(defgmdrum +Acoustic-Bass-Drum+		 35)
(defgmdrum +Bass-Drum-1+		 36)
(defgmdrum +Side-Stick+			 37)
(defgmdrum +Acoustic-Snare+		 38)
(defgmdrum +Hand-Clap+			 39)
(defgmdrum +Electric-Snare+		 40)
(defgmdrum +Low-Floor-Tom+		 41)
(defgmdrum +Closed-Hi-Hat+		 42)
(defgmdrum +High-Floor-Tom+		 43)
(defgmdrum +Pedal-Hi-Hat+		 44)
(defgmdrum +Low-Tom+			 45)
(defgmdrum +Open-Hi-Hat+		 46)
(defgmdrum +Low-Mid-Tom+		 47)
(defgmdrum +Hi-Mid-Tom+			 48)
(defgmdrum +Crash-Cymbal-1+		 49)
(defgmdrum +High-Tom+			 50)
(defgmdrum +Ride-Cymbal-1+		 51)
(defgmdrum +Chinese-Cymbal+		 52)
(defgmdrum +Ride-Bell+			 53)
(defgmdrum +Tambourine+			 54)
(defgmdrum +Splash-Cymbal+		 55)
(defgmdrum +Cowbell+			 56)
(defgmdrum +Crash-Cymbal-2+		 57)
(defgmdrum +Vibraslap+			 58)
(defgmdrum +Ride-Cymbal-2+		 59)
(defgmdrum +Hi-Bongo+			 60)
(defgmdrum +Low-Bongo+			 61)
(defgmdrum +Mute-Hi-Conga+		 62)
(defgmdrum +Open-Hi-Conga+		 63)
(defgmdrum +Low-Conga+			 64)
(defgmdrum +High-Timbale+		 65)
(defgmdrum +Low-Timbale+		 66)
(defgmdrum +High-Agogo+			 67)
(defgmdrum +Low-Agogo+			 68)
(defgmdrum +Cabasa+			 69)
(defgmdrum +Maracas+			 70)
(defgmdrum +Short-Whistle+		 71)
(defgmdrum +Long-Whistle+		 72)
(defgmdrum +Short-Guiro+		 73)
(defgmdrum +Long-Guiro+			 74)
(defgmdrum +Claves+			 75)
(defgmdrum +Hi-Wood-Block+		 76)
(defgmdrum +Low-Wood-Block+		 77)
(defgmdrum +Mute-Cuica+			 78)
(defgmdrum +Open-Cuica+			 79)
(defgmdrum +Mute-Triangle+		 80)
(defgmdrum +Open-Triangle+		 81)

;;; ======================================================================
;;;
;;; Sysex Message Utilities
;;;


;;; 
;;; System Exclusive messages have the general form
;;;
;;;    F0 <Manufacturer-ID> <data> ... F7
;;;

;;;
;;; Manufacturer's IDs.  0 is reserved for multi-byte ID's.
;;; 
;;; Stolen from http://www.io.com/~jimm/midi_ref.html#Manufacturers with 
;;; some company designators omitted where it seemed reasonable.

;;; USA Manufacturers
(define +Sequential-Circuits-ID+	      #x01)
(define +IDP-ID+			      #x02)
(define +Voyetra-ID+		      #x03) ; Voyetra/Octave Plateau
(define +Moog-ID+			      #x04) ; Moog Music
(define +Passport-ID+		      #x05) ; Passport Designs
(define +Lexicon-ID+		      #x06)
(define +Kurzweil-ID+		      #x07)
(define +Fender-ID+		      #x08)
(define +Gulbransen-ID+		      #x09)
(define +AKG-ID+			      #x0A) ; AKG Acoustics
(define +Voyce-ID+			      #x0B) ; Voyce Music
(define +Waveframe-ID+		      #x0C)
(define +ADA-ID+			      #x0D)
(define +Garfield-ID+		      #x0E) ; Garfield Electronics
(define +Ensoniq-ID+		      #x0F)
(define +Oberheim-ID+		      #x10)
(define +Apple-ID+			      #x11) ; Apple Computer
(define +Grey-Matter-ID+		      #x12)
(define +Digidesign-ID+		      #x13)
(define +Palm-Tree-ID+		      #x14) ; Palm Tree Instruments
(define +JL-Cooper-ID+		      #x15)
(define +Lowrey-ID+		      #x16)
(define +Adams-Smith-ID+		      #x17)
(define +E-mu-ID+			      #x18) ; E-mu Systems
(define +Harmony-ID+		      #x19) ; Harmony Systems
(define +ART-ID+			      #x1A)
(define +Baldwin-ID+		      #x1B)
(define +Eventide-ID+		      #x1C)
(define +Inventronics-ID+		      #x1D)
(define +Key-Concepts-ID+		      #x1E)
(define +Clarity-ID+		      #x1F)

;;; Europe
(define +Passac-ID+		      #x20)
(define +SIEL-ID+			      #x21)
(define +Synthaxe-ID+		      #x22)
(define +Stepp-ID+			      #x23)
(define +Hohner-ID+		      #x24)
(define +Twister-ID+		      #x25)
(define +Solton-ID+		      #x26)
(define +Jellinghaus-ID+		      #x27)
(define +Southworth-ID+		      #x28)
(define +PPG-ID+			      #x29)
(define +JEN-ID+			      #x2A)
(define +Solid-State-ID+		      #x2B) ; Solid Stat Logic
(define +Audio-Vertrieb-ID+	      #x2C)
(define +Hinton-ID+		      #x2D) ; Hinton Instruments
(define +Soundtracs-ID+		      #x2E)
(define +Elka-ID+			      #x2F)
(define +Dynachord-ID+		      #x30)
(define +Clavia-ID+		      #x33) ; Clavia Digital Instr.
(define +Audio-Architecture-ID+	      #x34)
(define +Soundcraft-ID+		      #x39) ; Soundcraft Electronics
(define +Wersi-ID+			      #x3B)
(define +Avab-ID+			      #x3C) ; Avab Electronik
(define +Digigram-ID+		      #x3D)
(define +Waldorf-ID+		      #x3E) ; Waldorf Electronics
(define +Quasimidi-ID+		      #x3F)

;;; Japan
(define +Kawai-ID+			      #x40)
(define +Roland-ID+		      #x41)
(define +Korg-ID+			      #x42)
(define +Yamaha-ID+		      #x43)
(define +Casio-ID+			      #x44)
(define +Moridaira-ID+		      #x45)
(define +Kamiya-ID+		      #x46)
(define +Akai-ID+			      #x47)
(define +Japan-Victor-ID+		      #x48)
(define +Meisosha-ID+		      #x49)
(define +Hoshino-Gakki-ID+		      #x4A)
(define +Fujitsu-ID+		      #x4B)
(define +Sony-ID+			      #x4C)
(define +Nishin-Onpa-ID+		      #x4D)
(define +TEAC-ID+			      #x4E)
(define +Matsushita-Electric-ID+	      #x50)
(define +Fostex-ID+		      #x51)
(define +Zoom-ID+			      #x52)
(define +Midori-ID+		      #x53) ; Midori Electronics
(define +Matsushita-Communication-ID+    #x54)
(define +Suzuki-ID+		      #x55)

;;; USA Extended
(define +Warner-ID+	     '(0 #x00 #x01)) ; Warner New Media
(define +Digital-Music-ID+ '(0 #x00 #x07)) ; Digital Music Corp.
(define +IOTA-ID+	     '(0 #x00 #x08)) ; IOTA Systems
(define +New-England-ID+   '(0 #x00 #x09)) ; New England Digital
(define +Artisyn-ID+	     '(0 #x00 #x0A))
(define +IVL-ID+	     '(0 #x00 #x0B)) ; IVL Technologies
(define +Southern-Music-ID+     '(0 #x00 #x0C)) ; Southern Music Systems
(define +Lake-Butler-ID+	     '(0 #x00 #x0D)) ; Lake Butler Sound Co.
(define +Alesis-ID+	     '(0 #x00 #x0E))
(define +DOD-ID+		     '(0 #x00 #x10)) ; DOD Electronics
(define +Studer-ID+	     '(0 #x00 #x11)) ; Studer Editech
(define +Perfect-Fretworks-ID+  '(0 #x00 #x14))
(define +KAT-ID+		     '(0 #x00 #x15))
(define +Opcode-ID+	     '(0 #x00 #x16))
(define +Rane-ID+		     '(0 #x00 #x17)) ; Rane Corporation
(define +Spatial-Sound-ID+	     '(0 #x00 #x18)) ; Spatial Sound/Anadi Inc.
(define +KMX-ID+		     '(0 #x00 #x19))
(define +Allen-&-Heath-ID+	     '(0 #x00 #x1A)) ; Allen & Heath Brenell
(define +Peavey-ID+	     '(0 #x00 #x1B)) ; Peavey Electronics
(define +360-ID+		     '(0 #x00 #x1C)) ; 360 Systems
(define +Spectrum-ID+	     '(0 #x00 #x1D)) ; Spectrum Design & Dev.
(define +Marquis-Musi-ID+	     '(0 #x00 #x1E))
(define +Zeta-ID+		     '(0 #x00 #x1F)) ; Zeta Systems
(define +Axxes-ID+		     '(0 #x00 #x20))
(define +Orban-ID+		     '(0 #x00 #x21))
(define +KTI-ID+		     '(0 #x00 #x24))
(define +Breakaway-ID+	     '(0 #x00 #x25)) ; Breakaway Technologies
(define +CAE-ID+		     '(0 #x00 #x26))
(define +Rocktron-ID+	     '(0 #x00 #x29)) ; Rocktron Corp.
(define +PianoDisc-ID+	     '(0 #x00 #x2A))
(define +Cannon-ID+	     '(0 #x00 #x2B)) ; Cannon Research Corp.
(define +Rogers-ID+	     '(0 #x00 #x2D)) ; Rogers Instrument Corp.
(define +Blue-Sky-ID+	     '(0 #x00 #x2E)) ; Blue Sky Logic
(define +Encore-ID+	     '(0 #x00 #x2F)) ; Encore Electronics
(define +Uptown-ID+	     '(0 #x00 #x30))
(define +Voce-ID+		     '(0 #x00 #x31))
(define +CTI-ID+		     '(0 #x00 #x32)) ; CTI Audio
(define +S&S-ID+		     '(0 #x00 #x33)) ; S&S Research
(define +Broderbund-ID+	     '(0 #x00 #x34)) ; Broderbund Software
(define +Allen-Organ-ID+	     '(0 #x00 #x35)) ; Allen Organ Co.
(define +Music-Quest-ID+	     '(0 #x00 #x37))
(define +Aphex-ID+		     '(0 #x00 #x38))
(define +Gallien-Krueger-ID+    '(0 #x00 #x39))
(define +IBM-ID+		     '(0 #x00 #x3A))
(define +Hotz-ID+		     '(0 #x00 #x3C)) ; Hotz Instruments Techn.
(define +ETA-ID+		     '(0 #x00 #x3D)) ; ETA Lighting
(define +NSI-ID+		     '(0 #x00 #x3E)) ; NSI Corporation
(define +Ad-Lib-ID+	     '(0 #x00 #x3F))
(define +Richmond-ID+	     '(0 #x00 #x40)) ; Richmond Sound Design
(define +Microsoft-ID+	     '(0 #x00 #x41))
(define +Software-Toolworks-ID+ '(0 #x00 #x42)) ; The Software Toolworks
(define +RJMG/Niche-ID+	     '(0 #x00 #x43))
(define +Intone-ID+	     '(0 #x00 #x44))
(define +GT-Electronics-ID+     '(0 #x00 #x47)) ; GT Electr./Groove Tubes
(define +InterMIDI-ID+	     '(0 #x00 #x48))
(define +Lone-Wolf-ID+	     '(0 #x00 #x55))
(define +Musonix-ID+	     '(0 #x00 #x64))

(define +SGI-ID+		     '(0 #x01 #x04)) ; Silicon Graphics, Inc.

;;; Europe Extended
(define +Dream-ID+		     '(0 #x20 #x00))
(define +Strand-ID+	     '(0 #x20 #x00)) ; Strand Lighting
(define +AMEK-ID+            '(0 #x20 #x00)) ; AMEK Systems & Controls
(define +Dr.Boehm-ID+	     '(0 #x20 #x00)) ; Dr Boehm/Musician Int'l
(define +Trident-ID+	     '(0 #x20 #x00))
(define +Real-World-ID+	     '(0 #x20 #x00)) ; Real World Design
(define +Yes-ID+		     '(0 #x20 #x00)) ; Yes Technology
(define +Audiomatica-ID+	     '(0 #x20 #x00))
(define +Bontempi-ID+	     '(0 #x20 #x00)) ; Bontempi/Farfisa
(define +FBT-ID+		     '(0 #x20 #x00)) ; F.B.T. Electronica
(define +Larking-ID+	     '(0 #x20 #x00)) ; Larking Audio
(define +Zero-88-ID+	     '(0 #x20 #x00)) ; Zero 88 Lighting
(define +Micon-ID+		     '(0 #x20 #x10)) ; Micon Audio Electronics
(define +Forefront-ID+	     '(0 #x20 #x10)) ; Forefront Technology
(define +Kenton-ID+	     '(0 #x20 #x10)) ; Kenton Electronics
(define +ADB-ID+		     '(0 #x20 #x10))
(define +Marshall-ID+	     '(0 #x20 #x10)) ; Jim Marshall Products
(define +DDA-ID+		     '(0 #x20 #x10))
(define +TC-ID+		     '(0 #x20 #x10)) ; TC Electronic


;;;
;;; Universal (Manufacturer-Independent) ID's

;;;
;;; Private-use sysex messages may use the Non-Commercial ID at own risk.

(define +Non-Commercial-ID+	      #x7D) ; aka "Eductaional Use"

;;;
;;; Sysex messages of interest for various manufacturer-independent devices
;;; may make use of the Real-Time and Non-Real-Time ID's.  The general
;;; syntax of such messages is 
;;;
;;;   F0  <ID>  <Device-ID>  <Sub-ID-1>  <Sub-ID-2>  <data>  ...  F7
;;;
;;; <Device-ID> is a channel number between 0 and 7F.  7F denotes a
;;; "global" universal sysex message.
;;;
;;; NOTE: The base Device-ID may or may not be independent from the
;;; device's (or parts) Base Channel.

(define +Non-Real-Time-ID+		      #x7E)

;;; Non-Real-Time Universal Sysex Message Sub-IDs
(define +Sample-Dump-Header-Sub-ID+		#x01)
(define +Sample-Dump-Packet-Sub-ID+		#x02)
(define +Dump-Request-Sub-ID+			#x03)
(define +MIDI-Time-Code-Setup-Sub-ID+		#x04)
(define +Sample-Dump-Extensions-Sub-ID+		#x05)
(define +Inquiry-Message-Sub-ID+			#x06)
(define +File-Dump-Sub-ID+				#x07)
(define +MIDI-Tuning-Standard-Sub-ID+		#x08)
(define +General-MIDI-Message-Sub-ID+		#x09)
(define +End-of-File-Sub-ID+			#x7B)
(define +Wait-Sub-ID+				#x7C)
(define +Cancel-Sub-ID+				#x7D)
(define +NAK-Sub-ID+				#x7E)
(define +Ack-Sub-ID+				#x7F)

(define +Real-Time-ID+		      #x7F)

;;; Real-Time Universal Sysex Message Sub-IDs
(define +Long-Form-MTC-Sub-ID+			#x01)
(define +MIDI-Show-Control-Sub-ID+			#x02)
(define +Notation-Information-Sub-ID+		#x03)
(define +Device-Control-Sub-ID+			#x04)
(define +Real-Time-MTC-Cueing-Sub-ID+		#x05)
(define +MIDI-Machine-Control-Command-Sub-ID+	#x06)
(define +MIDI-Machine-Control-Response-Sub-ID+	#x07)
(define +Single-Note-Retune-Sub-ID+		#x08)


;;;
;;; Conversion Utilities

(define (n-bit-twoscomp-p num bits signed? . args)
  (let ((error? (if (null? args) #f (car args))))
    (if signed?
      (if (and (< num 0) 
	       (<= (integer-length num) bits))
	#t
	(if error?
	  (err "Not a signed ~s-bit byte: ~s."
               bits num)
	  #f))
      (if (and (not (< num 0))
	       (<= (integer-length num) bits))
	#t
	(if error?
	  (err "Not an unsigned ~s-bit byte: ~s."
               bits num)
	  #f)))))

(define (n-bit-twoscomp num bits signed?)
  (n-bit-twoscomp-p num bits signed? #t)
  (if (< num 0)
      (+ num (expt 2 bits))
    num))

(define (n-bit-bytes num bytes bits lsb-first?)
  (n-bit-twoscomp-p num (* bytes bits) (< num 0) #t)
  (let ((l '()))
    (do ((i 0 (+ i 1)))
	((not (< i bytes)) #f)
      (set! l (cons (ldb (byte bits (* i bits)) num)
		    l)))
    (if lsb-first?
      (reverse l)
      l)))

;;; turn a sequence of bytes into a list of nibbles, either lsb-first or
;;; msb-first.  data may be either bytes or chars or strings or lists of
;;; these. 

(define (nibblize lsb-first? . data)
  (let ((res '()) 
	(o1 (if lsb-first? 0 4))
	(o2 (if lsb-first? 4 0)))
    (letrec ((nblize
	      (lambda (x)
		(cond ((not x) #f)
		      ((null? x) #f)
		      ((vector? x)
		       (do ((i 0 (+ i 1))
			    (e (vector-length x)))
			   ((not (< i e)) #f)
			 (nblize (vector-ref x i))))
		      ((string? x)
		       (do ((i 0 (+ i 1))
			    (e (string-length x)))
			   ((not (< i e)) #f)
			 (nblize (string-ref x i)))
		       (nblize 0))
		      ((pair? x)
		       (do ((i 0 (+ i 1))
			    (e (length x)))
			   ((not (< i e)) #f)
			 (nblize (list-ref x i))))
		      ((char? x)
		       (set! x (char->integer x))
		       (push (ldb (byte 4 o1) x) res)
		       (push (ldb (byte 4 o2) x) res))
		      ((exact? x)
		       (push (ldb (byte 4 o1) x) res)
		       (push (ldb (byte 4 o2) x) res))
		      (else
		       (err "Can't nibbilize ~s." x))))))
      (nblize data)
      (reverse res))))

; (use-modules (ice-9 format))
; (format t "#x~x" (n-bit-twoscomp #x3fff 14 #f))	=> #x3fff
; (format t "#x~x" (n-bit-twoscomp #x4000 14 #f))	=> Error
; (format t "#x~x" (n-bit-twoscomp #x-1 14 #f))		=> Error
; (format t "#x~x" (n-bit-twoscomp #x1fff 14 #t))		=> #x1fff
; (format t "#x~x" (n-bit-twoscomp #x2000 14 #t))		=> Error
; (format t "#x~x" (n-bit-twoscomp #x-2000 14 #t))		=> #x2000
; (format t "#x~x" (n-bit-twoscomp #x-1 14 #t))		=> #x3fff

; (n-bit-bytes #x3F80 2 7 t)		=> (0 127)
; (n-bit-bytes #xFF00 2 8 #f)		=> (255 0)
; (n-bit-bytes #x-1 2 3 #f)		=> (7 7)
; (n-bit-bytes #b10010110 8 1 #f)	=> (1 0 0 1 0 1 1 0)

; (nibblize #f "abc")
;    => (6 1 6 2 6 3  0 0) ; = #x61 #x62 #x63 #x00
; (nibblize t 1 2 3 4) 
;   => (1 0 2 0 3 0 4 0)
; (nibblize #f "abc" #f '(#x12 #x34 #(#x56 #x78) #x90))
;   => (6 1 6 2 6 3  0 0 1 2 3 4 5 6 7 8 9 0)
;       --- --- ---  ---
;       'a' 'b' 'c' '\0'


;;;
;;; For instance, to enable or disable the General MIDI System for all
;;; devices on a MIDI bus, one would send the sysex message
;;;
;;;   F0 7E 7F 09 xx F7
;;;
;;; where <xx> is either 0 for disable or 1 for enable.  Note, too, that
;;; the <Device-ID> is set to 7F, indicating a "global" message to 
;;; be received by all devices.

(define +All-Device-IDs+ #x7F)

(define (make-GM-mode-sysex-data gm-on? . args)
  (with-args (args &key (device-ID +All-Device-IDs+))
    (make-sysex-data +Non-Real-Time-ID+
		     device-ID
		     +General-MIDI-Message-Sub-ID+
		     (if gm-on? 1 0))))

; (make-GM-mode-sysex-data t)
;   => #(240 126 127 9 1 247)
; (make-GM-mode-sysex-data #f)
;   => #(240 126 127 9 0 247)

;;;
;;; Similarly, the device's master volume may be set using the following
;;; Real-Time sysex message:
;;;
;;;   F0 7F 7F 04 01 ll mm F7
;;;
;;; where <ll> and <mm> are the least-significant and most-significant 7
;;; bits of a 14-bit volume.  Again, <Device-ID> is set to 7F,
;;; indicating a "global" message to be received by all devices.

(define +Master-Volume-Sub-ID-2+ #x01)

(define (make-master-volume-sysex-data . args)
  (with-args (args &key coarse fine
                   (device-ID +All-Device-IDs+))
    (unless (or coarse fine)
      (err "No volume specified."))
    (unless (or (and coarse (n-bit-twoscomp-p coarse 7 #f))
	        (and fine (n-bit-twoscomp-p fine 14 #f)))
      
      (err "~a volume ~a is not a ~a-bit value."
           (if fine "Fine" "Course")
           (or coarse fine)
           (if fine "14" "7")))
    (make-sysex-data +Real-Time-ID+
		     device-ID
		     +Device-Control-Sub-ID+
		     +Master-Volume-Sub-ID-2+
		     (if fine (logand fine #b1111111) coarse)
		     (if fine (ash (logand fine #b11111110000000) -7) 
                         coarse))))

; (make-master-volume-sysex-data :coarse 127)
;   => #(240 127 127 4 1 127 127 247)	; |127|127| = "1111111 1111111"
; (make-master-volume-sysex-data :coarse 5)
;   => #(240 127 127 4 1 5 5 247)		; |5|5|     = "0000101 0000101"
; (make-master-volume-sysex-data :fine #x3fff)
;   => #(240 127 127 4 1 127 127 247)	; |127|127| = "1111111 1111111"

;;;
;;; Time-Code-Related Sysex Messages
;;;

;;;
;;; SMPTE Format
;;; 
;;; In each frame, 26 of the 80 bits carry the SMPTE time or 'address', in
;;; binary coded decimal.  32 bits are assigned as 8 groups of 4 USER BITS. 
;;; This capacity is generally used to carry extra info such as reel number
;;; and date.  Bits 43 and 59 are assigned as the Binary Group Flag Bits
;;; BGFB and are used to indicate when a standard character set is used to
;;; format the User Bits data. The Binary Group Flag Bits should be used
;;; only as shown in the truth table below. The Unassigned entries in the
;;; table should not be used, as they may be allocated specific meanings in
;;; the future.
;;;                                   Bit 43  Bit 59
;;;    No User Bits format specified      0       0
;;;    Eight-bit character set            1       0
;;;    Unassigned (Reserved)              0       1
;;;    Unassigned (Reserved)              1       1 
;;;
;;; The last sixteen Bits make up the SYNC WORD. A timecode reader uses
;;; these Bits to find the frame boundary, the tape direction, and the
;;; bit-rate of the sync tone. The values of these Bits are fixed as 0011
;;; 1111 1111 1101. 
;;;
;;;
;;;                         7         6         5         4
;;;    Bits 40-79: 9876543210987654321098765432109876543210
;;;                ----------------                           Sync Word
;;;                                ----                       User Bits 8
;;;                                    -                      BGFB
;;;                                     -                     <Reserved>
;;;                                      --                   Hours Tens
;;;                                        ----               User Bits 7
;;;                                            ----           Hours Ones
;;;                                                ----       User Bits 6
;;;                                                    -      BGFB
;;;                                                     ---   Minutes Tens
;;;                         3         2         1         0
;;;    Bits  0-39: 9876543210987654321098765432109876543210
;;;                ----                                       User Bits 5
;;;                    ----                                   Minutes Ones
;;;                        ----                               User Bits 4
;;;                            -                              Bi-Phase Corr.
;;;                             ---                           Seconds Tens
;;;                                ----                       User Bits 3
;;;                                    ----                   Seconds Ones
;;;                                        ----               User Bits 2
;;;                                            -              Drop-Frame Flag
;;;                                             -             Color Frame Flag
;;;                                              --           Frames Tens
;;;                                                ----       User Bits 1
;;;                                                    ----   Frames Ones

;;;
;;; SMPTE Full Frame Message
;;;
;;; The Full Frame simply cues a slave to a particular SMPTE time. The slave
;;; doesn't actually start running until it starts receiving Quarter Frame
;;; messages. (Which implies that a slave is stopped whenever it is not
;;; receiving Quarter Frame messages). The master should pause after sending
;;; a Full Frame, and before sending a Quarter Frame, in order to give the
;;; slave time to cue to the desired SMPTE time. 
;;;
;;; Syntax:
;;;
;;;   F0 +Real-Time-ID+ <Device-ID> 01 01 hr mn sc fr F7
;;; 
;;; The hr, mn, sc, and fr are the hours, minutes, seconds, and frames of
;;; the current SMPTE time. The hours byte also contains the SMPTE Type as
;;; per the Quarter Frame's Hours High Nibble message: 
;;; 
;;;   0 = 24 fps
;;;   1 = 25 fps
;;;   2 = 30 fps (Drop-Frame)
;;;   3 = 30 fps

(define +SMPTE-Full-Frame-Sub-ID-2+ #x01)
(define +SMPTE-User-Bits-Sub-ID-2+  #x02)


(define +SMPTE-Format-24fps+	 0)
(define +SMPTE-Format-25fps+	 1)
(define +SMPTE-Format-30fps-drop+	 2)
(define +SMPTE-Format-30fps+	 3)

(define (encode-SMPTE-data hr mn sc fr . args)
  (with-args (args &key subframes format)
    (unless (and (<= 0 hr 23) 
                 (<= 0 mn 59)
                 (<= 0 sc 59)
	         (<= 0 fr
                     (cond ((= format +SMPTE-Format-24fps+) 23)
                           ((= format +SMPTE-Format-25fps+) 24)
                           ((or (= format +SMPTE-Format-30fps-drop+)
                                (= format +SMPTE-Format-30fps+)) 29)
                           (else (err "Not a valid SMPTE format ID: ~a."
                                      format))))
	         (or (not subframes)
		     (<= 0 subframes 99)))
      (err "SMPTE values out of range: ~a ~a ~a ~a ~s."
           hr mn sc fr subframes))
    (set! hr (dpb format (byte 3 5) hr))
    (if subframes
      (list hr mn sc fr subframes)
      (list hr mn sc fr))))

; (encode-SMPTE-data 4 5 6 7 :format 3)
;   => (100 5 6 7)			; 100 = #b11 00100 = format 3, hour 4
; (encode-SMPTE-data 4 5 6 7 :format 0 :subframes 45)
;   => (4 5 6 7 45)
; (encode-SMPTE-data 23 59 59 29 :format +SMPTE-Format-30fps+)
;   => (119 59 59 29)			; 119 = #b11 10111 = format 3, hour 23

(define (make-SMPTE-full-frame-sysex-data hr mn sc fr . args)
  (with-args (args &key (format +SMPTE-Format-30fps+)
                   (device-ID +All-Device-IDs+))
    (make-sysex-data +Real-Time-ID+ device-ID
		     +Long-Form-MTC-Sub-ID+ +SMPTE-Full-Frame-Sub-ID-2+
		     (encode-SMPTE-data hr mn sc fr :format format))))

; (make-SMPTE-full-frame-sysex-data 1 2 3 4)
;   => #(240 127 127 1 1 97 2 3 4 247)	; 97 = #b11 00001 = format 3, hour 1

;;;
;;; SMPTE also provides for 32 "user bits", information for special
;;; functions which vary with each product.  Up to 4 characters or 8 digits
;;; can be written. Examples of use are adding a date code or reel number to
;;; a tape. The user bits tend not to change throughout a run of time code,
;;; so rather than stuffing this information into a Quarter Frame, MTC
;;; provides a separate SysEx message to transmit this info.
;;;
;;;   F0 7F cc 01 02 u1 u2 u3 u4 u5 u6 u7 u8 u9 F7
;;;
;;; cc is the SysEx channel (0 to 127). Only the low nibble of each of the
;;; first 8 data bytes is used. Only the 2 low bits of u9 is used.  These
;;; nibbles decode into an 8-bit format of aaaabbbb ccccdddd eeeeffff
;;; gggghhhh ii. It forms 4 8-bit characters, and a 2 bit Format Code. u1
;;; through u8 correspond to the SMPTE Binary Groups 1 through 8. u9 are the
;;; 2 Binary Group Flag Bits, defined by SMPTE.

(define +SMPTE-User-Bits-Raw+   #b00)
(define +SMPTE-User-Bits-Chars+ #b10)

(define (make-SMPTE-user-bits-sysex-data format data . args)
  (with-args (args &key (device-ID +All-Device-IDs+))
    (let* ((fmt (case format
                  ((:raw ) 0)
                  ((:bytes ) 1)
                  ((:chars ) 2)
                  ((:string ) 3)
                  (else
                   (err ":format not one of :raw :bytes :chars :string"))))
	   (size (if (= fmt 0) 4 8))
           (len #f)
           (ref #f))
      
      (cond ((string? data)
             (set! len (string-length data))
             (set! ref #'string-ref))
            ((vector? data)
             (set! len (vector-length data))
             (set! ref #'vector-ref))
            ((list? data)
             (set! len (length len))
             (set! ref #'list-ref)))
      
      (unless (= len (if (= size 4) 8 4))
        (err "~a format requires ~d data bytes, got: ~s."
             format 
             (if (= size 4) 8 4)
             data))
      (unless (case fmt
	        ((0 1)
                 (do ((f #t)
                      (i 0 (+ i 1))
                      (z #f))
                     ((or (not (< i len)) (not f)) f)
                   (set! z ( ref data))
                   (set! f (and (exact? z) (<= 0 z size))))) 
                ((2  )
                 (do ((f #t)
                      (i 0 (+ i 1)))
                     ((or (not (< i len)) (not f)) f)
                   (set! f (char? ( ref data) ))))
                ((3 )
                 (string? data)))
        (err "Not valid ~d-bit data: ~s" size data))
      (let ((data 
             (lambda (n)
               (if (= fmt 0)
                 ( ref data n)
                 (let ((d ( ref data (inexact->exact (floor n 2)))))
                   (when (>= fmt 2)
                     (set! d (char->integer d)))
                   (if (even? n)
                     (ash (logand d #b11110000) -4)
                     (logand d #b1111)))))))
        (make-sysex-data +Real-Time-ID+
		         device-ID
		         +Long-Form-MTC-Sub-ID+
		         +SMPTE-User-Bits-Sub-ID-2+
		         (data 0)
                         (data 1)
                         (data 2)
                         (data 3)
		         (data 4)
                         (data 5)
                         (data 6)
                         (data 7)
		         (if (= fmt 2)
			   +SMPTE-User-Bits-Chars+
			   +SMPTE-User-Bits-Raw+))))))



; (make-SMPTE-user-bits-sysex-data :raw '(1 2 3 4 5 6 7 8))
;   => #(240 127 127 1 2 1 2 3 4 5 6 7 8 0 247) ; all in nibbles
; (make-SMPTE-user-bits-sysex-data :bytes '(128 129 130 131))
;   => #(240 127 127 1 2 8 0 8 1 8 2 8 3 0 247) ; #x80 #x81 #x82 #x83, fmt=0
; (make-SMPTE-user-bits-sysex-data :chars '(#\T #\E #\S #\T))
;   => #(240 127 127 1 2 5 4 4 5 5 3 5 4 2 247) ; #x54455354 = :|TEST|, fmt=2
; (make-SMPTE-user-bits-sysex-data :string "TEST")
;   => #(240 127 127 1 2 5 4 4 5 5 3 5 4 2 247) ; as above

;;; 
;;; Notation Information
;;;

;;; The Bar Marker message indicates the start of a musical measure. It
;;; could also be used to setup and mark off bars of an introductory "count
;;; down". 
;;;
;;;   F0 7F cc 03 01 lb mb F7
;;; 
;;; cc is the SysEx channel (0 to 127).  lb mb is the desired bar number as
;;; a signed 14-bit value. Zero and negative numbers up to -8,190 indicate
;;; count off measures. For example, a value of -1 (ie, lb mb = 7F 7F) means
;;; that there is a one measure introduction. A value of zero would indicate
;;; no count off. Positive values indicate measures of the piece. The first
;;; measure is bar 1 (ie, lb mb = 01 00). A maximum neg number (lb mb = 00
;;; 40) indicates "stopped play" condition. A maximum positive value (lb mb
;;; = 7E 3F) indicates running condition,but no idea about measure
;;; number. This would be used by a device wishing to mark the passage of
;;; measures without keeping track of the actual measure number.

(define +Bar-Marker-Sub-ID-2+ #x01)

(define (make-measure-number-sysex-data num . args)
  (with-args (args &key (countoff #f)
                   (device-ID +All-Device-IDs+))
    (when countoff 
      (set! num (- (abs num))))		; ensure num is negative
    ;; convert to 14-bit two's-complement
    (set! num (n-bit-twoscomp num 14 t))
    (make-sysex-data +Real-Time-ID+
		     device-ID
		     +Notation-Information-Sub-ID+
		     +Bar-Marker-Sub-ID-2+
		     (ldb (byte 7 0) num)
		     (ldb (byte 7 7) num))))

; (make-measure-number-sysex-data #x1FFF)
;   => #(240 127 127 3 1 127 63 247)	; |63|127| = 0111111 1111111
; (make-measure-number-sysex-data #x-2000)
;   => #(240 127 127 3 1 0 64 247)	; |64|0|   = 1000000 0000000
; (make-measure-number-sysex-data #x-1FFF)
;   => #(240 127 127 3 1 1 64 247)	; |64|1|   = 1000000 0000001

;;;
;;; Time Signature
;;; 
;;;   F0 7F <Device-ID> 03 <Sub-ID2> ln nn dd qq [nn dd...] F7
;;; 
;;; ln is the number of data bytes following this field (3 if not a compound
;;; time signature).  nn dd are the numerator and denominator, defined as in 
;;; MIDI file time signatures.  Similarly, qq is the number of notated 32nd
;;; notes in a MIDI quarter note.  Additional pairs of nn/dd's are for 
;;; compound time signatures only.

(define +Time-Signature-Now-Sub-ID-2+          #x02)
(define +Time-Signature-Next-Measure-Sub-ID-2+ #x42)

;;; compound meters:
;;; 
;;;   3+3+2                   3    3
;;;   -----  as (3 3 2) 8,    - + --  as (3 3) (4 16), etc. 
;;;     8                     4   16

(define (make-time-signature-sysex-data numerators denominators . args)
  (with-args (args &key (32nds 8)
                   (defer #f)
                   (device-ID +All-Device-IDs+))
    ;; i should fix this for vectors...
    (unless (list? numerators)
      (set! numerators (list numerators)))
    (unless (list? denominators)
      (set! denominators (list denominators)))
    (let* ((len (max (length numerators) (length denominators)))
	   (args (loop with f and r
		       for i from 0
		       for n = (or (list-ref numerators i) n)
		       for d = (or (list-ref denominators i) d)
		       repeat len
		       do (multiple-value-setq (f r) 
                            (floor (log2 d)))
		       collect n
		       collect (if (not (zero? r))
				 (err "Not a power of 2: ~s" d)
			         f))))
      (make-sysex-data +Real-Time-ID+
		       device-ID
		       +Notation-Information-Sub-ID+
		       (if defer
                         +Time-Signature-Next-Measure-Sub-ID-2+
			 +Time-Signature-Now-Sub-ID-2+)
		       (1+ (* 2 len))
		       (first args) (second args) 32nds
		       (cdr (cdr args))))))

; (make-time-signature-sysex-data 3 4 :defer t)
;   => #(240 127 127 3 66 3 3 2 8 247)
; (make-time-signature-sysex-data 3 '(2 4)) 
;   => #(240 127 127 3  2 5 3 1 8 3 2 247)
; (make-time-signature-sysex-data '(3 3 2) 8) 
;   => #(240 127 127 3  2 7 3 3 8 3 3 2 3 247)
; (make-time-signature-sysex-data '(3 3) '(4 16))
;   => #(240 127 127 3  2 5 3 2 8 3 4 247)

;;;
;;; MTC Setup Messages
;;; 
;;; The Setup message can be used to implement one of 19
;;; defined "events". A master device uses this message to tell slave units
;;; what "events" to perform, and when to perform those events.
;;;
;;;   F0 7E <Device-ID> 04 <Sub-ID-2> hr mn sc fr ff sl sm ... F7
;;; 
;;; hr mn sc fr ff is the SMPTE time when the event is to occur. This is
;;; just like the Full Frame message,except that there is also a fractional
;;; frame parameter, ff, which is 1/100 of a frame (ie, a value from 0 to
;;; 99).  Again, hr contains the time code format in bits 4-5.  sl sm is this
;;; event's 14-bit Event Number.  id tells what this Event Type is. 
;;;
;;; The extended (`+Setup-Xtnd-*') messages have additional information
;;; consisting of a nibblized MIDI data stream, LS nibble first.  The
;;; exception is Set-Up Type OE, where the additional information is
;;; nibblized ASCII, LS nibble first.  An ASCII newline is accomplished by
;;; sending CR and LF in the ASCII. CR alone functions solely as a carriage
;;; return, and LF alone functions solely as a Line-Feed.
;;;
;;; For example, a MIDI Note On message such as 91 46 7F would be nibblized
;;; and sent as 01 09 06 04 0F 07.  In this way, any device can decode any
;;; message regardless of who it was intended for.  Device-specific messages
;;; should be sent as nibblized MIDI System Exclusive messages.

(define +Setup-Special-Sub-ID-2+                #x00)
(define +Setup-Punch-In-Point-Sub-ID-2+         #x01)
(define +Setup-Punch-Out-Point-Sub-ID-2+        #x02)
(define +Setup-Delete-Punch-In-Point-Sub-ID-2+  #x03)
(define +Setup-Delete-Punch-Out-Point-Sub-ID-2+	#x04)
(define +Setup-Event-Start-Point-Sub-ID-2+      #x05)
(define +Setup-Event-Stop-Point-Sub-ID-2+       #x06)
(define +Setup-Xtnd-Event-Start-Point-Sub-ID-2+ #x07)
(define +Setup-Xtnd-Event-Stop-Point-Sub-ID-2+  #x08)
(define +Setup-Delete-Event-Start-Point-Sub-ID-2+ #x09)
(define +Setup-Delete-Event-Stop-Point-Sub-ID-2+ #x0A)
(define +Setup-Cue-Point-Sub-ID-2+              #x0B)
(define +Setup-Xtnd-Cue-Point-Sub-ID-2+         #x0C)
(define +Setup-Delete-Cue-Point-Sub-ID-2+       #x0D)
(define +Setup-Event-Name-Sub-ID-2+		#x0E)

(define (%make-setup-data dev subid2 fmt hr mn sc fr ff num . args)
  (with-args (args &optional xtnd)
    (make-sysex-data +Non-Real-Time-ID+ 
                     dev
                     +MIDI-Time-Code-Setup-Sub-ID+ 
                     subid2
		     (encode-SMPTE-data hr mn sc fr :subframes ff :format fmt)
		     (cond ((and (list? num) (= (length num) 2))
                            (n-bit-twoscomp-p (car num) 7
                                              (< (car num) 0) #t)
                            (n-bit-twoscomp-p (cadr num) 7 
                                              (< (cadr num) 0) #t)
                            num)
                           ((and (vector? num) (= (vector-length num) 2))
                            (n-bit-twoscomp-p (vector-ref num 0) 7 
                                              (< (vector-ref num 0) 0)
                                              #t)
                            (n-bit-twoscomp-p (vector-ref num 1) 7 
                                              (< (vector-ref num 1) 0)
                                              #t)
                            num)
                           (else
                            (n-bit-bytes num 2 7 #t)))
		     (nibblize t xtnd))))

;;; Special Setup messages
;;; 
;;; Special refers to the set-up information that affects a unit globally
;;; (as opposed to individual tracks, sounds, programs,sequences, etc.). In
;;; this case, the Special Type takes the place of the Event Number. Six
;;; are defined.  Note that types 01 00 through 03 00 ignore the event time
;;; field. (These bytes are already in LSB-first order.)
;;;
;;; NOTE: The standard proposal states "Five are defined" and that "types 01 
;;;       00 through 04 00 ignore the event time".  I reckon that's wrong.

(define +Setup-Special-Time-Code-Offset-Type+	   '(#x00 #x00))
(define +Setup-Special-Enable-Event-List-Type+	   '(#x01 #x00))
(define +Setup-Special-Disable-Event-List-Type+	   '(#x02 #x00))
(define +Setup-Special-Clear-Event-List-Type+	   '(#x03 #x00))
(define +Setup-Special-System-Stop-Type+	   '(#x04 #x00))
(define +Setup-Special-Event-List-Request-Type+	   '(#x05 #x00))


;;; Specification of a Time Code Offset is typically needed to synchronize
;;; different devices/media.

(define (make-Time-Code-Offset-sysex-data hr mn sc fr ff . args)
  (with-args (args &key (format +SMPTE-Format-30fps+)
	           (device-ID +All-Device-IDs+))
    (%make-setup-data device-ID 
                      +Setup-Special-Sub-ID-2+
		      format hr mn sc fr ff
		      +Setup-Special-Time-Code-Offset-Type+)))

; (make-Time-Code-Offset-sysex-data 1 2 3 4 5 :format +SMPTE-Format-25fps+)
;   => #(240 126 127 4 0 33 2 3 4 5 0 0 247) ; 33 = #b1 00001 = format 1, hour 1

;;; Enable Event List means for a slave to enable execution of events in its
;;; internal "list of events" when each one's respective SMPTE time
;;; occurs. Disable Event List means for a slave to disable execution of
;;; events in its internal "list of events", but not to erase the list.
;;; Clear Event List means for a slave to erase all events in its internal
;;; list.

(define (make-Enable-Event-List-sysex-data . args)
  (with-args (args &key (device-ID +All-Device-IDs+))
    (%make-setup-data device-ID +Setup-Special-Sub-ID-2+
		      0 0 0 0 0 0
		      +Setup-Special-Enable-Event-List-Type+)))

(define (make-Disable-Event-List-sysex-data . args)
  (with-args (args &key (device-ID +All-Device-IDs+))
    (%make-setup-data device-ID 
                      +Setup-Special-Sub-ID-2+
		      0 0 0 0 0 0
		      +Setup-Special-Disable-Event-List-Type+)))

(define (make-Clear-Event-List-sysex-data  . args)
  (with-args (args &key (device-ID +All-Device-IDs+))
    (%make-setup-data device-ID 
                      +Setup-Special-Sub-ID-2+
		      0 0 0 0 0 0
		      +Setup-Special-Clear-Event-List-Type+)))

; (make-Enable-Event-List-sysex-data)
;   => #(240 126 127 4 0 0 0 0 0 0 1 0 247)
; (make-Disable-Event-List-sysex-data)
;   => #(240 126 127 4 0 0 0 0 0 0 2 0 247)
; (make-Clear-Event-List-sysex-data)
;   => #(240 126 127 4 0 0 0 0 0 0 3 0 247)

;;; System Stop refers to a time when the slave may shut down. This serves
;;; as a protection against Event Starts without Event Stops, tape machines
;;; running past the end of a reel, etc.

(define (make-System-Stop-sysex-data hr mn sc fr ff . args)
  (with-args (args &key (format +SMPTE-Format-30fps+)
                   (device-ID +All-Device-IDs+))
    (%make-setup-data device-ID
                      +Setup-Special-Sub-ID-2+
		      format hr mn sc fr ff
		      +Setup-Special-System-Stop-Type+)))

; (make-System-Stop-sysex-data 1 2 3 4 5 :format +SMPTE-Format-25fps+)
;   => #(240 126 127 4 0 33 2 3 4 5 4 0 247)


;;; Event List Request is sent by the master, and requests the slave to send
;;; all events in its list as a series of Setup messages, starting from the
;;; SMPTE time in this message. 

(define (make-Event-List-Request-sysex-data hr mn sc fr ff  . args)
  (with-args (args &key (format +SMPTE-Format-30fps+)
                   (device-ID +All-Device-IDs+))
    (%make-setup-data device-ID +Setup-Special-Sub-ID-2+
		      format hr mn sc fr ff
		      +Setup-Special-Event-List-Request-Type+)))


; (make-Event-List-Request-sysex-data 1 2 3 4 5 :format +SMPTE-Format-25fps+)
;   => #(240 126 127 4 0 33 2 3 4 5 4 0 247)

;;;
;;; Punch In and Punch Out refer to the enabling and disabling of record
;;; mode on a unit. The Event Number refers to the track to be recorded.
;;; Multiple punch in/punch out points (and any of the other event types
;;; below) may be specified by sending multiple Set-Up messages with
;;; different times.  Delete Punch In or Out deletes the matching point
;;; (time and event number) from the Cue List. 

(define (make-Punch-In-Point-sysex-data track hr mn sc fr ff . args)
  (with-args (args &key (format +SMPTE-Format-30fps+) 
                   (device-ID +All-Device-IDs+))
    (%make-setup-data device-ID 
                      +Setup-Punch-In-Point-Sub-ID-2+
		      format hr mn sc fr ff
		      track)))

(define (make-Punch-Out-Point-sysex-data track hr mn sc fr ff . args)
  (with-args (args &key (format +SMPTE-Format-30fps+) 
                   (device-ID +All-Device-IDs+))
    (%make-setup-data device-ID 
                      +Setup-Punch-Out-Point-Sub-ID-2+
		      format hr mn sc fr ff
		      track)))

(define (make-Delete-Punch-In-Point-sysex-data track hr mn sc fr ff . args)
  (with-args (args &key (format +SMPTE-Format-30fps+) 
                   (device-ID +All-Device-IDs+))
    (%make-setup-data device-ID
                      +Setup-Delete-Punch-In-Point-Sub-ID-2+
		      format hr mn sc fr ff
		      track)))

(define (make-Delete-Punch-Out-Point-sysex-data track hr mn sc fr ff
                                                . args)
  (with-args (args &key (format +SMPTE-Format-30fps+) 
                   (device-ID +All-Device-IDs+))
    (%make-setup-data device-ID 
                      +Setup-Delete-Punch-Out-Point-Sub-ID-2+
		      format hr mn sc fr ff
		      track)))

; (make-Punch-In-Point-sysex-data 7 23 59 59 29 99)
;   => #(240 126 127 4 1 119 59 59 29 99 7 0 247)	; 33 = #b11.10111 = fmt 3, h 23
; (make-Punch-Out-Point-sysex-data 7 23 59 59 29 99)
;   => #(240 126 127 4 2 119 59 59 29 99 7 0 247)
; (make-Delete-Punch-In-Point-sysex-data 7 23 59 59 29 99)
;   => #(240 126 127 4 3 119 59 59 29 99 7 0 247)
; (make-Delete-Punch-Out-Point-sysex-data 7 23 59 59 29 99)
;   => #(240 126 127 4 4 119 59 59 29 99 7 0 247)

;;;
;;; Event Start and Stop refer to the running or playback of an event, and
;;; imply that a large sequence of events or a continuous event is to be
;;; started or stopped. The event number refers to which event on the
;;; targeted slave is to be played. A single event (ie. playback of a
;;; specific sample, a fader movement on an automated console, etc.) may
;;; occur several times throughout a given list of cues.  These events will
;;; be represented by the same event number, with different Start and Stop
;;; times.
;;;
;;; Event Start and Stop with Additional Information refer to an event (as
;;; above) with additional parameters transmitted in the Set Up message
;;; between the Time and EOX. The additional parameters may take the form of
;;; an effects unit's internal parameters, the volume level of a sound
;;; effect, etc.
;;;
;;; Delete Event Start/Stop means to delete the matching (event number and
;;; time) event (with or without additional information) from the Cue List. 

(define (make-Event-Start-Point-sysex-data event-nr hr mn sc fr ff . arg)
  (with-args (args &key (format +SMPTE-Format-30fps+) 
                   (device-ID +All-Device-IDs+))
    (%make-setup-data device-ID
                      +Setup-Event-Start-Point-Sub-ID-2+
		      format hr mn sc fr ff
		      event-nr)))

(define (make-Event-Stop-Point-sysex-data event-nr hr mn sc fr ff . arg)
  (with-args (args &key (format +SMPTE-Format-30fps+)
                   (device-ID +All-Device-IDs+))
    (%make-setup-data device-ID 
                      +Setup-Event-Stop-Point-Sub-ID-2+
		      format hr mn sc fr ff
		      event-nr)))

(define (make-Xtnd-Event-Start-Point-sysex-data event-nr hr mn sc fr 
                                                ff data . arg)
  (with-args (args &key (format +SMPTE-Format-30fps+) 
                   (device-ID +All-Device-IDs+))
    (%make-setup-data device-ID 
                      +Setup-Xtnd-Event-Start-Point-Sub-ID-2+
		      format hr mn sc fr ff
		      event-nr data)))

(define (make-Xtnd-Event-Stop-Point-sysex-data event-nr hr mn sc fr
                                               ff data . arg)
  (with-args (args &key (format +SMPTE-Format-30fps+) 
                   (device-ID +All-Device-IDs+))
    (%make-setup-data device-ID
                      +Setup-Xtnd-Event-Stop-Point-Sub-ID-2+
		      format hr mn sc fr ff
		      event-nr data)))

(define (make-Delete-Event-Start-Point-sysex-data event-nr hr mn sc fr
                                                  ff . arg)
  (with-args (args &key (format +SMPTE-Format-30fps+) 
                   (device-ID +All-Device-IDs+))
    (%make-setup-data device-ID 
                      +Setup-Delete-Event-Start-Point-Sub-ID-2+
		      format hr mn sc fr ff
		      event-nr)))

(define (make-Delete-Event-Stop-Point-sysex-data event-nr hr mn sc fr
                                                 ff . arg)
  (with-args (args &key (format +SMPTE-Format-30fps+)
                   (device-ID +All-Device-IDs+))
    (%make-setup-data device-ID 
                      +Setup-Delete-Event-Stop-Point-Sub-ID-2+
		      format hr mn sc fr ff
		      event-nr)))

; (make-Event-Start-Point-sysex-data #x3f80 23 59 59 29 99)
;   => #(240 126 127 4 5 119 59 59 29 99 0 127 247)
; (make-Event-Stop-Point-sysex-data #x3f80 23 59 59 29 99)
;   => #(240 126 127 4 6 119 59 59 29 99 0 127 247)
; (make-Xtnd-Event-Start-Point-sysex-data #x3f80 23 59 59 29 99 "test")
;   => #(240 126 127 4 7 119 59 59 29 99 0 127 4 7 5 6 3 7 4 7 247)
; (make-Xtnd-Event-Stop-Point-sysex-data #x3f80 23 59 59 29 99 "test")
;   => #(240 126 127 4 8 119 59 59 29 99 0 127 4 7 5 6 3 7 4 7 247)
; (make-Delete-Event-Start-Point-sysex-data #x3f80 23 59 59 29 99)
;   => #(240 126 127 4 9 119 59 59 29 99 0 127 247)
; (make-Delete-Event-Stop-Point-sysex-data #x3f80 23 59 59 29 99)
;   => #(240 126 127 4 10 119 59 59 29 99 0 127 247)

;;;
;;; Cue Point refers to individual event occurences, such as marking "hit"
;;; points for sound effects, reference points for editing, and so on.  Each
;;; Cue number may be assigned to a specific reaction, such as a specific
;;; one-shot sound event (as opposed to a continuous event, which is handled
;;; by Start/Stop).  A single cue may occur several times throughout a given
;;; list of cues.  These events will be represented by the same event
;;; number, with different Start and Stop times. 
;;;
;;; Cue Point with Additional Information is exactly like Event Start/Stop
;;; with Additional Information, except that the event represents a Cue
;;; Point rather than a Start/Stop Point.
;;;
;;; Delete Cue Point means to Delete the matching (event number and time)
;;; Cue Event with or without additional information from the Cue List.

(define (make-Cue-Point-sysex-data cue-nr hr mn sc fr ff . args)
  (with-args (args &key (format +SMPTE-Format-30fps+) 
                   (device-ID +All-Device-IDs+))
    (%make-setup-data device-ID +Setup-Cue-Point-Sub-ID-2+
		      format hr mn sc fr ff
		      cue-nr)))

(define (make-Xtnd-Cue-Point-sysex-data cue-nr hr mn sc fr ff data . args)
  (with-args (args &key (format +SMPTE-Format-30fps+) 
                   (device-ID +All-Device-IDs+))
    (%make-setup-data device-ID +Setup-Xtnd-Cue-Point-Sub-ID-2+
		      format hr mn sc fr ff
		      cue-nr data)))

(define (make-Delete-Cue-Point-sysex-data cue-nr hr mn sc fr ff . args)
  (with-args (args &key (format +SMPTE-Format-30fps+)
                   (device-ID +All-Device-IDs+))
    (%make-setup-data device-ID +Setup-Delete-Cue-Point-Sub-ID-2+
		      format hr mn sc fr ff
		      cue-nr)))

; (make-Cue-Point-sysex-data 17 1 2 3 4 5)
;   => #(240 126 127 4 11 97 2 3 4 5 17 0 247)
; (make-Xtnd-Cue-Point-sysex-data 17 1 2 3 4 5 '("test" #\a #(-1 -8)))
;   => #(240 126 127 4 12 97 2 3 4 5 17 0 4 7 5 6 3 7 4 7 0 0 1 6 15 15 8 15 247)
;                        ^^         ^^^^ ^^^ ^^^ ^^^ ^^^ ^^^ ^^^ ^^^^^ ^^^^
;                        #b11.00001 17   74H 65H 73H 74H  \0 61H   FFH  F8H

; (make-Delete-Cue-Point-sysex-data 17 1 2 3 4 5)
;   => #(240 126 127 4 13 97 2 3 4 5 17 0 247)


;;;
;;; Event Name in Additional Information merely assigns a name to a given
;;; event number.  It is for human logging purposes.

(define (make-Event-Name-sysex-data event-nr hr mn sc fr ff name . args)
  (with-args (args &key (format +SMPTE-Format-30fps+)
                   (device-ID +All-Device-IDs+))
    (%make-setup-data device-ID +Setup-Event-Name-Sub-ID-2+
		      format hr mn sc fr ff
		      event-nr name)))

; (make-Event-Name-sysex-data 17 0 0 10 23 0 "test")
;   => #(240 126 127 4 14 96 0 10 23 0 17 0 4 7 5 6 3 7 4 7 0 0 247)

;;;
;;;


; Missing: 
; ========
;   - MIDI File implementation doesn't deal with Downloadable Sounds
;   - REALTIME messages may interleave sysex messages!
;   - ?? time code message??
;   - GS Standard additions
;   - DLS Level 1 support
;   - MIDI Show Control
;   - MIDI Tuning
;   - Master Balance (RPN?)

(define-generic midi-event-data1)
(define-generic midi-event-data2)

(define-class <midi-event> (<event>)
  (opcode :accessor midi-event-opcode :allocation :class)
  :name 'midi-event)

(define-method (midi-event-data1 (obj <midi-event>))
  #f)

(define-method (midi-event-data2 (obj <midi-event>))
  #f)

(define-class <midi-channel-event> (<midi-event>)
  (channel :init-value 0 :init-keyword :channel
           :accessor midi-event-channel)
  :name 'midi-channel-event)

;;; GOOPS BUG (?) redeclaring opcode for :init-value obliterates
;;; its :accessor  and :allocation settings declared by <midi-event>!

(define-class <midi-system-event> (<midi-event>)
  (opcode :init-value #xf0)
  (type :init-value 0 :init-keyword :type 
        :accessor midi-event-data1)
  (data :init-value #f :init-keyword :data
        :accessor midi-event-data2)
  :name 'midi-system-event)

(define-class <midi-meta-event> (<midi-event>)
  (opcode :init-value #xff)
  :name 'midi-meta-event)

;;;
;;; event->message 
;;; at some point soon i will get rid of messages
;;; see midi3.scm for message->event conversion
;;;

(define-method (midi-event->midi-message (event <midi-channel-event>))
  ;; GOOPS BUG (?) declaring slots for :init-values obliterates
  ;; the :accessor and :allocation settings declared by <midi-event>!
  (values
   (make-channel-message (slot-ref event 'opcode)
                         (midi-event-channel event)
                         (midi-event-data1 event)
                         (or (midi-event-data2 event)
                             0))
   #f))

(define-method (midi-event->midi-message (event <midi-system-event>))
  (let* ((type (midi-event-data1 event))
         (code (logior #xf0 type))
         (data (midi-event-data2 event)))
    (cond ((eq? type 0)
           (make-sysex 0 data))
          ((<= 1 type 3)                ; qframe, songpos, songsel
           (make-system-message 0 code (midi-event-data2 event)))
          (else
           (make-system-message 0 code)))))

(define-method (midi-event->midi-message (event <midi-meta-event>))
  (let ((op (slot-ref event 'opcode)))
    (cond ((eq? op +ml-file-sequence-number-opcode+)
           (make-sequence-number (midi-event-data1 event)))
          ((eq? op +ml-file-text-event-opcode+)
           ;; pass string as first arg
           (make-text-event (midi-event-data2 event)
                            (midi-event-data1 event)))
          ;;((eq op +ml-file-midi-channel-opcode+) )
          ;;((eq op +ml-file-midi-port-opcode+) )
          ((eq? op +ml-file-eot-opcode+)
           (make-eot ))
          ((eq? op +ml-file-tempo-change-opcode+)
           ;; tempo is usecs per midi quarter note.
           (make-tempo-change (midi-event-data1 event)))
          ((eq? op +ml-file-smpte-offset-opcode+)
           (apply (function make-smpte-offset)
                  (midi-event-data1 event)))
          ((eq? op +ml-file-time-signature-opcode+)
           (make-time-signature (midi-event-data1 event)
                                (midi-event-data2 event)
                                (midi-event-data3 event)
                                (midi-event-data4 event)))
          ((eq? op +ml-file-key-signature-opcode+)
           (make-key-signature (midi-event-data1 event)
                               (midi-event-data2 event)))
          ((eq? op +ml-file-sequencer-event-opcode+)
           (make-sequencer-event (midi-event-data1 event)))
          (else
           (err "Unimplemented meta-event opcode: ~s" op)))))


