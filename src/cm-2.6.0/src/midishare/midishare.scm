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
;;; $Revision: 1.21 $
;;; $Date: 2004/06/19 14:02:41 $

(define *mp* #f) ; midi.port's MidiShare client refnum.

(define *midi-connections*
  ;; input       output
  '("MidiShare" "MidiShare"))

(define-class <midishare-stream> (<midi-stream>)
  (connections :init-thunk (lambda () *midi-connections*)
               :init-keyword :connections
               :accessor midishare-stream-connections)
   (elt-type :init-value :byte)
  :name 'midishare-stream
  :metaclass <io-class>
  :file-types '("*.ms")
  :mime-type "audio/x-midishare"
  )

(define-class <midi-port> (<midishare-stream>)
  :name 'midi-port
  :metaclass <io-class>
  :file-types '("midi.port")
  :mime-type "audio/x-midishare")

(define-method (midishare-stream-refnum (obj <midishare-stream>))
  ;; return client refnum if open
  (let ((ref (event-stream-stream obj)))
    (if (pair? ref)
      (car ref)
      #f)))

(define-method (open-midishare-client (obj <midishare-stream>)
                                      name)
  (ms:MidiOpen name))

(define-method (close-midishare-client (obj <midishare-stream>))
  (ms:MidiClose (midishare-stream-refnum obj)))
                                      
(define-method (open-io (obj <midishare-stream>) dir . args)
  ;; this method is used by both midishare and player streams.
  ;; if stream is set then return open else open and cache refnum
  dir args
  ;; check to make sure midishare is actually loaded.
  ;; apparently cmu returns 0/1 not nil/t
  (let ((res (ms:midishare)))
    (unless (equal? res 1)
      (err "Can't open MidiShare connection: MidiShare not loaded.")))
  (let ((name (object-name obj)))
    (unless (event-stream-stream obj)
      (let* ((client (if (or (not name)
                             (string-ci=? name "midi.port"))
                       "Common Music"
                       (filename-name name)))
             (applid (ms:MidiGetNamedAppl Client))
             (refnum  (if (= applid -4)
                        (open-midishare-client obj client)
                        applid )))
        (when (= refNum -4)
          (err "Cannot open Midishare client ~S."
               client))
        (set! (event-stream-stream obj)
              (midishare-set-connections obj refnum))
        (set! (io-open obj)
              (event-stream-stream obj))))
    obj))

(define (midishare-set-connections obj client)
  ;; refnum is id of client app, usually "Common Music"
  (let ((conn (midishare-stream-connections obj))
        (inref #f)
        (outref #f)
        (conn? (lambda (x y) (= 1 (ms:midiIsConnected x y))))
        (getref
         (lambda (x)
           (cond ((string? x)
                  (if (string=? x "") #f 
                    (let ((a (ms:MidiGetNamedAppl x)))
                      (if (= a -4)
                        (err "Not a Midishare client: ~s." x)
                        a))))
                 ((integer? x)
                  (let ((a #f))
                    (dotimes (i (ms:MidiCountAppls))
                      (if (= x (ms:MidiGetIndAppl (+ i 1))) 
                        (set! a x)))
                    (or a (err "Not a Midishare client refnum: ~S." x))))
                 ((eq? x #t)
                  ;; default is to connect to midishare
                  0)
                 ((not x)
                  #f)
                 (else
                  (err "Not a MidiShare client: ~s." x))))))
    (cond ((pair? conn)
           (set! inref (getref (first conn)))
           (when inref
             (unless (conn? inref client)
               (ms:MidiConnect inref client -1)
               (unless (conn? inref client)
                 (err "Input connection from ~S to ~S failed."
                      (ms:MidiGetName inref) (ms:MidiGetName client)))))
           (when (pair? (cdr conn))
             (set! outref (getref (second conn)))
             (when outref
               (unless (conn? client outref)
                 (ms:MidiConnect client outref -1)
                 (unless (conn? client outref)
                   (err "Output connection from ~S to ~S failed."
                        (ms:MidiGetName client) (ms:MidiGetName outref)))))))
          (else
           (set! outref (getref conn))
           (when outref
             (unless (conn? client outref)
               ;; -1 = #t
               (ms:MidiConnect client outref -1)
               (unless (conn? client outref)
                 (err "Output connection from ~S to ~S failed."
                      (ms:MidiGetName client) (ms:MidiGetName outref)))))))
    ;; value is:
    ;; <clientref> <inref> <outref> <tracknum>
    ;; the tracknum is only used by player streams.
    (list client inref outref #f)))

(define-method (close-io (obj <midishare-stream>) . mode)
  (if (eq? (car mode) ':force)
    (begin
     (close-midishare-client obj)
     (set! (event-stream-stream obj) #f)
     (set! (io-open obj) #f)))
  (values))

;;;
;;; midi-port routines. represents the CM application for MidiShare. 
;;; there is only one instance of midi-port per session.
;;;

(define-method (open-io (obj <midi-port>) dir . args)
  dir args
  (when (not (io-open obj))
    (next-method)
    (set! *mp* (midishare-stream-refnum obj)))
  obj)

(define-method (close-io (obj <midi-port>) . mode)
  ;; dont close unless error.
  (when (eq? (car mode) ':force)
    ;; flush any exusting receive hook!
    (if (receive?) (receive))
    (next-method)
    (set! *mp* #f))
  (values))

(define (midi-open . args)
  (apply #'open-io "midi.port" #t args))

(define (midi-open? )
  (let ((p (find-object "midi.port")))
    (and p (io-open p) p)))

(define (midi-close )
  (if (midi-open? )
    (begin
     (close-io (find-object "midi.port") ':force))
    #f)
  (values))

;(define (midi-reset . args)
;  (with-args (args &key (port 0))
;    (if (midi-open?)
;      (ms:MidiSend *mp* (ms:new typeReset :port port))
;      (warn "MidiShare not open."))))
;
;(define (midi-all-notes-off )
;  (if (midi-open? )
;    (ms:MidiSend *mp* (ms:new typeCtrlChange :controller 120))
;    (warn "MidiShare not open.")))
;
;(define (midi-hush . args)
;  ;; MidiShare has no way of flushing pending messages.
;  ;; This closes and then reopens the midi connection.
;  (with-args (args &optional ms)
;    (let ((s (if (not ms)
;               (find-object "midi.port" #f)
;               (if (string? ms)
;                 (find-object ms #f)
;                 ms))))
;      (when (and (is-a? s <midishare-stream>)
;                 (io-open s))
;        (close-io s ':force)
;        (open-io s #t))
;      s)))

;;;
;;; initialize-stream and deinitialize-stream
;;;

(define-method (initialize-io (obj <midishare-stream>))
  ;; cache current time offset
  (set! (object-time obj)
        (ms:MidiGetTime))
  (channel-tuning-init obj))

(define-method (deinitialize-io (obj <midishare-stream>))
  (set! (object-time obj) 0))

;;;
;;; ms:new -- "high level" MidiEv constructor
;;;

(define (ms:new . args)
  (with-args (args type &key (date 0) (port 0) (chan 0) 
                   (pitch 60) (vel 64) (dur 500)
                   (pressure 0) (controller 0) (change 0)
                   (bend 0) (program 0)
                   (numerator 4) (denominator 4) (clocks 24) (32nds 8)
                   (sign 0) (mode 0) (tempo 120) (number 0) (prefix 0)
                   (text "") (position 0) (song 0) (data '())
                   (offset '()))
    (let ((ev (ms:MidiNewEv type)))
      ;; common to all MidiEvs
      (ms:date ev date)
      (ms:port ev port)
      (ms:chan ev chan)
      (cond ((<= typeNote type typeKeyOff)
             (ms:field ev 0 pitch)
             (ms:field ev 1 vel)
             (when (eq? type typeNote)
               (ms:field ev 2 dur)))  ; dur=16bits 
            ((= type typeKeyPress)
             (ms:field ev 0 pitch)
             (ms:field ev 1 pressure))
            ((= type typeCtrlChange)
             (ms:field ev 0 controller)
             (ms:field ev 1 change))
            ((= type typeProgChange)
             (ms:field ev 0 program))
            ((= type typeChanPress)
             (ms:field ev 0 pressure))
            ((= type typePitchBend)
             ;; :bend is -8192 to 8191, 0 is no bend.
             (ms:bend ev (if (<= -8192 bend 8191) bend
                             (err ":bend value ~s not between -8192 8191."
                                  bend))))
            ;;
            ;; system common and realtime
            ;;
            ((= type typeSongPos)
             (let ((num (/ position 6)))
               (multiple-value-bind (msb lsb)
                   (clfloor num 128)
                 (ms:field ev 0 msb)
                 (ms:field ev 1 lsb))))
            ((= type typeSongSel)
             (ms:field ev 0 song))
            ((<= typeClock type typeReset) ; no data bytes
             #f)
            ((= type typeSysEx)
             (do ((l data (cdr l)))
                 ((null? l) #f)
               (ms:MidiAddField ev (car l))))
            ;;
            ;; meta messages. cannot be sent to external synth
            ;;
            ((= type typeSeqNum)
             (ms:field ev 0 number))
            ((<= typeTextual type typeCuePoint)
             (ms:text ev text))
            ((= type typeChanPrefix)
             (ms:field ev 0 prefix))
            ((= type typeEndTrack)
             #f)
            ((= type typeTempo)
             (ms:field ev 0 (inexact->exact 
                          (floor (/ 60000000 tempo)))))
            ((= type typeSMPTEOffset)
             (unless (= (length offset) 5)
               (err ":offset value ~s not list (hr min sec frame subframe)"
                    offset))
             (ms:field ev 0 (+ (* (list-ref offset 0) 3600)
                            (* (list-ref offset 1) 60)
                            (list-ref offset 2)))
             (ms:field ev 1 (+ (* (list-ref offset 3) 100)
                            (list-ref offset 4))))
            ((= type typeTimeSign)
             (ms:field ev 0 numerator)
             (ms:field ev 1 denominator)
             (ms:field ev 2 clocks)
             (ms:field ev 3 32nds))
            ((= type typeKeySign)
             (ms:field ev 0 (if (<= -7 sign 7)
                           (if (< sign 0) (+ sign 256) sign)
                           (err ":sign value ~s is not between -7 and 7."
                                sign)))
             (ms:field ev 1 mode))
            (else
             (err "Unimplemented MidiShare event type opcode: ~s."
                  type)))
      ev)))

;;;
;;; MidiEv  printing
;;;

(define MidiEvNames
  #("Note" "KeyOn" "KeyOff" "KeyPress" "CtrlChange" "ProgChange" "ChanPress"
    "PitchBend" "SongPos" "SongSel" "Clock" "Start" "Continue" "Stop" "Tune"
    "ActiveSens" "Reset" "SysEx" "SeqNum" "Text" "Copyright" "SeqName"
    "InstrName" "Lyric" "Marker" "CuePoint" "ChanPrefix" "EndTrack" "Tempo"
    "SMPTEOffset" "TimeSign" "KeySign"))

(define (ms:MidiPrintEv ev . stream)
  (let ((to (if (null? stream) #t (car stream)))
        (ty (ms:evtype ev))
        (cl #f))

    (cond ((<= typeNote ty typeSysEx)
           (set! cl (vector-ref MidiEvNames ty)))
          ((<= typeSeqNum ty typeKeysign)
           (set cl (vector-ref MidiEvNames
                               (+ (- ty typeSeqNum) typeSysEx 1))))
          (else #f))
    (if (not cl)
      (format to "~s" ev)
      (let ((ch (ms:chan ev))
            (po (ms:port ev))
            (da (ms:date ev)))
        (format to "#<MidiEv ~a [~s/~s ~sms]" cl po ch da)
        ;; value printing is really primitive for now
        (cond ((<= typeNote ty typeKeyOn)
               (format to " ~s ~s" (ms:field ev 0) (ms:field ev 1))
               (if (= ty typeNote)
                 (format to " ~sms" (ms:field ev 3))))
              ((= ty typePitchBend)
               ;; could print bend value
               (format to " ~s ~s" (ms:field ev 0) (ms:field ev 1)))
              ((= ty typeProgChange)
               ;; could print GM Program name
               (format to " ~s" (ms:field ev 0)))
              ((= ty typeCtrlChange)
               ;; could print controller type
               (format to " ~s ~s" (ms:field ev 0) (ms:field ev 1)))
              ((= ty typeSongPos)
               (format to " ~s ~s" (ms:field ev 0) (ms:field ev 1)))
              ((= ty typeSongSel)
               (format to " ~s" (ms:field ev 0)))
              ((= ty typeSysEx)
               ;; could print ID and length
               #f)
              ((= ty typeSeqNum)
               (format to " ~s" (ms:field ev 0)))
              ((<= typeTextual ty typeCuePoint)
               ;; could print lisp text string
               #f)
              ((= ty typeChanPrefix)
               (format to " ~s" (ms:field ev 0)))
              ((= ty typeEndTrack)
               #f)
              ((= ty typeTempo)
               (format to " ~susec" (ms:field ev 0)))
              ((= ty typeSMPTEOffset)
               (format to " ~s ~s" (ms:field ev 0) (ms:field ev 1)))
              ((= ty typeTimeSign)
               (format to " ~s ~s ~s ~s" (ms:field ev 0) (ms:field ev 1)
                       (ms:field ev 2) (ms:field ev 3)))
              ((= ty typeKeySign)
               (format to " ~s ~s" (ms:field ev 0) (ms:field ev 1)))
              (else
               #f))
        (format to ">")))))

(define-method (write-event (obj <midi>) (stream <midishare-stream>)
                            scoretime)
  (let* ((beg (+ (object-time stream) 
                 (inexact->exact (floor (* scoretime 1000)))))
         (dur (inexact->exact (floor (* (midi-duration obj) 1000))))
         (key (midi-keynum obj))
         (amp (midi-amplitude obj))
         (loc (logical-channel (midi-channel obj)
                               (midi-stream-channel-map stream)))
         (prt (car loc))
         (chn (cadr loc))
         (evt #f))

    (cond ((and (exact? amp)
                (<= 0 amp 127))
           #f)
          ((and (inexact? amp)
                (<= 0.0 amp 1.0))
           (set! amp (inexact->exact
                      (floor (* amp 127)))))
          (else
           (err "Can't convert amplitude ~s to midi velocity."
                amp)))
    (ensure-microtuning key chn stream)
    (set! evt (ms:new typeNote :port prt :chan chn
                      :pitch key :vel amp :dur dur))
    (ms:MidiSendAt (midishare-stream-refnum stream) evt beg)
    ;(when *msdbg* (ms:PrintMidiEv evt) (format #t "~%"))
    (values)))


;;; translate mi_d opcodes to midishare opcodes, only supports channel
;;; messages. will be thrown out once cm's low-level byte messages
;;; have been removed from the system.
;;;

(define opcodes-evtypes
  (list
   ;; channel
   (list (list +ml-note-on-opcode+ 1) ;typeKeyOn
         (list +ml-note-off-opcode+ 2) ;typeKeyOff
         (list +ml-key-pressure-opcode+ 3) ;typeKeyPress
         (list +ml-control-change-opcode+ 4) ;typeCtrlChange
         (list +ml-program-change-opcode+ 5) ;typeProgChange
         (list +ml-channel-pressure-opcode+ 6) ;typeChanPress
         (list +ml-pitch-bend-opcode+ 7)) ;typePitchBend
   ;; system
;;    (list (list (ash +ml-msg-sysex-type+ -4) typeSysEx)
;;          (list (ash +ml-msg-mtc-quarter-frame-type+ -4) #f)
;;          (list (ash +ml-msg-song-position-type+ -4) typeSongPos)
;;          (list (ash +ml-msg-song-select-type+ -4) typeSongSel)
;;          (list (ash +ml-msg-cable-select-type+ -4) #f)
;;          (list (ash +ml-msg-tune-request-type+ -4) typeTune )
;;          (list (ash +ml-msg-eox-type+ -4) #f)
;;          (list (ash +ml-msg-timing-clock-type+ -4) typeClock)
;;          (list (ash +ml-msg-timing-tick-type+ -4) #f)
;;          (list (ash +ml-msg-start-type+ -4) typeStart)
;;          (list (ash +ml-msg-continue-type+ -4) typeContinue)
;;          (list (ash +ml-msg-stop-type+ -4) typeStop)
;;          (list (ash +ml-msg-active-sensing-type+ -4) typeActiveSens)
;;          (list (ash +ml-msg-system-reset-type+ -4) typeReset))
;;    ;; meta
;;    (list (list +ml-file-sequence-number-opcode+ typeSeqNum)
;;          (list +ml-file-text-event-opcode+ 
;;                (list +ml-file-text-event-opcode+ typeTextual)
;;                (list +ml-file-copyright-note-opcode+ typeCopyright)
;;                (list +ml-file-sequence/track-name-opcode+ typeSeqName)
;;                (list +ml-file-instrument-name-opcode+ typeInstrName)
;;                (list +ml-file-lyric-opcode+ typeLyric)
;;                (list +ml-file-marker-opcode+ typeMarker)
;;                (list +ml-file-cue-point-opcode+ typeCuePoint))
;;          (list +ml-file-midi-channel-opcode+ typeChanPrefix)
;;          (list +ml-file-midi-port-opcode+ typePortPrefix)
;;          (list +ml-file-eot-opcode+ typeEndTrack)
;;          (list +ml-file-tempo-change-opcode+ typeTempo)
;;          (list +ml-file-smpte-offset-opcode+ typeSMPTEOffset)
;;          (list +ml-file-time-signature-opcode+ typeTimeSign)
;;          (list +ml-file-key-signature-opcode+ typeKeySign)
;;          (list +ml-file-sequencer-event-opcode+ ) #f)
   ))

(define (midi-op->evtype op set)
  (let ((e (assoc op (list-ref opcodes-evtypes set))))
    (if e
      (or (cadr e) 
          (err "No MidiShare evType for event opcode ~s."
               op))
      (err "No MidiShare evType for event opcode ~s."
           op))))
         
(define-method (write-event (obj <midi-channel-event>)
                            (stream <midishare-stream>) 
                            scoretime)
  (let* ((opr (slot-ref obj 'opcode))
         (typ (midi-op->evType opr 0))
         (beg (+ (object-time stream) 
                 (inexact->exact (floor (* scoretime 1000)))))
         (loc (logical-channel (midi-event-channel obj)
                               (midi-stream-channel-map stream)))
         (dat (midi-event-data2 obj))
         (evt (ms:MidiNewEv typ)))
    (ms:port evt (car loc))
    (ms:chan evt (cadr loc))
    (ms:date evt beg)
    (ms:field evt 0 (midi-event-data1 obj))
    (if (= typ typePitchBend)
      (bend evt (midi-pitch-bend-bend obj))
      (if dat (ms:field evt 1 dat)))
    (ms:MidiSendAt (midishare-stream-refnum stream) evt beg)
    evt))

;;;
;;; reading and writing MidiEvs.  the proctable stuff is take from
;;; Example 21 of Grame's Midishare-Tutorial.lisp
;;;

(define (make-proctable num)
  (reset-proctable (make-vector num)))

(define (reset-proctable tbl)
  ;; set each pos in tabl to index of next (free) pos
  ;; last pos points to nil
  ;; (elt 0) always holds next available
  (do ((num (vector-length tbl))
       (pos 0 (+ pos 1)))
      ((= pos num)
       (vector-set! tbl (- num 1) #f)
       tbl)
    (vector-set! tbl pos (+ pos 1))))

(define *proctable* (make-proctable 64))

(define (add-proc proc)
  ;; install proc at current free loc, first storing the next
  ;; free loc at pos 0. return nil if no more room.
  (let ((free (vector-ref *proctable* 0)))
    (if free
      (begin
        (vector-set! *proctable* 0 (vector-ref *proctable* free))
        (vector-set! *proctable* free proc)
        free)
      #f)))

(define (rem-proc index)
  (vector-set! *proctable* index 
               (vector-ref *proctable* 0))
  (vector-set! *proctable* 0 index)
  (values))

;; (defcallback run-proc ...) in openmcl.lisp

(define (rt-sprout obj . args)
  ;; add process obj to MidiShare's queue
  ;; RUN-PROC is a callback defined in openmcl.lisp
  (with-args (args &optional ahead )
    (let ((id (add-proc obj )))
      (if id
        (ms:MidiTask run-proc (+ (ms:MidiGetTime)
                                 (or ahead 0) )
                     *mp* id 0 0)
        (warn "Can't sprout, no room left in process table!")))
    (values)))

(define (rt-output ev . args)
  ;; output ev to Midishare
  (with-args (args &optional ahead)
    (if ahead
      (ms:MidiSendAt *mp* ev (+ ahead (ms:MidiGetTime)))
      (ms:MidiSendIm *mp* ev))
    (values)))

(define (rt-now )
  (ms:MidiGetTime))

;;;
;;; real time input
;;;

(define *receive-hook* #f)

;; (defcallback midi-receive-hook (:unsigned-halfword refnum)
;;   (do ((go #t)
;;        (ev (ms:MidiGetEv refnum) (ms:MidiGetEv refnum)))
;;       ((or (not go) (ms:nullptrp ev))
;;        (values))
;;     (if *receive-hook*
;;       (funcall *receive-hook* ev)
;;       (set! go #f))))

(define (receive? )
  (and *receive-hook* #t))

(define (receive . args)
  ;; MIDI-RECEIVE-HOOK is a callback defined in openmcl.lisp
  (with-args (args &optional hook)
    (if (midi-open?)
      (if hook
        (if *receive-hook*
          (format #t "Already receiving MIDI, type (ms:receive) to stop.~%")
          (begin (set! *receive-hook* hook)
                 (ms:MidiSetRcvAlarm *mp* midi-receive-hook)))
        (if (not *receive-hook*)
          (format #t "Not currently receiving MIDI.~%")
          (begin (set! *receive-hook* #f)
                 (ms:MidiSetRcvAlarm *mp* (ms:nullptr)))))
      (format #t "MIDI not open!"))
    (values)))



