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
;;; $Revision: 1.12 $
;;; $Date: 2005/02/20 17:26:22 $

; (midi-file-print "/usr/local/lisp/scm/test.midi")
;File: /usr/local/lisp/scm/test.midi 
;Format: 0
;Tracks: 1
;Division: 480
;
;Track 0, length 40
;         0 #<Tempo Change 1000.0 ms>
;         0 #<Note-On 0 60 64>
;       240 #<Note-Off 0 60 127>
;       240 #<Note-On 0 64 64>
;       240 #<Note-Off 0 64 127>
;       240 #<Note-On 0 67 64>
;       240 #<Note-Off 0 67 127>
;         0 #<End of Track #(0)>
;#P"/usr/local/lisp/scm/test.midi"
;
; (defmacro define (x y) `(setf ,x ,y))
; (define f (open-io "/usr/local/lisp/scm/foo.midi" :output))
; (define f (open-io "macintosh hd:foo2.midi" :output))
; (initialize-io f)
; (midi-write-message (make-note-on 0 50 100) f 0 #f)
; (midi-write-message (make-note-on 0 50 100) f 0 NIL)
; (deinitialize-io f)
; (close-io f)
; (midi-file-print "/usr/local/lisp/scm/foo2.midi")
; (midi-file-print "macintosh hd:foo2.midi")
; (delete-file "/usr/local/lisp/scm/foo.midi")

(define *midi-pitch-bend-width* 2)

;;;
;;; *midi-channel-map* maps logical channels to tuples: (port chan)
;;; where port is an integer port number and chan is an int<16.

(define *midi-channel-map*
  (let* ((l (list #f))
         (h l))
    (do ((p 0 (+ p 1)))
        ((= p 4)
         (apply #'vector (cdr l)))
      (do ((c 0 (+ c 1)))
          ((= c 16) #f)
        (set-cdr! h (list (list p c)))
        (set! h (cdr h))))))

(define (logical-channel chan map)
  (if (vector? map)
    (vector-ref map chan)
    (list-ref map chan)))

(define %midituningtypes
  ;; WHAT A FREAKING MESS! I should never have added done the
  ;; "divisions-per-octave"
  '((#t :note note :note-by-note note-by-note) ; order of first two
    (1  :12-note 12-note :12-tone 12-tone )    ; entries is important
    (2 24 :24-note 24-note :24-tone 24-tone )
    (3 36 :36-note 36-note :36-tone 36-tone )
    (4 48 :48-note 48-note :48-tone 48-tone )
    (5 60 :60-note 60-note :60-tone 60-tone )
    (6 72 :72-note 72-note :72-tone 72-tone )
    (7 84 :84-note 84-note :84-tone 84-tone ) 
    (8 96 :96-note 96-note :96-tone 96-tone  )
    (9 108 :108-note 108-note :108-tone 108-note )
    (10 120 :120-note 120-note :120-tone 120-tone )
    (11 132 :132-note 132-note :132-tone 132-tone )
    (12 144 :144-note 144-note :144-tone 144-tone )
    (13 156 :156-note 156-note :156-tone 156-tone )
    (14 :168-note 168-note :168-tone 168-tone )
    (15 :180-note 180-note :180-tone 180-tone )
    (16 :180-note 180-note :180-tone 180-tone )))

(define-class <midi-stream> (<event-stream>)
  (channel-map :init-thunk (lambda () *midi-channel-map*)
               :init-keyword :channel-map
               :accessor midi-stream-channel-map)
  (bend-width :accessor midi-stream-bend-width
	      :init-keyword :pitch-bend-width 
	      :init-thunk (lambda () *midi-pitch-bend-width*))
  ;; BUG! Goops wont find :microtuning
  (channel-tuning :init-keyword :channel-tuning  :init-keyword :microtuning
		  :init-value #f :accessor midi-stream-channel-tuning )
  (tunedata :init-value '() :accessor midi-stream-tunedata
	    :init-keyword :tuning-channels)
  :name 'midi-stream
  )

(define *midi-file-default-tempo* 60)

(define-class <midi-file-stream> (<midi-stream>)
  (elt-type :init-value :byte :init-keyword :elt-type
            :accessor file-elt-type)
  (keysig :init-value #f :init-keyword :keysig
	      :accessor midi-file-keysig)
  (timesig :init-value #f  :init-keyword :timesig 
	       :accessor midi-file-timesig)
  (tempo :init-thunk (lambda () *midi-file-default-tempo*)
         :init-keyword :tempo 
	     :accessor midi-file-tempo)
  (scaler :init-value 1 :accessor midi-file-scaler)
  (status :init-value 0 :accessor midi-file-status)
  (size :init-value 0 :accessor midi-file-size)
  (tracks :init-value 1 :accessor midi-file-tracks)
  (track :init-value -1 :init-keyword :track
	 :accessor midi-file-track)          
  (tracklen :init-value 0 :accessor midi-file-tracklen)
  (divisions :init-value 480 :init-keyword :divisions
	     :accessor midi-file-divisions)
  (resolution :init-value #f :init-keyword :resolution
	      :accessor midi-file-resolution)
  (format :init-value 0 :init-keyword :format
	  :accessor midi-file-format)
  (ticks :init-value #f :accessor midi-file-ticks)
  (delta :init-value #f :accessor midi-file-delta)
  (message :init-value #f :accessor midi-file-message)
  (data  :init-value '() :accessor midi-file-data)
  :name 'midi-file-stream
  :metaclass <io-class>
  :mime-type "audio/midi"
  :file-types '("*.midi" "*.mid"))

(define (set-midi-output-hook! fn)
  (unless (or (not fn) (procedure? fn))
    (err "Not a midi hook: ~s" fn))
  (set! (io-class-output-hook <midi-file-stream>) fn)
  (values))

(define (set-midi-file-versions! val)
  (set! (io-class-file-versions <midi-file-stream>) val)
  (values))

;;;
;;; midi-file lowest level byte functions operate on file pointer.
;;;

(define +midi-file-header-length+ 14)
(define +miditrack-header-length+ 8)
(define +MThd+ #x4d546864)		; 'MThd'
(define +MTrk+ #x4d54726b)		; 'MTrk'

(define (read-byte fp)
  (char->integer (read-char fp)))

(define (write-byte by fp)
  (write-char (integer->char by) fp))

(define (read-bytes fp n)
  (do ((s 0)
       (i 0 (1+ i)))
      ((>= i n) s)
    (set! s (+ (ash s 8) (read-byte fp)))))

(define (write-bytes fp byts n)
  (do ((pos (* (1- n) 8) (- pos 8)))
      ((< pos 0) (values))
    (write-byte (ash (logand byts (ash #xff pos))
		     (- pos))
		fp)))

(define (read-variable-quantity fp)
  (let* ((b (read-byte fp))
	 (n (logand b #x7f)))
    (do ()
	((not (logtest #x80 b)) n)
      (set! b (read-byte fp))
      (set! n (+ (ash n 7) (logand b #x7f))))))

(define (write-variable-quantity n fp)
  (when (>= n #x200000)			; 1<<21
    (write-byte (logior (logand (ash n -21) #x7f) 128) fp))
  (when (>= n #x4000)			; 1<<14 
    (write-byte (logior (logand (ash n -14) #x7f) 128) fp))
  (when (>= n #x80)			; 1<<7
    (write-byte (logior (logand (ash n  -7) #x7f) 128) fp))
  (write-byte (logand n #x7F) fp)
  (values))

(define (variable-quantity-length n)
  (let ((l 1))
    (if (>= n 2097152) (set! l (+ 1 l)))
    (if (>= n 16384) (set! l (+ 1 l)))
    (if (>= n 128) (set! l (+ 1 l)))
    l))

(define (midi-file-read-header mf)
  (let* ((fp (io-open mf))
	 (bytes (read-bytes fp 4)))
    (unless (= bytes +MThd+)
      (err "Expected midi-file header mark but got ~s instead."
	   bytes))
    (read-bytes fp 4)			; skip header length bytes
    (values (read-bytes fp 2)		; format
	    (read-bytes fp 2)		; ntracks
	    (read-bytes fp 2))))	; time format

(define (midi-file-write-header mf fmat tracks division . resolution)
  (let ((fp (io-open mf)))
    (write-bytes fp +MThd+ 4)
    (write-bytes fp 6 4)		; header length stored 00 00 00 06
    (write-bytes fp fmat 2)
    (write-bytes fp tracks 2)
    (if (null? resolution)
      (write-bytes fp division 2)
      ;; division denotes the frame rate, resolution the subframes
      (begin (write-bytes fp (+ (- division) #x100) 1)
	     (write-bytes fp (car resolution) 1)))
    (values)))

(define (midi-file-read-track-header mf)
  (let* ((fp (io-open mf))
	 (byts (read-bytes fp 4)))
    (unless (= byts +MTrk+)
      (err "Expected midi-file track mark but got ~s instead." 
	   byts))
    (read-bytes fp 4)))

(define (midi-file-write-track-header mf len)
  (let ((fp (io-open mf)))
    (write-bytes fp +MTrk+ 4)
    (write-bytes fp len 4)
    (values)))

;;;
;;; midlevel rountines operate on midi-file objects
;;;

(define (midi-file-read-message mf)
  ;; returns delta ticks and message, side effects mf 
  (let ((fp (io-open mf))
	(ticks 0)
	(byte 0)
	(size 0)
	(raw 0))
    (set! ticks (read-variable-quantity fp))
    (set! (midi-file-delta mf) ticks)
    (set! (midi-file-data mf) #f)
    (when (> ticks 0)
      (set! (midi-file-ticks mf)
            (+ (midi-file-ticks mf) ticks)))
    (set! byte (read-byte fp))
    (cond ((< byte #xf0)		; channel message
	   (if (logtest byte 128)
	     ;; normal status. cache status byte and message size.
	     (begin
	      (set! raw byte)
	      (set! (midi-file-status mf) byte)
	      (set! size (vector-ref +channel-message-sizes+
				     (ash (logand byte #x70) -4)))
	      (set! (midi-file-size mf) size))
	     ;; running status. since we skip the status byte, the
	     ;; the current byte is already data so we read 1 less
	     ;; byte than size
	     (begin
	      (set! raw (logior (ash (midi-file-status mf) 8)
				byte))
	      (set! size (1- (midi-file-size mf)))))
	   ;; raw = status or status+data if running. read
	   ;; any remaining data bytes. use size-1 because size includes
	   ;; status byte. 
	   (when (> size 1)
	     (dotimes (i (1- size))  
	       (set! raw (logior (ash raw 8) (read-byte fp)))))
	   (set! (midi-file-message mf)
		 (%midi-encode-channel-message raw (midi-file-size mf)))
	   ;(values ticks (midi-file-message mf) #f)
           (values (midi-file-message mf)))
	  ((= byte +ml-file-meta-marker+) ; meta message
	   (set! byte (read-byte fp))
	   (cond ((= byte +ml-file-eot-opcode+) 
		  (read-byte fp)	; flush the byte
		  (multiple-value-bind (m d) (make-eot)
		    (set! (midi-file-message mf) m)
		    (set! (midi-file-data mf) d)
		    (values m)))
		 ((= byte +ml-file-tempo-change-opcode+)
		  ;; side effect the tempo file
		  (read-variable-quantity fp) ; flush length
		  (let ((usecs
			 (logior (ash (read-byte fp) 16)
				 (ash (read-byte fp)  8)
				 (read-byte fp))))
		    (multiple-value-bind (m d)
                        (make-tempo-change usecs)
		      (set! (midi-file-message mf) m)
		      (set! (midi-file-data mf) d)
                      ;; cache tempo in tempo map if track zero
                      ;(when (= (midi-file-track mf) 0)
                      ;  (set! (midi-file-tempo-mao mf)
                      ;        (list (list (midi-file-ticks mf)
                      ;                    m d))))
		      ;(values ticks m d)
                      (values m))))
		 ((= byte +ml-file-time-signature-opcode+)
		  (let ((len (read-variable-quantity fp))) ; flush length
		    (unless (= len 4) 
		      (err "unexpected time signature length: ~s" len))
		    (multiple-value-bind (m d)
			(apply (function make-meta-message)
			       +ml-file-time-signature-opcode+
			       (loop repeat len collect (read-byte fp)))
		      (set! (midi-file-message mf) m)
		      (set! (midi-file-data mf) d)
                      (values m))))
		 (else 
		  (set! size (read-variable-quantity fp))
		  (multiple-value-bind (m d)
		      (apply (function make-meta-message)
                             byte
			     (loop repeat size collect (read-byte fp)))
		    (set! (midi-file-message mf) m)
		    (set! (midi-file-data mf) d)
                    (values m)))))
	  (else				; sysex message ??? system too
	   ;; skip over sysex messages. this should be fixed.
	   (set! size (read-variable-quantity fp))
	   ;;(FILE-POSITION fp (+ size (FILE-POSITION fp)))
	   (dotimes (i size) (read-byte fp))
           (midi-file-read-message mf)))))


(define (midi-file-unread-message mf msg . args)
  (let ((fp (io-open mf))
	(delta (if (null? args) 0 (car args))))
    (set-file-position fp (- (+ (midimsg-size msg)
		                (variable-quantity-length delta)))
	               #f)))

;;;
;;; Meta Messages:
;;;   Bytes: FF <type> <length>+ <data>*
;;;   FF and <type> are encoded in message.  Since Meta messages are not
;;;   for MIDI transmission, the <length>+ bytes are already encoded and 
;;;   are prepended to <data> in a vector.
;;;     NOTE: "FF" is replaced by +ml-meta-tag+ (actually 0) to  
;;;     differentiate between system reset and meta messages.
;;; Sysex Messages:
;;;   Bytes: F0 <length>+ <data>* F7
;;;   Only F0 is encoded in message.  data holds the whole original sysex 
;;;   message consisting of F0, <data>* and F7, with no <length> field
;;;   since <length> is needed only for MIDI files.  
;;;   This variable-size parameter is thus determined in run-time.
;;;   The alternate sysex syntax (applicable only to midi files) 
;;;     F0 <data>*     {F7 <data>*}*     F7
;;;   is supported read-only, i.e., we always write sysex as a contiguous
;;;   message.

(define-method (midi-write-message (msg <number>) (mf <midi-file-stream>)
				   time data)
  (let ((fp (io-open mf))
	(size (midimsg-size msg))
	(type (midimsg-upper-status msg))) ; #xf=sys,#x0=meta
    (write-variable-quantity time fp)
    (cond ((< #x0 type #xf)		; channel msg
	   (write-byte (logior (ldb +enc-lower-status-byte+ msg)
			       (ash (ldb +enc-upper-status-byte+
					 msg)
				    4))
		       fp)
	   (when (> size 1) 
	     (write-byte (ldb +enc-data-1-byte+ msg) fp))
	   (when (> size 2)
	     (write-byte (ldb +enc-data-2-byte+ msg) fp)))
	  ((= type +ml-meta-type+)	; meta msg
	   (write-byte #xff fp)
	   (write-byte (ldb +enc-data-1-byte+ msg) fp)
	   (loop for i below (vector-length data)
		 do (write-byte (vector-ref data i) fp)))
	  ((= type #xf)			; system, real-time
	   (let ((byt (logior (ash type 4)
			      (midimsg-lower-status msg))))
	     (write-byte byt fp)
	     (case byt
	       ((#xf0 )			; sysex
		;; do not include the already written #xF0 in length!
		(write-variable-quantity (1- (vector-length data)) fp)
		;; do not repeat the initial #xF0
		(loop for i from 1 below (vector-length data)
		      do (write-byte (vector-ref data i) fp)))
	       ((#xf2 )			; song position
		(write-byte (ldb +enc-data-1-byte+ msg) fp)
		(write-byte (ldb +enc-data-2-byte+ msg) fp))
	       ((#xf3 )			; song select
		(write-byte (ldb +enc-data-1-byte+ msg) fp))
	       ((#xf6 ) #f)		; tune request
	       (else
		(err "~s is not a valid system message type."
		     byt)))))
	  (else
	   (err "msg type neither meta nor system! ")))
    (values)))

;;;
;;; midlevel midi-file routines
;;;
;;;
;;; raw? fps?  | div		usecs		scaler
;;; -----------+--------------------------------------------------------
;;;  -    -    | divs/beat	<irrelevant>	1/div
;;;  x    -    | divs/beat	usecs/beat	1/div * usecs/1000000
;;;  -    x    | divs/s		usecs/beat	1/div * 1000000/usecs
;;;  x    x    | divs/s		<irrelevant>	1/div
;;; 
;;; then set the scaler:
;;;
;;;   scaler = c/divisions       (tunit := beat | s | ms | ticks)

; (define (set-scaler mf usecs)
;   (let* ((tunit (or (midi-file-ttype mf) ':ticks))
; 	 (div (midi-file-divisions mf))
; 	 (res (midi-file-resolution mf))
; 	 (tscale (case tunit
; 		   ((:ticks ) #f) 
; 		   ((:seconds ) 1.0)
; ;		   ((:milliseconds ) 1000.0)
;                    (else
;                     (err "Time unit ~s not :ticks or :seconds." tunit)
;                     )
;                    ))
; 	 (fps? (number? res))
; 	 (raw? (eq? (midi-file-time-format mf) ':raw)))
;     (set! (midi-file-scaler mf)
; 	  (if (eq? tunit ':ticks)
; 	    1
; 	    (if raw? 
; 	      ;; scale to raw time
; 	      (if fps?
; 		;; if divs are time-based, usecs don't matter
; 		(/ 1.0 (* res div))
; 		;; else divs are beats, so scale by tempo to get time
; 		(* (/ 1.0 div) 
; 		   (/ (* usecs tscale) 1000000.0)))
; 	      (if fps?
; 		;; if divs represent absolute time, scale by inverse tempo
; 		;; to get beats
; 		(* (/ 1.0 (* res div)) 
; 		   (/ 1000000.0 (* usecs tscale)))
; 		;; else divs give the beat we want
; 		(/ 1.0 div)))))))

(define (midi-file-map-track fn mf . args)
  (let ((beg (if (null? args) #f (pop args)))
	(end (if (null? args) #f (pop args))))
    beg end
    (do ((msg (midi-file-read-message mf)
              (midi-file-read-message mf)))
        ((eot-p msg)  ;(or (ept-p m) (and end (> s end)))
         (set! (midi-file-track mf) #f)
         #t)
         (fn mf))))

(define (midi-file-set-track mf track)
  ;; set file position to tbe begining of track header
  ;; parse track header and position file pointer
  ;; to start of data.  file must be open and track must be valid.
  (let ((fil (io-open mf)))
    (set! (midi-file-track mf) track)
    (set! (midi-file-ticks mf) 0)
    ;; 8='mthd'+lenBytes
    (loop for p = +midi-file-header-length+ 
	  then (+ p 8 (midi-file-read-track-header mf))
	  for c from 0
	  do (set-file-position fil p #t)	;(file-position fil p)
	  while (< c track))
    (set! (midi-file-tracklen mf)
	  (midi-file-read-track-header mf))
    track))

;;;
;;; high level midi-file rountines
;;;

(define (channel-tuning-init io)
  (let ((tuning (midi-stream-channel-tuning io))
        )
    (if (not tuning)
      (begin
       ;; clear any existing bends if there was a tuning
       ;; and now there isnt
       ;(if (and old (is-a? io <event-port>)) ; is midi port
       ; (microtune-channels io 1 (midi-stream-bend-width io) 0))
       (set! (midi-stream-tunedata io) '()))
      (let ((tune #f)
	    (num1 #f)
	    (num2 #f)
	    (data #f)
            (type #f))
	(if (pair? tuning)
	  (set! tune (pop tuning))
	  (begin (set! tune tuning)
		 (set! tuning #f)))
        (set! type (find tune %midituningtypes :test #'member))
	(cond ((eq? type (car %midituningtypes))
	       ;; note by note tuning.
               (if (pair? tuning)
                 (begin (set! num1 (pop tuning))
                        (set! num2 (or (pop tuning) 15)))
                 (begin (set! num1 0)
                        (set! num2 15)))
               (unless (<= 0 num1 num2 15)
                 (err "tuning range ~s-~s not in channel range 0-15."
                      num1 num2))
               ;; data=(T <num> <max> <off> <width>)
               (set! data (list #t (- num2 num1) (- num2 num1) num1
                                (midi-stream-bend-width io))))
	      ((not (null type))
               ;; channel tuning
	       ;; data=(<chanoffset> <divisions>)
               (setq tune (car type)) ; force divisions per SEMItone
	       (set! num1 (if (pair? tuning) (pop tuning) 0))
	       ;;(set! num2 (/ tune 12)) ;; SEMItone divisions.
               (set! num2 tune)
	       (when (> (+ num1 num2) 15)
	         (err "tuning range ~s-~s not in channel range 0-15."
		      num1 (+ num1 num2)))
	       (if (equal tune 1)
                 ;; 1 is normal tuning, clear existing bends but dont
                 ;; cache
	         (begin
		  (microtune-channels io 1 
				      (midi-stream-bend-width io) 0)
		  (set! data '()))
	         (begin
		  (microtune-channels io num2 
				      (midi-stream-bend-width io) num1)
		  (set! data (list num1 num2)))))
              (else (err "~s is not a midi tuning. Valid tunings: ~s"
	                 (midi-stream-channel-tuning io)
                         (map #'car %midituningtypes))))
	(set! (midi-stream-tunedata io) data)
	data))))

;(defparameter #$midi.port (midi-tuning-init nil '(:dynamic 3 9)))
;(defparameter #$midi.port (midi-tuning-init nil ':dynamic))

(define (microtune-channels io divisions . args)
  (with-args (args &optional (width *midi-pitch-bend-width*)
		   (channel-offset 0))
    ;; clears/sets divisions number of pitch bends in file or port
   (if (= divisions 1)
      (loop for c below 16
         ;; midi-pitch-bend is defined in midi3.
	 for m = (make-instance (find-class 'midi-pitch-bend) :channel c
                                :time 0 :bend 0)
	 do (write-event m io 0))
      (loop repeat divisions
	 for c from channel-offset
	 for m = (let ((bend (round (rescale (/ c divisions) (- width) 
                                             width -8192 8191))))
		   (make-instance (find-class 'midi-pitch-bend)
                                  :channel c :time 0 :bend bend))
	 do (write-event m io 0)))))

;;; system note off resource. initialized to 50 entries.

(define %offs (make-cycl))
(dotimes (i 50) (%qe-dealloc %offs (list #f #f #f)))

(define-method (open-io (mf <midi-file-stream>) dir . args)
  ;; was an :after method
  args
  (next-method)
  (if (eq? dir ':output)		
    (let ((div (midi-file-divisions mf))) 
      ;; only writes level 0.
      (set! (midi-file-track mf) 0)
      ;; output scaler is divisions per (normalized)
      ;; beat times timescale
      (set! (midi-file-scaler mf) (* div 1.0))
      (midi-file-write-header mf 0 1 div)
      (midi-file-write-track-header mf 0))
    (multiple-value-bind (fmat tracks divisions)
	(midi-file-read-header mf)
      (set! (midi-file-format mf) fmat)
      (set! (midi-file-tracks mf) tracks)
      (set! (midi-file-divisions mf) divisions)
      ;(set-scaler mf 1000000)
      ;(set! (midi-file-tempo-map mf) #f)
      ;(unless (equal? (midi-file-track mf) -1)
      ;(midi-file-set-track mf (midi-file-track mf)))
      ))
  mf)

(define-method (initialize-io (mf <midi-file-stream>) )
  (when (eq? (io-direction mf) ':output)
    (let ((msg #f)
	  (data #f))

      (set! (object-time mf) 0)
      ;; pointer should be in proper place
      (when (midi-file-tempo mf)
	(multiple-value-setq (msg data)
			     (make-tempo-change
			      (inexact->exact
			       (floor
				(* 1000000
				   (/ 60 (midi-file-tempo mf)))))))
	(midi-write-message msg mf 0 data))
      (when (midi-file-timesig mf)
	(multiple-value-setq (msg data)
			     (apply (function make-time-signature)
				    (midi-file-timesig mf)))
	(midi-write-message msg mf 0 data))
      (when (midi-file-keysig mf)
	(multiple-value-setq (msg data)
			     (apply (function make-key-signature)
				    (midi-file-keysig mf)))
	(midi-write-message msg mf 0 data))
      ;; if micro divisions active write pitchbends to file.
      (channel-tuning-init mf)))
  (values))

(define-method (deinitialize-io (mf <midi-file-stream>) )
  (when (eq? (io-direction mf) ':output)
    (flush-pending-offs mf)))

(define-method (close-io (mf <midi-file-stream>) . mode)
  mode ; gag 'unused variable' message from cltl compilers
  (when (eq? (io-direction mf) ':output)
    (multiple-value-bind (m d) (make-eot)
      (midi-write-message m mf 0 d))
    (let* ((fp (io-open mf))
	   (off (+ +midi-file-header-length+ +miditrack-header-length+))
	   (end (set-file-position fp 0 #f)))
      (set-file-position fp +midi-file-header-length+ #t)
      (midi-file-write-track-header mf (- end off))
      ;(set! (midi-file-track mf) -1)
      (set! (midi-file-track mf) #f)
      ;; deallocate any remaining entries after error break.
      (%q-flush %offs)))
  (next-method))

(define (flush-pending-offs mf . args)
  (let ((time (if (null? args) most-positive-fixnum
		  (car args)))
	(last (object-time mf))
	(scaler (midi-file-scaler mf)))
    (do ((qe (%q-peek %offs) (%q-peek %offs)))
	((or (null? qe) (> (%qe-time qe) time))
	 last)
      (%q-pop %offs)      
      (midi-write-message (%qe-object qe) 
			  mf
			  (inexact->exact ; round
			   (* (- (%qe-time qe) last)
			      scaler))
			  #f)
      (set! last (%qe-time qe))
      (%qe-dealloc %offs qe)
      (set! (object-time mf) last))))

(define (midi-file-print . args)
  (with-args (args file &key (stream #t) (track 0) )
    (with-open-io (io file :input )
      (format stream "File: ~a ~%Format: ~s~%Tracks: ~s~%Division: ~s"
	      file
              (midi-file-format io) 
              (midi-file-tracks io)
              (midi-file-divisions io))
      (midi-file-set-track io track)
      (format stream "~%Track ~s, length ~s~%"
              (midi-file-track io) 
              (midi-file-tracklen io))
      (midi-file-map-track 
       (lambda (mf)
         (let ((q (midi-file-delta mf))
               (m (midi-file-message mf))
               (d (midi-file-data mf)))
           (midi-print-message m q :stream stream :data d)
           (newline)))
       io)
      (io-filename io))))

(define (oss-play-midi-file file . args)
  (let ((opts (if (null? args) ""
		  (string-append  " " (car args)))))
    (shell (format #f "/usr/bin/playmidi~a ~a > /dev/null &" 
           opts file))
    file))

(define *linux-midi-file-player* 
  "timidity -quiet=2")

(define (linux-play-midi-file file . args)
  (with-args (args &optional (fork? #t))
    (let ((cmd (string-append *linux-midi-file-player* " " file 
                              (if fork? " &" ""))))
      (shell cmd)
      file)))

(define *win-midi-file-player*
  "C:\\Program Files\\Windows Media Player\\mplayer2.exe")

(define (win-play-midi-file file . args)
  (let ((opts (if (null? args) ""
		  (string-append  " " (car args)))))
    (if (file-exists? *win-midi-file-player*)
      (shell (format #f "~a~a ~a" *win-midi-file-player* opts file))
      (begin
       (warn "The MIDI player ~s does not exist. Set the variable *win-midi-file-player* to the pathname (string) of a MIDI player on your machine and try again."
             *win-midi-file-player*)))  
    file))

(define *osx-midi-file-player* "open")

(define (osx-play-midi-file file . args)
  (with-args (args &optional (fork? t))
    (let ((cmd (string-append *osx-midi-file-player*
                              " " file (if fork? " &" ""))))
      (shell cmd)
      file)))
