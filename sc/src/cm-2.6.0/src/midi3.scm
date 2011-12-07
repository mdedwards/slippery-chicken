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
;;; $Date: 2005/03/21 20:12:11 $

; (define f (open-io "/usr/local/lisp/scm/foo.midi" :output))
; (initialize-io f)
; (write-event (new midi time 0 keynum 60 duration 1) f 0)
; (deinitialize-io f)
; (close-io f)
; (midi-file-print "/usr/local/lisp/scm/foo.midi")
; (delete-file  "/usr/local/lisp/scm/foo.midi"))

;(defprocess hoo ()
;  (process for k from 60 to 62
;	   output (new midi time (now) keynum k)
;	   wait 1))

; (events (hoo) "/usr/local/lisp/scm/poo.midi")
; (midi-file-print "/usr/local/lisp/scm/poo.midi")
; (delete-file  "/usr/local/lisp/scm/poo.midi"))

;;;
;;; channel messages
;;;

(defobject midi-note-on (midi-channel-event)
  ((opcode :initform +ml-note-on-opcode+ :initarg #f)
   (keynum :accessor midi-event-data1)
   (velocity :accessor midi-event-data2 :initform 64))
  (:parameters time channel keynum velocity)
  (:writers ))

(defobject midi-note-off (midi-channel-event)
  ((opcode :initform +ml-note-off-opcode+ :initarg #f)
   (keynum :accessor midi-event-data1)
   (velocity :accessor midi-event-data2 :initform 64))
  (:parameters time channel keynum velocity)
  (:writers ))

(defobject midi-key-pressure (midi-channel-event)
  ((opcode :initform +ml-key-pressure-opcode+ :initarg #f)
   (keynum :accessor midi-event-data1)
   (pressure :accessor midi-event-data2))
  (:parameters time channel keynum pressure)
  (:writers ))

(defobject midi-control-change (midi-channel-event)
  ((opcode :initform +ml-control-change-opcode+ :initarg #f)
   (controller :accessor midi-event-data1)
   (value :accessor midi-event-data2))
  (:parameters time channel controller value)
  (:writers ))

(defobject midi-program-change (midi-channel-event)
  ((opcode :initform +ml-program-change-opcode+ :initarg #f)
   (program :accessor midi-event-data1))
  (:parameters time channel program)
  (:writers ))

(defobject midi-channel-pressure (midi-channel-event)
  ((opcode :initform +ml-channel-pressure-opcode+ :initarg #f)
   (pressure :accessor midi-event-data1))
  (:parameters time channel pressure)
  (:writers ))

(defobject midi-pitch-bend (midi-channel-event)
  ((opcode :initform +ml-pitch-bend-opcode+ :initarg #f)
   (bend :initform 0))
  (:parameters time channel bend )
  (:writers ))

(define-method (midi-event-data1 (obj <midi-pitch-bend>))
  ;; return lsb of 2comp bend value
  (multiple-value-bind (ms7b ls7b)
      (floor (+ (midi-pitch-bend-bend obj) 8192) 128)
    ms7b
    ;; return lsb
    ls7b))

(define-method (midi-event-data2 (obj <midi-pitch-bend>))
  ;; return msb of 2comp bend value
  (multiple-value-bind (ms7b ls7b)
      (floor (+ (midi-pitch-bend-bend obj) 8192) 128)
    ls7b
    ;; return msb
    ms7b))

;;;
;;; meta messages
;;;

(defobject midi-sequence-number (midi-meta-event)
  ((opcode :initform +ml-file-sequence-number-opcode+ :initarg #f)
   (number :accessor midi-event-data1))
  (:parameters time number)
  (:writers ))

(defobject midi-text-event (midi-meta-event)
  ((opcode :initform +ml-file-text-event-opcode+ :initarg #f)
   (type :initform +ml-file-text-event-opcode+
         :accessor midi-event-data1)
   (text :accessor midi-event-data2))
  (:parameters time type text)
  (:writers ))

(defobject midi-eot (midi-meta-event)
  ((opcode :initform +ml-file-eot-opcode+ :initarg #f))
  (:parameters time )
  (:writers ))

(defobject midi-tempo-change (midi-meta-event)
  ((opcode :initform +ml-file-tempo-change-opcode+ :initarg #f)
   (usecs :accessor midi-event-data1))
  (:parameters time usecs)
  (:writers ))

(defobject midi-smpte-offset (midi-meta-event)
  ((opcode :initform +ml-file-smpte-offset-opcode+ :initarg #f)
   (offset :initform '() :accessor midi-event-data1))
  (:parameters time offset)
  (:writers ))

(defobject midi-time-signature (midi-meta-event)
  ((opcode :initform +ml-file-time-signature-opcode+ :initarg #f)
   (numerator :accessor midi-event-data1)
   (denominator :accessor midi-event-data2)
   (clocks :initform 24 :accessor midi-event-data3)
   (32nds :initform 8 :accessor midi-event-data4))
  (:parameters opcode numerator denominator clocks 32nds)
  (:writers ))

(defobject midi-key-signature (midi-meta-event)
  ((opcode :initform +ml-file-key-signature-opcode+ :initarg #f)
   (key :initform 0 :accessor midi-event-data1)
   (mode :initform 0 :accessor midi-event-data2))
  (:parameters time key mode)
  (:writers ))

(defobject midi-sequencer-event (midi-meta-event)
  ((opcode :initform +ml-file-sequencer-event-opcode+ :initarg #f)
   (data :accessor midi-event-data1))
  (:parameters time data)
  (:writers ))

;;;
;;; message->event conversion
;;; remove the message layer from cm at some point...
;;;

(define (midi-message->midi-event m . args)
  (with-args (args &key data time)
    (let ((ch #f))
      (cond ((midi-channel-message-p m)
             (set! ch (channel-message-channel m) )
             (cond ((note-on-p m)
                    (make <midi-note-on>
                          :time time :channel ch
                          :keynum (note-on-key m)
                          :velocity (note-on-velocity m)))
                   ((note-off-p m)
                    (make <midi-note-off>
                          :time time :channel ch
                          :keynum (note-on-key m)
                          :velocity (note-on-velocity m)))
                   ((key-pressure-p m)
                    (make <midi-key-pressure>
                          :time time :channel ch
                          :keynum (key-pressure-key m)
                          :pressure (key-pressure-pressure m)))
                   ((control-change-p m)
                    (make <midi-control-change>
                          :time time :channel ch
                          :controller (control-change-controller m)
                          :value (control-change-value m)))
                   ((program-change-p m)
                    (make <midi-program-change>
                          :time time :channel ch
                          :program (program-change-program m)))
                   ((pitch-bend-p m)
                    (make <midi-pitch-bend>
                          :time time :channel ch
                          :bend (- (+ (pitch-bend-lsb m)
                                      (* 128 (pitch-bend-msb m)))
                                     8192)))
                   ((channel-pressure-p m)
                    (make <midi-channel-pressure>
                          :time time :channel ch
                          :pressure (channel-pressure-pressure m)))
                   (else
                    (err "Message not supported: ~S." m))))
            ((midi-meta-message-p m)
             (cond ((time-signature-p m)
                    (make <midi-time-signature>
                          :time time
                          :numerator (vector-ref data 1)
                          :denominator (expt 2 (vector-ref data 2))
                          :clocks (vector-ref data 3)
                          :32nds (vector-ref data 4)))
                   ((key-signature-p m)
                    (let ((a (vector-ref data 1)))
                      (make <midi-key-signature>
                            :time time
                            :key (or (and (logbit? a #x80) 
                                          (- a #x100)) a)
                            :mode (vector-ref data 2))))
                   ((tempo-change-p m)
                    (make <midi-tempo-change>
                          :time time
                          :usecs (+ (ash (vector-ref data 1) 16)
                                    (ash (vector-ref data 2) 8)
                                    (vector-ref data 3))))
                   ((sequence-number-p m)
                    (make <midi-sequencer-event>
                          :time time
                          :number (+ (ash (vector-ref data 1) 8)
                                     (vector-ref data 2))))
                   ((text-meta-event-p m)
                    (make <midi-text-event>
                          :time time 
                          :type (meta-message-type m)
                          :text (text-meta-event-data-to-string data)))
                   ((eot-p m)
                    (make <midi-eot> :time time))
                   ((smpte-offset-p m)
                    (make <midi-smpte-offset>
                          :time time
                          :offset (loop for i from 1 to 5
                                       collect (vector-ref data i))))
                   ;; FIX! add classes..,
                   ;;((midi-port-p m) )
                   ;;((midi-channel-p m) )
                   (else
                    (err "Shouldnt: message not implemented: ~S." m))))
            ((midi-system-message-p m)
             (let ((type (ldb +enc-lower-status-byte+ m))
                   (size (midimsg-size m))
                   (data '()))
               (cond ((= size 3)
                      (set! data (list (midimsg-data1 m)
                                       (midimsg-data2 m))))
                     ((= size 2)
                      (set! data (list (midimsg-data1 m))))
                     ((sysex-p m)
                      ;; omit markers at beg and end of sysex data.
                      (set! data (loop for i from 1 
                                    below (- size 1)
                                    collect (vector-ref data i)))))
               (make <midi-system-event>
                     :time time :type type :data data)))
            (else
             (err "message not supported: ~S." m))))))

;;;
;;;
;;;

;;(defobject midimsg (event) ; remove at some point.
;;  ((msg :initform 0)
;;   (data :initform #f))
;;  (:parameters time msg data)
;;  (:writers ))

(defobject midi (event)
  ((keynum :initform 60)
   (duration :initform .5)
   (amplitude :initform 64)
   (channel :initform 0 :accessor midi-channel))
  (:parameters time duration keynum amplitude channel)
  (:writers ))

;;;
;;; this macro is used by several write-event methods to
;;; twiddle channel and keynumber values if microtuning
;;; is in effect.
;;;

(define *midi-skip-drum-channel* #f)

(defmacro ensure-microtuning (keyn chan stream)
  `(let ((num #f)
         (rem #f)
         (dat #f)) 
     (cond ((exact? ,keyn) ; ratios are inexact in Guile
	    #f)
           ((symbol? ,keyn)
	    (set! ,keyn (keynum ,keyn)))
	   ((inexact? ,keyn)
            ;; dat = (#t <num> <lim> <off> <wid>)
            (set! dat (midi-stream-tunedata ,stream))
	    (if (null? dat)
	      (set! ,keyn (inexact->exact (round ,keyn)))
	      (if (eq? (car dat) #t)
                ;; note-by-note tuning
	        (begin               
		 ;; SIDE EFFECT: rotate data to next channel
		 (set! num (cadr dat))
		 (let ((int (inexact->exact (floor ,keyn))))
		   (set! rem (- ,keyn int))
		   (set! ,keyn int))
                 ;; if next choice is drum channel skip it
                 (if (and *midi-skip-drum-channel*
                          (= (+ (cadddr dat) num) 8))
                   (incf num))
		 (if (< num (caddr dat))
		   (set! num (+ num 1))
		   (set! num 0))
		 (set-car! (cdr dat) num) ; (setf (second...))
		 (set! ,chan (+ (cadddr dat) num))
		 ;; SIDE EFFECT: write pitch bend 
		 (midi-write-message
		  (make-pitch-bend ,chan rem (car (cddddr dat)))
		  ,stream 0 #f))
	        (begin                  
                 ;; equal microtuning 
		 (set! num (cadr dat)) 
		 (let* ((qkey (quantize ,keyn (/ num)))
			(int (inexact->exact (floor qkey))))
		   (set! rem (- qkey int))
		   (set! ,keyn int))
		 (set! ,chan (+ (car dat)
			        (inexact->exact
			         (floor (* rem num)))))))))
	   (else (err "midi keynum ~s not key number or note."
		      ,keyn)))))

;;;
;;; write-event for midi object and midifiles
;;;

(define-method (write-event (obj <midi>) (mf <midi-file-stream>) time)
  (let ((beats time)
        (scaler (midi-file-scaler mf))
        (keyn (midi-keynum obj))
        (chan (midi-channel obj))
        (ampl (midi-amplitude obj))
	(last #f))
    ;; if amplitude is zero then don't output anything
    (cond ((exact? ampl)
           (if (= ampl 0)
             (set! keyn -1)
             (if (<= 1 ampl 127) #f
                 (err "MIDI: integer amplitude ~s not 0-127 inclusive."
                      ampl))))
          ((inexact? ampl)
           (if (= ampl 0.0)
             (set! keyn -1)
             (if (<= 0.0 ampl 1.0)
               (set! ampl (inexact->exact (floor (* ampl 127))))
               (err "MIDI: float amplitude ~s is not 0.0-1.0 inclusive."
                    ampl))))
          (else 
           (err "MIDI amplitude ~s is not an integer 0-127 or float 0.0-1.0."
                ampl)))
    (ensure-microtuning keyn chan mf)
    ;; if "resting" then dont update anything in the midifile...
    (unless (< keyn 0) ; rest
      (set! last
            (if (null? (%q-head %offs))
              (object-time mf)
              (flush-pending-offs mf beats)))
      (midi-write-message (make-note-on chan keyn ampl)
                          mf
                          (if (> beats last)
                            (inexact->exact (* (- beats last) scaler))
                            0)
                          #f)
      (set! (object-time mf) beats)
      (%q-insert (%qe-alloc %offs
                            (+ beats (midi-duration obj))
                            #f
                            (make-note-off chan keyn 127))
                 %offs))
    (values)))

;;(define-method (write-event (obj <midimsg>) (mf <midi-file-stream>) time)
;;  (let ((data (midimsg-data obj))
;;        (beats time)
;;        (last #f))
;;    (set! last
;;          (if (null? (%q-head %offs))
;;            (object-time mf)
;;            (flush-pending-offs mf beats)))
;;    (cond ((> beats last)
;;           (midi-write-message  
;;            (midimsg-msg obj)  
;;            mf
;;            (inexact->exact
;;              (round (* (- beats last) (midi-file-scaler mf))))
;;            data)
;;           (set! (object-time mf) beats))
;;          (else
;;           (midi-write-message (midimsg-msg obj) mf 0 data))) 
;;    (values)))

(define-method (write-event (obj <midi-event>) (mf <midi-file-stream>)
                            time)
  (multiple-value-bind (msg data)
      (midi-event->midi-message obj)
    (let ((beats time)
          (last #f))
      (set! last
            (if (null? (%q-head %offs))
              (object-time mf)
              (flush-pending-offs mf beats)))
      (cond ((> beats last)
             (midi-write-message  
              msg
              mf
              (inexact->exact
               (round (* (- beats last) (midi-file-scaler mf))))
              data)
             (set! (object-time mf) beats))
            (else
             (midi-write-message msg mf 0 data))) 
      (values))))

;;;
;;; if an object has a method defined on object->midi it can output
;;; to a midifile. The default method signals an error.
;;; 

(define-method (object->midi obj) 
  (err "No object->midi method defined for ~s." obj))

;;;
;;; write-event for non-midi objects call object->midi to
;;; convert the object to the midi message(s) to output.
;;;

(define-method (write-event (obj <object>) (mf <midi-file-stream>) scoretime)
  (write-event (object->midi obj) mf scoretime))

;;;
;;; to write a midi message to a differnt type of score file
;;; set the elements of midi-channel-names to the symbols
;;; names of the instruments
;;;

(define midi-channel-names
  (vector #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f))

(define (midi-channel->name chan)
  (vector-ref midi-channel-names chan))  

(define-method (write-event (obj <midi>) (fil <clm-stream>) scoretime)
  (let ((ins (midi-channel->name (midi-channel obj))))
    (if ins
      (format (io-open fil) "(~a ~s ~s ~s ~s)~%"
	      ins
	      scoretime
	      (midi-duration obj)
	      (hertz (midi-keynum obj))
	      (midi-amplitude obj)))))

(define-method (write-event (obj <midi>) (fil <sco-stream>) scoretime)
  (let ((ins (midi-channel->name (midi-channel obj))))
    (if ins
      (format (io-open fil) "~a ~s ~s ~s ~s~%"
	      ins
	      scoretime
	      (midi-duration obj)
	      (hertz (midi-keynum obj))
	      (midi-amplitude obj)))))

(define-method (write-event (obj <midi>) (fil <clm-audio-stream>) scoretime)
  (let ((ins (midi-channel->name (midi-channel obj))))
    (if ins
      ( (symbol-function ins)		; funcall
	scoretime
	(midi-duration obj)
	(hertz (midi-keynum obj))
	(midi-amplitude obj)))))

;;;
;;; import-events
;;;

; (import-events "Macintosh HD:nm:meta:17:snd:uniform-1.midi")
; (import-events "Macintosh HD:nm:meta:17:snd:uniform-1.midi"
;                :meta-exclude t)

;;; tempo change info. could be made full-blown object 

(define-list-struct tc 
  ticks   ; tick pos of change in track
  scaler  ; tempo scaler: (usecs/1000000)
  offset  ; time offset: (ticks/div)*scaler 
  )

(define-method (import-events (io <midi-file-stream>) . args)
  (with-args (args &key (tracks #t) seq meta-exclude channel-exclude 
                   (time-format ':beats ) tempo exclude-tracks
                   (keynum-format ':keynum)
                   (note-off-stack #t))
    (let ((results '())
          (notefn #f)
          (result #f)
          (class #f)
          (root #f)
          (tempo-map #f)
          (num-tracks #f))
      
      (if exclude-tracks
        (if tracks
          (if (eq? tracks #t) (set! tracks #f)
              (err ":tracks and :exclude-tracks are exclusive keywords."))
          (cond ((integer? exclude-tracks)
                 (set! exclude-tracks (list exclude-tracks)))
                ((pair? exclude-tracks) )
                (else 
                 (err ":exclude-tracks value '~s' not number, list or ~s."
                      exclude-tracks #t))))
        (cond ((eq? tracks #t))
              ((pair? tracks))
              ((integer? tracks)
               (set! tracks (list tracks)))
              (else
               (err ":tracks value '~s' not number, list or ~s."
                    tracks #t))))
      
      (case time-format
        ((:ticks ) #t)
        ((:beats ) #t)
        (else
         (err ":time-format value ~s is not :beats or :ticks."
              time-format)))
      
      (case keynum-format
        ((:keynum #f) #t)
        ((:note )
         (set! notefn #'note)) 
        ((:hertz ) 
         (set! notefn #'hertz))
        (else 
         (err ":keynum-format value '~s' not :keynum, :note or :hertz."
              keynum-format)))
      
      (unless (member channel-exclude '(#t #f))
        (unless (pair? channel-exclude)
          (set! channel-exclude (list channel-exclude)))
        (dolist (e channel-exclude)
          (unless (and (integer? e)
                       (<= +ml-note-off-opcode+ e
                           +ml-pitch-bend-opcode+))
            (err ":channel-exclude value '~s' not a channel message opcode."
                 e))))
      
      (unless (member meta-exclude '(#t #f))
        (unless (pair? meta-exclude)
          (set! meta-exclude (list meta-exclude)))
        (dolist (e meta-exclude)
          (unless (and (integer? e)
                       (or (<= +ml-file-text-event-opcode+ e 
                               +ml-file-cue-point-opcode+)
                           (<= +ml-file-tempo-change-opcode+ e
                               +ml-file-sequencer-event-opcode+)))
            (err ":meta-exclude value '~s' not a meta message opcode."
                 e))))
      
      (with-open-io (file io :input)
        (cond ((= 1 (midi-file-format file))
               (set! num-tracks (midi-file-tracks file))
               (if tracks
                 (if (eq? tracks #t)
                   (set! tracks (loop for i below num-tracks collect i)))
                 (set! tracks (loop for i below num-tracks
                                 unless (member i exclude-tracks)
                                 collect i))))
              (else 
               (set! num-tracks 1) 
               (if (eq? tracks #t) (set! tracks (list 0)))))
        ;; seq can be a seq, class name or name
        (cond ((is-a? seq <seq>))
              ((or (not seq) (find-class seq ))
               (set! class (or seq <seq>))
               (set! root (format #f "~a-track"
                                  (filename-name 
                                   (file-output-filename io))))
               (set! seq #f))
              (else  
               (set! root (format #f "~s" seq))
               (set! class <seq>)
               (set! seq #f)))

        (when (and (eq? time-format ':beats)
                   (not tempo))
          ;; parse tempo track regardless of what tracks are imported.
          ;; this causes track 0 to be visited twice if its a level 0
          ;; file or if user wants track 0 in a level 1. it could be made
          ;; more efficient by changing midi-import-track to side-effect
          ;; a tempo-map slot as it is parsing track zero.
          (set! tempo-map (parse-tempo-map file)))
        
        (dolist (track tracks)
          ;; each track can be number or
          ;; (number <chanexclude> <metaexclude>)
          (when (pair? track)
            (set! channel-exclude (if (number? (second track) )
                                    (list (second track)) 
                                    (second track)))
            (set! meta-exclude (if (number? (third track) )
                                 (list (third track)) 
                                 (third track)))
            (set! track (first track)))
          (cond ((<= 0 track (1- num-tracks))
                 (unless seq
                   (set! seq 
                         (make-instance class 
                                        :name (format #f "~a-~s" root track))))
                 (set! result (midi-file-import-track file track seq
                                                      notefn
                                                      note-off-stack 
                                                      channel-exclude
                                                      meta-exclude))
                 (push result results)
                 (set! seq #f))
                (else
                 (err "track '~s' out of range. Maximum track is ~s."
                      track (- num-tracks 1)))))
        
        (set! results (reverse results))
        
        ;; if importing :beats AND more than one tempo
        ;; then apply tempo map to each imported track
        ;; otherwise apply single tempo value if not :ticks
        (cond ((and (eq? time-format ':beats)
                    (not tempo)
                    (pair? (cdr tempo-map)))
               (let ((div (midi-file-divisions file)))
                 (dolist (tr results)
                   (apply-tempo-map div tempo-map tr))))
              ((not (eq? time-format :ticks))
               (let ((div (midi-file-divisions file))
                     (scaler (if tempo
                               ;; tempo overrides file
                               (/ 60.0 tempo)
                               ;; only one tempo-change
                               (if (pair? tempo-map)
                                 (* (tc-scaler (car tempo-map))
                                    1.0)
                                 ;; MIDI sez default mm=120
                                 .5))))
                 (dolist (tr results)
                   (apply-tempo-scaler div scaler tr)))))
        (if (null? results)
          #f
          (if (null? (cdr results))
            (car results)
            results))))))

(define (apply-tempo-scaler divs scaler track)
  ;; convert ticks to beats (tics/div) and then scale by
  ;; user's tempo factor. rather than divide and multiply
  ;; we combine div and scaler in a single operation:
  ;; (ticks/div)*scaler = ticks*(scaler/div) = ticks*mult
  (let ((mult (/ scaler divs)))
    (dolist (e (subobjects track))
      (set! (object-time e)
            (* (object-time e) mult))
      (when (is-a? e <midi>)
        (set! (midi-duration e)
              (* (midi-duration e) mult))))))

(define (tempo-change->scaler msg data)
  msg
  (/ (+ (ash (vector-ref data 1) 16)
        (ash (vector-ref data 2)  8)
        (vector-ref data 3))
     1000000))

(define (parse-tempo-map mf)
  (midi-file-set-track mf 0)

  ;; map over track 0 and cache tempo change data.
  ;; mapping fn is called after each message is read.
  (let ((res (list))
        (div (midi-file-divisions mf)))
    (midi-file-map-track 
     (lambda (mf)
       ;; get current message data
       (let ((m (midi-file-message mf)))
         (when (tempo-change-p m)
           (push (make-tc :ticks (midi-file-ticks mf)
                          :scaler (tempo-change->scaler
                                   m (midi-file-data mf))
                          :offset 0.0)  ; calc later
                 res))))
     mf)
    (set! res (reverse! res))
    
    ;; add tempo 120 if track did not start with 
    ;; tempo message.
    (when (or (null? res)
              (not (equal? 0 (tc-ticks (car res)))))
      (push (make-tc :ticks 0 :scaler 1/2 :offset 0.0)
            res))

    ;; now calculate time offset of each tempo change.
    ;; time offset equals previous tc's offset plus amount
    ;; of time last tc claimed.
    (unless (null? (cdr res))
      (let ((last (car res)))
        (dolist (this (cdr res))
          (tc-offset-set! this (+ (tc-offset last)
                                  ;; time claimed by last tc
                                  (* (/ (- (tc-ticks this)
                                           (tc-ticks last))
                                        div)
                                     (tc-scaler last))))
          (set! last this))))
    res))

(define (apply-tempo-map divs tmap track)
  (let ((data (subobjects track))
        (this (pop tmap))
        (flag #t))

    (loop while flag
      do
      ;; process data earlier than next tempo change
      ;(format #t "~%~%this=~s time=~s tmap=~s" this 
      ;        (object-time (car data))
      ;        tmap)
      (loop while (and (pair? data)
                       (or (null? tmap)
                           (< (object-time (car data))
                              (tc-ticks (car tmap)))))
        do
        ;(format #t "~%scaling ~s with ~s" (car data) this)
        ;; set object's time to number of beats since
        ;; tempo change scaled by tempo
        ;; plus time offset of current tempo.
        (set! (object-time (car data))
              (+ (tc-offset this)   ; onset time of last tempo change
                 ;;  scale to time
                 (* (tc-scaler this)
                    ;; convert ticks since last tempo change to beats
                    (/ (- (object-time (car data))
                          (tc-ticks this))
                       divs))))
        ;; scale duration of midi notes using current tempo.
        ;; this isnt quite right since the actual note off
        ;; may have been later than the next tempo change...
        (when (is-a? (car data) <midi>)
          (set! (midi-duration (car data))
                (* (/ (midi-duration (car data))
                      divs)
                   (tc-scaler this)
                   1.0)))
        (set! data (cdr data)))
      ;; now data ticks equal or greater than next tempo
      (unless (null? tmap)
        (set! this (pop tmap)))
      (if (null? data)
        (set! flag #f)))
    track))

(define (midi-file-import-track file track seq notefn 
                                note-off-stack channel-exclude 
                                meta-exclude)
  (let* ((data '())
         (tabl (make-hash-table 31)))
    (let ((mapper
           (lambda (mf)
             (let* ((b (midi-file-ticks mf))
                    (m (midi-file-message mf))
                    (s (channel-message-opcode m))
                    (n #f))
               (cond 
                ((channel-message-p m)
                 (cond
                  ((and channel-exclude
                        (or (eq? channel-exclude #t)
                            (member s channel-exclude)))
                   (set! n #f))
                  ((or (= s +ml-note-off-opcode+)
                       (and (= s +ml-note-on-opcode+)
                            (= 0 (channel-message-data2 m))))
                   (let ((on (if note-off-stack
                               (let ((l (hash-ref tabl
                                                  (channel-note-hash m)))
                                     (v #f))
                                 (when (and l (not (null? l)))
                                   (set! v (car l))
                                   (hash-set! tabl
                                              (channel-note-hash m)
                                              (cdr l))) 
                                 v)
                               (let ((x (hash-ref tabl 
                                                  (channel-note-hash m))))
                                 (if x
                                   (if (not (null? (cdr x)))
                                     (let* ((tail (loop with a = x
                                                        and b = (cdr x)
                                                        until (null?
                                                               (cdr b))
                                                        do
                                                        (set! a b)
                                                        (set! b (cdr b))
                                                        finally 
                                                        (return a)))
                                            (obj (cadr tail)))
                                       (set! (cdr tail) (list))
                                       obj)
                                     (progn
                                       (hash-set! tabl 
                                                  (channel-note-hash m)
                                                  (list))
                                       (car x)))
                                   #f)))))
                     (if on
                       (set! (midi-duration on) 
                             (- b (object-time on)))
                       (format #t "~%No Note-On for channel ~s keynum ~s."
                               (channel-message-channel m)
                               (channel-message-data1 m)))))
                  ((= s +ml-note-on-opcode+)
                   (set! n
                         (make-instance
                           <midi>
                           :time b
                           :keynum 
                           (if notefn
                             ( notefn
                                      (channel-message-data1 m))
                             (channel-message-data1 m))
                           :channel (channel-message-channel m)
                           :amplitude (/ (channel-message-data2 m) 
                                         127.0)))
                   (let ((v (hash-ref tabl (channel-note-hash m))))
                     (if (not v)
                       (hash-set! tabl (channel-note-hash m)
                                  (list n ))
                       (hash-set! tabl (channel-note-hash m)
                                  (cons n v)))))))
                ((meta-message-p m)
                 (unless (and meta-exclude
                              (or (eq? meta-exclude #t) 
                                  (eot-p m)
                                  (member (ldb +enc-data-1-byte+ m)
                                          meta-exclude)))
                   ;;(set! n (make-instance <midimsg>
                   ;;          :time b :msg m
                   ;;          :data (midi-file-data mf)))
                   (set! n (midi-message->midi-event
                            m :time b :data (midi-file-data mf)))
                   ))
                (else
                 ;;(set! n (make-instance <midimsg> :time b :msg m))
                 (set! n (midi-message->midi-event m :time b))
                 ))
               (when n (push n data))))))  ; end #'mapper
      (midi-file-set-track file track)
      (midi-file-map-track #'mapper file ))
    (set! (subobjects seq)
          (reverse! data))
    seq))





