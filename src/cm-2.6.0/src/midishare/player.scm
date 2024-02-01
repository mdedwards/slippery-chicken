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
;;; $Revision: 1.6 $
;;; $Date: 2005/01/31 14:26:25 $
;;;
;;; Support for Grame's Player sequencer app.

(define-class <player-stream> (<midishare-stream>)
  (seq :accessor player-stream-seq)
  (track :init-value #f :accessor player-stream-track)
  (mode :init-value :replace :accessor player-stream-mode
        :init-keyword :seq-mode)
  (play :init-value #t :accessor player-stream-play
        :init-keyword :play)
  (tempo :init-value 60.0 :accessor player-stream-tempo
         :init-keyword :tempo)
  :name 'player-stream
  :metaclass <io-class>
  :file-types '("*.mp")
  :mime-type "audio/x-player"
  )

(define-method (open-midishare-client (obj <player-stream>)
                                      name)
  (ms:OpenPlayer name))

(define-method (close-midishare-client (obj <player-stream>))
  (ms:ClosePlayer (midishare-stream-refnum obj)))

(define (player-stream-current-track obj)
  ;; current track number cached as 4th element
  (fourth (event-stream-stream obj)))

(define (player-stream-current-track-set! obj track)
  (list-set! (event-stream-stream obj) 3 track))

(define-method (initialize-io (obj <player-stream>))
  (channel-tuning-init obj)
  (let ((trk (or (player-stream-track obj)
                 1)))
    ;; :replace mode sets tracknum to a user specified num or 1
    ;; :add mode initializes tracknum if it is not already set and
    ;; increments the track number after each pass.
    (case (player-stream-mode obj)
      ((:replace replace)
       (player-stream-current-track-set! obj trk))
      ((:add add)
       (unless (player-stream-current-track obj)
         (player-stream-current-track-set!
          obj trk)))
      (else
       (err "Player mode ~s not ~S, :replace, or :add."
            false (player-stream-mode obj))))
    ;; allocate new seq to store events this pass.
    (set! (player-stream-seq obj) (MidiNewSeq))
    obj))

(define-method (deinitialize-io (obj <player-stream>))
  (next-method)
  ;; pass completed seq to player
  ;; If play is #t then the seq is played.
  ;; 
  (let ((refn (midishare-stream-refnum obj))
        (mode (player-stream-mode obj))
        (trkn (player-stream-current-track obj))
        (data (player-stream-seq obj))
        (play (player-stream-play obj) ))
    ;; :replace mode resets player with  new seq
    ;; :add mode adds seq as next track and then
    ;; increments the tracknum for the next run.
    (case mode
      ((:replace replace)
       (ms:SetAllTrackPlayer refn data 500))
      ((:add add)
       (ms:SetTrackPlayer refn trkn data)
       ;; increment track number
       (player-stream-current-track-set! obj (+ trkn 1)))
      ((#f )
       (set! play #f))
      (else
       (err "Player mode ~s not :replace or :add." mode)))

    ;; call player if :play is t
    (when play (player-play obj))
    obj))

;;;
;;; write-midi-message and write-event
;;;

;(define-method (midi-write-message (msg <number>) (obj <player-stream>)
;                                   time data)
;  (let ((trk (player-stream-current-track obj))
;        (seq (player-stream-seq obj))
;        (evt (midi-message->midiEv msg data)))
;    (ref evt trk)
;    (date evt (or time 0))
;    (ms:MidiAddSeq seq evt)
;    msg))

(define-method (write-event (obj <midi>) (stream <player-stream>) 
                            scoretime)
  (let* ((seq (player-stream-seq stream))
         (trk (player-stream-current-track stream))
         (beg (floor (* scoretime 1000)))
         (dur (floor (* (midi-duration obj) 1000)))
         (key (midi-keynum obj))
         (amp (midi-amplitude obj))
         (loc
          (logical-channel (midi-channel obj)
                           (midi-stream-channel-map stream)))
         (prt (car loc))
         (chn (cadr loc))
         (evt nil))
    (cond ((and (integerp amp) (<= 0 amp 127)) #f)
          ((and (floatp amp) (<= 0.0 amp 1.0))
           (set! amp (floor (* amp 127))))
          (else
           (error "Can't convert amplitude ~s to midi velocity."
                  amp)))
    (ensure-microtuning key chn stream)
    (set! evt (ms:new typeNote :port prt :chan chn 
                      :pitch key :vel amp :dur dur))
    (ms:ref evt trk)
    (ms:date evt beg)
    (ms:MidiAddSeq seq evt)
    (values)))

;;;
;;; Top-level control over player and sequences
;;;
              
(define-method (player-play (obj <player-stream>))
  (player-set-sync obj kExternalSync)
  ;; MS per Quarter
  (player-set-tempo obj (player-stream-tempo obj))
  (player-start obj)
  obj)
  
;(define-method (player-clear obj <player-stream>)
;  ;; free seq and reset internal track number
;  (let ((seq (player-stream-seq obj)))
;    (when (player-stream-seq obj)
;      (MidiFreeSeq seq))
;    (set! (player-stream-seq obj) #f)
;    (player-stream-current-track-set! obj #f)
;    (values)))

(define-method (player-set-sync (obj <player-stream>) sync)
  (if (or (eq? sync kInternalSync)
          (eq? sync kClockSync)
          (eq? sync kSMPTESync)
          (eq? sync kExternalSync))
    (let ((ref (midishare-stream-refnum obj)))
      (ms:SetSynchroInPlayer ref kExternalSync))
    (err "~s not a player sync value." sync))
  (values))

(define (bpm->usec bpm)
  (inexact->exact (floor (* (/ 60 bpm) 1000000))))

(define-method (player-set-tempo (obj <player-stream>) bpm)
  (let ((ref (midishare-stream-refnum obj)))
    (if ref
      (let ((usec (bpm->usec bpm)))
        (setf (player-stream-tempo obj) bpm)
        (ms:SetTempoPlayer ref usec))
      (err "~s not open." obj))
    obj))

(define-method (player-start (obj <player-stream>))
  (let ((ref (midishare-stream-refnum obj)))
    (unless ref (err "~s not open." obj))
    (ms:StartPlayer ref)
    (ms:SetTempoPlayer ref (bpm->usec (player-stream-tempo obj)))    
    obj))

(define-method (player-stop (obj <player-stream>))
  (let ((ref (midishare-stream-refnum obj)))
    (unless ref (err "~s not open." obj))
    (ms:StopPlayer ref)
    obj))

(define-method (player-pause (obj <player-stream>))
  (let ((ref (midishare-stream-refnum obj)))
    (unless ref (err "~s not open." obj))
    (ms:PausePlayer ref)
    obj))

(define-method (player-cont (obj <player-stream>))
  (let ((ref (midishare-stream-refnum obj)))
    (unless ref (err "~s not open." obj))
    (ms:ContPlayer ref)
    (ms:SetTempoPlayer ref (bpm->usec (player-stream-tempo obj)))
    obj))

;;;
;;; soloing/muting
;;;

(define-method (player-mute (obj <player-stream>) track)
  (let ((ref (midishare-stream-refnum obj)))
    (unless ref (err "~s not open." obj))
    (ms:SetParamPlayer ref track kMute kMuteOn)
    obj))

(define-method (player-unmute (obj <player-stream>) track)
  (let ((ref (midishare-stream-refnum obj)))
    (unless ref (err "~s not open." obj))
    (ms:SetParamPlayer ref track kMute kMuteOff)
    obj))

(define-method (player-solo (obj <player-stream>) track)
  (let ((ref (midishare-stream-refnum obj)))
    (unless ref (err "~s not open." obj))
    (ms:SetParamPlayer ref track kSolo kSoloOn)
    obj))

(define-method (player-unsolo (obj <player-stream>) track)
  (let ((ref (midishare-stream-refnum obj)))
    (unless ref (err "~s not open." obj))
    (ms:SetParamPlayer ref track kSolo kSoloOff)
    obj))

;;;
;;; Sequences and MIDI files
;;;

(define-method (player-load-midifile (obj <player-stream>) file)
  (let ((seq (MidiNewSeq))
        (info (MidiNewMidiFileInfos)))
    (ms:MidiFileLoad file seq info)
    (set! (player-stream-seq obj) seq)
    obj))

(define-method (player-save-midifile (obj <player-stream>) file)
  (let ((ref (midishare-stream-refnum obj)))
    (unless ref (err "~s not open." obj))
    (let* ((info (ms:MidiNewMidiFileInfos))
           (seq (ms:GetAllTrackPlayer ref)))
      (ms:mf-format info 1) ; level 1
      (ms:mf-timedef info TicksPerQuarterNote)
      (ms:mf-clicks info 500)
      (ms:MidiFileSave file seq info)
      (ms:MidiFreeSeq seq)
      (ms:MidiFreeMidiFileInfos info)
      obj)))




