;;; **********************************************************************
;;; Copyright (C) 2003 Heinrich Taube (taube@uiuc.edu) 
;;; 
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; **********************************************************************

;;; this file is compiled and loaded in place Midishare-Interface.lisp

(defpackage :midishare
  (:use :common-lisp)
  (:nicknames :ms))

(in-package :midishare)

(defmacro defstub (name )
  (let ((f (and (consp name) (eq (car name) 'setf))))
    `(defun ,name , (if f '(a b) '(&rest args))
       #-cmu (error "Attempt to call ~S without MIDISHARE loaded."
                    ',name)
       , (if f '(values a b) '(values args)))))

(proclaim '(special typeNote typeKeyOn typeKeyOff typeKeyPress 
            typeCtrlChange typeProgChange typeChanPress typePitchWheel 
            typePitchBend typeSongPos typeSongSel typeClock typeStart
            typeContinue typeStop typeTune typeActiveSens typeReset
            typeSysEx typeStream typePrivate typeSeqNum typeTextual
            typeCopyright typeSeqName typeInstrName typeLyric
            typeMarker typeCuePoint typeChanPrefix typeEndTrack
            typeTempo typeSMPTEOffset typePortPrefix typeKeySign
            typeTimeSign
            kMuteOn kMuteOff kSoloOn kSoloOff kMute kSolo 
            kExternalSync kInternalSync kClockSync kSMPTESync
            TicksPerQuarterNote))

(defparameter %%no-midishare 0) ;; stop CMU compiler optimization

(defun midishare ()
  ;; return false since MidiShare is not around
  %%no-midishare)

;(defun midisharesync ()  %%no-midishare)

(defstub midiGetVersion)
(defstub MidiOpen)
(defstub MidiClose)
(defstub MidiCountAppls)
(defstub MidiGetNamedAppl)
(defstub MidiGetIndAppl)
(defstub MidiErrIndex)
(defstub MidiGetName)
(defstub MidiConnect)
(defstub MidiGetTime)
(defstub MidiIsConnected)
(defstub MidiSendIm)
(defstub MidiSend)
(defstub MidiSendAt)
(defstub MidiNewEv)
(defstub port)
(defstub chan)
(defstub field)
(defstub bend)
(defstub text)
(defstub ref)
(defstub date)
(defstub evtype)
(defstub MidiCopyEv)
(defstub MidiFreeEv)
(defstub MidiAddField)
;(defstub MidiOpenSync)
;(defstub MidiCloseSync)
;(defstub MidiGetSyncEv)
(defstub nullptrp)
(defstub nullptr)
(defstub MidiFlushEvs)
(defstub MidiTask)
(defstub MidiSetRcvAlarm)
(defstub MidiGetEv)

;;; player

(defstub OpenPlayer)
(defstub ClosePlayer)
(defstub MidiNewSeq)
(defstub MidiAddSeq)
(defstub StartPlayer)
(defstub ContPlayer)
(defstub StopPlayer)
(defstub PausePlayer)
(defstub GetAllTrackPlayer)
(defstub SetAllTrackPlayer)
(defstub GetTrackPlayer)
(defstub SetTrackPlayer)
(defstub SetParamPlayer)
(defstub SetTempoPlayer)
;(defstub TicksPerQuarterNote)
(defstub SetSynchroInPlayer)
(defstub MidiNewMidiFileInfos)
(defstub MidiFileLoad)
(defstub MidiFileSave)
(defstub mf-clicks)
(defstub mf-format)
(defstub mf-timedef)
(defstub MidiFreeMidiFileInfos)
(defstub MidiFreeSeq)

(export '(midishare midiGetVersion MidiOpen MidiClose MidiCountAppls
          MidiGetNamedAppl MidiGetIndAppl MidiErrIndex MidiGetName
          MidiConnect MidiGetTime MidiIsConnected 
          MidiSendIm MidiSend MidiSendAt 
          typeNote typeKeyOn typeKeyOff typeKeyPress typeCtrlChange 
          typeProgChange typeChanPress typePitchWheel typePitchBend
          typeSongPos typeSongSel typeClock typeStart typeContinue
          typeStop typeTune typeActiveSens typeReset typeSysEx
          typeStream typePrivate typeSeqNum typeTextual
          typeCopyright typeSeqName typeInstrName typeLyric
          typeMarker typeCuePoint typeChanPrefix typeEndTrack
          typeTempo typeSMPTEOffset typePortPrefix typeKeySign
          typeTimeSign MidiNewEv port chan field bend text ref date
          evtype MidiCopyEv MidiFreeEv MidiAddField MidiTask
          MidiSetRcvAlarm MidiGetEv nullptrp nullptr MidiFlushEvs
          ;; MidiShareSync MidiOpenSync MidiCloseSync MidiGetSyncEv
          ;; player
          OpenPlayer ClosePlayer midiNewSeq MidiAddSeq
          StartPlayer ContPlayer StopPlayer PausePlayer
          kMuteOn kMuteOff kSoloOn kSoloOff kMute kSolo 
          kExternalSync kInternalSync kClockSync kSMPTESync
          GetAllTrackPlayer SetAllTrackPlayer
          GetTrackPlayer SetTrackPlayer 
          SetParamPlayer SetTempoPlayer
          TicksPerQuarterNote
          SetSynchroInPlayer MidiNewMidiFileInfos
          MidiFileLoad MidiFileSave mf-clicks mf-format mf-timedef
          MidiFreeMidiFileInfos MidiFreeSeq
          )
        :midishare)