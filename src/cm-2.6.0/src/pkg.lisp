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

;;; $Name: rel-2_6_0 $
;;; $Revision: 1.21 $
;;; $Date: 2005/02/26 14:31:10 $

(in-package :cl-user)

;;;
;;; The CM package definition. Keywords are used so that the names
;;; will reflect the inplementation's read-case sensitivity but will
;;; not pollute the current package's name space with any new symbols.
;;;
;;; The CLM, CMN, and MidiShare packages must exist when this file is
;;; loaded, either by loading the systems or their stubs files.
;;;

(defpackage :cm
  (:use :common-lisp )
  (:shadow :make-load-form
           ;; have to block these from CLM
           :io :ran :exit :quit :play :graph )
  ;; use keywords instead of strings for case sensitive lisps.
  (:import-from :clm 
                :mus-next
                :mus-bshort
                :mus-aifc
                :mus-riff
                :mus-lshort
                :*clm-with-sound-depth*
                :wsdat-play
                :init-with-sound
                :finish-with-sound
                :*clm-channels*
                :*clm-srate*
                :clm-load
                :*definstrument-hook*
                ;; these are also used by CM but defs don't conflict.
                ;#+(and clm2 (not clm3)) :graph 
                :spectrum :env :src 
                )
  (:import-from :cmn 
                :init-clm-input
                :*exact-rhythms*
                :score
                :staff-descriptors
                :stfdat-staff
                :staff-data 
                :set-staff-number
                :set-staff-clef
                :finish-clm-input
                :find-staff
                :add-staff
                :add-data-1
                :add-note-to-staff)
  (:import-from :midishare
		:midishare :midiGetVersion :MidiOpen :MidiClose :MidiCountAppls
		:MidiGetNamedAppl :MidiGetIndAppl :MidiErrIndex :MidiGetName
		:MidiConnect :MidiGetTime :MidiIsConnected
		:MidiSendIm :MidiSend :MidiSendAt :MidiAddSeq
		:typeNote :typeKeyOn :typeKeyOff :typeKeyPress :typeCtrlChange
		:typeProgChange :typeChanPress :typePitchWheel :typePitchBend
		:typeSongPos :typeSongSel :typeClock :typeStart :typeContinue
		:typeStop :typeTune :typeActiveSens :typeReset :typeSysEx
		:typeStream :typePrivate :typeSeqNum :typeTextual
		:typeCopyright :typeSeqName :typeInstrName :typeLyric
		:typeMarker :typeCuePoint :typeChanPrefix :typeEndTrack
		:typeTempo :typeSMPTEOffset :typePortPrefix :typeKeySign
		:typeTimeSign :MidiNewEv :port :chan :field :bend :text :port
		:ref :date :evtype :MidiCopyEv :MidiFreeEv :MidiAddField
                :midiTask :midiGetEv :MidiSetRcvAlarm
                :nullptr :nullptrp :MidiFlushEvs 
                ;;:MidiShareSync :MidiOpenSync :MidiCloseSync :MidiGetSyncEv
                
		:OpenPlayer :ClosePlayer :midiNewSeq
		:StartPlayer :ContPlayer :StopPlayer :PausePlayer
		:kMuteOn :kMuteOff :kSoloOn :kSoloOff :kMute :kSolo
		:kExternalSync :kInternalSync :kClockSync :kSMPTESync
		:GetAllTrackPlayer :SetAllTrackPlayer
		:GetTrackPlayer :SetTrackPlayer
		:SetParamPlayer :SetTempoPlayer
		:TicksPerQuarterNote
		:SetSynchroInPlayer :MidiNewMidiFileInfos
		:MidiFileLoad :MidiFileSave :mf-clicks :mf-format :mf-timedef)
  (:export

   ;; data.lisp
   :interval :decode-interval :*tempo* :*beat* :rhythm :*softest* :*loudest*
   :*power* :amplitude :interpl :interp :tendency :rescale-envelope :quantize
   :rescale :fit :cents->scaler :scaler->cents :between :pick :pickl :vary
   :drunk :odds :shuffle :ran :expl :explsegs :explseg :geosegs :geoseg
   :best-normal-form :prime-form :markov-analyze :histogram

   ;; objects.lisp
   :copy-object :fill-object :*print-instance* :print-instance :save-object
   :object-name :object-time :rename-object :list-named-object :find-object
   :container :seq :subobjects :subcontainers :map-subobjects 
   :map-subcontainers :insert-object :append-object :remove-object
   :list-subobjects :new :event :*time-slots* :defobject :process :defprocess

   ;; scales.lisp
   :tuning :mode :*scale* :*chromatic-scale* :rest?
   :note :keynum :hertz :transpose :invert :scale=
   :scale< :scale> :scale<= :scale>= :interval

  )
)

;;;
;;; intern and export ms:new and ms:MidiPrintEv
;;;

(let ((syms '(#:new  #:MidiPrintEv)))
  (map nil (lambda (s) (intern (symbol-name s) :midishare)) syms)
  (export (mapcar #'(lambda (x) (find-symbol (symbol-name x) :midishare))
                  syms)
          :midishare))
