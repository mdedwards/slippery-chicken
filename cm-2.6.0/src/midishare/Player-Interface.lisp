;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Player-Interface.lisp
;;
;;  Copyright (c) 1996,2002 GRAME.  All rights reserved.
;;
;;  
;;   Interface to the PlayerSharedLibrary
;;
;;   09/25/96 : Version 1.0
;;   07/07/97 : Version 1.11 New SendEvPlayer function, new kEventSync mode 
;;   18/03/98 : Version 2.0  New kExternalSync mode, new SetTempo function
;;   21/06/01 : Suppression de SendEvPlayer et SendSeqPlayer
;;   05/07/01 : Nettoyage
;;   26/11/02 : Interface for MacOSX
;;   03/09/03 : Add install-player-interface and remove-player-interface functions
;;   24/10/03 : Export Version symbol
;;   04/11/03 : Correct MCL 5.0 SetRecordModePlayer and 
;;              SetRecordFilterPlayer entry-points.
;;   21/11/03 : Converted to functional API. (HKT)
;;   19/04/03 : Add MidiFreeMidiFileInfos, MidiFreePlayerState and MidiFreePos
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :midishare)

(export '(kIdle kPause kRecording kPlaying kWaiting kMaxTrack kMuteOn
          kMuteOff kSoloOn kSoloOff kMute kSolo kNoTrack kEraseMode
          kMixMode kLoopOn kLoopOff kStepPlay kStepMute kInternalSync
          kClockSync kSMPTESync kExternalSync kNoSyncOut kClockSyncOut
          midifile0 midifile1 midifile2 TicksPerQuarterNote Smpte24
          Smpte25 Smpte29 Smpte30 noErr ErrOpen ErrRead ErrWrite ErrVol
          ErrGetInfo ErrSetInfo ErrMidiFileFormat PLAYERnoErr
          PLAYERerrAppl PLAYERerrEvent PLAYERerrMemory PLAYERerrSequencer
          player-framework p-bar p-beat p-unit s-bar s-beat s-unit s-date
          s-tempo s-num s-denom s-click s-quarter s-state
          s-syncin s-syncout
          mf-format mf-timedef mf-clicks mf-tracks
          OpenPlayer ClosePlayer open-player
          StartPlayer ContPlayer StopPlayer PausePlayer
          SetRecordModePlayer RecordPlayer SetRecordFilterPlayer
          SetPosBBUPlayer SetPosMsPlayer 
          SetLoopPlayer SetLoopStartBBUPlayer
          SetLoopEndBBUPlayer SetLoopStartMsPlayer SetLoopEndMsPlayer
          SetSynchroInPlayer SetSynchroOutPlayer SetSMPTEOffsetPlayer
          SetTempoPlayer GetStatePlayer GetEndScorePlayer 
          ForwardStepPlayer BackwardStepPlayer GetAllTrackPlayer
          GetTrackPlayer SetTrackPlayer SetAllTrackPlayer SetParamPlayer
          InsertAllTrackPlayer InsertTrackPlayer MidiFileSave
          MidiFileLoad midi-file-save midi-file-load MidiNewMidiFileInfos MidiFreeMidiFileInfos
          MidiNewPlayerState MidiFreePlayerState MidiNewPos MidiFreePos Version
          )
        :midishare)


;;===============================
;; Date structures ond constants
;;===============================

;;-------------------------------------------------------------------------- 
;; Player state   
;;-------------------------------------------------------------------------- 

(defconstant kIdle       0)
(defconstant kPause  	 1)
(defconstant kRecording  2)
(defconstant kPlaying    3)
(defconstant kWaiting    4)


;;-------------------------------------------------------------------------- 
;; Tracks state   
;;-------------------------------------------------------------------------- 

(defconstant kMaxTrack	256)
(defconstant kMuteOn  1)
(defconstant kMuteOff 0)
(defconstant kSoloOn  1)
(defconstant kSoloOff 0)
(defconstant kMute  0)
(defconstant kSolo  1)

;;-------------------------------------------------------------------------- 
;; Recording management  
;;-------------------------------------------------------------------------- 

(defconstant kNoTrack		-1)
(defconstant kEraseMode  	1)
(defconstant kMixMode 		0)

;;-------------------------------------------------------------------------- 
;; Loop  management  
;;-------------------------------------------------------------------------- 

(defconstant kLoopOn  	0)
(defconstant kLoopOff 	1)

;;-------------------------------------------------------------------------- 
;; Step Playing  
;;-------------------------------------------------------------------------- 

(defconstant kStepPlay  1)
(defconstant kStepMute  0)

;;-------------------------------------------------------------------------- 
;; Synchronisation  
;;-------------------------------------------------------------------------- 

(defconstant kInternalSync	0)
(defconstant kClockSync  	1)
(defconstant kSMPTESync 	2)
(defconstant kExternalSync 	3)

(defconstant kNoSyncOut	0)
(defconstant kClockSyncOut 1)

;;-------------------------------------------------------------------------- 
;; MIDIfile  
;;-------------------------------------------------------------------------- 

(defconstant midifile0  0)
(defconstant midifile1  1)
(defconstant midifile2  2)
  
(defconstant TicksPerQuarterNote     0)
(defconstant Smpte24                 24)
(defconstant Smpte25                 25)
(defconstant Smpte29                 29)
(defconstant Smpte30                 30)
  
  
;;-------------------------------------------------------------------------- 
;; Errors  :  for MidiFile
;;-------------------------------------------------------------------------- 
 
(defconstant noErr               0)   ;; no error
(defconstant ErrOpen             1)   ;; file open error       
(defconstant ErrRead             2)   ;; file read error       
(defconstant ErrWrite            3)   ;; file write error      
(defconstant ErrVol              4)   ;; Volume error          
(defconstant ErrGetInfo          5)   ;; GetFInfo error        
(defconstant ErrSetInfo          6)   ;; SetFInfo error        
(defconstant ErrMidiFileFormat   7)   ;; bad MidiFile format   

;;-------------------------------------------------------------------------- 
;; Errors  : for the player
;;-------------------------------------------------------------------------- 

(defconstant PLAYERnoErr           -1)  ;; No error            
(defconstant PLAYERerrAppl         -2)  ;; Unable to open MidiShare app  
(defconstant PLAYERerrEvent        -3)  ;; No more MidiShare Memory
(defconstant PLAYERerrMemory       -4)  ;; No more Mac Memory
(defconstant PLAYERerrSequencer    -5)  ;; Sequencer error



#+(and apple mcl powerpc)

#-ccl-5.0

;; The PlayerSharedPPC file must be located either in the 
;; System Folder/Extension Folder of in the same folder as
;; the PlayerPPC.lisp file.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :ff)
  (ccl::add-to-shared-library-search-path "PlayerSharedPPC"))


#+ccl-5.0

;; Utilities
;;===========

(progn 
  (defvar *player-framework* nil)
  
  (defun player-framework ()
    (or *player-framework*
        (setq *player-framework*
              (load-framework-bundle "Player.framework"))))
  
)


#+(and apple mcl powerpc)

;;================================
;; Record for position management
;;================================

(progn 

(defrecord Pos
  (bar  :short)  
  (beat :short)     
  (unit :short))

(defun MidiNewPos ()
  (ccl:make-record :Pos))

(defun MidiFreePos (pos)
  (ccl:dispose-record pos))

(defun p-bar (e &optional (d nil d?))
  (if d?
    (rset e :Pos.bar d)
    (rref e :Pos.bar)))

(defun p-beat (e &optional (d nil d?))
  (if d?
    (rset e :Pos.beat d)
    (rref e :Pos.beat)))
    
(defun p-unit (e &optional (d nil d?))
  (if d?
    (rset e :Pos.unit d)
    (rref e :Pos.unit)))


;;==============================
;; Record for state management
;;==============================

(defrecord PlayerState
  (date  :longint)
  (tempo :longint)
  (tsnum :short)
  (tsdenom :short)
  (tsclick :short)
  (tsquarter :short)
  (bar  :short)   
  (beat :short)     
  (unit  :short)
  (state  :short)
  (syncin  :short)
  (syncout  :short))

(defun MidiNewPlayerState ()
  (ccl:make-record :PlayerState))

(defun MidiFreePlayerState (state)
  (ccl:dispose-record state))

(defun s-bar (e )
   (rref e :PlayerState.bar))

(defun s-beat (e)
  (rref e :PlayerState.beat))
 
(defun s-unit (e)
  (rref e :PlayerState.unit))

(defun s-date (e )
  (rref e :PlayerState.date))

(defun s-tempo (e)
  (rref e :PlayerState.tempo))

(defun s-num (e)
  (rref e :PlayerState.tsnum))

(defun s-denom (e)
  (rref e :PlayerState.tsdenom))

(defun s-click (e )
  (rref e :PlayerState.tsclick))

(defun s-quarter (e )
  (rref e :PlayerState.tsquarter))

(defun s-state (e )
  (rref e :PlayerState.state))

(defun s-syncin (e )
  (rref e :PlayerState.syncin))

(defun s-syncout (e )
  (rref e :PlayerState.syncout))



;; HKT: REMOVED Already defined check
;;================
;(unless (macro-function 'mf-format)
  
  
;;======================
;; Record for MidiFile
;;======================
  
(defrecord MidiFileInfos
  (format  :longint)     
  (timedef  :longint)   
  (clicks :longint)      
  (tracks  :longint)  )   

(defun MidiNewMidiFileInfos ()
  (ccl:make-record :MidiFileInfos))

(defun MidiFreeMidiFileInfos (info)
  (ccl:dispose-record info))
  
(defun mf-format (e &optional (d nil d?))
  (if d?
      (rset e :MidiFileInfos.format d)
    (rref e :MidiFileInfos.format)))
  
(defun mf-timedef (e &optional (d nil d?))
  (if d?
      (rset e :MidiFileInfos.timedef d)
      (rref e :MidiFileInfos.timedef)))
  
(defun mf-clicks (e &optional (d nil d?))
  (if d?
      (rset e :MidiFileInfos.clicks d)
      (rref e :MidiFileInfos.clicks)))
  
(defun mf-tracks (e )
  (rref e :MidiFileInfos.tracks))
  
#-ccl-5.0
(eval-when (:compile-toplevel :load-toplevel :execute)

  ;; Interface to C entry points
  ;;================================
  
  (define-entry-point 
    (%version ("PlayerSharedPPC" "Version")) () :long)
  (defun version () (%version))
  
  (define-entry-point 
    (%openplayer ("PlayerSharedPPC" "OpenPlayer")) 
    ((name :ptr)) :short)
  (defun openplayer (name)
    (with-pstrs ((pstr name))
      (%openplayer pstr)))
  (defun open-player (name) (openplayer name))
  
  (define-entry-point 
    (%closeplayer ("PlayerSharedPPC" "ClosePlayer"))
    ((refnum :short)))
  (defun closeplayer (n) (%closeplayer n))
  
  ;; Transport control
  ;;===================
  
  (define-entry-point (%startplayer ("PlayerSharedPPC" "StartPlayer"))
    ((refnum :short)))
  (defun startplayer (refnum) (%startplayer refnum))
  
  (define-entry-point (%contplayer ("PlayerSharedPPC" "ContPlayer"))
    ((refnum :short)))
  (defun contplayer (refnum) (%contplayer refnum))
  
  (define-entry-point (%stopplayer ("PlayerSharedPPC" "StopPlayer"))
    ((refnum :short)))
  (defun stopplayer (refnum) (%stopplayer refnum))
  
  (define-entry-point (%pauseplayer ("PlayerSharedPPC" "PausePlayer"))
    ((refnum :short)))
  (defun pauseplayer (refnum) (%pauseplayer refnum))
  
  ;; Record management
  ;;===================
  
  (define-entry-point 
    (%setrecordmodeplayer ("PlayerSharedPPC" "SetRecordModePlayer"))
    ((refnum :short) (state :short)))
  (defun setrecordmodeplayer (refnum state)
    (%setrecordmodeplayer refnum state))
  
  (define-entry-point (%recordplayer ("PlayerSharedPPC" "RecordPlayer"))
    ((refnum :short) (tracknum :short)))
  (defun recordplayer (refnum tracknum) (%recordplayer refnum tracknum))
  
  (define-entry-point
    (%setrecordfilterplayer ("PlayerSharedPPC" "SetRecordFilterPlayer"))
    ((refnum :short) (filter :ptr)))
  (defun setrecordfilterplayer (refnum filter)
    (%setrecordfilterplayer refnum filter))
  
  ;; Position management
  ;;=====================
  
  (define-entry-point 
    (%setposbbuplayer ("PlayerSharedPPC" "SetPosBBUPlayer"))
    ((refnum :short) (pos :ptr)))
  (defun setposbbuplayer (refnum pos) (%setposbbuplayer refnum pos))
  
  (define-entry-point 
    (%setposmsplayer ("PlayerSharedPPC" "SetPosMsPlayer"))
    ((refnum :short) (date_ms :longint)))
  (defun setposmsplayer (refnum date_ms) (%setposmsplayer refnum date_ms))
  
  ;; Loop management
  ;;==================
  
  (define-entry-point (%setloopplayer ("PlayerSharedPPC" "SetLoopPlayer"))
    ((refnum :short) (state :short)))
  (defun setloopplayer (refnum state) (%setloopplayer refnum state))
  
  (define-entry-point
    (%setloopstartbbuplayer ("PlayerSharedPPC" "SetLoopStartBBUPlayer"))
    ((refnum :short) (pos :ptr)) :long)
  (defun setloopstartbbuplayer (refnum pos) 
    (%setloopstartbbuplayer refnum pos))
  
  (define-entry-point 
    (%setloopendbbuplayer ("PlayerSharedPPC" "SetLoopEndBBUPlayer"))
    ((refnum :short) (pos :ptr)) :long)
  (defun setloopendbbuplayer (refnum pos)
    (%setloopendbbuplayer refnum pos))
  
  (define-entry-point
    (%setloopstartmsplayer ("PlayerSharedPPC" "SetLoopStartMsPlayer"))
    ((refnum :short) (date_ms :longint)) :long)
  (defun setloopstartmsplayer (refnum date_ms) 
    (%setloopstartmsplayer refnum date_ms))
  
  (define-entry-point
    (%setloopendmsplayer ("PlayerSharedPPC" "SetLoopEndMsPlayer"))
    ((refnum :short) (date_ms :longint)) :long)
  (defun setloopendmsplayer (refnum date_ms) 
    (%setloopendmsplayer refnum date_ms))
  
  ;; Synchronisation management
  ;;============================
  
  (define-entry-point
    (%setsynchroinplayer ("PlayerSharedPPC" "SetSynchroInPlayer"))
    ((refnum :short) (state :short)))
  (defun setsynchroinplayer (refnum state) 
    (%setsynchroinplayer refnum state))
  
  (define-entry-point
    (%setsynchrooutplayer ("PlayerSharedPPC" "SetSynchroOutPlayer"))
    ((refnum :short) (state :short)))
  (defun setsynchrooutplayer (refnum state) 
    (%setsynchrooutplayer refnum state))
  
  (define-entry-point
    (%setsmpteoffsetplayer ("PlayerSharedPPC" "SetSMPTEOffsetPlayer"))
    ((refnum :short) (smptepos :ptr)))
  (defun setsmpteoffsetplayer (refnum smptepos)
    (%setsmpteoffsetplayer refnum smptepos))
  
  (define-entry-point
    (%settempoplayer ("PlayerSharedPPC" "SetTempoPlayer"))
    ((refnum :short) (tempo :longint)))
  (defun settempoplayer (refnum tempo) 
    (%settempoplayer refnum tempo))
  
  ;; State management
  ;;===================
  
  (define-entry-point 
    (%getstateplayer ("PlayerSharedPPC" "GetStatePlayer"))
    ((refnum :short) (playerstate :ptr)))
  (defun getstateplayer (refnum playerstate)
    (%getstateplayer refnum playerstate))
  
  (define-entry-point
    (%getendscoreplayer ("PlayerSharedPPC" "GetEndScorePlayer"))
    ((refnum :short) (playerstate :ptr)))
  (defun getendscoreplayer (refnum playerstate)
    (%getendscoreplayer refnum playerstate))
  
  ;; Step playing 
  ;;==============
  
  (define-entry-point 
    (%forwardstepplayer ("PlayerSharedPPC" "ForwardStepPlayer"))
    ((refnum :short) (flag :short)))
  (defun forwardstepplayer (refnum flag) 
    (%forwardstepplayer refnum flag))
  
  (define-entry-point
    (%backwardstepplayer ("PlayerSharedPPC" "BackwardStepPlayer"))
    ((refnum :short) (flag :short)))
  (defun backwardstepplayer (refnum flag) 
    (%backwardstepplayer refnum flag))
  
  ;; Tracks management
  ;;====================
  
  (define-entry-point
    (%getalltrackplayer ("PlayerSharedPPC" "GetAllTrackPlayer"))
    ((refnum :short)) :ptr)
  (defun getalltrackplayer (refnum) (%getalltrackplayer refnum))
  
  (define-entry-point 
    (%gettrackplayer ("PlayerSharedPPC" "GetTrackPlayer"))
    ((refnum :short) (tracknum :short)) :ptr)
  (defun gettrackplayer (refnum tracknum)
    (%gettrackplayer refnum tracknum))
  
  (define-entry-point
    (%settrackplayer ("PlayerSharedPPC" "SetTrackPlayer"))
    ((refnum :short) (tracknum :short) (seq :ptr)) :long)
  (defun settrackplayer (refnum tracknum seq) 
    (%settrackplayer refnum tracknum seq))
  
  (define-entry-point
    (%setalltrackplayer ("PlayerSharedPPC" "SetAllTrackPlayer"))
    ((refnum :short) (seq :ptr) (ticks_per_quarter :long)) :long)
  (defun setalltrackplayer (refnum seq ticks_per_quarter)
    (%setalltrackplayer refnum seq ticks_per_quarter))
  
  (define-entry-point
    (%setparamplayer ("PlayerSharedPPC" "SetParamPlayer"))
    ((refnum :short) (tracknum short) (param short) (value short)))
  (defun setparamplayer (refnum tracknum param value)
    (%setparamplayer refnum tracknum param value))
  
  (define-entry-point
    (%getparamplayer ("PlayerSharedPPC" "GetParamPlayer"))
    ((refnum :short) (tracknum short) (param short)) :short)
  (defun getparamplayer (refnum tracknum param)
    (%getparamplayer refnum tracknum param))
  
  (define-entry-point
    (%insertalltrackplayer ("PlayerSharedPPC" "InsertAllTrackPlayer"))
    ((refnum :short) (seq :ptr)) :long)
  (defun insertalltrackplayer (refnum seq) 
    (%insertalltrackplayer refnum seq))
  
  (define-entry-point 
    (%inserttrackplayer ("PlayerSharedPPC" "InsertTrackPlayer"))
    ((refnum :short) (tracknum short) (seq :ptr)) :long)
  (defun inserttrackplayer (refnum tracknum seq)
    (%inserttrackplayer refnum tracknum seq))
  
  ;; Midifile management
  ;;====================
  
  (define-entry-point (%midifilesave ("PlayerSharedPPC" "MidiFileSave"))
    ((name :ptr) (seq :ptr) (infos :ptr)) :long)
  (defun midifilesave (name seq infos) 
    (with-cstrs ((cname name))
      (%midifilesave cname seq infos)))
  
  (define-entry-point (%midifileload ("PlayerSharedPPC" "MidiFileLoad"))
    ((name :ptr) (seq :ptr) (infos :ptr)) :long)
  (defun midifileload (name seq infos) 
    (with-cstrs ((cname name))
      (%midifileload cname seq infos)))
  
  (defun midi-file-load (name seq info)
    (midifileload name seq info))
  (defun midi-file-save (name seq info)
    (midifilesave name seq info))
  )

#+ccl-5.0

;;=============================
;; Interface to C entry points
;;=============================

(progn
  
  (defun Version ()
    (ccl::ppc-ff-call (get-fun-addr  "Version" *player-framework*)
                      :signed-halfword))
  
  (defun OpenPlayer (name)
    (with-cstrs ((s name))
       (ccl::ppc-ff-call (get-fun-addr "OpenPlayer" *player-framework*) 
                         :address s :signed-halfword)))
  
  (defun ClosePlayer (refnum)
    (ccl::ppc-ff-call (get-fun-addr "ClosePlayer" *player-framework*)
                      :signed-halfword refnum :void))
  
  (defun open-player (name) (openplayer name))
  
  ;;===================
  ;; Transport control
  ;;===================
  
  (defun StartPlayer (refnum)
    (ccl::ppc-ff-call (get-fun-addr "StartPlayer" *player-framework*)
                      :signed-halfword refnum :void))
  
  (defun ContPlayer (refnum)
    (ccl::ppc-ff-call (get-fun-addr "ContPlayer" *player-framework*)
                      :signed-halfword refnum :void))
  
  (defun StopPlayer (refnum)
    (ccl::ppc-ff-call (get-fun-addr "StopPlayer" *player-framework*)
                      :signed-halfword refnum :void))
  
  (defun PausePlayer (refnum)
    (ccl::ppc-ff-call (get-fun-addr "PausePlayer" *player-framework*)
                      :signed-halfword refnum :void))
  
  ;;===================
  ;; Record management
  ;;===================
  
  (defun SetRecordModePlayer (refnum state)
    (ccl::ppc-ff-call (get-fun-addr "SetRecordModePlayer" *player-framework*)
                      :signed-halfword refnum  :signed-halfword state :void))
  
  (defun RecordPlayer (refnum tracknum)
    (ccl::ppc-ff-call (get-fun-addr "RecordPlayer" *player-framework*)
                      :signed-halfword refnum  :signed-halfword tracknum
                      :void))
  
  (defun SetRecordFilterPlayer (refnum filter)
    (ccl::ppc-ff-call (get-fun-addr "SetRecordFilterPlayer"
                                    *player-framework*)
                      :signed-halfword refnum  :address filter :void))
  
  ;;=====================
  ;; Position management
  ;;=====================
  
  (defun SetPosBBUPlayer (refnum pos)
    (ccl::ppc-ff-call (get-fun-addr "SetPosBBUPlayer" *player-framework*)
                      :signed-halfword refnum :address pos :void))
  
  (defun SetPosMsPlayer (refnum date_ms)
    (ccl::ppc-ff-call (get-fun-addr "SetPosMsPlayer" *player-framework*)
                      :signed-halfword refnum :signed-fullword date_ms
                      :void))
  
  ;;==================
  ;; Loop management
  ;;==================
  
  (defun SetLoopPlayer (refnum state)
    (ccl::ppc-ff-call (get-fun-addr "SetLoopPlayer" *player-framework*)
                      :signed-halfword refnum  :signed-halfword state
                      :void))
  
  (defun SetLoopStartBBUPlayer (refnum pos)
    (ccl::ppc-ff-call (get-fun-addr "SetLoopStartBBUPlayer"
                                    *player-framework*)
                      :signed-halfword refnum :address pos :void))
  
  (defun SetLoopEndBBUPlayer (refnum pos)
    (ccl::ppc-ff-call (get-fun-addr "SetLoopEndBBUPlayer"
                                    *player-framework*)
                      :signed-halfword refnum :address pos :void))
  
  (defun SetLoopStartMsPlayer (refnum date_ms)
    (ccl::ppc-ff-call (get-fun-addr "SetLoopStartMsPlayer"
                                    *player-framework*) 
                      :signed-halfword refnum :signed-fullword date_ms 
                      :void))
  
  (defun SetLoopEndMsPlayer (refnum date_ms)
    (ccl::ppc-ff-call (get-fun-addr "SetLoopEndMsPlayer" *player-framework*)
                      :signed-halfword refnum :signed-fullword date_ms
                      :void))
  
  ;;============================
  ;; Synchronisation management
  ;;============================
  
  (defun SetSynchroInPlayer (refnum state)
    (ccl::ppc-ff-call (get-fun-addr "SetSynchroInPlayer" *player-framework*)
                      :signed-halfword refnum  :signed-halfword state
                      :void))
  
  (defun SetSynchroOutPlayer (refnum state)
    (ccl::ppc-ff-call (get-fun-addr "SetSynchroOutPlayer"
                                    *player-framework*)
                      :signed-halfword refnum  :signed-halfword state
                      :void))
  
  (defun SetSMPTEOffsetPlayer (refnum smptepos)
    (ccl::ppc-ff-call (get-fun-addr "SetSMPTEOffsetPlayer"
                                    *player-framework*)
                      :signed-halfword refnum  :address smptepos
                      :void))
  
  (defun SetTempoPlayer (refnum tempo)
    (ccl::ppc-ff-call (get-fun-addr "SetTempoPlayer" *player-framework*)
                      :signed-halfword refnum  :signed-fullword tempo
                      :void))
  
  ;;===================
  ;; State management
  ;;===================
  
  (defun GetStatePlayer (refnum playerstate)
    (ccl::ppc-ff-call (get-fun-addr "GetStatePlayer" *player-framework*)
                      :signed-halfword refnum  :address playerstate
                      :void))
  
  (defun GetEndScorePlayer (refnum playerstate)
    (ccl::ppc-ff-call (get-fun-addr "GetEndScorePlayer" *player-framework*)
                      :signed-halfword refnum  :address playerstate
                      :void))
  
  ;;==============
  ;; Step playing 
  ;;==============
  
  (defun ForwardStepPlayer (refnum flag)
    (ccl::ppc-ff-call (get-fun-addr "ForwardStepPlayer" *player-framework*)
                      :signed-halfword refnum  :signed-halfword flag
                      :void))
  
  (defun BackwardStepPlayer (refnum flag)
    (ccl::ppc-ff-call (get-fun-addr "BackwardStepPlayer" *player-framework*)
                      :signed-halfword refnum  :signed-halfword flag
                      :void))
  
  ;;====================
  ;; Tracks management
  ;;====================
  
  (defun GetAllTrackPlayer (refnum)
    (ccl::ppc-ff-call (get-fun-addr "GetAllTrackPlayer" *player-framework*)
                      :signed-halfword refnum 
                      :address))
  
  (defun GetTrackPlayer (refnum tracknum)
    (ccl::ppc-ff-call (get-fun-addr "GetTrackPlayer" *player-framework*)
                      :signed-halfword refnum :signed-halfword tracknum
                      :address))
  
  (defun SetTrackPlayer (refnum tracknum seq)
    (ccl::ppc-ff-call (get-fun-addr "SetTrackPlayer" *player-framework*)
                      :signed-halfword refnum :signed-halfword tracknum
                      :address seq :signed-fullword))
  
  (defun SetAllTrackPlayer (refnum seq tpq)
    (ccl::ppc-ff-call (get-fun-addr "SetAllTrackPlayer" *player-framework*)
                      :signed-halfword refnum :address seq
                      :signed-fullword tpq 
                      :signed-fullword))
  
  (defun SetParamPlayer (refnum track param val)
    (ccl::ppc-ff-call (get-fun-addr "SetParamPlayer" *player-framework*)
                      :signed-halfword refnum :signed-halfword track  
                      :signed-halfword param  :signed-halfword val
                      :void))
  
  (defun InsertAllTrackPlayer (refnum seq)
    (ccl::ppc-ff-call (get-fun-addr "InsertAllTrackPlayer"
                                    *player-framework*)
                      :signed-halfword refnum :address seq 
                      :signed-fullword ))
  
  (defun InsertTrackPlayer (refnum track seq)
    (ccl::ppc-ff-call (get-fun-addr "InsertTrackPlayer" *player-framework*)
                      :signed-halfword refnum :signed-halfword track
                      :address seq 
                      :signed-fullword ))
  
  ;;====================
  ;; Midifile management
  ;;====================

  ;; Already defined
  ;;=================
  
  (unless (macro-function 'MidiFileSave)
    
    
    (defun MidiFileSave (name seq info)
      (with-cstrs ((s name))
         (ccl::ppc-ff-call (get-fun-addr "MidiFileSave" *player-framework*)
                           :address s :address seq :address info
                           :signed-fullword)))
    
    (defun MidiFileLoad (name seq info)
      (with-cstrs ((s name))
         (ccl::ppc-ff-call (get-fun-addr "MidiFileLoad" *player-framework*)
                           :address s :address seq :address info
                           :signed-fullword)))
    
    (defun midi-file-save (name seq info) (MidiFileSave  name seq info))
    (defun midi-file-load (name seq info) (MidiFileLoad  name seq info))
    
    )
  
  )

)

;;;
;;;                        OpenMCL
;;;

#+openmcl
(eval-when (:compile-toplevel :execute)
  ;; locate midishare's ffi databases
  (ccl:use-interface-dir :midishare))

#+openmcl
(progn
(defvar *player-framework* nil)

(defun player-framework ()
  (or *player-framework*
      (setf *player-framework*
            (ccl:open-shared-library
             "/System/Library/Frameworks/Player.framework/Player"))))

(defun p-bar (e &optional (d nil d?))
  (if d?
    (setf (ccl:pref e :<p>os.bar) d)
    (ccl:pref e :<p>os.bar)))

(defun p-beat (e &optional (d nil d?))
  (if d?
    (setf (ccl:pref e :<p>os.beat) d)
    (ccl:pref e :<p>os.beat)))
    
(defun p-unit (e &optional (d nil d?))
  (if d?
    (setf (ccl:pref e :<p>os.unit) d)
    (ccl:pref e :<p>os.unit)))

;;;
;;;
;;;

(defun s-bar (e )
  (ccl:pref e :<p>layer<s>tate.bar))

(defun s-beat (e)
  (ccl:pref e :<p>layer<s>tate.beat))
 
(defun s-unit (e)
  (ccl:pref e :<p>layer<s>tate.unit))

(defun s-date (e )
  (ccl:pref e :<p>layer<s>tate.date))

(defun s-tempo (e)
  (ccl:pref e :<p>layer<s>tate.tempo))

(defun s-num (e)
  (ccl:pref e :<p>layer<s>tate.tsnum))

(defun s-denom (e)
  (ccl:pref e :<p>layer<s>tate.tsdenom))

(defun s-click (e )
  (ccl:pref e :<p>layer<s>tate.tsclick))

(defun s-quarter (e )
  (ccl:pref e :<p>layer<s>tate.tsquarter))

(defun s-state (e )
  (ccl:pref e :<p>layer<s>tate.state))

(defun s-syncin (e )
  (ccl:pref e :<p>layer<s>tate.syncin))

(defun s-syncout (e )
  (ccl:pref e :<p>layer<s>tate.syncout))

;;;
;;;
;;;

(defun mf-format (e &optional (d nil d?))
  (if d?
    (setf (ccl:pref e :<m>idi<f>ile<i>nfos.format) d)
    (ccl:pref e :<m>idi<f>ile<i>nfos.format)))
  
(defun mf-timedef (e &optional (d nil d?))
  (if d?
    (setf (ccl:pref e :<m>idi<f>ile<i>nfos.timedef) d)
    (ccl:pref e :<m>idi<f>ile<i>nfos.timedef)))
  
(defun mf-clicks (e &optional (d nil d?))
  (if d?
    (setf (ccl:pref e :<m>idi<f>ile<i>nfos.clicks) d)
    (ccl:pref e :<m>idi<f>ile<i>nfos.clicks)))
  
(defun mf-tracks (e )
  (ccl:pref e :<m>idi<f>ile<i>nfos.tracks))

;;;
;;; Interface to C entry points
;;;

;;; HKT: does not appear to be defined in lib...
;(defun Version ()
;  (#_Version))
  
(defun OpenPlayer (name)
  (ccl:with-cstrs ((s name))
    (#_OpenPlayer s)))
  
(defun ClosePlayer (refNum)
  (#_ClosePlayer refNum))
  
(defun open-player (name)
  (OpenPlayer name))
  
;;===================
;; Transport control
;;===================
  
(defun StartPlayer (refNum)
  (#_StartPlayer refNum))

(defun ContPlayer (refNum)
  (#_ContPlayer refNum))
  
(defun StopPlayer (refNum)
  (#_StopPlayer refNum))
  
(defun PausePlayer (refNum)
  (#_PausePlayer refNum))
  
;;===================
;; Record management
;;===================
  
(defun SetRecordModePlayer (refNum state)
  (#_SetRecordModePlayer refNum state))
  
(defun RecordPlayer (refNum tracknum)
  (#_RecordPlayer refNum trackNum))
  
(defun SetRecordFilterPlayer (refNum filter)
  (#_SetRecordFilterPlayer refNum filter))
  
;;=====================
;; Position management
;;=====================
  
(defun SetPosBBUPlayer (refNum pos)
  (#_SetPosBBUPlayer refNum pos))
  
(defun SetPosMsPlayer (refNum date_ms)
  (#_SetPosMsPlayer refNum date_ms))
  
;;==================
;; Loop management
;;==================
  
(defun SetLoopPlayer (refNum state)
  (#_SetLoopPlayer refNum state))
  
(defun SetLoopStartBBUPlayer (refNum pos)
  (#_SetLoopStartBBUPlayer refNum pos))
  
(defun SetLoopEndBBUPlayer (refNum pos)
  (#_SetLoopEndBBUPlayer refNum pos))
  
(defun SetLoopStartMsPlayer (refNum date_ms)
  (#_SetLoopStartMsPlayer refNum date_ms))
  
(defun SetLoopEndMsPlayer (refNum date_ms)
  (#_SetLoopEndMsPlayer refNum date_ms))
  
;;============================
;; Synchronisation management
;;============================
  
(defun SetSynchroInPlayer (refNum state)
  (#_SetSynchroInPlayer refNum state))
  
(defun SetSynchroOutPlayer (refNum state)
  (#_SetSynchroOutPlayer refNum state))
  
(defun SetSMPTEOffsetPlayer (refNum smptepos)
  (#_SetSMPTEOffsetPlayer refNum smptepos))
  
(defun SetTempoPlayer (refNum tempo)
  (#_SetTempoPlayer refNum tempo))
  
;;===================
;; State management
;;===================
  
(defun GetStatePlayer (refNum playerstate)
  (#_GetStatePlayer refNum playerstate))
  
(defun GetEndScorePlayer (refNum playerstate)
  (#_GetEndScorePlayer refNum playerstate))
  
;;==============
;; Step playing 
;;==============
  
(defun ForwardStepPlayer (refNum flag)
  (#_ForwardStepPlayer refNum flag))
  
(defun BackwardStepPlayer (refNum flag)
  (#_BackwardStepPlayer refNum flag))
  
;;====================
;; Tracks management
;;====================
  
(defun GetAllTrackPlayer (refNum)
  (#_GetAllTrackPlayer refNum))
  
(defun GetTrackPlayer (refNum tracknum)
  (#_GetTrackPlayer refNum tracknum))
  
(defun SetTrackPlayer (refNum tracknum seq)
  (#_SetTrackPlayer refNum tracknum seq))
  
(defun SetAllTrackPlayer (refNum seq tpq)
  (#_SetAllTrackPlayer refNum seq tpq))
  
(defun SetParamPlayer (refNum track param val)
  (#_SetParamPlayer refNum track param val))
  
(defun InsertAllTrackPlayer (refNum seq)
  (#_InsertAllTrackPlayer refNum seq))
  
(defun InsertTrackPlayer (refNum track seq)
  (#_InsertTrackPlayer refNum track seq))

  
;;====================
;; Midifile management
;;====================

;; Already defined
;;=================

(defun MidiFileSave (name seq info)
  (ccl:with-cstrs ((s name))
    (#_MidiFileSave  s seq info )))

(defun MidiFileLoad (name seq info)
  (ccl:with-cstrs ((s name))
    (#_MidiFileLoad  s seq info)))

(defun midi-file-save (name seq info)
  (MidiFileSave  name seq info))

(defun midi-file-load (name seq info)
  (MidiFileLoad  name seq info))

(defun MidiNewMidiFileInfos ()
  (ccl::make-record :<m>idi<f>ile<i>nfos))

(defun MidiFreeMidiFileInfos (info)
  (ccl::free info))

(defun MidiNewPlayerState ()
  (ccl::make-record :<p>layer<s>tate))

(defun MidiFreePlayerState (state)
  (ccl::free state))

(defun MidiNewPos ()
  (ccl::make-record :<p>os))

(defun MidiFreePos (pos)
  (ccl::free pos))

)



;---------------------------------------------------------------------------------
;;                          Interface for CMULisp on Linux
;;---------------------------------------------------------------------------------

#+(and linux cmu)

;; The libPlayer file must be located either in the System Folder/Extension Folder

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((*warn-if-redefine* nil))
    ;; hkt: this doesnt work for latest midishare release
    ;;(load-foreign "/usr/lib/libPlayer.so"   )
    ;; hkt: load the last (latest) Player lib
    (let ((lib (car (reverse (directory "/usr/lib/libPlayer*")))))
      (load-foreign lib))
    (use-package "ALIEN"                    )
    (use-package "C-CALL"                   )
    )
  )


#+(and linux cmu)


(progn
  
  ;; Record for position management
  ;;================================
  
  (def-alien-type nil 
    (struct Pos
            (bar  short)  
            (beat short)     
            (unit short)
            )
    )
  
  (defun p-bar (e &optional (d nil d?)) 
    (if d?
      (setf (slot e 'bar) d)
      (slot e 'bar          )
      )
    )
  
  (defun p-beat (e &optional (d nil d?)) 
    (if d?
      (setf (slot e 'beat) d)
      (slot e 'beat          )
      )
    )
  
  (defun p-unit (e &optional (d nil d?)) 
    (if d?
      (setf (slot e 'unit) d)
      (slot e 'unit)
      )
    )
  
  (def-alien-type PosPtr (* (struct Pos) )  
    )
  
  
  ;; Record for state management
  ;;================================
  
  (def-alien-type nil  
    (struct PlayerState
            (date      long  )
            (tempo     long  )
            (tsnum     short )
            (tsdenom   short )
            (tsclick   short )
            (tsquarter short )
            (bar       short )   
            (beat      short )     
            (unit      short )
            (state     short )
            (syncin    short )
            (syncout   short )
            )
    )
  
  
  (defun s-bar (e ) 
    (slot e 'bar)
    )
  
  (defun s-beat (e) 
    (slot e 'beat)
    )
  
  (defun s-unit (e) 
    (slot e 'unit)
    )
  
  (defun s-date (e)  
    (slot e 'date)
    )
  
  (defun s-tempo (e)  
    (slot e 'tempo)
    )
  
  (defun s-num (e)  
    (slot e 'tsnum)
    )
  
  (defun s-denom (e)  
    (slot e 'tsdenom)
    )
  
  (defun s-click (e)  
    (slot e 'tsclick)
    )
  
  (defun s-quarter (e)  
    (slot e 'tsquarter)
    )
  
  (defun s-state (e) 
    (slot e 'state)
    )
  
  (defun s-syncin (e)  
    (slot e 'syncin)
    )
  
  (defun s-syncout (e)  
    (slot e 'syncout)
    )
  
  
  (def-alien-type PlayerStatePtr (* (struct PlayerState))) 
  
  ;; Already defined
  ;;================
  
  (unless (macro-function 'mf-format)
    
    ;; Record for MidiFile
    ;;================================
    
    
    (def-alien-type nil 
      (struct MidiFileInfos
              (format  long)     
              (timedef long)   
              (clicks  long)      
              (tracks  long)
              )
      )
    
    (defun  mf-format (e &optional (d nil d?))  
      (if d?
        (setf (slot e 'format) d)
        (slot e 'format          )
        )
      )
    
    
    (defun mf-timedef (e &optional (d nil d?))  
      (if d?
        (setf (slot e 'timedef) d)
        (slot e 'timedef          )
        )
      )
    
    
    (defun mf-clicks (e &optional (d nil d?)) 
      (if d?
        (setf (slot e 'clicks) d)
        (slot e 'clicks          )
        )
      )
    
    (defun mf-tracks (e)  
      (slot e 'tracks)
      )
    
    )
  
  
  (def-alien-type MidiFileInfosPtr (* (struct MidiFileInfos)))
  
  
  ;; Interface to C entry points
  ;;==============================================================================================================================================================
  
  (def-alien-routine "Version" long  
    )
  
  
  (def-alien-routine "OpenPlayer" short  
    (name c-string)
    )
  
  
  (def-alien-routine "ClosePlayer" void  
    (refnum short)
    )
  
  
  (defun open-player (name)  
    (multiple-value-bind(a b) 
                        (floor (OpenPlayer name) 65536)
      b)
    )
  
  ;; Transport control
  ;;===================
  

  (def-alien-routine "StartPlayer" void  
    (refnum short))
  
  
  (def-alien-routine "ContPlayer" void  
    (refnum short))
  
  
  (def-alien-routine "StopPlayer" void  
    (refnum short))
  
  
  (def-alien-routine "PausePlayer" void  
    (refnum short))
  
  ;; Record management
  ;;===================
  
  (def-alien-routine "SetRecordModePlayer" void  
    (refnum short) (state short))
  
  
  (def-alien-routine "RecordPlayer" void 
    (refnum short) (tracknum short))
  
  
  (def-alien-routine "SetRecordFilterPlayer" void
    (refnum short) (filter MidiFilterPtr))
  
  
  ;; Position management
  ;;=====================
  
  (def-alien-routine "SetPosBBUPlayer" void  
    (refnum short) (pos PosPtr))
   
  
  (def-alien-routine "SetPosMsPlayer" void  
    (refnum short)  (date_ms long))
  
  ;; Loop management
  ;;==================
  
  (def-alien-routine "SetLoopPlayer" void 
    (refnum short) (state short))
  
  
  (def-alien-routine "SetLoopStartBBUPlayer" long  
    (refnum short) (pos PosPtr))
  
  
  (def-alien-routine "SetLoopEndBBUPlayer"   long  
    (refnum short) (pos PosPtr))
  
  
  (def-alien-routine "SetLoopStartMsPlayer"  long  
    (refnum short) (date_ms long))
  
  
  (def-alien-routine "SetLoopEndMsPlayer"    long  
    (refnum short) (date_ms long))
  
  ;; Synchronisation management
  ;;============================
  
  (def-alien-routine "SetSynchroInPlayer" void  
    (refnum short) (state short))
  
  
  (def-alien-routine "SetSynchroOutPlayer" void  
    (refnum short) (state short))
   
  
  (def-alien-routine "SetSMPTEOffsetPlayer" void  
    (refnum short) (smptepos SmpteLocPtr))
   
  
  (def-alien-routine "SetTempoPlayer" void 
    (refnum short) (tempo long))
  
  
  ;; State management
  ;;===================
  
  (def-alien-routine "GetStatePlayer" void 
    (refnum short) (playerstate PlayerStatePtr))
   
  
  (def-alien-routine "GetEndScorePlayer" void  
    (refnum short) (playerstate PlayerStatePtr))
  
  
  ;; Step playing 
  ;;==============
  
  (def-alien-routine "ForwardStepPlayer" void  
    (refnum short) (flag short))
  
  
  (def-alien-routine "BackwardStepPlayer" void  
    (refnum short) (flag short))
  
  ;; Tracks management
  ;;====================
  
  (def-alien-routine "GetAllTrackPlayer" MidiSeqPtr  
    (refnum short))
  
  
  (def-alien-routine "GetTrackPlayer" MidiSeqPtr 
    (refnum short) (tracknum short))
  
  
  (def-alien-routine "SetTrackPlayer" long  
    (refnum short) (tracknum short) (seq MidiSeqPtr))
   
  
  (def-alien-routine "SetAllTrackPlayer" long   
    (refnum short) (seq MidiSeqPtr) (ticks_per_quarter long))
  
  
  (def-alien-routine "SetParamPlayer" void  
    (refnum short) (tracknum short) (param short) (value short))
  
  
  (def-alien-routine "GetParamPlayer" short 
    (refnum short) (tracknum short) (param short))
  
  
  (def-alien-routine "InsertAllTrackPlayer" long  
    (refnum short) (seq MidiSeqPtr))
  
  
  (def-alien-routine "InsertTrackPlayer" long  
    (refnum short) (tracknum short)(seq MidiSeqPtr))
   
  
  ;; Already defined
  ;;=================
  
    
    ;; Midifile management
    ;;====================
    
  (def-alien-routine "MidiFileSave" long  
    (name c-string) (seq MidiSeqPtr) (infos MidiFileInfosPtr))
    
    
  (def-alien-routine "MidiFileLoad" long  
    (name c-string) (seq MidiSeqPtr) (infos MidiFileInfosPtr))
    
    
  (defun midi-file-load (name seq info) 
    (MidiFileLoad name seq info))
    
    
  (defun midi-file-save (name seq info)  
    (MidiFileSave name seq info))
    

  
  )  ;; End of CMULisp interface


;;;-----------------------------------------------------------------------
;;;-----------------------------------------------------------------------
;;;
;;; 			To Install and Remove the Player Interface
;;;
;;;-----------------------------------------------------------------------
;;;-----------------------------------------------------------------------

;;;-----------------------------------------------------------------------
;;; 	 		Player Startup and Quit Actions
;;;-----------------------------------------------------------------------


#+(and apple mcl powerpc)

;;;..........................................: install-player-interface
#+CCL-5.0
(defun install-player-interface ()
  (unless (player-framework) (error "Player not installed")))

#-CCL-5.0
(defun install-player-interface ())

#+openmcl
(defun install-player-interface ()
  (unless (player-framework) (error "Player not installed")))

#+cmu
(defun install-player-interface ())

#+(and apple mcl powerpc)

;;;..........................................: remove-player-interface
#+CCL-5.0
(defun remove-player-interface ()
  (setq *player-framework* nil))
  
#-CCL-5.0
(defun remove-player-interface ())
 
#+openmcl
(defun remove-player-interface ()
  (setq *player-framework* nil))

#+cmu
(defun remove-player-interface ())

;;---------------------------------------------------------------------------------
;; 	 			**Evaluate this**
;;---------------------------------------------------------------------------------

(eval-when (:load-toplevel :execute)
  (add-startup-action #'install-player-interface)
  (add-quit-action #'remove-player-interface)
  (install-player-interface))

;;;
;;; Add #+player conditional.

(pushnew ':player *features*)
