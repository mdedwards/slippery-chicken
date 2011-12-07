;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Midishare-Interface.lisp
;;;
;;;  Copyright (c) 1990,2001 GRAME.  All rights reserved.
;;;
;;;  This file contains definitions for records and Pascal style routines,
;;;  used for interfacing Lisp with MidiShare real-time multitasking
;;;  Midi operating system.
;;;  It is in conformity with MPW Pascal MidiShareUnit.p .
;;;
;;;  History :
;;;  
;;;   11-Nov-90, First version. -Yo-
;;;   25-Nov-90, Ajoute def de TMidiSeq + FirstEv & LastEv -Yo-
;;;   25-Nov-90, Continue change en Cont -Yo-
;;;   26-Nov-90, Modification de firstEv, lastEv, link,
;;;              on ne pouvais pas ecrire par ex: (firstEv seq nil) qui
;;;              etait 
;;;              confondu avec (firstEv seq)
;;;   01-Dec-90, Ajout d'une macro DOEVENTS, analogue a DOLIST, pour
;;;              parcourir une chaine d'evenements.-Yo-
;;;              Ajout des fonctions : Clock, typeName, name, fieldslist,
;;;              printEv, printSeq. -Yo-
;;;              Ajout des fonctions ou macro : pushevent, dupevents, 
;;;              delevents, mergeevents. -Yo-
;;;   07-Dec-90, Correction de ProgChange. -Yo-
;;;   12-Dec-90  Ajout de linkSE,linkST
;;;-----------------------------------------------------------------------
;;;   15-Dec-90  Nouvelle version de l'interface, restreinte aux seules
;;;              fonctions de MidiShare et utilisant des macros. -Yo- 
;;;   09-Jan-91  Ajout d'une variante info dans la description d'un
;;;              evenement et des fonctions d'acces associees.
;;;   09-Jan-91  Ajout fonctions d'acces aux filtres
;;;   14-Mai-91  Adaptation MCL 2.0b1
;;;   19-Mai-91  Pb des ff-call. Enrobage par (Block nil ..)
;;;   22-Mai-91  Changement de nom des macro d'acces aux filtres
;;;   31-Mai-91  Ajout des "s", (eval-when () ...)
;;;   18-Jul-91  Ajout de la fonction bend (de l'ancienne version de
;;;              msh-interface)
;;;   04-Aou-91  Toutes les macros d'acces transferees dans le fichier 
;;;               extension
;;;   31-Oct-91  Modification de MidiForgetTask
;;;-----------------------------------------------------------------------
;;;   04-Dec-94  Suppression du package MidiShare !!!
;;;              Suppression des (block nil ..)
;;;-----------------------------------------------------------------------
;;;   22-07-96   Adaptation pour MCL PPC 3.9 : Le fonctionnement de
;;;              ff-call a change
;;;              pour les fonctions Pascal, il ne faut plus pusher dans
;;;              la pile la place pour le resultat !!!
;;;   23-07-96   Integration du fichiers "0 - quit-actions.lisp" et
;;;              d'une partie du fichier "2 - MidiShare-Extension.lisp"
;;;   13-04-01   Ajout du type PortPrefix 
;;;   15-06-01   Ajout des fonctions de gestion du filtre
;;;   19-06-01   Changement du fonctionnement des fonctions de connection
;;;              et etat des filtres 
;;;              pour rendre le code multi-platorm
;;;   24-06-01   Correction MidiForgetTask
;;;   25-06-01   add-startup-action et add-quit-action changees en
;;;              fonctions
;;;   27-06-01   Ajout du code correspondant a l'interface Linux 
;;;   05-07-01   Nettoyage
;;;   07-01-02   Ajout des fonctions de gestion des drivers sur Macintosh
;;;   19-11-02   Version MCL 5.0 : MacOSX
;;;   26-05-03   Correction de MidiFreeFilter
;;;   16-06-03   HKT: Added support for OpenMcl, defpackage, a read-time
;;;              #+:midishare conditional, and reformatted for standard
;;;              emacs buffer width of 74 character.
;;;   28-10-03   Rename type macro to evtype.
;;;   21-11-03   Converted to functional API. (HKT)
;;;   29-01-04   Make the MidiShare return 1 or 0 (instead of T and Nil)
;;;   17-03-04   Rename type macro to evtype in OpenMCL and CMUCL interfaces
;;;   19-03-04   New MidiNewSmpteLocation, MidiFreeSmpteLocation, MidiNewSyncInfo, MidiFreeSyncInfo


(in-package :cl-user)       
    
(defpackage "MIDISHARE"
  (:nicknames ms)
  (:use common-lisp
        #+ccl ccl
        #+cmu alien #+cmu c-call)
  (:export "TYPENOTE" "TYPEKEYON" "TYPEKEYOFF" "TYPEKEYPRESS" 
          "TYPECTRLCHANGE" "TYPEPROGCHANGE" "TYPECHANPRESS" "TYPEPITCHWHEEL" 
          "TYPEPITCHBEND" "TYPESONGPOS" "TYPESONGSEL" "TYPECLOCK" "TYPESTART" 
          "TYPECONTINUE" "TYPESTOP" "TYPETUNE" "TYPEACTIVESENS" "TYPERESET" "TYPESYSEX"
          "TYPESTREAM" "TYPEPRIVATE" "TYPEPROCESS" "TYPEDPROCESS" 
          "TYPEQFRAME" "TYPECTRL14B" "TYPENONREGPARAM" "TYPEREGPARAM"
          "TYPESEQNUM" "TYPETEXTUAL" "TYPECOPYRIGHT" "TYPESEQNAME"
          "TYPEINSTRNAME" "TYPELYRIC" "TYPEMARKER" "TYPECUEPOINT"
          "TYPECHANPREFIX" "TYPEENDTRACK" "TYPETEMPO" "TYPESMPTEOFFSET"
          "TYPETIMESIGN" "TYPEKEYSIGN" "TYPESPECIFIC" "TYPEPORTPREFIX"
          "TYPERCVALARM" "TYPEAPPLALARM" "TYPERESERVED" "TYPEDEAD"
          "MIDIERRSPACE" "MIDIERRREFNUM" "MIDIERRBADTYPE" "MIDIERRINDEX"
          "MODEMPORT" "PRINTERPORT" "MIDIEXTERNALSYNC" "MIDISYNCANYPORT"
          "SMPTE24FR" "SMPTE25FR" "SMPTE29FR" "SMPTE30FR"
          "MIDIOPENAPPL" "MIDICLOSEAPPL" "MIDICHGNAME" "MIDICHGCONNECT"
          "MIDIOPENMODEM" "MIDICLOSEMODEM" "MIDIOPENPRINTER"
          "MIDICLOSEPRINTER" "MIDISYNCSTART" "MIDISYNCSTOP" "MIDICHANGESYNC"
          "MIDIOPENDRIVER" "MIDICLOSEDRIVER" "MIDIADDSLOT" "MIDIREMOVESLOT"
          "MIDICHGSLOTCONNECT" "MIDICHGSLOTNAME"
          "INSTALL-MIDISHARE-INTERFACE" "REMOVE-MIDISHARE-INTERFACE"
          "LINK" "DATE" "EVTYPE" "REF" "PORT" "CHAN" "FIELD" "FIELDSLIST" "PITCH" "VEL" "DUR"
          "LINKSE" "LINKST" "KPRESS" "CTRL" "PARAM" "NUM" "PREFIX" "TEMPO" "SECONDS"
          "SUBFRAMES" "VAL" "PGM" "BEND" "CLK" "SONG" "FIELDS" "TEXT" "FMSG" "FCOUNT"
          "TSNUM" "TSDENOM" "TSCLICK" "TSQUARTER" "ALTERATION" "MINOR-SCALE" "INFO"
          "FIRSTEV" "LASTEV" "MIDISHARE"
          "MIDIGETVERSION" "MIDICOUNTAPPLS" "MIDIGETINDAPPL"
          "MIDIGETNAMEDAPPL" "MIDIOPEN" "MIDICLOSE"
          "MIDIGETNAME" "MIDISETNAME" "MIDIGETINFO" "MIDISETINFO" "MIDINEWFILTER"
          "MIDIFREEFILTER" "MIDIACCEPTCHAN" "MIDIACCEPTTYPE" "MIDIACCEPTPORT"
          "MIDIISACCEPTEDCHAN" "MIDIISACCEPTEDTYPE" "MIDIISACCEPTEDPORT"
          "MIDIGETFILTER" "MIDISETFILTER" "MIDIGETRCVALARM" "MIDISETRCVALARM"
          "MIDIGETAPPLALARM" "MIDISETAPPLALARM"
          "MIDICONNECT" "MIDIISCONNECTED" "MIDIGETPORTSTATE"
          "MIDISETPORTSTATE" "MIDIFREESPACE" "MIDINEWEV" "MIDICOPYEV" "MIDIFREEEV" "MIDISETFIELD"
          "MIDIGETFIELD" "MIDIADDFIELD" "MIDICOUNTFIELDS"
          "MIDINEWSEQ" "MIDIADDSEQ" "MIDIFREESEQ" "MIDICLEARSEQ" "MIDIAPPLYSEQ" "MIDIGETTIME"
          "MIDISENDIM" "MIDISEND" "MIDISENDAT"
          "MIDICOUNTEVS" "MIDIGETEV" "MIDIAVAILEV" "MIDIFLUSHEVS"
          "MIDIREADSYNC" "MIDIWRITESYNC"
          "MIDICALL" "MIDITASK" "MIDIDTASK" "MIDIFORGETTASKHDL" "MIDIFORGETTASK"
          "MIDICOUNTDTASKS" "MIDIFLUSHDTASKS" "MIDIEXEC1DTASK"
          "MIDINEWCELL" "MIDIFREECELL" "MIDITOTALSPACE" "MIDIGROWSPACE"
          "MIDIGETSYNCINFO" "MIDISETSYNCMODE" "MIDIGETEXTTIME"
          "MIDIINT2EXTTIME" "MIDIEXT2INTTIME" "MIDITIME2SMPTE"
          "MIDISMPTE2TIME" "MIDICOUNTDRIVERS" "MIDIGETINDDRIVER" "MIDIGETDRIVERINFOS"
          "MIDIGETINDSLOT" "MIDIGETSLOTINFOS" "MIDICONNECTSLOT"
          "MIDIISSLOTCONNECTED"
          "MIDINEWSMPTELOCATION" "MIDIFREESMPTELOCATION"
          "MIDINEWSYNCINFO" "MIDIFREESYNCINFO"  
          "NULLPTRP" "NULLPTR" "LOAD-FRAMEWORK-BUNDLE" "GET-FUN-ADDR"
          "ADD-STARTUP-ACTION" "ADD-QUIT-ACTION"
           ))

(in-package :midishare)

;;;-----------------------------------------------------------------------
;;;-----------------------------------------------------------------------
;;;
;;; 			MidiShare Constant Definitions
;;;
;;;-----------------------------------------------------------------------
;;;-----------------------------------------------------------------------

;;; Constant definitions for every type of MidiShare event

(defconstant typeNote          0 "note with pitch, velocity and duration")
(defconstant typeKeyOn         1 "key on with pitch and velocity")
(defconstant typeKeyOff        2 "key off with pitch and velocity")
(defconstant typeKeyPress      3 "key pressure with pitch and pressure value")
(defconstant typeCtrlChange    4 "control change with control number and control value")

(defconstant typeProgChange    5 "program change with program number")
(defconstant typeChanPress     6 "channel pressure with pressure value")
(defconstant typePitchWheel    7 "pitch bend with lsb and msb of the 14-bit value")
(defconstant typePitchBend     7 "pitch bender with lsb and msb of the 14-bit value")
(defconstant typeSongPos       8 "song position with lsb and msb of the 14-bit position")
(defconstant typeSongSel       9 "song selection with a song number")
(defconstant typeClock        10 "clock request (no argument)")
(defconstant typeStart        11 "start request (no argument)")
(defconstant typeContinue     12 "continue request (no argument)")
(defconstant typeStop         13 "stop request (no argument)")
(defconstant typeTune         14 "tune request (no argument)")
(defconstant typeActiveSens   15 "active sensing code (no argument)")
(defconstant typeReset        16 "reset request (no argument)")
(defconstant typeSysEx        17 "system exclusive with any number of data bytes. Leading $F0 and tailing $F7 are automatically supplied by MidiShare and MUST NOT be included by the user")
(defconstant typeStream       18 "special event with any number of unprocessed data/status bytes")
(defconstant typePrivate      19 "private event for internal use with 4 32-bits arguments")
(defconstant typeProcess     128 "interrupt level task with a function adress and 3 32-bits args")
(defconstant typeDProcess    129 "foreground task with a function address and 3 32-bits arguments")
(defconstant typeQFrame      130 "quarter frame message with a type from 0 to 7 and a value")

(defconstant typeCtrl14b     131)
(defconstant typeNonRegParam 132)
(defconstant typeRegParam    133)

(defconstant typeSeqNum	     134)
(defconstant typeTextual     135)
(defconstant typeCopyright   136)
(defconstant typeSeqName     137)
(defconstant typeInstrName   138)
(defconstant typeLyric	     139)
(defconstant typeMarker	     140)
(defconstant typeCuePoint    141)
(defconstant typeChanPrefix  142)
(defconstant typeEndTrack    143)
(defconstant typeTempo	     144)
(defconstant typeSMPTEOffset 145)

(defconstant typeTimeSign    146)
(defconstant typeKeySign     147)
(defconstant typeSpecific    148)
(defconstant typePortPrefix  149)

(defconstant typeRcvAlarm    150)
(defconstant typeApplAlarm   151)

(defconstant typeReserved    152 "events reserved for futur use")
(defconstant typedead        255 "dead task. Used by MidiShare to forget and inactivate typeProcess and typeDProcess tasks")

;;; Constant definition for every MidiShare error code

(defconstant MIDIerrSpace   -1 "too many applications")
(defconstant MIDIerrRefNu   -2 "bad reference number")
(defconstant MIDIerrBadType -3 "bad event type")
(defconstant MIDIerrIndex   -4 "bad index")

;;; Constant definition for the Macintosh serial ports

(defconstant ModemPort   0 "Macintosh modem port")
(defconstant PrinterPort 1 "Macintosh printer port")

;;; Constant definition for the synchronisation modes

(defconstant MidiExternalSync #x8000 
  "Bit-15 set for external synchronisation")
(defconstant MidiSyncAnyPort  #x4000
  "Bit-14 set for synchronisation on any port")

;;; Constant definition for SMPTE frame format

(defconstant smpte24fr 0 "24 frame/sec")
(defconstant smpte25fr 1 "25 frame/sec")
(defconstant smpte29fr 2 "29 frame/sec (30 drop frame)")
(defconstant smpte30fr 3 "30 frame/sec")

;;; Constant definition for MidiShare world changes

(defconstant MIDIOpenAppl     1 "application was opened")
(defconstant MIDICloseAppl    2 "application was closed")
(defconstant MIDIChgName      3 "application name was changed")
(defconstant MIDIChgConnect   4 "connection was changed")
(defconstant MIDIOpenModem    5 "Modem port was opened") ; obsolete
(defconstant MIDICloseModem   6 "Modem port was closed") ; obsolete
(defconstant MIDIOpenPrinter  7 "Printer port was opened")
(defconstant MIDIClosePrinter 8 "Printer port was closed")
(defconstant MIDISyncStart    9 "SMPTE synchronisation just start")
(defconstant MIDISyncStop    10 "SMPTE synchronisation just stop")

(defconstant MIDIChangeSync     10)
(defconstant MIDIOpenDriver     11)
(defconstant MIDICloseDriver    12)
(defconstant MIDIAddSlot        13)
(defconstant MIDIRemoveSlot     14)
(defconstant MIDIChgSlotConnect 15)
(defconstant MIDIChgSlotName    16)

;;;-----------------------------------------------------------------------
;;; 			 To Add Startup and Quit Actions
;;;-----------------------------------------------------------------------

(unless (and (boundp '*lisp-startup-functions*)  
             (boundp '*lisp-cleanup-functions*))
  (defvar *lisp-startup-functions* nil)
  (defvar *lisp-cleanup-functions* nil))

;;;..................................................: add-startup-action
(defun add-startup-action (fun)
  (pushnew fun *lisp-startup-functions*))

;;;..................................................: add-quit-action
(defun add-quit-action (fun)
  (pushnew fun *lisp-cleanup-functions*))


;;;-----------------------------------------------------------------------
;;; 	Interface for MCL 4.2, 4.3. and 5.0 on Macintosh
;;;-----------------------------------------------------------------------


#+(and apple mcl powerpc (not ccl-5.0))
;;; HKT: This was marked #-CCL-4.3.1 but my 4.3 and 4.3.1 need it
(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((*warn-if-redefine* nil))
    (require :ff)))

#+(and apple mcl powerpc) 

;;;-----------------------------------------------------------------------
;;;
;;; 				Utilities
;;;
;;;-----------------------------------------------------------------------

(progn

(defun %%get-string (ps) 
  "Same as %get-string but work with mac non-zone pointers"
  (let (name count)
    (setq count (%get-byte ps))
    (setq name (make-string count))  
    (dotimes (i count)
      (setq ps (%inc-ptr ps 1))
      (setf (aref name i) (coerce (%get-byte ps) 'character)))
    name))

;;; For bug (?) in MCL PPC 3.9 when returning signed word
  
(defun %%unsigned-to-signed-word (w)
  "convert an unsigned word to a signed word"
  (if (< w 32768) w (- w 65536)))

(defun %%word-high-byte (w)
  "most significant byte of a word"
  (ash w -8))
  
(defun nullptrp (p) (%null-ptr-p p))

(defun nullptr () (%null-ptr))

;;;-----------------------------------------------------------------------
;;;-----------------------------------------------------------------------
;;;
;;; 			MidiShare Data Structures
;;;
;;;-----------------------------------------------------------------------
;;;-----------------------------------------------------------------------

;;; Extension record for typeSysEx events

(defrecord TMidiSEX  
  (link (:pointer TMidiSEX))
  (data (:array :byte 12)))


;;; Extension record for typePrivate, typeProcess and typeDProcess events

(defrecord TMidiST
  (ptr1 :pointer)
  (ptr2 :pointer)
  (ptr3 :pointer)
  (ptr4 :pointer))

;;;-----------------------------------------------------------------------
;;; Common Record for all MidiShare events
;;;-----------------------------------------------------------------------

(defrecord TMidiEv
  (link (:pointer TMidiEv))
  (date :longint)
  (evtype :byte)
  (ref :byte)
  (port :byte)
  (chan :byte)
  (variant ((pitch :byte)
            (vel :byte)
            (dur :integer))
           ((data0 :byte)
            (data1 :byte)
            (data2 :byte)
            (data3 :byte))
           ((info :longint))
           ((linkSE (:pointer TMidiSEX)))
           ((linkST (:pointer TMidiST)))))


;;;-----------------------------------------------------------------------
;;; Record for a MidiShare Sequence
;;;-----------------------------------------------------------------------

(defrecord TMidiSeq
  (first (:pointer TMidiEv))    ; first event
  (last (:pointer TMidiEv))     ; last event
  (undef1 :pointer)   
  (undef2 :pointer) )  

;;;-----------------------------------------------------------------------
;;; Record for MidiShare SMPTE synchronisation informations
;;;-----------------------------------------------------------------------

(defrecord TSyncInfo
  (time :longint)
  (reenter :longint)
  (syncMode :unsigned-short)
  (syncLocked :byte)
  (syncPort :byte)
  (syncStart :longint)
  (syncStop :longint)
  (syncOffset :longint)
  (syncSpeed :longint)
  (syncBreaks :longint)
  (syncFormat :short))

;;;-----------------------------------------------------------------------
;;; Record for MidiShare SMPTE locations
;;;-----------------------------------------------------------------------

(defrecord TSmpteLocation
  (format :short)
  (hours :short)
  (minutes :short)
  (seconds :short)
  (frames :short)
  (fracs :short))

;;;-----------------------------------------------------------------------
;;; Constants and records for MidiShare drivers 
;;;-----------------------------------------------------------------------

(defconstant MidiInputSlot 1)
(defconstant MidiOutputSlot 2)
(defconstant MidiInputOutputSlot 3)

(defrecord TSlotRefNum
  (drvRef :short)
  (slotRef :short))
 
(defrecord TSlotInfos
  (name (string 31))
  (direction :byte)
  (cnx (string 31))
  (reserved0 :longint)
  (reserved1 :longint))
 
(defrecord TDriverInfos
  (name (string 31))
  (version :short)
  (slots :short)
  (reserved0 :longint)
  (reserved1 :longint))

;;;-----------------------------------------------------------------------
;;;-----------------------------------------------------------------------
;;;
;;; 		Macros for accessing MidiShare Events data structures
;;;
;;;-----------------------------------------------------------------------
;;;-----------------------------------------------------------------------

;;;-----------------------------------------------------------------------
;;;                    Macros common to every type of event
;;;-----------------------------------------------------------------------

;;;..................................................: link
(defun link (e &optional (d nil d?))
  "read or set the link of an event"
  (if d?
    (rset e :TMidiEv.link d)
    (rref e :TMidiEv.link)))

;;;..................................................: date
(defun date (e &optional d)
  "read or set the date of an event"
  (if d
    (rset e :TMidiEv.date d)
    (rref e :TMidiEv.date)))

;;;..................................................: type
(defun evtype (e &optional v)
  "read or set the type of an event. Be careful in modifying the type of an event"
  (if v
    (rset e :TMidiEv.evType v)
    (rref e :TMidiEv.evType)))

;;;..................................................: ref
(defun ref (e &optional v)
  "read or set the reference number of an event"
  (if v
    (rset e :TMidiEv.ref v)
    (rref e :TMidiEv.ref)))

;;;..................................................: port
(defun port (e &optional v)
  "read or set the port number of an event"
  (if v
    (rset e :TMidiEv.port v)
    (rref e :TMidiEv.port)))

;;;..................................................: chan
(defun chan (e &optional v)
  "read or set the chan number of an event"
  (if v
    (rset e :TMidiEv.chan v)
    (rref e :TMidiEv.chan)))

;;;..................................................: field
(defun field (e &optional f v)
  "give the number of fields or read or set a particular field of an event"
  (if f
    (if v
      (midiSetField e f v)
      (midiGetField e f))
    (midiCountFields e)))

;;;..................................................: fieldsList
(defun fieldsList (e &optional (n 4))
  "collect all the fields of an event into a list"
  (let (l)
    (dotimes (i (min n (midicountfields e)))
      (push (midigetfield e i) l))
    (nreverse l)))


;;;-----------------------------------------------------------------------
;;;                         Specific to typeNote events
;;;-----------------------------------------------------------------------

;;;..................................................: pitch
(defun pitch (e &optional v)
  "read or set the pitch of an event"
  (if v
    (rset e :TMidiEv.pitch v)
    (rref e :TMidiEv.pitch)))

;;;..................................................: vel
(defun vel (e &optional v)
  "read or set the velocity of an event"
  (if v
    (rset e :TMidiEv.vel v)
    (rref e :TMidiEv.vel)))

;;;..................................................: dur
(defun dur (e &optional v)
  "read or set the duration of an event"
  (if v
    (rset e :TMidiEv.dur v)
    (rref e :TMidiEv.dur)))


;;;-----------------------------------------------------------------------
;;;                       Specific to other types of events
;;;-----------------------------------------------------------------------

;;;..................................................: linkSE
(defun linkSE (e &optional (d nil d?))
  "read or set the link of an SEXevent "
  (if d?
    (rset e :TMidiEv.linkSE d)
    (rref e :TMidiEv.linkSE)))

;;;..................................................: linkST
(defun linkST (e &optional (d nil d?))
  "read or set the link of an STevent "
  (if d?
    (rset e :TMidiEv.linkST d)
    (rref e :TMidiEv.linkST)))

;;;..................................................: kpress
(defun kpress (e &optional v)
  (if v
    (rset e :TMidiEv.vel v)
    (rref e :TMidiEv.vel)))

;;;..................................................: ctrl
(defun ctrl (e &optional v)
  (if v
    (midisetfield e 0 v)
    (midigetfield e 0)))

;;;..................................................: param
(defun param (e &optional v)
  (if v
    (midisetfield e 0 v)
    (midigetfield e 0)))

;;;..................................................: num
(defun num (e &optional v)
  (if v
    (midisetfield e 0 v)
    (midigetfield e 0)))

;;;..................................................: prefix
(defun prefix (e &optional v)
  (if v
    (midisetfield e 0 v)
    (midigetfield e 0)))

;;;..................................................: tempo
(defun tempo (e &optional v)
  (if v
    (midisetfield e 0 v)
    (midigetfield e 0)))

;;;..................................................: seconds
(defun seconds (e &optional v)
  (if v
    (midisetfield e 0 v)
    (midigetfield e 0)))

;;;..................................................: subframes
(defun subframes (e &optional v)
  (if v
    (midisetfield e 1 v)
    (midigetfield e 1)))

;;;..................................................: val
(defun val (e &optional v)
  (if v
    (midisetfield e 1 v)
    (midigetfield e 1)))

;;;..................................................: pgm
(defun pgm (e &optional v)
  (if v
    (rset e :TMidiEv.pitch v)
    (rref e :TMidiEv.pitch)))

;;;..................................................: bend
(defun bend (e &optional v)
  "read or set the bend value of an event"
  (if v
    (multiple-value-bind (ms7b ls7b) (floor (+ v 8192) 128)
       (rset e :TMidiEv.pitch ls7b)
       (rset e :TMidiEv.vel ms7b))
    (- (+ (rref e :TMidiEv.pitch)
           (* 128 (rref e :TMidiEv.vel)))
        8192)))

;;;..................................................: clk
(defun clk (e &optional v)
  (if v
    (multiple-value-bind (ms7b ls7b) (floor (round (/ v 6)) 128)
      (rset e :TMidiEv.pitch ls7b)
      (rset e :TMidiEv.vel ms7b))
    (* 6 (+ (pitch e) (* 128 (vel e)))) ))

;;;..................................................: song
(defun song (e &optional v)
  (if v
    (rset e :TMidiEv.pitch v)
    (rref e :TMidiEv.pitch)))

;;;..................................................: fields
(defun fields (e &optional v)
  (if v
    (let ((e e)) (mapc #'(lambda (f) (midiaddfield e f)) v))
    (let (l (e e))
      (dotimes (i (midicountfields e)) (push (midigetfield e i) l))
      (nreverse l)) ))

;;;..................................................: text
(defun text (e &optional s)
  (if s
    (fields e (map 'list #'char-code s))
    (map 'string #'character (fields e)) ))

;;...................................................: fmsg
(defun fmsg (e &optional v)
  (if v
    (rset e :TMidiEv.pitch v)
    (rref e :TMidiEv.pitch)))

;;;..................................................: fcount
(defun fcount (e &optional v)
  (if v
    (rset e :TMidiEv.vel v)
    (rref e :TMidiEv.vel)))

;;;..................................................: tsnum
(defun tsnum (e &optional v)
  (if v
    (midisetfield e 0 v)
    (midigetfield e 0)))

;;;..................................................: tsdenom
(defun tsdenom (e &optional v)
  (if v
    (midisetfield e 1 v)
    (midigetfield e 1)))

;;;..................................................: tsclick
(defun tsclick (e &optional v)
  (if v
    (midisetfield e 2 v)
    (midigetfield e 2)))

;;;..................................................: tsquarter
(defun tsquarter (e &optional v)
  (if v
    (midisetfield e 3 v)
    (midigetfield e 3)))

;;;..................................................: alteration
(defun alteration (e &optional v)
  (if v
    (midisetfield e 0 v)
    (midigetfield e 0)))

;;;..................................................: minor-scale
(defun minor-scale (e &optional v)
  (if v
    (midisetfield e 1 (if v 1 0))
    (= 1 (midigetfield e 1))))

;;;..................................................: info
(defun info (e &optional d)
  "read or set the info of an event"
  (if d
    (rset e :TMidiEv.info d)
    (rref e :TMidiEv.info)))

;;;-----------------------------------------------------------------------
;;;-----------------------------------------------------------------------
;;;
;;; 		Macros for accessing MidiShare Sequences data structures
;;;
;;;-----------------------------------------------------------------------
;;;-----------------------------------------------------------------------

;;;..................................................: firstEv
(defun firstEv (s &optional (e nil e?))
  "read or set the first event of a sequence"
  (if e?
    (rset s :TMidiSeq.first e)
    (rref s :TMidiSeq.first)))

;;;..................................................: lastEv
(defun lastEv (s &optional (e nil e?))
  "read or set the last event of a sequence"
  (if e?
    (rset s :TMidiSeq.last e)
    (rref s :TMidiSeq.last)))

;;;-----------------------------------------------------------------------
;;;-----------------------------------------------------------------------
;;;
;;; 				MidiShare Entry Points
;;;
;;;-----------------------------------------------------------------------
;;;-----------------------------------------------------------------------


;;;-----------------------------------------------------------------------
;;;			 MCL 5.0
;;;-----------------------------------------------------------------------

#+CCL-5.0 
(progn

;; To load Frameworks on MacOSX
;;===============================

(defvar *__CFStringMakeConstantString-slep*
  (ccl::get-slep "__CFStringMakeConstantString"))

(defun CFSTR (string)
  (with-cstrs ((cstr string))
    (ccl::ff-call-slep *__CFStringMakeConstantString-slep*
                       :address cstr 
                       :address)))

(defun create-frameworks-url ()
  (rlet ((fsref :fsref))
    (let* ((err (#_FSFindFolder #$kOnAppropriateDisk
                                #$kFrameworksFolderType #$true fsref)))
      (declare (type (signed-byte 16) err))
      (if (eql #$noErr err)
        (let* ((url (#_CFURLCreateFromFSRef (%null-ptr) fsref)))
          (if (%null-ptr-p url)
            (error "Failed to create URL")
            url))
        (error "Couldn't find system Frameworks folder")))))

(ccl::defloadvar *frameworks-url* nil)

(defun frameworks-url ()
  (or *frameworks-url*
      (setq *frameworks-url* (create-frameworks-url))))

(defun load-framework-bundle (framework-name)
  (let* ((bundle-url 
          (#_CFURLCreateCopyAppendingPathComponent
           (%null-ptr)
           (frameworks-url)    ; file:///System/Library/Frameworks/
           (CFSTR framework-name)
           #$false)))
    (if (%null-ptr-p bundle-url)
      (error "Can't create URL for ~s in system frameworks folder" 
             framework-name)
      (let* ((bundle (#_CFBundleCreate (%null-ptr) bundle-url)))
        (if (%null-ptr-p bundle)
          (error "Can't create bundle for ~s" framework-name)
          (if (eql #$false (#_CFBundleLoadExecutable bundle))
            (error "Couldn't load bundle library for ~s" framework-name)
            bundle))))))

(defun lookup-function-in-framework (symbol-name bundle)
  (let* ((addr (#_CFBundleGetFunctionPointerForName
                bundle (CFSTR symbol-name))))
    (if (%null-ptr-p addr)
      (error "Couldn't resolve address of foreign function ~s" 
             symbol-name)
      ;; This may be a little confusing: MCL uses fixnums (whose low 2 
      ;; bits are zero) to represent function addresses (whose low 2 bits
      ;; are zero ...)Shove the pointer in a buffer, fetch a signed 32-bit
      ;; integer, shift it right 2 bits ... voila.
      (rlet ((buf :long))
        (setf (%get-ptr buf) addr)
        (ash (%get-signed-long buf) -2)))))

(defmacro get-fun-addr (name framework)
  `(lookup-function-in-framework ,name ,framework))
  

;;; MidiShare entry point

(defvar *midishare* nil)

(defun midishare-framework ()
  (or *midishare*
     (setq *midishare*
            (load-framework-bundle "MidiShare.framework"))))


;;;-----------------------------------------------------------------------
;;;			To Know about MidiShare and Active Sessions
;;;-----------------------------------------------------------------------


;;;..................................................: MidiShare
(defun MidiShare ()
  "returns true if MidiShare is installed"
  (if (nullptrp *midishare*) 0 1))

;;;..................................................: MidiGetVersion
(defun MidiGetVersion ()
  "Give MidiShare version as a fixnum. For example 131 as result, means : version 1.31"
  (ccl::ppc-ff-call (get-fun-addr "MidiGetVersion" *midishare*)
                    :signed-fullword))

;;;..................................................: MidiCountAppls
(defun MidiCountAppls ()
  "Give the number of MidiShare applications currently opened"
  (ccl::ppc-ff-call (get-fun-addr "MidiCountAppls" *midishare*)
                    :signed-fullword))

;;;..................................................: MidiGetIndAppl
(defun MidiGetIndAppl (index)
  "Give the reference number of a MidiShare application from its index, a fixnum between 1 and (MidiCountAppls)"
  (ccl::ppc-ff-call (get-fun-addr "MidiGetIndAppl" *midishare*) 
                    :signed-halfword index :signed-halfword))

;;;..................................................: MidiGetNamedAppl
(defun MidiGetNamedAppl (name)
  "Give the reference number of a MidiShare application from its name"
  (with-cstrs ((s name))
    (ccl::ppc-ff-call (get-fun-addr "MidiGetNamedAppl" *midishare*) 
                      :address s :signed-halfword)))


;;;-----------------------------------------------------------------------
;;;			To Open and Close a MidiShare session
;;;-----------------------------------------------------------------------

;;;..................................................: MidiOpen
(defun MidiOpen (name)
  "Open a new MidiShare application with name name. Give a unique reference number."
  (with-cstrs ((s name))
    (ccl::ppc-ff-call (get-fun-addr "MidiOpen" *midishare*)
                      :address s :signed-halfword)))

;;;..................................................: MidiClose
(defun MidiClose (refNum)
  "Close an opened MidiShare application from its reference number"
  (ccl::ppc-ff-call (get-fun-addr "MidiClose" *midishare*)
                    :signed-halfword refNum :void))


;;;-----------------------------------------------------------------------
;;;			To Configure a MidiShare session
;;;-----------------------------------------------------------------------

;;;..................................................: MidiGetName
(defun MidiGetName (refNum)
  "Give the name of a MidiShare application from its reference number"
  (let ((res (ccl::ppc-ff-call (get-fun-addr "MidiGetName" *midishare*)
                               :signed-halfword refNum :address)))
    (unless (nullptrp res)
      (%get-cstring res))))

;;;..................................................: MidiSetName
(defun MidiSetName (refNum name)
  "Change the name of a MidiShare application"
   (with-cstrs ((s name))
     (ccl::ppc-ff-call (get-fun-addr "MidiSetName" *midishare*)
                       :signed-halfword refNum :address s :void)))

;;;..................................................: MidiGetInfo
(defun MidiGetInfo (refNum)
  "Give the 32-bits user defined content of the info field of a MidiShare application. 
 Analogous to window's refcon."
  (ccl::ppc-ff-call (get-fun-addr "MidiGetInfo" *midishare*) 
                    :signed-halfword refNum :address))

;;;..................................................: MidiSetInfo
(defun MidiSetInfo (refNum p)
  "Set the 32-bits user defined content of the info field of a MidiShare application. 
 Analogous to window's refcon."
  (ccl::ppc-ff-call (get-fun-addr "MidiSetInfo" *midishare*)
                    :signed-halfword refNum :address p :void))

;;;..................................................: MidiNewFilter
(defun MidiNewFilter ()
  "Returns a new filter"
  (ccl::ppc-ff-call (get-fun-addr "MidiNewFilter" *midishare*)
                    :address))

;;;..................................................: MidiFreeFilter
(defun MidiFreeFilter (f)
  "Delete a filter"
  (ccl::ppc-ff-call (get-fun-addr "MidiFreeFilter" *midishare*)
                    :address f :void))

;;;..................................................: MidiAcceptChan
(defun MidiAcceptChan (f c s)
  "Change the chan state of a filter"
  (ccl::ppc-ff-call (get-fun-addr "MidiAcceptChan" *midishare*)
                    :address f :signed-halfword c :unsigned-byte s :void))

;;;..................................................: MidiAcceptType
(defun MidiAcceptType (f c s)
  "Change the type state of a filter"
  (ccl::ppc-ff-call (get-fun-addr "MidiAcceptType" *midishare*)
                    :address f :signed-halfword c 
                    :unsigned-byte s :void))

;;;..................................................: MidiAcceptPort
(defun MidiAcceptPort (f c s)
  "Change the port state of a filter"
  (ccl::ppc-ff-call (get-fun-addr "MidiAcceptPort" *midishare*)
                    :address f :signed-halfword c 
                    :unsigned-byte s :void))

;;;..................................................: MidiIsAcceptedChan
(defun MidiIsAcceptedChan (f c)
  "Returns the chan state of a filter"
  (ccl::ppc-ff-call (get-fun-addr "MidiIsAcceptedChan" *midishare*)
                    :address f :signed-halfword c :unsigned-byte))

;;;..................................................: MidiIsAcceptedType
(defun MidiIsAcceptedType (f c)
  "Returns the type state of a filter"
  (ccl::ppc-ff-call (get-fun-addr "MidiIsAcceptedType" *midishare*)
                    :address f :signed-halfword c :unsigned-byte))

;;;..................................................: MidiIsAcceptedPort
(defun MidiIsAcceptedPort (f c)
  "Returns the port state of a filter"
  (ccl::ppc-ff-call (get-fun-addr "MidiIsAcceptedPort" *midishare*)
                    :address f :signed-halfword c :unsigned-byte))

;;;..................................................: MidiGetFilter
(defun MidiGetFilter (refNum)
  "Give a pointer to the input filter record of a MidiShare application. 
 Give NIL if no filter is installed"
  (ccl::ppc-ff-call (get-fun-addr "MidiGetFilter" *midishare*)
                    :signed-halfword refNum :address))

;;;..................................................: MidiSetFilter
(defun MidiSetFilter (refNum p)
  "Install an input filter. The argument p is a pointer to a filter record."
  (ccl::ppc-ff-call (get-fun-addr "MidiSetFilter" *midishare*)
                    :signed-halfword refNum :address p :void))

;;;..................................................: MidiGetRcvAlarm
(defun MidiGetRcvAlarm (refNum)
  "Get the adress of the receive alarm"
  (ccl::ppc-ff-call (get-fun-addr "MidiGetRcvAlarm" *midishare*)
                    :signed-halfword refNum :address ))

;;;..................................................: MidiSetRcvAlarm
(defun MidiSetRcvAlarm (refNum alarm)
  "Install a receive alarm"
  (ccl::ppc-ff-call (get-fun-addr "MidiSetRcvAlarm" *midishare*)
                    :signed-halfword refNum :address alarm :void))

;;;..................................................: MidiGetApplAlarm
(defun MidiGetApplAlarm (refNum)
  "Get the adress of the context alarm"
  (ccl::ppc-ff-call (get-fun-addr "MidiGetApplAlarm" *midishare*)
                    :signed-halfword refNum :address ))

;;;..................................................: MidiSetApplAlarm
(defun MidiSetApplAlarm (refNum alarm)
  "Install a context alarm"
  (ccl::ppc-ff-call (get-fun-addr "MidiSetApplAlarm" *midishare*)
                    :signed-halfword refNum :address alarm :void))


;;;-----------------------------------------------------------------------
;;;			To Manage MidiShare IAC and Midi Ports
;;;-----------------------------------------------------------------------

;;;..................................................: MidiConnect
(defun MidiConnect (src dst s)
  "Connect or disconnect two MidiShare applications"
  (ccl::ppc-ff-call (get-fun-addr "MidiConnect" *midishare*)
                    :signed-halfword src :signed-halfword dst 
                    :unsigned-byte s :void))

;;;..................................................: MidiIsConnected
(defun MidiIsConnected (src dst)
  "Test if two MidiShare applications are connected"
  (ccl::ppc-ff-call (get-fun-addr "MidiIsConnected" *midishare*)
                    :signed-halfword src :signed-halfword dst 
                    :unsigned-byte))

;;;..................................................: MidiGetPortState
(defun MidiGetPortState (port)
  "Give the state : open or closed of a MidiPort"
  (ccl::ppc-ff-call (get-fun-addr "MidiGetPortState" *midishare*)
                    :signed-halfword port :unsigned-byte))

;;;..................................................: MidiSetPortState
(defun MidiSetPortState (port state)
  "Open or close a MidiPort"
  (ccl::ppc-ff-call (get-fun-addr "MidiSetPortState" *midishare*)
                    :signed-halfword port :unsigned-byte state :void))

;;;-----------------------------------------------------------------------
;;;			To Manage MidiShare events
;;;-----------------------------------------------------------------------

;;;..................................................: MidiFreeSpace
(defun MidiFreeSpace ()
  "Amount of free MidiShare cells"
  (ccl::ppc-ff-call (get-fun-addr "MidiFreeSpace" *midishare*)
                    :unsigned-fullword))

;;;..................................................: MidiNewEv
(defun MidiNewEv (typeNum)
  "Allocate a new MidiEvent"
  (ccl::ppc-ff-call (get-fun-addr "MidiNewEv" *midishare*)
                    :signed-halfword typeNum :address))

;;;..................................................: MidiCopyEv
(defun MidiCopyEv (ev)
  "Duplicate a MidiEvent"
  (ccl::ppc-ff-call (get-fun-addr "MidiCopyEv" *midishare*)
                    :address ev :address))

;;;..................................................: MidiFreeEv
(defun MidiFreeEv (ev)
  "Free a MidiEvent"
  (ccl::ppc-ff-call (get-fun-addr "MidiFreeEv" *midishare*)
                    :address ev :void))

;;; HKT: I converted the next four from macros to functions because 
;;; earlier macros use them BEFORE their macroexpansions are known.

;;;..................................................: MidiSetField
(defun MidiSetField (ev f v)
  "Set a field of a MidiEvent"
  (ccl::ppc-ff-call (get-fun-addr "MidiSetField" *midishare*)
                    :address ev :signed-fullword f 
                    :signed-fullword v :void))

;;;..................................................: MidiGetField
(defun MidiGetField (ev f)
  "Get a field of a MidiEvent"
  (ccl::ppc-ff-call (get-fun-addr "MidiGetField" *midishare*)
                    :address ev :signed-fullword f :signed-fullword))

;;;..................................................: MidiAddField
(defun MidiAddField (ev val)
  "Append a field to a MidiEvent (only for sysex and stream)"
  (ccl::ppc-ff-call (get-fun-addr "MidiAddField" *midishare*)
                    :address ev :signed-fullword val :void))

;;;..................................................: MidiCountFields
(defun MidiCountFields (ev)
  "The number of fields of a MidiEvent"
  (ccl::ppc-ff-call (get-fun-addr "MidiCountFields" *midishare*)
                    :address ev :signed-fullword))

;;;-----------------------------------------------------------------------
;;;			To Manage MidiShare Sequences
;;;-----------------------------------------------------------------------

;;;..................................................: MidiNewSeq
(defun MidiNewSeq ()
  "Allocate an empty sequence"
  (ccl::ppc-ff-call (get-fun-addr "MidiNewSeq" *midishare*)
                    :address))

;;;..................................................: MidiAddSeq
(defun MidiAddSeq (seq ev)
  "Add an event to a sequence"
   (ccl::ppc-ff-call (get-fun-addr "MidiAddSeq" *midishare*)
                     :address seq  :address ev :void ))

;;;..................................................: MidiFreeSeq
(defun MidiFreeSeq (seq)
  "Free a sequence and its content"
  (ccl::ppc-ff-call (get-fun-addr "MidiFreeSeq" *midishare*)
                    :address seq :void))

;;;..................................................: MidiClearSeq
(defun MidiClearSeq (seq)
  "Free only the content of a sequence. The sequence become empty"
  (ccl::ppc-ff-call (get-fun-addr "MidiClearSeq" *midishare*)
                    :address seq :void))

;;;..................................................: MidiApplySeq
(defun MidiApplySeq (seq proc)
  "Call a function for every events of a sequence"
   (ccl::ppc-ff-call (get-fun-addr "MidiApplySeq" *midishare*)
                     :address seq :address proc :void))

;;;-----------------------------------------------------------------------
;;;				   MidiShare Time
;;;-----------------------------------------------------------------------

;;;..................................................: MidiGetTime
(defun MidiGetTime ()
  "give the current time"
  (ccl::ppc-ff-call (get-fun-addr "MidiGetTime" *midishare*)
                    :unsigned-fullword))

;;;-----------------------------------------------------------------------
;;;				To Send MidiShare events
;;;-----------------------------------------------------------------------

;;;..................................................: MidiSendIm
(defun MidiSendIm (refNum ev)
  "send an event now"
  (ccl::ppc-ff-call (get-fun-addr "MidiSendIm" *midishare*)
                    :signed-halfword refNum :address ev :void ))

;;;..................................................: MidiSend
(defun MidiSend (refNum ev)
  "send an event using its own date"
  (ccl::ppc-ff-call (get-fun-addr "MidiSend" *midishare*)
                    :signed-halfword refNum :address ev :void ))

;;;..................................................: MidiSendAt
(defun MidiSendAt (refNum ev date)
  "send an event at date <date>"
  (ccl::ppc-ff-call (get-fun-addr "MidiSendAt" *midishare*)
                    :signed-halfword refNum :address ev 
                    :unsigned-fullword date :void ))

;;;-----------------------------------------------------------------------
;;;                           To Receive MidiShare Events
;;;-----------------------------------------------------------------------

;;;..................................................: MidiCountEvs
(defun MidiCountEvs (refNum)
  "Give the number of events waiting in the reception fifo"
  (ccl::ppc-ff-call (get-fun-addr "MidiCountEvs" *midishare*)
                    :signed-halfword refNum :unsigned-fullword))

;;;..................................................: MidiGetEv
(defun MidiGetEv (refNum)
  "Read an event from the reception fifo"
  (ccl::ppc-ff-call (get-fun-addr "MidiGetEv" *midishare*)
                    :signed-halfword refNum :address))

;;;..................................................: MidiAvailEv
(defun MidiAvailEv (refNum)
  "Get a pointer to the first event in the reception fifo without removing it"
  (ccl::ppc-ff-call (get-fun-addr "MidiAvailEv" *midishare*)
                    :signed-halfword refNum :address))

;;;..................................................: MidiFlushEvs
(defun MidiFlushEvs (refNum)
  "Delete all the events waiting in the reception fifo"
  (ccl::ppc-ff-call (get-fun-addr "MidiFlushEvs" *midishare*)
                    :signed-halfword refNum :void))

;;;-----------------------------------------------------------------------
;;;                             To access shared data
;;;-----------------------------------------------------------------------

;;;..................................................: MidiReadSync
(defun MidiReadSync (adrMem)
  "Read and clear a memory address (not-interruptible)"
  (ccl::ppc-ff-call (get-fun-addr "MidiReadSync" *midishare*)
                    :address adrMem :address))

;;;..................................................: MidiWriteSync
(defun MidiWriteSync (adrMem val)
  "write if nil into a memory address (not-interruptible)"
  (ccl::ppc-ff-call (get-fun-addr "MidiWriteSync" *midishare*)
                    :address adrMem :address val :address))

;;;-----------------------------------------------------------------------
;;;                               Realtime Tasks
;;;-----------------------------------------------------------------------

;;;..................................................: MidiCall
(defun MidiCall (proc date refNum arg1 arg2 arg3)
  "Call the routine <proc> at date <date> with arguments <arg1> <arg2> <arg3>"
  (ccl::ppc-ff-call (get-fun-addr "MidiCall" *midishare*)
                    :address proc :unsigned-fullword date 
                    :signed-halfword refNum  :signed-fullword arg1 
                    :signed-fullword arg2 :signed-fullword arg3 :void))

;;;..................................................: MidiTask
(defun MidiTask (proc date refNum arg1 arg2 arg3)
  "Call the routine <proc> at date <date> with arguments <arg1> <arg2> <arg3>. Return a pointer to the corresponding typeProcess event"
  (ccl::ppc-ff-call (get-fun-addr "MidiTask" *midishare*)
                    :address proc :unsigned-fullword date 
                    :signed-halfword refNum  :signed-fullword arg1 
                    :signed-fullword arg2 :signed-fullword arg3 
                    :void))

;;;..................................................: MidiDTask
(defun MidiDTask (proc date refNum arg1 arg2 arg3)
  "Call the routine <proc> at date <date> with arguments <arg1> <arg2> <arg3>. 
 Return a pointer to the corresponding typeDProcess event"
  (ccl::ppc-ff-call (get-fun-addr "MidiDTask" *midishare*)
                    :address proc :unsigned-fullword date 
                    :signed-halfword refNum  :signed-fullword arg1 
                    :signed-fullword arg2 :signed-fullword arg3 :void))


;; 19/11/02 A VERIFIER !! 
;;;..................................................: MidiForgetTaskHdl
(defun MidiForgetTaskHdl (thdl)
  "Forget a previously scheduled typeProcess or typeDProcess event created by MidiTask or MidiDTask"
  (ccl::ppc-ff-call (get-fun-addr "MidiForgetTask" *midishare*)
                    :address thdl :void))

;;;..................................................: MidiForgetTask
(defun MidiForgetTask (ev)
  "Forget a previously scheduled typeProcess or typeDProcess event created by MidiTask or MidiDTask"
  (without-interrupts 
    (%stack-block ((taskptr 4))
      (%put-ptr taskptr ev) (midiforgetTaskHdl taskptr))))

;;;..................................................: MidiCountDTasks
(defun MidiCountDTasks (refNum)
  "Give the number of typeDProcess events waiting"
  (ccl::ppc-ff-call (get-fun-addr "MidiCountDTasks" *midishare*)
                    :signed-halfword refNum  :signed-fullword))

;;;..................................................: MidiFlushDTasks
(defun MidiFlushDTasks (refNum)
  "Remove all the typeDProcess events waiting"
  (ccl::ppc-ff-call (get-fun-addr "MidiFlushDTasks" *midishare*)
                    :signed-halfword refNum  :void))

;;;..................................................: MidiExec1DTask
(defun MidiExec1DTask (refNum)
  "Call the next typeDProcess waiting"
  (ccl::ppc-ff-call (get-fun-addr "MidiExec1DTask" *midishare*)
                    :signed-halfword refNum  :void))

;;;-----------------------------------------------------------------------
;;;                        Low Level MidiShare Memory Management
;;;-----------------------------------------------------------------------

;;;..................................................: MidiNewCell
(defun MidiNewCell ()
  "Allocate a basic Cell"
  (ccl::ppc-ff-call (get-fun-addr "MidiNewCell" *midishare*)
                    :address))

;;;..................................................: MidiFreeCell
(defun MidiFreeCell (cell)
  "Delete a basic Cell"
   (ccl::ppc-ff-call (get-fun-addr "MidiFreeCell" *midishare*)
                     :address cell :void))

;;;..................................................: MidiTotalSpace
(defun MidiTotalSpace ()
  "Total amount of Cells"
  (ccl::ppc-ff-call (get-fun-addr "MidiTotalSpace" *midishare*)
                    :unsigned-fullword))

;;;..................................................: MidiGrowSpace
(defun MidiGrowSpace (n)
  "Total amount of Cells"
   (ccl::ppc-ff-call (get-fun-addr "MidiGrowSpace" *midishare*)
                     :signed-fullword n :unsigned-fullword))


;;;-----------------------------------------------------------------------
;;;                        SMPTE Synchronisation functions
;;;-----------------------------------------------------------------------

;;;..................................................: MidiGetSyncInfo
(defun MidiGetSyncInfo (syncInfo)
  "Fill syncInfo with current synchronisation informations"
  (ccl::ppc-ff-call (get-fun-addr "MidiGetSyncInfo" *midishare*)
                    :address syncInfo :void))

;;;..................................................: MidiSetSyncMode
(defun MidiSetSyncMode (mode)
  "set the MidiShare synchroniation mode"
  (ccl::ppc-ff-call (get-fun-addr "MidiSetSyncMode" *midishare*)
                    :unsigned-halfword mode :void))

;;;..................................................: MidiGetExtTime
(defun MidiGetExtTime ()
  "give the current external time"
  (ccl::ppc-ff-call (get-fun-addr "MidiGetExtTime" *midishare*)
                    :signed-fullword))

;;;..................................................: MidiInt2ExtTime
(defun MidiInt2ExtTime (time)
  "convert internal time to external time"
  (ccl::ppc-ff-call (get-fun-addr "MidiInt2ExtTime" *midishare*)
                    :signed-fullword time :signed-fullword))

;;;..................................................: MidiExt2IntTime
(defun MidiExt2IntTime (time)
  "convert internal time to external time"
  (ccl::ppc-ff-call (get-fun-addr "MidiExt2IntTime" *midishare*)
                    :signed-fullword time :signed-fullword))

;;;..................................................: MidiTime2Smpte
(defun MidiTime2Smpte (time format smpteLocation)
  "convert time to Smpte location"
  (ccl::ppc-ff-call (get-fun-addr "MidiTime2Smpte" *midishare*)
                    :signed-fullword time :signed-halfword format 
                    :address smpteLocation :void))

;;;..................................................: MidiSmpte2Time
(defun MidiSmpte2Time (smpteLocation)
  "convert time to Smpte location"
  (ccl::ppc-ff-call (get-fun-addr "MidiSmpte2Time" *midishare*)
                    :address smpteLocation :signed-fullword))



;;;-----------------------------------------------------------------------
;;;                        Drivers functions
;;;-----------------------------------------------------------------------

;; A TESTER

;;;..................................................: MidiCountDrivers
(defun MidiCountDrivers ()
  "number of opened drivers"
  (ccl::ppc-ff-call (get-fun-addr "MidiCountDrivers" *midishare*)
                    :signed-halfword))

;;;..................................................: MidiGetIndDriver
(defun MidiGetIndDriver  (index)
  "Give the reference number of a MidiShare driver from its index a fixnum"
  (ccl::ppc-ff-call (get-fun-addr "MidiGetIndDriver" *midishare*)
                    :signed-halfword index :signed-halfword))

;;;..................................................: MidiGetDriverInfos
(defun MidiGetDriverInfos  (refNum  info)
  "Give information about a driver"
  (ccl::ppc-ff-call (get-fun-addr "MidiGetDriverInfos" *midishare*)
                    :signed-halfword refNum :address info :unsigned-byte))

;;;..................................................: MidiGetIndSlot
(defun MidiGetIndSlot  (refNum  index)
  "Give the reference number of a driver slot from its order number."
  (ccl::ppc-ff-call (get-fun-addr "MidiGetIndSlot" *midishare*)
                    :signed-halfword refNum  :signed-halfword index 
                    :address))

;;;..................................................: MidiGetSlotInfos
(defun MidiGetSlotInfos  (slotRefNum info)
  "Give information about a slot"
   (ccl::ppc-ff-call (get-fun-addr "MidiGetSlotInfos" *midishare*)
                     :address slotRefNum :address info :unsigned-byte))

;;;..................................................: MidiConnectSlot
(defun MidiConnectSlot  (port slotRefNum state)
  "Make or remove a connection between a slot and a MidiShare logical port"
   (ccl::ppc-ff-call (get-fun-addr "MidiConnectSlot" *midishare*)
                     :signed-halfword port  :address slotRefNum 
                     :unsigned-byte state :void))

;;;..................................................: MidiIsSlotConnected
(defun MidiIsSlotConnected  (port slotRefNum)
  "Test a connection between a slot and a MidiShare logical port"
   (ccl::ppc-ff-call (get-fun-addr "MidiGetIndSlot" *midishare*)
                     :signed-halfword port  :address slotRefNum 
                     :unsigned-byte))

(defun MidiNewSmpteLocation ()
  (ccl:make-record :TSmpteLocation))

(defun MidiFreeSmpteLocation (location)
  (ccl::free location))

(defun MidiNewSyncInfo ()
  (ccl:make-record :TSyncInfo))

(defun MidiFreeSyncInfo (info)
  (ccl::free info))

) ; END CCL-5.0

#-CCL-5.0
(progn

;;;-----------------------------------------------------------------------
;;;			To Know about MidiShare and Active Sessions
;;;-----------------------------------------------------------------------


;;; Interface description for a MidiShare PROCEDURE 
;;; with a word and a pointer parameter
;;; (ff-call *midiShare* :word <arg1> :ptr <arg2> :d0 <MidiShare routine #>)
;;;
;;; Interface description for a MidiShare FUNCTION (previous to MCL PPC 3.9)
;;; with a word and a pointer parameter and a pointer result
;;; (ff-call *midiShare*  :ptr (%null-ptr) :word <arg1> :ptr <arg2> :d0 <MidiShare routine #> :ptr)
;;;
;;; Interface description for a MidiShare FUNCTION (with MCL PPC 3.9) 
;;; with a word and a pointer parameter and a pointer result
;;; (ff-call *midiShare* :word <arg1> :ptr <arg2> :d0 <MidiShare routine #> :ptr)
;;;


;;; Entry point of MidiShare (setup at boot time by the "MidiShare" init)

(defvar *midiShare*)


;;;..................................................: MidiShare
(defun MidiShare ()
  "returns true if MidiShare is installed"
  (if (and (= (%get-word *midiShare*) #xD080)
           (= (%get-word *midiShare* 2) #xD080)) 1 0))

;;;..................................................: MidiGetVersion
(defun MidiGetVersion ()
  "Give MidiShare version as a fixnum. For example 131 as result, means : version 1.31"
  (ff-call *midiShare* :d0 0 :word))

;;;..................................................: MidiCountAppls
(defun MidiCountAppls ()
  "Give the number of MidiShare applications currently opened"
  (ff-call *midiShare* :d0 1 :word))

;;;..................................................: MidiGetIndAppl
(defun MidiGetIndAppl (index)
  "Give the reference number of a MidiShare application from its index, a fixnum between 1 and (MidiCountAppls)"
  (%%unsigned-to-signed-word 
    (ff-call *midiShare* :word index :d0 2 :word)))

;;;..................................................: MidiGetNamedAppl
(defun MidiGetNamedAppl (name)
  "Give the reference number of a MidiShare application from its name"
  (with-pstrs ((s name))
    (%%unsigned-to-signed-word (ff-call *midiShare* :ptr s :d0 3 :word))))

;;;-----------------------------------------------------------------------
;;;			To Open and Close a MidiShare session
;;;-----------------------------------------------------------------------

;;;..................................................: MidiOpen
(defun MidiOpen (name)
  "Open a new MidiShare application, with name name. Give a unique reference number."
  (with-pstrs ((s name))
    (%%unsigned-to-signed-word (ff-call *midiShare* :ptr s :d0 4 :word))))

;;;..................................................: MidiClose
(defun MidiClose (refNum)
  "Close an opened MidiShare application from its reference number"
  (ff-call *midiShare* :word refNum :d0 5))


;;;-----------------------------------------------------------------------
;;;			To Configure a MidiShare session
;;;-----------------------------------------------------------------------

;;;..................................................: MidiGetName
(defun MidiGetName (refNum)
  "Give the name of a MidiShare application from its reference number"
    (%%get-string (ff-call *midiShare* :word refNum :d0 6 :ptr)))

;;;..................................................: MidiSetName
(defun MidiSetName (refNum name)
  "Change the name of a MidiShare application"
  (with-pstrs ((s name))
    (ff-call *midiShare* :word refNum :ptr s :d0 7 )))

;;;..................................................: MidiGetInfo
(defun MidiGetInfo (refNum)
  "Give the 32-bits user defined content of the info field of a MidiShare application. Analogous to window's refcon."
  (ff-call *midiShare* :word refNum :d0 8 :ptr))

;;;..................................................: MidiSetInfo
(defun MidiSetInfo (refNum p)
  "Set the 32-bits user defined content of the info field of a MidiShare application. 
 Analogous to window's refcon."
  (ff-call *midiShare* :word refNum :ptr p :d0 9))

;;;..................................................: MidiNewFilter
(defun MidiNewFilter ()
  "Returns a new filter"
  (ff-call *midiShare* :d0 #x51 :ptr))

;;;..................................................: MidiFreeFilter
(defun MidiFreeFilter (f)
  "Delete a filter"
  (ff-call *midiShare* :ptr f :d0 #x52))

;;;..................................................: MidiAcceptChan
(defun MidiAcceptChan (f c s)
  "Change the chan state of a filter"
  (ff-call *midiShare* :ptr f :word c :word s :d0 #x54 ))

;;;..................................................: MidiAcceptType
(defun MidiAcceptType (f c s)
  "Change the type state of a filter"
  (ff-call *midiShare* :ptr f :word c :word s :d0 #x55 ))

;;;..................................................: MidiAcceptPort
(defun MidiAcceptPort (f c s)
  "Change the port state of a filter"
  (ff-call *midiShare* :ptr f :word c :word s :d0 #x53 ))

;;;..................................................: MidiIsAcceptedChan
(defun MidiIsAcceptedChan (f c)
  "Returns the chan state of a filter"
  (%%word-high-byte (ff-call *midiShare* :ptr f :word c :d0 #x57 :word)))

;;;..................................................: MidiIsAcceptedType
(defun MidiIsAcceptedType (f c)
  "Returns the type state of a filter"
  (%%word-high-byte (ff-call *midiShare* :ptr f :word c :d0 #x58 :word)))

;;;..................................................: MidiIsAcceptedPort
(defun MidiIsAcceptedPort (f c)
  "Returns the port state of a filter"
  (%%word-high-byte (ff-call *midiShare* :ptr f :word c :d0 #x56 :word)))

;;;..................................................: MidiGetFilter
(defun MidiGetFilter (refNum)
  "Give a pointer to the input filter record of a MidiShare application. Give NIL if no filter is installed"
  (ff-call *midiShare* :word refNum :d0 10 :ptr))

;;;..................................................: MidiSetFilter
(defun MidiSetFilter (refNum p)
  "Install an input filter. The argument p is a pointer to a filter record."
  (ff-call *midiShare* :word refNum :ptr p :d0 11))

;;;..................................................: MidiGetRcvAlarm
(defun MidiGetRcvAlarm (refNum)
  "Get the adress of the receive alarm"
  (ff-call *midiShare* :word refNum :d0 #x0C :ptr))

;;;..................................................: MidiSetRcvAlarm
(defun MidiSetRcvAlarm (refNum alarm)
  "Install a receive alarm"
  (ff-call *midiShare* :word refNum :ptr alarm :d0 #x0D))

;;;..................................................: MidiGetApplAlarm
(defun MidiGetApplAlarm (refNum)
  "Get the adress of the context alarm"
  (ff-call *midiShare* :word refNum :d0 #x0E :ptr))

;;;..................................................: MidiSetApplAlarm
(defun MidiSetApplAlarm (refNum alarm)
  "Install a context alarm"
  (ff-call *midiShare* :word refNum :ptr alarm :d0 #x0F))

;;;-----------------------------------------------------------------------
;;;			To Manage MidiShare IAC and Midi Ports
;;;-----------------------------------------------------------------------

;;;..................................................: MidiConnect
(defun MidiConnect (src dst state)
  "Connect or disconnect two MidiShare applications"
  (ff-call *midiShare* :word src :word dst :word state :d0 #x10))

;;;..................................................: MidiIsConnected
(defun MidiIsConnected (src dst)
  "Test if two MidiShare applications are connected"
  (%%word-high-byte (ff-call *midiShare* :word src :word dst :d0 #x11 :word)))

;;;..................................................: MidiGetPortState
(defun MidiGetPortState (port)
  "Give the state : open or closed, of a MidiPort"
  (%%word-high-byte (ff-call *midiShare* :word port :d0 #x12 :word)))

;;;..................................................: MidiSetPortState
(defun MidiSetPortState (port state)
  "Open or close a MidiPort"
  (ff-call *midiShare* :word port :word state :d0 #x13))

;;;-----------------------------------------------------------------------
;;;			To Manage MidiShare events
;;;-----------------------------------------------------------------------

;;;..................................................: MidiFreeSpace
(defun MidiFreeSpace ()
  "Amount of free MidiShare cells"
  (ff-call *midiShare* :d0 #x14 :long))

;;;..................................................: MidiNewEv
(defun MidiNewEv (typeNum)
  "Allocate a new MidiEvent"
  (ff-call *midiShare* :word typeNum :d0 #x15 :ptr))

;;;..................................................: MidiCopyEv
(defun MidiCopyEv (ev)
  "Duplicate a MidiEvent"
  (ff-call *midiShare* :ptr ev :d0 #x16 :ptr))

;;;..................................................: MidiFreeEv
(defun MidiFreeEv (ev)
  "Free a MidiEvent"
  (ff-call *midiShare* :ptr ev :d0 #x17))

;;;..................................................: MidiSetField
(defun MidiSetField (ev field val)
  "Set a field of a MidiEvent"
  (ff-call *midiShare* :ptr ev :long field :long val :d0 #x3A))

;;;..................................................: MidiGetField
(defun MidiGetField (ev field)
  "Get a field of a MidiEvent"
  (ff-call *midiShare* :ptr ev :long field :d0 #x3B :long))

;;;..................................................: MidiAddField
(defun MidiAddField (ev val)
  "Append a field to a MidiEvent (only for sysex and stream)"
  (ff-call *midiShare* :ptr ev :long val :d0 #x1A))

;;;..................................................: MidiCountFields
(defun MidiCountFields (ev)
  "The number of fields of a MidiEvent"
  (ff-call *midiShare* :ptr ev :d0 #x3C :long))

;;;-----------------------------------------------------------------------
;;			To Manage MidiShare Sequences
;;;-----------------------------------------------------------------------

;;;..................................................: MidiNewSeq
(defun MidiNewSeq ()
  "Allocate an empty sequence"
  (ff-call *midiShare* :d0 #x1D :ptr))

;;;..................................................: MidiAddSeq
(defun MidiAddSeq (seq ev)
  "Add an event to a sequence"
  (ff-call *midiShare* :ptr seq :ptr ev :d0 #x1E))

;;;..................................................: MidiFreeSeq
(defun MidiFreeSeq (seq)
  "Free a sequence and its content"
  (ff-call *midiShare* :ptr seq :d0 #x1F))

;;;..................................................: MidiClearSeq
(defun MidiClearSeq (seq)
  "Free only the content of a sequence. The sequence become empty"
  (ff-call *midiShare* :ptr seq :d0 #x20))

;;;..................................................: MidiApplySeq
(defun MidiApplySeq (seq proc)
  "Call a function for every events of a sequence"
  (ff-call *midiShare* :ptr seq :ptr proc :d0 #x21))

;;;-----------------------------------------------------------------------
;;;				   MidiShare Time
;;;-----------------------------------------------------------------------

;;;..................................................: MidiGetTime
(defun MidiGetTime ()
  "give the current time"
  (ff-call *midiShare* :d0 #x22 :long))

;;;-----------------------------------------------------------------------
;;;				To Send MidiShare events
;;;-----------------------------------------------------------------------

;;;..................................................: MidiSendIm
(defun MidiSendIm (refNum ev)
  "send an event now"
  (ff-call *midiShare* :word refNum :ptr ev :d0 #x23))

;;;..................................................: MidiSend
(defun MidiSend (refNum ev)
  "send an event using its own date"
  (ff-call *midiShare* :word refNum :ptr ev :d0 #x24))

;;;..................................................: MidiSendAt
(defun MidiSendAt (refNum ev date)
  "send an event at date <date>"
  (ff-call *midiShare* :word refNum :ptr ev :long date :d0 #x25))

;;;-----------------------------------------------------------------------
;;;                          To Receive MidiShare Events
;;;-----------------------------------------------------------------------

;;;..................................................: MidiCountEvs
(defun MidiCountEvs (refNum)
  "Give the number of events waiting in the reception fifo"
  (ff-call *midiShare* :word refNum :d0 #x26 :long))

;;;..................................................: MidiGetEv
(defun MidiGetEv (refNum)
  "Read an event from the reception fifo"
  (ff-call *midiShare* :word refNum :d0 #x27 :ptr))

;;;..................................................: MidiAvailEv
(defun MidiAvailEv (refNum)
  "Get a pointer to the first event in the reception fifo without removing it"
  (ff-call *midiShare* :word refNum :d0 #x28 :ptr))

;;;..................................................: MidiFlushEvs
(defun MidiFlushEvs (refNum)
  "Delete all the events waiting in the reception fifo"
  (ff-call *midiShare* :word refNum :d0 #x29))

;;;-----------------------------------------------------------------------
;;;                             To access shared data
;;;-----------------------------------------------------------------------

;;;..................................................: MidiReadSync
(defun MidiReadSync (adrMem)
  "Read and clear a memory address (not-interruptible)"
  (ff-call *midiShare* :ptr adrMem :d0 #x2A :ptr))

;;;..................................................: MidiWriteSync
(defun MidiWriteSync (adrMem val)
  "write if nil into a memory address (not-interruptible)"
  (ff-call *midiShare* :ptr adrMem :ptr val :d0 #x2B :ptr))

;;;-----------------------------------------------------------------------
;;;                               Realtime Tasks
;;;-----------------------------------------------------------------------

;;;..................................................: MidiCall
(defun MidiCall (proc date refNum arg1 arg2 arg3)
  "Call the routine <proc> at date <date> with arguments <arg1> <arg2> <arg3>"
  (ff-call *midiShare* :ptr proc :long date :word refNum :long arg1 :long arg2 :long arg3 :d0 #x2C))

;;;..................................................: MidiTask
(defun MidiTask (proc date refNum arg1 arg2 arg3)
  "Call the routine <proc> at date <date> with arguments <arg1> <arg2> <arg3>.  Return a pointer to the corresponding typeProcess event"
  (ff-call *midiShare* :ptr proc :long date :word refNum :long arg1 :long arg2 :long arg3 :d0 #x2D :ptr))

;;;..................................................: MidiDTask
(defun MidiDTask (proc date refNum arg1 arg2 arg3)
  "Call the routine <proc> at date <date> with arguments <arg1> <arg2> <arg3>. 
 Return a pointer to the corresponding typeDProcess event"
  (ff-call *midiShare* :ptr proc :long date :word refNum :long arg1 :long arg2 :long arg3 :d0 #x2E :ptr))

;;;..................................................: MidiForgetTaskHdl
(defun MidiForgetTaskHdl (thdl)
  "Forget a previously scheduled typeProcess or typeDProcess event created by MidiTask or MidiDTask"
  (ff-call *midiShare* :ptr thdl :d0 #x2F))

;;;..................................................: MidiForgetTask
(defun MidiForgetTask (ev)
  "Forget a previously scheduled typeProcess or typeDProcess event created by MidiTask or MidiDTask"
  (without-interrupts 
    (%stack-block ((taskptr 4))
      (%put-ptr taskptr ev) (midiforgetTaskHdl taskptr))))

;;;..................................................: MidiCountDTasks
(defun MidiCountDTasks (refNum)
  "Give the number of typeDProcess events waiting"
  (ff-call *midiShare* :word refNum :d0 #x30 :long))

;;;..................................................: MidiFlushDTasks
(defun MidiFlushDTasks (refNum)
  "Remove all the typeDProcess events waiting"
  (ff-call *midiShare* :word refNum :d0 #x31))

;;;..................................................: MidiExec1DTask
(defun MidiExec1DTask (refNum)
  "Call the next typeDProcess waiting"
  (ff-call *midiShare* :word refNum :d0 #x32))

;;;-----------------------------------------------------------------------
;;;                        Low Level MidiShare Memory Management
;;;-----------------------------------------------------------------------

;;;..................................................: MidiNewCell
(defun MidiNewCell ()
  "Allocate a basic Cell"
  (ff-call *midiShare* :d0 #x33 :ptr))

;;;..................................................: MidiFreeCell
(defun MidiFreeCell (cell)
  "Delete a basic Cell"
  (ff-call *midiShare* :ptr cell :d0 #x34))

;;;..................................................: MidiTotalSpace
(defun MidiTotalSpace ()
  "Total amount of Cells"
  (ff-call *midiShare* :d0 #x35 :long))

;;;..................................................: MidiGrowSpace
(defun MidiGrowSpace (n)
  "Total amount of Cells"
  (ff-call *midiShare* :long n :d0 #x36 :long))


;;;-----------------------------------------------------------------------
;;;                        SMPTE Synchronisation functions
;;;-----------------------------------------------------------------------

;;;..................................................: MidiGetSyncInfo
(defun MidiGetSyncInfo (syncInfo)
  "Fill syncInfo with current synchronisation informations"
  (ff-call *midiShare* :ptr syncInfo :d0 #x38))

;;;..................................................: MidiSetSyncMode
(defun MidiSetSyncMode (mode)
  "set the MidiShare synchroniation mode"
  (ff-call *midiShare* :word mode :d0 #x39))

;;;..................................................: MidiGetExtTime
(defun MidiGetExtTime ()
  "give the current external time"
  (ff-call *midiShare* :d0 #x3D :long))

;;;..................................................: MidiInt2ExtTime
(defun MidiInt2ExtTime (time)
  "convert internal time to external time"
  (ff-call *midiShare* :long time :d0 #x3E :long))

;;;..................................................: MidiExt2IntTime
(defun MidiExt2IntTime (time)
  "convert internal time to external time"
  (ff-call *midiShare* :long time :d0 #x3F :long))

;;;..................................................: MidiTime2Smpte
(defun MidiTime2Smpte (time format smpteLocation)
  "convert time to Smpte location"
  (ff-call *midiShare* :long time :word format :ptr smpteLocation :d0 #x40))

;;;..................................................: MidiSmpte2Time
(defun MidiSmpte2Time (smpteLocation)
  "convert time to Smpte location"
  (ff-call *midiShare* :ptr smpteLocation :d0 #x41 :long))


;;;-----------------------------------------------------------------------
;;;                        Drivers functions
;;;-----------------------------------------------------------------------


;; A TESTER
;;;..................................................: MidiCountDrivers
(defun MidiCountDrivers ()
  "number of opened drivers"
  (%%unsigned-to-signed-word (ff-call *midiShare* :d0 #x46 :word)))

;;;..................................................: MidiGetIndDriver
(defun MidiGetIndDriver  (index)
  "Give the reference number of a MidiShare driver from its index, a fixnum"
  (%%unsigned-to-signed-word (ff-call *midiShare* :word index :d0 #x47 :word)))

;;;..................................................: MidiGetDriverInfos
(defun MidiGetDriverInfos  (refNum  info)
  "Give information about a driver"
  (%%word-high-byte 
   (ff-call *midiShare* :word refNum :ptr info :d0 #x48 :word)))

;;;..................................................: MidiGetIndSlot
(defun MidiGetIndSlot  (refNum  index)
  "Give the reference number of a driver slot from its order number."
   (ff-call *midiShare* :word refNum :word index  :d0 #x4A :long))

;;;..................................................: MidiGetSlotInfos
 (defun MidiGetSlotInfos  (slotRefNum info)
    "Give information about a slot"
    (%%word-high-byte
     (ff-call *midiShare* :long slotRefNum :ptr info :d0 #x4C :word)))

;;;..................................................: MidiConnectSlot
 (defun MidiConnectSlot  (port slotRefNum state)
    "Make or remove a connection between a slot and a MidiShare logical port"
    ;; HKT: :short state changed to :word state
    (ff-call *midiShare* :word port :long slotRefNum
             :word  state :d0 #x4D))

;;;..................................................: MidiIsSlotConnected
(defun MidiIsSlotConnected  (port slotRefNum)
  "Test a connection between a slot and a MidiShare logical port"
  (%%word-high-byte
   (ff-call *midiShare* :word port :long slotRefNum :d0 #x4E :word)))


) ; END #-:CCL-5.0


;;;-----------------------------------------------------------------------
;;;-----------------------------------------------------------------------
;;;
;;; 			To Install and Remove the MidiShare Interface
;;;
;;;-----------------------------------------------------------------------
;;;-----------------------------------------------------------------------

;;;-----------------------------------------------------------------------
;;; 	 		MidiShare Startup and Quit Actions
;;;-----------------------------------------------------------------------


;;;..........................................: install-midishare-interface
#+CCL-5.0
(defun install-midishare-interface ()
  (midishare-framework)
  (unless (midishare) (error "MidiShare not installed")))

#-CCL-5.0
(defun install-midishare-interface ()
  (setq *midiShare* (%get-ptr (%int-to-ptr #xB8)))
  (unless (midishare) (error "MidiShare not installed")))

;;;..........................................: remove-midishare-interface
(defun remove-midishare-interface ()
  (setq *midiShare* nil))

) ; End of MCL interface


#+openmcl
(eval-when (:compile-toplevel :execute)
  ;; locate midishare's ffi databases
  (ccl:use-interface-dir :midishare))

#+openmcl

;;;-----------------------------------------------------------------------
;;; 			Interface for OpenMCL/OSX
;;;-----------------------------------------------------------------------

(progn

;;;
;;; Midishare entry point. (midishare-framework) must be called
;;; before Midishare can be used.
;;;

(defvar *midishare* nil)

(defun midishare-framework ()
  (or *midishare*
      (setf *midishare*
            (ccl:open-shared-library
             "/System/Library/Frameworks/MidiShare.framework/MidiShare"))))

; (midishare-framework)

;;;
;;; Macros for accessing MidiShare Events data structures
;;;

(defun nullptrp (p) (ccl:%null-ptr-p p))
(defun nullptr () (ccl:%null-ptr))

;;;   Functions common to every type of event

(defun link (e &optional (d nil d?))
  "read or set the link of an event"
  (if d?
    (setf (ccl:pref e :<tm>idi<e>v.link) d)
    (ccl:pref e :<tm>idi<e>v.link)))

(defun date (e &optional d)
  "read or set the date of an event"
  (if d
    (setf (ccl:pref e :<tm>idi<e>v.date) d)
    (ccl:pref e :<tm>idi<e>v.date)))

(defun evtype (e &optional v)
  "read or set the type of an event. Be careful in modifying the type of an event"
  (if v
    (setf (ccl:pref e :<tm>idi<e>v.ev<t>ype) v)
    (ccl:pref e :<tm>idi<e>v.ev<t>ype)))

(defun ref (e &optional v)
  "read or set the reference number of an event"
  (if v
    (setf (ccl:pref e :<tm>idi<e>v.ref<n>um) v)
    (ccl:pref e :<tm>idi<e>v.ref<n>um)))

(defun port (e &optional v)
  "read or set the port number of an event"
  (if v
    (setf (ccl:pref e :<tm>idi<e>v.port) v)
    (ccl:pref e :<tm>idi<e>v.port)))

(defun chan (e &optional v)
  "read or set the chan number of an event"
  (if v
    (setf (ccl:pref e :<tm>idi<e>v.chan) v)
    (ccl:pref e :<tm>idi<e>v.chan)))

(defun field (e &optional f v)
  "give the number of fields or read or set a particular field of an event"
  (if f
    (if v
      (midiSetField e f v)
      (midiGetField e f))
    (midiCountFields e)))

(defun fieldsList (e &optional (n 4))
  "collect all the fields of an event into a list"
  (let (l)
    (dotimes (i (min n (midicountfields e)))
      (push (midigetfield e i) l))
    (nreverse l)))

;;;    Specific to typeNote events

(defun pitch (e &optional v)
  "read or set the pitch of an event"
  (if v
    (setf (ccl:pref e :<tm>idi<e>v.info.note.pitch) v)
    (ccl:pref e :<tm>idi<e>v.info.note.pitch)))

(defun vel (e &optional v)
  "read or set the velocity of an event"
  (if v
    (setf (ccl:pref e :<tm>idi<e>v.info.note.vel) v)
    (ccl:pref e :<tm>idi<e>v.info.note.vel)))

(defun dur (e &optional v)
  "read or set the duration of an event"
  (if v
    (setf (ccl:pref e :<tm>idi<e>v.info.note.dur) v)
    (ccl:pref e :<tm>idi<e>v.info.note.dur)))

;;;    Specific to other types of events

(defun linkSE (e &optional (d nil d?))
  "read or set the link of an SEXevent "
  (if d?
    (setf (ccl:pref e :<tm>idi<e>v.info.link<se>) d)
    (ccl:pref e :<tm>idi<e>v.info.link<se>)))

(defun linkST (e &optional (d nil d?))
  "read or set the link of an STevent "
  (if d?
    (setf (ccl:pref e :<tm>idi<e>v.info.link<st>) d)
    (ccl:pref e :<tm>idi<e>v.info.link<st>)))

(defun kpress (e &optional v)
  (if v
    (setf (ccl:pref e :<tm>idi<e>v.info.note.vel) v)
    (ccl:pref e :<tm>idi<e>v.info.note.vel)))

(defun ctrl (e &optional v)
  (if v
    (midisetfield e 0 v)
    (midigetfield e 0)))

(defun param (e &optional v)
  (if v
    (midisetfield e 0 v)
    (midigetfield e 0)))

(defun num (e &optional v)
  (if v
    (midisetfield e 0 v)
    (midigetfield e 0)))

(defun prefix (e &optional v)
  (if v
    (midisetfield e 0 v)
    (midigetfield e 0)))

(defun tempo (e &optional v)
  (if v
    (midisetfield e 0 v)
    (midigetfield e 0)))

(defun seconds (e &optional v)
  (if v
    (midisetfield e 0 v)
    (midigetfield e 0)))

(defun subframes (e &optional v)
  (if v
    (midisetfield e 1 v)
    (midigetfield e 1)))

(defun val (e &optional v)
  (if v
    (midisetfield e 1 v)
    (midigetfield e 1)))

(defun pgm (e &optional v)
  (if v
    (setf (ccl:pref e :<tm>idi<e>v.info.note.pitch) v)
    (ccl:pref e :<tm>idi<e>v.info.note.pitch)))

(defun bend (e &optional v)
  "read or set the bend value of an event"
  (if v
    (multiple-value-bind (ms7b ls7b) (floor (+ v 8192) 128)
      (setf (ccl:pref e :<tm>idi<e>v.info.note.pitch) ls7b)
      (setf (ccl:pref e :<tm>idi<e>v.info.note.vel) ms7b))
    (- (+ (ccl:pref e :<tm>idi<e>v.info.note.pitch) 
          (* 128 (ccl:pref e :<tm>idi<e>v.info.note.vel)))
       8192)))

(defun clk (e &optional v)
  (if v
    (multiple-value-bind (ms7b ls7b) (floor (round (/ v 6)) 128)
      (setf (ccl:pref e :<tm>idi<e>v.info.note.pitch) ls7b)
      (setf (ccl:pref e :<tm>idi<e>v.info.note.vel) ms7b))
    (* 6 (+ (pitch e) (* 128 (vel e)))) ))

(defun song (e &optional v)
  (if v
    (setf (ccl:pref e :<tm>idi<e>v.info.note.pitch) v)
    (ccl:pref e :<tm>idi<e>v.info.note.pitch)))

(defun fields (e &optional v)
  (if v
    (let ((e e)) (mapc #'(lambda (f) (midiaddfield e f)) v))
    (let (l (e e))
      (dotimes (i (midicountfields e))
        (push (midigetfield e i) l)) (nreverse l)) ))

(defun text (e &optional s)
  (if s
    (fields e (map 'list #'char-code s))
    (map 'string #'character (fields e)) ))

(defun fmsg (e &optional v)
  (if v
    (setf (ccl:pref e :<tm>idi<e>v.info.note.pitch) v)
    (ccl:pref e :<tm>idi<e>v.info.note.pitch)))

(defun fcount (e &optional v)
  (if v
    (setf (ccl:pref e :<tm>idi<e>v.info.note.vel) v)
    (ccl:pref e :<tm>idi<e>v.info.note.vel)))

(defun tsnum (e &optional v)
  (if v
    (midisetfield e 0 v)
    (midigetfield e 0)))

(defun tsdenom (e &optional v)
  (if v
    (midisetfield e 1 v)
    (midigetfield e 1)))

(defun tsclick (e &optional v)
  (if v
    (midisetfield e 2 v)
    (midigetfield e 2)))

(defun tsquarter (e &optional v)
  (if v
    (midisetfield e 3 v)
    (midigetfield e 3)))

(defun alteration (e &optional v)
  (if v
    (midisetfield e 0 v)
    (midigetfield e 0)))

(defun minor-scale (e &optional v)
  (if v
    (midisetfield e 1 (if v 1 0))
    (= 1 (midigetfield e 1))))

(defun info (e &optional d)
  "read or set the info of an event"
  (if d
    (setf (ccl:pref e :<tm>idi<e>v.info) d)
    (ccl:pref e :<tm>idi<e>v.info)))

;;;
;;;  Macros for accessing MidiShare Sequences data structures
;;;

(defun firstEv (s &optional (e nil e?))
  "read or set the first event of a sequence"
  (if e?
    (setf (ccl:pref s :<tm>idi<s>eq.first) e)
    (ccl:pref s :<tm>idi<s>eq.first)))

(defun lastEv (s &optional (e nil e?))
  "read or set the last event of a sequence"
  (if e?
    (setf (ccl:pref s :<tm>idi<s>eq.last) e)
    (ccl:pref s :<tm>idi<s>eq.last)))

;;;
;;; To Know about MidiShare and Active Sessions
;;;

(defun MidiShare ()
  "returns true if MidiShare is installed"
  (if (null *midishare*) 0 1)) ; hkt: The MCL version checked a macptr.

(defun MidiGetVersion () 
  "Give MidiShare version as a fixnum. For example 131 as result, means : version 1.31"
  (#_MidiGetVersion))

(defun MidiCountAppls ()
  "Give the number of MidiShare applications currently opened"
  (#_MidiCountAppls ))

(defun MidiGetIndAppl (index)
  "Give the reference number of a MidiShare application from its index, a fixnum between 1 and (MidiCountAppls)"
  (#_MidiGetIndAppl index))

(defun MidiGetNamedAppl (name)
  "Give the reference number of a MidiShare application from its name"
  (ccl:with-cstrs ((s name))
    (#_MidiGetNamedAppl s)))

;;;
;;; To Open and Close a MidiShare session
;;;

(defun MidiOpen (name)
  "Open a new MidiShare application, with name name. Give a unique reference number."
  (ccl:with-cstrs ((s name))
    (#_MidiOpen s)))

(defun MidiClose (refNum)
  "Close an opened MidiShare application from its reference number"
  (#_MidiClose refNum))

;;;
;;; To Configure a MidiShare session
;;;
;;; HKT: check the f and p args for what these pointers are passing..

(defun MidiGetName (refNum)
  "Give the name of a MidiShare application from its reference number"
  (let ((res (#_MidiGetName refNum)))
    (unless (ccl:%null-ptr-p res)
      (ccl:%get-cstring res))))

(defun MidiSetName (refNum name)
  "Change the name of a MidiShare application"
  (ccl:with-cstrs ((s name))
    (#_MidiSetName refNum s)))

(defun MidiGetInfo (refNum)
  "Give the 32-bits user defined content of the info field of a MidiShare application. Analogous to window's refcon."
  (#_MidiGetInfo refNum))

(defun MidiSetInfo (refNum p)
  "Set the 32-bits user defined content of the info field of a MidiShare application. Analogous to window's refcon."
  (#_MidiSetInfo refNum p))

(defun MidiNewFilter ()
  "Returns a new filter"
  (#_MidiNewFilter))

(defun MidiFreeFilter (f)
  "Delete a filter"
  (#_MidiFreeFilter f))

(defun MidiAcceptChan (f c s)
  "Change the chan state of a filter"
  (#_MidiAcceptChan f c s))

(defun MidiAcceptType (f c s)
  "Change the type state of a filter"
  (#_MidiAcceptType f c s))

(defun MidiAcceptPort (f c s)
  "Change the port state of a filter"
  (#_MidiAcceptPort f c s))

(defun MidiIsAcceptedChan (f c)
  "Returns the chan state of a filter"
  (#_MidiIsAcceptedChan f c))

(defun MidiIsAcceptedType (f c)
  "Returns the type state of a filter"
  (#_MidiIsAcceptedType f c))

(defun MidiIsAcceptedPort (f c)
  "Returns the port state of a filter"
  (#_MidiIsAcceptedPort f c))

(defun MidiGetFilter (refNum)
  "Give a pointer to the input filter record of a MidiShare application. Give NIL if no filter is installed"
  (#_MidiGetFilter refNum))

(defun MidiSetFilter (refNum p)
  "Install an input filter. The argument p is a pointer to a filter record."
  (#_MidiSetFilter refNum p))

(defun MidiGetRcvAlarm (refNum)
  "Get the adress of the receive alarm"
  (#_MidiGetRcvAlarm refNum))

(defun MidiSetRcvAlarm (refNum alarm)
  "Install a receive alarm"
  (#_MidiSetRcvAlarm refNum alarm))

(defun MidiGetApplAlarm (refNum)
  "Get the adress of the context alarm"
  (#_MidiGetApplAlarm refNum))

(defun MidiSetApplAlarm (refNum alarm)
  "Install a context alarm"
  (#_MidiSetApplAlarm refNum alarm))

;;;
;;; To Manage MidiShare IAC and Midi Ports
;;;

(defun MidiConnect (src dst s)
  "Connect or disconnect two MidiShare applications"
  (#_MidiConnect src dst s))

(defun MidiIsConnected (src dst)
  "Test if two MidiShare applications are connected"
  (#_MidiIsConnected src dst))

(defun MidiGetPortState (port)
  "Give the state : open or closed, of a MidiPort"
  (#_MidiGetPortState port))

(defun MidiSetPortState (port state)
  "Open or close a MidiPort"
  (#_MidiSetPortState port state))

;;;
;;; To Manage MidiShare events
;;;

(defun MidiFreeSpace ()
  "Amount of free MidiShare cells"
  (#_MidiFreeSpace))

(defun MidiNewEv (typeNum)
  "Allocate a new MidiEvent"
  (#_MidiNewEv typeNum))

(defun MidiCopyEv (ev)
  "Duplicate a MidiEvent"
  (#_MidiCopyEv ev))

(defun MidiFreeEv (ev)
  "Free a MidiEvent"
  (#_MidiFreeEv ev))

(defun MidiSetField (ev f v)
  "Set a field of a MidiEvent"
  (#_MidiSetField ev f v))

(defun MidiGetField (ev f)
  "Get a field of a MidiEvent"
  (#_MidiGetField ev f))

(defun MidiAddField (ev val)
  "Append a field to a MidiEvent (only for sysex and stream)"
  (#_MidiAddField ev val))

(defun MidiCountFields (ev)
  "The number of fields of a MidiEvent"
  (#_MidiCountFields ev))

;;;
;;; To Manage MidiShare Sequences
;;;

(defun MidiNewSeq ()
  "Allocate an empty sequence"
  (#_MidiNewSeq))

(defun MidiAddSeq (seq ev)
  "Add an event to a sequence"
  (#_MidiAddSeq seq ev))

(defun MidiFreeSeq (seq)
  "Free a sequence and its content"
  (#_MidiFreeSeq seq))

(defun MidiClearSeq (seq)
  "Free only the content of a sequence. The sequence become empty"
  (#_MidiClearSeq seq))

(defun MidiApplySeq (seq proc)
  "Call a function for every events of a sequence"
  (#_MidiApplySeq seq proc))

;;;
;;; MidiShare Time
;;;

(defun MidiGetTime ()
  "give the current time"
  (#_MidiGetTime))

;;;
;;; To Send MidiShare events
;;;

(defun MidiSendIm (refNum ev)
  "send an event now"
  (#_MidiSendIm refNum ev))

(defun MidiSend (refNum ev)
  "send an event using its own date"
  (#_MidiSend refNum ev))

(defun MidiSendAt (refNum ev date)
  "send an event at date <date>"
  (#_MidiSendAt refNum ev date))

;;;
;;;  To Receive MidiShare Events
;;;

(defun MidiCountEvs (refNum)
  "Give the number of events waiting in the reception fifo"
  (#_MidiCountEvs refNum))

(defun MidiGetEv (refNum)
  "Read an event from the reception fifo"
  (#_MidiGetEv refNum))

(defun MidiAvailEv (refNum)
  "Get a pointer to the first event in the reception fifo without removing it"
  (#_MidiAvailEv refNum))

(defun MidiFlushEvs (refNum)
  "Delete all the events waiting in the reception fifo"
  (#_MidiFlushEvs refNum))

;;; To access shared data

(defun MidiReadSync (adrMem)
  "Read and clear a memory address (not-interruptible)"
  (#_MidiReadSync adrMem))

(defun MidiWriteSync (adrMem val)
  "write if nil into a memory address (not-interruptible)"
  (#_MidiWriteSync adrMem val))

;;;
;;; Realtime Tasks
;;;

(defun MidiCall (proc date refNum arg1 arg2 arg3)
  "Call the routine <proc> at date <date> with arguments <arg1> <arg2> <arg3>"
  (#_MidiCall proc date refNum arg1 arg2 arg3))

(defun MidiTask (proc date refNum arg1 arg2 arg3)
  "Call the routine <proc> at date <date> with arguments <arg1> <arg2> <arg3>. Return a pointer to the corresponding typeProcess event"
  (#_MidiTask proc date refNum arg1 arg2 arg3))

(defun MidiDTask (proc date refNum arg1 arg2 arg3)
  "Call the routine <proc> at date <date> with arguments <arg1> <arg2> <arg3>. Return a pointer to the corresponding typeDProcess event"
  (#_MidiDTask proc date refNum arg1 arg2 arg3))

(defun MidiForgetTaskHdl (thdl)
  "Forget a previously scheduled typeProcess or typeDProcess event created by MidiTask or MidiDTask"
  (#_MidiForgetTask thdl))

(defun MidiForgetTask (ev)
  "Forget a previously scheduled typeProcess or typeDProcess event created by MidiTask or MidiDTask"
  (ccl:without-interrupts 
   (ccl:%stack-block ((taskptr 4))
     (ccl:%put-ptr taskptr ev) 
     (MidiForgetTask taskptr))))

(defun MidiCountDTasks (refNum)
  "Give the number of typeDProcess events waiting"
  (#_MidiCountDTasks refNum))

(defun MidiFlushDTasks (refNum)
  "Remove all the typeDProcess events waiting"
  (#_MidiFlushDTasks refNum))

(defun MidiExec1DTask (refNum)
  "Call the next typeDProcess waiting"
  (#_MidiExec1DTask refNum))

;;;
;;; Low Level MidiShare Memory Management
;;;

(defun MidiNewCell ()
  "Allocate a basic Cell"
  (#_MidiNewCell))

(defun MidiFreeCell (cell)
  "Delete a basic Cell"
  (#_MidiFreeCell cell))

(defun MidiTotalSpace ()
  "Total amount of Cells"
  (#_MidiTotalSpace))

(defun MidiGrowSpace (n)
  "Total amount of Cells"
  (#_MidiGrowSpace n))

;;;
;;; SMPTE Synchronisation functions
;;;

(defun MidiGetSyncInfo (syncInfo)
  "Fill syncInfo with current synchronisation informations"
  (#_MidiGetSyncInfo syncInfo))

(defun MidiSetSyncMode (mode)
  "set the MidiShare synchroniation mode"
  (#_MidiSetSyncMode mode))

(defun MidiGetExtTime ()
  "give the current external time"
  (#_MidiGetExtTime))

(defun MidiInt2ExtTime (time)
  "convert internal time to external time"
  (#_MidiInt2ExtTime time))

(defun MidiExt2IntTime (time)
  "convert internal time to external time"
  (#_MidiExt2IntTime time))

(defun MidiTime2Smpte (time format smpteLocation)
  "convert time to Smpte location"
  (#_MidiTime2Smpte time format smpteLocation))

(defun MidiSmpte2Time (smpteLocation)
  "convert time to Smpte location"
  (#_MidiSmpte2Time smpteLocation))

;;;
;;; Drivers functions
;;;

(defun MidiCountDrivers ()
  "number of opened drivers"
  (#_MidiCountDrivers))

(defun MidiGetIndDriver (index)
  "Give the reference number of a MidiShare driver from its index, a fixnum"
  (#_MidiGetIndDriver index))

(defun MidiGetDriverInfos (refNum info)
  "Give information about a driver"
  (#_MidiGetDriverInfos refNum info))

;;;
;;; BUG! For some reaon the MidiGetIndSLot decl in Midishare.h:
;;;   SlotRefNum MidiGetIndSlot (short refnum, short index);
;;; gets translated as this in the Midishare.{ffi,lisp} files:
;;;   (ccl::define-external-function (MIDIGETINDSLOT "MidiGetIndSlot")
;;;     (:<s>lot<r>ef<n>um (:signed 16) (:signed 16) )
;;;     :void )
;;; which is wrong. I'm not sure what to do about it yet... --hkt
;;;

(defun MidiGetIndSlot (refNum index)
  "Give the reference number of a driver slot from its order number."
  (error "Fix me.")
  (#_MidiGetIndSlot :bogus-arg refNum index))

(defun MidiGetSlotInfos (slotRefNum info)
  "Give information about a slot"
  (#_MidiGetSlotInfos slotRefNum info))

(defun MidiConnectSlot (port slotRefNum state)
  "Make or remove a connection between a slot and a MidiShare logical port"
  (#_MidiConnectSlot port slotRefNum state))

(defun MidiIsSlotConnected  (port slotRefNum)
  "Test a connection between a slot and a MidiShare logical port"
  (#_MidiIsSlotConnected port slotRefNum))

(defun MidiNewSmpteLocation ()
  (ccl::make-record :<ts>mpte<l>ocation))

(defun MidiFreeSmpteLocation (location)
  location
  (warn "MidiFreeSmpteInfo: Dont know how to freee record.")
  ;(ccl::dispose-record location)
  )

(defun MidiNewSyncInfo ()
  (error "Cannot find record :<ts>inc<i>nfo")
  ;(ccl::make-record :<ts>inc<i>nfo)
  )

(defun MidiFreeSyncInfo (location)
  location
  (warn "MidiFreeSyncInfo: Dont know how to freee record.")
  ;(ccl::dispose-record location)
  )

;;;
;;; To Install and Remove the MidiShare Interface
;;;

(defun install-midishare-interface ()
  (midishare-framework)
  (unless (midishare) (error "MidiShare not installed")))

(defun remove-midishare-interface ()
  (setf *midiShare* nil))

(eval-when (:load-toplevel :execute)

  (push #'install-midishare-interface ccl:*lisp-startup-functions*)
  (push #'remove-midishare-interface ccl:*lisp-cleanup-functions*)
  ;; this is only needed while I test -- it lets me use #_ and #$ even
  ;; if openmcl.dfsl is loaded without compiling. otherwise you can
  ;; delete the line.
  (ccl:use-interface-dir :midishare)
  (install-midishare-interface))

) ; end #+openmcl


;;;-----------------------------------------------------------------------
;;; 			Interface for CMULisp on Linux
;;;-----------------------------------------------------------------------

#+(and linux cmu)

;;;-----------------------------------------------------------------------
;;;
;;; 				Utilities                                         
;;;
;;;-----------------------------------------------------------------------

(progn

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((*warn-if-redefine* nil))
    (load-foreign "/usr/lib/libMidiShare.so")
    (use-package "ALIEN"                    )
    (use-package "C-CALL"                   )
    )
  )

;; define a general pointer type
(def-alien-type ptr  (* t))

;; Test if p is a null pointer
                                                                      
(defun nullptrp (p)
  (if (typep p '(alien (* t))) 
    (zerop (system:sap-int (alien-sap p)))
    (zerop (system:sap-int p))))

;; Returns a numm pointer

(defun nullptr () (system:int-sap 0))


;;;-----------------------------------------------------------------------
;;;-----------------------------------------------------------------------
;;;                                                                                 
;;; 				MidiShare Data Structures                           
;;;                                                                                  
;;;-----------------------------------------------------------------------
;;;-----------------------------------------------------------------------


;; Extension record for typeSysEx events

(def-alien-type nil
  (struct TMidiSEX
	  (link  (*(struct TMidiSEX)) )
	  (data  (array char 12)      )
  )
)


;;; Extension record for typePrivate, typeProcess and typeDProcess events

(def-alien-type nil
  (struct TMidiST
	  (ptr1 ptr )
	  (ptr2 ptr )
	  (ptr3 ptr )
	  (ptr4 ptr )
  )
)
	  

;;;-----------------------------------------------------------------------
;;; Common Record for all MidiShare events                                        
;;;-----------------------------------------------------------------------

;; Possible types for the last field of the structure
                                                      
(def-alien-type nil                                   
  (struct PitchVelDur                                
	  (pitch char  )                              
	  (vel   char  )                             
	  (dur   short )))                         
                                                     
(def-alien-type nil                                   
  (struct datas                                       
	  (data0 char)                               
	  (data1 char)                                
	  (data2 char)                               
	  (data3 char) ))                             
                                                      
(def-alien-type T_info long)                          
(def-alien-type T_linkSE (*(struct TMidiSEX)))        
(def-alien-type T_linkST (*(struct TMidiST) ))       
                                                      
(def-alien-type nil                                   
   (union evData                                      
	  (note (struct pitchVelDur))                
	  (data (struct datas      ))                 
	  (info T_info              )                
	  (linkSE T_linkSE          )               
	  (linkST T_linkST          )))            
                                                     
                                                      
;;; The last field of a TMidiEv is either :  
;;;                                                    
;;;     - a note (with a pitch, a velocite and a duration) 
;;;     - a 4 byte field (4 au total)            
;;;     - a fiels info  (4 bytes)                                   
;;;     - a link to a TMidiSEX                     
;;;     - a link to a TMidiST                      
;;;                                                    

(def-alien-type nil
  (struct TMidiEv
	  (link    (*(struct TMidiEv))  )
	  (date    long                 )
	  (evtype  char                 )
	  (ref     char                 )
	  (port    char                 )
	  (chan    char                 )
	  (data  (union evData)         )
   )
)


;;;-----------------------------------------------------------------------
;;; Record for a MidiShare Sequence                                                 
;;;-----------------------------------------------------------------------

(def-alien-type nil
  (struct TMidiSeq
	  (first   (*(struct TMidiEv)) )
	  (last    (*(struct TMidiEv)) )
	  (undef1  ptr                 )
	  (undef2  ptr                 )
  )
)

;;;-----------------------------------------------------------------------
;;; Record for MidiShare SMPTE synchronisation informations                          
;;;-----------------------------------------------------------------------

(def-alien-type nil
  (struct TSyncInfo
	  (time        long   )
	  (reenter     long   )
	  (syncMode    short  )
	  (syncLocked  char   )
	  (syncPort    char   )
	  (syncStart   long   )
	  (syncStop    long   )
	  (syncOffset  long   )
	  (syncSpeed   long   )
	  (SyncBreaks  long   )
	  (syncFormat  short  )
  )
)

;;;-----------------------------------------------------------------------
;;; Record for MidiShare SMPTE locations                                              
;;;-----------------------------------------------------------------------

(def-alien-type nil
  (struct TSmpteLocation
	  (format   short )
	  (hours    short )
	  (minutes  short )
	  (seconds  short )
	  (frames   short )
	  (fracs    short )
  )
)

;;;-----------------------------------------------------------------------
;;; Record for MidiShare Filters                                                   
;;;-----------------------------------------------------------------------

(def-alien-type nil
  (struct TFilter
	  (port    (array char 32) )
	  (evType  (array char 32) )
	  (channel (array char 2)  )
	  (unused  (array char 2)  )
  )
)

;;;-----------------------------------------------------------------------
;;; pointers on alien data structures                                               
;;;-----------------------------------------------------------------------

(def-alien-type MidiSEXPtr     ( *(struct TMidiSEX) )      ) 
(def-alien-type MidiSTPtr      ( *(struct TMidiST))        ) 
(def-alien-type MidiEvPtr      ( *(struct TMidiEv))        )
(def-alien-type MidiSeqPtr     ( *(struct TMidiSeq))       )
(def-alien-type SyncInfoPtr    ( *(struct TSyncInfo))      )
(def-alien-type SmpteLocPtr    ( *(struct TSmpteLocation)) )
(def-alien-type MidiFilterPtr  ( *(struct TFilter))        )


;;;-----------------------------------------------------------------------
;;;-----------------------------------------------------------------------
;;;                                                                                  
;;; 		Macros for accessing MidiShare Events data structures              
;;;                                                                                 
;;;-----------------------------------------------------------------------
;;;-----------------------------------------------------------------------


;;;-----------------------------------------------------------------------
;;;                      Macros common to every type of event
;;;-----------------------------------------------------------------------

;;;..................................................: link
;;                                                                                                                                          
(defun link (e &optional (d nil d?))
  "read or set the link of an event"
  (if d?
    (midiSetLink e d);  (setf (slot e 'link) d )                 
    (midiGetLink e   );  (slot e 'link           )
  )
)

;;;..................................................: date
;;                                                                                                                                       
(defun date (e &optional d)
  "read or set the date of an event"
  (if d
      (MidiSetDate e d); (setf (slot e 'date) d )
      (MidiGetDate e   ); (slot e 'date           )                
  )
)

;;;..................................................: type
;;                                                                                                                                          
(defun evtype (e &optional v)
  "read or set the type of an event. Be careful in 
 modifying the type of an event"
  (if v
      (MidiSetType e v); (setf (slot e 'evType) v )
      (MidiGetType e   ); (slot e 'evType           )              
  )
)

;;;..................................................: ref
;;                                                                                                                                          
(defun ref (e &optional v)
  "read or set the reference number of an event"
  (if v
      (MidiSetRefNum e v); (setf (slot e 'ref) v)
      (MidiGetRefNum e    ); (slot e 'ref          )                
  )
)

;;;..................................................: port
;;                                                                                                                                             
(defun port (e &optional v)
  "read or set the port number of an event"
  (if v
      (MidiSetPort e v); (setf (slot e 'port) v)
      (MidiGetPort e   ); (slot e 'port          )                 
   )
)

;;;..................................................: chan
;;                                                                                                                                           
(defun chan (e &optional v)
  "read or set the chan number of an event"
  (if v
      (MidiSetChan e v); (setf (slot e 'chan) v)
      (MidiGetChan e   ); (slot e 'chan          )                  
  )
)

;;;..................................................: field
;;                                                                                                                                           
(defun field (e &optional f v)
  "give the number of fields or read or set a particular field of an event"
  (if f
    (if v
      (midisetfield e f v)
      (midigetfield e f))
    (midicountfields e)))

;;;..................................................: fieldsList
;;                                                                                                                   
(defun fieldsList (e &optional (n 4))
  "collect all the fields of an event into a list"
  (let (l)
    (dotimes (i (min n (midicountfields e)))
      (push (midigetfield e i) l))
    (nreverse l)))


;;;-----------------------------------------------------------------------
;;;                       Specific to typeNote events
;;;-----------------------------------------------------------------------

;;;..................................................: pitch
;;                                                                                                                                            
(defun pitch (e &optional v)
  "read or set the pitch of an event"
  (if v
      (midisetfield e 0 v); (setf (slot (slot (slot e 'data) 'note) 'pitch) v)
      (midigetfield e 0   ); (slot (slot (slot e 'data) 'note) 'pitch          )  
  )
)

;;;..................................................: vel
;;                                                                                                                                         
(defun vel (e &optional v)
  "read or set the velocity of an event"
  (if v
      (midisetfield e 1 v); (setf (slot (slot (slot e 'data) 'note) 'vel) v)
      (midigetfield e 1   ); (slot (slot (slot e 'data) 'note) 'vel          )    
  )
)

;;;..................................................: dur
;;                                                                                                                                             
(defun dur (e &optional v)
  "read or set the duration of an event"
  (if v
      (midisetfield e 2 v); (setf (slot (slot (slot e 'data) 'note) 'dur) v)
      (midigetfield e 2   ); (slot (slot (slot e 'data) 'note) 'dur          )    
  )
)


;;;-----------------------------------------------------------------------
;;;                        Specific to other types of events
;;;-----------------------------------------------------------------------

;;;..................................................: linkSE
;;                                                                                               
(defun linkSE (e &optional (d nil d?))
  "read or set the link of an SEXevent "
  (if d?
      (setf (slot (slot e 'data) 'linkSE) d)
      (slot (slot e 'data) 'linkSE          )
  )
)

;;;..................................................: linkST
;;                                                                                                
(defun linkST (e &optional (d nil d?))
 "read or set the link of an STevent "
  (if d?
      (setf (slot (slot e 'data) 'linkST) d)
      (slot (slot e 'data) 'linkST          )
  )
)


;;;..................................................: kpress
;;                                                                                               
(defun kpress (e &optional v)
  (if v
      (midisetfield e 1 v); (setf (slot (slot (slot e 'data) 'note) 'vel) v)
      (midigetfield e 1   ); (slot (slot (slot e 'data) 'note) 'vel          )  
  )
)


;;;..................................................: ctrl
;;                                                                                               
(defun ctrl (e &optional v)
  (if v
    (midisetfield e 0 v)
    (midigetfield e 0)))


;;;..................................................: param
;;                                                                                             
(defun param (e &optional v)
  (if v
    (midisetfield e 0 v)
    (midigetfield e 0)))


;;;..................................................: num
;;                                                                                              
(defun num (e &optional v)
  (if v
    (midisetfield e 0 v)
    (midigetfield e 0)))


;;;..................................................: prefix
;;                                                                                               
(defun prefix (e &optional v)
  (if v
    (midisetfield e 0 v)
    (midigetfield e 0)))


;;;..................................................: tempo
;;                                                                                                
(defun tempo (e &optional v)
  (if v
    (midisetfield e 0 v)
    (midigetfield e 0)))


;;;..................................................: seconds
;;                                                                                               
(defun seconds (e &optional v)
  (if v
    (midisetfield e 0 v)
    (midigetfield e 0)))


;;;..................................................: subframes
;;                                                                                                
(defun subframes (e &optional v)
  (if v
    (midisetfield e 1 v)
    (midigetfield e 1)))


;;;..................................................: val
;;                                                                                               
(defun val (e &optional v)
  (if v
    (midisetfield e 1 v)
    (midigetfield e 1)))


;;;..................................................: pgm
;;                                                                                               
(defun pgm (e &optional v)
  (if v
      (midisetfield e 0 v); (setf (slot (slot (slot e 'data) 'note) 'pitch) v)
      (midigetfield e 0   ); (slot (slot (slot e 'data) 'note) 'pitch          )  
  )
)


;;;..................................................: bend
;;                                                                                                
(defun bend (e &optional v)
  "read or set the bend value of an event"
  (if v
      (multiple-value-bind (ms7b ls7b) (floor (+ v 8192) 128)
        (midisetfield e 0 ls7b); (setf (slot (slot (slot e 'data) 'note) 'pitch) ls7b)
        (midisetfield e 1 ms7b); (setf (slot (slot (slot e 'data) 'note) 'vel  ) ms7b)
        )
    (- (+ (midigetfield e 0) (* 128 (midigetfield e 1))) 8192)                                                
    ;; (- (+ (slot (slot (slot e 'data) 'note) 'pitch) (* 128 (slot (slot (slot e 'data) 'note) 'vel))) 8192)
  )
)


;;;..................................................: clk
;;                                                                                                
(defun clk (e &optional v)
  (if v
      (multiple-value-bind (ms7b ls7b) (floor (round (/ v 6)) 128)
         (midisetfield e 0 ls7b); (setf (slot (slot (slot e 'data) 'note) 'pitch) ls7b)
         (midisetfield e 1 ms7b); (setf (slot (slot (slot e 'data) 'note) 'vel  ) ms7b)
       )
      (* 6 (+ (midigetfield e 0) (*128 (midigetfield e 1))))                                              
      ;; (* 6 (+ (slot (slot (slot e 'data) 'note) 'pitch) (* 128 (slot (slot (slot e 'data) 'note) 'vel))))
  )
)


;;;..................................................: song
;;                                                                                                
(defun song (e &optional v)
  (if v
      (midisetfield e 0 v); (setf (slot (slot (slot e 'data) 'note) 'pitch) v)
      (midigetfield e 0   ); (slot (slot (slot e 'data) 'note) 'pitch          )   
  )
)


;;;..................................................: fields
;;                                                                                               
(defun fields (e &optional v)
  (if v
    (let ((e e))
      (mapc #'(lambda (f) (midiaddfield e f)) v))
    (let (l (e e))
      (dotimes (i (midicountfields e))
        (push (midigetfield e i) l))
      (nreverse l)) ))


;;;..................................................: text

(defun text (e &optional s)
  (if s
    (fields e (map 'list #'char-code s))
    (map 'string #'character (fields e)) ))


;;;..................................................: fmsg

(defun fmsg (e &optional v)
  (if v
    (midisetfield e 0 v); (setf (slot (slot (slot e 'data) 'note) 'pitch) v)
    (midigetfield e 0   ); (slot (slot (slot e 'data) 'note) 'pitch          )   
  )
)

;;;..................................................: fcount

(defun fcount (e &optional v)
  (if v
    (midisetfield e 1 v); (setf (slot (slot (slot e 'data) 'note) 'vel) v)
    (midigetfield e 1   ); (slot (slot (slot e 'data) 'note) 'vel)                
  )
)

;;;..................................................: tsnum

(defun tsnum (e &optional v)
  (if v
    (midisetfield e 0 v)
    (midigetfield e 0)))


;;;..................................................: tsdenom

(defun tsdenom (e &optional v)
  (if v
    (midisetfield e 1 v)
    (midigetfield e 1)))


;;;..................................................: tsclick

(defun tsclick (e &optional v)
  (if v
    (midisetfield e 2 v)
    (midigetfield e 2)))


;;;..................................................: tsquarter

(defun tsquarter (e &optional v)
  (if v
    (midisetfield e 3 v)
    (midigetfield e 3)))

;;;..................................................: alteration

(defun alteration (e &optional v)
  (if v
    (midisetfield e 0 v)
    (midigetfield e 0)))

;;;..................................................: minor-scale

(defun minor-scale (e &optional v)
  (if v
    (midisetfield e 1 (if v 1 0))
    (= 1 (midigetfield e 1))))

;;;..................................................: info

(defun info (e &optional d)
  "read or set the info of an event"
  (if d
    (setf (slot (slot e 'data) 'info) d)
    (slot (slot e 'data) 'info          )
  )
)

;;;-----------------------------------------------------------------------
;;;-----------------------------------------------------------------------
;;;
;;; 	Accessing MidiShare Sequences data structures
;;;
;;;-----------------------------------------------------------------------
;;;-----------------------------------------------------------------------

;;;..................................................: firstEv

(defun firstEv (s &optional (e nil e?))
  "read or set the first event of a sequence"
  (if e?
      (midiSetFirstEv s e); (setf (slot s 'first) e)
      (midiGetFirstEv s   ); (slot s 'first          )          
  )
)

;;;..................................................: lastEv

(defun lastEv (s &optional (e nil e?))
  "read or set the last event of a sequence"
  (if e?
      (midiSetLastEv s e); (setf (slot s 'last) e)
      (midiGetLastEv s   ); (slot s 'last          )           
  )
)


;;;-----------------------------------------------------------------------
;;;-----------------------------------------------------------------------
;;;                                                                                    
;;; 				MidiShare Entry Points                                
;;;                                                                                   
;;;-----------------------------------------------------------------------
;;;-----------------------------------------------------------------------

;; Interface description for a MidiShare PROCEDURE 
;; with short parameter
;; (def-alien-routine "My_Procedure" void
;;       (parameter1 short))
;;
;; Interface description for a MidiShare FUNCTION
;; with 2 short parameters and a short result
;; (def-alien-routine "My_Function" short
;;       (parameter1 short) (parameter2 short))


;; Entry point of MidiShare (setup at boot time by the "MidiShare" init)

(defvar *midiShare*)

;;;-----------------------------------------------------------------------
;;;			To Know about MidiShare and Active Sessions
;;;-----------------------------------------------------------------------

;;;..................................................: MidiShare 

(def-alien-routine "MidiShare" char 
  "returns true if MidiShare is installed")

;;;..................................................: MidiGetVersion

(def-alien-routine "MidiGetVersion" short
  "Give MidiShare version as a fixnum. For example 131 as result means : version 1.31")

;;;..................................................: MidiCountAppls

(def-alien-routine "MidiCountAppls" short
  "Give the number of MidiShare applications currently opened")

;;;..................................................: MidiGetIndAppl

(def-alien-routine "MidiGetIndAppl" short
  (index short)
  "Give the reference number of a MidiShare application from its index a fixnum
 between 1 and (MidiCountAppls)")

;;;..................................................: MidiGetNamedAppl

(def-alien-routine "MidiGetNamedAppl" short
  (name c-string)
  "Give the reference number of a MidiShare application from its name")


;;;-----------------------------------------------------------------------
;;;			To Open and Close a MidiShare session
;;;-----------------------------------------------------------------------

;;;..................................................: MidiOpen

(def-alien-routine "MidiOpen" short
  (name c-string)
  "Open a new MidiShare application with name name. Give a unique reference number.")

;;;..................................................: MidiClose

(def-alien-routine "MidiClose" void
  (refNum short)
  "Close an opened MidiShare application from its reference number")



;;;-----------------------------------------------------------------------
;;;			To Configure a MidiShare session
;;;-----------------------------------------------------------------------

;;;..................................................: MidiGetName

(def-alien-routine "MidiGetName" c-string
  (refNum short)
  "Give the name of a MidiShare application from its reference number")

;;;..................................................: MidiSetName

(def-alien-routine "MidiSetName" void
  (refNum short) (name c-string)                                       
  "Change the name of a MidiShare application")

;;;..................................................: MidiGetInfo

(def-alien-routine "MidiGetInfo" (* t)
  (refNum short)
  "Give the 32-bits user defined content of the info field of a MidiShare application. 
 Analogous to window's refcon.")

;;;..................................................: MidiSetInfo

(def-alien-routine "MidiSetInfo" void
  (refNum short) (p (* t))
  "Set the 32-bits user defined content of the info field of a MidiShare application. 
 Analogous to window's refcon.")


;;;..................................................: MidiNewFilter

(def-alien-routine "MidiNewFilter" MidiFilterPtr
  "Returns a new filter")

;;;..................................................: MidiFreeFilter

(def-alien-routine "MidiFreeFilter" void
  (f MidiFilterPtr)
  "Delete a filter")

;;;..................................................: MidiAcceptChan

(def-alien-routine "MidiAcceptChan" void
  (f MidiFilterPtr) (c short) (s short)
  "Change the chan state of a filter")

;;;..................................................: MidiAcceptType

(def-alien-routine "MidiAcceptType" void
  (f MidiFilterPtr) (c short) (s short)
  "Change the type state of a filter")

;;;..................................................: MidiAcceptPort

(def-alien-routine "MidiAcceptPort" void
  (f MidiFilterPtr) (c short) (s short)
  "Change the port state of a filter")

;;;..................................................: MidiIsAcceptedChan

(def-alien-routine "MidiIsAcceptedChan" short
  (f MidiFilterPtr) (c short)
  "Returns the chan state of a filter")

;;;..................................................: MidiIsAcceptedType

(def-alien-routine "MidiIsAcceptedType" short
  (f MidiFilterPtr) (c short)
  "Returns the type state of a filter")

;;;..................................................: MidiIsAcceptesPort

(def-alien-routine "MidiIsAcceptedPort" short
  (f MidiFilterPtr) (c short)
  "Returns the port state of a filter")

;;;..................................................: MidiGetFilter

(def-alien-routine "MidiGetFilter" MidiFilterPtr
  (refNum short)
  "Give a pointer to the input filter record of a MidiShare application. 
 Give NIL if no filter is installed")

;;;..................................................: MidiSetFilter

(def-alien-routine "MidiSetFilter" void
  (refNum short) ( p MidiFilterPtr)
  "Install an input filter. The argument p is a pointer to a filter record.")

;;;..................................................: MidiGetRcvAlarm

(def-alien-routine "MidiGetRcvAlarm" ptr
  (refNum short)
  "Get the adress of the receive alarm")

;;;..................................................: MidiSetRcvAlarm

(def-alien-routine "MidiSetRcvAlarm" void
  (refNum short) (alarm ptr )
  "Install a receive alarm")

;;;..................................................: MidiGetApplAlarm

(def-alien-routine "MidiGetApplAlarm" ptr
  (refNum short)
  "Get the adress of the context alarm")

;;;..................................................: MidiSetApplAlarm

(def-alien-routine "MidiSetApplAlarm" void
  (refNum short) (alarm ptr)
  "Install a context alarm")



;;;-----------------------------------------------------------------------
;;;			To Manage MidiShare IAC and Midi Ports
;;;-----------------------------------------------------------------------

;;;..................................................: MidiConnect

(def-alien-routine "MidiConnect" void
  (src short) (dst short) (state char)
  "Connect or disconnect two MidiShare applications")

;;;..................................................: MidiIsConnected

(def-alien-routine "MidiIsConnected" char
  (src short) (dst short)
  "Test if two MidiShare applications are connected")

;;;..................................................: MidiGetPortState

(def-alien-routine "MidiGetPortState" char
  (port short)
  "Give the state : open or closed of a MidiPort")

;;;..................................................: MidiSetPortState

(def-alien-routine "MidiSetPortState" void
  (port short) (state char)
  "Open or close a MidiPort")


;;;-----------------------------------------------------------------------
;;;			To Manage MidiShare events
;;;-----------------------------------------------------------------------

;;;..................................................: MidiFreeSpace

(def-alien-routine "MidiFreeSpace" long
  "Amount of free MidiShare cells")

;;;..................................................: MidiNewEv
;;                                                                                                                                        
(def-alien-routine "MidiNewEv" MidiEvPtr
  (typeNum short)
  "Allocate a new MidiEvent")

;;;..................................................: MidiCopyEv

(def-alien-routine "MidiCopyEv" MidiEvPtr
  (ev MidiEvPtr)
  "Duplicate a MidiEvent")

;;;..................................................: MidiFreeEv

(def-alien-routine "MidiFreeEv" void
  (ev MidiEvPtr)
  "Free a MidiEvent")

;;;..................................................: MidiSetField

(def-alien-routine "MidiSetField" void
  (ev MidiEvPtr) (field long) (val long)
  "Set a field of a MidiEvent")

;;;..................................................: MidiGetField

(def-alien-routine "MidiGetField" long
  (ev MidiEvPtr) (field long)
  "Get a field of a MidiEvent")

;;;..................................................: MidiAddField

(def-alien-routine "MidiAddField" void
  (ev MidiEvPtr) (val long)
  "Append a field to a MidiEvent (only for sysex and stream)")

;;;..................................................: MidiCountFields

(def-alien-routine "MidiCountFields" long
  (ev MidiEvPtr)
  "The number of fields of a MidiEvent")

;;;..................................................: MidiGetDate

(def-alien-routine "MidiGetDate" long
  (ev MidiEvPtr))

;;;..................................................: MidiSetDate

(def-alien-routine "MidiSetDate" void
  (ev MidiEvPtr) (date long))
  
;;;..................................................: MidiGetLink

(def-alien-routine "MidiGetLink" MidiEvPtr
  (ev MidiEvPtr))

;;;..................................................: MidiSetLink

(def-alien-routine "MidiSetLink" void
  (ev MidiEvPtr) (linkEv MidiEvPtr))

;;;..................................................: MidiGetRefnum

(def-alien-routine "MidiGetRefNum" short
  (ev MidiEvPtr))

;;;..................................................: MidiSetRefnum

(def-alien-routine "MidiSetRefNum" void
  (ev MidiEvPtr) (date short))

;;;..................................................: MidiGetType

(def-alien-routine "MidiGetType" short
  (ev MidiEvPtr))

;;;..................................................: MidiSetType

(def-alien-routine "MidiSetType" void
  (ev MidiEvPtr) (date short))

;;;..................................................: MidiGetChan

(def-alien-routine "MidiGetChan" short
  (ev MidiEvPtr))

;;;..................................................: MidiSetChan

(def-alien-routine "MidiSetChan" void
  (ev MidiEvPtr) (date short))


;;;..................................................: MidiGetPort

(def-alien-routine "MidiGetPort" short
  (ev MidiEvPtr))

;;;..................................................: MidiSetPort

(def-alien-routine "MidiSetPort" void
  (ev MidiEvPtr) (date short))

;;;-----------------------------------------------------------------------
;;;			To Manage MidiShare Sequences
;;;-----------------------------------------------------------------------


;;;..................................................: MidiGetFirstEv

(def-alien-routine "MidiGetFirstEv" MidiEvPtr
  (seq MidiSeqPtr))

;;;..................................................: MidiSetFirstEv

(def-alien-routine "MidiSetFirstEv" void
  (seq MidiSeqPtr) (ev MidiEvPtr))


;;;..................................................: MidiGetLastEv

(def-alien-routine "MidiGetLastEv" MidiEvPtr
  (seq MidiSeqPtr))

;;;..................................................: MidiSetLastEv

(def-alien-routine "MidiSetLastEv" void
  (seq MidiSeqPtr) (ev MidiEvPtr))


;;;..................................................: MidiNewSeq

(def-alien-routine "MidiNewSeq" MidiSeqPtr       
  "Allocate an empty sequence")

;;;..................................................: MidiAddSeq

(def-alien-routine "MidiAddSeq" void
  (seq MidiSeqPtr ) (ev MidiEvPtr)
  "Add an event to a sequence")

;;;..................................................: MidiFreeSeq

(def-alien-routine "MidiFreeSeq" void
  (seq MidiSeqPtr )
  "Free a sequence and its content")
;  (ff-call *midiShare* :ptr seq :d0 #x1F))

;;;..................................................: MidiClearSeq

(def-alien-routine "MidiClearSeq" void
  (seq MidiSeqPtr )
  "Free only the content of a sequence. The sequence become empty")
;  (ff-call *midiShare* :ptr seq :d0 #x20))

;;;..................................................: MidiApplySeq

(def-alien-routine "MidiApplySeq" void
  (seq MidiSeqPtr ) (proc ptr)
  "Call a function for every events of a sequence")


;;;-----------------------------------------------------------------------
;;;				   MidiShare Time
;;;-----------------------------------------------------------------------

;;;..................................................: MidiGetTime

(def-alien-routine "MidiGetTime" long
  "give the current time")

;;;-----------------------------------------------------------------------
;;;				To Send MidiShare events
;;;-----------------------------------------------------------------------

;;;..................................................: MidiSendIm

(def-alien-routine "MidiSendIm" void
  (refNum short) (ev MidiEvPtr)
  "send an event now")

;;;..................................................: MidiSend

(def-alien-routine "MidiSend" void
  (refNum short) (ev MidiEvPtr)
  "send an event using its own date")

;;;..................................................: MidiSendAt

(def-alien-routine "MidiSendAt" void
  (refNum short) (ev MidiEvPtr) (date long)
  "send an event at date <date>")

;;;-----------------------------------------------------------------------
;;;                            To Receive MidiShare Events
;;;-----------------------------------------------------------------------

;;;..................................................: MidiCountEvs

(def-alien-routine "MidiCountEvs" long
  (refNum short)
  "Give the number of events waiting in the reception fifo")

;;;..................................................: MidiGetEv

(def-alien-routine "MidiGetEv" MidiEvPtr
  (refNum short)
  "Read an event from the reception fifo")

;;;..................................................: MidiAvailEv

(def-alien-routine "MidiAvailEv" MidiEvPtr
  (refNum short)
  "Get a pointer to the first event in the reception fifo without removing it")

;;;..................................................: MidiFlushEvs

(def-alien-routine "MidiFlushEvs" void
  (refNum short)
  "Delete all the events waiting in the reception fifo")

;;;-----------------------------------------------------------------------
;;;                             To access shared data
;;;-----------------------------------------------------------------------

;;;..................................................: MidiReadSync

(def-alien-routine "MidiReadSync" (* t)
  (adrMem (* t) )
  "Read and clear a memory address (not-interruptible)")

;;;..................................................: MidiWriteSync

(def-alien-routine "MidiWriteSync" (* t)
  (adrMem (* t) ) (val (* t) )
  "write if nil into a memory address (not-interruptible)")

;;;-----------------------------------------------------------------------
;;;                              Realtime Tasks
;;;-----------------------------------------------------------------------

;;;..................................................: MidiCall

(def-alien-routine "MidiCall" void
  (proc ptr) (date long) (refNum short) (arg1 long) (arg2 long) (arg3 long)
  "Call the routine <proc> at date <date> with arguments <arg1> <arg2> <arg3>")

;;;..................................................: MidiTask

(def-alien-routine "MidiTask" ptr
  (proc ptr) (date long) (refNum short) (arg1 long) (arg2 long) (arg3 long)
  "Call the routine <proc> at date <date> with arguments <arg1> <arg2> <arg3>. Return a pointer to the corresponding typeProcess event")

;;;..................................................: MidiDTask

(def-alien-routine "MidiDTask" ptr
  (proc ptr) (date long) (refNum short) (arg1 long) (arg2 long) (arg3 long)
  "Call the routine <proc> at date <date> with arguments <arg1> <arg2> <arg3>. Return a pointer to the corresponding typeDProcess event")


;;;......................: MidiForgetTaskH * a VOIR POUR LES MODIFS *
;;(defun MidiForgetTaskHdl (thdl)
;;"Forget a previously scheduled typeProcess or typeDProcess event created by MidiTask or MidiDTask"
;;  (ff-call *midiShare* :ptr thdl :d0 #x2F))
;;;...............................................: MidiForgetTask
;;(defun MidiForgetTask (ev)
;;"Forget a previously scheduled typeProcess or typeDProcess event created by MidiTask or MidiDTask"
;;  (without-interrupts
;;    (%stack-block ((taskptr 4))
;;      (%setf-macptr taskptr ev) (midiforgetTaskHdl taskptr))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;..................................................: MidiCountDTasks

(def-alien-routine "MidiCountDTasks" long
  (refNum short)
  "Give the number of typeDProcess events waiting")

;;;..................................................: MidiFlushDTasks

(def-alien-routine "MidiFlushDTasks" void
  (refNum short)
  "Remove all the typeDProcess events waiting")

;;;..................................................: MidiExec1DTask

(def-alien-routine "MidiExec1DTask" void
  (refNum short)
  "Call the next typeDProcess waiting")


;;;-----------------------------------------------------------------------
;;;                        Low Level MidiShare Memory Management
;;;-----------------------------------------------------------------------

;;;..................................................: MidiNewCell

(def-alien-routine "MidiNewCell" MidiEvPtr
  "Allocate a basic Cell")

;;;..................................................: MidiFreeCell

(def-alien-routine "MidiFreeCell" void
  (cell MidiEvPtr)
  "Delete a basic Cell")

;;;..................................................: MidiTotalSpace

(def-alien-routine "MidiTotalSpace" long
  "Total amount of Cells")

;;;..................................................: MidiGrowSpace

(def-alien-routine "MidiGrowSpace" long
  (n long)
  "Total amount of Cells")


;;;-----------------------------------------------------------------------
;;;                        SMPTE Synchronisation functions
;;;-----------------------------------------------------------------------

;;;..................................................: MidiGetSyncInfo

(def-alien-routine "MidiGetSyncInfo" void
  (syncInfo SyncInfoPtr   )
  "Fill syncInfo with current synchronisation informations")

;;;..................................................: MidiSetSyncMode

(def-alien-routine "MidiSetSyncMode" void
  (mode short)
  "set the MidiShare synchroniation mode")

;;;..................................................: MidiGetExtTime

(def-alien-routine "MidiGetExtTime" long
  "give the current external time")

;;;..................................................: MidiInt2ExtTime

(def-alien-routine "MidiInt2ExtTime" long
  (time long)
  "convert internal time to external time")

;;;..................................................: MidiExt2IntTime

(def-alien-routine "MidiExt2IntTime" long
  (time long)
  "convert internal time to external time")

;;;..................................................: MidiTime2Smpte

(def-alien-routine "MidiTime2Smpte" void
  (time long) (format short) (smpteLocation SmpteLocPtr )
  "convert time to Smpte location")

;;;..................................................: MidiSmpte2Time

(def-alien-routine "MidiSmpte2Time" long
  (smpteLocation SmpteLocPtr )
  "convert time to Smpte location")

;;;..........................................: install-midishare-interface

(defun install-midishare-interface ()
  (unless (midishare) (error "MidiShare not installed")))

;;;..........................................: remove-midishare-interface

(defun remove-midishare-interface ())


) ;; End of CMULisp interface


;;;-----------------------------------------------------------------------
;;;	 			**Evaluate this**
;;;-----------------------------------------------------------------------

(eval-when (:load-toplevel :execute)
  (add-startup-action #'install-midishare-interface)
  (add-quit-action #'remove-midishare-interface)
  (install-midishare-interface))
 
(eval-when (:load-toplevel :execute)
  (pushnew ':midishare *features*))


