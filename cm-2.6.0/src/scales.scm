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
;;; $Revision: 1.18 $
;;; $Date: 2005/03/15 14:15:23 $

;;;
;;; Main scale functions reference *scale* for a default scale object.
;;; Initalized to the standard chromatic scale at the end of the file.
;;;

(define *chromatic-scale* #f) ; set at end of file

(define *scale* #f)           ; set at end of file

(define (rest? f)
  (or (eq? f 'r)
      (eq? f -1)
      (equal? f 0.0)))

;;;
;;; in GOOPS a local slot redeclaration completely overrides the 
;;; inherited slot definition
;;;

(define-class <scale> (<container>)
  (octave :accessor scale-octave)
  (lowest :accessor scale-lowest :init-value #f
	  :init-keyword :lowest)
  (divisions :accessor scale-divisions)
  (into :accessor scale-into)
  (keynum-offset :init-value 0 :accessor scale-keynum-offset
		 :init-keyword :keynum-offset)
  :name 'scale)

(define-class <tuning> (<scale>)
  (steps :init-value '() :accessor scale-steps
	 :init-keyword :cents :init-keyword :ratios)
  (table :accessor scale-table :init-value 10 
	 :init-keyword :octaves)
  (defoct :init-value 4 :init-keyword :default-octave
	  :accessor scale-default-octave)
  (octave-offset :init-value 0 :accessor scale-octave-offset )
  :name 'tuning)

;;;
;;; Modes are transposable subsets of an owning scale. steps can be
;;; expressed as notes, 1/2 steps or encoded intervals.
;;;        

(define-class <mode> (<scale>)
  (steps :init-value '() :accessor scale-steps
	 :init-keyword :steps :init-keyword :degrees
         :init-keyword :notes)
  (lowest :accessor scale-lowest :init-value #f :init-keyword :tonic)
  (tuning :init-thunk (lambda () *chromatic-scale*)
          :init-keyword :scale
          :accessor mode-tuning )
  :name 'mode)

;;;
;;; tuning methods
;;;

(define-list-struct sd	        ; scale degree
  note			        ; name|acc|oct
  keynum			; integer > 0
  octave			; integer
  class				; integer pitch class 0 to 11
  name				; note name without octave
  accidental)			; #f or int -2:2 (= ff f n s ss)

(define-method (initialize (obj <tuning>) args)
  (next-method) ; was an after method
  (let ((extern #f)
        (steps? #t)  ; if t spec is increments between degrees
        (cents? #t)  ; if t spec is in cents else ratios
        (equal? #f)  ; if t equal divisions of octave
        (nums #f)
	(syms #f)
	(data #f)
	(len #f)
	(octave #f))
    
    (dopairs (a v args)
      (case a
        ((:cents ) 
         (set! cents? #t)
         (set! extern v))
        ((:ratios )
         (set! cents? #f)
         (set! extern v))))
    (unless extern
      (err "Tuning needs :ratios or :cents initialization."))
    
    (when (null? extern)
      (set! extern (list-prop args ':ratios))
      (if (not extern)
	(err "Tuning needs :ratios or :cents initialization.")
	(set! cents? #f)))
    
    (cond ((pair? (car extern))
           (set! nums
                 (loop for e in extern 
                       collect
                       (if (and (pair? e) (number? (car e)))
                         (car e)
                         (if (number? e) e
                             (err "Bad degree: ~s in ~s."
				  e extern)))))
           ;; nums can be steps (increments) or degrees.
           ;; if the latter, nums will be in strictly monotonically
           ;; increasing order.
           (set! steps? (not (apply (function <) nums)))
           (when (and (not steps?)
                      (pair? (end extern))
                      (scale-table obj))
             (err "No ending octave size (cent or ratio): ~s "
		  extern))
           (set! syms (loop for e in extern 
                            while (pair? e)
                            collect 
                            (if (null? (cdr e))
			      (error "Degree spec missing notes: ~s." e)
			      (cdr e)))))
          ((number? (car extern))
           (set! nums extern)
           ;; nums can be steps (increments) or degrees.
           ;; if the latter, nums will be in strictly monotonically
           ;; increasing order.
           (set! steps? (not (apply (function <) nums)))
           (set! syms #f))
          (else
           (err "Bad degree spec ~s." extern)))
    
    ;; see if nums has equal divisions
    (set! equal?
          (if steps? 
            (apply (function =) nums)
            (let ((one (if cents?
                         (- (second nums) (first nums) )
                         (/ (second nums) (first nums)))))
              (loop for tail on nums
                    while (not (null? (cdr tail)))
                    always
		    (let ((a (first tail))
			  (b (second tail)))
		      (= one (if cents? (- b a) (/ b a))))))))
    (if equal?
      (if steps?
        (begin
         ;; nums is equal steps in cent or ratio format
         (set! data (if cents?
                      (cents->scaler (first nums))
                      (first nums)))
         (set! octave (if cents?
                        (cents->scaler (apply (function +) nums))
                        (apply (function +) nums)))
         (set! len (length nums)))
        (begin
         ;; nums is equal degrees in cent or ratio format
         (set! data (if cents? 
                      (cents->scaler (second nums))
                      (second nums)))
         (set! octave (if cents? 
                        (cents->scaler (end nums))
                        (end nums)))
         (set! len (1- (length nums)))))
      (begin
       ;; nums is unequal steps between degrees in
       ;; either cent or ratio format
       (if steps?
         (set! data
               (do ((tail nums (cdr tail))
                    (l (list (if cents? 0 1)))
                    (k #f))
                   ((null? tail)
                    l)
                 (if (not k)
                   (set! k (car tail))
                   (set! k (if cents?
                             (+ k (car tail))
                             (* k (car tail)))))
                 (push k l)))
         (set! data (reverse nums)))
       ;; data is in reverse order so octave size is 
       ;; first number
       (set! octave (if cents? (cents->scaler (pop data))
                        (pop data)))
       (set! data (if cents? (map (function cents->scaler)
                                  (reverse data))
                      (reverse data)))
       (set! len (length data))
       (set! (scale-into obj)
             (loop for s in data
                   collect (logn s octave)))))
    ;; equal division tunings data is a single number
    ;; otherwise its a list of scalers defining 1 octave.
    (set! (scale-steps obj) data)
    (set! (scale-divisions obj) len)
    (if (not syms)
      (set! (scale-table obj) #f)
      ;; if symbols were specified add hashtable. the
      ;; :octaves init sets boundaries for note name
      ;; generation. 
      (let ((octaves (scale-table obj))
	    (table (make-hash-table 103))
	    (degrees
	     (loop for deg in syms
		   for i from 0
		   collect
		   (loop for d in deg
			 unless (pair? d) do (set! d (list d))
			 collect
			 (let ((n (pop d)))
			   (make-sd :note n
				    :keynum i
				    ;; +-octave twiddling 
				    ;; for equivalents like CF and BS
				    :octave (list-prop d :octave)
				    :class i 
				    :name n
				    :accidental
				    (list-prop d :accidental)))))))
	(set! (scale-table obj) table)
	(fill-tuning-table obj degrees octaves)))
    (set! (scale-octave obj) octave)
    (if (scale-lowest obj)
      (unless (number? (scale-lowest obj))
        (err "lowest not hertz value: ~s."
             (scale-lowest obj)))
      (set! (scale-lowest obj)
            (* 6.875 (expt 2 3/12)) ;8.175798915643707
            ))))

(define (fill-tuning-table tuning degrees octaves)
  ;; add rest entries and if the tuning has octaves then also
  ;; define "note class entries" using the specified scale degrees.
  ;; note class entries have no octave attached to them and they
  ;; are not hashed by their keynums.

  ;; this was :before method
  (let ((table (scale-table tuning))
        (rst (make-sd :note 'r :keynum -1)))
    (hash-set! table 'r (list rst))
    (hash-set! table -1 (list rst))
    (when octaves
      (dolist (deg degrees)
	(dolist (d deg)
	  ;; note => (<sd> . others )
	  ;; sd for note at the front
	  (hash-set! table (sd-note d)
		     (cons d (remove d deg)))))))
  ;;; this was main method
  (let ((table (scale-table tuning))
	(divisions (scale-divisions tuning))
        (begoct #f)
	(endoct #f))
    (if (pair? octaves)
      (begin (set! begoct (first octaves))
	     (set! endoct (second octaves)))
      (if octaves 
        (begin (set! begoct 0)
	       (set! endoct (1- octaves)))
	(begin (set! begoct 0 )
	       (set! endoct 0))))
    (set! (scale-octave-offset tuning) begoct)
    ;; iterate octaves
    (do ((num begoct (+ num 1))
	 (oct 0 (+ oct 1)))
	((> num endoct) #f)
      ;; iterate degrees in each octave
      (do ((lst degrees (cdr lst))
	   (i 0 (+ i 1))
	   (deg #f))
	  ((null? lst) #f)
	(set! deg (car lst))
	;; deg is a list of enharmonic equivalent sd's.
	(let* ((k (+ (* oct divisions) i))
	       (l
		(loop for d in deg 
		      for n = (sd-note d)
		      for o = (+ num (or (sd-octave d) 0))
		      collect
		      (make-sd :note
			       ;; only cons a symbol if more than 
			       ;; one octave.
			       (if octaves
				 (string->symbol (format #f "~A~A" n o))
				 (sd-note d))
			       :keynum k 
			       :octave oct
			       :class (sd-class d)
			       :name n
			       :accidental (sd-accidental d)))))
	  ;; hash keys are note names and keynums
	  ;; hash value is list of enharmonnc degrees.
	  ;; the first degree in this list is the
	  ;; "real" entry for the note name.
	  (hash-set! table k l)
	  (dolist (x l) 
	    (hash-set! table (sd-note x) (cons x (remove x l))))))))
  ;; map over raw scale degree and ensure that these
  ;; have null octave fields if octaves were specified.
  ;; this was :after method
  (when octaves
    (dolist (degs degrees)
      (dolist (d degs)
	(sd-octave-set! d #f)))))

(define (scale-ref table ref)
  ;; return hash value else if symbol try symbol in CM package/module
  (or (hash-ref table ref)
      (if (symbol? ref)
        (hash-ref table (string->symbol (symbol->string ref)))
        #f)))

(define (equal-tuning? tuning)
  (let ((steps (scale-steps tuning)))
    (if (pair? steps) #f steps)))

;;;
;;; Mode methods
;;;

(define-method (mode-tuning (obj <scale>))
  #f)

(define-method (scale-default-octave (obj <mode>))
  (scale-default-octave (mode-tuning obj)))

(define-method (scale-table (obj <mode>))
  (scale-table (mode-tuning obj)))

(define (octave-equivalent note . args)
  (with-args (args &optional (octave 0)
		   (scale *chromatic-scale*))
    ;; return same note in a differnent octave.
    (let* ((table (scale-table scale))
	   (entries (scale-ref table note))
	   (entry (and entries (first entries))))
      (if entry
	(loop for e in (scale-ref table
				 (+ (sd-class entry)
				    (* octave (scale-divisions scale))))
	      when (eq? (sd-name e) (sd-name entry))
	      return e)
	(err "~s is not a note in the standard chromatic scale."
             note)))))

(define (ascending-mode-order notes tuning)
  ;; insure ascending mode order in tuning
  ;; first place all notes in middle octave
  (set! notes (if (symbol? (car notes))
                (mapcar (lambda (n) 
                          (or (note-name n tuning)
                              (err "Note '~s' not defined in ~s."
                                   n tuning)))
                        notes)
                (mapcar (lambda (k) (pitch-class k tuning)) notes)))
  (let* ((old (car notes))
         (pc? (number? old))
         (oct 0)
         (oth #f)
         (res '()))
    (dolist (n notes) ;(rest notes)
      (if (scale< n old tuning)
        (set! oct (+ oct 1)))
      (set! oth (octave-equivalent n oct tuning))
      (push (if pc? (cadr oth) (car oth)) res)
      (set! old n))
    (reverse! res)))

(define-method (initialize (obj <mode>) args)
  (next-method)
  (let* ((spec #f)
         (owner (mode-tuning obj))
         (low (scale-lowest obj))
	 (steps #f)
         (type #f)
	 (len #f)
	 (octave #f))
    (dopairs (a v args)
      (case a
        ((:steps ) 
         (if (symbol? (car v))
           (begin
            (warn "Bad :step value ~s, use :degrees to pass notes to a mode."
                  v)
            (set! spec v)
            (set! type ':degrees))
           (begin
            (set! spec v)
            (set! type a))))
        ((:degrees :notes)
         (when spec
           (err "Found duplicate keywords ~s and ~s."
                type a))
         (set! spec v)
         (set! type a))
        (else #f)))

    (if (member type '(:degrees :notes))
      ;; spec is notes or keynums defining one octave
      ;; convert to intervals up from lowest (collected in reverse order)
      ;; and insure entries are in ascending order
      (begin
       (set! spec (ascending-mode-order spec owner))
       (unless low
         (set! low (if (symbol? (car spec))
                     (sd-name (octave-equivalent (first spec) 0 owner))
                     (sd-keynum  (octave-equivalent (first spec) 0 owner)))))
       ;; if :note then we store typed intervals
       (if (eq? type ':notes)
         (begin 
          (unless (symbol? (car spec))
            (err "mode :notes spec not note names: ~s." spec))
          (unless (eq? owner *chromatic-scale*)
            (err "mode :notes but not chromatic scale: ~s."
                 owner))
          (set! steps
               (do ((l (list (interval 0 0)))
                    (x (cdr spec) (cdr x)))
                   ((null? x) l)
                 (push (interval (car spec) (car x)) l))))
         (begin
          ;; convert note names to keynums if not chromatic scale
          (when (symbol? (car spec))
            (set! spec (keynum spec :in owner)))
          (set! steps
                (do ((x spec (cdr x))
                     (l '())
                     (n (car spec)))
                    ((null? x) l)
                  (push (- (car x) n) l))))))
      (begin
       ;; spec is interval distances between steps. the intervals
       ;; may be typed.
       (unless low (set! low 0))
       (when (pair? (car spec))
         (set! spec (map (function interval) spec)))
       (if (%interval-encoded? (car spec)) ; typed interval
         (set! steps
               (do ((l '())
                    (s (encode-interval 'p 1))
                    (k spec (cdr k)))
                   ((null? k) (cons s l))
                 (push s l)
                 (set! s (transpose s (car k)))))
         (set! steps
               (do ((l '())
                    (s 0)
                    (k spec (cdr k)))
                   ((null? k) (cons s l))
                 (push s l)
                 (incf s (car k)))))))
    ;; first entry in steps is size of octave in tuning
    (set! octave (pop steps))
    ;; reorder steps low->high
    (set! steps (reverse steps))
    (set! len (length steps))
    (set! (scale-steps obj) steps)
    ;(set! (scale-lowest obj) tonic)
    ;(set! (scale-keynum-offset obj) offset)
    (set! (scale-divisions obj) len)
    ;; N.B. octave can be TYPED interval!
    (set! (scale-octave obj) octave)
    ;; into  stores mode degree numbers for each
    ;; semitone in tuning. fill in steps first
    (let ((into (make-list (interval-semitones octave) #f)))
      (loop for step in steps
            for degree from 0
            do
	    (list-set! into (interval-semitones step) degree))
      (set! (scale-into obj) into))
    ;(print (list :low-> low))
    (transpose obj low owner)))

; (define o (new mode :steps '(2 1 2 1 2 1 2 1)))
; (define s (new mode :steps '(2 1 2 1 1)))
; (define b (new mode :steps '(2 1 2 3 2 3 3 2 1 2 2 1)))

(define-method (print-object (obj <mode>) stream)
  (let* ((name (object-name obj))
         (low (scale-lowest obj))
         (pos (if (= 0 (sd-octave low))
                (or (sd-name low) (sd-keynum low))
                (or (sd-note low) (sd-keynum low))))
         (str (string-downcase (symbol->string (class-name (class-of obj))))))
    (if name
      (format stream "#<~a: ~s (on ~a)>" str name pos)
      (format stream "#<~a (on ~a)>" str pos))))

;;;
;;; tuning and mode lookup functions
;;;

(define-method (tuning->mode (mode <mode>) keynum force?)
  ;; if force? is true then convert tuning keynum into
  ;; modal coordinates, othewise return false if keynum is
  ;; not in mode.
  (let (;(tuning (mode-tuning mode))
        ;; div is steps per octave
        (div (interval-semitones (scale-octave mode)))
        (num (- keynum (scale-keynum-offset mode))) ; offset=tonic pc
        (into (scale-into mode))
	(deg #f))
    ;; round because num may be float keynum
    (multiple-value-bind (oct int)
	(clfloor (inexact->exact (round num)) div)
      (set! deg (or (list-ref into int)
                    (if force?
                      ;; slam to closest degree if not testing
                      (loop for i from int downto 0 
                         for j = (list-ref into i)
                         when j return j)
                      #f)))
      (if deg
         (+ (* oct (scale-divisions mode))
            deg)
         #f))))

(define-method (mode->tuning (mode <mode>) keynum return)
  ;; convert keynum in modal coordinates to tuning coordinates
  (let ((scale (mode-tuning mode))
        (div (scale-divisions mode)) ; num steps in mode's own octave
        (int #f))
    (multiple-value-bind (oct rem)
	(clfloor (inexact->exact (round keynum)) div)
      ;; int may be typed interval
      (set! int (list-ref (scale-steps mode) rem))
      ;; user wants note name in typed interval mode
      (if (and (eq? return ':note)
               (%interval-encoded? int))
        ;; transpose the mode's lowest note by its typed octave size 
        ;; then add in typed interval. this could be optimized for 
        ;; normal octave modes on chromatic scale.
        (do ((low (car (scale-lowest mode)))
             (num 0 (+ num 1)))
            ((= num oct)
             (transpose low int scale))
          (set! low (transpose low (scale-octave mode) scale)))
        ;; mode's octave size is intervals spanned in tuning
        ;; and may be a typed interval
        (let ((num (+ (* oct (interval-semitones (scale-octave mode)))
                      (scale-keynum-offset mode)
                      (interval-semitones int))))
          (if (eq? return ':note)
            (tuning-keynum->note scale num #f #t)
            (if (eq? return ':hertz)
              (tuning-keynum->hertz scale num )
              num)))))))

(define-method (tuning-hertz->keynum (obj <tuning>) hz)
  ;; returns a floating point keynum for equal tunings
  ;; or an integer keynum rounded to the nearest degree
  ;; in an unequal tuning.
  (let ((low (scale-lowest obj))
	(oct (scale-octave obj))
	(int (equal-tuning? obj))
	(div (scale-divisions obj))
	(off (scale-keynum-offset obj)))
    (if int				; equal divisions
      (- (* (- (logn hz oct) 
	       (logn low oct))
	    div) 
	 off)
      ;; find hz: round to nearest integer keynum
      (let* ((into (scale-into obj))
             (val (logn (/ hz low) oct))
             (octnum (inexact->exact (floor val)))
             (octrem (- val octnum))
             (y (car into))
             (int #f))
        ;; into is list of ordered degree coeffs
        ;; defining one octave. return the index of
        ;; the closest degree in list. x and y
        ;; are current coeffs to check. y already
        ;; set to first in list.
        (set! into (cdr into))
        (do ((i 0 (+ i 1))    ; i is position of x
             (x y y))
            (int 
             ;;(- (+ (* div octnum) int) off)
             ;; if x is the last step we also have to
             ;; check proximity to octave value (1.0)
             (if (and (= int (- div 1))
                      (< (- 1 octrem) (- octrem x)))
               (- (* div (1+ octnum)) off) ; round up to octave
               (- (+ (* div octnum) int) off)))
          ;; increment y to next degree 
          (set! y (car into))
          (if (<= x octrem y)
            (if (< (- y octrem) (- octrem x))
              (set! int (+ 1 i))
              (set! int i))
            ;; if no more pairs return y index
            (if (null? (cdr into))
              (set! int (+ 1 i))
              (set! into (cdr into)))))))))

(define-method (tuning-hertz->note (scale <tuning>) hz acci err?)
  (tuning-keynum->note scale (tuning-hertz->keynum scale hz)
                       acci err?))

;;;
;;; keynum conversion to hertz, note and keynum in other scale.
;;;

(define-method (tuning-keynum->hertz (obj <tuning>) knum)
  (let ((low (scale-lowest obj))
	(off (scale-keynum-offset obj))
	(div (scale-divisions obj))
	(octsiz (scale-octave obj)))
    (if (equal-tuning? obj)		; equal divisions of octave.
      (* low (expt octsiz (/ (+ knum off) div)))
      ;; gethz:
      (multiple-value-bind (oct step)
	  (clfloor knum div)
	;; if keynum was float then step may be float remainder
	(* low (expt octsiz oct)
	   (list-ref (scale-steps obj)
		     (inexact->exact (floor step))))))))

(define-method (tuning-keynum->note (obj <tuning>) knum acci err?)
  ;; if acci is non-nil the note matching that accidental is returned.
  ;; acci can also be an ordered preference list of accidentals.
  (let* ((table (scale-table obj))
         (entries (and table
		       (scale-ref table
				 (inexact->exact (round knum))))))
    (if entries
      (let ((it
             (if acci
               (if (pair? acci)
                 (loop for a in acci
                       for x = (find a entries 
                                     :key (function sd-accidental))
                       when x return x)
                 (find acci entries :key (function sd-accidental) ))
               (first entries))))
        (if it
          (sd-note it)
          (if err?
            (err "No note with accidental ~s for keynum ~s in ~s." 
		 acci (inexact->exact (round knum)) obj)
            #f)))
      (if err?
        (if table
          (err "No note for keynum ~s in ~s." knum obj)
          (err "No note table defined in ~s." obj))
        #f))))

;;;
;;; note conversion. most of the hair is due to the fact
;;; that octave numbers can be omitted from note names.
;;;

(define %oct #f)

(define-method (tuning-note->hertz (obj <tuning>) note err?)
  (let* ((table (scale-table obj))
	 (entries (and table (scale-ref table note)))
         (entry (and entries (first entries))))
    (if entry
      (if (sd-octave entry)		; got a note.
        (begin
	 ;; update %oct if bound.
	 (if %oct (set! %oct (sd-octave entry)))
	 (tuning-keynum->hertz obj (sd-keynum entry)))
        (let ((k (+ (* (or %oct (scale-default-octave obj))
                       (scale-divisions obj)) 
                    (sd-class entry))))
          (tuning-keynum->hertz obj k)))
      (if err?
        (if table
          (err "~s is not a note in ~s." note obj)
          (err "No note table defined in ~s." obj))
        #f))))

;;;
;;; tuning-note->keynum converts note to keynum via hashtable
;;;

(define-method (tuning-note->keynum (obj <tuning>) note err?)
  (let* ((table (scale-table obj))
	 (entries (and table (scale-ref table note)))
         (entry (and entries (first entries))))
    (if entry
      (if (eq? note 'r)
        (sd-keynum entry)
        (if (sd-octave entry)		; got a note.
          (begin
	   ;; update %oct if bound.
	   (if %oct (set! %oct (sd-octave entry)))
	   (sd-keynum entry))
          (+ (* (or %oct (scale-default-octave obj))
                (scale-divisions obj)) 
             (sd-class entry))))
      (if err?
        (if table
          (err "No note for ~s in ~s." note obj)
          (err "No note table defined in ~s." obj))
        #f))))

(define-method (tuning-note->note (obj <tuning>) note acci err?)
  ;; converts notes witout octave to current octave.
  (if acci
    (tuning-keynum->note obj (tuning-note->keynum obj note err?)
                        acci err?)
    (let* ((table (scale-table obj))
           (entries (and table (scale-ref table note))))
      (if entries
        (tuning-keynum->note obj
                            (tuning-note->keynum obj note err?)
                            (sd-accidental (car entries))
                            err?)
        (if err?
          (if table
            (err "No note ~s in ~s." note obj)
            (err "No note table defined in ~s." obj))
          #f)))))

;;;
;;; KEYNUM, NOTE, and HERTZ are the main entry points to tunings
;;; and modes.
;;;

(define %hertz '(:hertz hertz :hz hz))
(define (mode? x) (is-a? x <mode>))
(define (tuning? x) (is-a? x <tuning>))

(defmacro with-default-octave (scale . body)
  (let ((v (gensym "v")))
    `(let ((,v #f))
      (dynamic-wind
       (lambda () #f)
       (lambda ()
	 (set! %oct (or %oct (scale-default-octave ,scale)))
	 (set! ,v ,@body))
       (lambda () (set! %oct #f)))
      ,v)))
  
(define (hertz freq . args)
  (let ((hz? #f)
        (scale #f)
        (mode #f)
        (tuning #f)
        (oper #f))

    (unless freq
      (err "~s is not a note name, key number, Hertz value or list."
           false))
    (when (and (pair? args) (member (first args) %hertz)
               (odd? (length args)))
      (warn "Found :HZ tag, use the :HZ keyword parameter instead: ~s"
            (list* 'note ':hz true (rest args)))
      (set! args (list* ':hz true (rest args))))
    (dopairs (sym val args)
      (case sym
        ((:hz) (set! hz? val))
        ((:in :through)
         (when oper
           (err "Found more than one of :in or :through in ~s."
                (cons 'note args)))
         (set! oper sym)
         (set! scale val))
        (else
         (err "~s not a valid keyword: :hz :in :through"  sym))))
    (unless scale (set! scale *scale*))
    (if (is-a? scale <mode>)
      (begin
       (if (eq? oper ':in)
         (err "Not a tuning: ~s ~s." oper scale))
       (set! tuning (mode-tuning scale))
       (set! mode scale))
      (begin 
       (set! tuning scale)))
      (if (pair? freq)
        (with-default-octave tuning
	  (loop for f in freq
             collect (apply (function hertz) f args)))
        ;; return untouched value if already hz and no filtering.
        (if (and (not mode) hz?)
          freq
          ;; :in keeps floating point keynums, :through filters
          ;; through interger keynums in tuning or mode.
          (let ((key (if hz? 
                       (tuning-hertz->keynum tuning freq)
                       (if (symbol? freq)
                         (tuning-note->keynum tuning freq #t)
                         freq))))
            (if mode
              (mode->tuning mode
                            (tuning->mode mode key #t)
                            ':hertz)
              (tuning-keynum->hertz tuning
                                    (if (eq? oper ':through)
                                      (inexact->exact (round key))
                                    key))))))))

(define (keynum freq . args)
  ;; return key number of freq in mode or tuning.
  (let ((hz? #f)
        (scale #f)
        (mode #f)
        (tuning #f)
        (oper #f)
        (test? #f))

    (unless freq
      (err "~s is not a note name, key number, Hertz value or list."
           false))
    (when (and (pair? args) (member (first args) %hertz)
               (odd? (length args)))
      ;; I hate this warning -- what was wrong with the old way?
      ;;(warn "Found :HZ tag, use the :HZ keyword parameter instead: ~s"
      ;;      (list* 'note ':hz true (rest args)))
      (set! args (list* ':hz true (rest args))))
    (dopairs (sym val args)
      (case sym
        ((:hz) (set! hz? val))
        ((:in :in? :through :to)
         (when oper
           (err "More than one of: :in, :in? or :through in ~s."
                (cons 'note args)))
         (if (eq? sym ':in?) (set! test? #t))
         (set! oper sym)
         (set! scale val))
        (else
         (err "~s not a valid keyword: :hz :in :in? :through"  sym))))
    (unless scale (set! scale *scale*))
    (if (is-a? scale <mode>)
      (begin
       ;; :in <mode> converts FROM modal coord to tuning keynum
       (if (eq? oper ':in)
         (set! oper ':from))
       (set! tuning (mode-tuning scale))
       (set! mode scale))
      (begin 
       (set! tuning scale)))

    (if (pair? freq)
      (with-default-octave tuning
        (loop for f in freq 
           collect (apply (function keynum) f args)))
      (let ((key (if hz? 
                   (tuning-hertz->keynum tuning freq)
                   (if (symbol? freq)
                     (tuning-note->keynum tuning freq (not test?))
                     freq))))
        ;; key is a keynum or false. If :from then key is
        ;; already in modal coords, otherwise convert.
        (if mode
          (let ((in (if (eq? oper ':from) 
                      key
                      (tuning->mode mode key (not (eq? oper ':in?))))))
            (if (not in) 
              #f
              (if (eq? oper ':to)
                in
                (mode->tuning mode in ':keynum))))
          (if (eq? oper ':through) 
            (values (inexact->exact (round key)))
            key))))))

(define (note freq . args)
  (let ((oper #f)
        (hz? #f)
        (scale #f)
        (test? #f)
        (tuning #f)
	(mode #f)
	(acci #f))
    (unless freq
      (err "~s is not a note name, key number, Hertz value or list."
           false))
    (when (and (pair? args)(member (first args) %hertz)
               (odd? (length args)))
      (warn "Found :hz tag, use the :hz keyword parameter instead: ~s"
            (list* 'note ':hz true (rest args)))
      (set! args (list* ':hz true (rest args))))
    (dopairs (sym val args)
      (case sym
        ((:hz) (set! hz? val))
        ((:in :in? :through)
         (when oper
           (err "Found more than one of :in, :in? or :through in ~s."
                (cons 'note args)))
         (if (eq? sym ':in?) (set! test? #t))
         (set! oper sym)
         (set! scale val))
	((:accidental) (set! acci val))
        (else
         (err "~s not a valid keyword: :hz :in :in? :through"
              sym))))
    (unless scale (set! scale *scale*))
    (if (is-a? scale <mode>)
      (begin
       ;; :in <mode> converts from modal coord to tuning keynum
       (if (eq? oper ':in)
         (set! oper ':from))
       (set! tuning (mode-tuning scale))
       (set! mode scale))
      (begin 
       (if  (eq? oper ':through)
         (err "Not a mode: ~s ~s." oper scale))
       (set! tuning scale)))
    
    ;; freq must be keynum if :from specified
    (when (and (eq? oper ':from)
               (or hz? (symbol? freq)))
      (err ":from conversion: ~s is not a key number in ~s."
           freq mode))
    (cond ((pair? freq)
           (with-default-octave tuning
             (loop for f in freq 
                   collect (apply (function note) f args))))
          ((symbol? freq)
           ;; freq is a symbol so only :in, :through and in?: are possible.
           ;; first insure freq is a tuning note. this has to be done
           ;; because freq may be a symbol without an octave.
           (let ((ref (tuning-note->note tuning freq acci (not test?))))
             (if (not mode)
               ref
               (if (not ref)            ; failed :in? test
                 #f
                 ;; :in? or :through for modes. In either case we must
                 ;; generate a note filtered through the mode.
                 (let* ((key (tuning-note->keynum tuning ref #t))
                        (out (mode->tuning mode
                                           (tuning->mode mode key #t)
                                           ':note)))
                   ;; if :in? only return out if same note.
                   (if test?
                     (if (eq? out ref) ref #f)
                     out))))))
          (else
           ;; freq is keynum or hertz. insure keynum
           (let ((keyn (if hz? (tuning-hertz->keynum tuning freq)
                           freq)))
             (if (not mode)
               (tuning-keynum->note tuning keyn acci (not test?))
               (let* ((in (if (eq? oper ':from)
                            keyn
                            (tuning->mode mode keyn (not test?))))
                      (out (and in (mode->tuning mode in ':note))))
                 (if test? out
                     (or out (err "No note for keynum ~s in mode ~s." 
                                  keyn mode))))))))))

;;;
;;; note properties in standard chromatic scale
;;; pitch-class, note-name, note-octave, note-accidental
;;;

(define (pitch-class note . args)
  (with-args (args &optional (scale *scale*))
    (let* ((table (scale-table scale))
	   (entry (and table (scale-ref table note))))
      (if entry (sd-class (first entry)) #f))))

(define (octave-number note . args)
  ;; return the octave number of a note in tuning.
  ;; if real? is true then return actual actual octave
  ;; else return the note's external number
  (with-args (args &optional (scale *scale*) real?)
    (let* ((table (scale-table scale))
	   (entry (and table (scale-ref table note))))
      (if entry 
	(if (sd-octave (first entry))
          (if real?
            (sd-octave (first entry))
            (+ (sd-octave (first entry))
               (scale-octave-offset scale)))
	  #f)
	#f))))

(define (note-name note . args)
  (with-args (args &optional (scale *scale*)) 
    (let* ((table (scale-table scale))
	   (entry (and table (scale-ref table note))))
      (if entry (sd-name (first entry)) #f))))

(define (note-accidental note . args)
  (with-args (args &optional (scale *scale*))
    (let* ((table (scale-table scale))
	   (entry (and table (scale-ref table note))))
      (if entry (sd-accidental (first entry)) #f))))

;;;
;;; scale comparison
;;;

(define (scale< a b . args)
  (with-args (args &optional (scale *scale*))
    (< (if (number? a) a (tuning-note->keynum scale a #t))
       (if (number? b) b (tuning-note->keynum scale b #t)))))

(define (scale<= a b . args)
  (with-args (args &optional (scale *scale*))
    (<= (if (number? a) a (tuning-note->keynum scale a #t))
	(if (number? b) b (tuning-note->keynum scale b #t)))))

(define (scale= a b . args)
  (with-args (args &optional (scale *scale*))
    (= (if (number? a) a (tuning-note->keynum scale a #t))
       (if (number? b) b (tuning-note->keynum scale b #t)))))

(define (scale>= a b . args)
  (with-args (args &optional (scale *scale*))
    (>= (if (number? a) a (tuning-note->keynum scale a #t))
	(if (number? b) b (tuning-note->keynum scale b #t)))))

(define (scale> a b . args)
  (with-args (args &optional (scale *scale*))
    (> (if (number? a) a (tuning-note->keynum scale a #t))
       (if (number? b) b (tuning-note->keynum scale b #t)))))

(define (scale-min a b . args)
  (with-args (args &optional (scale *scale*))
    (if (< (if (number? b) b (tuning-note->keynum scale b #t))
	   (if (number? a) a (tuning-note->keynum scale a #t)))
      b a)))

(define (scale-max a b . args)
  (with-args (args &optional (scale *scale*))
    (if (> (if (number? b) b (tuning-note->keynum scale b #t))
	   (if (number? a) a (tuning-note->keynum scale a #t)))
      b a)))

(define-method (scale-mod (freq <number>) modulus . args)
  (with-args (args &key (offset #t) (in *scale*) accidental)
    accidental
    (let* ((div (scale-divisions in))
	   (val (if (symbol? offset)
		  (if (eq? offset #t)
		    (* (floor (/ (max freq 0) div)) div)
		    (keynum offset :in in))
		  offset)))
      (+ val (mod (- freq val) modulus)))))

(define-method (scale-mod (freq <symbol>) modulus . args)
  (with-args (args &key (offset t) (in *scale*) accidental)
    (tuning-keynum->note in (scale-mod (keynum freq) modulus
				       :offset offset :in in)
			 accidental #t)))

(define-method (scale-mod (freq <pair>) modulus  . args)
  (with-args (args &key (offset #t) (in *scale*) accidental)
    (if (eq? offset #t)
      (set! offset (first freq)))
    (loop for f in freq 
	  collect (scale-mod f modulus :in in 
			     :offset offset
			     :accidental accidental))))

(define-method (note-in-scale? note (scale <scale>))
  (let ((table (scale-table scale)))
    (and table (scale-ref table note) note)))

(define (scale-order lis . args)
  (with-args (args &optional (order ':up) unique?)
    (if unique?
      (set! lis (remove-duplicates lis))
      (set! lis (copy-list lis)))
    (cond ((or (eq? order #t)
               (eq? order ':up))
           (sort! lis #'scale<))
          ((eq? order ':down )
           (sort! lis #'scale>))
          ((eq? order ':random )
           (shuffle lis))
          ((not order)
           lis)
          (else
           (sort! lis order)))))

;;;
;;; transpose
;;;

(define-method (transpose (note <pair>) int . args)
  (with-args (args &optional (scale *scale*))
    (with-default-octave scale
      (loop for n in note
	    collect (transpose n int scale)))))

(define-method (transpose (note <symbol>) int . args)
  (with-args (args &optional (scale *scale*))
    (if (eq? note 'r)
      note
      (if (number? int)
	(let* ((table (scale-table scale))
	       (entries (and table (scale-ref table note)))
	       (entry (and entries (first entries))))
	  (if entry
	    (if (sd-octave entry)	; got a note, update %oct
	      (if %oct (set! %oct (sd-octave entry)))
	      (set! entry (octave-equivalent
			   note
			   (or %oct (scale-default-octave scale))
			   scale)))
	    (err "~s is not a note in ~s." note scale))
	  (if (and (eq? scale *chromatic-scale*)
		   (integer? int)
		   (%interval-encoded? int))
	    (let* ((names '((cff cf c cn cs css) (dff df d dn ds dss)
			    (eff ef e en es ess) (fff ff f fn fs fss)
			    (gff gf g gn gs gss) (aff af a an as ass)
			    (bff bf b bn bs bss)))
		   ;; get position of note in names. transpose 
		   ;; the spelling +- letters in the interval.
		   (trans
		    (list-ref names
			      (modulo (+ (position (sd-name entry)
						   names 
                                                   :test (function member))
					 (* (%interval-letters int)
					    (%interval-sign int)))
				      7)))
		   ;; transpose keynum +- size of interval
		   (keyn (+ (sd-keynum entry)
			    (interval-semitones int)))
		   ;; get all entries at the new keynum. if note
		   ;; is a note class make sure we dont go negative
		   (entries (scale-ref table
				      (if (sd-octave entry)
					keyn (mod keyn 12))))
		   ;; find the entry whise name that corresponds to one
		   ;; of the names in the transposed set of names. 
		   (newnote
		    (or (loop for n in trans
			      for e = (find n entries 
                                            :key (function sd-name))
			      when e return e)
			(err "Can't transpose ~a by ~a." 
			     note (decode-interval int)))))
	      (if (sd-octave entry)
		(sd-note newnote)
		(sd-name newnote)))
	    (tuning-keynum->note scale (+ (sd-keynum entry) int)
				 #f #t)))
	(err "Don't know how to transpose ~s by ~s." 
	     note int)))))

(define-method (transpose (note <number>) int . args)
  (with-args (args &optional (scale *scale*))
    (if (number? int)
      (if (and (integer? note)
	       (%interval-encoded? note))
	(if (and (integer? int)
		 (%interval-encoded? int))
	  (let* ((sign1 (%interval-sign note))
		 (sign2 (%interval-sign int))
		 (width (+ (* (%interval-letters note)
			      sign1)
			   (* (%interval-letters int)
			      sign2)))
		 (steps (+ (* (%interval-semitones note)
			      sign1)
			   (* (%interval-semitones int)
			      sign2)))
		 ;; maps of interval and type
		 (perfs '(0 1 3 5 6))
		 (imperfs '(0 1 2 4 5 6))
		 (offset #f)
                 (types #f)
                 (type #f))
	    (case (modulo width 7)
	      ((0 ) 
	       (set! offset -2)
	       (set! types perfs))
	      ((1 )
	       (set! offset -1)
	       (set! types imperfs))
	      ((2 )
	       (set! offset 1)
	       (set! types imperfs))
	      ((3 )
	       (set! offset 3)
	       (set! types perfs))
	      ((4 )
	       (set! offset 5)
	       (set! types perfs))
	      ((5 )
	       (set! offset 6)
	       (set! types imperfs))
	      ((6 )
	       (set! offset 8)
	       (set! types imperfs)))
	    (set! type (list-ref types
				 (modulo (- steps offset) 12)))
	    (encode-interval (first (list-ref interval-names type))
			     (+ width 1)))
	  (+ (interval-semitones note) int))
	(+ note (if (and (integer? int)
			 (%interval-encoded? int))
		  (interval-semitones int)
		  int)))
      (if (symbol? int)
	(transpose int note scale)
	(err "Don't know how to transpose ~s by ~s." 
             note int)))))

(define-method (transpose (obj <mode>) note . args)
  (with-args (args &optional (scale *scale*))
    (set! scale (mode-tuning obj))
    (unless (note-in-scale? note scale)
      (err "~s is not defined in mode's tuning" note))
    ;; is pitch-class
    (if (eq? (note-name note scale) note)
      (begin
       (set! (scale-lowest obj)
             (octave-equivalent note 0 scale))
       (set! (scale-keynum-offset obj)
             (sd-class (scale-lowest obj))))
      (let* ((oct (octave-number note scale #t))
             (ref (octave-equivalent note oct scale)))
        (set! (scale-lowest obj) ref)
        (set! (scale-keynum-offset obj)
              (sd-keynum ref))))
    obj))

;;;
;;; Invert
;;;

;(define-method (invert (lst <pair>) . args)
;  (with-args (args &optional point (scale *scale*))
;    (let ((center (keynum (or point (first lst)) :in scale )))
;      (loop for n in lst
;	    for i = (* (- center (keynum n )) 2)
;	    collect (transpose n i scale)))))

(define-method (invert (lst <pair>) . args)
  (with-args (args &optional pc?)
    (let* ((n (car lst))
           (k (keynum n)))
      (loop for x in lst 
           for d = (- k (keynum x)) 
            collect (transpose n (if pc? (mod d 12) d))))))

;;;
;;; interval
;;;

(define-method (interval (ref <pair>) . args)
  (with-args (args &optional ref2)
    ref2
    (interval (first ref) (second ref))))

(define-method (interval (ref <integer>) . args)
  (with-args (args &optional ref2)
    ;; interpret two numbers as steps and letters spanned.
    ;; ref2 is treated as the absolute letter span counting
    ;; in the same direction as the interval.
    (if (null? ref2)
      ref
      (let* ((steps ref)
	     (width (+ 1 (modulo (abs ref2) 7)))
	     (perfs '(0 1 3 5 6))	; positions in interval-names
	     (imperfs '(0 1 2 4 5 6))
	     (offset #f)
	     (types #f)
	     (type #f)
	     (int #f))
	(case width
	  ((1 ) 
	   (set! offset -2)
	   (set! types perfs))
	  ((2 )
	   (set! offset -1)
	   (set! types imperfs))
	  ((3 ) 
	   (set! offset  1)
	   (set! types imperfs))
	  ((4 )
	   (set! offset  3)
	   (set! types perfs))
	  ((5 )
	   (set! offset  5)
	   (set! types perfs))
	  ((6 )
	   (set! offset  6)
	   (set! types imperfs))
	  ((7 )
	   (set! offset  8)
	   (set! types imperfs)))
	(set! type (list-ref types (modulo (- (abs steps) offset) 12)))
	;; use semitones to determine octave placements
	(set! int (+ (* (inexact->exact 
			 (floor (/ (abs steps) 12))) 7) 
		     width))
	(encode-interval (first (vector-ref interval-names type))
			 (if (< steps 0) (- int) int))))))
      
(define-method (interval (ref <symbol>) . args)
  (with-args (args &optional ref2)
    ;; if ref and ref2 are notes then return interval
    ;; distance between then (if ref is a note class
    ;; an ascending interval is always returned.) otherwise
    ;; parse ref as a symbolic interval type.
    (cond ((note-in-scale? ref *chromatic-scale*)
	   (let* ((table (scale-table *chromatic-scale*))
		  (entries1 (and table (scale-ref table ref)))
		  (entry1 (and entries1 (first entries1)))
		  (entries2 (and table (scale-ref table ref2)))
		  (entry2 (and entries2 (first entries2)))
		  (names '((cff cf c cn cs css) (dff df d dn ds dss)
			   (eff ef e en es ess) (fff ff f fn fs fss)
			   (gff gf g gn gs gss) (aff af a an as ass)
			   (bff bf b bn bs bss)))
		  (letters #f)
		  (semitones #f))
	     ;; swap so higher second note makes ascending interval
	     (rotatef entry1 entry2)
	     (if (not entry1) 
	       (error "~s not in standard chromatic scale." ref2))
	     ;; calculate abs letter difference
	     (set! letters (modulo (- (position (sd-name entry1) names  
						:test (function member))
				      (position (sd-name entry2) names  
						:test (function member)))
				   7))
	     (set! semitones (- (sd-keynum entry1)
				(sd-keynum entry2)))
           
	     ;; if note is note class always calculate
	     ;; interval with second note above
	     (when (and (not (sd-octave entry1))
			(< semitones 0))
	       (set! semitones (+ semitones 12)))
           
	     ;; letter names ascend from c so we have
	     ;; to use complement if negative interval.
	     (if (< semitones 0)
	       (interval semitones (- (- 7 letters)))
	       (interval semitones letters))))
	  ((null? ref2)
	   (let* ((str (symbol->string ref))
		  (beg 0)
		  (end (string-length str))
		  (pos (- end 1))	; position past the type spec
		  (sign 1)
		  (size #f))
	     (if (char-numeric? (string-ref str pos))
	       (loop while (and (> (- pos 1) 0)
				(char-numeric? 
				 (string-ref str (- pos 1))))
		 do (set! pos (- pos 1))
		 finally
		 (set! size
		       (string->number (substring str pos end))))
	       (err "~s is not an interval." ref))
	     (if (char=? (string-ref str 0) #\-)
	       (begin (set! sign -1)
		      (incf beg))
	       (if (char=? (string-ref str 0) #\+)
		 (begin 
		   (incf beg))))
	     (encode-interval (substring str beg pos)
			      (* size sign))))
	  (else
	   (encode-interval ref ref2)))))

; (decode-interval (interval 'e4 'e4))
; (decode-interval (interval 'e4 'e5))
; (decode-interval (interval 'e5 'e4))
; (decode-interval (interval 'e4 'd4))
; (decode-interval (interval 'e4 'd5))
; (decode-interval (interval 'c4 'd4))
; (decode-interval (interval 'c4 'd3))
; (decode-interval (interval 'c4 'ef5))
; (decode-interval (interval 'c4 'a2))
; (decode-interval (interval 'd4 'a4))
; (decode-interval (interval 'a4 'd4))
; (decode-interval (interval 'b3 'ds4))
; (decode-interval (interval 'b3 'ds3))
; (decode-interval (interval 'b3 'g3))
; (decode-interval (interval 'b3 'g4))
; (decode-interval (interval 'd4 'b4))
; (decode-interval (interval 'b3 'd4))
; (decode-interval (interval 'b4 'd4))
; (decode-interval (interval 'b3 'f4))
; (decode-interval (interval 'f4 'b4))
; (decode-interval (interval 'cf4 'd4))

;(loop for n in '(ef4 f4 g4 af4 bf4 c5 d5 ef5) 
;      collect (decode-interval (interval 'ef4 n)))

;(loop for n in '(ef4 f4 gf4 af4 bf4 cf5 d5 ef5) 
;      collect (decode-interval (interval 'ef4 n)))

;(transpose 'g5 (interval '-aug2))
;(transpose 'g5 (interval '-aug7))
;(transpose 'ef5 (interval 'aug2))
;(transpose 'ef5 (interval 'aug7))
;(transpose 'ef5 (interval '-aug2))
;(transpose 'ef5 (interval '-aug7))
;(interval-semitones (interval '(:min -7)))
;(decode-interval (interval '-aug7))
;(interval-semitones (interval '-aug7))
;(defun interval-bits (i)
;  (format t "~%
;(interval-semitones (interval 'aug7))

;;;
;;;
;;;

(set! *chromatic-scale*
      (make-instance <tuning>
		     :name "chromatic-scale"
		     :octaves '(-1 10)
		     :lowest 6.875
		     :keynum-offset 3
		     :default-octave 5
		     :cents '((100 
			       (c  ) 
			       (cn  :accidental  n)
			       (bs  :accidental  s :octave -1)
			       (dff :accidental ff))
			      (100 
			       (cs  :accidental  s)
			       (df  :accidental  f)
			       (bss :accidental ss :octave -1))
			      (100 
			       (d   )
			       (dn  :accidental  n)
			       (css :accidental ss)
			       (eff :accidental ff))
			      (100 
			       (ef  :accidental  f)
			       (ds  :letter d :accidental  s)
			       (fff :letter f :accidental ff))
			      (100 
			       (e   )
			       (en  :accidental  n)
			       (ff  :accidental  f)
			       (dss :accidental ss))
			      (100 
			       (f   ) 
			       (fn  :accidental  n)
			       (es  :accidental  s)
			       (gff :accidental ff))
			      (100 
			       (fs  :accidental  s)
			       (gf  :accidental  f)
			       (ess :accidental ss))
			      (100 
			       (g   )
			       (gn  :accidental  n)
			       (fss :accidental ss)
			       (aff :accidental ff))
			      (100 
			       (af  :accidental  f)
			       (gs  :accidental  s))
			      (100 
			       (a   )
			       (an  :accidental  n)
			       (gss :accidental ss)
			       (bff :accidental ff))
			      (100 
			       (bf  :accidental  f)
			       (as  :accidental  s)
			       (cff :accidental ff :octave +1))
			      (100 
			       (b   )
			       (bn  :accidental  n)
			       (cf  :accidental  f  :octave +1)
			       (ass :accidental ss)))))

(set! *scale* *chromatic-scale*)

;;;
;;; eof
;;;
