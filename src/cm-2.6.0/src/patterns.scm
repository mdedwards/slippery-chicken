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
;;; $Revision: 1.7 $
;;; $Date: 2005/03/17 19:39:45 $

(define +constant-data+    (ash 1 0))  ; avoid hair when possible
(define +constant-weights+ (ash 1 1))  ; avoid random index recalc
(define +count-periods+    (ash 1 2))  ; period counts subperiods
(define +count-values+     (ash 1 3))  ; period counts values
(define +depth-first+      (ash 1 4))  ; pattern moves on eop
(define +breadth-first+    (ash 1 5))  ; pattern moves each time
(define +coerce-to-note+   (ash 1 6))  ; return pitches
(define +coerce-to-pitch+  (ash 1 7))  ; return note names
(define +parallel-offsets+ (ash 1 8))  ; voicing mode

(define +range-stepping+   (ash 1 10))  
(define +range-initially+  (ash 1 11))
(define +range-unbounded+  (ash 1 12)) 
(define +range-dynamic+    (ash 1 13)) 
(define +constant-minmax+  (ash 1 14))  ; avoid minmax recalc
(define +default-period+   (ash 1 15))  ; no period specified
(define +range-random+     (ash 1 16))  ; no period specified

(define +nad+ ':not-a-datum)           ; "not a datum" marker
(define +eop+ ':end-of-period)         ; "end of period" marker
(define +eod+ ':end-of-data)         ; "end of period" marker

;;;
;;; the period struct holds information for various period calculations. 
;;; count is number of reads remaining in current period. when count=0
;;; the period is reinitialized. length is maximum count of the period,
;;; either a number or T if dynamic length. if stream is not nil it a
;;; new length will be read from it each time the period is initialized.
;;; omit is the number of times this stream is skipped in its parent's
;;; pattern, if dynamic. Reps keeps track of the number of periods. Max
;;; is the max number of periods allowed, after which the pattern always
;;; returns +exhausted+

(define-list-struct period
  (count 0) length stream default (omit 0) (reps 0))

;;;
;;;
;;;

(define-class <pattern> (<container>) 
  (flags :init-keyword :flags :init-value 0
	 :accessor pattern-flags)
  (data  :init-keyword :of 
	 :init-keyword :data
	 :init-keyword :notes
	 :init-keyword :keynums
	 :init-keyword :rhythms
	 :init-keyword :amplitudes
	 :init-keyword :intervals
	 :init-value '()
	 :accessor pattern-data)
  (length :accessor pattern-length)
  (datum :init-thunk (lambda () +nad+)
	 :accessor pattern-datum)
  (period :init-keyword :for 
	  :accessor pattern-period)
  (value :init-keyword :last-value
	 :init-thunk (lambda () +nad+)
	 :accessor pattern-value)
  (state :init-keyword :last-state
	 :init-thunk (lambda () +nad+)
	 :accessor pattern-state)
  (repeat :init-keyword :repeat
	  :init-value most-positive-fixnum
	  :accessor pattern-repeat)
  (parser :init-keyword :parser :init-value #f 
	  :accessor pattern-parser )
  (returning :init-value #f :init-keyword :returning
	     :accessor pattern-returning)
  (counting :init-keyword :counting :init-value :periods
	    :accessor pattern-counting)
  (traversing :init-keyword :traversing
	      :init-value :depth-first
	      :accessor pattern-traversing)
  :name 'pattern)

;;;
;;; canonicalize-pattern-data returns three values: a list of parsed
;;; data, the number of elements parsed, and T or NIL if the data
;;; is constant (contains no substreams).
;;;

(define (maybeparse fn val)
  (if (or (not fn) (is-a? val <pattern>))
    val
    (fn val)))

(define-method (canonicalize-pattern-data (obj <pattern>)
					  data parser inits)
  ;;(format #t "pattern canonicalize: ~s~%" obj)
  inits
  (if parser
    (loop for d in data 
          count d into len
          count (pattern? d) into num
          collect (maybeparse parser d) into lst
          finally (return (values lst len (= num 0))))
    (values data (length data) (not (some #'pattern? data)))))

;;;
;;; make-load-form "decompiles" pattern
;;;

(define-method (make-load-form (obj <pattern>))
  (let ((inits (pattern-external-inits obj))
        (gvar (string->symbol (format #f "<~a>"
                                      (class-name (class-of obj))))))
    ;; handle data and period specially
    `(make-instance ,gvar ,@ inits)))

;;;
;;; pattern-external-inits returns the list of slot inits
;;; to include in a make-load-form expression. every subclass
;;; of pattern should call (next-method) and append any
;;; local inits to that list.
;;;

(define-method (pattern-external-inits (obj <pattern>))
  (let ((inits
         (slot-init-forms obj :eval #t 
                          :ignore-defaults #t 
                          :only '(repeat parser returning counting 
                                  traversing)
                          :key #'expand-pattern-value)))
    (unless (logtest (pattern-flags obj) +default-period+)
      (let ((per (pattern-period obj)))
        (push 
         (expand-pattern-value (or (period-stream per)
                                   (period-length per)))
         inits)
        (push ':for inits)))
    inits))

(define (expand-pattern-value val)
  (cond ((pattern? val)
         (make-load-form val))
        ((pair? val)
         (if (eq? (first val) 'quote)
           val
           (cons 'list 
                 (loop for x in val 
                       collect (expand-pattern-value x)))))
        ((symbol? val) `(quote ,val))
        (else val)))

;;;
;;; pattern-period-length returns the current period length
;;;

(define-method (pattern-period-length (obj <pattern>))
  (period-length (pattern-period obj)))

;;;
;;; default-period-length returns the default period length for a
;;; given pattern class. defaults to the to the number of elements
;;; the pattern contains.
;;;

(define-method (default-period-length (obj <pattern>))
  (pattern-length obj))

(define-method (initialize (obj <pattern>) args)
  (next-method)				; was after method

  ;;(format #t "pattern initialize: ~s data=~s~%" 
  ;;        obj (pattern-data obj))
  (let ((flags (pattern-flags obj))
;        (data (pattern-data obj))
        (data #f)
        (len #f)
	(parser (pattern-parser obj))
	(constant? #f))
    
;    (unless (list? data)
;      (set! data (list data))
;      (set! (pattern-data obj) data))

    ;; if a slot has multiple initargs their values must be parsed from
    ;; the args since goops doesn't recogize them as true initargs...
    (do ((a args (cddr a)))
	((or data (null? a)) #f)
      (case (car a)
        ((:of :data )
         (set! data (cadr a)))
	((:notes )
	 (set! data (cadr a))
	 (set! parser #'note))
	((:keynums )
	 (set! data (cadr a))
	 (set! parser #'keynum))
	((:rhythms )
	 (set! data (cadr a))
	 (set! parser #'rhythm))
	((:amplitudes )
	 (set! data (cadr a))
	 (set! parser #'amplitude))))
    
    (unless (list? data) (set! data (list data)))
    ;; parse external data into canonical form. oct is bound
    ;; in case note, keynum or hertz are called as parsers.
    (with-default-octave *scale*
      (multiple-value-setq
       (data len constant?) 
       (canonicalize-pattern-data obj data parser args)))
    (set! (pattern-data obj) data)
    (set! (pattern-length obj) len)
    ;;(format #t "done with canonicalization.~%")

    ;; parse counting option
    (let ((counting (pattern-counting obj)))
      (when counting
        (case counting
          ((:periods :period periods period)
           (set! flags (logior flags +count-periods+)))
          ((:values :value values value)     
           (set! flags (logior flags +count-values+)))
          (else
           (err "~s not one of 'counting' keywords:  periods values."
                counting)))))
    
    ;; parse traversing option
    (let ((traversing (pattern-traversing obj)))
      (when traversing
        (case traversing
          ((:depth-first :depth depth-first depth) 
           (set! flags (logior flags +depth-first+)))
          ((:breadth-first :breadth breadth-first breadth)
           (set! flags (logior flags +breadth-first+)))
          (else
           (err "~s not 'traversing' keword: depth-firs,t breadth-first."
                traversing)))))
    
    (when constant?
      (set! flags (logior flags +constant-data+)))

    ;; if constant data and counting subperiods, switch to counting
    ;; values instead since its the same thing and we can avoid
    ;; resetting subperiods if period length is nevertheless expressed
    ;; dynamically.
    (cond ((logtest flags +count-values+)
           (set! flags (logand flags (lognot +count-periods+))))
          (else
           (if (logtest flags +constant-data+)
             (set! flags (logior (logand flags (lognot +count-periods+))
                                 +count-values+))
             (set! flags (logior flags +count-periods+)))))
    
    
    (let* ((default (default-period-length obj))
           (period (if (slot-bound? obj 'period)
                     (or (pattern-period obj) default)
                     default)))
      ;; if no period was specfied patterns can 
      ;; do what they want with it.
      (if (and (slot-bound? obj 'period)
               (not (null? (slot-ref obj 'period))))
        (set! period (slot-ref obj 'period))
        (begin
         (set! period default)
         (set! flags (logior flags +default-period+))))
      (set! (pattern-period obj)
            (if (or (number? period)
		    (eq? period #t))
              (make-period :length period :default default)
              (make-period :stream period :default default))))
    (set! (pattern-flags obj) flags)
    (values)))

;;;
;;; type predicate for patterns.
;;;

(define-method (pattern? obj) obj #f)

(define-method (pattern? (obj <pattern>)) obj)

;;;
;;; Predicates for testing end-of-period and end-of-data.
;;;

(define-method (eop? x)
  (eq? x +eop+))

(define-method (eop? (x <pattern>)) 
  (eop? (pattern-state x)))

(define-method (eod? x)
  (eq? x +eod+))

(define-method (eod? (x <pattern>))
  (eod? (pattern-value x))) 

(define-method (pattern-state obj) obj +eop+)

;;;
;;; next returns the next value read from the object.
;;; this around method implements the basic behavior of patterns.
;;; it first checks the stream's period length and calls reset-period 
;;; if at end. if the next period length is 0 it immediately returns
;;; +nad+, which causes a superior stream (if any) to skip over the
;;; current stream as it increments its pattern.  otherwise, the method
;;; then increments the streams pattern until it yields a datum that is 
;;; not +nad+ and that call-next-method does not return +nad+ from. if
;;; the stream's data is known to contain only constant values, ie no
;;; substreams, the testing loop is skipped. once call-next-method
;;; returns a value (not +nad+), the period and pattern of the stream
;;; are incremented according to their mode. for period incrementing, 
;;; +count-periods+ increments the period count only on +eop+, and
;;; +count-values+ increments the period count every time. for pattern
;;; incrementing, +depth-first+ increments the pattern only on +eop+,
;;; and +breadth-first+ increments the pattern every time.
;;;

(define (next obj . args)
  (with-args (args &optional num)
    (if num
      (if (number? num )
        (let ((l (list #f)))
          (do ((i 0 (+ 1 i))
               (e l (cdr e)))
              ((>= i num)
               (cdr l))
            (set-cdr! e (list (next-1 obj)))))
        (if (pattern? obj)
          (let ((l (list #f)))
            (do ((n (next-1 obj) )
                 (e l (cdr e))
                 (f #f))
                ((or (eq? n +eod+) f)
                 (cdr l))
              (set-cdr! e (list n))
              (if (eop? obj)
                (set! f #t)
                (set! n (next-1 obj))))) 
          (list obj)))
      (next-1 obj))))

(define-method (next-1 obj)
  obj)

(define-method (next-1 (obj <pattern>))
  (let ((period (pattern-period obj))
	(nomore #f))

    ;; reset period, return
    (when (= (period-count period) 0)
      (when (>= (period-reps period)
		(pattern-repeat obj))
	(set! (pattern-value obj) +eod+)
	(set! (pattern-state obj) +eop+)
	(set! nomore +eod+))
      (when (and (not nomore)
		 (= (reset-period obj) 0))
	(set! nomore +nad+)
	(set! (pattern-value obj) +nad+)
	(set! (pattern-state obj) +eop+)))

    (if nomore
      nomore
      (let ((flags (pattern-flags obj))
	    (retfn (pattern-returning obj))
	    (value #f)
	    (state #f))

	;; increment datum until not +nad+
	(if (logtest flags +constant-data+)
	  (begin
	    (set! (pattern-datum obj) (next-in-pattern obj))
	    (set! value (next-1 (pattern-datum obj)))
	    (set! state +eop+))
	  (loop with dyn? = (and (logtest flags +count-periods+)
				 (eq? (period-length period) #t))
		do  
		;; increment over 0 length substreams
		(loop while (eq? (pattern-datum obj) +nad+)
		  do (set! (pattern-datum obj)
			   (if dyn?
			     (skip-datum? (next-in-pattern obj))
			     (next-in-pattern obj))))
		(set! value (next-1 (pattern-datum obj)))
		(set! state (pattern-state (pattern-datum obj)))
		;; increment over +nad+ values returned by obj.
		(if (eq? value +nad+)
		  (set! (pattern-datum obj) value)
		  (return ))))
	;;(format #t "datum=~s value=~s state=~s~%" datum value state)
	;; increment period and pattern as appropriate.
	(cond ((eq? state +eop+)
	       (period-count-set! period (- (period-count period) 1))
	       (set! (pattern-datum obj) +nad+)
	       (set! state #f))
	      (else
	       (when (logtest flags +breadth-first+)
		 (set! (pattern-datum obj) +nad+))
	       (when (logtest flags +count-values+)
		 (period-count-set! period
				    (- (period-count period) 1)))))
    
	(if (= (period-count period) 0)
	  (begin (set! state +eop+)
		 (period-reps-set! period
				   (+ 1 (period-reps period))))
	  (set! state state))

	(if retfn
	  (set! value ( retfn value)));; funcall

	(set! (pattern-state obj) state)
	(set! (pattern-value obj) value)
	value))))

;;;
;;; skip-datum? returns +nad+ if the current stream should be
;;; skipped in the pattern. this only happens if we have dynamic
;;; periodicity and the datum had a 0 length period when it was
;;; encountered by reset-period.
;;;

(define-method (skip-datum? (obj <pattern>))
  (let ((period (pattern-period obj)))
    (if (> (period-omit period) 0)
        (begin (period-omit-set! period
				 (- (period-omit period) 1))
	       +nad+)
        obj)))

(define-method (skip-datum? obj) obj)

;;;
;;; reset-period sets and returns the length of the next period.
;;; period length of constant datum is always 1.
;;;

(define-method (reset-period obj) obj 1)

(define-method (reset-period (obj <pattern>))
  (let ((period (pattern-period obj))
        (dyn #f)
	(len #f))

    ;; if period is supplied as a stream get next length via item
    (when (period-stream period)
      (period-length-set! period
			  (next-1 (period-stream period))))
    (set! dyn (eq? (period-length period) #t))
    (set! len
	  (if dyn
	    (period-default period)
	    (period-length period)))
    ;; if we have dynamic period length we adjust next period length
    ;; for the number of 0 subperiods that this period will encounter.
    ;; in order for this to work, all substream periods must be reset
    ;; now, at the same that the super stream is reset. we can only
    ;; do this if we know that all subperiods are currently at end
    ;; of period, ie if we are counting by subperiods. if so, then by
    ;; definition all the substreams must be at end-of-period  or we
    ;; couldn't have gotton here in the first place. after resetting
    ;; substream period lengths we decrement our current stream's period
    ;; length by the number of zero periods found.
    (when (and dyn
               (logtest (pattern-flags obj) +count-periods+))
       (let ((zeros 0))
         (map-pattern-data #'(lambda (x)
			       (when (= (reset-period x) 0) 
				 (let ((p (pattern-period x)))
				   (period-omit-set! p 
						     (+ (period-omit p)
							1)))
				 (incf zeros)))
			   obj)
         (when (> zeros 0)
	   (set! len (max (- len zeros) 0)))
         ))
    (period-count-set! period len)
    len))

;;;
;;; pattern implementations.
;;;
;;; cycle continously loops over its data. the data are held
;;; in a list of the form: (data . data). successive elements are
;;; popped from the cdr, when the cdr is null it's reset to the car.
;;;

(define-class <cycle> (<pattern>)
  :name 'cycle)

(define-method (pattern-external-inits (obj <cycle>))
  (append 
   (list ':of
         (cons 'list
               (loop for x in (car (pattern-data obj))
                     collect (expand-pattern-value x))))
   (next-method)))

(define-method (initialize (obj <cycle>) args)
  (next-method) ; was after
  ;;(format #t "cycle initialize: ~s data=~s~%" 
  ;;        obj (pattern-data obj))
  (let ((cyc (make-cycl)))
    (cycl-data-set! cyc (pattern-data obj))
    (set! (pattern-data obj) cyc)
    (values)))

(define-method (next-in-pattern (obj <cycle>))
  (let ((cyc (pattern-data obj)))
    (if (null? (cycl-tail cyc))
      (cycl-tail-set! cyc (cycl-data cyc)))
    (pop-cycl cyc)))

(define-method (map-pattern-data fn (obj <cycle>))
  (for-each fn (cycl-data (pattern-data obj))))

; (define a (make <cycle> :of (list 1 2 3)))

;;;
;;; palindrome visits the reverse of its data.
;;;

(define-class <palindrome> (<pattern>)
  (elide :init-value #f :init-keyword :elide
	 :accessor palindrome-elide)
  :name 'palindrome)

(define-method (pattern-external-inits (obj <palindrome>))
  (append 
   (list ':of (cons 'list
                    (loop for x in (car (pattern-data obj))
                          collect (expand-pattern-value x))))
   (next-method) 
   (if (not (palindrome-elide obj))
     (list)
     (list ':elide
           (expand-pattern-value 
            (palindrome-elide obj))))))

(define-method (map-pattern-data fn (obj <palindrome>))
  (for-each fn (cycl-data (pattern-data obj))))

(define-method (default-period-length (obj <palindrome>))
  (* 2 (pattern-length obj)))

(define-method (initialize (obj <palindrome>) args)
  (next-method)
  (let ((cyc (make-cycl)))
    (cycl-data-set! cyc (pattern-data obj))
    (set! (pattern-data obj) cyc)
    (values)))

(define-method (next-in-pattern (obj <palindrome>))
  (let ((cycl (pattern-data obj)))
    (when (null? (cycl-tail cycl)) 
      (let ((mode (next-1 (palindrome-elide obj)))
            (half (cycl-data cycl))
            (next '())
            (long (pattern-length obj))
            (bits (pattern-flags obj)))
        (cond ((eq? mode #t)
               (set! long (- (* long 2) 2))
               (set! next (append half (cdr (reverse (cdr half))))))
              ((not mode)
               (set! long (* long 2))
               (set! next (append half (reverse half))))
              ((memq mode '(:first first :start start :left left))
               (set! long (- (* long 2) 1))
               (set! next (append half (reverse (cdr half)))))
              ((memq mode '(:last last :end end :right right))
               (set! long (- (* long 2) 1))
               (set! next (append half (cdr (reverse half)))))
              (else
               (err "~s is not an elide value: ~s, ~s, :first, :last."
                    mode true false)))
        (when (logtest bits +default-period+)
          (period-count-set! (pattern-period obj) long))
        (cycl-tail-set! cycl next)))
    (pop-cycl cycl)))

;(define-method (canonicalize-pattern-data (obj <palindrome>) b c d)
;  ;; this method rewrites data to comform to a palindrome. the
;  ;; data are treated thereafter simply as a cycle.
;  ;;(format #t "palindrome canonlicalize: ~s data=~s~%" 
;  ;;         obj (pattern-data obj))
;  b c d
;  (multiple-value-bind (data len flag) (next-method)
;    (let ((elide (palindrome-elide obj ))
;          (half data))
;      (cond ((eq? elide #t)
;             (set! len (- (* len 2) 2))
;             (loop with rev = '()
;		   for tail on (cdr half)
;                   while (not (null? (cdr tail)))
;                   do (push (car tail) rev)
;                   finally (set! data (append half rev))))
;            ((not elide)
;             (set! len (* len 2))
;             (loop with rev = '()
;		   for i in half
;                   do (push i rev)
;                   finally (set! data (append half rev))))
;            ((member elide '(:first first start :start left :left ))
;             (set! len (- (* len 2) 1))
;             (loop with rev = '()
;		   for i in (cdr half)
;                   do (push i rev)
;                   finally (set! data (append half rev))))
;            ((member elide '(:last first last :end right :right))
;             (set! len (- (* len 2) 1))
;             (loop with rev = '()
;		   for tail on half
;                   while (not (null? (cdr tail)))
;                   do (push (car tail) rev)
;                   finally (set! data (append half rev))))
;            (else
;             (err "~s is not an :elide value (#t #f :first :last)"
;		  elide)))
;      (values data len flag))))         

;;;
;;; line sticks on the last element.
;;;

(define-class <line> (<pattern>) 
  :name 'line)

(define-method (pattern-external-inits (obj <line>))
  (append 
   (list ':of (cons 'list
                    (loop for x in (car (pattern-data obj))
                          collect (expand-pattern-value x))))
   (next-method)))

(define-method (initialize (obj <line>) args)
  (next-method)
  (let ((cyc (make-cycl)))
    (cycl-data-set! cyc (pattern-data obj))
    ;; set the tail only this one time
    (cycl-tail-set! cyc (cycl-data cyc))
    (set! (pattern-data obj) cyc)
    (values)))

(define-method (next-in-pattern (obj <line>))
  (let ((cycl (pattern-data obj)))
    ;; if no cdr then car is last item
    (if (null? (cdr (cycl-tail cycl)))
      (car (cycl-tail cycl))
      (pop-cycl cycl))))

(define-method (reset-period (obj <line>))
  ;; was :before method
  (if (null? (cdr (cycl-tail (pattern-data obj)) ))
    (set! (pattern-length obj) 1))
  (next-method))

(define-method (map-pattern-data fn (obj <line>))
  (for-each fn (cycl-data (pattern-data obj))))

;;;
;;; heap is a cycle that shuffles its elements 
;;; each time through
;;;

(define-class <heap> (<cycle>)
  (random-state :init-thunk (lambda () *random-state*)
		:init-keyword :state :accessor pattern-random-state)
  :name 'heap)

(define-method (pattern-external-inits (obj <heap>))
  (let ((inits (next-method)))
    (if (eq? *random-state* (pattern-random-state obj))
      inits
      (append inits
              (list :state (pattern-random-state obj))))))

(define-method (initialize (obj <heap>) args)
  (next-method)
  (let ((cyc (pattern-data obj)))
    (cycl-data-set! cyc (list-copy (cycl-data cyc)))
    (values)))

(define-method (next-in-pattern (obj <heap>))
  (let ((cyc (pattern-data obj))
	(shufl
	 (lambda (lis len state)
	   (loop for i below len
		 for j = (random len state)
		 for v = (list-ref lis i)
		 do 
		 (list-set! lis i (list-ref lis j))
		 (list-set! lis j v))
	   lis)))
    (if (null? (cycl-tail cyc))
      (cycl-tail-set! cyc
		      (shufl (cycl-data cyc)
			     (pattern-length obj)
			     (pattern-random-state obj))))
    (pop-cycl cyc)))


;;;
;;; random chooses using weighted selection. its data are
;;; kept in a list of the form: ((&rest choices) . last-choice).
;;;

(define *random-range* #f)

(define-list-struct random-item
  datum index (weight 1) (min 1) max (count 0) id minmax)

(define-class <random> (<pattern>)
  (range :init-thunk (lambda () *random-range*)
	 :accessor random-pattern-range)
  (random-state :init-thunk (lambda () *random-state*)
		:init-keyword :state
		:accessor pattern-random-state)
  :name 'random)

(define-method (pattern-external-inits (obj <random>))
  (let ((lst (next-method))
        (fnc
         (lambda (n)
           (let ((dat (expand-pattern-value
                       (random-item-datum n)))
                 (lst (list)))
             ;; push in reverse order.
             (unless (null (random-item-max n))
               (push (expand-pattern-value (random-item-max n)) lst)
               (push ':max lst))
             (unless (or (not (numberp (random-item-min n)))
                         (= (random-item-min n) 1))
               (push (expand-pattern-value (random-item-min n)) lst)
               (push ':min lst))
             (unless (and (numberp (random-item-weight n))
                          (= (random-item-weight n) 1))
               (push (expand-pattern-value (random-item-weight n))
                     lst)
               
               (push ':weight lst))
             (if (null lst)
               dat (cons 'list (cons dat lst)))))))
    (append (list :of (cons 'list
                            (loop for x in (car (pattern-data obj))
                                  collect ( fnc x))))
            lst
            (if (eq? *random-state*
                     (pattern-random-state obj))
              (list)
              (list :state (pattern-random-state obj))))))


(define-method (default-period-length (obj <random>))
  ;; set the default period length of an all-subpattern random pattern
  ;; to 1 else to the number of elements. since a random pattern
  ;; establishes no particular order itself, setting the period to 1
  ;; allows the number of elements in the current period to reflect
  ;; the sub patterns. A better defaulting would be to check mixed
  ;; elements at run time and adjust the period length accordingly,
  (do ((tail (pattern-data obj) (cdr tail))
       (flag #t))
      ((or (not flag) (null? tail))
       (if flag 1 (pattern-length obj)))
    (set! flag (pattern? (random-item-datum (car tail))))))

(define-method (initialize (obj <random>) args)
  (next-method)
  (let ((pool (pattern-data obj))
        (sum (if (integer? *random-range*) 0 0.0))
        (const #t))
    (loop for item in pool
          for min = (random-item-min item)
          for max = (random-item-max item)
          unless (and (or (not min) (number? min))
                      (or (not max) (number? max)))
          do
	  (begin (set! const #f)
		 (random-item-min-set! item #f)
		 (random-item-max-set! item #f)
		 (random-item-minmax-set! item (cons min max))))
    (when const
      (set! (pattern-flags obj)
            (logior +constant-minmax+ (pattern-flags obj))))
    ;; check the stream for constant weights. if true, calculate the
    ;; range now and set a flag so we dont recalulate each period.
    (loop for item in pool
          for weight = (random-item-weight item)
	  while sum 
          if (number? weight)
          do (begin
	      (incf sum weight)
	      (random-item-index-set! item sum))
          else do (set! sum #f)
          finally 
          (when (and sum *random-range*)
            (dolist (item pool)
	      (random-item-index-set!
	       item
	       (* (/ (random-item-index item) sum)
		  *random-range*)))))
    (when sum
      (unless *random-range*
        (set! (random-pattern-range obj) sum))
      (set! (pattern-flags obj)
            (logior +constant-weights+ (pattern-flags obj))))
    ;; all routines treat pool as: ((&rest choices) . last-choice)
    ;; no initial last choice. a first choice for the stream could
    ;; be implemented as a last with min=1
    (set! (pattern-data obj) (list pool))))

(define-method (canonicalize-pattern-data (obj <random>)
					  data parser inits)
  inits
  (let ((parse-random-item
	 (lambda (extern)
           (apply 
            #'(lambda (datum . keys)
                (set! datum (maybeparse parser datum))
                (loop with orig = keys and args = '()
                      and key and val
                      while (not (null? keys))
                      do 
		      (set! key (pop keys))
                      (set! val (if keys (pop keys)
				    (err "Uneven random list: ~s." 
					 orig)))
                      (push val args)
                      (case key
                        ((weight :weight)
			 (push ':weight args)) 
                        ((min :min) 
			 (push ':min args)) 
                        ((max :max)
			 (push ':max args))
                        (else
                         (err "~s not one of: :weight, :min, :max."
                              key)))
                      finally 
                      (return
			(apply #'make-random-item :datum datum args))))
            (if (pair? extern)
	      extern
	      (list extern))))))
    (let ((intern (map #'parse-random-item data))) 
      (values intern
	      (length intern)
	      (not (some (lambda (x) (pattern? (random-item-datum x)))
			 intern))))))

(define-method (reset-period (obj <random>))
  (let ((reset (next-method))		; was :after method
	(flags (pattern-flags obj)))
    (unless (logtest flags +constant-minmax+)
      (let ((b #f))
	(dolist (i (car (pattern-data obj)))
	  (set! b (random-item-minmax i))
	  (when b
	    (random-item-min-set! i (next-1 (car b)))
	    (random-item-max-set! i (next-1 (cdr b)))))))
    (unless (logtest flags +constant-weights+)
      (loop with s = (if (integer? *random-range*) 0 0.0) 
	    for item in (car (pattern-data obj))
	    do
	    (incf s (next-1 (random-item-weight item)))
	    (random-item-index-set! item s)
	    finally
	    (if *random-range*
	      (dolist (i (car (pattern-data obj)))
		(random-item-index-set! i
					(* (/ (random-item-index i)
					      s)
					   *random-range*)))
	      (set! (random-pattern-range obj) s))))
    reset))

(define-method (next-in-pattern (obj <random>))
  ;; pool is ((&rest choices) . last-choice)
  (let* ((pool (pattern-data obj))
         (last (cdr pool)))
    (if (and (not (null? last))
	     (begin
	      (random-item-count-set! last
				      (+ 1 (random-item-count last)))
	      (<  (random-item-count last)
		  (random-item-min last))))
      (random-item-datum last)
      (let ((range (random-pattern-range obj))
            (state (pattern-random-state obj))
            (choices (car pool))
            (next #f))
        (set! next
	      (loop for item = (loop with index = (random range state)
				     for x in choices
				     when (< index (random-item-index x))
				     return x)
		    unless (and (random-item-max item)
				(= (random-item-count item)
				   (random-item-max item)))
		    return item))
        (unless (eq? next last)
          (dolist (i choices)
	    (random-item-count-set! i 0)))
        (set-cdr! pool next)
        (random-item-datum next)))))

(define-method (map-pattern-data fn (obj <random>))
  (for-each #'(lambda (x) ( fn (random-item-datum x))) ; funcall
	    (car (pattern-data obj))))

;;;
;;; markov 
;;;

(define-class <markov> (<pattern>)
  (past :init-value '() :init-keyword :past
	:accessor markov-pattern-past)
  (order :accessor markov-pattern-order)
  (produce :init-value #f :init-keyword :produce
           :accessor markov-pattern-produce)
  :name 'markov)

(define-method (pattern-external-inits (obj <markov>))
  ;; FIX --  past is not saved...
  (let ((fnc 
         (lambda (n)
           `(quote
             ,(append (first n)
                      (list '->)
                      (loop with s = 0
                            for x in (cddr n)
                            for w = (- (second x) s)
                            collect
                            (if (= w 1) 
                              (car x) (list (car x) w))
                            do (set! s (second x))))))))
    (append
     (list ':of (cons 'list
                      (loop for x in (cdr (pattern-data obj))
                            collect ( fnc x))))
     (next-method) 
     (if (not (markov-pattern-produce obj))
       (list)
       (list ':produce
             (expand-pattern-value
              (markov-pattern-produce obj)))))))

(define-method (canonicalize-pattern-data (obj <markov>) data parser
                                          inits)
  parser
  (let ((parse-markov-spec 
	 (lambda (spec)
           (let ((tail (or (member '-> spec)
                           (member ':-> spec)))
                 (range 0) 
                 (inputs '())
		 (parse '())
		 (outputs '())
                 )
             (if tail
               (begin (set! inputs (loop until (eq? spec tail)
				         collect (pop spec)))
		      (set! parse (cdr tail)))
               (begin (set! inputs '())
		      (set! parse spec)))
             (dolist (s parse)
               (let ((val #f)
                     (pat #f)
		     (wei #f))
                 (if (pair? s)
                   (begin (set! val (first s))
			  (set! wei (if (null? (cdr s)) 1 (second s)))
                          ;; weight may be number or pattern
                          (set! pat wei)
                          (unless (number? wei)
                            (set! wei #f)))
                   (begin (set! val s) (set! wei 1) (set! pat 1)))
                 ;; set range to #f if any weight is pattern
                 ;; else precalc range for the constant weights
                 (if (and wei range)
                   (incf range wei)
                   (set! range #f))
                 (push (list val range pat) outputs)))
             (cons inputs (cons range (reverse outputs)))))))
    (let ((const #t))
      (dopairs (a v inits)
        (case a
          ((:produce )
           (set! const (not (some #'pattern? v))))))
      (loop for s in data
            for p = (parse-markov-spec s)
            collect p into lis
            maximize (length (first p)) into order
            finally (begin (set! (markov-pattern-order obj) order)
                           (return
                            (values lis (length data)
                                    const)))))))

(define-method (initialize (obj <markov>) args)
  (next-method)				; was an :after method
  (unless (pair? (markov-pattern-past obj))
    (set! (markov-pattern-past obj)
          (make-list (markov-pattern-order obj) '*)))
  (values))

(define-method (next-in-pattern (obj <markov>))
  ;; markov data kept as a list of lists. each list is in the form:
  ;; ((<inputs>) range . <output>)
  (letrec ((select-output
            (lambda (range outputs)
              ;; if range is false then one or more weights in the
              ;; outputs are patterns. in this case we map all the
              ;; outputs to update weights of every outcome and then
              ;; select.  otherwise (range is number) we simply select
              ;; an outcome from the precalculated distribution.
              (if (not range)
                (do ((tail outputs (cdr tail))
                     (out #f)
                     (sum 0))
                    ((null? tail)
                     (select-output sum outputs))
                  ;; out is outcome: (val rng <pat/wei>)
                  (set! out (car tail))
                  ;; if third element is number use it else read it
                  (set! sum (+ sum (if (number? (caddr out))
                                     (caddr out)
                                     (next-1 (caddr out)))))
                  ;; always update second element to new value
                  (set-car! (cdr out) sum))
                (let ((n (random range)))
                  (loop for o in outputs
                     when (< n (second o) )
                     return (first o))))))
            (match-past
            (lambda (inputs past)
              (loop for i in inputs
                 for j in past
                 unless (or (eq? i '*) (equal? i j) (eq? j '*))
                 return #f
                 finally (return #t)))))
    (let ((past (markov-pattern-past obj))
          (data (markov-pattern-produce obj))
          (item #f))
      (loop for i in (pattern-data obj)
            when (or (null? past) (match-past (first i) past))
            do 
            (return
             (let ((last #f))
               (set! item (select-output (second i) (cddr i)))
               (unless (null? past)
                 (if (null? (cdr past))
                   (set-car! past item)
                   (begin
                    ;; rotate past choices leftward
                    (set! last (last-pair past))
                    (set-car! past item)
                    (set-cdr! last past)
                    (set! (markov-pattern-past obj) (cdr past))
                    (set-cdr! (cdr last) '()))))
               item))
            finally
	    (err "No outputs for past choices ~s." 
		 (markov-pattern-past obj)))
      (if data
        (let ((x (member item data)))
          (if x (second x) item))
        item))))

;;;
;;; graph traverses its nodes by applying a selection
;;; function to the graph. data is list:
;;; (<node> . nodes)
;;; where <node> is the last selected node initialized to (first nodes)
;;;

(define-list-struct graph-node id datum to props)

(define-class <graph> (<pattern>) 
  (selector :init-thunk (lambda () #'default-graph-node-select)
	    :init-keyword :selector :accessor graph-selector)
  (last :init-value #f :init-keyword :last
	:accessor graph-last)
  (props :init-value '() :init-keyword :props
	 :accessor graph-props)
  (starting-node-index :init-value 0 
                       :init-keyword :starting-node-index
                       :accessor graph-starting-node-index)
  :name 'graph)

(define-method (pattern-external-inits (obj <graph>))
  (let ((fnc
         (lambda (n)
           (cons 'list
                 (list (expand-pattern-value (graph-node-datum n))
                       ':id (graph-node-id n)
                       ':to (expand-pattern-value
                             (graph-node-to n)))))))
    (append
     (list ':of (cons 'list
                      (loop for x in (pattern-data obj)
                            collect ( fnc x))))
     (next-method )
     (if (null? (graph-props obj))
       (list)
       (list ':props (expand-pattern-value
                      (graph-props obj))))
     (if (eq? (graph-selector obj)
              #'default-graph-node-select)
       (list)
       (list ':selector (graph-selector obj))))))


(define-method (initialize (obj <graph>) args)
  (next-method) ; was :after method
  (let ((nodes (pattern-data obj))
        (last (graph-last obj)))
    (when last
      (set! (graph-last obj)
        (if (pair? last)
            (cons (length last) last)
          (cons last (make-list last '*)))))
    (set! (pattern-data obj)
	  (cons (list-ref nodes (graph-starting-node-index obj))
                nodes)))
  (values))

(define-method (canonicalize-pattern-data (obj <graph>) 
					  data parser inits)
  inits
  (let ((parse-graph-item 
	 (lambda (extern)
	   (unless (pair? extern) 
	     (err "Graph node ~s not list." extern))
	   (apply #'(lambda (datum . keys)
		      (loop with orig = keys and args = '()
			    and id and to and key and val
			    until (null? keys )
			    do
			    (set! key (pop keys))
			    (set! val (if (null? keys)
					(err "Bad graph node: ~s." orig)
					(pop keys)))
			    (push val args)
			    (case key
			      ((id :id)
			       (set! id #t)
			       (push ':id args)) 
			      ((to :to -> :->)
			       (set! to #t) 
			       (push ':to args))
			      ((props :props)
			       (push ':props args))
                              (else
                               (err "~s not one of: :id, :to, :props."
                                    key )))
			    finally 
			    (begin
			     (unless id
			       (push datum args)
			       (push ':id args))
                             to
			     ;(unless to
			     ;  (err "Missing :to in ~s." orig))
			     (return 
                              (apply #'make-graph-node
                                     :datum (maybeparse parser datum)
                                     args)))))
		  extern))))
    (let ((intern (map #'parse-graph-item data)))
      (values intern
              (length intern)
	      (not (some (lambda (x) (pattern? (graph-node-datum x)))
			 intern))))))

(define (default-graph-node-select obj node lastids)
  (let ((to (graph-node-to node)))
    (if (pair? to)
      (if (eq? (car to) ':idsel)
        (markov-select obj node to lastids)
	(err ":to not id, :idsel or pattern: ~s." to ))
      (next-1 to))))

(define (markov-select obj node table lastids)
  ;;  table is a list (:idsel <id1> <obj1> ...)
  obj
  (let ((prob (loop for tail on (cdr table) by #'cddr
                    when (match-ids (car tail) lastids)
                    return (cadr tail))))
    (unless prob
      (err "Node for ~s has no entry for ~s in ~s."
	   (graph-node-id node) lastids table))
    (next-1 prob)))

(define (match-ids user last)
  ;; match the user's ids with the past choices. * is a wildcard.
  ;; matching could get really fancy if we wanted it to.
  (cond ((null? user) #t)
        ((null? last) #f)
        ((pair? user)
         (and (match-ids (car user) (car last))
              (match-ids (cdr user) (cdr last))))
        ((eq? user last) #t)
        ((eq? user '*) #t)
        ((eq? last '*) #t)
        (else #f)))

(define-method (next-in-pattern (obj <graph>))
  (let* ((last (graph-last obj)) ; #f or (n . pasts)
         (graph (pattern-data obj))
         (nodes (cdr graph))
         (this (car graph))
	 (next #f))

    (set! next ( (graph-selector obj)	; funcall
		 obj this (if last (cdr last))))
    (if next
      (let ((node (find next nodes :key #'graph-node-id)))
	(if node 
	  (begin
	   ;; next item becomes selected node. car of last is
	   ;; the number of choices to remember.  push old 
	   ;; selection onto the list and flush the the oldest
	   ;; element. since we cant setf nthcdrin some lisps,
	   ;; we setf the cdr of nthcdr-1...
	   (set-car! graph node)
	   (when last
	     (set-cdr! last (cons (graph-node-id this) (cdr last)))
	     (set-cdr! (list-tail (cdr last) (1- (car last)))
		       '())))
	  (err "No node for id ~s." next)))
      (err "No next node from ~s." (graph-node-id this)))
    (graph-node-datum this)))

(define-method (map-pattern-data fn (obj <graph>))
  (for-each #'(lambda (x) ( fn (graph-node-datum x))) ; funcall
	    (pattern-data obj)))

;;;
;;; accumulation adds the current item to the set of items
;;; selected so far: A A B A B C | A A B A B C 
;;;

(define-class <accumulation> (<pattern>)
  (indices :init-thunk (lambda () (cons 0 0))
	   :accessor accumulation-indicies)
  :name 'accumulation)

(define-method (next-in-pattern (obj <accumulation>))
  (let ((indices (accumulation-indicies obj)))
    (let ((val (list-ref (pattern-data obj) (car indices))))
      (if (= (car indices) (cdr indices))
	(begin (set-car! indices 0)
	       (set-cdr! indices
			 (modulo (+ 1 (cdr indices)) 
				 (pattern-length obj))))
	(set-car! indices (+ 1 (car indices))))
      val)))

(define-method (map-pattern-data fn (obj <accumulation>))
  (for-each fn (pattern-data obj)))

(define-method (default-period-length (obj <accumulation>))
  (let ((len (pattern-length obj)))
    (loop for i from 1 to len sum i)))

;;;
;;; funcall calls a function to return the items
;;; constituting the data for the next period.
;;;

(define-class <funcall> (<pattern>)
  :name 'funcall)

(define-method (pattern-external-inits (obj <funcall>))
  (append (list ':of
                (expand-pattern-value (car (pattern-data obj))))
          (next-method)))

(define-method (default-period-length (obj <funcall>)) 1)

(define-method (initialize (obj <funcall>) args)
  (next-method) ; was an :after method
  (let ((data (pattern-data obj)))
    (unless (and (pair? data)
                 (procedure? (car data)))
      (err "Funcall not function: ~s." data))
    (values)))

(define-method (next-in-pattern (obj <funcall>))
  (let ((data (pattern-data obj)))
    (when (null? (cdr data))
      (let ((vals ( (car data) ))	; funcall
	    (len #f))
        (cond ((null? vals)
	       (set! vals (list +nad+)))
	      ((not (pair? vals))
	       (set! vals (list vals))))
        (set! len (length vals))
	(set-cdr! data vals)
        (set! (pattern-length obj) len)
        ;; if period was not specified then set the period
        ;; to length of data, otherwise leave alone
	(when (logtest (pattern-flags obj) +default-period+)
          (let ((p (pattern-period obj)))
	    (period-count-set! p len)
	    (period-length-set! p len)))))
    (cdr-pop data)))

;(define x (new funcall :of (lambda () (list 1 2 3))))
;(next x t)
;(define x (new funcall :of (lambda () (list 1 2 3)) :for 5))
;(next x t)

(define-method (map-pattern-data fn (obj <funcall>))
  (for-each fn (cdr (pattern-data obj)))) ; funcall

;;;
;;; rotation
;;;

(define-class <rotation> (<cycle>)
  (change :init-value 0 :init-keyword :rotations
	  :accessor rotation-change)
  :name 'rotation)

(define-method (pattern-external-inits (obj <rotation>))
  (append
   (list ':of (cons 'list (loop for x in (car (pattern-data obj))
                                collect (expand-pattern-value x))))
   (if (equal (rotation-change obj) 0)
     (list)
     (list ':rotations
           (expand-pattern-value (rotation-change obj))))))

(define-method (initialize (obj <rotation>) args)
  (next-method) ; was an :after method
  ;; pattern is initialized now so that rotations only happen
  ;; after the first cycle.
  (let ((data (pattern-data obj)))
    (set-cdr! data (car data))))

(define-method (next-in-pattern (obj <rotation>))
  (let ((ring (pattern-data obj)))
    (when (null? (cdr ring))
      (let ((change (next-1 (rotation-change obj)))
            (start #f)
	    (step #f)
	    (width #f)
	    (end #f))
        (if (pair? change)
          (begin (set! start (pop change))
		 (set! step (if (null? change) #f
				(pop change)))
		 (set! width (if (null? change) #f
				 (pop change)))
		 (set! end (if (null? change) #f
			       (pop change))))
          (set! start change))
        (unless start (set! start 0))
        (unless step (set! step 1))
        (unless width (set! width 1))
        (unless end (set! end (- (pattern-length obj) width)))
	(set-cdr! ring
		  (rotate-items (car ring) start
				end step width))))
    (cdr-pop ring)))

(define (rotate-items items start end step width)
  (loop for i from start below end by step 
	for a = (list-ref items i)
	for b = (list-ref items (+ i width))
	do
	(list-set! items i b)
	(list-set! items (+ i width) a))
  items)

;; (define a (new rotation of (list 1 2 3 4)))
;; (next a #t)

;;;
;;; rewrite
;;;

; (define a (new rewrite of '((a -> (b)) (b -> (b a a)))))

(define-list-struct rewrite-node
  datum id to props)

(define-list-struct rewrite-rule
  trigger
  (successors '())
  context)

(define-class <rewrite> (<pattern>)
  (table :init-value #f :init-keyword :initially
	 :accessor rewrite-table)
  (rules :init-value '() :init-keyword :rules
	 :accessor rewrite-rules)
  (generations :init-thunk (lambda () most-positive-fixnum)
	       :init-keyword :generations
	       :accessor rewrite-generations)
  :name 'rewrite)

(define-method (pattern-external-inits (obj <rewrite>))
  (let ((fnc
         (lambda (n)
           (cons 'list
                 (append
                  (list (expand-pattern-value (rewrite-node-datum n)))
                  (if (eq? (rewrite-node-datum n) 
                           (rewrite-node-id n)) 
                    (list)
                    (list ':id (rewrite-node-id n)))
                  (list ':to
                        `(quote
                          ,(loop for x in (rewrite-node-to n)
                                 collect
                                 (if (list? x) (rewrite-node-id x)
                                     x)))))))))
    
    (append (list
             ':of (cons 'list
                        (hash-fold (lambda (k v l)
                                     k
                                     (cons ( fnc v) l))
                                   '()
                                   (rewrite-table obj))))
            (next-method) )))

(define-method (initialize (obj <rewrite>) args)
  (next-method)				; was an :after method
  (let ((table (make-hash-table 103))
        (nodes (pattern-data obj))
        (rules (rewrite-rules obj))
        (preset #f))

    ;; :initially initarg uses table slot to avoid additional slot
    (set! preset (or (rewrite-table obj)
                     (list (rewrite-node-id (first nodes)))))

    ;; enter each node in hashtable by id.
    (dolist (n nodes)
      (hash-set! table (rewrite-node-id n) n))

    ;; as a runtime opimization we prefetch constant TO ids. this saves
    ;; a call to item and a hash lookup each rewrite. if not constant
    ;; (a pattern) its stored in the props field, which is otherwise 
    ;; unused by the pattern and the ids are looked up each rewrite.
    (dolist (n nodes)
      (let ((x (rewrite-node-to n)))
        (if (pattern? x)
          (begin (rewrite-node-props-set! n (rewrite-node-to n))
		 (rewrite-node-to-set! n #f))
          (begin (rewrite-node-to-set! n
				       (lookup-successors x table))))))

    ;; set max generations.
    (let ((count (rewrite-generations obj)))
      (set! (rewrite-generations obj)
	    (cons 1 (if (> count 1) count
			(err "Generations: ~s not > 1." count)))))

    ;; preset 1st generation. defaults to first node.
    (set! nodes 
          (loop for id in (if (list? preset) preset (list preset))
                collect
                (or (hash-ref table id)
                    (err "Id ~s not in rewrite nodes." id))))
    (set! (rewrite-table obj) table)
    (set! (pattern-data obj) (cons nodes nodes))
    (unless (null? rules)
      (set! (rewrite-rules obj) 
            (parse-rules rules table)))
    (values)))

(define-method (canonicalize-pattern-data (obj <rewrite>) 
					  data parser inits)
  inits
  (let ((parse-rewrite-node
	 (lambda (extern)
           (let ((datum #f)
		 (keys '()))
             (if (pair? extern)
               (begin (set! datum (car extern))
		      (set! keys (cdr extern)))
               (set! datum extern))
             (loop with args = '() and id and key and val
                   until (null? keys)
		   do
                   (set! key (pop keys))
                   (set! val  (if (null? keys)
				(err "Not a rewrite spec: ~S." extern)
				(pop keys)))
                   (push val args)
                   (case key
                     ((id :id)
		      (set! id val)
		      (push ':id args)) 
                     ((to -> :to :->)
		      (push ':to args))
                     (else
                      (err "~s not one of rewrite keywords: id ->." key)))
                   finally 
                   (begin
                     (unless id
		       (push datum args)
		       (push ':id args))
                     (return
                      (apply #'make-rewrite-node 
                             :datum (maybeparse parser datum)
                             args))))))))
    (let ((intern (map #'parse-rewrite-node data)))
      (values intern
	      (length intern)
              (not (some (lambda (x) (pattern? (rewrite-node-datum x)))
			 intern))))))

(define-method (map-pattern-data fn (obj <rewrite>))
  (for-each #'(lambda (x) ( fn (rewrite-node-datum x))) ; funcall
	    (car (pattern-data obj))))

(define (parse-rules rules table)
  (letrec ((getnode
	    (lambda (id table rule)
	      (or (hash-ref table id)
		  (err "No node for id ~s in rule ~s." 
                       id rule))))
           (getnodes
	    (lambda (ids table rule)
	      (loop for id in ids 
		    collect (getnode id table rule)))))
    (loop for rule in rules
          collect
          (loop with form = rule and left = #t and x
                while (not (null? form))
                do 
		(set! x (pop form))
                if (or (eq? x '->) (eq? x ':->))
		do
		(set! left #f)
                else if left collect x into lh
                else collect x into rh
                finally
                (return
		  (let ((len (length lh)))
		    (unless (not left)
		      (err "Missing -> in rule ~s." rule))
		    (cond ((= len 0)
			   (err "Missing left hand side in ~s." rule))
			  ((= len 1)
                           (make-rewrite-rule
                            :successors (getnodes rh table rule)
                            :trigger (getnode (if (pair? (car lh)) 
                                                (caar lh) (car lh))
                                              table rule)))
			  (else
			   (loop for x in lh 
				 for i from 0
				 when (pair? x)
				 do
				 (set! (elt lh i) (car x)) ; remove ()
				 (return 
				   (make-rewrite-rule
				    :successors (getnodes rh table rule)
				    :trigger (getnode (car x) table rule)
				    :context
				    (cons (cons i len) 
					  (getnodes lh table rule))))
				 finally
				 (err "No trigger in lh side of ~s."
				      rule))))))))))

(define (lookup-successors successor table)
  ;; get successor nodes. successor may be an id, list of ids or nil.
  (if (pair? successor)
    (loop for to in successor
          collect
          (or (hash-ref table to)
              (err "No rewrite node for id ~s." to)))
    (if successor
      (list (or (hash-ref table successor)
                (err "No rewrite node for ~s." successor)))
      #f)))

(define-method (next-in-pattern (obj <rewrite>))
  (let ((nodes (pattern-data obj)))
    (when (null? (cdr nodes))
      (let ((count (rewrite-generations obj)))
        (if (< (car count) (cdr count))
;; 	  (let* ((rules (rewrite-rules obj))
;; 		 (old (car nodes))
;; 		 (new (if (null? rules )
;; 			(node-rewrite old (rewrite-table obj))
;; 			(rule-rewrite old rules))))
;; 	    (unless new
;; 	      (err "Empty rewrite for generation ~s." old))
;; 	    (begin (set-cdr! nodes new)
;; 		   (set-car! nodes new))
;; 	    (set-car! count (+ (car count) 1)))
          (rewrite-generation obj #t #f)
	  (set-cdr! nodes (car nodes)))))
    (rewrite-node-datum (cdr-pop nodes))))

; (define a (new rewrite of '((a -> (b)) (b -> (b a a)))))
; (next a 20)

(define (rewrite-generation obj . args)
  (with-args (args &optional (new #f) (ids #t))
    (let ((data (pattern-data obj)))
      (if (not new)
        (if (not ids) (car data) (mapcar #'rewrite-node-datum (car data)))
        (let* ((nodes (pattern-data obj))
               (rules (rewrite-rules obj))
               (old (car nodes))
               (new (if (null? rules)
                      (node-rewrite old (rewrite-table obj))
                      (rule-rewrite old rules))))
          (unless new
            (err "Rewrite: generation #~D is empty!"
                 (+ (car (rewrite-generations obj)) 1)))
          (begin (set-cdr! nodes new)
                 (set-car! nodes new))
          (if (not ids) 
            (car nodes)
            (mapcar #'rewrite-node-datum (car data))))))))


(define (node-rewrite gen table)
  ;; nodes specify their rewrites directly just fetch successors
  (loop for node in gen
        for next = (let ((to (rewrite-node-to node)))
		     (if to (copy-list to) 
			 (lookup-successors 
			  (next-1 (rewrite-node-props node))
			  table)))
        nconc next))

(define (rule-rewrite generation rules)
  (loop with context and length
        for index from 0
        for node in generation
        append
	(loop for rule in rules
	      do
	      (when (eq? node (rewrite-rule-trigger rule))
		(set! context (rewrite-rule-context rule))
		(if context ;; IS THIS A LIST??
		  (let* ((seq (cdr context))
			 (beg (- index (caar context)))
			 (end (+ beg (cdar context))))
		    (when (and (<= 0 beg end
				   (or length
				       (begin
					(set! length
					      (length generation))
					length)
				       ))
			       (not (mismatch seq generation :start2 beg
					      :end2 end
					      :test #'(lambda (a b) 
							(or (eq a '*)
							    (eq a b))))))

		      (return (rewrite-rule-successors rule))))
		  (return (rewrite-rule-successors rule))))
              finally (return (list))
              )))

;;;
;;; RANGE
;;;

(defmacro %range-stepping? (flags)
 `(logtest ,flags +range-stepping+) )
(defmacro %range-unbounded? (flags)
  `(logtest ,flags +range-unbounded+))
(defmacro %range-initially? (flags)
  `(logtest ,flags +range-initially+))
(defmacro %range-dynamic? (flags)
  `(logtest ,flags +range-dynamic+))
(defmacro %range-random? (flags)
  `(logtest ,flags +range-random+))

(define-class <range> (<pattern>)
  (from :init-value #f :init-keyword :from
        :init-keyword :initially
	:accessor range-from)
  (to :init-value #f :init-keyword :to 
      :init-keyword :below :init-keyword :pickto
      :accessor range-to)
  (downto :init-value #f :init-keyword :downto
	  :init-keyword :above :accessor range-downto)
  (by :init-value #f :init-keyword :by 
      :init-keyword :stepping :accessor range-by)
  (incf :init-value #f :accessor range-incf)
  (test :init-value #f :accessor range-test)
  :name 'range)

(define-method (pattern-external-inits (obj <range>))
  ;; from to downto above below by pickto
  (let ((bits (pattern-flags obj)))
    (append
     (list (if (%range-initially? bits)
             ':initially ':from)
           (expand-pattern-value (range-from obj)))
     (if (range-to obj)
       (list (if (%range-random? bits) 
               ':pickto
               (if (funcall (range-test obj) 2 0 2)
                 ':below ':to))
             (expand-pattern-value 
              (range-to obj)))
       (if (range-downto obj)
         (list (if (funcall (range-test obj) 0 0 2)
                 ':above :downto)
               (expand-pattern-value 
                (range-downto obj)))
         (list)))
     (list (if (%range-stepping? bits)
             ':stepping ':by)
           (expand-pattern-value (range-by obj)))
     (next-method))))

(define-method (initialize (obj <range>) args)
  (next-method) ; was an :after method
  (let* ((raw (pattern-data obj))
	 (data (car raw))
	 (init (cadr raw))
	 (flag #f)
	 (test #f)
	 (bits #f))

    ;; data (inc from val min max) 
    ;; init (<from> <to> <down> <by> <incf> <flags> <bits>))
    (set! (pattern-data obj) data)
    (set! (range-from obj) (list-ref init 0))
    (set! (range-to obj) (list-ref init 1))
    (set! (range-downto obj) (list-ref init 2))
    (set! (range-by obj) (list-ref init 3))
    (set! (range-incf obj) (list-ref init 4))
    (set! flag (list-ref init 5))
    (set! bits (list-ref init 6))
    
    (case flag
      ((#b00001 )			; to 
       (set! test #'(lambda (x min max) min (> x max)))) 
      ((#b00010 )			; below
       (set! test #'(lambda (x min max) min (>= x max))))
      ((#b00100 )			; downto
       (set! test #'(lambda (x min max) max (< x min))))
      ((#b01000 )			; above
       (set! test #'(lambda (x min max) max (<= x min))))
      ((#b00101 )			; downto & to
       (set! test #'(lambda (x min max) (or (< x min) (> x max)))))
      ((#b00110 )			; downto & >below
       (set! test #'(lambda (x min max) (or (< x min) (>= x max)))))
      ((#b01001 )			; above & to
       (set! test #'(lambda (x min max) (or (<= x min) (> x max)))))

      ((#b01010 )			; above<->below
       (set! test #'(lambda (x min max) (or (<= x min) (>= x max)))))
      ((#b10000)                        ; within
       #f )
      ((0 ) 
       (set! bits (logior bits +range-unbounded+)))
      (else
       (err "Not a range specification: ~s." args)))
    
    (when (and (%range-initially? bits)
               (not (%range-unbounded? bits)))
      (error ":initially excludes lower or upper bound."))
    
    ;; if bounds were specified without explicit period
    ;; then range period is dynamic
    (when (eq? (period-length (pattern-period obj))
               most-positive-fixnum)
      (set! bits (logior bits +range-dynamic+)))
    
    ;; add in range bit flags
    (set! (pattern-flags obj) (logior (pattern-flags obj) bits))
    (set! (range-test obj) test)))

(define-method (canonicalize-pattern-data (obj <range>) 
					  data parser inits)
  ;; this is called BEFORE range's initialize-instance method
  data parser
  (let ((from 0)
	(to #f)
	(downto #f)
	(by 1)
	(incf #'+)
        (flag 0)
        (bits 0)
	(const? #t))
    (dopairs (a v inits)
             (case a
	       ((:from )
	        (set! from v))
	       ((:initially )
	        (set! from v)
                (set! bits (logior bits +range-initially+)))
               ((:to )
	        (set! to v)
                (set! flag (logior flag #b00001))
                (set! incf #'+))
               ((:below ) 
	        (set! to v)
                (set! flag (logior flag #b00010))
                (set! incf #'+))
               ((:downto )
	        (set! downto v)
                (set! flag (logior flag #b00100))
                (set! incf #'-))
               ((:above )
	        (set! downto v)
                (set! flag (logior flag #b01000))
                (set! incf #'-))
               ((:pickto ) 
	        (set! to v)
                (set! bits (logior bits +range-random+))
                (set! flag (logior flag #b10000))
                (set! incf #'between))
               
               ((:by )
                (set! by v))
               ((:stepping )
                (set! by v)
                (set! bits (logior bits +range-stepping+)))))
    
    (if (or (pattern? from)
	    (pattern? to)
	    (pattern? downto)
	    (pattern? by))
      (set! const? #f))
    
    ;; data ((inc from val min max) 
    ;;       (<from> <to> <down> <by> <flags> <bits> <incf>))
    ;; initialize-instance sets inc to NIL if stepping 
    (values (if const?
              (list (list by from #f downto to )
		    (list from to downto by incf flag bits))
              (list (list #f #f #f #f #f)
		    (list from to downto by incf flag bits)))
	    (if (or to downto) most-positive-fixnum 1)
            const?)))

(define (reset-range? data test)
  ;; data is (inc from val min max)
  (or (not (third data)) ; no current FROM
      (and test (apply test (cddr data)))))

(define-method (next-in-pattern (obj <range>))
  (let ((bits (pattern-flags obj))
        (data (pattern-data obj))
        (test (range-test obj))
        (from #f))
    (when (reset-range? data test)
      (if (logtest bits +constant-data+)
        (begin
         ;; DATA IS (INC FROM VAL MIN MAX)
         (unless (and (%range-initially? bits)
                      (third data))
           ;;(set! (third data) (second data))
           (set-car! (cddr data) (cadr data))))
        (begin
         (unless (%range-stepping? bits)
           ;; set first element
           (set-car! data (next-1 (range-by obj))))
         ;; set second element
         (set-car! (cdr data) (next-1 (range-from obj)))
         (unless (and (%range-initially? bits)
                      (third data))
           ;; set third element
           (set-car! (cddr data) (cadr data)))
         ;; set fourth
         (set-car! (cdddr data) (next-1 (range-downto obj)))
         ;; set fifth
         (set-car! (cddddr data) (next-1 (range-to obj))))))

    (if (%range-random? bits)
      (begin
       (set! from ( (range-incf obj)      ; funcall
                    (second data)
                    (car (cddddr data)))) ; 5th elt is TO
       (set-car! (cddr data) from))
      (begin
       (set! from (third data))
       (set-car! (cddr data)
                 ( (range-incf obj)	; funcall
                   from
                   (if (%range-stepping? bits) 
                     (next-1 (range-by obj))
                     (first data))))))
    
    ;; signal EOP if dynamic and range will reset next time.
    (if (and (%range-dynamic? bits)
             (reset-range? data test))
      (period-count-set! (pattern-period obj) 1))
    from))

(define-method (reset-period (obj <range>))
  (let ((val (next-method))) ; was an :after method
    ;; reset the bounds if not dynamic
    (let ((bits (pattern-flags obj)))
      (if (and (%range-unbounded? bits)
	       (not (%range-initially? bits)))
	(set-car! (cddr (pattern-data obj)) #f)))
    val))

; (define x (new range from 1))              ; only returns 1
; (define x (new range initially 1))         ; never reinitialized
; (define x (new range from 1 for 5))        ; reset after 5
; (define x (new range initially 1 for 5))   ; never reset
; (define x (new range from 1 to 10))        ; reset at limit
; (define x (new range from 1 to 10 for 5))  ; reset at limit


; (define x (new range initially 0 by -2 for 10))
; (describe-object x)
; (next x t)

; (define x (new range from 1 to 10))
; (next x t)
; (describe-object x)

; (define x (new range from 1 downto -4))
; (next x t)
; (describe-object x)

; (define x (new range from 1 above -4))
; (next x t)
; (describe-object x)

; (define x (new range from 20 
;              to 30 downto 10
;              stepping (new random -1 1 -2 2)))
; (describe-object x)
; (next x)

; (define x (new range from (new cycle 1 5) to 10 for 5))
; (next x t)
; (describe-object x)
; (next x)

; (define x (new range :from -09 :pickto 10 :for 5))
; (describe x)
; (%range-random? (pattern-flags x))
; (next x t)

;;;
;;; transposer methods
;;; WHY ISNT THIS A SUBCLASS OF PATTERN??

(define-class <transposer> (<container>)
  (of :init-keyword :of :accessor transposer-of)
  (by :accessor transposer-by :init-keyword :by
      :init-keyword :stepping :init-keyword :on)
  (form :accessor transposer-form :init-value #f
	:init-keyword :form)
  (mod :accessor transposer-mod :init-value #f 
       :init-keyword :mod)
  (scale :accessor transposer-scale 
	 :init-thunk (lambda () *scale*)
	 :init-keyword :scale)
  :name 'transposer)

;; REMOVE at some ppoint..

(define-method (pattern? (obj <transposer>)) obj)

(define-method (make-load-form (obj <transposer>))
  `(make-instance <transposer>
     ,@(append
        (list ':of (expand-pattern-value (transposer-of obj)))
        (let ((by (transposer-by obj)))
          (if (third by)
            (list ':stepping (expand-pattern-value
                              (second by)))
            (list ':by (if (second by)
                         (expand-pattern-value
                          (second by))
                         (first by)))))
        (if (not (transposer-form obj))
          (list)
          (list ':form (expand-pattern-value
                        (transposer-form obj))))
        (if (not (transposer-mod obj))
          (list)
          (list ':mod (expand-pattern-value
                       (transposer-mod obj))))
        (if (eq? (transposer-scale obj)
                 *scale*)
          (list)
          (list ':scale
                `(find-object
                  (quote ,(object-name (transposer-scale obj)))))))))

(define-method (initialize (obj <transposer>) args)
  (next-method) ; was :after method
  (let ((data #f)
	(stepping? #f))
    
    ;; parse args, check for :stepping and :on aliases
    (dopairs (s v args)
      (case s
        ((:stepping )
         (when data
           (err "Duplicate ':stepping', ':by' or ':on' in: ~s"
                args))
         (set! stepping? #t)
         (set! data v))
        ((:by :on )
         (when data
           (err "Duplicate ':stepping', ':by' or ':on' in: ~s"
                args))
         (set! data v))))
    (unless data
      (err "Missing ':stepping', ':by' or ':on' in: ~s"
           args) )
    
    ;; by is (<value> <pattern> <stepping?>)
     (set! (transposer-by obj)
          (if stepping?
            (list #f data #t)
            (if (pattern? data)
              (list #f data #f)
              (list data #f #f))))
    
    (unless (slot-bound? obj 'of)
      (err "Missing ':of' data."))
    
    (set! data (transposer-form obj))
    (when data
      ;; form if pattern: (<value> <pattern>)
      ;; otherwise (<value>
      (set! (transposer-form obj)
            (if (symbol? data)
              (list data #f)
              (list #f data))))))

(define-method (eop? (obj <transposer>))
  (eop? (transposer-of obj)))

;; THis was next-1 !
(define-method (next-1 (obj <transposer>))
  (let* ((by (transposer-by obj))
	 (step? (third by))
	 (scale (transposer-scale obj))
	 (form (transposer-form obj))
	 (data #f)
	 (flag #f))
    
    ;; by is  (<value> <source>  <stepping?>)
    ;; <value> is #f if it needs to be updated
    ;; from <source>. if value is read also 
    ;; read new form.
    (when (not (first by))
      (set-car! by (next-1 (second by)))
      (when (and form (second form))
	(set-car! form (next-1 (second form)))))
    
    ;; get data to transpose
    (set! data (next-1 (transposer-of obj) ))
    (set! flag (eop? (transposer-of obj))) ; is this right?
    ;; tranpose by current offset
    (set! data (transpose data (first by) scale))
    ;; twiddle transpositon
    (when form
      (case (first form)
        ((r :r retrograde :retrograde)
         (set! data (reverse data)))
        ((p :p prime :prime))
        ((i :i inversion :inversion)
         (set! data (invert data)))
        ((ri :ri retrograde-inversion :retrograde-inversion)
         (set! data (reverse (invert data))))
        (else
         (err "Not a transposition form: ~s." form))))
    (when (transposer-mod obj)
      (set! data (scale-mod data (transposer-mod obj)
			    :offset (first data)
			    :in (transposer-scale obj))))
    ;; if current data is eop or if stepping
    ;; then force new BY selection next time.
    (if (and (or flag step?) (second by))
      (set-car! by #f))
    data))

;;;
;;; chord
;;;

(define-class <chord> (<pattern>)
  :name 'chord)

(define-method (pattern-external-inits (obj <chord>))
  (append
   (list ':of
         (if (null? (cdr (pattern-data obj)))
           (expand-pattern-value obj)
           (cons 'list (loop for x in (pattern-data obj)
                             collect
                             (expand-pattern-value x)))))
   (next-method)))

(define-method (canonicalize-pattern-data (pat <chord>) 
					  data parser inits)
  inits
  (if parser
    (loop for datum in data 
          count datum into length
          count (pattern? datum) into streams
          collect (maybeparse parser datum) into list
          finally (return (values list 1 (= streams 0))))
    (values data 1 (not (some #'pattern? data)))))

(define-method (default-period-length (obj <chord>))
  1)

(define-method (next-in-pattern (obj <chord>))
  (let ((data (pattern-data obj)))
    (if (logtest (pattern-flags obj) +constant-data+)
      data
      (if (null? (cdr data))
	(next (first data) #t)
	(apply #'append (map #'(lambda (x) (next x #t)) data))
	))))

;;;
;;; pval holds a lisp form to be evaluated in a pattern.
;;; (pval x)
;;;

(define-class <pval> ()
  (thunk :init-keyword :of :accessor pval-thunk)
  :name 'pval)

(define-method (make-load-form (obj <pval>))
  `(pval ,(pval-thunk obj)))

(define-method (pattern? (obj <pval>)) obj)

(defmacro pval (expr)
  `(make-instance <pval> :of (lambda () ,expr)))

(define-method (next-1 (obj <pval>))
  ( (pval-thunk obj) )) ; funcall

;;;
;;; Join (defmultiple-item)
;;;

(define-class <join> (<pattern>)
  (format :accessor join-format :init-keyword :format
          :init-value #f)
  (cache :accessor join-cache :init-value #f)
  :name 'join)

(define-method (pattern-external-inits (obj <join>))
  (append (list ':of (cons 'list
                           (loop for x in (pattern-data obj)
                                 collect (expand-pattern-value x))))
          (list ':format (expand-pattern-value 
                          (join-format obj)))
          (next-method)))

(define-method (initialize (obj <join>) args)
  (next-method ) ; was :after
  (let* ((per (pattern-period obj))
         (len (length (pattern-data obj)))
         (data (pattern-data obj))
         (fmat (join-format obj))
         (ppat #f))  ; patterns read once per period
    
    ;; do i still need to do this?
    (period-stream-set! per (period-default per))
    
    (cond ((not fmat)
           (set! fmat (make-list len ':eop))
           (set! (join-format obj) fmat))
          ((not (list? fmat))
           (err "Expected :format list but got ~s instead." fmat))
          ((not (= (length fmat) len))
           (err ":format list has ~s elements but length of data is ~s."
                (length fmat) len)))
    ;; if user specified a format list each element is:
    ;;   :eop - datum marks eop
    ;;   #t   - datum read each time
    ;;   #f   - datum read each period
    ;; set each element in format to either:
    ;;   :eop  - datum is pattern and sets eop
    ;;   :each - datum is pattern read each time
    ;;   :once - datum is pattern read each period
    ;;   #f    - datum is not pattern (next-1 never called)
    (dotimes (i len)
      (case (list-ref fmat i)
        ((:eop ) 
         (if (pattern? (list-ref data i))
           #f
           (list-set! fmat i #f)))
        ((#t )
         ;; user says read each time
         (if (pattern? (list-ref data i))
           (list-set! fmat i ':each)
           (list-set! fmat i #f)))
        ((#f )
         ;; user says read each period
         (if (pattern? (list-ref data i))
           (begin (set! ppat #t)
                  (list-set! fmat i ':once))
           (list-set! fmat i #f)))
        (else
         (err "Bad :format value ~s: not :eop, ~s or ~s"
              (list-ref fmat i) #t #f))))
    (set! (join-format obj) fmat)
    
    (when ppat
      ;; cache patterns read once per period
      ;; elements either #f or pattern to read
      (set! ppat (make-list len #f))
      (dotimes (i len)
        (if (eq? (list-ref fmat i) ':once)
          (begin (list-set! ppat i (list-ref data i))
                 (list-set! data i #f))))
      (set! (join-cache obj) ppat))
    (values)))

(define-method (canonicalize-pattern-data (obj <join>) data parser inits)
  inits
  (let ((subs (not (some #'pattern? data))))
    (if parser
      (loop for datum in data
            count datum into length
            collect (maybeparse parser datum) into list
            finally
            (return (values list 
                            (if subs 1 most-positive-fixnum)
                            (not subs))))
      (values data (if subs 1 most-positive-fixnum) (not subs)))))

(define-method (reset-period (obj <join>))
  ;; update data from patterns that get read once per period.
  (let ((val (next-method)))            ; was an :after method
    (let ((c (join-cache obj)))
      (when c
        ;; read patterns marked once per period
        ;; each element in c either #f or pattern
        (let ((d (pattern-data obj))
              (i 0))
          (dolist (x c)
            (if x (list-set! d i (next-1 x)))
            (set! i (+ i 1))))))
    val))

(define (join-eop? data fmat)
  ;; join at eop if every pattern marked :eop is at eop.
  (do ((end? #t))
      ((null? data) end?)
    (if (eq? (car fmat) ':eop)
      (if (eop? (car data))
        #f
        (set! end? #f)))
    (set! data (cdr data))
    (set! fmat (cdr fmat))))

(define-method (next-in-pattern (obj <join>))
  (let ((next (list #f))
        (data (pattern-data obj))
        (fmat (join-format obj) ))
    (do ((l1 data (cdr l1))
         (l2 fmat (cdr l2))
         (l3 next (cdr l3)))
        ((null? l1) #f)
      (if (not (car l2)) ; not a pattern
        (set-cdr! l3 (list (car l1)))
        (set-cdr! l3 (list (next-1 (car l1))))))
    (when (join-eop? data fmat)
      (period-count-set! (pattern-period obj) 1))
    (cdr next)))

; (setf x (new join of (list (new cycle of '(a b)) 2)))
;
; (setf x (new cycle 
;          of (list (new join of (list (new cycle of '(a b)) 1))
;                   (new join of (list (new cycle of '(c d)) 2)))))
             
; (next x)


;;;
;;; copier return multiple copies of a pattern's periods
;;;

(define-class <copier> (<pattern>)
  (source :accessor copier-source)
  (repfor :accessor copier-repfor :init-value #f
          :init-keyword :repeat-for)
  :name 'copier)

(define-method (pattern-external-inits (obj <copier>))
  (append (list ':of (expand-pattern-value (copier-source obj)))
          (next-method)))

(define-method (initialize (obj <copier>) args)
  (next-method)
  (let ((data (pattern-data obj)))
    (set! (copier-source obj) (car data))
    (set! (pattern-data obj) (list))
    (values)))

(define-method (next-in-pattern (obj <copier>))
  (let ((data (pattern-data obj)))
    (if (null? data)
      (let* ((per (pattern-period obj))
             (res (next (copier-source obj) #t))
             (len (length res))
             (for (period-length per))
             (rep (copier-repfor obj)))
        (if rep
          (begin
            (set! for (next rep))
            (period-length-set! per len)
            (period-count-set! per len))
          (period-count-set! per (* len for)))
        (let ((sav res))
          (dotimes (i (- for 1))
            (set! res (append res (list-copy sav)))))
        (set! (pattern-data obj) (cdr res))
        (car res))
      (begin
       (set! (pattern-data obj) (cdr data))
       (car data)))))

;(setq x (new copier :of (new cycle :of '(a b c) :for 2)  :for 3))
;(next x t)
;(setq x (new copier :of (new cycle :of '(a b c) :for 2) :repeat-for 3))
;(next x t)
;(setq x (new copier :of (new cycle :of '(a b c) :for 2)
;             :for (new cycle :of '(2 3))))
;(next x t)
;(setq x (new copier :of (new cycle :of '(a b c) :for 2)
;             :repeat-for (new cycle :of '(2 3))))
;(next x t)
; (set! x (new copier :of (list (new cycle :of '(a b))) :for 2))
; (set! x (new copier :of (list (new cycle :of '(a b))) :for (new cycle :of '(2 3))))
; (describe x)
; (next x #t)


