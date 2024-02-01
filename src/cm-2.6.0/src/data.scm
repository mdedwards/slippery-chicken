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
;;; $Date: 2005/03/21 17:51:24 $

;;; Typed Intervals maintain information about spelling and quailty.
;;; They are only useful in ths standard chromatic scale. A typed
;;; interval is a tagged 20-bit fixnum:
;;;
;;;     0001 pttt dlll oooo ssss
;;;
;;;     bits   value meaning
;;;     -----------------------------------------------
;;;     0-3    ssss   number of semitones spanned (0-11).
;;;     4-7    oooo   number of octaves spanned (0-15).
;;;     8-10   lll    number of letters spanned (0-6).
;;;     11     d      direction, 1 ascending 0 for ascending.
;;;     12-14  ttt    interval types (0-6).
;;;     15     p      1 if can be perfect, 0 if major/minor.
;;;     16-20  0001   typed interval tag

(define interval-semitones-byte (byte 4 0))
(define interval-octaves-byte (byte 4 4))
(define interval-letters-byte (byte 3 8))
(define interval-direction-byte (byte 1 11))
(define interval-type-byte (byte 3 12))
(define interval-class-byte (byte 1 15))
(define interval-meta-byte (byte 4 16))
(define interval-meta-flag #b0001)
(define interval-down-flag 1)
(define interval-up-flag 0)
(define interval-perfect-flag 1)
(define interval-majmin-flag 0)
(define interval-data-mask #b11111111111)

;;; interval-names holds the valid interval names. the first entry in
;;; each sublist is the canonical name. entries are searched by 
;;; string names. if found the position in the list is encoded into
;;; the type field of the interval. to complement an interval the
;;; position is rotated around 3:  ((pos-3)*-1)-3

(define interval-names
  #((doubly-diminished ddim dd)
     (diminished dim d)     
     (minor min mi m)
     (perfect perf p)
     (major maj ma mj)
     (augmented aug a)
     (doubly-augmented aaug aa)))

(defmacro %interval-encoded? (n)
  `(= (ldb interval-meta-byte ,n) interval-meta-flag))

(defmacro %interval-class (n)
  `(ldb interval-class-byte ,n))

(defmacro %interval-type (n)
  `(ldb interval-type-byte ,n))

(defmacro %interval-direction (n) 
  `(ldb interval-direction-byte ,n))

(defmacro %interval-sign (n) 
  `(if (= (ldb interval-direction-byte ,n) 
          interval-down-flag) -1 1))

(defmacro %interval-letters (n)
  `(ldb interval-letters-byte ,n))

(defmacro %interval-octaves (n)
  `(ldb interval-octaves-byte ,n))

(defmacro %interval-semitones (n)
  `(ldb interval-semitones-byte ,n))

(define (interval-quality-type int)
  (if (%interval-encoded? int)
    (if (= (%interval-class int) interval-perfect-flag)
      'perfect 'imperfect)
    #f))

(define (interval-quality int)
  (if (%interval-encoded? int)
    (car (vector-ref interval-names (%interval-type int)))
    #f))

(define (interval-letters int)
  (if (%interval-encoded? int)
    (%interval-letters int)
    #f))

(define (interval-semitones int . args)
  (with-args (args &optional abs?)
    (if (%interval-encoded? int)
      (* (+ (%interval-semitones int)
	    (* (%interval-octaves int) 12))
	 (if (= (%interval-direction int) interval-down-flag)
	   (if abs? 1 -1)
	   1))
      int)))

(define (interval-size int . args)
  (with-args (args &optional abs?)
    (if (%interval-encoded? int)
      (* (+ (%interval-letters int)
	    (* (%interval-octaves int) 7)
	    1)
	 (if (= (%interval-direction int) interval-down-flag)
	   (if abs? 1 -1)
	   1))
      int)))

(define (interval-sign int)
  (if (%interval-encoded? int)
    (%interval-sign int)
    (if (< int 0) -1 1)))

(define (complement-interval int)
  (if (%interval-encoded? int)
    (let ((span (%interval-letters int))
          (semi (%interval-semitones int))
          (octs (%interval-octaves int)))
      (dpb (+ 3 (* (- (%interval-type int) 3) -1))
           interval-type-byte
           (dpb (modulo (- 7 span) 7)
                interval-letters-byte
                (dpb (if (= span octs 0) 1 0)
                     interval-octaves-byte
                     (dpb (modulo (- 12 semi) 12)
                          interval-semitones-byte
                          int)))))
    (modulo (- 12 int) 12)))

(define (invert-interval int)
  (if (%interval-encoded? int)
    (dpb (if (= (%interval-direction int) interval-down-flag)
           interval-up-flag
           interval-down-flag)
         interval-direction-byte 
         int)
    (* int -1)))
; (format #t "~b" (logand #b111111 (lognot #b1100)))

(define (decode-interval int) 
  (if (%interval-encoded? int)
    (let ((octaves (%interval-octaves int))
          (letters (%interval-letters int)))
      ;; augmented 7ths are in the lower octave
      (when (and (> (%interval-type int) 4)
                 (= letters 6))
        (set! octaves (- octaves 1)))
      ;; dimihished 8vs are in the upper octave
      (when (and (< (%interval-type int) 2)
                 (= letters 0))
           (set! octaves (+ octaves 1)))
      
      (list (interval-quality int)
            (* (1+ (+ letters
                      (* 7 octaves)))
               (%interval-sign int))))
    int))

(define (encode-interval interval . args)
  (with-args (args &optional letters)
    (let ((direction 1)
	  (quality #f)
	  (class #f)
	  (semitones #f)
	  (octaves #f))
      (if letters
	(begin (set! quality interval))
	(begin
	 (set! quality (car interval))
	 (set! letters (cadr interval))))
    
      (unless (number? letters)
	(err "~s is not a valid interval." interval))
    
      ;; convert letters to zero based mod 7 octave.
      (if (= letters 0)
	(err "~s is not a one-based interval (1=Unison and 8=Octave)."
             interval)
	(if (< letters 0)
	  (begin (set! direction -1)
		 (set! letters (- (abs letters) 1)))
	  (set! letters (- letters 1))))
    
      ;; determine initial semitone size based on distance
      ;; between letters in the major scale.
      (set! octaves (inexact->exact (floor (/ letters 7))))
      (set! letters (modulo letters 7))
      (set! semitones (list-ref '(0 2 4 5 7 9 11) letters))
    
      ;; lookup quality in interval-names
      (set! interval quality)		; save name for error checking
      (set! quality (position (symbol->string quality) 
			      interval-names
			      :test 
			      (lambda (a b)
				(find a b :key (function symbol->string)
				      :test (function string-ci=?)))))
    
      ;; check quality and make adjustments to semitones accordingly
      (if (member letters '(0 3 4))
	(begin				; "perfect" intervals
	  (set! class interval-perfect-flag)
	  (case quality 
	    ((0 ) (set! semitones (- semitones 2))) 
	    ((1 ) (set! semitones (- semitones 1)))
	    ((3 ) #f)
	    ((5 ) (set! semitones (+ semitones 1))) 
	    ((6 ) (set! semitones (+ semitones 2)))
	    (else
	     (err "~s invalid modifier for perfect interval ~s."
		  interval (+ 1 letters)))))
	(begin				; "maj/min" intervals
	  (set! class interval-majmin-flag)
	  (case quality
	    ((0 ) (set! semitones (- semitones 3)))
	    ((1 ) (set! semitones (- semitones 2)))
	    ((2 ) (set! semitones (- semitones 1)))
	    ((4 ) #f )
	    ((5 ) (set! semitones (+ semitones 1)))
	    ((6 ) (set! semitones (+ semitones 2)))
	    (else
	     (err "~s invalid modifier of major/minor interval ~s."
		  interval (+ 1 letters))))))
    
      ;; now check for additional octaves due to aug 7ths etc.
      (if (>= semitones 12)
	(set! octaves (+ octaves 1))
	(if (< semitones 0)
	  (set! octaves (- octaves 1)))) ; diminished octs
    
      (set! semitones (modulo semitones 12))
      (set! direction (if (= direction -1)
			interval-down-flag interval-up-flag))
      (dpb interval-meta-flag interval-meta-byte 
	   (dpb class interval-class-byte
		(dpb quality interval-type-byte
		     (dpb direction
			  interval-direction-byte
			  (dpb letters
			       interval-letters-byte
			       (dpb octaves interval-octaves-byte
				    (logand semitones #xff))))))))))

;;;
;;; Rhythm
;;;

(define *tempo* 60.0)
(define *beat* .25)
(define *rhythms* (make-hash-table 31))

(let ((entries '((1/64  x    64th)
		 (1/32  t    32nd)
		 (1/16  s    16th)
		 (1/8   e     8th)
		 (1/4   q     4er)
		 (1/2   h)
		 (1     w)
		 (2     d)
		 (4     l)
		 (8     m))))
  (let ((rsym (lambda (pre raw post)
		(string->symbol
		 (string-append 
                  (if (not pre) ""
                      (if (string? pre) pre
                          (symbol->string pre)))
                  (if (string? raw) raw
                      (symbol->string raw))
                  (if (not post) ""
                      (if (string? post) post
                          (symbol->string post))))))))
    (dolist (e entries)
      (let ((rat (car e)))
	(dolist (s (cdr e))
	  (hash-set! *rhythms* s rat)
	  (hash-set! *rhythms*  (rsym 't s #f) (* rat 2/3))
	  (dolist (x '(("." 4/8) (".." 6/8) ("..." 7/8)))
	    (let ((d (car x))
		  (r (cadr x)))
	      (hash-set! *rhythms* (rsym #f s d) 
			 (+ rat (* rat r)))
	      (hash-set! *rhythms* (rsym 't s d) 
			 (+ (* rat 2/3) (* rat 2/3 r))))))))))

(define-generic rhythm)
              
(define-method (rhythm (val <number>) . args)
  (with-args (args &optional (tempo *tempo*) (beat *beat*))
    (unless (number? beat)
      (set! beat (parse-rhythm-string (symbol->string beat))))
    (* (/ val beat) (/ 60 tempo))))

(define-method (rhythm (val <pair>) . args)
  (with-args (args &optional (tempo *tempo*) (beat *beat*))
    (loop for v in val collect (rhythm v tempo beat))))

(define %rest-char #\-)

(define-method (rhythm (val <symbol>) . args)
  (with-args (args &optional (tempo *tempo*) (beat *beat*))
    (let ((n (hash-ref *rhythms* val)))
      (if n
	(rhythm n tempo beat)
	(let* ((str (symbol->string val))
               (num (if (char-ci=? (string-ref str 0) %rest-char)
                      (- (parse-rhythm-string (substring str 1)))
                      (parse-rhythm-string str ))))
          (hash-set! *rhythms* val num)
          (rhythm (hash-ref *rhythms* val) tempo beat))))))

;(loop for r in  '(t q s 
;                  tq ts tt t4 t16 t32
;                  qq qs qt q4 q16 q32
;                  sq ss st s4 s16 s32
;                  te qe se)
;      collect (list r (parse-rhythm-string (symbol->string r))))

(define (parse-rhythm-string rhythm)
  (let ((chars '(#\m #\l #\b #\w #\h
                 #\q #\e #\s #\t #\x))
        (ops '(#\+ #\- #\* #\/ )))
    (letrec ((parse-rhythm-token 
	      (lambda (str)
	        (let ((end (string-length str))
		      (pos #f)
		      (chr #f)
		      (trp #f)
		      (qup #f)
		      (sep #f)
		      (beg #f)
		      (rhy #f))
		  ;; T and Q are Triplet and Quintuplet markers if they
		  ;; precede a number or rhythm token.
		  (if (and (> end 1)
			   (or (char-numeric? (string-ref str 1))
			       (find (string-ref str 1) chars
                                     :test (function char-ci=?)))
			   (begin
			    (set! trp (char-ci=? (string-ref str 0) #\t))
			    (set! qup (char-ci=? (string-ref str 0) #\q))
			    (set! sep (char-ci=? (string-ref str 0) #\s))
			    (or trp qup sep)))
		    (set! beg 1)
		    (set! beg 0))
		  (set! chr (string-ref str beg))
		  (set! pos (position chr chars
                                      :test (function char-ci=?)))
		  (cond (pos
		         (set! beg (+ beg 1))
		         (set! rhy (expt 2 (- pos 3))))
		        (else
		         (set! pos beg)
		         (loop with flg = #f
			       while (< beg end)
			       do (set! chr (string-ref str beg))
			       if (or (char-numeric? chr)
				      (and (char=? chr #\.)
					   (not flg)
					   (< beg (1- end))
					   (char-numeric?
					    (string-ref str (1+ beg)))
					   (set! flg #t)))
			       do (set! beg (+ beg 1))
			       else do (return))
		         (if (> beg pos)
			   (set! rhy (string->number 
				      (substring str pos beg)))
			   (err "Not a rhythm: ~A" str))
		         (set! pos (+ beg 2))
		         (when (and (<= pos end)
				    (member (substring str beg end)
					    '("th" "TH" "nd" "ND"
                                              "st" "ST")))
			   (set! beg pos))))
		  (loop while (and (< beg end)
				   (char=? (string-ref str beg) #\.))
		        count (begin (set! beg (+ beg 1))) into dot
		        finally 
		        (unless (= dot 0)
		          (set! rhy (/ rhy (- 2 (expt 2 (- dot)))))))
		  (if trp
		    (set! rhy (* rhy 3/2)))
		  (if qup
		    (set! rhy (* rhy 5/4)))
		  (if sep
		    (set! rhy (* rhy 7/4)))
		  (if (= beg end)
		    rhy
		    (err "Can't parse ~A as a rhythm." str)))))
             (next-token-position
	      (lambda (string lb len)
	        (loop with chr for i from lb below len
		      do (set! chr (string-ref string i))
		      until (find chr ops :test (function char-ci=?))
		      finally (return i)))))
            ;; parse rhythmic expression. operator precedence is not supported.
            (let* ((len (string-length rhythm))
                   (lb 0)
                   (ub (next-token-position rhythm lb len)))
              (unless (< lb ub) (err "Not a rhythm: ~A" rhythm))
              (loop with num = (/ 1 (parse-rhythm-token
			             (substring rhythm lb ub)))
                    and val and op
                    for i from 0
                    while (< ub len)
                    do
                    (set! op (string-ref rhythm ub))
                    (set! lb (+ ub 1))
                    (set! ub (next-token-position rhythm lb len))
                    (unless (< lb ub)
                      (err "Not a rhythm: ~A." rhythm))
                    (cond ((char=? op #\+)
                           (set! val (/ 1 (parse-rhythm-token
                                           (substring rhythm lb ub))))
                           (set! num (+ num val)))
                          ((char=? op #\-)
                           (set! val (/ 1 (parse-rhythm-token
                                           (substring rhythm lb ub))))
                           (decf num val))
                          ((char=? op #\*)
                           (set! val (string->number
			              (substring rhythm lb ub)))
                           (set! num (* num val)))
                          ((char=? op #\/)
                           (set! val (string->number
			              (substring rhythm lb ub)))
                           (set! num (/ num val)))
                          ((char=? op #\x)
                           (set! val (string->number
			              (substring rhythm lb ub)))
                           (set! num (loop repeat val collect num))))
                    finally (return num))))))

;;;
;;; amplitude
;;;

(define *softest* 0.0)
(define *loudest* 1.0)
(define *power* 1.0)
(define *logical-amplitudes*
  '(niente 0/10 pppp 1/10 ppp 2/10 pp 3/10 p 4/10
    mp 5/10 mf 6/10 f 7/10 ff 8/10 fff 9/10 ffff 10/10))

(define-generic amplitude)

(define-method (amplitude (amp <symbol>) . args)
  (with-args (args &optional (softest *softest*) 
		   (loudest *loudest*) (power *power*))
    (amplitude (or (list-prop *logical-amplitudes* amp)
		   (err "'~s' is not a logical amplitude."
			amp))
	       softest loudest power)))

(define-method (amplitude (amp <number>) . args)
  (with-args (args &optional (softest *softest*) 
		   (loudest *loudest*) (power *power*))
    (+ softest (* (- loudest softest) (expt amp power)))))


;;;
;;; Envelope Lists 
;;;

;(define (lookup num lst)
;  (let ((tail lst)
;	(last #f))
;    (loop while (and (not (null? tail))
;			  (<= (car tail) num))
;      do 
;      (set! last tail)
;      (set! tail (cddr tail)))
;    (if (not last) (cadr lst) (cadr last))))

(define (lookup num lst)
  (do ((tail lst)
       (last #f))
      ((or (null? tail)
	   (> (car tail) num))
       (if (not last) (cadr lst) (cadr last)))
    (set! last tail)
    (set! tail (cddr tail))))

(define (interp1 x coords base)
  (let ((head coords))
    (let* ((x1 (if (null? coords)
		 (err "bad coordinate list: ~s" head)
		 (pop coords)))
	   (y1 (if (null? coords)
		 (err "bad coordinate list: ~s" head)
		 (pop coords)))
	   (x2 x1)
	   (y2 y1))
      (do ()
	  ((or (null? coords) (> x2 x)) #f)
	(set! x1 x2)
	(set! y1 y2 )
	(set! x2 (if (null? coords)
		   (err "bad coordinate list: ~s" head)
		   (pop coords))) 
	(set! y2 (if (null? coords)
		   (err "bad coordinate list: ~s" head)
		   (pop coords))))
      (cond ((>= x x2)
	     y2)
	    ((<= x x1)
	     y1)
	    ((= base 1)
	     (+ y1 (* (- x x1) (/ (- y2 y1) (- x2 x1)))))
	    (else
	     (let ((pct (/ (- x x1) (- x2 x1))))
	       (+ y1 (* (/ (- y2 y1) (- base 1.0))
			(- (expt base pct) 1.0)))))))))

(define (interpl x coords . args)
  (with-args (args &key min max (offset min) scale (base 1))
    (if (and offset max) (set! scale (- max offset)))
    (let ((y (interp1 x coords base)))
      (if offset
	(+ (if scale (* y scale) y) offset)
	(if scale (* y scale) y)))))

(define (interp x . coords)
  (interp1 x coords 1))

(define (tendency x low high . args)
  (with-args (args &key min max (offset min) scale 
                   (ranfn (function random))
                   (state *random-state*) &aux value)
    (if (and offset max) (set! scale (- max offset)))
    (when (pair? low) (set! low (interp1 x low 1)))
    (when (pair? high) (set! high (interp1 x high 1)))
    (when (> low high)
      (rotatef low high))
    (set! value (if (= low high)
		  low
		  ;; 1.0 insures float
		  (+ low (ranfn (* 1.0 (- high low)) state))))
    (when scale (set! value (* value scale)))
    (if offset
      (+ value offset)
      value)))

(define (rescale-envelope env . args)
  (with-args (args &key x-min x-max y-min y-max)
    (let ((minx #f)
	  (miny #f)
	  (maxx #f)
	  (maxy #f))
      (loop for tail on env by #'cddr
	    for x = (car tail)
	    for y = (cadr tail)
	    do
	    (unless minx (set! minx x))
	    (set! maxx x)
	    (if miny (set! miny (min miny y))
		(set! miny y))
	    (if maxy (set! maxy (max maxy y))
		(set! maxy y)))
      (if (and (or x-min x-max) (or y-min y-max))
	(begin
	  (or x-min (set! x-min minx))
	  (or x-max (set! x-max maxx))
	  (or y-min (set! y-min miny))
	  (or y-max (set! y-max maxy))
	  (loop for tail on env by #'cddr
		for x = (car tail)
		for y = (cadr tail)
		collect (rescale x minx maxx x-min x-max)
		collect (rescale y miny maxy y-min y-max)))
	(if (or x-min x-max)
	  (begin 
	    (or x-min (set! x-min minx))
	    (or x-max (set! x-max maxx))
	    (loop for tail on env by #'cddr
		  for x = (car tail)
		  for y = (cadr tail)
		  collect (rescale x minx maxx x-min x-max)
		  collect y))
	  (begin
	    (or y-min (set! y-min miny))
	    (or y-max (set! y-max maxy))
	    (loop for tail on env by #'cddr
		  for x = (car tail)
		  for y = (cadr tail)
		  collect x
		  collect (rescale y miny maxy y-min y-max))))))))

;;;
;;; Scaling and offsetting
;;;

;;; floor of n + 1/2 avoids cltl odd/even rounding
(define (quantize value step)
  (* (floor (+ (/ value step) 1/2)) step))

(define (rescale value oldmin oldmax newmin newmax)
  (+ (* (/ (- newmax newmin) (- oldmax oldmin)) (- value oldmin))
     newmin))

(define (mymod num div)
  (if (and (exact? num)
           (exact? div))
    (modulo num div)
    (let* ((res (/ num div))
           (flo (floor res)))            ; always float!
      (- num (* flo div)))))

;(mymod 50.0 12)

(define (fit number lb ub . args)
  (with-args (args &optional (mode :reflect))
    (when (> lb ub)
      (rotatef lb ub))
    (if (<= lb number ub) number
        (let ((b (if (> number ub) ub lb))
              (r (- ub lb)))
          (case mode
            ((:limit )
             b)
            ((:reflect )
             (let* ((2r (* 2 r))
                    (v (rem (- number b) 2r)))
               (+ (if (> (abs v) r)
                    ( (if (>= v 0) (function -) (function +))
		      v 2r)
                    (- v))
                  b)))
            ((:wrap )
             (+ (if (= b ub) lb ub)
                (rem (- number b) r)))
            (else
             (err "~s is not :limit, :reflect or :wrap" mode)))))))

(define (cents->scaler cents)
  (expt 2 (/ cents 1200)))

(define (scaler->cents scaler)
  (inexact->exact (* (log2 scaler) 1200)))

;;;
;;; Basic randomness
;;;

(define (between lb ub . args)
  (with-args (args &optional exception (state *random-state*))
    (let ((range (- ub lb)))
      (if (not (> range 0))
	lb
	(if exception
	  (loop for num = (+ lb (random range state))
		when (not (= exception num))
		return num)
	  (+ lb (random range state)))))))

(define (pick . args)
  (list-ref args (random (length args))))

(define (pickl seq . args)
  (with-args (args &key (end (length seq)) (start 0)
		   avoid (state *random-state*))
    (let ((range (- end start)))
      (if avoid
	(loop with x = avoid
	      while (eq? x avoid)
	      do (set! x (list-ref seq (+ start (random range state))))
	      finally (return x))
	(list-ref seq (+ start (random range state)))))))

(define (vary value variance . args)
  (with-args (args &optional (where :around)
                   (state *random-state*))
    (if (or (<= variance 0) (= value 0))
      value
      (let ((vary (random (abs (* value variance 1.0)) state)))
	(case where
	  ((:center :around)
	   (+ (- value (abs (* value variance .5))) vary))
	  ((:above )
	   (+ value vary))
	  ((:below )
	   (- value vary))
          (else
           (err "~s is not :center :around :above or :below"
                where)))))))

(define (drunk n width . args)
  (with-args (args &key (low most-negative-fixnum) 
		   (high most-positive-fixnum) (mode :reflect)
                   (avoid #f) (state *random-state*))
    (let ((amt (between (- width) width avoid state)))
      (incf n amt)
      (unless (<= low n high)
	(cond 
	  ((eq? mode ':reflect )
	   (set! n (fit n low high)))
	  ((eq? mode ':reset )
	   (set! n (+ low (/ (- high low) 2))))
	  ((eq? mode ':jump )
	   (set! n (between low high)))
	  ((eq? mode ':limit )
	   (set! n (max low (min n high))))
          ((eq? mode ':stop )
	   (set! n #f))
          ((number? mode) (set! n mode))
          (else 
           (err "~s is not :refect :reset :jump  :limit or :stop."
                mode))))
      n)))

(define (odds n . args)
  (with-args (args &optional (if-true-val #t) (if-false-val #f) 
		   (state *random-state*))
    (if (< (random 1.0 state) n) if-true-val if-false-val)))

(define (shuffle seq . args)
  (with-args (args &key (start 0) (end (length seq) )
		   (state *random-state*) (copy #t)
		   &aux (width (- end start)))
    (if (< width 2)
      seq
      (begin
	(when copy
	  (set! seq (copy-list seq)))
	(loop for i from start to (- end 1)
	      for j = (+ start (random width state)) 
	      for v = (list-ref seq i)
	      do 
	      (list-set! seq i (list-ref seq j))
	      (list-set! seq j v))
	seq))))

(define (ran . args)
  (with-args (args &key (type ':uniform) from (below 1.0)
		   (state *random-state*)
		   a b distribution)
    (let ((num #f)
	  (res #f))
      (if (and from below)
        (begin
         (when (< below from)
           (rotatef below from))
         (set! num (- below from)))
        (set! num (or below
		      (err "Missing ':below' (upper bound value)."))))
      (let ((betad
	     (lambda (a b s)
	       ;; beta distribution returns 0<=x<=1. A and B control 
	       ;; density. when a=b=1 uniform distribution results.
	       ;; When a=b the distribution is symmetric around .5.
	       ;; When a<1 and b<1 then density of large and small 
	       ;; numbers increases. When a>1 and b>1, density is similar
	       ;; to the normal distribution.
               (loop with 1/a = (/ 1 a) and 1/b = (/ 1 b)
                     for r1 = (random 1.0 s)
                     for r2 = (random 1.0 s)
                     for y1 = (expt r1 1/a)
                     for y2 = (expt r2 1/b)
                     for sum = (+ y1 y2)
                     while (> sum 1.0)
                     finally (return (/ y1 sum)))))
	    (gaussian
	     (lambda (s)
               (let ((a (random 1.0 s))
                     (b (random 1.0 s)))
                 (* (sqrt (* -2.0 (log (- 1 a))))
                    (cos (* 6.283185307179586 b))))))
	    (exp2 
	     (lambda (l s)
	       (let ((u #f)
		     (v #f)
		     (2*e^-1 (* 2 (exp -1.0))))
	         (loop do (set! u (- 1.0 (random 1.0 s)))
		       (set! v (* 2*e^-1 (random 1.0 s)))
		       until (<= v (* -2.0 u (log u)))
		       finally (return (/ v u l))))))
	    (cauchy 
	     (lambda (pos? s)
	       (let ((r (* (if pos?
			     1.5707963267948966
			     pi)
			   (random 1.0 s))))
		 (/ (sin r) (cos r)))))
	    (poisson
	     (lambda (l s)
               ;; lambda can't be negative
               (if (< l 0)
                 (error "Lambda parameter ~s to poisson is negative" l))
               (let ((b (exp (- l)))
                     (n 0))
                 (loop for p = (random 1.0 s) then (* p (random 1.0 s))
                       do (incf n)
                       until (< p b))
                 n )))
	    (cheezy-gamma 
	     (lambda (nu s)
               (let ((r 1))
                 (loop repeat (round nu) do
                       (set! r (* r (- 1 (random 1.0 s)))))
                 (- (log r)))))
	    )
        (case (or distribution type)
          ((:uniform :u uniform u)
           (set! res (random num state)))
          ((:low :lp :low-pass lp low-pass)
           (set! res (min (random num state) (random num state))))
          ((:high :hp :high-pass hp high-pass)
           (set! res (max (random num state) (random num state))))
          ((:mean :triangular :bp :band-pass bp band-pass)
           (set! res (/ (+ (random num state) (random num state)) 2)))
          ((:beta beta)
           (if a
             (if b #f (set! b a))
             (if b (set! a b)
	         (begin (set! a .5) (set! b .5))))
           (set! res (* num (betad a b state))))
          ((:slow-exp slow-exp)
           (set! res (* num (exp2 a state))))
          ((:exponential exponential :exp exp)
           (set! res (* num (/ (- (log (- 1 (random 1.0 state)))) a))))
          ((:cauchy cauchy)
           (set! res (* num (cauchy a state))))
          ((:poisson poisson)
           (set! res (* num (poisson a state))))
          ((:gamma gamma)
           (set! res (* num (cheezy-gamma a state))))
          ((:gaussian gaussian :gauss gauss)
           (set! res (* num (gaussian state))))
          (else
           (err "~s is not a random distribution type."
                (or distribution type))))
        (if from (+ from res) res)))))

;;;
;;; ransegs
;;;

(define (ransegs num . args)
  (with-args (args &key (type ':uniform)
                   (min 0.0 mnp) (max 1.0 mxp)  (sum 1.0 smp)
                   a b)
    (if smp (set! max sum))
    (if (not (< min max))
      (err "Minimum value ~s not less than maximum ~s." 
           min max))
    (if (and smp (or mnp mxp))
      (err "sum ~s: keyword :sum not allowed with :min or :max."
           sum))
    (let ((done (if (not smp) num (+ num 1)))
          (segs (list))
          (mini most-positive-fixnum)
          (sums 0)
          (maxi most-negative-fixnum))
      (do ((i 0)
           (r (ran :type type :a a :b b)
              (ran :type type :a a :b b)))
          ((= i done) 
           (set! segs (sort segs #'<))
           (set! mini (first segs)))
        (unless (member r segs)
          (set! segs (cons r segs))
          (set! sums (+ sums r))
          (if (> r maxi) (set! maxi r))
          (set! i (+ i 1))))
      (do ((tail (cdr segs) (cdr tail)))
          ((null? (cdr tail))
           (set-car! segs min)
           (set-car! tail max)
           (if (not smp)
             segs
             ;; we have 1 more point than values to return, reuse list
             ;; by storing deltas 1 pos previous and then setting last
             ;; cdr to nil
             (do ((last segs (cdr last))
                  (tail (cdr segs) (cdr tail))
                  (prev #f))
                 ((null? tail)
                  (set-cdr! prev (list))
                  segs)
               (set! prev last)
               (set-car! last (- (car tail) (car last))))))
        (set-car! tail (rescale (car tail) mini maxi min max))))))

;;;
;;; Exponentiation
;;;

(define (expl power . args)
  (with-args (args &key (y0 0.0) (y1 1.0) (base 2))
    (unless (<= 0 power 1)
      (err "Power ~s is not between 0 and 1." power))
    (if (= base 1)
      (+ y0 (* (- y1 y0) power))
      (+ y0 (* (/ (- y1 y0) (- base 1.0))
	       (- (expt base power) 1.0))))))

;;;
;;; explsegs returns a list of exponentially scaled segments that sum to
;;; the specified number. Num is the number to sum to.  Len is the number
;;; of segments to sum over.  Power is the power curve for the
;;; exponential scaling.  power numbers 0<n<1 yield concave slopes.
;;;

(define (explsegs len sum power)
  (if (= len 0) 
    '()
    (loop for i from 1 to len
          for l = 0 then v
          for v = (expl (/ i len) :base power)
          collect (* sum (- v l)))))

;;;
;;; like explsegs but returns each value individually
;;;

(define (explseg i len num power )
  (if (>= i len) 
    (* 0 num) ; return 0 in same type as nu,
    (* num (- (expl (/ (+ i 1) len) :base power)
              (if (<= i 0) 0 (expl (/ i len) :base power))))))

;; mk
;; returns a geometric series of len elements
;; with a total sum equal to sum
;; base is the geometric multiplier
;;

(define (geosegs len sum base)
  (if (= len 0) #f
      (let ((a (* sum (/ (- 1 base) (- 1 (expt base len))))))
        (loop for n from 0 below len
              collect a
              do (set! a (* a base))))))

;; mk
;; like geosegs but returns each value individually
;; index from 0...len-1
;;

(define (geoseg i len sum base)
  (if (>= i len) (* 0 sum)  ; return 0 in same type as nu,
    (let ((a (* sum (/ (- 1 base) (- 1 (expt base len))))))
      (* a (expt base i)))))

;;;
;;; doeach iterates evaluates depedning on var being a list
;;;

(defmacro doeach (pars . forms)
  (unless (pair? pars)
    (err "doeach: ~s not list (var source &optional return)" 
         pars))
  (let* ((args pars)
         (var (pop args))
         (source (pop args))
         (retrn (if (null? args) #f (pop args))))
    (let ((src (gensym)))
      `(let ((,var ,source))
         (if (pair? ,var)
           (let ((,src ,var))
             (dolist (,var ,src ,retrn) ,@forms))
           (begin ,@forms ,retrn))))))

;;;
;;; Prime Forms

(define (encode-set notes)
  (let ((set 0))
    (dolist (e notes)
      (set! set (logior set (ash 1 (modulo (keynum e) 12)))))
  set))

(define (decode-set set)
  (loop for i below 12
        when (= (ldb (byte 1 i) set) 1)
        collect i))

(define (encode-inverse-set notes)
  (let ((set 0))
    (dolist (e notes)
      (set! set (logior set (ash 1 (modulo (- (keynum e)) 12)))))
    set))

(define (best-normal-form set)
  ;; Return the BNF and transposition of set.
  (let ((min most-positive-fixnum)
	(transp #f)
	(byt (byte 1 11)))
    (dotimes (i 12)
      (when (< set min)
	(set! min set)
	(set! transp (modulo (- 12 i) 12)))
      (set! set (logand (logior (ldb byt set) (ash set 1)) 4095)))
    (values min transp)))

(define (prime-form notes . args)
  ;; Return the prime form (BNF or inverted BNF, whichever is
  ;; more compact of notes and the sets transposition."
  (with-args (args &optional dont-decode)
    (let ((bnf #f)
	  (tr #f)
	  (tr2 #f)
	  (ibnf #f))
      tr2
      (multiple-value-setq (bnf tr)
			   (best-normal-form (encode-set notes)))
      (multiple-value-setq (ibnf tr2)
			   (best-normal-form (encode-inverse-set notes)))
      (if (< bnf ibnf)
	(values (if dont-decode bnf (decode-set bnf)) tr)
	(values (if dont-decode ibnf (decode-set ibnf)) tr)))))

;;;
;;; Markov analysis
;;;

;(define happy-birthday '(c4 c4 d4 c4 f4 e4 c4 c4 d4 c4 g4 f4
;			 c4 c4 c5 a4 f4 e4 d4 bf4 bf4 a4 f4 g4 f4))
;(markov-analyze happy-birthday :order 2)

(define (markov-analyze seq . args)
  (with-args (args &key (order 1) 
		   (print? #t)		; t pattern table
		   (pattern? #t)	; #f or pattern
		   sort?
		   (print-decimals 3)
		   key)
    (let ((len (length seq)) 
	  (labels '())			; the set of all outcomes 
	  (table '())
	  ;(table-column-width 6)
	  (row-label-width 8) 
	  (pat #f)
	  (field (+ print-decimals 2)))	; n.nnn 
      (letrec ((add-outcome
		(lambda (prev next) 
		  (let ((entry (find prev table :test #'equal?
                                     :key #'car))) ; was assoc 
		    (if (not entry) 
		      (push (list prev
                                  (format #f "~s" prev) 
                                  (list next 1))
                            table) 
		      (let ((e (assoc next (cddr entry)))) 
			(if e 
			  (set-car! (cdr e) (+ 1 (cadr e)))
			  (set-cdr! (last-pair (cdr entry))
				    (list (list next 1)))))))))
	       (before?
		(lambda (x y l) 
		  (if (null? x) #t 
                    (let ((p1 (position (car x) l :test #'equal?)) 
                          (p2 (position (car y) l :test #'equal?))) 
                      (cond ((< p1 p2) #t) 
                            ((= p1 p2) 
                             (before? (cdr x) (cdr y) l)) 
                            (else #f))))))
	       (liststring 
		(lambda (l)
		  (if (null? l) ""
		      (let ((a (format #f "~s" (car l))))
			(do ((x (cdr l) (cdr x)))
			    ((null? x) a)
			  (set! a
				(string-append 
				 a (format #f " ~s" (car x))))))))))
              
	      (dotimes (i len) 
	        (loop with prev = (list)
		      for j to order 
		      for x = (let ((raw (list-ref seq (modulo (+ i j) len)))) 
			        (if key (key raw) raw)) 
		      ;; gather history in reverse order 
		      when (< j order) do (push x prev) 
		      finally 
		      (begin (add-outcome (reverse prev) x ) 
		             (or (find x labels)
			         (push x labels)))))
	      
	      ;; sort the outcomes according to user specification:
	      ;; a list, a sorting function or nil. 
	      (cond ((pair? sort?) 
	             (set! labels sort?)) 
	            (sort? 
	             (set! labels (sort labels sort?))) 
	            ((number? (car labels))
	             (set! labels (sort labels #'<)))
	            ((and (car labels) (symbol? (car labels)))
	             (set! labels (sort labels
				        (lambda (x y) 
                                          (string-ci<? (format #f "~a" x)
                                            (format #f "~a" y))))))
	            (else 
	             (set! labels (reverse labels)))) 
	      ;; map over data, normalize weights 
	      (loop for row in table 
	            for lab = (cadr row)	; label
	            for val = (cddr row) 
	            maximize (string-length lab) into len 
	            do 
	            (let ((total (loop for e in val sum (cadr e)))) 
		      (set! total (* total 1.0)) 
		      (loop for e in val 
		            do (set-car! (cdr e)
				         (decimals (/ (cadr e) total) 
					           print-decimals)))) 
	            finally (set! row-label-width (max len row-label-width))) 
              
	      ;; sort table according to value order. 
	      (set! table 
	            (sort table #'(lambda (x y)
			            (before? (car x) (car y) labels)))) 
              
	      (when (member print? '(#t table :table))
	        (let* ((sp " ")
		       (ln (make-string field #\-))) 
	          ;; print column header row
	          (begin (newline)
		         (dotimes (i row-label-width) (write-char #\*))
		         (dolist (l labels)
		           (display sp);; column seperator
		           (let* ((s (format #f "~a" l))
			          (n (string-length s)))
		             ;; write column pad
		             (dotimes (i (max (- field n) 0))
			       (write-char #\space))
		             (display s))))
	          ;; print each row
	          (dolist (row table)
	            (newline)
	            (let* ((s (liststring (car row)))
		           (n (string-length s)))
		      ;; print left pad for row label
		      (dotimes (i (max (- row-label-width n) 0))
		        (write-char #\space))
		      ;; print row label min row-label-width.
		      (dotimes (i (min row-label-width n))
		        (write-char (string-ref s i))))
	            (dolist (l labels)
		      (let ((v (assoc l (cddr row))))
		        (if (not v)
		          (begin  (display sp) (display ln))
		          (let* ((s (number->string (cadr v)))
			         (n (string-length s)))
		            (display sp)
		            ;; pad number
		            (dotimes (i (max (- field n) 0))
			      (write-char #\space))
		            (display s)
		            )))))
	          (newline)))
              
	      (when (or pattern? (member print? '(#t pattern :pattern)))
	        (set! pat  
		      (loop for row in table 
		            collect (append (car row) '(->) (cddr row)))))
	      (when (member print? '(#t pattern :pattern))
	        (pprint `(new markov of ', pat)))
	      (if pattern?
                ;; patterns not defined yet, cant use new or <markov>
	        (make-instance (find-class 'markov) :of pat)
	        (values))))))

(define (histogram numbers lo hi slots)
  (let ((hist (make-list slots 0))
        (rang (- hi lo)))
    (loop with i
      for x in numbers
      if (and (>= x lo) (< x hi))
      do
      (set! i (inexact->exact (floor (* slots (/ (- x lo) rang)))))
      (list-set! hist i (+ (list-ref hist i) 1)))
    hist ))
