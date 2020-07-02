(in-package :slippery-chicken)

;;; Make a small class to hold data from the call to clm::get-spectrum. Later, a
;;; hacked version of clm::create-analysis-data makes a hash table
;;; populated with instances of this class. This is so freq,  amp and other data
;;; are intrinsically linked.
;;; The hashtable is then converted into a list of events, which are ordered
;;; into sublists, and from there into bars, before a final call to bars-to-sc.

;;; Stuff that kinda works
;;; 1. sc generation
;;; 2. midi output

;;; TODO
;;; 1. Make work with more than one partial. Chords or a separate voice? or a
;;; choice?
;;; 2. Start times of notes not precise enough, not sure why
;;; 3. Score output is all over the shop, fix this

#|
EXAMPLE
(import-audio "~/sc-test-scale.wav")
(handle-ties +import+)
(midi-play +import+)
|#

;;; Class stuff
(defclass snapshot (named-object)
  ((start-time :accessor start-time :initarg :start-time :initform -1)
   (end-time :accessor end-time :initform nil)
   (dur :accessor dur :initform nil)
   (freq :accessor freq :initarg :freq :initform nil)
   (amp :accessor amp :initarg :amp :initform 0)
   (partial :accessor partial :initform 0)))

(defmethod print-object :before ((as snapshot) stream)
  (format stream "~%SNAPSHOT: start-time: ~,3f, ~
                  ~%               end-time: ~,3f, ~
                  ~%               dur: ~,3f, ~
                  ~%               freq: ~,5f, ~
                  ~%               amp: ~,5f, ~
		  ~%               partial: ~a"
          (start-time as)(end-time as)(dur as)(freq as)(amp as)(partial as)))

(defun make-snapshot (start-time &key end-time dur freq amp partial)
  (when (and end-time (not dur))
    (setf dur (- end-time start-time)))
  (when (and (not end-time) dur)
    (setf end-time (+ dur start-time)))
  (let ((as (make-instance 'snapshot)))
    (setf (slot-value as 'start-time) start-time
	  (slot-value as 'end-time) end-time
	  (slot-value as 'dur) dur
	  (slot-value as 'freq) freq
	  (slot-value as 'amp) amp
	  (slot-value as 'partial) partial)
    as))



;;; Hacked version of clm::create-analysis-data
(defun create-analysis-hash (sndfile
                             &key
                               ;; How ofen to perform freq analysis (secs)
                               ;; or if a list then these are times to
                               ;; do the analysis at  
                               (interval 0.01)
                               ;; Where to end in the sound file, if nil,
                               ;; analyse it all
                               (end nil) 
                               (num-partials 30)
                               (srate 44100)
                               (fftsize 8192) 
                               (max-peaks 200)
                               (start-analysis 0.0) 
                               (highest-bin (/ fftsize 8))
			       perform-new-analysis?
			       (printing t)
			       (normalise t))
  (let* ((stop (or end (clm::sound-duration sndfile)))
	 (hs (make-hash-table))
         (times (if (listp interval)
                    interval
                    (loop for start from start-analysis below stop by interval 
			  collect start)))
	 (max-amp 0.0))
    (loop ;; for start from start-analysis below stop by interval do
	  for start in times for count from 0 do
	    (when printing
	      (format t "~&Analysing ~a at time ~a" sndfile start))
	    (multiple-value-bind (freqs amps)
		(clm::get-spectrum sndfile 
				   :start-analysis start
				   :num-partials num-partials
				   :order-by clm::'freq
				   :srate srate
				   :fftsize fftsize
				   :normalise nil
				   :max-peaks max-peaks
				   :highest-bin highest-bin
				   :perform-new-analysis? perform-new-analysis?)
	      (loop for f in freqs and a in amps and i from 0
		    ;; maximizing a into max-a
		    ;; minimizing a into min-a
		    do
		       (setf (gethash count hs)
			     (make-snapshot (decimal-places start 5)
						 :dur interval
						 :freq f
						 :amp a
						 :partial i))
		       ;; (print (gethash count hs))
		       (when (> a max-amp) (setf max-amp a)))))
    ;; normalise
    (when normalise
      (loop for i being the hash-values in hs 
	    for a = (amp i)
	    do
	       (setf (amp i)
		     (if (almost-zero a 0.01)
			 0
			 (decimal-places (/ a max-amp) 5)))))
    hs))

;;; turn the hashtable into a list of events.
(defmethod hash-to-event-list ((hs hash-table)
			       &key (tempo 60)
				 (midi-channel 1)
				 (microtones-midi-channel 2)
				 (midi-tolerance 1)
				 (amp-tolerance 0.01)
				 )
  (loop for ev being the hash-values in hs
	with last-ev and tied-to and tied-from and new-ev
	if last-ev
	  if (and (equal-within-tolerance (freq-to-midi (freq ev))
					  (freq-to-midi (freq last-ev))
					  midi-tolerance)
		  (equal-within-tolerance (amp ev)
					  (amp last-ev)
					  amp-tolerance))
	    do (setf tied-to t
		     (dur new-ev) (+ (dur new-ev)
				     (dur ev)))
	else do (setf tied-to nil)
	     and collect
		 (make-event (freq-to-note (freq new-ev))
			     (round-if-close (decimal-places (dur new-ev) 3) 0.01)
			     :start-time (start-time new-ev)
			     :duration t
			     :midi-channel midi-channel
			     :microtones-midi-channel microtones-midi-channel
			     :amplitude (amp new-ev)
			     :tempo tempo)
		 and do (setf new-ev ev)
	end
	else do (setf new-ev ev)
	end
	do
	   (setf last-ev ev)))


;;; make a long list of events and consolidate adjacent ones where possible into
;;; longer events
(defun event-list-to-bar-list (event-list &key (tempo 60) (time-sig '(4 4)))
  (unless (listp event-list) (error "nope"))
  (let* ((l '())
	 (ev-list '())
	 (bar-dur (num-beats-at-tempo (make-time-sig time-sig)
				      (make-tempo tempo))))
    (loop for e in event-list
	  with tally = 0
	  do
	     (cond ((< (+ tally (duration e)) bar-dur)
		    (push e ev-list)
		    (incf tally (duration e))
		    (setf tally (decimal-places tally 3)))
		   ((= (+ tally (duration e)) bar-dur)
		    (push e ev-list)
		    (push (nreverse ev-list) l)
		    (setf tally 0 ev-list '()))
		   ((> (+ tally (duration e)) bar-dur)
		    (let* ((remaining-dur (- bar-dur tally))
			   (ev-slice (slice-event e remaining-dur)))
		      (setf (is-tied-from (first ev-slice)) t)
		      (push (first ev-slice) ev-list)
		      (push (nreverse ev-list) l)
		      (setf tally 0 ev-list '())
		      (loop for i in (split-into-units
				      (duration (second ev-slice)) bar-dur)
			    with split-e
			    do
			       (setf split-e
				     (make-event (pitch-or-chord e)
						 (decimal-places i 3)
						 :duration t))
			       
			       (if (= i bar-dur)
				   (progn
				     (setf (is-tied-to split-e) t
					   (is-tied-from split-e) t)
				     (push (list split-e) l))
				   (progn
				     (setf (is-tied-to split-e) t
					   (is-tied-from split-e) nil)
				     (push split-e ev-list)
				     (incf tally i)
				     (setf tally (decimal-places tally 3))))))))
	  finally
	     (unless (zerop tally)
	       (push (make-rest (decimal-places (- bar-dur tally) 3)
				:duration t :tempo tempo)
		     ev-list)
	       (push (nreverse ev-list) l)))
    (nreverse l)))



(defmethod slice-event ((e event) slice-time)
  (unless (> (duration e) slice-time)
    (error "whoops"))
  (setf slice-time (decimal-places slice-time 3))
  (let ((ev-slice (clone e))
	(ev-rest (clone e))
	(rest-time (decimal-places (- (duration e) slice-time) 3)))
    (setf (duration ev-slice) slice-time
	  (is-tied-from ev-slice) t
	  (duration ev-rest) rest-time
	  (is-tied-to ev-rest) t)
    (list ev-slice ev-rest)))
		      

(defun bar-list-to-bars (bar-list &key (time-sig '(4 4)) (tempo 60)
				    (midi-channel 1) (microtones-midi-channel 2))
  (let* ((bars '()))
    (loop for bar in bar-list
       for count from 1
       with empty-bar
       do
	 (setf empty-bar (make-rthm-seq-bar (list time-sig)))
	 (fill-with-rhythms empty-bar bar 
			    :midi-channel midi-channel
			    :microtones-midi-channel microtones-midi-channel)
	 (unless (is-full empty-bar)
	   (setf empty-bar (fix-bar empty-bar tempo)))
	 (setf (bar-num empty-bar) count)
	 (push empty-bar bars))
    (nreverse bars)))

;;; is the bar full? if not, add a little extra
(defmethod fix-bar ((bar rthm-seq-bar) &optional (tempo 60))
  (multiple-value-bind (full val) (is-full bar nil t)
    (unless full
      (loop for r in (split-into-units val
				       (num-beats-at-tempo (get-time-sig bar)
							   (make-tempo tempo)))
	    do
	(add-event bar (make-rest (decimal-places r 3) :duration t)))
      (gen-stats bar)))
  (if (is-full bar)
      bar
      (fix-bar bar tempo)))

;;; gives a list of divisions and remainders
;;; (split-into-units 23 4) => '(4 4 4 4 4 3.0)
(defun split-into-units (num &optional (unit 4) (max t))
  (when (and max (> num 10000))
    (error "~%split-into-fours: mate, ~a is massive and this'll take ~ 
            way too long" num))
  (let ((splits '()))
    (if (<= num unit)
	(push num splits)
	(multiple-value-bind (div rem) (floor num unit)
	  (loop repeat (abs div) do
	       (push unit splits))
	  (unless (zerop rem)
	    (push (decimal-places rem 5) splits))))
    (nreverse splits)))

;;; call this
(defun import-audio (audiofile &key
				 (start 1)
				 (end nil)
				 (interval 0.01)
				 (printing t)
				 (time-sig '(4 4))
				 (trim-offcuts t)
				 (consolidate t)
				 (num-partials 1)
				 (srate 44100)
				 (fftsize 8192)
				 (max-peaks 200)
				 (highest-bin (/ fftsize 8))
				 (tempo 60)
				 (midi-channel 1)
				 (microtones-midi-channel 2)
				 (midi-tolerance 1)
				 (amp-tolerance 0.01)
				 (instrument 'computer)
				 (player 'player-one)
				 (sc-name '+import+))
  (let* ((e (create-analysis-hash audiofile; path to audio file
				  :start-analysis start
				  :end end
				  :interval interval
				  :fftsize fftsize
				  :highest-bin highest-bin
				  :num-partials 1
				  :num-partials num-partials
				  :srate srate
				  :normalise t
				  :max-peaks max-peaks
				  :printing printing))
	 (hl (hash-to-event-list e :tempo tempo
				   :midi-channel midi-channel
				   :microtones-midi-channel
				   microtones-midi-channel
				   :midi-tolerance midi-tolerance
				   :amp-tolerance amp-tolerance))
	 (el (event-list-to-bar-list hl :time-sig time-sig
					:tempo tempo))
	 (bl (bar-list-to-bars el :time-sig time-sig
				  :tempo tempo
				  :midi-channel midi-channel
				  :microtones-midi-channel microtones-midi-channel))
	 (sc (bars-to-sc bl :sc-name sc-name :instrument instrument
			    :player player :tempo tempo
			    :midi-channels (list midi-channel microtones-midi-channel))))
    (when trim-offcuts
      (map-over-notes sc 1 nil nil
		      #'(lambda (n) (when (and (<= (duration n) 0.1)
					       (or (not (is-tied-to n))
						   (not (is-tied-from n))))
				      (force-rest n)))))
    (when consolidate
      (consolidate-all-rests sc)
      (consolidate-all-rests sc))
    sc))
