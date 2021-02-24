;;; autoc.ins
;;;
;;; MDE Mon Feb 25 19:56:05 2019 -- Michael Edwards changes: nothing except for
;;; using defscins instead of definstrument, in order to incorporate this into
;;; slippery-chicken 
;;;
;;; Autocorrelation fundamental estimation for CLM 4
;;; by Bret Battey
;;; version 1.0 February 10, 2003 for CLM2
;;; July 10, 2003 - fixed rms measurement calculation and db conversion
;;; July 11, 2003 - fixed run loop indexing
;;; March 30, 2004 - moved "percent complete" print inside run loop
;;;                - in final list construction, forced coercion of
;;;                  double-float freq values to single-float
;;; April 15, 2007 - removed the Mac OS9 MCL compile options to enable 
;;;                  succesful compile under CLM 3
;;; April 19, 2008 - Confirmed CLM-4 operation.
;;;                  Fixed file-readin to work under sbcl (and maybe
;;;                  in general?), since it was reading through file
;;;                  too quickly. Not sure when/how that started
;;;                  happening.
;;; 
;;; Usage:
;;; (autoc file <keyword parameters>)
;;;       Where file is a file path (example: "~/Sound/zip.aif")
;;;       Returns a CLM XY (time and frequency) envelope.
;;;
;;; KEYWORD PARAMETERS
;;; ------------------
;;;
;;; beg = Analysis start time in the input file. 
;;;       Default = 0.
;;; dur = Duration of the analysis. 
;;;       Default = NIL = to the end of file.
;;; rfreq = Rendering frequency (measurements per second). 
;;;         Default = 100
;;; db-floor = Minimum amplitude to be analyzed. Analysis windows having
;;;            and RMS amplitude below this threshold will be considered
;;;            as silent and return a fundamental frequency of 0.
;;;            Default = -30 dB
;;;
;;; GUIDELINES FOR VALID DATA
;;;
;;; max-jump-cents = The maximum "allowable" jump between two consecutive 
;;;                  measurements
;;;                  as autoc tries to ensure that returned data is continuous. 
;;;                  Expressed in cents.
;;;                  Default = 100.
;;; min-freq = Minimum allowable fundamental frequency. Measurements below this
;;;            threshold will be considered invalid. 
;;;            Default = 100.
;;; max-freq = Maximum allowable fundamental frequency. Measurements above this
;;;            threshold will be considered invalid.
;;;            Default = 1000.
;;;
;;; INLINE PROCESSING
;;;
;;; After analyzing a windows, autoc can test for invalid results and try new
;;; autocorrelation measurements with different parameters in hopes of getting
;;; a valid result:
;;;
;;; max-samp-offset = If the returned value for a window represents a jump exceeding
;;;                   max-jump-cents or is outside the valid frequency range, 
;;;                   autoc can offset the FFT window by one sample and try again. 
;;;                   It will do this up to max-samp-offset times. Try raising this value 
;;;                   if you are getting numerous gaps in the returned data.
;;;                   Default = 0 (no offsetting performed).
;;; max-ffts = If the returned value for a window represents a jump exceeding
;;;            max-jump-cents or is outside the valid frequency range,  autoc can 
;;;            try a different fft size. This may return better data. 
;;;            If max-ffts is 1, then a 2048
;;;            sample fft will be used. If max-ffts is 2, then 2048 followed by 1024.
;;;            If max-ffts is 3, then 2048 followed by 1024 followed by 4096.
;;;            Default = 1 (only uses a size of 2048). 
;;; 
;;; POST PROCESSING
;;;
;;; After the file is analyzed, post-processing functions may be applied to clean up the data. 
;;;
;;; post-process = Flag to control whether post-processing is utilized.
;;;                Default = T.
;;;
;;; max-filter-time = The maximum data gap that autoc will attempt to bridge with
;;;                   linear interpolation. That is, in the case of apparent silence
;;;                   or values out of range of discontinuous values, autoc will
;;;                   look ahead for seemingly convincing data up to max-filter-time.
;;;                   If it finds such, it will use linear interpolation to fill the
;;;                   gap between the two valid values.
;;;
;;; OTHER
;;;
;;; status = Flag to determine if percentage complete status display is printed.
;;;          Default = T.
;;; debug = Flag to determine if debugging messages are printed. 
;;;         Default = NIL.
;;;

;;; USAGE NOTES
;;; ------------
;;;
;;; The variable *autoc-lastwin* will contain the autocorrelation function of the
;;; last window in the analysis. This can be useful for plotting and debugging
;;; purposes.
;;;
;;; A primary reference on autocorrelation is
;;; Rabinber, L., "On the Use of Autocorrelation Analysis for Pitch
;;; Detection," from 'IEEE Transactions of Acoustics, Speech, and Signal
;;; Processing', Vol. ASSP-25, No 1, February 1977.
;;;
;;; Unlike that article's approach, AUTOC does not attempt to pre-process the
;;; sound to remove formants that could provide deceptive harmonics. Instead,
;;; emphasis is placed on the inline and post-processing routines to try to
;;; attain continuous solutions.
;;;
;;; A signal could be preprocessed, with a low-pass-filter for example, before
;;; being passed to AUTOC, and that may improve results.
;;;
(in-package :clm)

(defvar *autoc-lastwin*) ; will contain the autocorrelation of the last window

(defmacro ac-cents->scaler (cents)
  `(expt 2 (/ ,cents 1200)))

(defmacro autoc-linear->db (x) 
  `(if (< ,x .00001)
     -100
     (* 20 (log ,x 10.0))))

;;; AUTOCORRELATION AND ANALYSIS MACROS
     
(defmacro ac-complex-conjugate (fdr fdi fftsize)
  `(dotimes (k ,fftsize)
     (let ((r (aref ,fdr k))
           (i (aref ,fdi k)))
       (setf (aref ,fdr k) (* r r))
       (setf (aref ,fdi k) (* i (* -1 i))))))

(defmacro ac-find-zero-crossings (fdr fft2 zcrosses zcrossct)
  `(progn
     (clear-array ,zcrosses)
     (setf ,zcrossct 0)
     (dotimes (k ,fft2)
       (let ((this (aref ,fdr k))
             (next (aref ,fdr (1+ k))))
         (when (or (= 0 this)
                   (and (plusp this) (minusp next))
                   (and (minusp this) (plusp next)))
           (setf (aref ,zcrosses ,zcrossct) k)
           (incf ,zcrossct))))))

(defmacro ac-remove-negmin-brackets (fdr zcrosses zcrossct)
  `(let ((k 0))
     (loop until (>= k (- ,zcrossct 1)) do
           (let* ((thisx (aref ,zcrosses k))
                  (nextx (aref ,zcrosses (+ 1 k)))
                  (tweenx (round (/ (+ nextx thisx) 2)))
                  (tweeny (aref ,fdr tweenx)))
             (if (minusp tweeny)
               (progn
                 (loop for j from k below ,zcrossct do
                       (setf (aref ,zcrosses j) (aref ,zcrosses (1+ j))))
                 (decf ,zcrossct))
               (incf k 2))))
     ;; if odd count, remove last point 
     (when (oddp ,zcrossct)
       (decf ,zcrossct)
       (setf (aref ,zcrosses ,zcrossct) 0))))

(defmacro ac-find-peaks (fdr zcrosses zcrossct peaks peaksct)
  `(progn
     (clear-array ,peaks)
     (setf ,peaksct 0)
     (loop for k from 0 below ,zcrossct by 2 do
         (let* ((startx (floor (aref ,zcrosses k)))
                (endx (floor (aref ,zcrosses (1+ k))))
                (maxy 0)
                maxx)
           (loop for x from startx to endx do
                 (let ((thisy (aref ,fdr x)))
                   (when (> thisy maxy)
                     (setf maxy thisy)
                     (setf maxx x))))
           (setf (aref ,peaks ,peaksct) maxx)
           (incf ,peaksct)))))

(defmacro ac-check-amplitude (fdr peaks peaksct min-peak)
  `(let ((flag NIL))
     (loop for k below ,peaksct do
           (if (> (aref ,fdr (floor (aref ,peaks k))) ,min-peak)
             (progn
               (setf flag T)
               (loop-finish))))
     flag))

(defmacro ac-lineval (x x1 y1 x2 y2)
  `(let* ((a (/ (- ,x2 ,x) (- ,x2 ,x1)))
          (b (- 1 A)))
     (+ (* a ,y1) (* b ,y2))))

(defmacro ac-amp-find-max-peak (fdr peaks peaksct debug)
  `(let ((maxpeakindex 0)
         (maxpeaky 0)
         (lefty (aref, fdr 0)))
     (loop for i from 0 below ,peaksct do
           (let* ((peakx (floor (aref ,peaks i)))
                  (peaky (aref ,fdr peakx)))
             (if (> peaky maxpeaky)
               (setf maxpeaky peaky
                     maxpeakindex i))))
     (when ,debug
       (clm-print "Max peak index = ~D, x = ~D, y = ~F.~%" 
                  maxpeakindex (floor (aref ,peaks maxpeakindex)) maxpeaky))
     (when (> maxpeakindex 0)
       (when ,debug
         (clm-print "Discarding earlier peaks.~%"))
       (loop for j from maxpeakindex below ,peaksct do
           (setf (aref ,peaks (- j maxpeakindex)) (aref ,peaks j)))
       (decf ,peaksct maxpeakindex))
     maxpeakindex))

(defmacro ac-get-ratio (basefreq newfreq)
`(progn
   (if (or (zerop ,basefreq) (zerop ,newfreq))
     0
     (let ((ratio (/ ,newfreq ,basefreq)))
       (if (< ratio 1.0) (setf ratio (/ ratio)) ratio)))))

;;; POST-PROCESSING MACROS

(defmacro ac-check-candidates (freqs freqsct max-jump-ratio max-filter-candidates k)
  `(let ((max-candidates ,max-filter-candidates)
         (end ,freqsct)
         (debug NIL))
     (loop for j from (1+ ,k) to ,freqsct
           for c from 1 do
           (let* ((cand (aref ,freqs j))
                  (cratio (ac-get-ratio prevy cand)))
             (when debug
               (clm-print "   #~D, candidate #~D, ~F ratio ~F vs. max ~F~%" 
                          j c cand cratio (expt ,max-jump-ratio c)))
             ;; test viability
             (if (and (not (zerop cratio))
                      (<= cratio (expt ,max-jump-ratio c))) ; max-jump-ratio expands per candidate
               ;; if good candidate, linear interpolate to that point
               ;; and exit loop
               (progn
                 (loop for m from ,k below j do
                       (let ((inter (ac-lineval m (1- ,k) prevy j cand)))
                         (setf (aref ,freqs m) inter)
                         (when debug
                           (clm-print "     #~D point interpolated to ~F~%" 
                                      m inter))))
                 (setf ,k (1+ j))
                 (loop-finish))
               ;; if failed candidate, test for exceeded candidates
               ;; and end of list
               (progn
                 (when debug
                   (clm-print "     candidate rejected~%"))
                 (when (= c max-candidates)
                   (when debug 
                     (clm-print "   Max candidates exceeded.~%"))
                   ;; if we've jumped down to zero, make it a 
                   ;; clean cut to zero and increment ,k past end
                   ;; of fill
                   (if (and (zerop cand) (plusp prevy))
                     (progn
                       (when debug
                         (clm-print "   Jump down to zero. Setting ~D to ~D to 0.~%" ,k j))
                       (loop for m from ,k to j do
                             (setf (aref ,freqs m) 0))
                       (setf ,k (1+ j)))
                     ;; otherwise increment ,k once
                     (incf ,k))
                   (loop-finish))
                 (when (= j end) 
                   (when debug
                     (clm-print "End of array, setting ~D to ~D to ~F ~%" 
                                ,k (1- j) 
                                (if (zerop thisy) 0 prevy)))
                   ;;if end, just fill up to end with last good y
                   (progn
                     (loop for m from ,k to (1- j) do
                           (setf (aref ,freqs m) (if (zerop thisy) 0 prevy)))
                     (setf ,k j)
                     (loop-finish)))
                 )) ; end legit if
             ) ; end let
           )))

(defmacro ac-jumpup-filter (freqs freqsct max-jump-ratio 
                            max-filter-candidates k)
  `(let* ((max-candidates ,max-filter-candidates)
          (end ,freqsct)
          (endminus (1- end))
          (candend (1- (+ ,k max-candidates)))
          (debug NIL))  
     (progn
       (when debug
         (clm-print "   Handling jump up from zero.~%"))
       (loop for m from ,k do
             ;; if end of array reached, fill in zeroes & exit
             (when (= m endminus)
               (when debug
                 (clm-print "     End reached. Filling ~D to ~D with zero.~%" 
                            ,k m))
               (loop for n from ,k to m do
                     (setf (aref ,freqs n) 0))
               (setf ,k m)
               (loop-finish))
             ;; otherwise, test for the real plateau
             (let* ((thisy (aref ,freqs m))
                    (nexty (aref ,freqs (1+ m)))
                    (thisr (ac-get-ratio nexty thisy)))
               (when debug
                 (clm-print "     Plateau candidate x=~D, thisy ~F nexty ~F ratio ~F.~%" m thisy nexty thisr))
               (when (<= thisr ,max-jump-ratio)
                 ;; if plateau beyond current point, fill with zeroes
                 (if (< ,k m)
                   (progn
                     (when debug
                       (clm-print "     Setting ~D to ~D to zero~%" ,k (1- m)))
                     (loop for n from ,k to (1- m) do
                           (setf (aref ,freqs n) 0)))
                   (when debug
                     (clm-print "     Clean jump. No alterations needed~%")))
                 (setf ,k (1+ m))
                 (loop-finish)))
             ;; if we've hit the end of the candidates window, just increment k
             (when (= m candend)
               (when debug
                 (clm-print "    Max candidates exceeded.~%"))
               (incf ,k))
             ))))

(defmacro ac-filter-spikes (freqs freqsct max-jump-ratio max-filter-candidates)
  `(let ((k 1)
         (max-candidates ,max-filter-candidates)
         (debug NIL))
     (when debug
       (clm-print "~%Filter spikes: max-jump-ratio = ~F~%" ,max-jump-ratio))
     (loop while (< k ,freqsct) do
           (let* ((thisy (aref ,freqs k))
                  (prevy (aref ,freqs (1- k)))
                  (ratio (ac-get-ratio prevy thisy)))
             (when debug
               (clm-print "#~D thisy ~F prevy ~F ratio ~F~%" 
                          k thisy prevy ratio))
             ;; test legitimacy
             (if (or (and (zerop thisy) (zerop prevy))
                     (and (not (zerop ratio))
                          (<= ratio ,max-jump-ratio)))
               ;; if point is legitimate, increment and loop
               (incf k)
               ;; else if the last freq, set to previous freq
               (if (= k (1- ,freqsct))
                 (progn
                   (setf (aref ,freqs k) prevy)
                   (loop-finish))
                 ;; else look for corrections.
                 (if (and (plusp thisy) (zerop prevy))
                   (ac-jumpup-filter ,freqs ,freqsct 
                                     ,max-jump-ratio ,max-filter-candidates k)
                   (ac-check-candidates ,freqs ,freqsct ,max-jump-ratio 
                                        ,max-filter-candidates k)
                   )
                 ) ; end jump-up if
               ) ; end last-freq if
             ) ; end let*
           ) ; end main-legit if
     ))                              

(defmacro ac-makexyfromy (ylist &key (startx 0) (incrementx (/ 100)))
  `(loop for j from ,startx by ,incrementx
        for i in (cdr ,ylist)
        collect (float j) 
        collect (coerce i 'single-float)))

 
;;; -------------------------------------------
;;; MAIN AUTCORRELATION CODE
;;; -------------------------------------------

(defmacro ac-main 
    (fdr fdi fwi fftsize zcrosses zcrossct lowfq-warn-flag peaks peaksct peak)
  `(let ((fft2 (floor (/ fftsize 2)))
         (debug NIL))
     ;; apply window
     (dotimes (k ,fftsize)
       (setf (aref ,fdr k) (* (aref ,fdr k) (aref ,fwi k))))
     ;; take fft
     (fft ,fdr ,fdi ,fftsize)
     ;; calculate complex conjugate 
     (ac-complex-conjugate ,fdr ,fdi ,fftsize)
     ;; inverse fft
     (fft ,fdr ,fdi ,fftsize -1)
     ;; find zero-crossings
     (ac-find-zero-crossings ,fdr fft2 ,zcrosses ,zcrossct)
     (if debug (clm-print "~D total z-crossings~%" ,zcrossct))
     ;; remove brackets around minima
     (ac-remove-negmin-brackets ,fdr ,zcrosses ,zcrossct)
     (if debug 
         (clm-print "~D peak bracketing z-crossings~%" ,zcrossct))
     (when (and ,lowfq-warn-flag (= 0 ,zcrossct))
       (clm-print 
        "~%Warning: autoc found 0 peaks in at least 1 autocorrelation. db-floor may be too low.~%")
       (setf ,lowfq-warn-flag NIL))
     ;; find peaks between zeroes pairs
     (ac-find-peaks ,fdr ,zcrosses ,zcrossct ,peaks ,peaksct)
     (if debug (clm-print "~D total peaks~%" ,peaksct))
     ;; discard low amplitude peaks
     (ac-amp-find-max-peak ,fdr ,peaks ,peaksct debug)
     (if debug (clm-print "~F max peak~%" (aref ,peaks 0)))
     (setf ,peak (aref ,peaks 0))))

;;; -------------------------------------------
;;; INSTRUMENT DEFINITION AND POST-PROCESSING
;;; -------------------------------------------


(defscins autoc
    (file
     &key
     (beg 0.0) 
     (dur NIL)
     (rfreq 100) 
     (db-floor -30)
     (max-samp-offset 0)
     (max-ffts 1)
     (max-jump-cents 100) (max-filter-time 0.1)
     (min-freq 100) (max-freq 1000) (post-process T)
     (status T) (debug NIL))
  (let* ((fil (open-input* file)))
    (unwind-protect
         (let* 
             ((incr (/ rfreq))
              (fsr (sound-srate file))
              (incrsamps (round (* incr fsr)))
              (start (floor (* beg fsr)))
              (dur (if dur dur (- (sound-duration file) beg)))
              (end (+ start (floor (* dur fsr))))
              (fdr (make-double-float-array 4096))
              (fdi (make-double-float-array 4096))
              (fwi1024 (make-fft-window hamming-window 1024))
              (fwi2048 (make-fft-window hamming-window 2048))
              (fwi4096 (make-fft-window hamming-window 4096))
              (fftsizes (make-integer-array 3))
              fftsize
              (filptr 0)
              (fft2 (floor (/ 4096 2)))
              (zcrosses (make-double-float-array fft2))
              (zcrossct 0)
              (peaks (make-double-float-array fft2))
              (peaksct 0)
              (peak 0)
              (winfreq 0)
              (max-jump-ratio (if max-jump-cents
                                  (ac-cents->scaler max-jump-cents) NIL))
              (max-filter-candidates (* max-filter-time rfreq))
              (windows (floor (/ (- end start) incrsamps)))
              (freqs (make-double-float-array windows))
              (j 0)
              (lowfq-warn-flag T)
              (p-complete 0)
              (backlooks 3)
              (contig NIL)
              (finish NIL)
              (backstart 0)
              (closest NIL)
              (rms-divisor 0)
              (sum-of-squares 0)
              (rms 0)
              )
           (when (> max-ffts 3)
             (clm-print "~%max-fft's cannot be greater than 3. Setting to 3.~%")
             (setf max-ffts 3))
           (setf (aref fftsizes 0) 2048
                 (aref fftsizes 1) 1024
                 (aref fftsizes 2) 4096)
           (run*  
            (fdr freqs)
            (loop for i from start to end do
                 (when (and status (not debug))
                   (when (= i start)
                     (clm-print "~%autoc percent complete: "))
                   (let ((nc (round (* (/ (1+ j) windows) 100))))
                     (when (> nc p-complete)
                       (clm-print "~D " nc)
                       (setf p-complete nc))))
                 (if debug 
                     (clm-print 
                      "~%Window #~D, ~F sec., file frame ~D --------------~%" 
                      j (/ i fsr) i))
                 (setf closest NIL)
                 (setf contig T)
                 (setf backstart (max 1 (- j backlooks)))
                 (loop for b from backstart below j do
                      (let ((thisy (aref freqs (floor b)))
                            (lasty (aref freqs (floor (1- b)))))
                        (when debug
                          (clm-print 
                           "Compare win#~D, ~F and win#~D, ~F ratio = ~F cf ~F~%"
                           (1- b) lasty b thisy (ac-get-ratio lasty thisy) 
                           max-jump-ratio))
                        (when 
                            (or (or
                                 (< lasty min-freq)
                                 (> lasty max-freq))
                                (or
                                 (< thisy min-freq)
                                 (> thisy max-freq))
                                (> (ac-get-ratio lasty thisy) max-jump-ratio))
                          (when debug
                            (clm-print "Previous points not contiguous.~%"))
                          (setf contig NIL)
                          (loop-finish))))
                 (when (and debug contig)
                   (clm-print "Previous points contiguous.~%"))
                 (loop for os from 0 to max-samp-offset do
                      (if debug 
                          (clm-print 
                           "~%Trying sample offset ~D~%" os))
                      (setf filptr (+ i os))
                      (clear-array fdr)
                      (clear-array fdi)
                      (loop for f from 0 below max-ffts do
                           (setf fftsize (aref fftsizes f))
                           (if debug 
                               (clm-print 
                                "~%Trying fftsize ~D~%" fftsize))
                         ;; check amplitude
                           (setf rms-divisor (sqrt fftsize))
                           (setf sum-of-squares 0)
                           (loop for fp from filptr to (+ filptr fftsize) do
                                (setf sum-of-squares 
                                      (+ sum-of-squares (expt (ina fp fil) 2))))
                           (setf rms (autoc-linear->db 
                                      (sqrt (/ sum-of-squares rms-divisor))))
                           (when debug (clm-print "~F dB peak~%" rms))
                           (if (>= rms db-floor)
                               (let ((k 0))
                                 (loop for fp from filptr to (+ filptr fftsize)
                                    do
                                    (setf (aref fdr k) (ina fp fil))
                                    (incf k))
                                 ;; call the main autoc function
                                 (ac-main fdr fdi (case fftsize
                                                    (2048 fwi2048)
                                                    (1024 fwi1024)
                                                    (4096 fwi4096)) 
                                          fftsize zcrosses zcrossct
                                          lowfq-warn-flag peaks peaksct peak)
                                 )
                               ;; low amplitude peaks => freq = 0, exit
                               (progn
                                 (if debug 
                                     (clm-print "Window discarded due to low amplitude~%"))
                                 (setf winfreq 0)
                                 (loop-finish))) ; end amplitude-if
                         ;; calculate frequency
                           (setf winfreq (if (= peak 0) 0 (/ fsr peak)))
                           (when debug 
                             (clm-print "~F frequency~%" winfreq))
                         ;; check for continuity
                           (if closest
                               (let ((lasty (aref freqs (1- j))))
                                 (if (< (ac-get-ratio lasty winfreq)
                                        (ac-get-ratio lasty closest))
                                     (setf closest winfreq)))
                               (setf closest winfreq))
                           (when 
                               (and
                                (>= winfreq min-freq)
                                (<= winfreq max-freq)
                                (if contig
                                    (let ((lasty (aref freqs (1- j))))
                                      (<= (ac-get-ratio lasty winfreq) 
                                          max-jump-ratio))
                                    T))
                             (setf finish T)
                             (loop-finish))
                           )            ; end multi fft loop 
                      (when (and (not finish) (= os max-samp-offset))
                        (when debug
                          (clm-print 
                           "Resorting to zero~%"))
                        (setf winfreq 0))
                      (when finish
                        (setf finish NIL)
                        (loop-finish))
                      )                 ; end multi offset loop 

               ;; increment / housekeeping
                 (if debug (clm-print "~F final frequency~%" winfreq))
                 (setf (aref freqs j) winfreq)
                 (incf i incrsamps)
                 (incf j)
                 (when (>= i end)
                   (when post-process
                     (ac-filter-spikes 
                      freqs j max-jump-ratio max-filter-candidates)))
                 )                      ; end runloop
            )                           ;end run
           (close-input fil)
           (when status (format t "~%"))
           (setf *autoc-lastwin* (coerce fdr 'list))
           (ac-makexyfromy (coerce freqs 'list) :startx beg :incrementx incr)
           )                            ; end main let
      )                                 ; end unwind protect
    )                                   ; end outer let
  )                                     ; end definstrument



#|


(compile-file "/Users/bbattey/Studio/picacs/clm/autoc/autoc.ins")
(load "/Users/bbattey/Studio/picacs/clm/autoc/autoc")

(defparameter myfile 
  "/Users/bbattey/Studio/picacs/picacsdemo.aif")
(defparameter freqs '())
(progn
  (setf freqs (autoc myfile :post-process T))
  (gplotter (gplot-xy freqs))
)
 

(defun get-fund (file)
  (let* ((penv (autoc file))
         (y (loop for y in (cdr penv) by #'cddr collect y))
         (avg (/ (apply #'+ y) (length y))))
    (format t "~%average pitch: ~F~%" avg)
    (format t "~A + ~F hertz~%" 
            (cm::note avg :hz t) (- avg (cm::hertz (cm::note avg :hz t))))
    (format t "pitch range: ~F - ~F~%" (apply #'min y) (apply #'max y))))


|#
