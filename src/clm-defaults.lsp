(in-package :clm)

(setf *clm-srate* 44100
      *clm-channels* 2
      ;; wave files by default (NB quicktime and itunes on macintel don't
      ;; understand 32bit float sound files (though CLM can play them).
      *clm-header-type* mus-riff
      *clm-play* nil
      ;; MDE Wed Apr 18 09:36:29 2012 -- no, aiffs like sean or
      ;; sc-test-full.lsp fails 
      ;;clm-header-type mus-aiff
      *clm-data-format* mus-lfloat
      ;; we might want to delete this automatically at logout...
      *clm-file-name*
      (concatenate 'string
                   (slippery-chicken::get-sc-config 'default-dir)
                   "clm-"
                   (slippery-chicken::now-string)
                   ".wav")
      ;; MDE Wed Mar 20 09:43:40 2024, Heidhausen --  this is the new variable I
      ;; suggested to Bill so that we can define in which directory .c. and .o
      ;; files should be written. This way we can avoid having to use decscins
      ;; and /or :c-file strategies in instruments that may not even belong to
      ;; us 
      *clm-c-directory* (sc::get-sc-config 'tmp-dir)
      ;; constant power panning
      *clm-locsig-type* mus-interp-sinusoidal
      *clm-reverb-channels* 2
      *clm-statistics* nil
      *clm-delete-reverb* t)
