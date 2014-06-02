(setf *default-pathname-defaults*
      (truename (sb-unix::posix-getenv "SC_RESOURCES_DIR")))
(load "cmn/cmn-all.lisp")
(load "clm-4/all.lisp")
(load "sbcl/lib/sbcl/asdf/asdf.fasl")
(load "sbcl/lib/sbcl/sb-posix/sb-posix.fasl")
(load "sbcl/lib/sbcl/sb-bsd-sockets/sb-bsd-sockets.fasl")
(load "sc/src/all.lsp")

(in-package :clm)
(unless (probe-file "clm-4/nrev.fasl")
  (compile-file "clm-4/nrev.ins"))
(load "clm-4/nrev.fasl")
(setf *clm-srate* 44100
      *clm-channels* 2
      ;; wave files by default (NB quicktime and itunes on macintel don't
      ;; understand 32bit float sound files (though CLM can play them).
       *clm-header-type* mus-riff
      ;; MDE Wed Apr 18 09:36:29 2012 -- no, aiffs like sean or
      ;; sc-test-full.lsp fails 
      ;;*clm-header-type* mus-aiff
      *clm-data-format* mus-lfloat
      *clm-play* nil
      ;; we might want to delete this automatically at logout...
      *clm-file-name* (format nil "~aDesktop/clm.wav" (user-homedir-pathname))
      ;; constant power panning
      *clm-locsig-type* mus-interp-sinusoidal
      *clm-reverb-channels* 2
      *clm-statistics* t
      *clm-delete-reverb* t)

(cl-user::sc)
