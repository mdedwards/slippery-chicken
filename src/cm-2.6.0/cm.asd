;;;
;;; ASDF system definition file.
;;; For information on ASDF see: http://www.cliki.net/asdf
;;;
;;; To load CM from a non-standard install location:
;;;
;;; (require 'asdf)
;;; (pushnew #p"/Lisp/cm/" asdf:*central-registry* :test #'equal)
;;; (asdf:operate 'asdf:load-op 'cm)
;;;
;;; To download/install/load CM from its archive:
;;;
;;; (require 'asdf)
;;; #+SBCL (require 'asdf-install)
;;; #-SBCL
;;; (progn (pushnew "/path/to/asdf-install/" asdf:*central-registry*)
;;;        (asdf:operate 'asdf:load-op 'asdf-install))
;;; (asdf-install:install 'cm)
;;; (asdf:operate 'asdf:load-op 'cm)
;;;

(defsystem "cm"
    :description "Common Music"
    :version "2.6.0"
    :author "Rick Taube <taube@uiuc.edu>"
    :licence "GPL"
    :components ((:module "src" :components ((:file "cm"))
                          :perform (load-op :after (op c)
                                            (cl-user::cm)))))

