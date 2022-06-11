;;;; SPDX-FileCopyrightText: 2021 Benedict Hanshen Wang <Admin@BenedictHanshenWang.com>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

;; Load this file inside a shell to build a standalone executable binary,

;; To use your local .sbclrc which possibly contains lines `ql:add-to-init-file'.

;; "sbcl --load /path/to/this/file.lisp"

;; Bypass any SBCL Initialization Files on Production deployments with,
;; http://www.sbcl.org/manual/#Command-Line-Options

;; "sbcl --dynamic-space-size 2048 --control-stack-size 10 --no-userinit
;; --no-sysinit --load /path/to/this/file.lisp"

(in-package #:cl-user) ; The buildpack requires cl-user package space.
;; See weitzCommonLispRecipes2016, page 102.
(setf *read-default-float-format* 'double-float)
#+sbcl (require :sb-posix) ; This is needed for "(uiop:getenv)".
(require "asdf") ; The recommended way from the manual to load bundled ASDF.

(format t "~%~&        ====== MAKE.LISP ======~%")
;; I also like to live dangerously.
(declaim (optimize (safety 2) (speed 3) (space 0) (debug 0) (compilation-speed 0)))

;;; I. ENVIRONMENT VARIABLES
(defvar *build-dir* (if (uiop:getenvp "BUILD_DIR")
                            (pathname-directory (pathname (concatenate 'string (uiop:getenv "BUILD_DIR") "/")))
                            ;; Local BUILD_DIR should be similar to
                            ;; /home/$USER/quicklisp/local-projects/project-isidore/
                            (pathname-directory (uiop:getcwd))))

;;; II. ASDF CONFIGURATION
;;; See ASDF manual section 4.1 Configuring ASDF to find your systems.

;; Upgrade > ASDF 3.3.5 to use package local nicknames with package inferred
;; systems. See commit 1962a26.
(load (make-pathname :directory (append *build-dir* '("lib"))
                     :defaults "upgrade-asdf.lisp"))
(provide "asdf")
(asdf:initialize-source-registry `(:source-registry
                                   (:tree ,(make-pathname :directory
                                                          *build-dir*))
                                   :inherit-configuration))

;;; III. QUICKLISP CONFIGURATION
;;; On SBCL MAKE.LISP is called with --no-userinit and --no-sysinit. We cannot
;;; assume the existence of a ".sbclrc" initialization file which loads
;;; quicklisp into the lisp image.
(let ((ql-setup
        ;; Return existing quicklisp system pathname if located with ASDF.
        (when (asdf:find-system "quicklisp" nil)
          (uiop:merge-pathnames*
           (asdf:system-source-directory "quicklisp") "setup.lisp")))
      ;; Needs quicklisp system to set valid value.
      (ql-dist-version nil))
  (if (pathnamep ql-setup)
      ;; Load already existent "quicklisp/setup.lisp".
      (load ql-setup)
      ;; No prior quicklisp system found, load
      ;; project-isidore/lib/quicklisp.lisp and install in present subdirectory.
      (progn
        (load (make-pathname :directory (append *build-dir* '("lib"))
                             :defaults "bootstrap-quicklisp.lisp"))
        (funcall (symbol-function
                  (find-symbol "INSTALL" (find-package "QUICKLISP-QUICKSTART")))
                 :path (make-pathname :directory (append *build-dir* '("lib"))))
        ;; Install distribution as specified by ENVIRONMENT VARIABLE
        ;; `QL_DIST_VER'. Fallback to (ql-dist::dist-version "quicklisp") or
        ;; locate distribution; (ql::available-versions (ql::dist "quicklisp"))
        (setf ql-dist-version (if (uiop:getenvp "QL_DIST_VER")
                                  (uiop:getenv "QL_DIST_VER")
                                  (funcall (symbol-function
                                            (find-symbol "DIST-VERSION" (find-package "QL-DIST")))
                                           "quicklisp")))
        (if (rest (assoc ql-dist-version
                         (funcall (symbol-function
                                   (find-symbol "AVAILABLE-VERSIONS" (find-package "QUICKLISP")))
                                  (funcall (symbol-function
                                            (find-symbol "DIST" (find-package "QUICKLISP")))
                                           "quicklisp"))
                         :test #'string-equal))
            (funcall (symbol-function
                      (find-symbol "INSTALL-DIST" (find-package "QL-DIST")))
                     (rest (assoc ql-dist-version
                                  (funcall (symbol-function
                                            (find-symbol "AVAILABLE-VERSIONS" (find-package "QUICKLISP")))
                                           (funcall (symbol-function
                                                     (find-symbol "DIST" (find-package "QUICKLISP")))
                                                    "quicklisp"))
                                  :test #'string-equal))
                     :replace t :prompt nil)
            (format t "~&        ~a is an invalid QL_DIST_VER" ql-dist-version)))))

;;; IV. BUILDING
;; Nginx is used for SSL Termination and as a Reverse Proxy.
(push :hunchentoot-no-ssl *features*)
;; Whitespace to enhance readability in Heroku logs.
(format t "~&        *BUILD-DIR* = ~a" (make-pathname :directory *build-dir*))
(format t "~&        COMMON-LISP-ENV = ~a:~a (Provided ASDF version ~a) on ~a~%"
        (lisp-implementation-type) (lisp-implementation-version)
        (asdf:asdf-version) (machine-type))
(format t "~&        FIXNUM BITS:~a~%" (integer-length most-positive-fixnum))
(format t "~&        FEATURES = ~a~%" *features*)
(format t "~&        QL CLIENT ~a on DIST VER ~a~%"
        (ql:client-version)
        (ql::version (ql::dist "quicklisp")))
(ql:quickload "project-isidore/test")
(if (asdf:test-system "project-isidore")
    (progn
      ;; Attempt to prune LISP image of testing artifacts to save space?
      ;; More prudent to ensure tests are side effect free...
      ;; (loop for system in
      ;;       '(:parachute :dexador :project-isidore-test/tests)
      ;;       do (progn
      ;;            (delete-package system)
      ;;            (asdf:clear-system system)))
      (format t "~&        ====== Build Successful | Deo Gratias ======~%")
      (format t "~&        ====== END OF MAKE.LISP ======~%")
      (asdf:make "project-isidore") ; see "project-isidore.asd" for more.
      ;; SBCL's `save-lisp-and-die' kills the lisp process at the end. However this
      ;; behaviour is implementation dependent. This command is here in case it does
      ;; not kill the lisp process.
      (uiop:quit 0))
    (progn
      (format t "~&        ====== Fix Failed Tests | Build Cancelled ======~%")
      (format t "~&        ====== END OF MAKE.LISP ======~%")
      (uiop:quit 100)))
