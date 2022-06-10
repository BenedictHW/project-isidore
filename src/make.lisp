;;;; SPDX-FileCopyrightText: 2021 Benedict Hanshen Wang <Admin@BenedictHanshenWang.com>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

;; Load this file inside a shell to build a standalone executable binary,
;; "sbcl --load /path/to/this/file.lisp"

(in-package #:cl-user) ; The buildpack requires cl-user package space.
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
                            (pathname-directory (uiop:pathname-parent-directory-pathname (uiop:getcwd)))))

(defvar *cache-dir* (if (uiop:getenvp "CACHE_DIR")
                            (pathname-directory (pathname (concatenate 'string (uiop:getenv "CACHE_DIR") "/")))
                            ;; Local CACHE_DIR should be similar to
                            ;; /home/$USER/ as setup.lisp can usually be
                            ;; found at /home/$USER/quicklisp/setup.lisp.
                            (pathname-directory (uiop:pathname-parent-directory-pathname
                                                 (uiop:pathname-parent-directory-pathname
                                                  (asdf:system-source-directory "quicklisp"))))))

;; Whitespace to enhance readability in Heroku logs.
(format t "~&        *BUILD-DIR* = ~a" (make-pathname :directory *build-dir*))
(format t "~&        *CACHE-DIR* = ~a" (make-pathname :directory *cache-dir*))
(format t "~&        COMMON-LISP-ENV = ~a:~a (Provided ASDF version ~a) on ~a~%" (lisp-implementation-type)
        (lisp-implementation-version) (asdf:asdf-version) (machine-type))
(format t "~&        Fixnum bits:~a~%" (integer-length most-positive-fixnum))
(format t "~&        Features = ~a~%" *features*)

;;; II. ASDF CONFIGURATION
;;; See ASDF manual section 4.1 Configuring ASDF to find your systems.
(asdf:initialize-source-registry `(:source-registry
                                   (:tree ,(make-pathname :directory
                                                          *build-dir*))
                                   (:tree ,(make-pathname :directory
                                                          *cache-dir*))
                                   :inherit-configuration))

;;; III. QUICKLISP CONFIGURATION
;;; Because we cannot assume the existence of a ".sbclrc" initialization file
;;; which loads quicklisp into the lisp image, we need to either load an already
;;; existent/cached setup.lisp or load the downloaded quicklisp.lisp.
(let ((ql-setup (make-pathname :directory (append *cache-dir* '("quicklisp"))
                               :defaults "setup.lisp"))
      (ql-dist-version nil))
  ;; Load `*cache-dir*'/quicklisp/setup.lisp exists if it exists.
  (if (probe-file ql-setup)
      (load ql-setup)
      ;; Otherwise install quicklisp by loading the downloaded quicklisp.lisp.
      (progn
        (load (make-pathname :directory *build-dir* :defaults "quicklisp.lisp"))
        (funcall (symbol-function
                  (find-symbol "INSTALL" (find-package "QUICKLISP-QUICKSTART")))
                 :path (make-pathname :directory (pathname-directory
                                                  ql-setup)))
        ;; Install distribution as specified by `ql-dist-version'.
        (setf ql-dist-version (if (uiop:getenvp "QL_DIST_VER")
                                  (uiop:getenv "QL_DIST_VER")
                                  (funcall (symbol-function
                                            (find-symbol "DIST-VERSION" (find-package "QL-DIST")))
                                           "quicklisp")))
        (funcall (symbol-function
                  (find-symbol "INSTALL-DIST" (find-package "QL-DIST")))
                 (format nil "http://beta.quicklisp.org/dist/quicklisp/~A/distinfo.txt" ql-dist-version)
                 :replace t :prompt nil))))

;;; IV. BUILDING
;; Nginx is used for SSL Termination and as a Reverse Proxy.
(push :hunchentoot-no-ssl *features*)
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
