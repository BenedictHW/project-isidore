;;;; SPDX-FileCopyrightText: 2021 Hanshen Wang <Hanshen@HanshenWang.com>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(in-package #:cl-user) ; The buildpack requires cl-user package space.
#+sbcl (require :sb-posix) ; This is needed for "(uiop:getenv)".
(require "asdf") ; The recommended way from the manual to load bundled ASDF.

(format t "~%~&        ====== MAKE.LISP ======~%")
;; I also like to live dangerously.
(declaim (optimize (safety 0) (speed 3) (space 0) (debug 0) (compilation-speed 0)))
;; I do see a noticeable benefit of a 2 time reduction of memory usage both in
;; the local docker image (docker stats) and with load testing of the production
;; server (loader.io). It's a bit harder to test for performance, as I can't
;; interact with the Lisp image with the SBCL profiler or with the (time)
;; function.

;;; I. ENVIRONMENT VARIABLES
;;; Set environment variables if they cannot be found. When running the
;;; buildpack locally, i.e. with `*build-dir*', `*buildpack-dir*', `*cache-dir*'
;;; and `ql-dist-version' environment variables unset, one should
;;; run the shell command "sbcl --dynamic-space-size 512 --load make.lisp"
;;; while in the /bin project sub-directory.
(defvar *buildpack-dir* (if (uiop:getenvp "BUILDPACK_DIR")
                            (pathname-directory (pathname (concatenate 'string (uiop:getenv "BUILDPACK_DIR") "/")))
                            ;; Local BUILDPACK_DIR should be equal to
                            ;; /home/$USER/project-isidore/bin/
                            (pathname-directory (uiop:getcwd))))

(defvar *build-dir* (if (uiop:getenvp "BUILD_DIR")
                            (pathname-directory (pathname (concatenate 'string (uiop:getenv "BUILD_DIR") "/")))
                            ;; Local BUILD_DIR should be equal to
                            ;; /home/$USER/project-isidore/
                            (pathname-directory (uiop:pathname-parent-directory-pathname (uiop:getcwd)))))

(defvar *cache-dir* (if (uiop:getenvp "CACHE_DIR")
                            (pathname-directory (pathname (concatenate 'string (uiop:getenv "CACHE_DIR") "/")))
                            ;; Local CACHE_DIR should be equal to
                            ;; /home/$USER/ as setup.lisp can usually be
                            ;; found at /home/$USER/quicklisp/setup.lisp.
                            (pathname-directory (uiop:pathname-parent-directory-pathname
                                                 (uiop:pathname-parent-directory-pathname
                                                  (asdf:system-source-directory "quicklisp"))))))

;; Whitespace to enhance readability in Heroku logs.
(format t "~&        *BUILD-DIR* = ~a" (make-pathname :directory *build-dir*))
(format t "~&        *CACHE-DIR* = ~a" (make-pathname :directory *cache-dir*))
(format t "~&        *BUILDPACK-DIR* = ~a" (make-pathname :directory *buildpack-dir*))
(format t "~&        COMMON-LISP-ENV = ~a:~a (Provided ASDF version ~a) on ~a~%" (lisp-implementation-type)
        (lisp-implementation-version) (asdf:asdf-version) (machine-type))
(format t "~&        Fixnum bits:~a~%" (integer-length most-positive-fixnum))
;; Compile Hunchentoot without SSL support.
(pushnew :hunchentoot-no-ssl *features*)
(format t "~&        Features = ~a~%" *features*)

;;; II. ASDF CONFIGURATION
;;; Notify ASDF of the source code location.

;; Tell ASDF to store binaries in the cache dir.
(setf (uiop:getenv "XDG_CACHE_HOME") (concatenate
                                      'string (uiop:getenv "CACHE_DIR") "/.asdf/"))

;; Notify ASDF that our build and cache dir is an awesome place to find '.asd'
;; files.
(asdf:initialize-source-registry `(:source-registry
                                   (:tree ,(make-pathname :directory
                                                          *build-dir*))
                                   (:tree ,(make-pathname :directory
                                                          *cache-dir*))
                                   :inherit-configuration))

;;; III. QUICKLISP CONFIGURATION
;;; Because we cannot assume the existence of a ".sbclrc" initialization file,
;;; we need to either load an already existent/cached setup.lisp or load the
;;; downloaded quicklisp.lisp.

;; There may not be the luxury of having Quicklisp already added to the .sbclrc
;; initialization file to automatically load the quicklisp system into the lisp
;; image at startup. If `*cache-dir*'/quicklisp exists, load SETUP.LISP.
(let ((ql-setup (make-pathname :directory (append *cache-dir* '("quicklisp"))
                               :defaults "setup.lisp"))
      (ql-dist-version nil))
  (if (probe-file ql-setup)
      (load ql-setup)
      ;; Otherwise install quicklisp by loading the downloaded quicklisp.lisp.
      (progn
        (load (make-pathname :directory (append *buildpack-dir* '("lib"))
                             :defaults "quicklisp.lisp"))
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

;;; IV. TESTING
;; Load Application into Lisp image.
(ql:quickload "project-isidore")

;; Load separate Project Isidore test system.
(ql:quickload "project-isidore-test")

;; Run Application tests.
(asdf:test-system "project-isidore")

;;; V. BUILDING
;; Initialize bible dataset and indexes sequentially as the search index is
;; dependent on the bible dataset being already loaded in memory.

(format t "~&        ====== Build Successful | Deo Gratias ======~%")

(format t "~&        ====== END OF MAKE.LISP ======~%")

;; SBCL's garbage collector is conservative. Manually call garbage collector
;; after allocating large amounts due to `create-search-index'.
#+sbcl (sb-ext:gc :full t)

;; Dump image. For details go to src/project-isidore.asd.
(asdf:make "project-isidore")

;; SBCL's `save-lisp-and-die' unsurprisingly kills the lisp process at the end.
;; However this behaviour is implementation dependent. This command is here in
;; case it does not kill the lisp process.
(uiop:quit)
