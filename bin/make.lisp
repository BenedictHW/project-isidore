;;;; SPDX-FileCopyrightText: 2021 Hanshen Wang <Hanshen@HanshenWang.com>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(in-package #:cl-user) ; buildpack requires cl-user package space
(require :sb-posix)
(require "asdf") ; recommended way from manual to load ASDF

;; HARDCODED PATHNAME
(defvar *production-buildp* nil
  "Identify build as local or production based off of pathname comparison.
  The matched directory is hardcoded to the author's personal working directory.
  MAKE.LISP is intended to be called from the shell via 'sbcl --load
  make.lisp'")

(format t "~&        ====== MAKE.LISP ======~%")
(if (probe-file "/home/hanshen/project-isidore/bin/make.lisp")
    (setf *production-buildp* nil)
    (setf *production-buildp* t))
(format t "~&        Is this a production build? ~a ~%" *production-buildp*)

;;; Setup Production Environment
(when (equalp *production-buildp* t)
  (flet ((env-to-dirs (x)
           (pathname-directory (pathname (concatenate 'string (uiop:getenv
           x) "/")))))
    (defvar *buildpack-dir* (env-to-dirs "BUILDPACK_DIR"))
    (defvar *build-dir* (env-to-dirs "BUILD_DIR"))
    (defvar *cache-dir* (env-to-dirs "CACHE_DIR"))
    (defvar *quicklisp-dist-version* (uiop:getenv "QL_DIST_VER")))

  ;; Whitespace to enhance readability in Heroku logs
  (format t "~&        *BUILD-DIR* = ~a" (make-pathname :directory *build-dir*))
  (format t "~&        *CACHE-DIR* = ~a" (make-pathname :directory *cache-dir*))
  (format t "~&        *BUILDPACK-DIR* = ~a~%" (make-pathname :directory
  *buildpack-dir*))

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

  ;; Install Quicklisp
  ;; If `*cache-dir*'exists, load SETUP.LISP.
  (let ((ql-setup (make-pathname :directory (append *cache-dir* '("quicklisp"))
  :defaults "setup.lisp")))
    (if (probe-file ql-setup)
        (load ql-setup)
        (progn
          (load (make-pathname :directory (append *buildpack-dir* '("lib"))
          :defaults "quicklisp.lisp"))
          (funcall (symbol-function
                    (find-symbol "INSTALL" (find-package "QUICKLISP-QUICKSTART")))
                   :path (make-pathname :directory (pathname-directory
                   ql-setup)))
          (funcall (symbol-function
                    (find-symbol "INSTALL-DIST" (find-package "QL-DIST")))
                   (format
                   nil "http://beta.quicklisp.org/dist/quicklisp/~A/distinfo.txt"
                   *quicklisp-dist-version*)
                   :replace t :prompt nil)))))

;;; Load Application into Lisp image.
(ql:quickload :project-isidore)

;;; Run Application tests.
(ql:quickload :project-isidore-test)
(asdf:test-system :project-isidore)

(defun application-toplevel ()
  "Application entry point. Emulate a \"main\" function. Used in
  SAVE-LISP-AND-DIE to save Application as a Lisp image. Note PORT is a keyword
  argument that defaults to 8080. Heroku dynamically sets the PORT variable to
  be binded."
  (project-isidore:initialize-application
   :port (if (equalp NIL (uiop:getenv "PORT"))
             8080
             (parse-integer (uiop:getenv "PORT")))
   :dispatch-folder "assets/"
   :cmd-user-interface t)
  (loop (sleep 600))) ; sleep forever

;;; Save the application as an image
;; Heroku buildpack's bin/release refers to ./ProjectIsidore as the application
;; name. Store binary locally under /project-isidore/ for local builds.
(when (equalp *production-buildp* nil)
  (defvar *build-dir*
    (pathname-directory
     (pathname (asdf:system-relative-pathname :project-isidore "../")))))


(format t "~&        ====== Build Successful | Deo Gratias ======~%")

(format t "~&        ====== END OF MAKE.LISP ======~%")

(let ((app-file (make-pathname :directory *build-dir*
                               :defaults "ProjectIsidore")))
  (progn (project-isidore:create-datastore)
         (project-isidore:create-search-index)
         (sb-ext:save-lisp-and-die app-file
                                   :toplevel #'cl-user::application-toplevel
                                   :executable t)))
(uiop:quit)
