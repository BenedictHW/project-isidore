;;;; compile.lisp
;;;
;;; Copyright (c) 2021 Hanshen Wang.
;;;
;;; Author: Hanshen Wang <Hanshen@HanshenWang.com>
;;; URL: https://github.com/HanshenWang/project-isidore
;;;
;;; This file is part of Project Isidore.
;;;
;;; Project Isidore is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Project Isidore is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Project Isidore.  If not, see <https://www.gnu.org/licenses/>.

(in-package #:cl-user) ; buildpack requires cl-user package space
(require :sb-posix)
(require 'asdf)

(defvar *production-buildp*
  (if (probe-file "/home/hanshen/project-isidore/bin/compile.lisp")
      (setf *production-buildp* nil) (setf *production-buildp* t))
  "Identify build as local/production (remote) based off of pathname comparison.
  The matched directory is hardcoded to the author's personal working
  directory.")

(format t "~&        ====== COMPILE.LISP ======~%")

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
  (format t "~&        *build-dir* = ~a" (make-pathname :directory *build-dir*))
  (format t "~&        *cache-dir* = ~a" (make-pathname :directory *cache-dir*))
  (format t "~& *buildpack-dir* = ~a~%" (make-pathname :directory
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

;;; Run the app's own build.
(ql:quickload :project-isidore)

(defun application-toplevel ()
  "Application entry point. Emulate a \"main\" function. Used in
  SAVE-LISP-AND-DIE to save Application as an Lisp image."
  ;; Set PORT for local builds or it will return NIL
  (when (equalp *production-buildp* nil) (setf (uiop:getenv "PORT") "8080"))
  (project-isidore:initialize-application
   :port (parse-integer (uiop:getenv "PORT"))
   :dispatch-folder "assets/")
  (loop (sleep 600))) ; sleep forever


;;; Save the application as an image
;; buildpack's bin/release refers to ./lispapp as the application name.
;; store binary locally under /project-isidore/bin/
(when (equalp *production-buildp* nil)
  (defvar *build-dir*
    (pathname-directory
     (pathname (asdf:system-relative-pathname :project-isidore "bin/")))))

(let ((app-file (make-pathname :directory *build-dir*
                               :defaults "lispapp")))
  (sb-ext:save-lisp-and-die app-file
                            :toplevel #'cl-user::application-toplevel
                            :executable t))

(format t "~&        ====== END OF COMPILE.LISP ======~%")
