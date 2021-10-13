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

;;; Identify if this build is local/production (remote)
;;; Change to nil if building locally
(defconstant +productionp+ t)

(format t "~&        ====== COMPILE.LISP ======")

;;; Setup Production Environment
(when (equalp +productionp+ t)
  (flet ((env-to-dirs (x)
           (pathname-directory (pathname (concatenate 'string (sb-posix:getenv x) "/")))))
    (defvar *buildpack-dir* (env-to-dirs "BUILDPACK_DIR"))
    (defvar *build-dir* (env-to-dirs "BUILD_DIR"))
    (defvar *cache-dir* (env-to-dirs "CACHE_DIR"))
    (defvar *quicklisp-dist-version* (sb-posix:getenv "QL_DIST_VER")))

  ;; Whitespace to enhance readability in Heroku logs
  (format t "~&        *build-dir* = ~a" (make-pathname :directory *build-dir*))
  (format t "~&        *cache-dir* = ~a" (make-pathname :directory *cache-dir*))
  (format t "~&        *buildpack-dir* = ~a~%" (make-pathname :directory *buildpack-dir*))

  ;; Tell ASDF to store binaries in the cache dir.
  (sb-posix:setenv "XDG_CACHE_HOME" (concatenate 'string (sb-posix:getenv "CACHE_DIR")
                                                 "/.asdf/") 1)

  ;; Notify ASDF that our build and cache dir is an awesome place to find '.asd' files.
  (asdf:initialize-source-registry `(:source-registry
                                     (:tree ,(make-pathname :directory *build-dir*))
                                     (:tree ,(make-pathname :directory *cache-dir*))
                                     :inherit-configuration))

  ;; Install Quicklisp
  (let ((ql-setup (make-pathname :directory (append *cache-dir* '("quicklisp")) :defaults "setup.lisp")))
    (if (probe-file ql-setup)
        (load ql-setup)
        (progn
          (load (make-pathname :directory (append *buildpack-dir* '("lib")) :defaults "quicklisp.lisp"))
          (funcall (symbol-function (find-symbol "INSTALL" (find-package "QUICKLISP-QUICKSTART")))
                   :path (make-pathname :directory (pathname-directory ql-setup)))
          (funcall (symbol-function (find-symbol "INSTALL-DIST" (find-package "QL-DIST")))
                   (format nil "http://beta.quicklisp.org/dist/quicklisp/~A/distinfo.txt" *quicklisp-dist-version*)
                   :replace t :prompt nil)))))

;;; Run the app's own build.
(ql:quickload :project-isidore)

(defvar *acceptor* nil)

(defvar *root* "/app" "This is always the app root on Heroku.")

(defun application-toplevel ()
  (when (equalp +productionp+ nil)
    (sb-posix:setenv "PORT" "8080" 0)) ; or PORT will return NIL
  "Application entry point. Emulate a \"main\" function. Used in
  SAVE-LISP-AND-DIE to save Application as an Lisp image."
  (project-isidore:initialize-application :productionp t
  :port (parse-integer (sb-posix:getenv "PORT")) :dispatch-folder "assets/")
  (loop (sleep 600))) ; sleep forever


;;; Save the application as an image
;; buildpack's bin/release refers to ./lispapp as the application name.
(when (equalp +productionp+ nil)
  (setf *build-dir* "bin/")) ; local binary stored under

(let ((app-file (make-pathname :directory *build-dir* :defaults "lispapp")))
  (save-lisp-and-die app-file :toplevel #'application-toplevel :executable t))

(format t "~&        ====== END OF COMPILE.LISP ======")
