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

(in-package :cl-user)
(require 'asdf)

(flet ((env-to-dirs (x)
         (pathname-directory (pathname (concatenate 'string
						    #+sbcl (sb-posix:getenv x)
						    #+ccl (getenv x) "/")))))
  (defvar *build-dir* (env-to-dirs "BUILD_DIR"))
  (defvar *cache-dir* (env-to-dirs "CACHE_DIR"))
  (defvar *buildpack-dir* (env-to-dirs "BUILDPACK_DIR")))

(format t "~&  *build-dir* = ~a" (make-pathname :directory *build-dir*))
(format t "~&  *cache-dir* = ~a" (make-pathname :directory *cache-dir*))
(format t "~&  *buildpack-dir* = ~a" (make-pathname :directory *buildpack-dir*))

;;; Tell ASDF to store binaries in the cache dir.
#+ccl (ccl:setenv "XDG_CACHE_HOME" (concatenate 'string (getenv "CACHE_DIR") "/.asdf/"))
#+sbcl (sb-posix:setenv "XDG_CACHE_HOME" (concatenate 'string (sb-posix:getenv "CACHE_DIR")
						     "/.asdf/") 1)

;;; Notify ASDF that our build and cache dir is an awesome place to find asf files.
(asdf:initialize-source-registry `(:source-registry
                                    (:tree ,(make-pathname :directory *build-dir*))
                                    (:tree ,(make-pathname :directory *cache-dir*))
                                    :inherit-configuration))

;;; App can redefine this to do runtime initializations
(defun initialize-application (&key port)
  (declare (ignore port)))

;;; Default toplevel, app can redefine.
(defun heroku-toplevel ()
  (initialize-application :port (parse-integer  #+sbcl (sb-posix:getenv "PORT")
						#+ccl (getenv "PORT")))
  (loop (sleep 600)))			;sleep forever

(defvar *root* "/app")			;this is always the app root on Heroku now?

;;; Run the app's own build.
(format t "~&* Load application's heroku-compile.lisp ")
(load (make-pathname :directory *build-dir* :defaults "heroku-compile.lisp"))

;;; Save the application as an image
(let ((app-file (make-pathname :directory *build-dir* :defaults "lispapp")))
  ;; note that the buildpack's bin/release refers to this application name.
  (format t "~&* create slug's ./lispapp via save-application")
  #+ccl (save-application app-file
			  :prepend-kernel t
			  :toplevel-function #'heroku-toplevel)
  #+sbcl (save-lisp-and-die app-file
			    :toplevel #'heroku-toplevel
			    :executable t))
