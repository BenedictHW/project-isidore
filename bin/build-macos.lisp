;;;; build.lisp
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
(defvar *quicklisp-dist-version* "2021-10-21")


;;; Setup Production Environment
;; Notify ASDF that our build and cache dir is an awesome place to find '.asd'
;; files.
(asdf:initialize-source-registry `(:source-registry
                                   (:tree ,(make-pathname :directory "/Users/runner/work/project-isidore/project-isidore/"))
                                   :inherit-configuration))
;; Load Quicklisp
(let ((ql-setup (make-pathname :directory "/Users/runner/work/project-isidore/project-isidore/quicklisp" :defaults "setup.lisp")))
  (if (probe-file ql-setup)
      (load ql-setup)
      (progn
        (load (make-pathname :directory
                             "/Users/runner/work/project-isidore/project-isidore/"
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
                 :replace t :prompt nil))))

;;; Run the app's own build.
(ql:quickload :project-isidore)

(defun application-toplevel ()
  "Application entry point. Emulate a \"main\" function. Used in
  SAVE-LISP-AND-DIE to save Application as an Lisp image."
  ;; Set PORT for local builds or it will return NIL
  (setf (uiop:getenv "PORT") "8080")
  (project-isidore:initialize-application
   :port 8080
   :dispatch-folder "assets/"
   :cmd-user-interface t)
  (loop (sleep 600))) ; sleep forever


;;; Save the application as an image

(let ((app-file (make-pathname :directory "/Users/runner/work/project-isidore/project-isidore/"
                               :defaults "ProjectIsidore")))
  (sb-ext:save-lisp-and-die app-file
                            :toplevel #'cl-user::application-toplevel
                            :executable t))
