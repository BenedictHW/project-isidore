;;;; heroku.lisp
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

(in-package #:cl-user)

;;; application entry point
(defvar *acceptor* nil)
;; Takes a PORT parameter as Heroku assigns a different PORT per environment
(defun initialize-application (&key port)
  (project-isidore:generate-index-css "/app/assets/index.css")
  (project-isidore:generate-global-css "/app/assets/global.css")
  (project-isidore:generate-index-js :input "index.lisp" :output "/app/assets/index.js")
  (setf hunchentoot:*dispatch-table*
        ;; for an explanation of ` and , and '. see https://stackoverflow.com/questions/60378335/quote-comma-in-common-lisp
        `(hunchentoot:dispatch-easy-handlers
          ,(hunchentoot:create-folder-dispatcher-and-handler ; Requires full system path
                                                    "/" "/app/assets/"))) ; /app is the root on a heroku filesystem
  (when *acceptor*
    (hunchentoot:stop *acceptor*))
  (setf *acceptor*
        (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port port))))
