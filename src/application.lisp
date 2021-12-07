;;;; application.lisp
;;;
;;; Copyright (c) 2021 Hanshen Wang.
;;;
;;; Author: Hanshen Wang <Hanshen@HanshenWang.com>
;;; URL: https://github.com/HanshenWang/project-isidore
;;;
;;; This file is part of Project Isidore.
;;;
;;; Project Isidore is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by the Free
;;; Software Foundation, either version 3 of the License, or (at your option)
;;; any later version.
;;;
;;; Project Isidore is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
;;; more details.
;;;
;;; You should have received a copy of the GNU General Public License along with
;;; Project Isidore. If not, see <https://www.gnu.org/licenses/>.

(defpackage #:project-isidore/application
  (:use #:common-lisp
        #:project-isidore/model
        #:project-isidore/views)
  (:import-from #:hunchentoot)
  (:import-from #:log4cl)
  ;; No package local nicknames. See commit 1962a26.
  (:export #:initialize-application
           #:terminate-application
           #:list-project-isidore-dependencies)
  (:documentation
   "Starting the Web server for the application is defined by INITIALIZE-APPLICATION.
  This package also contains URI handler and logic. A rough mapping onto the
  Model View Controller (MVC) design pattern."))

(in-package #:project-isidore/application)

;;; Project Isidore web routing. The :uri keyword of define-easy-handler
;;; maps to DOMAINNAME/HOST as such: /about maps to http://localhost:8080/about
;;; or https://hanshenwang.com/about
(hunchentoot:define-easy-handler (root :uri "/") ()
  (index-page))

(hunchentoot:define-easy-handler (about :uri "/about") ()
  (about-page))

(hunchentoot:define-easy-handler (work :uri "/work") ()
  (work-page))

(hunchentoot:define-easy-handler (contact :uri "/contact") ()
  (contact-page))

(hunchentoot:define-easy-handler (subscribe :uri "/subscribe") ()
  (subscribe-page))

(hunchentoot:define-easy-handler (create-subscriber :uri "/create-subscriber") ()
  (unless (hunchentoot:parameter "friend-email") (hunchentoot:redirect "/subscribe"))
  (let ((title (hunchentoot:parameter "friend-title"))
        (name (hunchentoot:parameter "friend-name"))
        (email (hunchentoot:parameter "friend-email")))
    (mailinglist-add title name email)
    (subscribe-success-page email)))

(hunchentoot:define-easy-handler (unsubscribe :uri "/unsubscribe") ()
  (unsubscribe-page))

(hunchentoot:define-easy-handler (delete-subscriber :uri "/delete-subscriber") ()
  (unless (hunchentoot:parameter "friend-email") (hunchentoot:redirect "/unsubscribe"))
  (let ((email (hunchentoot:parameter "friend-email")))
    (mailinglist-delete email)
    (unsubscribe-success-page email)))

(hunchentoot:define-easy-handler (view-bible :uri "/bible") (verses)
  ;; HTTP response header is needed.
  (setf (hunchentoot:content-type*) "text/html")
  ;; localhost:8080/bible?verses=1-2-3-4-5-6
  (bible-page verses))

(defvar *acceptor* nil "To be used in INITIALIZE-APPLICATION to create an
instance of class HUNCHENTOOT:ACCEPTOR to listen to a PORT")

(defun initialize-application (&key (port 8080)
(dispatch-folder (asdf:system-relative-pathname :project-isidore "../assets/"))
(cmd-user-interface nil))
  "Start a web server at PORT. Set DATABASE_URL to connect to PostgreSQL
database. Optional PORT,DISPATCH-FOLDER and CMD-USER-INTERFACE. Takes a PORT
parameter as Heroku assigns a different PORT per dyno/environment.
DISPATCH-FOLDER determines from which URL static assets will be served.
CMD-USER-INTERFACE when set to true will determine if C-c will exit. See
APPLICATION-TOPLEVEL for the main function or entry point in MAKE.LISP. "
  (log4cl:log-info "

========================================
Project Isidore v1.1.0 (A.D. 2021-10-20)
========================================

Copyright 2021 Hanshen Wang <Hanshen@HanshenWang.com>

Project Isidore is free software, provided as is, and comes with ABSOLUTELY NO WARRANTY.
This program is licensed under the GNU LESSER GENERAL PUBLIC LICENSE Version 3, 29 June 2007
You are welcome to redistribute the program or any parts hereof under certain conditions.
Please visit https://www.gnu.org/licenses/lgpl-3.0.en.html for License details.

Homepage: https://www.hanshenwang.com/blog/project-isidore-doc.html
")
  (when (uiop:getenv "DATABASE_URL")
    (setf *database-url* (uiop:getenv "DATABASE_URL")))
  (setf (cl-who:html-mode) :HTML5
        hunchentoot:*show-lisp-errors-p* t
        hunchentoot:*dispatch-table*
        `(hunchentoot:dispatch-easy-handlers
          ;; http://localhost:PORT/example.jpg will dispatched to
          ;; /project-isidore/assets/example.jpg
          ;; Requires full system path
          ;; /app is the root on a heroku filesystem
          ,(hunchentoot:create-folder-dispatcher-and-handler "/" dispatch-folder)))
  (unless (equalp *acceptor* nil) ; only true upon first loading
    (when (hunchentoot:started-p *acceptor*)
    (return-from initialize-application (format t "Server already running at PORT ~A. Stop server with TERMINATE-APPLICATION" port))))
  (setf *acceptor*
        (hunchentoot:start
         (make-instance 'hunchentoot:easy-acceptor :port port
                                          :access-log-destination nil)))
  (format t "Server successfully started at PORT ~A" port)
  (log4cl:log-info "

Project Isidore initialization successful ...

Navigate to https://localhost:~A to continue ...
" port)
  (when cmd-user-interface
    (let ((do-sigint-poll t))
      (log4cl:log-info "

Close this window or press Control+C to exit the program ...")
      (terminate-application do-sigint-poll))))

(defun terminate-application (&optional sigint-poll)
  "Stop the web server started by INITIALIZE-APPLICATION, if it exists. When
called with a non NIL value for SIGINT-POLL, it will listen for SIGINT and
  gracefully shut down the web server and exit the lisp process."
  (when sigint-poll
    ;; warning: hardcoded "hunchentoot".
    (handler-case (bordeaux-threads:join-thread
                   (find-if (lambda (th)
                              (search "hunchentoot"
                                      (bordeaux-threads:thread-name th)))
                            (bordeaux-threads:all-threads)))
      ;; Catch a user's C-c
      (#+sbcl sb-sys:interactive-interrupt
       #+ccl  ccl:interrupt-signal-condition
       #+clisp system::simple-interrupt-condition
       #+ecl ext:interactive-interrupt
       #+allegro excl:interrupt-signal
       () (progn
            (format *error-output* "Aborting.~&")
            (hunchentoot:stop *acceptor*)
            (uiop:quit)
            (log4cl:log-info "Server successfully stopped")))
      (error (c) (format t "Whoops, an unknown error occured:~&~a~&" c))))
  (unless (equalp *acceptor* nil) ; only true upon first loading
    (if (hunchentoot:started-p *acceptor*)
        (progn
          (hunchentoot:stop *acceptor*)
          (return-from terminate-application
            (format t "Server successfully stopped")))))
  (format t "No server running. Start server with INITIALIZE-APPLICATION"))

;;; From https://ambrevar.xyz/modern-common-lisp/index.html
(declaim (ftype (function (string) list) package-dependencies))

(defun package-dependencies (pkg-name)
  "Collect explicit dependencies of an ASDF system."
  (let (depends)
    (labels ((iter (openlist)
               (if (null openlist) depends
                   ;; is this a subsystem of foo?
                   (let ((find (search  pkg-name (first openlist))))
                     (if (and find (zerop find))
                         (iter (append (asdf:system-depends-on (asdf:find-system (first openlist))) (rest openlist)))
                         ;; if not, it's a direct dependency: collect it
                         (progn
                           (pushnew (first openlist) depends :test 'equalp)
                           (iter (rest openlist))))))))
      (iter (list pkg-name)))))

(defun list-project-isidore-dependencies ()
  "Returns a list of all third party libraries needed to load Project Isidore."
  (package-dependencies "project-isidore"))
