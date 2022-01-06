#| application.lisp

Copyright (c) 2021 Hanshen Wang <Hanshen@HanshenWang.com>

This file is part of Project Isidore.

Project Isidore is free software: you can redistribute it and/or modify it
under the terms of the GNU Affero General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

Project Isidore is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public
License for more details.

You should have received a copy of the GNU Affero General Public License
along with Project Isidore. If not, see <https://www.gnu.org/licenses/>.
|#

(defpackage #:project-isidore/application
  (:use #:common-lisp
        #:project-isidore/model
        #:project-isidore/views)
  (:import-from #:hunchentoot)
  (:import-from #:log4cl)
  ;; No package local nicknames. See commit 1962a26.
  (:export #:initialize-application
           #:terminate-application)
  (:documentation
   "Project Isidore Web Server and Controller.

Starting the Web server for the application is defined by
`initialize-application'. This package also contains URI handler and routing
logic. The Project Isidore web interface has a rough mapping onto the Model View
Controller (MVC) design pattern.

`hunchentoot:define-easy-handler' links an uri with a function postfixed with
'-page'. It is said function which will generate the output HTML.

In increasing order of complexity:

1. Serve static assets.

See the `hunchentoot:create-folder-dispatcher-and-handler' form in
`initialize-application'.

2. Serve dynamically generated HTML

See `index-page' (inline CSS + JS example), `about-page', `work-page' and
`contact-page'.

3. Serve dynamically generated HTML from persistent CLOS object state.

Persistent CLOS object state equals our datastore in this use case. Start with
`bible-page'. "))

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

(hunchentoot:define-easy-handler
    (create-subscriber :uri "/create-subscriber") ()
  (unless
      (hunchentoot:parameter "friend-email")
    (hunchentoot:redirect "/subscribe"))
  (let ((title (hunchentoot:parameter "friend-title"))
        (name (hunchentoot:parameter "friend-name"))
        (email (hunchentoot:parameter "friend-email")))
    (mailinglist-add title name email)
    (subscribe-success-page email)))

(hunchentoot:define-easy-handler (unsubscribe :uri "/unsubscribe") ()
  (unsubscribe-page))

(hunchentoot:define-easy-handler
    (delete-subscriber :uri "/delete-subscriber") ()
  (unless
      (hunchentoot:parameter "friend-email")
    (hunchentoot:redirect "/unsubscribe"))
  (let ((email (hunchentoot:parameter "friend-email")))
    (mailinglist-delete email)
    (unsubscribe-success-page email)))

;; All state is captured in the URI VERSES.
(hunchentoot:define-easy-handler (view-bible :uri "/bible") (verses)
  ;; HTTP response header is needed.
  (setf (hunchentoot:content-type*) "text/html")
  ;; localhost:8080/bible?verses=1-2-3-4-5-6
  (bible-page verses))

(defvar *acceptor* nil "To be used in `initialize-application' to create an
instance of class `hunchentoot:acceptor' to listen to a PORT")

(defun initialize-application (&key (port 8080)
(dispatch-folder (asdf:system-relative-pathname :project-isidore "../assets/"))
(cmd-user-interface nil))
  "Start a web server at PORT.

Set DATABASE_URL to connect to PostgreSQL database.

Optional PORT,DISPATCH-FOLDER and CMD-USER-INTERFACE.

Takes a PORT parameter as Heroku assigns a different PORT per dyno/environment.

DISPATCH-FOLDER determines from which URL static assets will be served.

CMD-USER-INTERFACE when set to true will determine if C-c will exit. See
APPLICATION-TOPLEVEL for the main function or entry point in MAKE.LISP. "
  (log4cl:log-info "

========================================
Project Isidore v1.1.0 (A.D. 2021-10-20)
========================================

Copyright 2021 Hanshen Wang <Hanshen@HanshenWang.com>

Project Isidore is free software, provided as is, and comes with ABSOLUTELY NO
WARRANTY. This program is licensed under the GNU AFFERO GENERAL PUBLIC LICENSE
Version 3, 19 November 2007 You are welcome to redistribute the program or any
parts hereof under certain conditions. Please visit
https://www.gnu.org/licenses/agpl-3.0.html for License details.

Homepage: https://www.hanshenwang.com/blog/project-isidore-doc.html
")
  (when (uiop:getenv "DATABASE_URL")
    (setf *database-url* (uiop:getenv "DATABASE_URL")))
  ;; Chose HTML5 encoding over default XHTML.
  (setf (cl-who:html-mode) :HTML5
        ;; Will show backtrace on status code 500 pages.
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
      (return-from initialize-application
        (format t "Server already running at PORT ~A. Stop server with TERMINATE-APPLICATION" port))))
  (setf *acceptor*
        (hunchentoot:start
         (make-instance 'hunchentoot:easy-acceptor :port port
                                          :access-log-destination nil)))
  (format t "Server successfully started at PORT ~A" port)
  (log4cl:log-info "

Project Isidore initialization successful ...

Navigate to http://localhost:~A to continue ...
" port)
  (when cmd-user-interface
    (let ((do-sigint-poll t))
      (log4cl:log-info "

Close this window or press Control+C to exit the program ...")
      (terminate-application do-sigint-poll))))

(defun terminate-application (&optional sigint-poll)
  "Stop the web server started by `initialize-application', if it exists. When
called with a non NIL value for SIGINT-POLL, it will listen for SIGINT and
gracefully shut down the web server and exit the lisp process."
  (when sigint-poll
    ;; Warning: hardcoded "hunchentoot".
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

