;;;; SPDX-FileCopyrightText: 2021 Hanshen Wang <Hanshen@HanshenWang.com>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(defpackage #:project-isidore/application
  (:use #:common-lisp
        #:project-isidore/views)
  (:import-from #:hunchentoot)
  ;; No package local nicknames. See commit 1962a26.
  (:export :*acceptor*
           #:initialize-application
           #:terminate-application
           #:application-toplevel)
  (:documentation
   "Project Isidore Web Interface.

Starting the Web server for the application is defined by
`initialize-application'. The Project Isidore web interface has a rough mapping
onto the Model View Controller (MVC) design pattern.

1. Serve static assets.

This includes the HTML blog articles, reference manual and code coverage report
all located under \"project-isidore/assets/\". See the
`hunchentoot:create-folder-dispatcher-and-handler' form in
`initialize-application'."))

(in-package #:project-isidore/application)

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
  (format t "

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
  (setf ;; Will show backtrace on status code 500 pages.
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
  (format t "

Project Isidore initialization successful ...

Navigate to http://localhost:~A to continue ...
" port)
  (when cmd-user-interface
    (let ((do-sigint-poll t))
      (format t "

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
            (format t "Server successfully stopped")))
      (error (c) (format t "Whoops, an unknown error occured:~&~a~&" c))))
  (unless (equalp *acceptor* nil) ; only true upon first loading
    (if (hunchentoot:started-p *acceptor*)
        (progn
          (hunchentoot:stop *acceptor*)
          (return-from terminate-application
            (format t "Server successfully stopped")))))
  (format t "No server running. Start server with INITIALIZE-APPLICATION"))

(defun application-toplevel ()
  "Application entry point. Emulate a \"main\" function. Used in
  SAVE-LISP-AND-DIE to save Application as a Lisp image. Note PORT is a keyword
  argument that defaults to 8080. Heroku dynamically sets the PORT variable to
  be binded."
  (initialize-application :port (if (equalp NIL (uiop:getenv "PORT"))
                                    8080
                                    (parse-integer (uiop:getenv "PORT")))
                          :dispatch-folder "assets/"
                          :cmd-user-interface t)
  ;; Sleep forever.
  (loop (sleep 600)))
