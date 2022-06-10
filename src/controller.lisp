;;;; SPDX-FileCopyrightText: 2021 Benedict Hanshen Wang <Admin@BenedictHanshenWang.com>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:project-isidore/controller
  (:use #:common-lisp
        #:series
        #:project-isidore/views
        #:project-isidore/model)
  (:import-from #:alexandria)
  (:import-from #:hunchentoot)
  (:import-from #:hunchensocket)
  (:import-from #:quux-hunchentoot)
  (:import-from #:slynk)
  (:import-from #:snooze)
  ;; No package local nicknames. See commit 1962a26.
  (:export :*server*
           #:initialize-application
           #:terminate-application
           #:application-toplevel)
  (:documentation
   "Project Isidore Web Interface.

Starting the Web server for the application is defined by
`initialize-application'. The Project Isidore web interface has a rough mapping
onto the Model View Controller (MVC) design pattern.

Project Isidore Controller.

This package contains URI handler and routing logic.

1. Serve static assets via HTTP.

This includes the HTML blog articles, reference manual, code coverage report,
external CSS files, JS files, webp photos and png photos all located under
\"project-isidore/assets/\". See the
`hunchentoot:create-folder-dispatcher-and-handler' form.

`hunchentoot:define-easy-handler' links an uri with a function postfixed with
'-page'. It is said function which will generate the \"view\", or output HTML.

2. Serve dynamically generated HTML via HTTP.

See `index-page' (inline CSS + JS example), `about-page', `work-page' and
`contact-page'.

3. Serve dynamically generated HTML from persistent CLOS object state via HTTP.

Persistent CLOS object state equals our datastore in this use case.
`create-datastore' as defined in MODEL.LISP deserializes data found in the
\"project-isidore/data/\" See `bible-page'.

4. Serve Websocket resources.

Work-in-progress.

"))

(in-package #:project-isidore/controller)


(defclass snooze-acceptor (hunchensocket:websocket-acceptor
                           hunchentoot:easy-acceptor) ())


(defclass chat-room (hunchensocket:websocket-resource)
  ((name :initarg :name :initform (error "Name this room!") :reader name))
  (:default-initargs :client-class 'user))

(defclass user (hunchensocket:websocket-client)
  ((name :initarg :user-agent :reader name :initform (error "Name this user!"))))

(defvar *chat-rooms* (list (make-instance 'chat-room :name "/bongo")
                           (make-instance 'chat-room :name "/fury")))

(defun find-room (request)
  (find (hunchentoot:script-name request) *chat-rooms* :test #'string= :key #'name))

(defun broadcast (room message &rest args)
  (loop for peer in (hunchensocket:clients room)
        do (hunchensocket:send-text-message peer (apply #'format nil message args))))

(defmethod hunchensocket:client-connected ((room chat-room) user)
  (broadcast room "~a has joined ~a" (name user) (name room)))

(defmethod hunchensocket:client-disconnected ((room chat-room) user)
  (broadcast room "~a has left ~a" (name user) (name room)))

(defmethod hunchensocket:text-message-received ((room chat-room) user message)
  (broadcast room "~a says ~a" (name user) message))

(setf hunchensocket:*websocket-dispatch-table*
      (list 'find-room))


(setf hunchentoot:*dispatch-table*
      (list (hunchentoot:create-static-file-dispatcher-and-handler
             "/favicon.ico"
             (asdf:system-relative-pathname
              :project-isidore "assets/favicon.ico"))
            (hunchentoot:create-folder-dispatcher-and-handler
             "/assets/"
             (asdf:system-relative-pathname
              :project-isidore "assets/"))
            (rip:make-hunchentoot-app '((rip:*home-resource* . homepage)))))

(defvar *server* nil "To be used in `initialize-application' to create an
instance of class `hunchentoot:acceptor' to listen to a PORT")

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
            (format *error-output* "~%Aborting.~&~%")
            (slynk:stop-server :port 4005)
            (hunchentoot:stop *server*)
            (format t "~%Server successfully stopped.~%")
            (uiop:quit)))
      (error (c) (format t "Whoops, an unknown error occured:~&~a~&" c))))
  (when *server*
    (hunchentoot:stop *server*)
    (setq *server* nil)
    (format t "~%Server successfully stopped.~%")
    (return-from terminate-application t)))

(defun initialize-application (&key (port 8080))
  "Start a web server at PORT. Takes a PORT parameter as Heroku assigns a
different PORT per dyno/environment. CMD-USER-INTERFACE when set to true will
determine cause C-c (control+c) to exit.

Slynk server is used to connect to a running production LISP image.

See APPLICATION-TOPLEVEL for the main function or entry point in MAKE.LISP. "
  (terminate-application)
  (setf ;; To create an executable binary, MAKE.LISP calls
        ;; `sb-ext:save-lisp-and-die', which closes all open file streams. We
        ;; open `*search-index*' and `*database*' again upon application start.
        *search-index* (make-instance 'montezuma:index
                                      :path (asdf:system-relative-pathname
                                             :project-isidore "data/montezuma/")
                                      :default-field "*"
                                      :fields '("b" "c" "v" "t" "h"))
        *database* (rs:open-rucksack (asdf:system-relative-pathname
                                      :project-isidore "data/rucksack/"))
        rip:*catch-errors* :verbose
        *server* (hunchentoot:start
                  (make-instance 'snooze-acceptor
                                 :port port
                                 :address "127.0.0.1"
                                 :taskmaster (make-instance
                                              'quux-hunchentoot:thread-pooling-taskmaster
                                              :max-thread-count 8
                                              :max-accept-count 1000000)
                                 :access-log-destination nil)))
  (format t "~%
========================================
Project Isidore v1.2.1 (A.D. 2022-01-15)
========================================

Copyright (c) 2021 Benedict Hanshen Wang <Admin@BenedictHanshenWang.com>

Project Isidore is free software, provided as is, and comes with ABSOLUTELY NO
WARRANTY. This program is licensed under the GNU AFFERO GENERAL PUBLIC LICENSE
Version 3, 19 November 2007 You are welcome to redistribute the program or any
parts hereof under certain conditions. Please visit
https://www.gnu.org/licenses/agpl-3.0.html for License details.

Homepage: https://www.benedicthanshenwang.com/assets/blog/project-isidore-doc.html

Source code repository: https://github.com/BenedictHW/project-isidore

Navigate to http://localhost:~A to continue... ~%" port)
  (return-from initialize-application t))

(defun application-toplevel ()
  "Application entry point. Emulate a \"main\" function. Used in
  SAVE-LISP-AND-DIE to save Application as a Lisp image. Note PORT is a keyword
  argument that defaults to 8080. Heroku dynamically sets the PORT variable to
  be binded."
  (let ((port (if (equalp NIL (uiop:getenv "PORT"))
                  8080
                  (parse-integer (uiop:getenv "PORT")))))
    ;; We only want one connection to a remote lisp.
    (when (= 8091 port)
      (progn
        (slynk:create-server :port 4005 :dont-close t)
        (setf slynk:*use-dedicated-output-stream* nil)))
    (initialize-application :port port)
    (format t "~% Close this window or press Control+C to exit the program...~%")
    (terminate-application t)
    (bt:join-thread (find-if (lambda (th)
                               (search "hunchentoot"
                                       (bt:thread-name th)))
                             (bt:all-threads)))))

(rip:defroute homepage (:get "text/html")
  (index-page))

(rip:defroute about (:get "text/html")
  (about-page))

(rip:defroute work (:get "text/html")
  (work-page))

(rip:defroute contact (:get "text/html")
  (contact-page))

(rip:defroute subscribe (:get "text/html")
  (subscribe-page))


(rip:defresource bible (verb content-type beginning-verse &optional ending-verse)
  (:documentation "Provide `bible-page' with a list of BIBLE-UIDs. Lists are defined by a
BEGINNING-VERSE and an optional ENDING-VERSE")
  (:genpath bible-path))

;; See also `rip:no-such-resource' and `rip:invalid-resource-arguments' for more
;; examples.
(define-condition negative-range (rip:http-condition)
  ((endpoints :initarg :endpoints :accessor endpoints-of))
  (:default-initargs
   :status-code 416))

(defmethod rip:explain-condition ((condition negative-range)
                                  (resource (eql #'bible))
                                  (ct snooze-types:text/html))
  (negative-range-condition-page (endpoints-of condition)))

(defmethod rip:explain-condition ((condition rip:no-such-resource)
                                  resource ; Generic 404 page.
                                  (ct snooze-types:text/html))
    (404-condition-page (hunchentoot:script-name*)))

(defmethod rip:explain-condition ((condition rip:invalid-resource-arguments)
                                  (resource (eql #'bible))
                                  (ct snooze-types:text/html))
  (400-condition-page))

(rip:defroute bible (:get "text/html" (beginning-uid integer) &optional ending-uid)
  (unless ending-uid
    (setf ending-uid beginning-uid))
  (cond
    ;; FIXME This cond subform is accidental complexity due to improper
    ;; semantics in class `bible'. `bible-obj-with-id' with an id of 0 to 7
    ;; shows the heading slot has the value of book as NIL. I need to decide
    ;; what to do with the preface before `bible-obj-with-id' with id 8. This is
    ;; less likely in the defroute below, as I can imagine much more readily
    ;; /bible/0 as a curious GET request whereas a mix of alphanumeric like
    ;; /bible/fo0b3r will select the symbol defroute below.
    ((and (<= 0 beginning-uid)
          (>= 7 beginning-uid))
     (signal 'rip:invalid-resource-arguments))
    ((> beginning-uid ending-uid)
     (signal 'negative-range :endpoints (list beginning-uid ending-uid)))
    ((or (not (valid-bible-uid-p beginning-uid))
         (not (valid-bible-uid-p ending-uid)))
     (signal 'rip:invalid-resource-arguments))
    ((<= beginning-uid ending-uid)
     (bible-page (alexandria:iota (+ 1 (- ending-uid beginning-uid)) :start beginning-uid)))
    (t
     (signal 'rip:http-error))))

(defun parse-uid-sym (uid-sym)
  "Human readable call to `bible-page'. UID-SYM must be of the following format,
BOOK-delimiter-CHAPTER-delimiter-VERSE, where delimiters are the SAME single
NON-alphanumeric unreserved character. CHAPTER and VERSE must be numeric,
whereas BOOK must be alphanumeric. Unreserved characters are defined as
uppercase and lowercase letters, decimal digits, hyphen, period, underscore,
and tilde. See RFC3986 2.3. Unreserved Characters
https://www.ietf.org/rfc/rfc3986.txt

Ex. /bible/genesis-2-10,/bible/47~4~2 etc.

> (parse-uid-sym 'genesis-2-10)
51
> (parse-uid-sym '1.2.10)
51
> (parse-uid-sym '1.2-10)
ARG-COUNT-ERROR
"
  (let ((list (alexandria:extremum
                (mapcar #'(lambda (unreserved-char)
                            (ppcre:split unreserved-char (string uid-sym)))
                        '("-" "\\." "_" "~"))
                #'> :key #'length)))
    (if (= 3 (length list)) ; Assume a sublist of 3 is properly parsed.
        (destructuring-bind (book chapter verse)
            list
          (get-bible-uid
           (if (parse-integer book :junk-allowed t)
               ;; It has a number, but is it a prefix or by its lonesome?
               (if (ppcre:scan "[0-9]{1}[a-zA-Z]" book)
                   (bible-book-convert-dwim
                    (concatenate 'string
                                 (subseq book 0 1) ; IV Kings will not work.
                                 " "
                                 (cadr (ppcre:split "[0-9]" book))))
                   (parse-integer book))
               (bible-book-convert-dwim book))
           (parse-integer chapter)
           (parse-integer verse)))
        (signal 'rip:invalid-resource-arguments))))

(rip:defroute bible (:get "text/html" (beginning-sym symbol) &optional ending-sym)
  (unless ending-sym
    (setf ending-sym beginning-sym))
  (let ((beginning-uid (parse-uid-sym beginning-sym))
        (ending-uid (parse-uid-sym ending-sym)))
    (cond ((> beginning-uid ending-uid)
           (signal 'negative-range :endpoints (list beginning-sym ending-sym)))
          ((or (not (valid-bible-uid-p beginning-uid))
               (not (valid-bible-uid-p ending-uid)))
           (signal 'rip:invalid-resource-arguments))
          ((<= beginning-uid ending-uid)
           (bible-page (alexandria:iota (+ 1 (- ending-uid beginning-uid)) :start beginning-uid)))
          (t
           (signal 'rip:http-error)))))


(rip:defresource bible-search (verb content-type &key query)
  (:documentation "This resource defines a read-only GET request of
`project-isidore:*search-index*'. In the GET request, a QUERY parameter (of type
STRING) is passed to `project-isidore:bible-search-page'.")
  (:genpath bible-search-path))

(defmethod rip:uri-to-arguments ((resource (eql #'bible-search)) uri)
  "See `rip::read-for-resource-1'. The default behaviour of
`rip::safe-simple-read-from-string' if a string is not passed is to try
`rip::parse-integer-then-float' and then to try `rip::parse-symbol' on the
input. This is all good and well for default behaviour but for the fact that if
a GET request contains a `:' then it will look for a non-existent package while
trying to pass the defroute a symbol. As the Snooze README states, while
`rip:read-for-resource' can be specialized for the resource, it is better to go
one step up the chain and specialize the generic function that passes
`rip:read-for-resource' its inputs. And so for the resource `bible-search', the
GET request will always be parsed as a string.

Given GET request = /bible-search?query=bread+water+%2Bc%3A29

Sly's trace dialog tells us it was called with

> #<SNOOZE-COMMON:RESOURCE-GENERIC-FUNCTION PROJECT-ISIDORE/CONTROLLER::BIBLE-SEARCH (1)>
> \"?query=bread+water+%2Bc%3A29\"

And returns

< (:QUERY \"bread water +c:29\") to `rip:defroute' BIBLE-SEARCH. "
  (list
   :query
   (cadr (ppcre:split
          "="
          (quri:url-decode (quri:uri-query (quri:uri (string uri))))))))

(rip:defroute bible-search (:get "text/html" &key (query string))
  "Calls the view `project-isidore:bible-search-page' with STRING QUERY."
  (bible-search-page query))
