;;;; SPDX-FileCopyrightText: 2021 Hanshen Wang <Hanshen@HanshenWang.com>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(defpackage #:project-isidore/controller
  (:use #:common-lisp
        #:project-isidore/views
        #:project-isidore/application)
  (:import-from #:hunchentoot)
  ;; No package local nicknames. See commit 1962a26.
  (:export :*acceptor*
           #:initialize-application
           #:terminate-application)
  (:documentation
   "Project Isidore Controller.

This package contains URI handler and routing logic.

`hunchentoot:define-easy-handler' links an uri with a function postfixed with
'-page'. It is said function which will generate the \"view\", or output HTML.

1. Serve dynamically generated HTML

See `index-page' (inline CSS + JS example), `about-page', `work-page' and
`contact-page'.

2. Serve dynamically generated HTML from persistent CLOS object state.

Persistent CLOS object state equals our datastore in this use case.
`create-datastore' as defined in MODEL.LISP deserializes data found in the
\"project-isidore/data/\" See `bible-page'. "))

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
    (subscribe-success-page title name email)))

(hunchentoot:define-easy-handler (unsubscribe :uri "/unsubscribe") ()
  (unsubscribe-page))

(hunchentoot:define-easy-handler
    (delete-subscriber :uri "/delete-subscriber") ()
  (unless
      (hunchentoot:parameter "friend-email")
    (hunchentoot:redirect "/unsubscribe"))
  (let ((email (hunchentoot:parameter "friend-email")))
    (unsubscribe-success-page email)))

;; All state is captured in the URI VERSES.
(hunchentoot:define-easy-handler (bible :uri "/bible") (verses)
  ;; HTTP response header is needed.
  (setf (hunchentoot:content-type*) "text/html")
  ;; localhost:8080/bible?verses=1-2-3-4-5-6
  (bible-page verses))

(hunchentoot:define-easy-handler (bible-search :uri "/bible-search") (query)
  (setf (hunchentoot:content-type*) "text/html")
  (bible-search-page query))
