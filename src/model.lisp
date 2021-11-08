;;;; model.lisp
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

(defpackage #:project-isidore/src/model
  (:use #:common-lisp)
  (:import-from #:postmodern)
  (:import-from #:log4cl)
  (:export
   #:db-params
   :*database-url*
   :*localdb-params*
   :mailinglist
   #:mailinglist-add
   #:mailinglist-delete)
  (:documentation
   "Database Access Object Schema & basic Create, Read, Update and Delete operations"))

(in-package #:project-isidore/src/model)

;;; Database functions
(defparameter *database-url* nil
  "Production Database URL. To be retrieved from production
environment variables and parsed by DB-PARAMS. This URL is NOT constant and will
be changed periodically by Heroku.")
(defparameter *local-db-params* (list "test" "user1" "user1" "localhost") "Local
Database Parameters. A local PostgreSQL installation with the creation of
\"user1\" user and \"test\" database is needed")

(defun db-params ()
  "Sets *DATABASE-URL* parameter. Heroku database URL format is
postgres://username:password@host:port/database_name. If we are testing on
localhost, use the db-parameters from *LOCAL-DB-PARAMS*."
  (setf postmodern:*default-use-ssl* :try) ; Heroku PostgreSQL requires SSL to connect
  (if *database-url*
      (let* ((url (second (cl-ppcre:split "//" *database-url*))) ; remove
                                        ; postgres://
             (user (first (cl-ppcre:split ":" (first (cl-ppcre:split "@"
                                                                     url)))))
             (password (second (cl-ppcre:split ":" (first (cl-ppcre:split "@"
                                                                          url)))))
             (host (first (cl-ppcre:split ":" (second (cl-ppcre:split "@"
                                                                      url)))))
             ;; PORT parameter not needed. (port (first (cl-ppcre:split "/"
             ;; (third (cl-ppcre:split ":" url)))))
             (database (second (cl-ppcre:split "/" (second (cl-ppcre:split "@"
                                                                           url))))))
        (list database user password host))
      *local-db-params*))

(defclass mailinglist ()
  ((id :col-type integer :col-identity t :accessor id :documentation "Unique ID
  number for each subscriber. Note ID is unique and not reused upon deletion.")
   (title :col-type string :initarg :title :reader friend-title :documentation
          "For usage in E-mail greetings: TITLE NAME")
   (name :col-type string :initarg :name :reader friend-name :documentation "For
  usage in E-mail greetings: TITLE NAME")
   (email :col-type string :col-unique t :initarg :email :reader friend-email
          :documentation "Subscriber E-mail. Must be unique."))
  (:metaclass postmodern:dao-class)
  (:table-name mailinglist)
  (:documentation "MAILINGLIST table schema contains all E-mails that are to
  receive notifications on PROJECT-ISIDORE blog article updates."))

(defun mailinglist-add (title name email)
  (postmodern:with-connection (db-params)
    (postmodern:make-dao 'mailinglist :title title :name name :email email))
  (log4cl:log-info "~A successfully added to mailing list." email))

(defun mailinglist-delete (email)
  (let ((emailobj (car (postmodern:with-connection (db-params) (postmodern:select-dao 'mailinglist (:= 'email email))))))
  (postmodern:with-connection (db-params) (postmodern:delete-dao emailobj))
  (log4cl:log-info "~A removed from mailing list." email)))

