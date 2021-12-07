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

(defpackage #:project-isidore/model
  (:use #:common-lisp)
  (:import-from #:postmodern)
  (:import-from #:log4cl)
  (:import-from #:bknr.datastore)
  ;; No package local nicknames. See commit 1962a26.
  (:export
   #:db-params
   :*database-url*
   :*localdb-params*
   :mailinglist
   #:mailinglist-add
   #:mailinglist-delete
   #:create-datastore
   :bible
   #:get-bible-uid)
  (:documentation
   "Database Access Object Schema & basic Create, Read, Update and Delete operations"))

(in-package #:project-isidore/model)

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

(defun create-datastore ()
  "Initialize Datastore."
  (let ((object-subsystem
          (make-instance
           'bknr.datastore:store-object-subsystem)))
    (make-instance 'bknr.datastore:mp-store
                   :directory
                   (asdf:system-relative-pathname :project-isidore "../data/")
                   :subsystems
                   (list object-subsystem))))

(defclass bible (bknr.datastore:store-object)
  ((book :reader book
         :initarg :book)
   ;; Relative to book.
   (chapter :reader chapter
            :initarg :chapter)
   ;; Relative to chapter.
   (verse :reader verse
          :initarg :verse)
   (text :reader text
         :initarg :text)
   (haydock-text :reader haydock-text
                 :initarg :haydock-text))
  (:metaclass bknr.datastore:persistent-class)
  (:documentation "Each verse of the Bible is created as an instance of class `bible', each with appropriate text in it's slot. HAYDOCK-TEXT however, may be unbound."))

(defun filter-list-by-book (list book)
  "Locates within a provided list of bible objects whose slot BOOK matches the argument BOOK. Argument BOOK must be of type string.

  Example:
  (filter-list-by-book (bknr.datastore:store-objects-with-class 'bible) \"Ruth\")"

  (remove-if-not (lambda (x) (and x (string-equal book (slot-value x 'book)))) list))

(defun filter-list-by-chapter (list chapter)
  "Locates within a provided list of bible objects whose slot CHAPTER matches the argument CHAPTER. Argument CHAPTER must be of type integer.

  Example:
  (filter-list-by-chapter (bknr.datastore:store-objects-with-class 'bible) 50)"

  (remove-if-not (lambda (x) (and x (equalp chapter (slot-value x 'chapter)))) list))

(defun filter-list-by-verse (list verse)
  "Locates within a provided list of bible objects whose slot VERSE matches the argument VERSE. Argument VERSE must be of type integer.

  Example:
  (filter-list-by-verse (bknr.datastore:store-objects-with-class 'bible) 50)"

  (remove-if-not (lambda (x) (and x (equalp verse (slot-value x 'verse)))) list))

(defun get-bible-uid (book chapter verse)
  "Return a unique identifier assigned to each instance of class `bible'.
 As class `bible' is of CLOS metaclass `bknr.datastore:persistent-class', this
returns the aforementioned unique identifier as an integer value bound to slot
ID. BOOK can either be a string or an integer. If the instance does not exist, NIL is returned.

   Example:
   (get-bible-uid \"Matthew\" 3 6) => 27916
   (get-bible-uid 47 3 6) => 27916
   (get-bible-uid 12983 29394 2938498) => NIL"
  (when (slot-exists-p (car
                     (filter-list-by-verse
                      (filter-list-by-chapter
                       (filter-list-by-book
                        (bknr.datastore:store-objects-with-class 'bible)
                        (if (integerp book)
                            (bible-book-convert-dwim book)
                            book))
                       chapter) verse)) 'bknr.datastore::id)
    (slot-value
     (car
      (filter-list-by-verse
       (filter-list-by-chapter
        (filter-list-by-book
         (bknr.datastore:store-objects-with-class 'bible)
         (if (integerp book)
             (bible-book-convert-dwim book)
             book))
        chapter) verse)) 'bknr.datastore::id)))

(defun bible-book-convert-dwim (bible-book)
  "Given a Bible book string name or integer, convert to the opposite format.

   Example:
   (bible-book-convert-dwim \"Matthew\") => 47
   (bible-book-convert-dwim 47) => \"Matthew\" "
  ;; Has to be one line otherwise splitting (cons "II \n Chronicles") will mean (bible-book-convert-dwim "II Chronicles") returns NIL but (bible-book-convert-dwim "II \n Chronicles") will return 14.
  (let ((bible-book-numbers (list (cons "Genesis" 1) (cons "Exodus" 2) (cons "Leviticus" 3) (cons "Numbers" 4) (cons "Deuteronomy" 5) (cons "Joshua" 6) (cons "Judges" 7) (cons "Ruth" 8) (cons "I Samuel" 9) (cons "II Samuel" 10) (cons "I Kings" 11) (cons "II Kings" 12) (cons "I Chronicles" 13) (cons "II Chronicles" 14) (cons "Ezra" 15) (cons "Nehemiah" 16) (cons "Tobit" 17) (cons "Judith" 18) (cons "Esther" 19) (cons "Job" 20) (cons "Psalms" 21) (cons "Proverbs" 22) (cons "Ecclesiastes" 23) (cons "Song of Solomon" 24) (cons "Wisdom" 25) (cons "Sirach" 26) (cons "Isaiah" 27) (cons "Jeremiah" 28) (cons "Lamentations" 29) (cons "Baruch" 30) (cons "Ezekiel" 31) (cons "Daniel" 32) (cons "Hosea" 33) (cons "Joel" 34) (cons "Amos" 35) (cons "Obadiah" 36) (cons "Jonah" 37) (cons "Micah" 38) (cons "Nahum" 39) (cons "Habakkuk" 40) (cons "Zephaniah" 41) (cons "Haggai" 42) (cons "Zechariah" 43) (cons "Malachi" 44) (cons "I Maccabees" 45) (cons "II Maccabees" 46) (cons "Matthew" 47) (cons "Mark" 48) (cons "Luke" 49) (cons "John" 50) (cons "Acts" 51) (cons "Romans" 52) (cons "I Corinthians" 53) (cons "II Corinthians" 54) (cons "Galatians" 55) (cons "Ephesians" 56) (cons "Philippians" 57) (cons "Colossians" 58) (cons "I Thessalonians" 59) (cons "II Thessalonians" 60) (cons "I Timothy" 61) (cons "II Timothy" 62) (cons "Titus" 63) (cons "Philemon" 64) (cons "Hebrews" 65) (cons "James" 66) (cons "I Peter" 67) (cons "II Peter" 68) (cons "I John" 69) (cons "II John" 70) (cons "III John" 71) (cons "Jude" 72) (cons "Revelation of John" 73))))
  ;; Explicitly declare valid types.
  (unless (or (stringp bible-book)
              (integerp bible-book))
    (format t "bible-book-convert-dwim called with invalid argument, ~a"
    bible-book))
  ;; string-equal is case insensitive. string= is case sensitive.
  (if (stringp bible-book)
      (cdr (assoc bible-book bible-book-numbers :test #'string-equal))
      (car (rassoc bible-book bible-book-numbers)))))

