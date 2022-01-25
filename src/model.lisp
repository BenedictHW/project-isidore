;;;; SPDX-FileCopyrightText: 2021 Hanshen Wang <Hanshen@HanshenWang.com>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:project-isidore/model
  (:use #:common-lisp
        #:project-isidore/data)
  ;; PostgreSQL wrapper library.
  (:import-from #:postmodern)
  ;; In-memory Datastore library.
  (:import-from #:bknr.datastore)
  ;; Indexing engine library.
  (:import-from #:montezuma)
  ;; No package local nicknames. See commit 1962a26.
  (:export
   #:db-params :*database-url* :*localdb-params*

   :mailinglist #:mailinglist-add #:mailinglist-delete

   #:create-datastore :bible

   #:get-bible-uid
   #:get-bible-text #:get-heading-text #:get-haydock-text #:get-haydock-text-with-ref

   #:bible-book-convert-dwim #:bible-url-to-uid :+bible-book-url-alist+
   #:make-bible-chapter-url-list

   :*search-index*
   #:create-search-index
   #:search-bible

   #:roman-to-decimal #:roman-numeral-p :*reference-regex*)
  (:documentation
   "Project Isidore Object Schema.

Contains two unrelated schemas, the class `mailinglist' and the class `bible'
with their respective operations.

Note `mailinglist' is used in the traditional relational database fashion.
Currently only PostgreSQL client-server architecture is supported by the
deployment platform Heroku.

Note `bible' is NOT a database, but an in-memory datastore. It is comparable to
Redis and Memcache, but most similar to Object Prevalance libraries like the
Java Prevayler library.

For an indepth explanation on in-memory datastores, see:

Memory-Centric Data Management A Monash Information Services White Paper by Curt
A. Monash, Ph.D. May, 2006, accessible at http://www.monash.com/whitepapers.html

See pg 668 of weitzCommonLispRecipes2016 for cookbook recipes on BKNR.DATASTORE.
"))

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
  ;; Heroku PostgreSQL requires SSL to connect
  (setf postmodern:*default-use-ssl* :try)
  (if *database-url*
      (let* ((url (second (cl-ppcre:split "//" *database-url*))) ; remove
                                        ; postgres://
             (user (first (cl-ppcre:split ":" (first
                                               (cl-ppcre:split "@" url)))))
             (password (second (cl-ppcre:split ":" (first
                                                    (cl-ppcre:split "@" url)))))
             (host (first (cl-ppcre:split ":" (second
                                               (cl-ppcre:split "@" url)))))
             ;; PORT parameter not needed. (port (first (cl-ppcre:split "/"
             ;; (third (cl-ppcre:split ":" url)))))
             (database (second (cl-ppcre:split "/"
                                               (second (cl-ppcre:split
                                                        "@"
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
  "Creates new entry in the `mailinglist' table."
  (postmodern:with-connection (db-params)
    (postmodern:make-dao 'mailinglist :title title :name name :email email))
  (format t "~A successfully added to mailing list." email))

(defun mailinglist-delete (email)
  "Removes entry from the `mailinglist' table."
  (let ((emailobj (car (postmodern:with-connection (db-params)
                         (postmodern:select-dao 'mailinglist
                             (:= 'email email))))))
  (postmodern:with-connection (db-params) (postmodern:delete-dao emailobj))
  (format t "~A removed from mailing list." email)))

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
  ((verse :initarg :verse :reader verse-of
          :type cons
          :index-type bknr.datastore::unique-index
          :index-initargs (:test #'equal)
          :index-reader bible-with-verse
          :index-values whole-bible-list
          :documentation
          "The reference verse includes the book, chapter and line number.
Ex. \"((BOOK . 1) (CHAPTER . 2) (VERSE . 3))\"")

   (text :initarg :text :reader text-of
         :type string
         :documentation
         "Text of the object instance.")

   (haydock-text :initarg :haydock-text :reader haydock-text-of
                 :type string
                 :documentation
                 "Haydock text of the object instance."))
  (:metaclass bknr.datastore:persistent-class)
  (:documentation
   "Each verse of the Bible is created as an object instance of class `bible',
  each with appropriate text in it's slot. HAYDOCK-TEXT however, may be
  unbound."))

(defun get-bible-uid (book chapter verse)
    "Return a unique identifier assigned to each instance of class `bible'.
   As class `bible' is of CLOS metaclass `bknr.datastore:persistent-class', this
  returns the aforementioned unique identifier as an integer value bound to slot
  ID. BOOK CHAPTER and VERSE must be an integer with the range 0 - 35816.

  Example:
  (get-bible-uid 47 3 6) => 27916
  (get-bible-uid 12983 29394 2938498) => NIL "
  (declare (optimize speed)
           (fixnum book)
           (fixnum chapter)
           (fixnum verse))
  ;; KLUDGE I assume there is an out of bounds/off by one error in
  ;; BKNR.DATASTORE's :index-reader function. Hence a special case for the last ID.
  (if (and (= book 73)
           (= chapter 22)
           (= verse 21))
      35816
      (slot-value (bible-with-verse
                   (list
                    (cons 'PROJECT-ISIDORE/MODEL::BOOK book)
                    (cons 'PROJECT-ISIDORE/MODEL::CHAPTER chapter)
                    (cons 'PROJECT-ISIDORE/MODEL::VERSE verse)))
                  'bknr.datastore::id)))

(defun bible-book-convert-dwim (bible-book)
  "Given a Bible book string name or integer, convert to the opposite format.

   Example:
   (bible-book-convert-dwim \"Matthew\") => 47
   (bible-book-convert-dwim 47) => \"Matthew\" "
  (if (stringp bible-book)
      ;; string-equal is case insensitive. string= is case sensitive.
      (cdr (assoc bible-book +bible-book-numbers-alist+ :test #'string-equal))
      (car (rassoc bible-book +bible-book-numbers-alist+))))

(defun get-bible-text (bible-uid)
  "Returns a string if bible-uid is valid else return NIL.
The bible-uid can be found by calling `get-bible-uid' with valid arguments."
  (text-of (bknr.datastore:store-object-with-id bible-uid)))

(defun get-heading-text (bible-uid)
  "Returns a string if bible-uid is valid else return NIL.
The bible-uid can be found by calling `get-bible-uid' with valid arguments."
  (concatenate
   'string
   (bible-book-convert-dwim(cdar (verse-of (bknr.datastore:store-object-with-id bible-uid)))) " "
   (write-to-string (cdadr (verse-of (bknr.datastore:store-object-with-id bible-uid)))) ":"
   (write-to-string (cdaddr (verse-of (bknr.datastore:store-object-with-id bible-uid))))))

(defun get-haydock-text (bible-uid)
  "Returns a string if bible-uid is valid else return NIL.
The bible-uid can be found by calling `get-bible-uid' with valid arguments."
  ;; Only get text if commentary exists for given BIBLE-UID.
  (when (slot-boundp (bknr.datastore:store-object-with-id bible-uid)
                     'haydock-text)
    (haydock-text-of (bknr.datastore:store-object-with-id bible-uid))))

(defun bible-ref-to-url (string bible-uid)
"Haydock commentary cross references are in the form ABBREVIATED BOOK NAME,
ROMAN NUMERAL, and INTEGER representing BOOK CHAPTER and VERSE respectively. Ex.
\"Ps. cxliv. 9.\" A list of ABBREVIATED BOOK NAMES can be found in
`+bible-book-abbrev-alist+'INTERNAL-CHAPTER is to properly process C. xxiii. 3.
where C. is short form for chapter. "
  (let ((internal-chapter (cdar (verse-of (bknr.datastore:store-object-with-id bible-uid))))
        (book nil)
        (chapter nil)
        (verse nil))
        (loop for metadata-list in (cl-ppcre:split "\\s" (remove #\. string))
              ;; 35019
              ;; ("" "Ps." "cxliv." "9.")
              do (cond
                   ;; Discard empty string.
                   ((string-equal "" metadata-list) nil)
                   ;; Book is nil, decimal number found.
                   ((and (not book)
                         (parse-integer metadata-list :junk-allowed t))
                    (setf book (format nil "~@r" (parse-integer metadata-list :junk-allowed t))))
                   ;; No roman or decimal number found.
                   ((and (not (roman-numeral-p metadata-list))
                         (not (parse-integer metadata-list :junk-allowed t)))
                    (if book
                        (setf book (concatenate 'string book " "
                                                (string-trim '(#\Space #\Tab #\Newline) metadata-list)))
                        (setf book (string-trim '(#\Space #\Tab #\Newline) metadata-list))))
                   ;; Roman numeral found.
                   ((roman-numeral-p metadata-list)
                    (setf chapter (roman-to-decimal metadata-list)))
                   ;; Book is not nil, decimal number found.
                   ((and book
                         (parse-integer metadata-list :junk-allowed t))
                    (progn
                      (setf verse (parse-integer metadata-list :junk-allowed t))
                      (setf book (cond ((integerp book) book)
                                       ((string-equal "C" book) internal-chapter)
                                       (t (cdr (assoc book +bible-book-abbrev-alist+ :test #'string-equal)))))
                      (return-from bible-ref-to-url
                        (if (not book)
                            nil
                        (concatenate 'string "/bible?verses="
                                     (write-to-string book) "-"
                                     (write-to-string chapter) "-"
                                     (write-to-string verse) "-"
                                     (write-to-string book) "-"
                                     (write-to-string chapter) "-"
                                     (write-to-string verse))))))))))

(defparameter *reference-regex* (ppcre:create-scanner "([1-4]?\\s?[a-zA-Z]{1,6}\\.?\\s[clxvi]+\\.\\s[0-9]{1,3}\\.)")
  "Regex explanation.
[1-4]? 4 K. is the highest for 4 Kings. ? signifies the number may or may not be
there.
\s whitespace.
[a-zA-Z]{1,6} Esdras is the longest book name I have found thus far.
\. optional period.
\s whitespace.
[clxvi]+\. Any number of the listed letters until a period.
\s whitespace.
[0-9]{1,3} 3 digits that make up a verse number.
\. period.
An extra backslash is need to escape the backslash itself.
([1-4]?\s[a-zA-Z]{1,6}\.\s[clxvi]+\.\s[0-9]{1,3}\.) ")

(defun encode-haydock-text (string bible-uid)
  (when (slot-boundp (bknr.datastore:store-object-with-id bible-uid)
                     'haydock-text)
    (flet ((convert (match y)
             (if (not (bible-ref-to-url match bible-uid))
                 (format nil "~a" match)
                 (format nil "<a href=\"~a\">~a</a>"
                         (bible-ref-to-url match bible-uid) y))))
      (ppcre:regex-replace-all *reference-regex* string #'convert :simple-calls t))))

(defun get-haydock-text-with-ref (bible-uid)
  (let ((haydock-text (get-haydock-text bible-uid)))
  (if haydock-text
      (encode-haydock-text haydock-text bible-uid))))

(defvar *roman-chars* "ivxlcdm")
(defvar *roman-nums*  '(1 5 10 50 100 500 1000))

(defun roman-numeral-to-decimal (roman-numeral)
  (let ((i (position (coerce roman-numeral 'character) *roman-chars*)))
    (and i (nth i *roman-nums*))))

(defun map-roman-numerals-to-decimal (roman-numerals)
  (map 'list #'roman-numeral-to-decimal roman-numerals))

(defun roman-numeral-p (string)
  (if (not (loop for test-char across "abefghjknopqrstuwyz0123456789"
      when (find test-char string :test #'char-equal)
        collect test-char))
      t
      nil))

(defun roman-to-decimal (roman)
  "The Roman numeral symbols are I, V, X, L, C, D, and M, standing respectively
for 1, 5, 10, 50, 100, 500, and 1,000. The book with the highest chapter number
is indeed the Psalms, at 150. The chapter with the highest verse number is also
in the Psalms: Psalm 118 (119):176. "
  (if (roman-numeral-p roman)
      (loop as (A B) on (map-roman-numerals-to-decimal roman)
            if A sum (if (and B (< A B)) (- A) A))
      nil))

(defun bible-url-to-uid (bible-url)
  "Accepts a string of six integers and returns a list of 2 integers.
The list includes the beginning bible-uid and the ending bible-uid.

Example:
(bible-url-to-uid \"24-1-1-26-1-1\") => (18352 18907) "
  (let ((start-book (parse-integer(first (cl-ppcre:split "-" bible-url))))
        (start-chapter (parse-integer (second (cl-ppcre:split "-" bible-url))))
        (start-verse (parse-integer (third (cl-ppcre:split "-" bible-url))))
        (end-book (parse-integer(fourth (cl-ppcre:split "-" bible-url))))
        (end-chapter (parse-integer (fifth (cl-ppcre:split "-" bible-url))))
        (end-verse (parse-integer (sixth (cl-ppcre:split "-" bible-url)))))
    ;; If the ending uid is smaller than the beginning uid.
    (if (> (get-bible-uid start-book start-chapter start-verse)
           (get-bible-uid end-book end-chapter end-verse))
        (list
         (get-bible-uid end-book end-chapter end-verse)
         (get-bible-uid start-book start-chapter start-verse))
        (list
         (get-bible-uid start-book start-chapter start-verse)
         (get-bible-uid end-book end-chapter end-verse)))))

(defun make-bible-chapter-url-list (bible-url)
  "Selects the right links from `+bible-chapter-url-alist+' based on the
BIBLE-URL

Example:

(project-isidore/model:make-bible-chapter-url-list \"1-1-1-1-3-1\") =>

((\"/bible?verses=1-1-1-1-1-31\" . \"Genesis 1\")
 (\"/bible?verses=1-2-1-1-2-25\" . \"Genesis 2\")
 (\"/bible?verses=1-3-1-1-3-24\" . \"Genesis 3\"))"
  (let* ((beginning-uid
          (position (rassoc (concatenate 'string
                                         (bible-book-convert-dwim
                                          (cdar
                                           (verse-of (bknr.datastore:store-object-with-id
                                                      (car (bible-url-to-uid bible-url))))))
                                         " "
                                         (write-to-string
                                          (cdadr
                                           (verse-of (bknr.datastore:store-object-with-id
                                                      (car (bible-url-to-uid bible-url)))))))
                            +bible-chapter-url-alist+ :test #'string-equal)
                    +bible-chapter-url-alist+))
        (ending-uid
          (position (rassoc (concatenate 'string
                                         (bible-book-convert-dwim
                                          (cdar
                                           (verse-of (bknr.datastore:store-object-with-id
                                                      (cadr (bible-url-to-uid bible-url))))))
                                         " "
                                         (write-to-string
                                          (cdadr
                                           (verse-of (bknr.datastore:store-object-with-id
                                                      (cadr (bible-url-to-uid bible-url)))))))
                            +bible-chapter-url-alist+ :test #'string-equal)
                    +bible-chapter-url-alist+)))
    ;; If in chapter view, resubmit book url to this function
    ;; to generate persistent chapter links.
    ;; Special case: last book of the bible.
    (cond ((equalp 73 (parse-integer (first (cl-ppcre:split "-" bible-url))))
           (nthcdr 1312 +bible-chapter-url-alist+))
          ;; Special case: books with one chapter.
          ((or (equalp 36 (parse-integer
                           (first (cl-ppcre:split "-" bible-url))))
               (equalp 64 (parse-integer
                           (first (cl-ppcre:split "-" bible-url))))
               (equalp 70 (parse-integer
                           (first (cl-ppcre:split "-" bible-url))))
               (equalp 71 (parse-integer
                           (first (cl-ppcre:split "-" bible-url))))
               (equalp 72 (parse-integer
                           (first (cl-ppcre:split "-" bible-url)))))
           (reverse
            (set-difference
             (nthcdr beginning-uid
                     +bible-chapter-url-alist+)
             ;; HACK KLUDGE FIXME
             ;; otherwise last chapter gets cut off.
             ;; this will give a nil error if the last
             ;; verse/chapter of the bible is inputted.
             ;; (+ 1 ending-uid)
             (nthcdr (+ 1 ending-uid) +bible-chapter-url-alist+))))
          ((equalp (parse-integer (second (ppcre:split "-" bible-url)))
                   (parse-integer (fifth (ppcre:split "-" bible-url))))
           (make-bible-chapter-url-list
            (cadr (cl-ppcre:split
                   "=" (car (rassoc (bible-book-convert-dwim
                                     (parse-integer (first
                                                     (cl-ppcre:split
                                                      "-" bible-url))))
                                    +bible-book-url-alist+
                                    :test #'string-equal))))))
          (t
           (reverse
            (set-difference
             (nthcdr beginning-uid
                     +bible-chapter-url-alist+)
             ;; HACK KLUDGE FIXME
             ;; otherwise last chapter gets cut off.
             ;; this will give a nil error if the last
             ;; verse/chapter of the bible is inputted.
             ;; (+ 1 ending-uid)
             (nthcdr (+ 1 ending-uid) +bible-chapter-url-alist+)))))))

(defparameter *search-index*
  (make-instance 'montezuma:index
                  ;; :path (asdf:system-relative-pathname :project-isidore "../data/")
                  :default-field "*"
                  :fields '("b" "c" "v" "t" "h")) )

(defun search-bible (query &optional options)
  "Searches the Bible and Haydock's commentary. Returns an association list of
Bible unique ID's and a relevance score"
  (let ((results '()))
    (montezuma:search-each *search-index* query
                           #'(lambda (doc score)
                               (declare (optimize)
                                        (fixnum doc))
                               (push (cons doc score) results))
                           options)
    (nreverse results)))

(defun create-search-index ()
  " Montezuma index will have same indices as BKNR.DATASTORE class object
bible. Therefore double check that `get-bible-uid' bible-uid and
`montezuma:get-document *search-index*' bible-uid return the same values
for the same input bible-uid. x = bible-uid.

DO NOT CALL
(montezuma:optimize *search-index*) as this will cause the tokens stored in
project-isidore/data/index/ to be over 100 megabytes, the current hard
limit to Microsoft's Github platform."
  ;; The document fields are short so as to make end-user querying more
  ;; efficient.
  (loop for x from 0 to 35816
        do (montezuma:add-document-to-index
            *search-index* `(;; Book.
                             ("b" . , (bible-book-convert-dwim (cdar (verse-of (bknr.datastore:store-object-with-id x)))))
                             ;; Chapter.
                             ("c"  . , (cdadr (verse-of (bknr.datastore:store-object-with-id x))))
                             ;; Verse.
                             ("v"  . , (cdaddr (verse-of (bknr.datastore:store-object-with-id x))))
                             ;; Text.
                             ("t"  . , (get-bible-text x))
                             ;; Haydock Text.
                             ("h"  . , (get-haydock-text x))))))
