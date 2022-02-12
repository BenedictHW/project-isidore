;;;; SPDX-FileCopyrightText: 2021 Hanshen Wang <Hanshen@HanshenWang.com>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:project-isidore/model
  (:use #:common-lisp
        #:project-isidore/data)
  ;; Embedded Database.
  (:import-from #:rucksack)
  ;; Indexing engine library.
  (:import-from #:montezuma)
  ;; No package local nicknames. See commit 1962a26.
  (:export
   :*database* :bible

   #:get-bible-uid

   #:get-bible-text #:get-heading-text
   #:get-footnotes-text #:get-cross-references-text

   #:get-footnotes-text-with-links
   #:get-cross-references-text-with-links

   #:bible-book-convert-dwim #:bible-url-to-uid :+bible-book-url-alist+
   #:make-bible-chapter-url-list

   :*search-index* #:search-bible

   :*reference-regex*)
  (:documentation
   "Project Isidore Object Schema.

Data serialization.
How to represent tree structure? XML, JSON or SEXP?
https://unthought.net/2016/08/16-xml-json-trees-and-lisp/
https://eli.thegreenplace.net/2012/03/04/some-thoughts-on-json-vs-s-expressions

Microsoft Word uses XML. Web browsers render (X)HTML. Jupyter notebooks use JSON.

I see no great benefits to inserting an extra layer. CLOS Obj. > JSON > XML.

OUTDATED (Back when I used BKNR.Datastore)

For an indepth explanation on in-memory datastores, see:

Memory-Centric Data Management A Monash Information Services White Paper by Curt
A. Monash, Ph.D. May, 2006, accessible at http://www.monash.com/whitepapers.html

See pg 668 of weitzCommonLispRecipes2016 for cookbook recipes on BKNR.DATASTORE.
"))

(in-package #:project-isidore/model)

(defparameter *database*
  (rs:open-rucksack
   (asdf:system-relative-pathname :project-isidore "../data/rucksack/")))

;; 37199 (BIBLE-UID range = 0-37198) objects of class bible should exist. If
;; total objects exceeds the cache size, it can be set with, (setf
;; (rs:cache-size (rs:rucksack-cache rs:*rucksack*)) 1 000 000) . The default is
;; 100 000.

(rs:with-transaction ()
  (defclass bible ()
    ((unique-id :initarg :unique-id :reader unique-id-of
                :type fixnum
                :index :number-index
                :unique t)

     ;; Having the heading information in one slot instead of three prevents
     ;; mapping over class BIBLE thrice in `get-bible-uid' to locate a
     ;; UNIQUE-ID.
     (heading :initarg :heading :reader heading-of
              :type cons
              :documentation
              "The reference verse includes the book, chapter and line number.
Ex. \"((BOOK . 1) (CHAPTER . 2) (VERSE . 3))\"")

     (text :initarg :text :reader text-of
           :type string
           :documentation
           "Text of the object instance.")

     (cross-references :initarg :cross-references :reader cross-references-of
                       :type string
                       :documentation
                       "Cross references of object instance.")

     (footnotes    :initarg :footnotes :reader footnotes-of
                   :type string
                   :documentation
                   "Haydock text of the object instance."))
    (:index t)
    (:metaclass rs:persistent-class)
    (:documentation
     "Each verse of the Bible is created as an object instance of class `bible',
  each with appropriate text in it's slot. FOOTNOTES however, may be
  unbound.")))

(defun get-bible-uid (book chapter verse)
  "Return a unique identifier assigned to each instance of class `bible'.
   As class `bible' is of CLOS metaclass `rs:persistent-class', this
  returns the aforementioned unique identifier as an integer value bound to slot
  ID. BOOK CHAPTER and VERSE must be an integer with the range 1 - 35817.

  Example:
  (get-bible-uid 47 3 6) => 27917
  (get-bible-uid 12983 29394 2938498) => NIL "
  (rs:with-transaction ()
    (rs:rucksack-map-class
     *database* 'bible (lambda (obj)
                         (if
                          (equalp (list
                                   (cons 'PROJECT-ISIDORE/MODEL::BOOK book)
                                   (cons 'PROJECT-ISIDORE/MODEL::CHAPTER chapter)
                                   (cons 'PROJECT-ISIDORE/MODEL::VERSE verse))
                                  (heading-of obj))
                          (return-from get-bible-uid (unique-id-of obj)))))))

(defun bible-obj-with-id (bible-uid)
  "Returns the instance of object class `bible' when BIBLE-UID matches
UNIQUE-ID. If BIBLE-UID is invalid return NIL. The BIBLE-UID can be found by
calling `get-bible-uid'."
  (rs:with-transaction ()
    (rs:rucksack-map-slot *database* 'bible 'unique-id
                          (lambda (obj)
                            (return-from bible-obj-with-id obj))
                          :equal bible-uid)))

(defun bible-book-convert-dwim (book)
  "Given a Bible book string name or integer, convert to the opposite format.

   Example:
   (bible-book-convert-dwim \"Matthew\") => 47
   (bible-book-convert-dwim 47) => \"Matthew\" "
  (if (stringp book)
      ;; string-equal is case insensitive. string= is case sensitive.
      (cdr (assoc book +bible-book-numbers-alist+ :test #'string-equal))
      (car (rassoc book +bible-book-numbers-alist+))))

(defun get-bible-text (bible-uid)
  "Returns the text slot of object class `bible' in a string. If BIBLE-UID is invalid return NIL. The BIBLE-UID can be found by calling `get-bible-uid'."
  (text-of (bible-obj-with-id bible-uid)))

(defun get-heading-text (bible-uid)
  "Returns the title slot of object class `bible' in a string. If BIBLE-UID is invalid return NIL. The BIBLE-UID can be found by calling `get-bible-uid'."
  (let* ((heading-alist (heading-of (bible-obj-with-id bible-uid)))
         (book (bible-book-convert-dwim (cdar heading-alist)))
         (chapter (write-to-string (cdadr heading-alist)))
         (verse (write-to-string (cdaddr heading-alist))))
    (concatenate 'string book " " chapter ":" verse )))

(defun get-footnotes-text (bible-uid)
  "Returns a string if bible-uid is valid else return NIL.
  Only get text if commentary exists for given BIBLE-UID.
The bible-uid can be found by calling `get-bible-uid' with valid arguments."
  (when (slot-boundp (bible-obj-with-id bible-uid) 'footnotes)
    (footnotes-of (bible-obj-with-id bible-uid))))

(defun get-cross-references-text (bible-uid)
  "Returns a string if bible-uid is valid else return NIL.
  Only get text if commentary exists for given BIBLE-UID.
The bible-uid can be found by calling `get-bible-uid' with valid arguments."
  (when (slot-boundp (bible-obj-with-id bible-uid) 'cross-references)
    (cross-references-of (bible-obj-with-id bible-uid))))

(defun bible-ref-to-url (string bible-uid)
"Haydock commentary cross references are in the form BOOK, CHAPTER, VERSE. Ex.
\"Psalm 12:9.\" INTERNAL-CHAPTER is to properly process C. xxiii. 3.
where C. is short form for chapter. "
  (let ((internal-chapter (cdar (heading-of (bible-obj-with-id bible-uid))))
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
                    (setf book (write-to-string (parse-integer metadata-list :junk-allowed t))))
                   ;; No decimal number found.
                   ((not (parse-integer metadata-list :junk-allowed t))
                    (if book
                        (setf book (concatenate 'string book " "
                                                (string-trim '(#\Space #\Tab #\Newline) metadata-list)))
                        (setf book (string-trim '(#\Space #\Tab #\Newline) metadata-list))))
                   ;; Book is not nil, decimal number found.
                   ((and book
                         (parse-integer metadata-list :junk-allowed t))
                    (progn
                      (setf chapter (parse-integer metadata-list :junk-allowed t))
                      (setf verse (parse-integer (car (last (cl-ppcre:split ":" metadata-list))) :junk-allowed t))
                      (setf book (cond ((integerp book) book)
                                       ((string-equal "C" book) internal-chapter)
                                       (t (cdr (assoc book +bible-book-numbers-alist+ :test #'string-equal)))))
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

(defparameter *reference-regex*
  (ppcre:create-scanner "([1-4]?\\s?[a-zA-Z]{1,15}\\s[0-9]{1,3}\\:[0-9]{1,3})")
  "Regex explanation.
[1-4]? 4 K. is the highest for 4 Kings. ? signifies the number may or may not be
there.
\s whitespace.
[a-zA-Z]{1,15} Paralipomenon is the longest book name I have found thus far.
\. optional period.
\s whitespace.
[0-9]{1,3} 3 digits that make up a chapter number.
\: colon.
[0-9]{1,3} 3 digits that make up a verse number.
An extra backslash is need to escape the backslash itself.")

(defun encode-cross-references-to-html-links (string bible-uid)
  (when (or (get-footnotes-text bible-uid)
            (get-cross-references-text bible-uid))
    (flet ((convert (match y)
             (if (not (bible-ref-to-url match bible-uid))
                 (format nil "~a" match)
                 (format nil "<a href=\"~a\">~a</a>"
                         (bible-ref-to-url match bible-uid) y))))
      (ppcre:regex-replace-all *reference-regex* string #'convert :simple-calls t))))

(defun get-footnotes-text-with-links (bible-uid)
  (let ((footnotes-text (get-footnotes-text bible-uid)))
  (when footnotes-text
      (encode-cross-references-to-html-links footnotes-text bible-uid))))

(defun get-cross-references-text-with-links (bible-uid)
  (let ((cross-references-text (get-cross-references-text bible-uid)))
    (when cross-references-text
        (encode-cross-references-to-html-links cross-references-text bible-uid))))

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
                                           (heading-of (bible-obj-with-id (car (bible-url-to-uid bible-url))))))
                                         " "
                                         (write-to-string
                                          (cdadr
                                           (heading-of (bible-obj-with-id
                                                      (car (bible-url-to-uid bible-url)))))))
                            +bible-chapter-url-alist+ :test #'string-equal)
                    +bible-chapter-url-alist+))
        (ending-uid
          (position (rassoc (concatenate 'string
                                         (bible-book-convert-dwim
                                          (cdar
                                           (heading-of (bible-obj-with-id
                                                      (cadr (bible-url-to-uid bible-url))))))
                                         " "
                                         (write-to-string
                                          (cdadr
                                           (heading-of (bible-obj-with-id
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
                 :path (asdf:system-relative-pathname :project-isidore "../data/montezuma/")
                 :default-field "*"
                 :fields '("b" "c" "v" "t" "f" "x"))
  "Used in `search-bible' to query Bible data. Since `sb-ext:save-lisp-and-die'
  closes all open streams, this parameter is set during
  `initialize-application'.")

(defun search-bible (query &optional options)
  "Searches the Bible and Haydock's commentary. Returns an association list of
Bible unique ID's and a relevance score "
  (let ((results '()))
    (montezuma:search-each *search-index* query
                           #'(lambda (doc score)
                               (declare (optimize)
                                        (fixnum doc))
                               (push (cons doc score) results))
                           options)
    (nreverse results)))
