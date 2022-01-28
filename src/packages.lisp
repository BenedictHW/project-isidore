;;;; SPDX-FileCopyrightText: 2021 Hanshen Wang <Hanshen@HanshenWang.com>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:project-isidore/packages
  (:nicknames #:project-isidore)
  (:use #:common-lisp)
  ;; No package local nicknames. See commit 1962a26.
  (:use-reexport #:project-isidore/model
                 #:project-isidore/views
                 #:project-isidore/controller)
  (:export #:list-project-isidore-dependencies)
  (:documentation
   "Project Isidore Meta Package.

Export a single package PROJECT-ISIDORE with all the external symbols from all
inferred packages. Inferred here is taken to signify the ASDF Package Inferred
System.

For the web application entry point, see INITIALIZE-APPLICATION. This symbol is
located in APPLICATION.LISP. For a comprehensive index of exported symbols and
definitions, see the Reference Manual at
/project-isidore/assets/reference/manual.html "))

(in-package #:project-isidore/packages)

;;; It is idiomatic lisp to specify loading order and dependencies manually in
;;; package(s).lisp. Project Isidore system eschews this idiom in favour of
;;; ASDF's package inferred style. As a result, third-party dependencies are not
;;; longer manually specified. In order to retrieve a list of dependencies...
;; From https://ambrevar.xyz/modern-common-lisp/index.html
(declaim (ftype (function (string) list) package-dependencies))

(defun package-dependencies (pkg-name)
  "Collect explicit dependencies of an ASDF system."
  (let (depends)
    (labels ((iter (openlist)
               (if (null openlist) depends
                   ;; is this a subsystem of foo?
                   (let ((find (search  pkg-name (first openlist))))
                     (if (and find (zerop find))
                         (iter
                           (append
                            (asdf:system-depends-on
                             (asdf:find-system
                              (first openlist))) (rest openlist)))
                         ;; if not, it's a direct dependency: collect it
                         (progn
                           (pushnew (first openlist) depends :test 'equalp)
                           (iter (rest openlist))))))))
      (iter (list pkg-name)))))

(defun list-project-isidore-dependencies ()
  "Returns a list of all third party libraries needed to load Project Isidore.
Example:
(in-package :cl-user)
(list-project-isidore-dependencies) =>"
;; ("hunchentoot" "parenscript" "cl-who" "cl-css" "montezuma" "bknr.datastore"
;;  "postmodern")
  (package-dependencies "project-isidore"))
