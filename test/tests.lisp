;;;; tests.lisp
;;;
;;; Copyright (c) 2021 Hanshen Wang.
;;;
;;; Author: Hanshen Wang <Hanshen@HanshenWang.com>
;;; URL: https://github.com/HanshenWang/project-isidore
;;;
;;; This file is part of Project Isidore.
;;;
;;; Project Isidore is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Project Isidore is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Project Isidore.  If not, see <https://www.gnu.org/licenses/>.

(defpackage #:project-isidore-test/tests
  (:use #:common-lisp
        #:project-isidore)
  (:import-from #:parachute)
  (:export #:master-suite)
  (:documentation
   "Project Isidore regression tests. Run locally with (asdf:test-system :project-isidore)"))

(in-package #:project-isidore-test/tests)

(parachute:define-test master-suite
  :description "The master suite of all Project Isidore tests")

(parachute:define-test test-app-init-finish
  :description "Check that INITIALIZE-APPLICATION finishes"
  :parent master-suite
  (parachute:finish (project-isidore:initialize-application)))

(parachute:define-test does-global-css-exist
  :description "Global.css must exist in project-isidore/assets/global.css"
  :parent master-suite
  (parachute:true (uiop:file-exists-p (asdf:system-relative-pathname
                                :project-isidore "../assets/global.css"))))

(parachute:define-test generate-bible-html-finish
  :description "Check that BIBLE-PAGE finishes"
  :parent master-suite
  (parachute:finish (progn
                       (project-isidore:create-datastore)
                       (project-isidore:bible-page "1-1-1-73-22-21"))))
