;;;; project-isidore.asd
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

(asdf:defsystem #:project-isidore
  :name "Project Isidore"
  :author "Hanshen Wang <hanshen@hanshenwang.com>"
  :description "Personal Web Application"
  :license  "GNU Lesser Public License 3.0"
  :version "1.0.0"
  :serial t
  :depends-on (#:hunchentoot
               #:cl-who
               #:cl-css
               #:parenscript
               #:postmodern
               #:log4cl
               #:ceramic)
  :components ((:module "src"
                :components ((:file "application"))))
  :in-order-to ((asdf:test-op (asdf:test-op :project-isidore/test))))

(asdf:defsystem #:project-isidore/test
  :name "Project Isidore Tests"
  :author "Hanshen Wang <hanshen@hanshenwang.com>"
  :description "Personal Web Application Test Suite"
  :license "GNU Lesser Public License 3.0"
  :version "0.0.1"
  :serial t
  :depends-on (#:project-isidore
               #:parachute)
  :components((:module "test"
               :components ((:file "project-isidore-tests"))))
  :perform (asdf:test-op (op c)
                    (uiop:symbol-call :parachute :test :project-isidore/test)))
