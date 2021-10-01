;;;; project-isidore-tests.lisp
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

(defpackage #:project-isidore/test
  (:use #:cl)
  (:local-nicknames (#:pi #:project-isidore)
                    (#:fa #:parachute))
  (:export #:test
           #:master-suite))

(in-package #:project-isidore/test)

(fa:define-test master-suite :description "The master suite of all project isidore tests")

(fa:define-test dummy-test
  :description "placeholder test until compilation can be tested"
  :parent master-suite
  (fa:of-type integer 5))
