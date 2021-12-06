;;;; project-isidore-test.asd
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

#-asdf3.1 (error "Project Isidore requires ASDF 3.1 or later. Please upgrade your ASDF.")

(asdf:defsystem #:project-isidore-test
  :depends-on (:project-isidore
               :project-isidore-test/tests)
  :class :package-inferred-system
  :perform (asdf:test-op (op c)
                    (uiop:symbol-call :parachute :test :project-isidore-test/tests)))
