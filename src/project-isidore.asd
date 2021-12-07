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

#-asdf3.1 (error "Project Isidore requires ASDF 3.1 or later. Please upgrade your ASDF.")

(asdf:defsystem #:project-isidore
  :name "Project Isidore"
  :version "1.1.0"
  :author "Hanshen Wang <hanshen@hanshenwang.com>"
  :description "Personal Web Application"
  :license  "GNU Lesser Public License 3.0"
  :homepage "https://www.hanshenwang.com/blog/project-isidore-doc.html"
  :bug-tracker "https://github.com/HanshenWang/project-isidore/issues"
  :source-control (:git "https://github.com/HanshenWang/project-isidore.git")
  :class :package-inferred-system
  :depends-on (:project-isidore/packages)
  :in-order-to ((asdf:test-op (asdf:test-op :project-isidore-test))))