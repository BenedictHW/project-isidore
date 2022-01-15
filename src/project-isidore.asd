;;;; SPDX-FileCopyrightText: 2021 Hanshen Wang <Hanshen@HanshenWang.com>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

#-asdf3.1 (error "Project Isidore requires ASDF 3.1 or later. Please upgrade your ASDF.")

(asdf:defsystem #:project-isidore
  :name "Project Isidore"
  :version "1.2.0"
  :author "Hanshen Wang <hanshen@hanshenwang.com>"
  :description "Personal Web Application"
  :license  "GNU Affero General Public License 3.0 or later"
  :homepage "https://www.hanshenwang.com/blog/project-isidore-doc.html"
  :bug-tracker "https://github.com/HanshenWang/project-isidore/issues"
  :source-control (:git "https://github.com/HanshenWang/project-isidore.git")
  :class :package-inferred-system
  :depends-on (:project-isidore/packages)
  :in-order-to ((asdf:test-op (asdf:test-op :project-isidore-test)))
  ;; Documentation of "program-op" here,
  ;; https://common-lisp.net/project/asdf/asdf.html#Predefined-operations-of-ASDF
  :build-operation "program-op"
  :build-pathname "ProjectIsidore"
  ;; Defined in src/application.lisp.
  :entry-point "project-isidore:application-toplevel")

(register-system-packages "project-isidore/packages" '(:project-isidore))
