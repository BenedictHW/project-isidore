;;;; SPDX-FileCopyrightText: 2021 Hanshen Wang <Hanshen@HanshenWang.com>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

#-asdf3.1 (error "Project Isidore requires ASDF 3.1 or later. Please upgrade your ASDF.")

(asdf:defsystem #:project-isidore-test
  :name "Project Isidore Tests"
  :version "0.0.1"
  :author "Hanshen Wang <hanshen@hanshenwang.com>"
  :maintainer "Hanshen Wang <hanshen@hanshenwang.com"
  :description "Test Suite for Project Isidore"
  :license "GNU Affero General Public License 3.0 or later"
  :homepage "https://www.hanshenwang.com/blog/project-isidore-doc.html"
  :bug-tracker "https://github.com/HanshenWang/project-isidore/issues"
  :source-control (:git "https://github.com/HanshenWang/project-isidore.git")
  :class :package-inferred-system
  :depends-on (:project-isidore
               :project-isidore-test/tests)
  :perform (asdf:test-op (op c)
                    (uiop:symbol-call :parachute :test :project-isidore-test/tests)))
