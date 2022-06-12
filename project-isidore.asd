;;;; SPDX-FileCopyrightText: 2021 Benedict Hanshen Wang <Admin@BenedictHanshenWang.com>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

#-asdf3.3 (error "Project Isidore requires ASDF 3.3 or later. Please upgrade your ASDF. See commit bf210b8.")

(asdf:defsystem "project-isidore"
  :name "Project Isidore"
  :version "1.2.1"
  :author "Benedict Hanshen Wang <admin@benedicthanshenwang.com>"
  :maintainer "Benedict Hanshen Wang <admin@benedicthanshenwang.com>"
  :description "Personal Web Application"
  :long-description
  "Project Isidore is currently a blog as well as a modern reproduction of the
1859 Haydock Bible."
  :license  "GNU Affero General Public License 3.0 or later"
  :homepage "https://www.benedicthanshenwang.com/blog/project-isidore-doc.html"
  :bug-tracker "https://github.com/BenedictHW/project-isidore/issues"
  :source-control (:git "https://github.com/BenedictHW/project-isidore.git")
  :class :package-inferred-system
  :pathname "src"
  :depends-on ("project-isidore/packages" "deploy")
  :in-order-to ((asdf:test-op (asdf:test-op "project-isidore/test")))
  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname "ProjectIsidore"
  :entry-point "project-isidore:application-toplevel")

(register-system-packages "project-isidore/packages" '(:project-isidore))

(asdf:defsystem "project-isidore/test"
  :class :package-inferred-system
  :pathname "test"
  :depends-on ("project-isidore/test/tests")
  :perform (asdf:test-op
            (op c) (uiop:symbol-call :parachute :test :project-isidore/test/tests)))

(asdf:defsystem "project-isidore/infrastructure"
  :class :package-inferred-system
  :pathname "infrastructure"
  :depends-on ("project-isidore/infrastructure/production"))
