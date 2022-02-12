;;;; SPDX-FileCopyrightText: 2021 Hanshen Wang <Hanshen@HanshenWang.com>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

#-asdf3.1 (error "Project Isidore requires ASDF 3.1 or later. Please upgrade your ASDF.")

(asdf:defsystem "project-isidore-test"
  :class :package-inferred-system
  :pathname "test"
  :depends-on ("project-isidore-test/tests")
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :project-isidore-test/tests)))
