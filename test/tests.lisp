;;;; SPDX-FileCopyrightText: 2021 Hanshen Wang <Hanshen@HanshenWang.com>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(defpackage #:project-isidore-test/tests
  (:use #:common-lisp
        #:project-isidore)
  ;; Testing framework.
  (:import-from #:parachute)
  ;; HTTP client library.
  (:import-from #:dexador)
  (:export #:master-suite)
  (:documentation
   "Project Isidore Regression Tests.

To run locally evaluate in the listener, (asdf:test-system :project-isidore) The
test suite is run prior to the build process. See MAKE.LISP."))

(in-package #:project-isidore-test/tests)

(parachute:define-test master-suite
  :description "The master suite of all Project Isidore tests")

(parachute:define-test can-app-init-and-exit
  :description "Check that application starts and exits gracefully. Terminate
  application must be run otherwise save-lisp-and-die cannot save core as there
  needs to be one thread, and initializing the application will create a
  hunchentoot thread."
  :parent master-suite
  (parachute:skip-on (win32) "Hunchentoot has poor Microsoft Windows support."
                     (parachute:true (progn
                                       (project-isidore:initialize-application :port 3510)
                                       (project-isidore:terminate-application)))))

(parachute:define-test does-global-css-exist
  :description "Global.css must exist in project-isidore/assets/global.css. The
  project generates CSS much in the same way it generates HTML. However unlike
  the generated HTML, CSS is not generated upon run time. The reason being
  complexity regarding how Common Lisp handles pathnames. Instead the function
  `generate-global-css' is manually called. This test ensures the static file
  global.css does indeed exist."
  :parent master-suite
  (parachute:true (uiop:file-exists-p (asdf:system-relative-pathname
                                :project-isidore "assets/global.css"))))

(parachute:define-test is-production-server-status-200
  :description "As per Heroku documentation: 'Whenever your app experiences an
  error, Heroku will return a standard error page with the HTTP status code
  503.'. This test serves as an early warning that the previous deploy may be
  successfully built, but there exists a runtime error.

  See Project Isidore documentation for more on the Drakma HTTP client library
  used.

  See https://httpstatuses.com/ for a list of HTTP status codes."
  :parent master-suite
  (parachute:true
   (= 200 (nth-value 1 (dex:get "https://www.BenedictHanshenWang.com/")))))

(parachute:define-test generate-data-finish
  :description "Check that `bible-page' and `bible-search-page' finishes."
  :parent master-suite
  (parachute:finish (progn
                      (project-isidore:bible-page (list (project-isidore:get-bible-uid 1 1 1)
                                                        (project-isidore:get-bible-uid 73 22 21)))
                      (project-isidore:bible-search-page "water"))))

(parachute:define-test regex-validity
  :description "Check `*reference-regex*' still works with the version of
  CL-PPCRE in use."
  :parent master-suite
  (parachute:true (string-equal "1 Esdras 5:1" (car (ppcre:all-matches-as-strings project-isidore:*reference-regex* (project-isidore:get-footnotes-text 27144))))))
