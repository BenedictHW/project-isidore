;;;; SPDX-FileCopyrightText: 2021 Hanshen Wang <Hanshen@HanshenWang.com>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(defpackage #:project-isidore-test/tests
  (:use #:common-lisp
        #:project-isidore)
  ;; Testing framework.
  (:import-from #:parachute)
  ;; HTTP client library.
  (:import-from #:drakma)
  (:export #:master-suite)
  (:documentation
   "Project Isidore Regression Tests.

To run locally evaluate in the listener,

(ql:quickload :project-isidore-test)

(asdf:test-system :project-isidore)

The test suite is run prior to the build process. See MAKE.LISP."))

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
      (parachute:finish (progn
                          (project-isidore:initialize-application)
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
                                :project-isidore "../assets/global.css"))))

(parachute:define-test generate-bible-html-finish
  :description "Check that BIBLE-PAGE finishes. Datastore needs to be closed,
  otherwise when make.lisp tries to open an already opened datastore, an error
  will be signaled."
  :parent master-suite
  (parachute:finish (progn
                       (project-isidore:create-datastore)
                       (project-isidore:bible-page "1-1-1-73-22-21")
                       (bknr.datastore:close-store))))

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
   (if (= 200 (nth-value 1 (drakma:http-request "https://www.HanshenWang.com/")))
       (= 200 (nth-value 1 (drakma:http-request "https://www.HanshenWang.com/")))
       ;; Secondary URL is a fallback if the HanshenWang.com domain expires.
       (= 200 (nth-value 1 (drakma:http-request "https://project-isidore.herokuapp.com/"))))))
