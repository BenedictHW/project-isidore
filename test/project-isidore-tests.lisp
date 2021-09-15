#|
Copyright 2021 Hanshen Wang

Distributed under the LGPL license (see LICENSE file)
|#

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
