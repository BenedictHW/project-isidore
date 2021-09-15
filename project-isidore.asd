(asdf:defsystem #:project-isidore
  :name "Project Isidore"
  :author "Hanshen Wang <hanshen@hanshenwang.com>"
  :description "Personal Web Application"
  :license  "GNU Lesser Public License 3.0"
  :version "0.0.1"
  :serial t
  :depends-on (#:hunchentoot
               #:cl-who
               #:cl-css
               #:parenscript)
  :components ((:file "application")
               (:file "heroku"))
  :in-order-to ((asdf:test-op (asdf:test-op :project-isidore/test))))

(asdf:defsystem #:project-isidore/test
  :name "Project Isidore Tests"
  :author "Hanshen Wang <hanshen@hanshenwang.com>"
  :description "Personal Web Application Test Suite"
  :license "GNU Lesser Public License 3.0"
  :version "0.0.1"
  :serial t
  :depends-on (#:project-isidore
               #:parachute)
  :components((:module "test"
               :components ((:file "project-isidore-tests"))))
  :perform (asdf:test-op (op c)
                    (uiop:symbol-call :parachute :test :project-isidore/test)))
