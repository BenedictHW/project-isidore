(asdf:defsystem #:project-isidore
  :name "Project Isidore"
  :author "Hanshen Wang <hanshen@hanshenwang.com>"
  :description "Personal Web Application"
  :license  "GNU Lesser Public License 3.0"
  :version "0.0.1"
  :serial t
  :depends-on (#:hunchentoot
               #:cl-who)
  :components ((:file "application")
               (:file "heroku")))
