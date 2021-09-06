(in-package #:cl-user)

;;; application entry point
(defvar *acceptor* nil)
;; Takes a PORT parameter as Heroku assigns a different PORT per environment
(defun initialize-application (&key port)
  (project-isidore:generate-index-css "/app/static/index.css")
  (setf hunchentoot:*dispatch-table*
        ;; for an explanation of ` and , and '. see https://stackoverflow.com/questions/60378335/quote-comma-in-common-lisp
        `(hunchentoot:dispatch-easy-handlers
          ,(hunchentoot:create-folder-dispatcher-and-handler ; Requires full system path
                                                    "/" "/app/static/"))) ; /app is the root on a heroku filesystem

  (when *acceptor*
    (hunchentoot:stop *acceptor*))

  (setf *acceptor*
        (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port port))))
