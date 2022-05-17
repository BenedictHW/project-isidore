;;;; generate-doc.lisp
;;; See subheading 'Generate Reference Manual' at
;;; https://www.hanshenwang.com/public/blog/project-isidore-doc.html/

(load "~/quicklisp/setup.lisp") ; Quicklisp is installed in default location
(ql:quickload :project-isidore) ; If you need to download dependencies
(ql:quickload :net.didierverna.declt)
;; Generate 'project-isidore.texi' in TEXI-DIRECTORY
(net.didierverna.declt:declt :project-isidore
                             :texi-name "reference-manual"
                             :texi-directory
                             (asdf:system-relative-pathname
                              :project-isidore "assets/")
                             :library-name "Project Isidore"
                             ;; links are machine specific
                             :hyperlinks nil
                             ;; :long will print generation time. This will be
                             ;; picked up by git. Otherwise I would pick :long
                             :declt-notice :short)
;; https://lispcookbook.github.io/cl-cookbook/os.html#input-and-output-from-subprocess
(defparameter *shell* (uiop:launch-program "bash" :input :stream
                                                  :output :stream))
;; Change to proper directory
(defparameter *reference-manual-path* (concatenate
                             'string "cd "
                             (namestring
                              (asdf:system-relative-pathname
                               :project-isidore "assets/"))))
(write-line *reference-manual-path* (uiop:process-info-input *shell*))
;; Convert .texi to .html
(write-line
   ;; For makeinfo flags, see
   ;; https://www.gnu.org/software/texinfo/manual/texinfo/texinfo.html#HTML-CSS
 "makeinfo --html 'reference-manual.texi' --no-split --css-include='global.css'"
 (uiop:process-info-input *shell*))
(force-output (uiop:process-info-input *shell*))
(uiop:quit)
