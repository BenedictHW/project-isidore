;;;; generate-doc.lisp
;;;
;;; Copyright (c) 2021 Hanshen Wang <Hanshen@HanshenWang.com>
;;;
;;; This file is part of Project Isidore.
;;;
;;; Project Isidore is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU Affero General Public License as published by the
;;; Free Software Foundation, either version 3 of the License, or (at your
;;; option) any later version.
;;;
;;; Project Isidore is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public
;;; License for more details.
;;;
;;; You should have received a copy of the GNU Affero General Public License
;;; along with Project Isidore. If not, see <https://www.gnu.org/licenses/>.

;;; See subheading 'Generate Reference Manual' at
;;; https://www.hanshenwang.com/blog/project-isidore-doc.html/

(load "~/quicklisp/setup.lisp") ; Quicklisp is installed in default location
(ql:quickload :project-isidore) ; If you need to download dependencies
(ql:quickload :net.didierverna.declt)
;; Generate 'project-isidore.texi' in TEXI-DIRECTORY
(net.didierverna.declt:declt :project-isidore
                             :texi-name "manual"
                             :texi-directory
                             (asdf:system-relative-pathname
                              :project-isidore "../assets/reference/")
                             :library-name "Project Isidore"
                             ;; links are machine specific
                             :hyperlinks nil
                             ;; :long will print generation time. This will be
                             ;; picked up by git. Otherwise I would pick :long
                             :declt-notice :short)
;; https://lispcookbook.github.io/cl-cookbook/os.html#input-and-output-from-subprocess
(defparameter *shell* (uiop:launch-program "bash" :input :stream :output
                                                         :stream))
;; Change to proper directory
(defparameter *manual-path* (concatenate
                             'string "cd "
                             (namestring
                              (asdf:system-relative-pathname
                               :project-isidore "../assets/reference/"))))
(write-line *manual-path* (uiop:process-info-input *shell*))
;; Convert .texi to .html
(write-line
   ;; For makeinfo flags, see
   ;; https://www.gnu.org/software/texinfo/manual/texinfo/texinfo.html#HTML-CSS
 "makeinfo --html 'manual.texi' --no-split --css-include='../global.css'"
 (uiop:process-info-input *shell*))
(force-output (uiop:process-info-input *shell*))
