;;;; index.lisp
;;;
;;; Copyright (c) 2021 Hanshen Wang.
;;;
;;; Author: Hanshen Wang <Hanshen@HanshenWang.com>
;;; URL: https://github.com/HanshenWang/project-isidore
;;;
;;; This file is part of Project Isidore.
;;;
;;; Project Isidore is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Project Isidore is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Project Isidore.  If not, see <https://www.gnu.org/licenses/>.

;;; See generate-index-jss under application.lisp
(ps:chain ps-dom2-symbols:document
       (ps-dom2-symbols:add-event-listener "DOMContentLoaded"
                                           (lambda (event)
                                             (ps:var data-text (ps:array "Hey there," "Bonjour." "¡Hola!" "Привет." "Hello!" "Guten Tag." "Good Day," "Welcome!" "Konnichiwa,"))
                                             (defun type-writer(text i fn-callback)
                                               (cond ((< i (length text))
                                                      (setf (ps:chain ps-dom2-symbols:document(query-selector "h1")ps-dom-nonstandard-symbols:inner-h-t-m-l) (+(ps:chain text (substring 0 (+ i 1))) "<span aria-hidden=\"true\"></span>"))
                                                      (ps-window-wd-symbols:set-timeout (lambda () (type-writer text (+ i 1) fn-callback)) 100))
                                                     ((equal (ps:typeof fn-callback) "function")
                                                      (ps-window-wd-symbols:set-timeout fn-callback 700))))
                                             (defun start-text-animation (i)
                                               (when (equal (ps:typeof (aref data-text i)) "undefined")
                                                 (ps-window-wd-symbols:set-timeout (lambda () (start-text-animation 0))2000))
                                               (when (< i (length (aref data-text i)))
                                                 (type-writer (aref data-text i) 0 (lambda () (start-text-animation (+ i 1))))))
                                             (start-text-animation 0))))
