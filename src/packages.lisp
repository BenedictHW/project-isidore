;;;; packages.lisp
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

(uiop/package:define-package #:project-isidore/src/packages
    (:nicknames #:project-isidore)
  ;; No package local nicknames. See commit 1962a26.
  (:use-reexport #:project-isidore/src/application
                 #:project-isidore/src/model
                 #:project-isidore/src/styles
                 #:project-isidore/src/views)
  (:documentation
   "Project Isidore meta package. Export a single package PROJECT-ISIDORE with
    all the external symbols from all inferred packages. Inferred here is taken
    to signify the ASDF Package Inferred System.

    For the web application entry point, see INITIALIZE-APPLICATION. For a
    comprehensive index of exported symbols and definitions, see the Reference
    Manual at /project-isidore/assets/reference/manual.html "))

