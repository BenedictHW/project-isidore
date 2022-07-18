;;;; SPDX-FileCopyrightText: 2021 Benedict Hanshen Wang <foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:project-isidore/theophilus
  (:use #:common-lisp)
  (:use-reexport #:alexandria #:serapeum)
  (:documentation
   "If the Aristotelian tradition so loved and integrated by the Angelic Doctor
shares heritage with the Greek culture within the great Library of Alexandria,
and its so-called daughter of the great Library, the temple of Serapeum, then
let Saint Theophilus of Alexandria be the discerning flame that took what is
good and true from said traditions -- while fighting idolotrous aspects -- and
handed it down to Western Civilization (nee Christendom).

https://en.wikipedia.org/wiki/Serapeum_of_Alexandria

It's a personal utility package. Clearly I have nothing better to do than to
come up with fanciful names.

Symbol Conflicts:

1. SCAN, SERIES, SERAPEUM

Reexport symbols in these utility packages under one unified namespace."))

(in-package #:project-isidore/theophilus)

