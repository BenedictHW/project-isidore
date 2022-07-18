;;;; SPDX-FileCopyrightText: 2021 Benedict Hanshen Wang <foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:project-isidore/webserver
  (:use #:common-lisp #:series)
  (:use-reexport #:hunchentoot #:hunchensocket #:quux-hunchentoot)
  (:documentation
   "Added features to the Hunchentoot web server

1. Websocket protocol, RFC 6455, with hunchensocket.

2. A thread-pooling taskmaster for better performance with quux-hunchentoot.

Reexport symbols in these three packages under one unified namespace: webserver."))

(in-package #:project-isidore/webserver)

