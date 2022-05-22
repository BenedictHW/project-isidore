;;;; SPDX-FileCopyrightText: 2021 Hanshen Wang <Hanshen@HanshenWang.com>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:project-isidore/gserver-taskmaster
  (:use #:common-lisp
        #:series)
  (:import-from #:cl-gserver)
  ;; No package local nicknames. See commit 1962a26.
  (:export #:tmgr-worker
           #:make-tmgr-worker
           #:get-processed-requests)
  (:documentation
   "This is an imported version of cl-tbnl-gserver-tmgr. The original can be found
here: https://github.com/mdbergmann/cl-tbnl-gserver-tmgr. The coding style of
the author is not compatible with ASDF's package-inferred style. The system and
packages have different names i.e. system cl-gserver has no corresponding
\"cl-gserver\" package but rather \"cl-gserver.actor\" \"cl-gserver.queue\" etc.
In addition the proliferation of nicknames in cl-gserver means I have
conflicting packages names. Rucksack has a (defpackage :queue) form while
cl-gserver has (:nicknames :queue). This means I have to manually vendor the
cl-gserver library.
"))

(in-package #:project-isidore/gserver-taskmaster)

(defstruct worker-state
  (processed-requests 0 :type integer))

(defclass tmgr-worker (cl-gserver.actor:actor) ())

(defun make-tmgr-worker (asystem)
  (cl-gserver.actor-context:actor-of asystem
                                     :receive #'receive
                                     :dispatcher :pinned
                                     :state (make-worker-state)))

(defun receive (worker message current-state)
  (declare (ignore worker))
  (case (first message)
    (:process (process-request
               (second message)
               (third message)
               current-state))
    (t (cons current-state current-state))))

(defun process-request (acceptor socket current-state)
  (handler-case
      (progn
        (with-slots (processed-requests) current-state
          (hunchentoot:process-connection acceptor socket)
          (cons
           current-state
           (make-worker-state
            :processed-requests
            (1+ processed-requests)))))))

;; ---------------------------
;; worker facade -------------
;; ---------------------------

(defun get-processed-requests (worker)
  (with-slots (cl-gserver.actor-cell:state) worker
    (slot-value cl-gserver.actor-cell:state 'processed-requests)))
