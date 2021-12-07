;;;; migration.lisp
;;;
;;; Copyright (c) 2021 Hanshen Wang.
;;;
;;; Author: Hanshen Wang <Hanshen@HanshenWang.com>
;;; URL: https://github.com/HanshenWang/project-isidore
;;;
;;; This file is part of Project Isidore.
;;;
;;; Project Isidore is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by the Free
;;; Software Foundation, either version 3 of the License, or (at your option)
;;; any later version.
;;;
;;; Project Isidore is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
;;; more details.
;;;
;;; You should have received a copy of the GNU General Public License along with
;;; Project Isidore. If not, see <https://www.gnu.org/licenses/>.

(defpackage #:project-isidore/migration
  (:use #:common-lisp
        #:project-isidore/model)
  (:import-from #:bknr.datastore)
  (:import-from #:cl-org-mode)
  ;; No package local nicknames. See commit 1962a26.
  (:documentation
   "Migrate data from .txt org-mode file syntax into instances of Common Lisp Object System objects. Email the maintainer of the project listed under PROJECT-ISIDORE.ASD for the .txt plaintext files in org-mode file syntax. The functions in this file should be run sequentially."))

(in-package #:project-isidore/migration)

(defun create-bible (input-book input-chapter input-verse input-text)
  "Helper function for class `bible'. Creates an instance of object `bible' with the four required parameters. See `parse-org-bible' for usage."
  (make-instance 'bible :book input-book
                        :chapter input-chapter
                        :verse input-verse
                        :text input-text))

(defun parse-org-bible ()
  "Convert from org-mode syntax text bible to CLOS objects. Make sure bible.txt
can be found at '../data/bible.txt'. After import of bible.txt, there should be
35817 objects (verses) in total. They are indexed by BKNR.Datastore from 0 to
35816. Verify with (bknr.datastore:store-objects-with-id '35816). Or verify
with, (describe (first (nthcdr 35000 #v2:0))) where #v2:0 is a sly
backreference."
  ;; Definition of current-node relies on org-content so
  ;; sequential binding of let is necessary.
  (let* ((org-content (cl-org-mode::read-org-file "../data/bible.txt"))
         (current-node (cl-org-mode::node.next-node org-content))
         (next-node (cl-org-mode::node.next-node current-node))
         (current-book nil)
         (current-chapter nil)
         (current-verse nil)
         (current-text nil))
  (dotimes (i 73039) ; Equal to the number of lines in org file.
    ;; If a heading slot exists in the current-node.
    (if (slot-exists-p current-node 'cl-org-mode::heading)
        (progn
          ;; Number of * correspond to heading level.
          ;; Check if heading level is equal to 1,
          (when (= 1 (count #\* (slot-value current-node 'cl-org-mode::heading-level-indicator)))
            ;; In the source org file, 1st level headings represent books.
            ;; Save the book title as a string.
            (setf current-book (slot-value current-node 'cl-org-mode::heading)))
          ;; Check If heading level is equal to 2,
          (when (= 2 (count #\* (slot-value current-node 'cl-org-mode::heading-level-indicator)))
            ;; In the source org file, 2nd level headings represent chapters.
            ;; Ex. extract and parse the integer 1 from "Genesis 1".
            (setf current-chapter (parse-integer (first (last (cl-ppcre:split " " (slot-value current-node 'cl-org-mode::heading)))):junk-allowed t)))
          ;; Check If heading level is equal to 3,
          (when (= 3 (count #\* (slot-value current-node 'cl-org-mode::heading-level-indicator)))
            ;; In the source org file, 3rd level headings represent verses.
            ;; Ex. extract and parse an integer 2 from "Genesis 1:2".
            (setf current-verse (parse-integer (first (last (cl-ppcre:split ":" (slot-value current-node 'cl-org-mode::heading)))))))))
    ;; Source filed is structured so that all non-header lines are a verse.
    ;; If that is the case, then create an instance of class `bible'.
    (if (slot-exists-p current-node 'cl-org-mode::text)
        (progn
          (setf current-text (slot-value current-node 'cl-org-mode::text))
          (create-bible current-book current-chapter current-verse current-text)))
    ;; Increment the current-node and the next node.
    (setf current-node next-node)
    (setf next-node (cl-org-mode::node.next-node current-node))
    ;; There 73041 lines, manually add the last verse.
    ;; If instead we put (dotimes (i 73041)) then the next-node
    ;; will go overbounds and return a NIL error.
    (when (= 73038 i)
    ;; The newline is there intentionally,
    ;; to keep conformity with the rest of the verses.
    (create-bible "Revelation of John" 22 21 "The grace of our Lord Jesus Christ be with you all. Amen.
    ")))))

;; Anytime objects from the datastore are modified, they ought to be wrapped
;; in a transaction so they can be 'replayed'.
(bknr.datastore:deftransaction
    create-haydock-commentary (input-bible-uid input-text)
    (setf
     (slot-value
      (bknr.datastore:store-object-with-id input-bible-uid) 'project-isidore/model::haydock-text) input-text))

(defvar *line-counter* 0 "Counter used to find out i variable for dotimes loop")

(defun parse-org-haydock-commentary ()
  "Convert from org-mode syntax text bible to CLOS objects. Make sure
  haydock.txt can be found at '../data/haydock.txt' Run this function after
  `parse-org-bible'."
  (let* ((org-content (cl-org-mode::read-org-file "../data/haydock.txt"))
         (current-node (cl-org-mode::node.next-node org-content))
         (next-node (cl-org-mode::node.next-node current-node))
         (current-book nil)
         (current-chapter nil)
         (current-verse nil)
         (current-text nil))
  (dotimes (i 43099)
    (if (slot-exists-p current-node 'cl-org-mode::heading)
        (progn
          (when (= 1 (count #\* (slot-value current-node 'cl-org-mode::heading-level-indicator)))
            (setf current-book (slot-value current-node 'cl-org-mode::heading)))
          (when (= 2 (count #\* (slot-value current-node 'cl-org-mode::heading-level-indicator)))
            (setf current-chapter (parse-integer (first (last (cl-ppcre:split " " (slot-value current-node 'cl-org-mode::heading)))):junk-allowed t)))
          (when (= 3 (count #\* (slot-value current-node 'cl-org-mode::heading-level-indicator)))
            (setf current-verse (parse-integer (first (last (cl-ppcre:split " " (slot-value current-node 'cl-org-mode::heading)))):junk-allowed t)))))
    (if (slot-exists-p current-node 'cl-org-mode::text)
        (progn
          (setf current-text (slot-value current-node 'cl-org-mode::text))
          (create-haydock-commentary (get-bible-uid current-book current-chapter current-verse) current-text)))
    ;; (setf *line-counter* (incf *line-counter*))
    (setf current-node next-node)
    (setf next-node (cl-org-mode::node.next-node current-node))
    (when (= i 43098)
      (create-haydock-commentary (get-bible-uid 73 22 20) " 21. He that giveth testimony of these things, i.e. God, and Jesus Christ by an Angel, saith, surely, (or even
so, or truly, these are certain truths) I come quickly, to reward the good and punish the evil. To which words S.
John himself replieth with a zealous prayer and earnest desire, saying, Amen, let it be so. — Come, Lord Jesus:
come, and remain always in my soul by thy grace, and make me partaker of thy glory for ever and ever. Amen.
Wi. — Conclusion. The Church in sighs and groans, and by the mouth of her children, solicits the coming of
Jesus Christ, her divine Spouse. The fruit to be drawn from the perusal of this sacred book, is ardently to desire
the kingdom of God, to sigh after the day of eternity, to feel the weight of the yoke of the present life, and the
disgrace of our exile, and to live here below as strangers. Enkindle in me, O Lord, this desire; enable my poor
soul to join with the beloved disciple in this prayer: Come, Lord Jesus; that she may go and lose herself in Thee,
who art her Centre, her God, her All.
")))))