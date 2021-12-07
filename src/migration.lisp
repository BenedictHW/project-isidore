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
  (:export
   :*bible-book-url-alist*)
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
(defparameter *bible-book-url-alist*
  (list (cons "/bible?verses=1-1-1-1-50-25" "Genesis")
        (cons "/bible?verses=2-1-1-2-40-36" "Exodus")
        (cons "/bible?verses=3-1-1-3-27-34" "Leviticus")
        (cons "/bible?verses=4-1-1-4-36-13" "Numbers")
        (cons "/bible?verses=5-1-1-5-34-12" "Deuteronomy")
        (cons "/bible?verses=6-1-1-6-24-33" "Joshua")
        (cons "/bible?verses=7-1-1-7-21-24" "Judges")
        (cons "/bible?verses=8-1-1-8-4-22" "Ruth")
        (cons "/bible?verses=9-1-1-9-31-13" "I Samuel")
        (cons "/bible?verses=10-1-1-10-24-25" "II Samuel")
        (cons "/bible?verses=11-1-1-11-22-54" "I Kings")
        (cons "/bible?verses=12-1-1-12-25-30" "II Kings")
        (cons "/bible?verses=13-1-1-13-29-30" "I Chronicles")
        (cons "/bible?verses=14-1-1-14-36-23" "II Chronicles")
        (cons "/bible?verses=15-1-1-15-10-44" "Ezra")
        (cons "/bible?verses=16-1-1-16-13-31" "Nehemiah")
        (cons "/bible?verses=17-1-1-17-14-17" "Tobit")
        (cons "/bible?verses=18-1-1-18-16-31" "Judith")
        (cons "/bible?verses=19-1-1-19-16-24" "Esther")
        (cons "/bible?verses=20-1-1-20-42-16" "Job")
        (cons "/bible?verses=21-1-1-21-150-6" "Psalms")
        (cons "/bible?verses=22-1-1-22-31-31" "Proverbs")
        (cons "/bible?verses=23-1-1-23-12-14" "Ecclesiastes")
        (cons "/bible?verses=24-1-1-24-8-14" "Song of Solomon")
        (cons "/bible?verses=25-1-1-25-19-20" "Wisdom")
        (cons "/bible?verses=26-1-1-26-51-38" "Sirach")
        (cons "/bible?verses=27-1-1-27-66-24" "Isaiah")
        (cons "/bible?verses=28-1-1-28-52-34" "Jeremiah")
        (cons "/bible?verses=29-1-1-29-5-22" "Lamentations")
        (cons "/bible?verses=30-1-1-30-6-72" "Baruch")
        (cons "/bible?verses=31-1-1-31-48-35" "Ezekiel")
        (cons "/bible?verses=32-1-1-32-14-42" "Daniel")
        (cons "/bible?verses=33-1-1-33-14-10" "Hosea")
        (cons "/bible?verses=34-1-1-34-3-21" "Joel")
        (cons "/bible?verses=35-1-1-35-9-15" "Amos")
        (cons "/bible?verses=36-1-1-36-1-21" "Obadiah")
        (cons "/bible?verses=37-1-1-37-4-11" "Jonah")
        (cons "/bible?verses=38-1-1-38-7-20" "Micah")
        (cons "/bible?verses=39-1-1-39-3-19" "Nahum")
        (cons "/bible?verses=40-1-1-40-3-19" "Habakkuk")
        (cons "/bible?verses=41-1-1-41-3-20" "Zephaniah")
        (cons "/bible?verses=42-1-1-42-2-24" "Haggai")
        (cons "/bible?verses=43-1-1-43-14-21" "Zechariah")
        (cons "/bible?verses=44-1-1-44-4-6" "Malachi")
        (cons "/bible?verses=45-1-1-45-16-24" "I Maccabees")
        (cons "/bible?verses=46-1-1-46-15-40" "II Maccabees")
        (cons "/bible?verses=47-1-1-47-28-20" "Matthew")
        (cons "/bible?verses=48-1-1-48-16-20" "Mark")
        (cons "/bible?verses=49-1-1-49-24-53" "Luke")
        (cons "/bible?verses=50-1-1-50-21-25" "John")
        (cons "/bible?verses=51-1-1-51-28-31" "Acts")
        (cons "/bible?verses=52-1-1-52-16-27" "Romans")
        (cons "/bible?verses=53-1-1-53-16-24" "I Corinthians")
        (cons "/bible?verses=54-1-1-54-13-13" "II Corinthians")
        (cons "/bible?verses=55-1-1-55-6-18" "Galatians")
        (cons "/bible?verses=56-1-1-56-6-24" "Ephesians")
        (cons "/bible?verses=57-1-1-57-4-23" "Philippians")
        (cons "/bible?verses=58-1-1-58-4-18" "Colossians")
        (cons "/bible?verses=59-1-1-59-5-28" "I Thessalonians")
        (cons "/bible?verses=60-1-1-60-3-18" "II Thessalonians")
        (cons "/bible?verses=61-1-1-61-6-21" "I Timothy")
        (cons "/bible?verses=62-1-1-62-4-22" "II Timothy")
        (cons "/bible?verses=63-1-1-63-3-15" "Titus")
        (cons "/bible?verses=64-1-1-64-1-25" "Philemon")
        (cons "/bible?verses=65-1-1-65-13-25" "Hebrews")
        (cons "/bible?verses=66-1-1-66-5-20" "James")
        (cons "/bible?verses=67-1-1-67-5-14" "I Peter")
        (cons "/bible?verses=68-1-1-68-3-18" "II Peter")
        (cons "/bible?verses=69-1-1-69-5-21" "I John")
        (cons "/bible?verses=70-1-1-70-1-13" "II John")
        (cons "/bible?verses=71-1-1-71-1-15" "III John")
        (cons "/bible?verses=72-1-1-72-1-25" "Jude")
        (cons "/bible?verses=73-1-1-73-22-21" "Revelation of John"))
  "Associative list of bible books with their respective links for easy conversion into HTML links.")
