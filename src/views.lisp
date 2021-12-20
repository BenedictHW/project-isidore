;;;; views.lisp
;;;
;;; Copyright (c) 2021 Hanshen Wang.
;;;
;;; Author: Hanshen Wang <Hanshen@HanshenWang.com>
;;; URL: https://github.com/HanshenWang/project-isidore
;;;
;;; This file is part of Project Isidore.
;;;
;;; Project Isidore is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU Lesser General Public License as published by the Free
;;; Software Foundation, either version 3 of the License, or (at your option)
;;; any later version.
;;;
;;; Project Isidore is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for
;;; more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public License along with
;;; Project Isidore. If not, see <https://www.gnu.org/licenses/>.

(uiop:define-package #:project-isidore/views
  (:use #:common-lisp
        #:project-isidore/styles
        #:project-isidore/model)
  (:import-from #:cl-who)
  (:import-from #:parenscript)
  ;; No package local nicknames. See commit 1962a26.
  (:export
   #:index-page
   #:about-page
   #:work-page
   #:contact-page
   #:subscribe-page
   #:subscribe-success-page
   #:unsubscribe-page
   #:unsubscribe-success-page
   #:bible-page)
  (:documentation
   "Project Isidore Web Page View Generation.

Uses the CL-WHO and PARENSCRIPT libraries to generate both HTML and Javascript
respectively.

CSS is generated in STYLES.LISP.

When developing with both the =view= and =style= files, it is useful to view the
generated HTML, CSS or JS. However SLIME or SLY will cut off long streams in the
REPL and insert =[sly-elided string of length x]=. To disable this behavior,

#+begin_src lisp
  ;; https://github.com/joaotavora/sly/issues/334
  ;; Consider putting this in your .sbclrc or lisp configuration file.
  (setf (cdr (assoc 'slynk:*string-elision-length* slynk:*slynk-pprint-bindings*)) nil)
#+end_src "))

(in-package #:project-isidore/views)

(defmacro web-page-template ((&key title) &body body)
  "Template HTML for application webpages. Other than the landing page (aka
'index.html'), the static blog post HTML files and other generated HTML files,
all other web app pages uses this boilerplate."
  `(cl-who:with-html-output-to-string (*standard-output* nil :prologue t :indent t)
     (:html :lang "en"
            (:head
             (:meta :charset "utf-8" :name "viewport"
             :content "width=device-width, initial-scale=1")
             (:title, title)
             (:link :rel "preconnect" :href="https://fonts.googleapis.com")
             (:link :rel "preconnect" :href="https://fonts.gstatic.com"
             :crossorigin)
             (:link
             :href "https://fonts.googleapis.com/css2?family=Cinzel:wght@400;500&family=EB+Garamond:ital@0;1&family=Montserrat:ital@0;1&display=swap"
             :rel "stylesheet")
             (:link :type "text/css" :href "global.css" :rel "stylesheet"))
            (:body
             (:div :class "header header-fixed"
                   (:div :class "navbar container"
                         (:div :class "logo" (:a :href "/" "Hanshen Wang"))
                         (:input :type "checkbox" :id "navbar-toggle")
                         (:label :for "navbar-toggle" (:i))
                         (:nav :class "menu"
                               (:ul
                                (:li (:a :href "/about" "About"))
                                (:li (:a :href "/work" "Work"))
                                (:li (:a :href "/blog/archive.html" "Blog"))
                                (:li (:a :href "/contact" "Contact"))))))
             (:div :class "main" ,@body))
            (:hr)
            (:footer
             (:div :class "copyright-container"
                   (:div :class "copyright" "Copyright &copy; 2021 Hanshen Wang.
                   All Rights Reserved."))))))

(defun index-page ()
  (cl-who:with-html-output-to-string (*standard-output* nil :prologue t :indent t)
    (:html :lang "en"
           (:head
            (:meta :charset "utf-8" :name "viewport" :content
                   "width=device-width, initial-scale=1" :description "Personal
                   Web Application")
            (:title "HanshenWang.com")
            (:link :rel "preconnect" :href "https://fonts.googleapis.com")
            (:link :rel "preconnect" :href "https://fonts.gstatic.com"
            :crossorigin)
            (:link
            :href "https://fonts.googleapis.com/css2?family=Cinzel:wght@400;500&family=EB+Garamond:ital@0;1&family=Montserrat:ital@0;1&display=swap"
            :rel "stylesheet")
            (:style (cl-who:str
                     (index-css))))
           (:body
            (:ul :class "slideshow" ; background CSS3 slideshow
                 (:li (:span "pic1.webp"))
                 (:li (:span "pic2.webp"))
                 (:li (:span "pic3.webp"))
                 (:li (:span "pic4.webp"))
                 (:li (:span "pic5.webp"))
                 (:li (:span "pic6.webp")))
            (:div :class "container"
                  (:header
                   (:div :class "portfolio-container"
                         (:div :class "portfolio-section"
                               (:img :src "profile.webp" :alt "Author Profile
                               Picture"))
                         (:div :class "portfolio-section"
                               ;; index.js typewriter effect needs the greeting
                               ;; to be HTML tag h1.
                               (:h1 "Hey There!") (:br)
                               (:h2 "I'm Hanshen.") (:br)
                               (:h2 "Nice to meet you.")))
                   (:hr)
                   (:br)
                   (:p "Welcome to my personal website! This website was built
  with" (:a :href "https://nextjs.org" :target "_blank" :rel "noreferrer" "<s>
  Next.js") "and React </s>" (:a :href "https://edicl.github.io/hunchentoot/"
                                 :target "_blank"
                                 :rel "noreferrer""Hunchentoot") "and Common
                                 Lisp. My resume can be found under the work
                                 tab. I hope you find what you're looking for,
                                 and may the wind be always at your back.")
                   (:p :class "portfolio-button"
                       (:a :href "/about" "About")
                       (:a :href "/work" "Work")
                       (:a :href "/blog/archive.html" "Blog")
                       (:a :href "/contact" "Contact")))))
           (:script :type "text/javascript"
                    (cl-who:str
                     ;; For a tutorial see: https://app.leby.org/post/fun-with-parenscript/
                     (parenscript:ps-inline
                         ((parenscript:chain ps-dom2-symbols:document
                                    (ps-dom2-symbols:add-event-listener "DOMContentLoaded"
                                                                        (lambda (event)
                                                                          (parenscript:var data-text (parenscript:array "Hey there," "Bonjour." "¡Hola!" "Привет." "Hello!" "Guten Tag." "Good Day," "Welcome!" "Konnichiwa,"))
                                                                          (defun type-writer(text i fn-callback)
                                                                            (cond ((< i (length text))
                                                                                   (setf (parenscript:chain ps-dom2-symbols:document(query-selector "h1")ps-dom-nonstandard-symbols:inner-h-t-m-l) (+(parenscript:chain text (substring 0 (+ i 1))) "<span aria-hidden=\"true\"></span>"))
                                                                                   (ps-window-wd-symbols:set-timeout (lambda () (type-writer text (+ i 1) fn-callback)) 100))
                                                                                  ((equal (parenscript:typeof fn-callback) "function")
                                                                                   (ps-window-wd-symbols:set-timeout fn-callback 700))))
                                                                          (defun start-text-animation (i)
                                                                            (when (equal (parenscript:typeof (aref data-text i)) "undefined")
                                                                              (ps-window-wd-symbols:set-timeout (lambda () (start-text-animation 0))2000))
                                                                            (when (< i (length (aref data-text i)))
                                                                              (type-writer (aref data-text i) 0 (lambda () (start-text-animation (+ i 1))))))
                                                                          (start-text-animation 0)))))))))))

(defun about-page ()
  (web-page-template (:title "HanshenWang.com")
    (:h1 :class "title" "About")
    (:h1 "The story so far...")
    (:p "Hey there. I'm currently a student up in the Great White North.
    Apart from studies and work, I enjoy swimming, piano, and eating
    peanut butter straight from the jar.")
    (:h1 "Why does this website exist?")
    (:p "This website's raison d'etre is partly because of the free time granted
    by the COVID pandemic of 2020-2021. A historic event, and my heart goes out
    to those suffering still. I am under no illusion the opportunities my
    station in life affords me. In the same vein of thought, I built this
    website because the internet -- in its current form -- has given me
    countless graces
    (and temptations, but that's to be expected). So please forgive my
    amateurish writing, for it is my wish that you will take away something of
    use, and pay it forwards.")))

(defun work-page ()
  (web-page-template (:title "HanshenWang.com")
    (:h1 :class "title" "Work")
    (:h1 "Professional Portfolio ")
    (:a :target "_blank"
    :href "https://drive.google.com/file/d/1-D_CkQhgazvBCNr5v3CyxEbHZRXFYXKy/view?usp=sharing" "Click
    this link to view or download my resume (link to Google Drive).")
    (:br)
    (:p "Please kindly" (:a :href "/contact" "shoot me an email") "should the
    link above be broken.")
    (:h1 "Side Projects")
    (:p "Here one will find my Github repos, my book/course notes (all mistakes
    are mine), and any other miscellaneous items that can be used at one's
    leisure. As with the above," (:a :href "/contact" "do contact me") "if
    something is missing")
    (:ul
     (:li (:a :target "_blank" :href "https://github.com/HanshenWang" "Github
     Repositories"))
     (:li (:a :href "/blog/archive.html" "Collected Notes")))))

(defun contact-page ()
  (web-page-template (:title "HanshenWang.com")
    (:h1 :class "title" "Contact")
    (:h1 "Ways to get in contact")
    ;; https://stackoverflow.com/questions/483212/effective-method-to-hide-email-from-spam-bots
    (:p "Questions, comments, death threats? Don't hesitate to reach out to me
    via email at:" (:a :class "cryptedmail" :data-name "hanshen"
    :data-domain "hanshenwang" :data-tld "com" :onclick "window.location.href =
    'mailto:' + this.dataset.name + '@' + this.dataset.domain + '.' +
    this.dataset.tld; return false;" :href "#"))
    (:p "My PGP Key Fingerprint: 06DD A936 90F7 75E3 715B 628C CA94 9A6D 46BC
    2BBE")
    (:p "My PGP Public Key is available" (:a :target "_blank"
    :href "0x06DDA93690F775E3715B628CCA949A6D46BC2BBE.asc" "here") "and as a
    secondary source, at" (:a :target "_blank"
    :href "https://keys.openpgp.org" "https://keys.openpgp.org."))
    (:p "To receive blog article updates please use" (:a :target "_blank" :href
  "https://hanshenwang.com/blog/rss.xml" "the Blog RSS Feed") " or " (:a :target
  "_blank" :href "https://hanshenwang.com/subscribe" "Subscribe to the Mailing
  List."))
    (:h1 :id "article-history" "Blog Article Transparency Policy")
    (:p "All edits made to an article after the initial publication date can be
    found" (:a :target "_blank"
    :href "https://github.com/HanshenWang/project-isidore/tree/master/assets/blog" "in
    the version-controlled Github repository ."))))

(defun subscribe-page ()
  (web-page-template (:title "HanshenWang.com")
    (:h1 :class "title" "Subscribe to Mailing List")
    (:p "The Real Simple Syndication (RSS) protocol best represents the World
  Wide Web as originally visualized: as an information highway. It does so while
  respecting your privacy, and maintains a high signal-to-noise ratio. \"Walled
  gardens\" have a vested interest beyond delivering news, but rather primarily
  are businesses in advertising and data harvesting.")
    (:a :target "_blank"
        :href "https://www.wired.com/story/rss-readers-feedly-inoreader-old-reader/"
  "Further information on getting started with RSS can be found here.")
    (:p "I highly recommend the use of RSS for newsgroups and news reading.
  Still, an option exists to receive new blog articles by E-mail. This mailing
  list exists for that sole purpose and nothing else.")
    (:h1 "Sign up")
    (:form :action "/create-subscriber" :method "post"
           (:label "Title: ")
           (:input :name "friend-title" :size "50" :type "text"
           :required "required")
           (:br)
           (:label "Name: ")
           (:input :name "friend-name" :size "50" :type "text"
           :required "required")
           (:br)
           (:label "E-mail: ")
           (:input :name "friend-email" :size "50" :type "email"
           :required "required")
           (:input :name "commit" :type "submit" :value "Submit"))))

(defun subscribe-success-page (email)
  (web-page-template (:title "HanshenWang.com")
    (:h1 :class "title" "Subscribe to Mailing List")
    (cl-who:htm (:p "Thank you. The E-mail Address: " (:code (cl-who:str email)) " has
  been successfully subscribed."))
    (:a :target "_blank" :href "/" "Return to homepage." )))

(defun unsubscribe-page ()
  (web-page-template (:title "HanshenWang.com")
    (:h1 :class "title" "Unsubscribe from Mailing List")
    (:form :action "/delete-subscriber" :method "post"
           (:label "E-mail: ")
           (:input :name "friend-email" :size "50" :type "email"
           :required "required")
           (:input :name "commit" :type "submit" :value "Submit"))))

(defun unsubscribe-success-page (email)
  (web-page-template (:title "HanshenWang.com")
    (:h1 :class "title" "Unsubscribe from Mailing List")
    (cl-who:htm (:p "The E-mail Address: " (:code (cl-who:str email)) " has been
  successfully unsubscribed. Have a good one."))
    (:a :target "_blank" :href "/" "Return to homepage." )))

(defun bible-page (bible-url)
  "127.0.0.1:8080/bible?verses=1-2-3-4-5-6 where BIBLE-URL \"1-2-3-4-5-6\" is a string with BEGINNINGbook-chapter-verse-ENDINGbook-chapter-verse."
  (web-page-template (:title "Tabular Douay Rheims Bible")
    (:h1 :class "title" "Tabular Douay Rheims Bible")
    (:h4 "Presents Fr. Haydock's commentary side-by-side for ease of reading. For more information.")
    (:div :style "overflow:auto"
          ;; Present links to all books of the bible.
          (loop for (link . title) in *bible-book-url-alist*
                do (cl-who:htm
                    (:div :style "width:200px;float:left"
                          (:a :href link (:b (cl-who:esc title)))))))
    (:br)
    (:div :style "overflow:auto"
          ;; Present links to all chapters of currently selected book.
          (loop for (link . title) in (make-bible-chapter-url-list bible-url)
                do (cl-who:htm
                    (:div :style "width:200px;float:left"
                          (:a :href link (:b (cl-who:esc title)))))))
    (:table
     ;; Present tabular view of bible text.
     (loop for bible-uid from (car (bible-url-to-uid bible-url))
             to (cadr (bible-url-to-uid bible-url))
           do (cl-who:htm
               (:tr
                (:td (cl-who:htm (cl-who:esc (get-heading-text bible-uid))))
                (:td (cl-who:htm (cl-who:esc (get-bible-text bible-uid))))
                (:td :width "50%" (cl-who:htm (cl-who:esc (get-haydock-text bible-uid))))))))))

