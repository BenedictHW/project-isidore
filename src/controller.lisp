;;;; SPDX-FileCopyrightText: 2021 Hanshen Wang <Hanshen@HanshenWang.com>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(defpackage #:project-isidore/controller
  (:use #:common-lisp
        #:project-isidore/views
        #:project-isidore/model
        #:clog)
  ;; No package local nicknames. See commit 1962a26.
  (:export #:initialize-application
           #:application-toplevel)
  (:documentation
   "Project Isidore Controller.

This package contains URI handler and routing logic.

`hunchentoot:define-easy-handler' links an uri with a function postfixed with
'-page'. It is said function which will generate the \"view\", or output HTML.

1. Serve dynamically generated HTML

See `index-page' (inline CSS + JS example), `about-page', `work-page' and
`contact-page'.

2. Serve dynamically generated HTML from persistent CLOS object state.

Persistent CLOS object state equals our datastore in this use case.
`create-datastore' as defined in MODEL.LISP deserializes data found in the
\"project-isidore/data/\" See `bible-page'. "))

(in-package #:project-isidore/controller)

;;; Application start.

(defun initialize-application (&key (port 8080))
  "Start a web server at PORT. Takes a PORT parameter as
Heroku assigns a different PORT per dyno/environment. "
  (format t "~%
========================================
Project Isidore v1.2.1 (A.D. 2022-01-15)
========================================

Copyright (c) 2021 Hanshen Wang <Hanshen@HanshenWang.com>

Project Isidore is free software, provided as is, and comes with ABSOLUTELY NO
WARRANTY. This program is licensed under the GNU AFFERO GENERAL PUBLIC LICENSE
Version 3, 19 November 2007 You are welcome to redistribute the program or any
parts hereof under certain conditions. Please visit
https://www.gnu.org/licenses/agpl-3.0.html for License details.

Homepage: https://www.hanshenwang.com/blog/project-isidore-doc.html

Source code repository: https://github.com/HanshenWang/project-isidore ~% ")
  ;; `sb-ext:save-lisp-and-die' closes all open streams. We need to open these
  ;; streams again.
  (setf *search-index*
        (make-instance 'montezuma:index
                       :path (asdf:system-relative-pathname
                              :project-isidore "data/montezuma/")
                       :default-field "*"
                       :fields '("b" "c" "v" "t" "h"))
        *database*
        (rs:open-rucksack
         (asdf:system-relative-pathname :project-isidore "data/rucksack/")))
  (initialize 'on-index
              :static-root
              (merge-pathnames
               "assets/" (asdf:system-source-directory :project-isidore)))
  (set-on-new-window 'on-main :path "/main")
  (set-on-new-window 'on-bible :path "/bible")
  (set-on-new-window 'on-bible-search :path "/bible-search")
  (format t "~%
Navigate to http://localhost:~A to continue... ~%" port)
  (return-from initialize-application t))

(defun application-toplevel ()
  "Application entry point. Emulate a \"main\" function. Used in
  SAVE-LISP-AND-DIE to save Application as a Lisp image. Note PORT is a keyword
  argument that defaults to 8080. Heroku dynamically sets the PORT variable to
  be binded."
  (initialize-application :port (if (equalp NIL (uiop:getenv "PORT"))
                                    8080
                                    (parse-integer (uiop:getenv "PORT"))))
  ;; Sleep forever.
  (loop (sleep 600)))

(defun on-index (body)
  (setf (title (html-document body)) "HanshenWang.com")
  (load-css (html-document body) "index.css")
  (let* ((sb (create-style-block body))
         (bg (create-section body :ul :class "slideshow" :content
                             (who:with-html-output-to-string (*standard-output* nil :prologue nil :indent nil)
                               (loop for picture in '("pic1.webp"
                                                      "pic2.webp"
                                                      "pic3.webp"
                                                      "pic4.webp"
                                                      "pic5.webp"
                                                      "pic6.webp")
                                     do (who:htm (:li (:span (cl-who:str picture))))))))
         (container (create-div body :class "container"))
         (header (place-inside-bottom-of container (create-section body :header)))
         (portfolio-container (place-inside-bottom-of header (create-div body :class "portfolio-container")))
         (portfolio-picture (place-inside-bottom-of portfolio-container (create-div body :class "portfolio-section" :content (who:with-html-output-to-string (*standard-output* nil :prologue nil :indent nil) (:img :src "profile.webp" :alt "Author Profile Picture")))))
         (portfolio-title (place-inside-bottom-of portfolio-container (create-div body :class "portfolio-section")))
         (portfolio-greeting (place-inside-bottom-of portfolio-title (create-section body :h1 :content "Hey There!")))
         (portfolio-name (place-inside-bottom-of portfolio-title (create-section body :h2 :content "I'm Hanshen.")))
         (portfolio-intro (place-inside-bottom-of portfolio-title (create-section body :h2 :content "Nice to meet you.")))
         (portfolio-hr (create-hr header))
         (portfolio-text (place-inside-bottom-of header (create-child body (who:with-html-output-to-string (*standard-output* nil :prologue nil :indent nil) (:p "Welcome to my personal website! This website was built with" (:a :href "https://nextjs.org" :target "_blank" :rel "noreferrer" (cl-who:htm (:s "Next.js and React"))) (:a :href "https://edicl.github.io/hunchentoot/" :target "_blank" :rel "noreferrer" " Hunchentoot") "and Common Lisp. My resume can be found under the work tab. I hope you find what you're looking for,and may the wind be always at your back.")))))
         (portfolio-links (place-inside-bottom-of header (create-child body (who:with-html-output-to-string (*standard-output* nil :prologue nil :indent nil) (:p :class "portfolio-button" (:a :href "/main?page=about" "About") (:a :href "/main?page=work" "Work") (:a :href "/blog/archive.html" "Blog") (:a :href "/main?page=contact" "Contact")))))))
    (create-br portfolio-greeting)
    (create-br portfolio-name)
    (create-br portfolio-hr)
    (add-style sb :element "hr" '(("height" :1px)))
    ;; (format t "=> ~A" (outer-html (document-element (html-document body))))
    (set-on-click portfolio-intro
                  (lambda (obj)
                    (declare (ignore obj))
                    (setf (inner-html body) (about-page))))
    (loop repeat 3
          do (loop for greeting in '("Hey there," "Bonjour." "¡Hola!" "Привет." "Hello!" "Guten Tag." "Good Day," "Welcome!" "Konnichiwa,")
                   do (progn
                        (setf (text portfolio-greeting) greeting)
                        (sleep 1))))
    (run body)))

(defun on-main (body)
  (setf (title (html-document body)) "HanshenWang.com")
  (load-css (html-document body) "global.css")
  (let* ((header-div (create-div body :class "header header-fixed"))
         (navbar-div (place-inside-bottom-of
                      header-div (create-div body :class "navbar container")))
         (logo-div (place-inside-bottom-of
                    navbar-div (create-div body :class "logo")))
         (logo (create-a logo-div :content "Hanshen Wang"
                                  :class "nav-link"
                                  :link "/"))
         (tmp (place-inside-bottom-of navbar-div (create-child body (who:with-html-output-to-string (*standard-output* nil :prologue nil :indent nil) (:input :type "checkbox")) :html-id "navbar-toggle")))
         (tmp (place-inside-bottom-of navbar-div (create-child body (who:with-html-output-to-string (*standard-output* nil :prologue nil :indent nil) (:label :for "navbar-toggle" (:i)) ))))
         (navbar-menu (place-inside-bottom-of navbar-div (create-section body :nav :class "menu")))
         (navbar-list (place-inside-bottom-of navbar-menu (create-unordered-list body)))
         (about-button (place-inside-bottom-of
                        navbar-list (create-child
                                     body (who:with-html-output-to-string
                                              (*standard-output* nil :prologue nil :indent nil)
                                            (:li (:a "About"))))))
         (work-button (place-inside-bottom-of
                       navbar-list (create-child
                                    body (who:with-html-output-to-string
                                             (*standard-output* nil :prologue nil :indent nil)
                                           (:li (:a "Work"))))))
         (blog-button (place-inside-bottom-of
                       navbar-list (create-child
                                    body (who:with-html-output-to-string
                                             (*standard-output* nil :prologue nil :indent nil)
                                           (:li (:a :href "/blog/archive.html" "Blog"))))))
         (contact-button (place-inside-bottom-of
                          navbar-list (create-child
                                       body (who:with-html-output-to-string
                                                (*standard-output* nil :prologue nil :indent nil)
                                              (:li (:a "Contact"))))))
         (params (form-get-data body))
         (page-text (create-child
                     body (cond ((string-equal "about" (cdr (car params)))
                                 (about-text))
                                ((string-equal "work" (cdr (car params)))
                                 (work-text))
                                ((string-equal "contact" (cdr (car params)))
                                 (contact-text))
                                ((string-equal "subscribe" (cdr (car params)))
                                 (subscribe-text))
                                )))
         (tmp (create-section body :hr))
         (footer (create-section body :div :class "copyright" :content "Copyright &copy; 2021 Hanshen Wang."))
         )
    ;; (format t "=> ~A" (outer-html (document-element (html-document body))))
    (set-on-click about-button
                  (lambda (obj)
                    (declare (ignore obj))
                    (setf (inner-html page-text) (about-text))))
    (set-on-click work-button
                  (lambda (obj)
                    (declare (ignore obj))
                    (setf (inner-html page-text) (work-text))))
    (set-on-click contact-button
                  (lambda (obj)
                    (declare (ignore obj))
                    (setf (inner-html page-text) (contact-text))))
    (run body)))

(defun on-bible (body)
  (let ((params (form-get-data body)))
    (create-div body :content (bible-page (cdr (car params))))
    (run body)))

(defun on-bible-search (body)
  (let ((params (form-get-data body)))
    (create-div body :content (bible-search-page (cdr (car params))))
    (run body)))

(defun work-text ()
  (who:with-html-output-to-string (*standard-output* nil :prologue nil :indent nil)
    (:div :class "main"
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
           (:li (:a :href "/blog/archive.html" "Collected Notes"))))))

(defun about-text ()
  (who:with-html-output-to-string (*standard-output* nil :prologue nil :indent nil)
    (:div :class "main"
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
    use, and pay it forwards."))
    ))

(defun contact-text ()
  (who:with-html-output-to-string (*standard-output* nil :prologue nil :indent nil)
    (:div :class "main"
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
                                                               "_blank" :href "https://hanshenwang.com/main?page=subscribe" "Subscribe to the Mailing
  List."))
          (:h1 :id "article-history" "Blog Article Transparency Policy")
          (:p "All edits made to an article after the initial publication date can be
    found" (:a :target "_blank"
               :href "https://github.com/HanshenWang/project-isidore/tree/master/assets/blog" "in
    the version-controlled Github repository ."))
          )))

(defun subscribe-text ()
  (who:with-html-output-to-string (*standard-output* nil :prologue nil :indent nil)
    (:div :class "main"
          (:h1 :class "title" "Subscribe to Mailing List")
          (:p "The Real Simple Syndication (RSS) protocol best represents the World
  Wide Web as originally visualized: as an information highway. It does so while
  respecting your privacy, and maintains a high signal-to-noise ratio. \"Walled
  gardens\" have a vested interest beyond delivering news, but rather are
  primarily businesses in advertising and data harvesting.")
          (:a :target "_blank"
              :href "https://www.wired.com/story/rss-readers-feedly-inoreader-old-reader/"
              "Further information on getting started with RSS can be found here.")
          (:a :target "_blank"
              :href "http://hanshenwang.com/blog/rss.xml"
              "My blog RSS is here.")
          (:p "I highly recommend the use of RSS for newsgroups and news reading.
  Still, an option exists to receive new blog articles by E-mail. This mailing
  list exists for that sole purpose and nothing else. Subscriptions are
  double-opt in.")
          (:h1 :style "text-align:center;" "Sign up")
          (:form :style "text-align:center;" :action "https://HanshenWang.us14.list-manage.com/subscribe/post?u=361768abae01d8b2d0358aa3d&amp;id=1433484afa"
                 :method "POST"
                 :id "mc-embedded-subscribe-form"
                 :name "mc-embedded-subscribe-form"
                 :class "validate"
                 :target "_blank"
                 (:label :for "mce-EMAIL" "Email Address")
                 (:input :type "email" :value "" :name "EMAIL" :class "required email" :id "mce-EMAIL" :size "50")
                 (:input :type "submit" :value "Subscribe" :name "subscribe" :id "mc-embedded-subscribe" :class "button"))
          )))
