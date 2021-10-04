;;;; application.lisp
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

;;;; initialize-application is the function launched by the heroku buildpack. This has been moved to heroku.lisp as the function must be under the "cl-user" namespace and not project-isidore.
(defpackage #:project-isidore
  ;; :use means there is no need to prefix hunchentoot:define-easy-handler and instead use just define-easy-handler
  ;; As per https://lispcookbook.github.io/cl-cookbook/packages.html#about-use-ing-packages-being-a-bad-practice,
  ;; use nicknames instead
  (:use #:cl)
  (:local-nicknames (#:ws #:hunchentoot) ; web server
                    (#:ht #:cl-who) ; hypertext markup generation
                    (#:css #:cl-css)
                    (#:js #:parenscript))
  (:export #:start-dev-server
           #:stop-dev-server
           #:generate-index-css
           #:generate-global-css
           #:generate-index-js))

;; This must match the defpackage above, remember repl upon startup defaults to cl-user package.
(in-package #:project-isidore)

;; The double colons are to access unexported functions & symbols
;; single colon if it is exported. Be careful with unexported/internal symbols

;; Other than the landing page (aka "index.html") all other web app pages uses this boilerplate
;; Taken from  https://github.com/rajasegar/cl-bootstrap/blob/master/demo/demo.lisp
(defmacro app-page ((&key title) &body body)
  ;; =, g g= to call =jump-to-definition=
  ;; https://stackoverflow.com/questions/30150186/what-does-backtick-mean-in-lisp
  ;; http://www.lispworks.com/documentation/HyperSpec/Body/02_df.htm
  `(ht:with-html-output-to-string (*standard-output* nil :prologue t :indent t)
     (:html :lang "en"
            (:head
             (:meta :charset "utf-8" :name "viewport" :content "width=device-width, initial-scale=1")
             (:title, title)
             (:link :rel "preconnect" :href="https://fonts.googleapis.com")
             (:link :rel "preconnect" :href="https://fonts.gstatic.com" :crossorigin)
             (:link :href  "https://fonts.googleapis.com/css2?family=Cinzel:wght@400;500&family=EB+Garamond:ital@0;1&family=Montserrat:ital@0;1&display=swap" :rel "stylesheet")
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
                   (:div :class "copyright" "Copyright &copy; 2021 Hanshen Wang. All Rights Reserved."))))))

(ws:define-easy-handler (root :uri "/") ()
  (ht:with-html-output-to-string (*standard-output* nil :prologue t :indent t)
    (:html :lang "en"
           (:head
            (:meta :charset "utf-8" :name "viewport" :content
                   "width=device-width, initial-scale=1" :description "Personal Web Application")
            (:title "HanshenWang.com")
            (:link :rel "preconnect" :href "https://fonts.googleapis.com")
            (:link :rel "preconnect" :href "https://fonts.gstatic.com" :crossorigin)
            (:link :href  "https://fonts.googleapis.com/css2?family=Cinzel:wght@400;500&family=EB+Garamond:ital@0;1&family=Montserrat:ital@0;1&display=swap" :rel "stylesheet")
            (:link :type "text/css" :href "index.css" :rel "stylesheet")
            (:script :src "index.js"))
           (:body
            (:ul :class "cb-slideshow" ; background CSS3 slideshow
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
                               (:img :src "profile.webp" :alt "Author Profile Picture"))
                         (:div :class "portfolio-section"
                               (:h1 "Hey There!") (:br)
                               (:h2 "I'm Hanshen.") (:br)
                               (:h2 "Nice to meet you.")))
                   (:hr)
                   (:br)
                   (:p "Welcome to my personal website! This website was built
  with" (:a :href "https://nextjs.org" :target "_blank" :rel "noreferrer" "<s>
  Next.js") "and React </s>" (:a :href "https://edicl.github.io/hunchentoot/"
                                 :target "_blank" :rel "noreferrer""Hunchentoot") "and Common Lisp. My resume can be found under the work tab. I hope you find what you're looking for, and may the wind be always at your back.")
                   (:p :class "portfolio-button"
                       (:a :href "/about" "About")
                       (:a :href "/work" "Work")
                       (:a :href "/blog/archive.html" "Blog")
                       (:a :href "/contact" "Contact"))))))))
(ws:define-easy-handler (about :uri "/about") ()
  (app-page (:title "HanshenWang.com")
    (:h1 :class "title" "About")
    (:h1 "The story so far...")
    (:p "Hey there. I'm currently a student up in the Great White North.
    Apart from studies and work, I enjoy swimming, piano, and eating
    peanut butter straight from the jar.")
    (:h1 "Why does this website exist?")
    (:p "This website's raison d'etre is partly because of the free time granted by
    the COVID pandemic of 2020-2021. A historic event, and my heart goes out to
    those suffering still. I am under no illusion the opportunities my station
    in life affords me. In the same vein of thought, I built this website
    because the internet -- in its current form -- has given me countless graces
    (and temptations, but that's to be expected). So please forgive my
    amateurish writing, for it is my wish that you will take away something of
    use, and pay it forwards.")))
(ws:define-easy-handler (work :uri "/work") ()
  (app-page (:title "HanshenWang.com")
    (:h1 :class "title" "Work")
    (:h1 "Professional Portfolio ")
    (:a :target "_blank" :href "https://drive.google.com/file/d/1-D_CkQhgazvBCNr5v3CyxEbHZRXFYXKy/view?usp=sharing" "Click this link to view or download my resume (link to Google Drive).")
    (:br)
    (:p "Please kindly" (:a :href "/contact" "shoot me an email") "should the link above be broken.")
    (:h1 "Side Projects")
    (:p "Here one will find my Github repos, my book/course notes (all mistakes are mine), and any other miscellaneous items that can be used at one's leisure. As with the above," (:a :href "/contact" "do contact me") "if something is missing")
    (:ul
     (:li (:a :target "_blank" :href "https://github.com/HanshenWang" "Github Repositories"))
     (:li (:a :href "/blog/archive.html" "Collected Notes")))))
(ws:define-easy-handler (contact :uri "/contact") ()
  (app-page (:title "HanshenWang.com")
    (:h1 :class "title" "Contact")
    (:h1 "Ways to get in contact")
    ;; https://stackoverflow.com/questions/483212/effective-method-to-hide-email-from-spam-bots
            (:p "Questions, comments, death threats? Don't hesitate to reach out to me via email at:" (:a :class "cryptedmail" :data-name "hanshen" :data-domain "hanshenwang" :data-tld "com" :onclick "window.location.href = 'mailto:' + this.dataset.name + '@' + this.dataset.domain + '.' + this.dataset.tld; return false;" :href "#"))
            (:p "My PGP Key Fingerprint:	06DD A936 90F7 75E3 715B 628C CA94 9A6D 46BC 2BBE")
            (:p "My PGP Public Key is available" (:a :target "_blank" :href
  "0x06DDA93690F775E3715B628CCA949A6D46BC2BBE.asc" "here") "and as a secondary
  source, at" (:a :target "_blank" :href "https://keys.openpgp.org" "https://keys.openpgp.org."))
    (:h1 :id "article-history" "Blog Article Transparency Policy")
    (:p "All edits made to an article after the initial publication date can be found" (:a :target "_blank" :href "https://github.com/HanshenWang/project-isidore/" "in the version-controlled Github repository (under the /assets/blog/ folder)."))))

(defun generate-index-css (output-location)
  "Generated index.css file for index.html use. When using cl-css, \"~~\" is
  needed to output a single \"~\", otherwise an error will be thrown"
  (css:compile-css
   output-location
   '(
     ;; CSS Background Slideshow
     (".cb-slideshow,.cb-slideshow:after"
      :position" fixed"
      :width" 100%"
      :height" 100%"
      :top" 0px"
      :left" 0px"
      :z-index" 0")
     (".cb-slideshow:after"
      :content" ''")
     (".cb-slideshow li span"
      :width" 100%"
      :height" 100%"
      :position" absolute"
      :top" 0px"
      :left" 0px"
      :color" transparent"
      :background-size" cover"
      :background-position" 50% 50%"
      :background-repeat" none"
      :opacity" 0"
      :z-index" 0"
      :-webkit-backface-visibility" hidden"
      :-webkit-animation" imageAnimation 36s linear infinite 0s"
      :-moz-animation" imageAnimation 36s linear infinite 0s"
      :-o-animation" imageAnimation 36s linear infinite 0s"
      :-ms-animation" imageAnimation 36s linear infinite 0s"
      :animation" imageAnimation 36s linear infinite 0s")
     (".cb-slideshow li:nth-child(1) span"
      :background-image "url(pic1.webp)")
     (".cb-slideshow li:nth-child(2) span"
      :background-image "url(pic2.webp)"
      :-webkit-animation-delay" 6s"
      :-moz-animation-delay" 6s"
      :-o-animation-delay" 6s"
      :-ms-animation-delay" 6s"
      :animation-delay" 6s")
     (".cb-slideshow li:nth-child(3) span"
      :background-image" url(pic3.webp)"
      :-webkit-animation-delay" 12s"
      :-moz-animation-delay" 12s"
      :-o-animation-delay" 12s"
      :-ms-animation-delay" 12s"
      :animation-delay" 12s")
     (".cb-slideshow li:nth-child(4) span"
      :background-image" url(pic4.webp)"
      :-webkit-animation-delay" 18s"
      :-moz-animation-delay" 18s"
      :-o-animation-delay" 18s"
      :-ms-animation-delay" 18s"
      :animation-delay" 18s")
     (".cb-slideshow li:nth-child(5) span"
      :background-image" url(pic5.webp)"
      :-webkit-animation-delay" 24s"
      :-moz-animation-delay" 24s"
      :-o-animation-delay" 24s"
      :-ms-animation-delay" 24s"
      :animation-delay" 24s")
     (".cb-slideshow li:nth-child(6) span"
      :background-image" url(pic6.webp)"
      :-webkit-animation-delay" 30s"
      :-moz-animation-delay" 30s"
      :-o-animation-delay" 30s"
      :-ms-animation-delay" 30s"
      :animation-delay" 30s")
     (".cb-slideshow li:nth-child(2) div"
      :-webkit-animation-delay" 6s"
      :-moz-animation-delay" 6s"
      :-o-animation-delay" 6s"
      :-ms-animation-delay" 6s"
      :animation-delay" 6s")
     (".cb-slideshow li:nth-child(3) div"
      :-webkit-animation-delay" 12s"
      :-moz-animation-delay" 12s"
      :-o-animation-delay" 12s"
      :-ms-animation-delay" 12s"
      :animation-delay" 12s")
     (".cb-slideshow li:nth-child(4) div"
      :-webkit-animation-delay" 18s"
      :-moz-animation-delay" 18s"
      :-o-animation-delay" 18s"
      :-ms-animation-delay" 18s"
      :animation-delay" 18s")
     (".cb-slideshow li:nth-child(5) div"
      :-webkit-animation-delay" 24s"
      :-moz-animation-delay" 24s"
      :-o-animation-delay" 24s"
      :-ms-animation-delay" 24s"
      :animation-delay" 24s")
     (".cb-slideshow li:nth-child(6) div"
      :-webkit-animation-delay" 30s"
      :-moz-animation-delay" 30s"
      :-o-animation-delay" 30s"
      :-ms-animation-delay" 30s"
      :animation-delay" 30s")
     ;; Animation for the slideshow images
     ("@-webkit-keyframes imageAnimation"
      ("0%"
       :opacity" 0"
       :-webkit-animation-timing-function" ease-in")
      ("8%"
       :opacity" 1"
       :-webkit-animation-timing-function" ease-out")
      ("17%"
       :opacity" 1")
      ("25%"
       :opacity" 0")
      ("100%"
       :opacity" 0"))

     ("@-moz-keyframes imageAnimation"
      ("0%"
       :opacity" 0"
       :-moz-animation-timing-function" ease-in")
      ("8%"
       :opacity" 1"
       :-moz-animation-timing-function" ease-out")
      ("17%"
       :opacity" 1")
      ("25%"
       :opacity" 0")
      ("100%"
       :opacity" 0"))
     ("@-o-keyframes imageAnimation"
      ("0%"
       :opacity" 0"
       :-o-animation-timing-function" ease-in")
      ("8%"
       :opacity" 1"
       :-o-animation-timing-function" ease-out")
      ("17%"
       :opacity" 1")
      ("25%"
       :opacity" 0")
      ("100%"
       :opacity" 0"))
     ("@-ms-keyframes imageAnimation"
      ("0%"
       :opacity" 0"
       :-ms-animation-timing-function" ease-in")
      ("8%"
       :opacity" 1"
       :-ms-animation-timing-function" ease-out")
      ("17%"
       :opacity" 1")
      ("25%"
       :opacity" 0")
      ("100%"
       :opacity" 0"))
     ("@keyframes imageAnimation"
      ("0%"
       :opacity" 0"
       :animation-timing-function" ease-in")
      ("8%"
       :opacity" 1"
       :animation-timing-function" ease-out")
      ("17%"
       :opacity" 1")
      ("25%"
       :opacity" 0")
      ("100%"
       :opacity" 0")
      )
     ;; Show at least something when animations not supported
     (".no-cssanimations .cb-slideshow li span"
      :opacity" 1"
      )
     ;; General Demo Style
     ("html,body,div,dl,dt,dd,ul,ol,li,h1,h2,pre,form,fieldset,input,textarea,p,blockquote,th,td" :margin 0 :padding 0)
     ("body"
      :background" #000"
      :font-weight" 400"
      :font-size" 15px"
      :color" #fff"
      :text-shadow" 1px 1px 1px rgba(0,0,0,0.2)"
      :overflow-y" scroll"
      :overflow-x" hidden")
     ("h1,h2"
      :font-family" 'Cinzel', 'Times New Roman', serif"
      :font-size"100%"
      :font-weight"normal")
     ("a"
      :color" #00d9ff"
      :text-decoration" none")
     ;; Black intro panel
     (".container"
      :position" relative"
      :text-align" center")
     (".container > header"
      :position" absolute"
      :left" 0"
      :right" 0"
      :top" 5rem"
      :margin" auto"
      :max-width" 30rem"
      :padding" 3rem 3rem"
      :background-color" rgba(0, 0, 0, 0.3)"
      :border-radius" 80px"
      :box-shadow" 0 0 30px 20px rgba(0,0,0,0.3)"
      :text-shadow" 1px 1px 1px rgba(0,0,0,0.2)"
      :display" block"
      :text-align" center")
     (".portfolio-container"
      :display"flex"
      :flex-direction"row")
     (".portfolio-section"
      :flex" 50%")
     (".container > header img"
      :width" 150px"
      :height" 150px"
      :margin" 0rem auto"
      :border-radius" 9999px")
     (".container > header h1"
      :font-family" 'Cinzel', 'Times New Roman', serif"
      :font-size" 35px"
      :line-height" 35px"
      :position" relative"
      :font-weight" 400"
      :color" #fff"
      :text-shadow" 1px 1px 1px rgba(0,0,0,0.3)"
      :padding" 0px 0px 5px 0px")
     (".container > header h2"
      :font-family" 'Cinzel', 'Times New Roman', serif"
      :font-size" 1.5em"
      :font-style" bold"
      :color" #f8f8f8"
      :text-shadow" 1px 1px 1px rgba(0,0,0,0.6)")
     (".container > header p"
      :font-family" 'EB Garamond', 'Times New Roman', serif")
     ;; Button Style
     ("p.portfolio-button"
      :display" block"
      :padding" 15px 0px")
     ( "p.portfolio-button a"
      :display" inline-block"
      :border" 1px solid #425de6"
      :padding" 4px 10px 3px"
      :font-family" 'Cinzel', 'Times New Roman', serif"
      :font-size" 13px"
      :line-height" 18px"
      :margin" 2px 3px"
      :font-weight" 500"
      :-webkit-box-shadow" 0px 1px 1px rgba(0,0,0,0.1)"
      :-moz-box-shadow"0px 1px 1px rgba(0,0,0,0.1)"
      :box-shadow" 0px 1px 1px rgba(0,0,0,0.1)"
      :color"#fff"
      :-webkit-border-radius" 5px"
      :-moz-border-radius" 5px"
      :border-radius" 5px"
      :background" #6295e3"
      :background" -moz-linear-gradient(top, #6295e3 0%, #6286e3 44%, #425de6 100%)"
      :background" -webkit-gradient(linear, left top, left bottom, color-stop(0%,#6295e3), color-stop(44%,#6286e3), color-stop(100%,#425de6))"
      :background" -webkit-linear-gradient(top, #6295e3 0%,#6286e3 44%,#425de6 100%)"
      :background" -o-linear-gradient(top, #6295e3 0%,#6286e3 44%,#425de6 100%)"
      :background" -ms-linear-gradient(top, #6295e3 0%,#6286e3 44%,#425de6 100%)"
      :background" linear-gradient(top, #6295e3 0%,#6286e3 44%,#425de6 100%)")
     ("p.portfolio-button a:hover"
      :background" #425de6")
     ("p.portfolio-button a:active"
      :background" #425de6"
      :background" -moz-linear-gradient(top, #425de6 0%, #6286e3 56%, #6295e3 100%)"
      :background" -webkit-gradient(linear, left top, left bottom, color-stop(0%,#425de6), color-stop(56%,#6286e3), color-stop(100%,#6295e3))"
      :background" -webkit-linear-gradient(top, #425de6 0%,#6286e3 56%,#6295e3 100%)"
      :background" -o-linear-gradient(top, #425de6 0%,#6286e3 56%,#6295e3 100%)"
      :background" -ms-linear-gradient(top, #425de6 0%,#6286e3 56%,#6295e3 100%)"
      :background" linear-gradient(top, #425de6 0%,#6286e3 56%,#6295e3 100%)"
      :-webkit-box-shadow" 0px 1px 1px rgba(255,255,255,0.9)"
      :-moz-box-shadow"0px 1px 1px rgba(255,255,255,0.9)"
      :box-shadow" 0px 1px 1px rgba(255,255,255,0.9)")
     ;; Media Queries
     ("@media screen and (max-width: 767px)"
      (".container > header"
       :text-align" center")
      ("p.portfolio-button"
       :position" relative"
       :top" auto"
       :left" auto")
      (".portfolio-container"
       :flex-direction" column")))))
(defun generate-global-css (output-location)
  "Generate global.css file for site-wide use"
  (css:compile-css
   output-location
   '(
     ;; Navbar CSS
     (".header-fixed "
      :position" relative"
      :left" 0"
      :top" 0"
      :z-index" 1"
      :width" 100%"
      :margin" 0"
      :background-color" #FDF6E3"
      )

     (".navbar "
      :display" flex"
      :justify-content" space-between"
      :align-items" center"
      :color" #fff"
      :line-height" 60px"
      )

     (".navbar .logo "
      :flex" 3"
      )

     (".navbar .logo a "
      :display" block"
      :margin-left" 10%"
      :font-size" 22px"
      :font-family" 'Cinzel'"
      :font-weight" 500"
      :color" #000"
      :text-decoration" none"
      )
     (".navbar .logo a:hover "
      :color" #777777"
      )

     (".navbar nav "
      :flex" 8"
      )

     (".navbar label "
      :user-select" none"
      :cursor" pointer"
      :padding" 28px 20px"
      :position" relative"
      :z-index" 3"
      )

     (".navbar label i "
      :height" 2px"
      :position" relative"
      :transition" background .2s ease-out"
      :width" 18px"
      :font-style" normal"
      :font-weight" normal"
      )
     (".navbar label i:before, .navbar label i:after "
      :content" ''"
      :height" 100%"
      :position" absolute"
      :transition" all .2s ease-out"
      :width" 100%"
      )
     (".navbar label i, .navbar label i:before, .navbar label i:after "
      :display" block"
      :background" black"
      )
     (".navbar label i:before "
      :top" 5px"
      )
     (".navbar label i:after "
      :top" -5px"
      )

     (".navbar #navbar-toggle "
      :display" none"
      )

     (".header #navbar-toggle:checked ~~ .menu "
      :visibility" visible"
      :opacity" 0.99"
      )
     (".header #navbar-toggle:checked ~~ label "
      :background" gray"
      :border-radius" 50%"
      )
     (".header #navbar-toggle:checked ~~ label i "
      :background" transparent"
      )
     (".header #navbar-toggle:checked ~~ label i:before "
      :transform" rotate(-45deg)"
      )
     (".header #navbar-toggle:checked ~~ label i:after "
      :transform" rotate(45deg)"
      )
     (".header #navbar-toggle:checked ~~ label:not(.steps) i:before, .header #navbar-toggle:checked ~~ label:not(.steps) i:after "
      :top" 0"
      )

     ("@media (max-width: 768px) "
      (".navbar nav "
       :visibility" hidden"
       :opacity" 0"
       :z-index" 2"
       :position" fixed"
       :top" 0px"
       :left" 0px"
       :width" 100%"
       :height" 100%"
       :transition" all 0.3s ease-out"
       :display" table"
       :background" #f5deb3"
       )
      (".navbar nav ul "
       :margin" 0"
       :padding" 20px 0"
       :display" table-cell"
       :vertical-align" middle"
       )
      (".navbar nav li "
       :display" block"
       :text-align" center"
       :padding" 20px 0"
       :text-align" center"
       :font-size" 50px"
       :min-height" 50px"
       :font-weight" bold"
       :cursor" pointer"
       :transition" all 0.3s ease-out"
       )
      (".navbar nav li:hover "
       :background" #FDF6E3"
       )
      (".navbar nav li:hover a "
       :color" brown"
       :transition" all 0.3s ease-out"
       )
      (".navbar nav li a "
       :font-family" 'Cinzel', 'Times New Roman', serif"
       :color" #212121"
       )
      )

     ("@media (min-width: 768px) "
      (".navbar nav ul "
       :margin" 0"
       :padding" 0"
       :display" flex"
       :justify-content" space-around"
       :text-align" center"
       :list-style" none"
       )
      (".navbar nav li "
       :flex" 1"
       )
      (".navbar nav li a "
       :display" block"
       :padding" 0 8px"
       :font-size" 16px"
       :font-weight" 400"
       :font-family" 'Cinzel'"
       :line-height" 60px"
       :color" #000"
       :text-decoration" none"
       )
      (".navbar nav li.active "
       :background" #555"
       )
      (".navbar nav li:hover "
       :background" #EEE8D5"
       )
      (".navbar label "
       :display" none"
       )
      )
     ;; End of Navbar CSS
     ("html"
      :background-color" #FDF6E3"
      )
     ("body "
      :width" 95%"
      :margin" 2% auto"
      :font-size" 14px"
      :line-height" 1.4em"
      :font-family" 'EB Garamond', 'Times New Roman', serif"
      :font-weight" 400"
      :color" #333"
      )

     ("@media screen and (min-width: 600px) "
      ("body "
       :font-size" 18px"
       )
      )

     ("@media screen and (min-width: 910px) "
      ("body "
       :width" 900px"
       )
      )

     ("#content "
      :width" 100%"
      )

     ("::selection "
      :background" #D6EDFF"
      )

     ("p "
      :line-height "2em"
      :margin" 1em auto"
      )

     ("ol,ul "
      :margin" 0 auto"
      )

     ("dl "
      :margin" 0 auto"
      )

     (".title "
      :text-align" center"
      :margin" 0.8em auto"
      :color" black"
      :font-family"'Cinzel'"
      :font-weight" 500"
      )

     (".subtitle "
      :text-align" center"
      :font-size" 1.5em"
      :line-height" 1.4"
      :font-weight" bold"
      :margin" 1em auto"
      )

     (".updated "
      :text-align" center"
      )

     (".abstract "
      :text-align" center"
      :margin" auto"
      :width" 80%"
      :font-style" italic"
      )

     (".abstract p:last-of-type:before "
      :content" \"    \""
      :white-space" pre"
      )

     (".status "
      :font-size" 90%"
      :margin" 2em auto"
      )

     ("[class^=\"section-number-\"] "
      :margin-right" .5em"
      )

     ("[id^=\"orgheadline\"] "
      :clear" both"
      )

     ("#footnotes "
      :font-size" 90%"
      )

     (".footpara "
      :display" inline"
      :margin" .2em auto"
      )

     (".footdef "
      :margin-bottom" 1em"
      )

     (".footdef sup "
      :padding-right" .5em"
      )

     ("a "
      :color" #609cdb"
      :text-decoration" none"
      )

     ("a:hover "
      :color" #003355"
      :border-bottom" 1px dotted"
      )

     ("figure "
      :padding" 0px"
      :margin" 1em auto"
      :text-align" center"
      )

     ("img "
      :max-width" 100%"
      :vertical-align" middle"
      )

     (".MathJax_Display "
      :margin" 0!important"
      :width" 90%!important"
      )

     ("h1,h2,h3,h4,h5,h6 "
      :color" #A5573E"
      :line-height" 1em"
      :font-weight" 400"
      :font-family" 'Montserrat', 'Arial', non-serif"
      )

     ("h2 a "
      :color" #A5573E"
      )

     ("h3 a "
      :color" #A5573E"
      )

     ("h4 a "
      :color" #A5573E"
      )

     ("h5 a "
      :color" #A5573E"
      )

     ("h6 a "
      :color" #A5573E"
      )

     ("h1,h2,h3 "
      :line-height" 1.4em"
      )

     ("h4,h5,h6 "
      :font-size" 1em"
      )

     ("@media screen and (min-width: 600px) "
      ("h1 "
       :font-size" 2em"
       )
      ("h2 "
       :font-size" 1.5em"
       )
      ("h3 "
       :font-size" 1.3em"
       )
      ("h1,h2,h3 "
       :line-height" 1.4em"
       )
      ("h4,h5,h6 "
       :font-size" 1.1em"
       )
      )

     ("dt "
      :font-weight" bold"
      )

     ("table "
      :margin" 1em auto"
      :border-top" 2px solid"
      :border-bottom" 2px solid"
      :border-collapse" collapse"
      )

     ("thead "
      :border-bottom" 2px solid"
      )

     ("table td + td, table th + th "
      :border-left" 1px solid gray"
      )

     ("table tr "
      :border-top" 1px solid lightgray"
      )

     ("td,th "
      :padding" 0.3em 0.6em"
      :vertical-align" middle"
      )

     ("caption.t-above "
      :caption-side" top"
      )

     ("caption.t-bottom "
      :caption-side" bottom"
      )

     ("caption "
      :margin-bottom" 0.3em"
      )

     ("figcaption "
      :margin-top" 0.3em"
      )

     ("th.org-right "
      :text-align" center"
      )

     ("th.org-left "
      :text-align" center"
      )

     ("th.org-center "
      :text-align" center"
      )

     ("td.org-right "
      :text-align" right"
      )

     ("td.org-left "
      :text-align" left"
      )

     ("td.org-center "
      :text-align" center"
      )

     ("blockquote "
      :margin" 1em 2em"
      :padding-left" 1em"
      :border-left" 3px solid #ccc"
      )

     ("kbd "
      :background-color" #f7f7f7"
      :font-size" 80%"
      :margin" 0 .1em"
      :padding" .1em .6em"
      )

     (".todo "
      :background-color" red"
      :color" white"
      :padding" .1em 0.3em"
      :border-radius" 3px"
      :background-clip" padding-box"
      :font-size" 80%"
      :font-family" 'Courier New', monospace"
      :line-height" 1"
      )

     (".done "
      :background-color" green"
      :color" white"
      :padding" .1em 0.3em"
      :border-radius" 3px"
      :background-clip" padding-box"
      :font-size" 80%"
      :font-family" 'Courier New', monospace"
      :line-height" 1"
      )

     (".priority "
      :color" orange"
      :font-family" 'Courier New', monospace"
      )

     ;; /* Because tag span is set to float.  This is more like a hacking.  Maybe we need a cleaner solution. */
     ("#table-of-contents li "
      :clear" both"
      )

     (".tag "
      :font-family" 'Courier New', monospace"
      :font-size" 0.7em"
      :font-weight" normal"
      )

     (".tag span "
      :padding" 0.3em 0.3em"
      :float" right"
      :margin-right" .5em"
      :border" 1px solid #bbb"
      :border-radius" 3px"
      :background-clip" padding-box"
      :color" #333"
      :background-color" #eee"
      :line-height" 1"
      )

     (".timestamp "
      :color" #333"
      :font-size" 90%"
      )

     (".timestamp-kwd "
      :color" #5f9ea0"
      )

     (".org-right "
      :margin-left" auto"
      :margin-right" 0px"
      :text-align" right"
      )

     (".org-left "
      :margin-left" 0px"
      :margin-right" auto"
      :text-align" left"
      )

     (".org-center "
      :margin-left" auto"
      :margin-right" auto"
      :text-align" center"
      )

     (".underline "
      :text-decoration" underline"
      )

     ("#postamble p, #preamble p "
      :font-size" 90%"
      :margin" .2em"
      )

     (".copyright-container "
      :display" flex"
      :justify-content" space-between"
      :align-items" center"
      :flex-wrap" wrap"
      :padding" 0.4ex 1em"
      )

     (".generated "
      :font-family" 'Courier New', monospace"
      :text-align" center"
      )

     ("p.verse "
      :margin-left" 3%"
      )

     (":not(pre) > code "
      :padding" 2px 5px"
      :margin" auto 1px"
      :border" 1px solid #DDD"
      :border-radius" 3px"
      :background-clip" padding-box"
      :background-color "#F5F5F5"
      :color" #333"
      :font-size" 80%"
      )

     (".org-src-container "
      :border" 1px solid #ccc"
      :box-shadow" 3px 3px 3px #eee"
      :background-color "#F5E8C4"
      :font-family" 'Courier New', monospace"
      :font-size" 80%"
      :margin" 1em auto"
      :padding" 0.1em 0.5em"
      :position" relative"
      )

     (".org-src-container>pre "
      :overflow" auto"
      )

     (".org-src-container>pre:before "
      :display" block"
      :position" absolute"
      :background-color" #b3b3b3"
      :top" 0"
      :right" 0"
      :padding" 0 0.5em"
      :border-bottom-left-radius" 8px"
      :border" 0"
      :color" white"
      :font-size" 80%"
      )

     ;; /* from http://demo.thi.ng/org-spec/ */

     (".org-src-container>pre.src-sh:before "
      :content" 'sh'"
      )
     (".org-src-container>pre.src-bash:before "
      :content" 'bash'"
      )
     (".org-src-container>pre.src-emacs-lisp:before "
      :content" 'Emacs Lisp'"
      )
     (".org-src-container>pre.src-R:before "
      :content" 'R'"
      )
     (".org-src-container>pre.src-org:before "
      :content" 'Org'"
      )
     (".org-src-container>pre.src-cpp:before "
      :content" 'C++'"
      )
     (".org-src-container>pre.src-c:before "
      :content" 'C'"
      )
     (".org-src-container>pre.src-html:before "
      :content" 'HTML'"
      )
     (".org-src-container>pre.src-js:before "
      :content" 'Javascript'"
      )
     (".org-src-container>pre.src-javascript:before "
      :content" 'Javascript'"
      )

     ;; // More languages from http://orgmode.org/worg/org-contrib/babel/languages.html

     (".org-src-container>pre.src-abc:before "
      :content" 'ABC'"
      )
     (".org-src-container>pre.src-asymptote:before "
      :content" 'Asymptote'"
      )
     (".org-src-container>pre.src-awk:before "
      :content" 'Awk'"
      )
     (".org-src-container>pre.src-C:before "
      :content" 'C'"
      )
     (".org-src-container>pre.src-calc:before "
      :content" 'Calc'"
      )
     (".org-src-container>pre.src-clojure:before "
      :content" 'Clojure'"
      )
     (".org-src-container>pre.src-comint:before "
      :content" 'comint'"
      )
     (".org-src-container>pre.src-css:before "
      :content" 'CSS'"
      )
     (".org-src-container>pre.src-D:before "
      :content" 'D'"
      )
     (".org-src-container>pre.src-ditaa:before "
      :content" 'Ditaa'"
      )
     (".org-src-container>pre.src-dot:before "
      :content" 'Dot'"
      )
     (".org-src-container>pre.src-ebnf:before "
      :content" 'ebnf'"
      )
     (".org-src-container>pre.src-forth:before "
      :content" 'Forth'"
      )
     (".org-src-container>pre.src-F90:before "
      :content" 'Fortran'"
      )
     (".org-src-container>pre.src-gnuplot:before "
      :content" 'Gnuplot'"
      )
     (".org-src-container>pre.src-haskell:before "
      :content" 'Haskell'"
      )
     (".org-src-container>pre.src-io:before "
      :content" 'Io'"
      )
     (".org-src-container>pre.src-java:before "
      :content" 'Java'"
      )
     (".org-src-container>pre.src-latex:before "
      :content" 'LaTeX'"
      )
     (".org-src-container>pre.src-ledger:before "
      :content" 'Ledger'"
      )
     (".org-src-container>pre.src-ly:before "
      :content" 'Lilypond'"
      )
     (".org-src-container>pre.src-lisp:before "
      :content" 'Lisp'"
      )
     (".org-src-container>pre.src-makefile:before "
      :content" 'Make'"
      )
     (".org-src-container>pre.src-matlab:before "
      :content" 'Matlab'"
      )
     (".org-src-container>pre.src-max:before "
      :content" 'Maxima'"
      )
     (".org-src-container>pre.src-mscgen:before "
      :content" 'Mscgen'"
      )
     (".org-src-container>pre.src-Caml:before "
      :content" 'Objective'"
      )
     (".org-src-container>pre.src-octave:before "
      :content" 'Octave'"
      )
     (".org-src-container>pre.src-org:before "
      :content" 'Org'"
      )
     (".org-src-container>pre.src-perl:before "
      :content" 'Perl'"
      )
     (".org-src-container>pre.src-picolisp:before "
      :content" 'Picolisp'"
      )
     (".org-src-container>pre.src-plantuml:before "
      :content" 'PlantUML'"
      )
     (".org-src-container>pre.src-python:before "
      :content" 'Python'"
      )
     (".org-src-container>pre.src-ruby:before "
      :content" 'Ruby'"
      )
     (".org-src-container>pre.src-sass:before "
      :content" 'Sass'"
      )
     (".org-src-container>pre.src-scala:before "
      :content" 'Scala'"
      )
     (".org-src-container>pre.src-scheme:before "
      :content" 'Scheme'"
      )
     (".org-src-container>pre.src-screen:before "
      :content" 'Screen'"
      )
     (".org-src-container>pre.src-sed:before "
      :content" 'Sed'"
      )
     (".org-src-container>pre.src-shell:before "
      :content" 'shell'"
      )
     (".org-src-container>pre.src-shen:before "
      :content" 'Shen'"
      )
     (".org-src-container>pre.src-sql:before "
      :content" 'SQL'"
      )
     (".org-src-container>pre.src-sqlite:before "
      :content" 'SQLite'"
      )
     (".org-src-container>pre.src-stan:before "
      :content" 'Stan'"
      )
     (".org-src-container>pre.src-vala:before "
      :content" 'Vala'"
      )
     (".org-src-container>pre.src-axiom:before "
      :content" 'Axiom'"
      )
     (".org-src-container>pre.src-browser:before "
      :content" 'HTML'"
      )
     (".org-src-container>pre.src-cypher:before "
      :content" 'Neo4j'"
      )
     (".org-src-container>pre.src-elixir:before "
      :content" 'Elixir'"
      )
     (".org-src-container>pre.src-request:before "
      :content" 'http'"
      )
     (".org-src-container>pre.src-ipython:before "
      :content" 'iPython'"
      )
     (".org-src-container>pre.src-kotlin:before "
      :content" 'Kotlin'"
      )
     (".org-src-container>pre.src-Flavored Erlang  lfe:before "
      :content" 'Lisp'"
      )
     (".org-src-container>pre.src-mongo:before "
      :content" 'MongoDB'"
      )
     (".org-src-container>pre.src-prolog:before "
      :content" 'Prolog'"
      )
     (".org-src-container>pre.src-rec:before "
      :content" 'rec'"
      )
     (".org-src-container>pre.src-ML  sml:before "
      :content" 'Standard'"
      )
     (".org-src-container>pre.src-Translate  translate:before "
      :content" 'Google'"
      )
     (".org-src-container>pre.src-typescript:before "
      :content" 'Typescript'"
      )
     (".org-src-container>pre.src-rust:before "
      :content" 'Rust'"
      )

     (".inlinetask "
      :background" #ffffcc"
      :border" 2px solid gray"
      :margin" 10px"
      :padding" 10px"
      )

     ("#org-div-home-and-up "
      :font-size" 70%"
      :text-align" right"
      :white-space" nowrap"
      )

     (".linenr "
      :font-size" 90%"
      )

     (".code-highlighted "
      :background-color" #ffff00"
      )

     ("#bibliography "
      :font-size" 90%"
      )

     ("#bibliography table "
      :width" 100%"
      )

     (".creator "
      :display" block"
      )

     ("@media screen and (min-width: 600px) "
      (".creator "
       :display" inline"
       :float" right"
       )
      )
     (".cryptedmail:after "
      :font-family" 'EB Garamond', 'Times New Roman', serif"
      :font-size"1.2 rem"
      :content" attr(data-name) \"@\" attr(data-domain) \".\" attr(data-tld)"
      ))))
(defun generate-index-js (&key (input #P"index.lisp") (output #P"assets/index.js"))
  "Generate script.js file for index.html use. For a tutorial see: https://app.leby.org/post/fun-with-parenscript/"
  (ensure-directories-exist output)
  (with-open-file (stream output :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format stream (js:ps-compile-file input))))


;;; for development
(defvar *app-dev* nil)

(defun start-dev-server (&key (port 8080) (host "localhost"))
  (when (ws:started-p *app-dev*)
    (return-from start-dev-server (format t "Server already running at http://~A:~A/~%" host port)))
  (generate-index-css "assets/index.css") ; path name is relative
  (generate-global-css "assets/global.css")
  (generate-index-js :input "index.lisp" :output "assets/index.js")
  (setf ws:*dispatch-table*
        `(ws:dispatch-easy-handlers
          ,(ws:create-folder-dispatcher-and-handler
            "/" "assets/")))
  (setf *app-dev*
        (ws:start (make-instance 'ws:easy-acceptor
                                 :port port
                                 :address host)))
  (format t "Server successfully started at http://~A:~A/~%" host port))

(defun stop-dev-server ()
  "Stop the web server started by start-dev-server, if it exists"
  (if (ws:started-p *app-dev*)
      (progn
        (ws:stop *app-dev*)
        (format t "Server successfully stopped"))
      (format t "No server running. Start server with start-dev-server")))
