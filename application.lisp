;;;; ================
;;;; application.lisp
;;;; ================
;;;; initialize-application is the function launched by the heroku buildpack. This has been moved to heroku.lisp as the function must be under the "cl-user" namespace and not project-isidore.
(defpackage #:project-isidore
  ;; :use means there is no need to prefix hunchentoot:define-easy-handler and instead use just define-easy-handler
  ;; As per https://lispcookbook.github.io/cl-cookbook/packages.html#about-use-ing-packages-being-a-bad-practice,
  ;; use nicknames instead
  (:use #:cl)
  (:local-nicknames (#:ws #:hunchentoot) ; web server
                    (#:ht #:cl-who) ; hypertext markup generation
                    (#:css #:cl-css))
  (:export #:start-dev-server
           #:stop-dev-server))

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
             (:link :type "text/css" :href "global.css" :rel "stylesheet")
             (:link :rel "preconnect" :href "https://fonts.gstatic.com")
             (:link :href "https://fonts.googleapis.com/css2?family=Cinzel:wght@400;500;600&family=EB+Garamond:ital,wght@0,400;0,500;0,600;1,400;1,500;1,600&family=Lato:ital,wght@0,100;0,300;0,400;0,700;1,100;1,300;1,400;1,700&family=Source+Code+Pro:ital,wght@0,300;0,400;0,500;0,600;1,300;1,400;1,500;1,600&display=swap" :rel "stylesheet"))
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
            (:link :type "text/css" :href "index.css" :rel "stylesheet")
            (:link :rel "preconnect" :href "https://fonts.gstatic.com")
            (:link :href "https://fonts.googleapis.com/css2?family=Cinzel:wght@400;500;600&family=EB+Garamond:ital,wght@0,400;0,500;0,600;1,400;1,500;1,600&family=Lato:ital,wght@0,100;0,300;0,400;0,700;1,100;1,300;1,400;1,700&family=Source+Code+Pro:ital,wght@0,300;0,400;0,500;0,600;1,300;1,400;1,500;1,600&display=swap" :rel "stylesheet")
            (:script :src "script.js"))
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
                   (:p :class "codrops-demos"
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
    (:h1 :id "article-history" "Blog Article Transparency Policy")
    (:p "All edits made to an article after the initial publication date can be found" (:a :target "_blank" :href "https://github.com/HanshenWang/project-isidore/" "in the version-controlled Github repository (under the /static/blog/ folder)."))))

;;; for development
(defvar *app-dev* nil)

(defun stop-dev-server ()
  (if (probe-file "/home/hanshen/project-isidore/static/index.css")
      ;; Deletes the index.css file generated with cl-css
      (delete-file "/home/hanshen/project-isidore/static/index.css"))
  (if (ws:started-p *app-dev*)
        (ws:stop *app-dev*)))

(defun start-dev-server ()
  (stop-dev-server)
  ;; Generated index.css file for index.html use
  (css:compile-css
   "/home/hanshen/project-isidore/static/index.css"
   '(
     ;; CSS Reset
     ("body,div,dl,dt,dd,ul,ol,li,h1,h2,h3,h4,h5,h6,pre,form,fieldset,input,textarea,p,blockquote,th,td" :margin 0 :padding 0)
     ("html,body"
      :margin"0"
      :padding"0"
      )
     ("table"
      :border-collapse"collapse"
      :border-spacing"0"
      )
     ("fieldset,img"
      :border"0"
      )
     ("input"
      :border"1px solid #b0b0b0"
      :padding"3px 5px 4px"
      :color"#979797"
      :width"190px"
      )
     ("address,caption,cite,code,dfn,th,var"
      :font-style"normal"
      :font-weight"normal"
      )
     ("ol,ul"
      :list-style"none"
      )
     ("caption,th"
      :text-align"left"
      )
     ("h1,h2,h3,h4,h5,h6"
      :font-family" 'Lato', sans-serif"
      :font-size"100%"
      :font-weight"normal"
      )
     ("q:before,q:after"
      :content"''"
      )
     ("abbr,acronym"
      :border"0"
      )
     ;; General Demo Style

     ("body"
      :background" #000"
      :font-weight" 400"
      :font-size" 15px"
      :color" #fff"
      :text-shadow" 1px 1px 1px rgba(0,0,0,0.2)"
      :overflow-y" scroll"
      :overflow-x" hidden"
      )
     (".ie7 body"
      :overflow"hidden"
      )
     ("a"
      :color" #00d9ff"
      :text-decoration" none"
      )
     (".container"
      :position" relative"
      :text-align" center"
      )
     (".clr"
      :clear" both"
      )
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
      :text-align" center"
      )
     (".portfolio-container"
      :display"flex"
      :flex-direction"row"
      )
     (".portfolio-section"
      :flex" 50%"
      )
     (".container > header img"
      :width" 150px"
      :height" 150px"
      :margin" 0rem auto"
      :border-radius" 9999px"
      )
     (".container > header h1"
      :font-family" 'Lato', sans-serif"
      :font-size" 35px"
      :line-height" 35px"
      :position" relative"
      :font-weight" 400"
      :color" #fff"
      :text-shadow" 1px 1px 1px rgba(0,0,0,0.3)"
      :padding" 0px 0px 5px 0px"
      )
     (".container > header h1 span"

      )
     (".container > header h2, p.info"
      :font-family" 'Lato', sans-serif"
      :font-size" 1.5em"
      :font-style" bold"
      :color" #f8f8f8"
      :text-shadow" 1px 1px 1px rgba(0,0,0,0.6)"
      )
     (".container > header p"
      :font-family" 'EB Garamond', serif"
      )
     ;; Header Style
     (".codrops-top"
      :font-family" Arial, sans-serif"
      :line-height" 24px"
      :font-size" 11px"
      :width" 100%"
      :background" #000"
      :opacity" 0.9"
      :text-transform" uppercase"
      :z-index" 9999"
      :position" relative"
      :-moz-box-shadow" 1px 0px 2px #000"
      :-webkit-box-shadow" 1px 0px 2px #000"
      :box-shadow" 1px 0px 2px #000"
      )
     (".codrops-top a"
      :padding" 0px 10px"
      :letter-spacing" 1px"
      :color" #ddd"
      :display" block"
      :float" left"
      )
     (".codrops-top a:hover"
      :color" #fff"
      )
     (".codrops-top span.right"
      :float" right"
      )
     (".codrops-top span.right a"
      :float" none"
      :display" inline"
      )

     ("p.codrops-demos"
      :display" block"
      :padding" 15px 0px"
      )
     ( "p.codrops-demos a,p.codrops-demos a.current-demo,p.codrops-demos a.current-demo:hover"
      :display" inline-block"
      :border" 1px solid #425de6"
      :padding" 4px 10px 3px"
      :font-family" 'Cinzel', serif"
      :font-size" 13px"
      :line-height" 18px"
      :margin" 2px 3px"
      :font-weight" 800"
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
      :background" linear-gradient(top, #6295e3 0%,#6286e3 44%,#425de6 100%)"
      )
     ("p.codrops-demos a:hover"
      :background" #425de6"
      )
     ("p.codrops-demos a:active"
      :background" #425de6"
      :background" -moz-linear-gradient(top, #425de6 0%, #6286e3 56%, #6295e3 100%)"
      :background" -webkit-gradient(linear, left top, left bottom, color-stop(0%,#425de6), color-stop(56%,#6286e3), color-stop(100%,#6295e3))"
      :background" -webkit-linear-gradient(top, #425de6 0%,#6286e3 56%,#6295e3 100%)"
      :background" -o-linear-gradient(top, #425de6 0%,#6286e3 56%,#6295e3 100%)"
      :background" -ms-linear-gradient(top, #425de6 0%,#6286e3 56%,#6295e3 100%)"
      :background" linear-gradient(top, #425de6 0%,#6286e3 56%,#6295e3 100%)"
      :-webkit-box-shadow" 0px 1px 1px rgba(255,255,255,0.9)"
      :-moz-box-shadow"0px 1px 1px rgba(255,255,255,0.9)"
      :box-shadow" 0px 1px 1px rgba(255,255,255,0.9)"
      )
     ("p.codrops-demos a.current-demo,p.codrops-demos a.current-demo:hover"
      :color" #A5727D"
      :background" #425de6"
      )
     ;; Media Queries
     ("@media screen and (max-width: 767px)"
      (".container > header"
       :text-align" center"
       )
      ("p.codrops-demos"
       :position" relative"
       :top" auto"
       :left" auto"
       )
      (".portfolio-container"
       :flex-direction" column"
       )
      )

     ("span"
      :border-right" .05em solid"
      :animation" caret 1s steps(1) infinite"
      )

     ("@keyframes caret"
      ("50%"
       :border-color" transparent"
       )
      )
     (".cb-slideshow,.cb-slideshow:after"
      :position" fixed"
      :width" 100%"
      :height" 100%"
      :top" 0px"
      :left" 0px"
      :z-index" 0"
      )
     (".cb-slideshow:after"
      :content" ''"
      )
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
      :animation" imageAnimation 36s linear infinite 0s"
      )
     (".cb-slideshow li div"
      :z-index" 1000"
      :position" absolute"
      :bottom" 30px"
      :left" 0px"
      :width" 100%"
      :text-align" center"
      :opacity" 0"
      :color" #fff"
      :-webkit-animation" titleAnimation 36s linear infinite 0s"
      :-moz-animation" titleAnimation 36s linear infinite 0s"
      :-o-animation" titleAnimation 36s linear infinite 0s"
      :-ms-animation" titleAnimation 36s linear infinite 0s"
      :animation" titleAnimation 36s linear infinite 0s"
      )
     (".cb-slideshow li div h3"
      :font-family" 'BebasNeueRegular', 'Arial Narrow', Arial, sans-serif"
      :font-size" 240px"
      :padding" 0"
      :line-height" 200px"
      )
     (".cb-slideshow li:nth-child(1) span"
      :background-image "url(pic1.webp)"
      )
     (".cb-slideshow li:nth-child(2) span"
      :background-image "url(pic2.webp)"
      :-webkit-animation-delay" 6s"
      :-moz-animation-delay" 6s"
      :-o-animation-delay" 6s"
      :-ms-animation-delay" 6s"
      :animation-delay" 6s"
      )
     (".cb-slideshow li:nth-child(3) span"
      :background-image" url(pic3.webp)"
      :-webkit-animation-delay" 12s"
      :-moz-animation-delay" 12s"
      :-o-animation-delay" 12s"
      :-ms-animation-delay" 12s"
      :animation-delay" 12s"
      )
     (".cb-slideshow li:nth-child(4) span"
      :background-image" url(pic4.webp)"
      :-webkit-animation-delay" 18s"
      :-moz-animation-delay" 18s"
      :-o-animation-delay" 18s"
      :-ms-animation-delay" 18s"
      :animation-delay" 18s"
      )
     (".cb-slideshow li:nth-child(5) span"
      :background-image" url(pic5.webp)"
      :-webkit-animation-delay" 24s"
      :-moz-animation-delay" 24s"
      :-o-animation-delay" 24s"
      :-ms-animation-delay" 24s"
      :animation-delay" 24s"
      )
     (".cb-slideshow li:nth-child(6) span"
      :background-image" url(pic6.webp)"
      :-webkit-animation-delay" 30s"
      :-moz-animation-delay" 30s"
      :-o-animation-delay" 30s"
      :-ms-animation-delay" 30s"
      :animation-delay" 30s"
      )
     (".cb-slideshow li:nth-child(2) div"
      :-webkit-animation-delay" 6s"
      :-moz-animation-delay" 6s"
      :-o-animation-delay" 6s"
      :-ms-animation-delay" 6s"
      :animation-delay" 6s"
      )
     (".cb-slideshow li:nth-child(3) div"
      :-webkit-animation-delay" 12s"
      :-moz-animation-delay" 12s"
      :-o-animation-delay" 12s"
      :-ms-animation-delay" 12s"
      :animation-delay" 12s"
      )
     (".cb-slideshow li:nth-child(4) div"
      :-webkit-animation-delay" 18s"
      :-moz-animation-delay" 18s"
      :-o-animation-delay" 18s"
      :-ms-animation-delay" 18s"
      :animation-delay" 18s"
      )
     (".cb-slideshow li:nth-child(5) div"
      :-webkit-animation-delay" 24s"
      :-moz-animation-delay" 24s"
      :-o-animation-delay" 24s"
      :-ms-animation-delay" 24s"
      :animation-delay" 24s"
      )
     (".cb-slideshow li:nth-child(6) div"
      :-webkit-animation-delay" 30s"
      :-moz-animation-delay" 30s"
      :-o-animation-delay" 30s"
      :-ms-animation-delay" 30s"
      :animation-delay" 30s"
      )
     ;; Animation for the slideshow images
     ("@-webkit-keyframes imageAnimation"
      ("0%"
       :opacity" 0"
       :-webkit-animation-timing-function" ease-in"
       )
      ("8%"
       :opacity" 1"
       :-webkit-animation-timing-function" ease-out"
       )
      ("17%"
       :opacity" 1"
       )
      ("25%"
       :opacity" 0"
       )
      ("100%"
       :opacity" 0"
       )
      )

     ("@-moz-keyframes imageAnimation"
      ("0%"
       :opacity" 0"
       :-moz-animation-timing-function" ease-in"
       )
      ("8%"
       :opacity" 1"
       :-moz-animation-timing-function" ease-out")
      ("17%"
       :opacity" 1")
      ("25%"
       :opacity" 0")
      ("100%"
       :opacity" 0")
      )
     ("@-o-keyframes imageAnimation"
      ("0%"
       :opacity" 0"
       :-o-animation-timing-function" ease-in"
       )
      ("8%"
       :opacity" 1"
       :-o-animation-timing-function" ease-out")
      ("17%"
       :opacity" 1")
      ("25%"
       :opacity" 0")
      ("100%"
       :opacity" 0")
      )
     ("@-ms-keyframes imageAnimation"
      ("0%"
       :opacity" 0"
       :-ms-animation-timing-function" ease-in"
       )
      ("8%"
       :opacity" 1"
       :-ms-animation-timing-function" ease-out")
      ("17%"
       :opacity" 1")
      ("25%"
       :opacity" 0")
      ("100%"
       :opacity" 0")
      )
     ("@keyframes imageAnimation"
      ("0%"
       :opacity" 0"
       :animation-timing-function" ease-in"
       )
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
     ;; Animation for the title

     ("@-webkit-keyframes titleAnimation "
      ("0%"
       :opacity" 0"
       )
      ("8%"
       :opacity" 1"
       )
      ("17%"
       :opacity" 1"
       )
      ("19%"
       :opacity" 0"
       )
      ("100%"
       :opacity" 0"
       )
      )
     ("@-moz-keyframes titleAnimation "
      ("0%"
       :opacity" 0"
       )
      ("8%"
       :opacity" 1"
       )
      ("17%"
       :opacity" 1"
       )
      ("19%"
       :opacity" 0"
       )
      ("100%"
       :opacity" 0"
       )
      )
     ("@-o-keyframes titleAnimation "
      ("0%"
       :opacity" 0"
       )
      ("8%"
       :opacity" 1"
       )
      ("17%"
       :opacity" 1"
       )
      ("19%"
       :opacity" 0"
       )
      ("100%"
       :opacity" 0"
       )
      )
     ("@-ms-keyframes titleAnimation "
      ("0%"
       :opacity" 0"
       )
      ("8%"
       :opacity" 1"
       )
      ("17%"
       :opacity" 1"
       )
      ("19%"
       :opacity" 0"
       )
      ("100%"
       :opacity" 0"
       )
      )
     ("@keyframes titleAnimation "
      ("0%"
       :opacity" 0"
       )
      ("8%"
       :opacity" 1"
       )
      ("17%"
       :opacity" 1"
       )
      ("19%"
       :opacity" 0"
       )
      ("100%"
       :opacity" 0"
       )
      )
     ;; Show at least something when animations not supported
     (".no-cssanimations .cb-slideshow li span"
      :opacity" 1"
      )
     ("@media screen and (max-width: 1140px)"
      (".cb-slideshow li div h3"
       :font-size" 140px"
       )
      )
     ("@media screen and (max-width: 600px)"
      (".cb-slideshow li div h3"
       :font-size" 80px"
       )
      )
     ))
  (setf ws:*dispatch-table*
        `(ws:dispatch-easy-handlers
          ,(ws:create-folder-dispatcher-and-handler
            "/" "/home/hanshen/project-isidore/static/")))
  (setf *app-dev*
        (ws:start (make-instance 'ws:easy-acceptor :port 4242))))
