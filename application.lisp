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
                    (#:ht #:cl-who))) ; hypertext markup generation

;; This must match the defpackage above, remember repl upon startup defaults to cl-user package.
(in-package #:project-isidore)

;; The double colons are to access unexported functions & symbols
;; single colon if it is exported. Be careful with unexported/internal symbols
;; (setf (bts::html-mode) :html5)

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
            (:meta :charset "utf-8" :name "viewport" :content "width=device-width, initial-scale=1")
            (:title "HanshenWang.com")
            (:link :type "text/css" :href "landing.css" :rel "stylesheet")
            (:link :type "text/css" :href "index.css" :rel "stylesheet")
            (:link :rel "preconnect" :href "https://fonts.gstatic.com")
            (:link :href "https://fonts.googleapis.com/css2?family=Cinzel:wght@400;500;600&family=EB+Garamond:ital,wght@0,400;0,500;0,600;1,400;1,500;1,600&family=Lato:ital,wght@0,100;0,300;0,400;0,700;1,100;1,300;1,400;1,700&family=Source+Code+Pro:ital,wght@0,300;0,400;0,500;0,600;1,300;1,400;1,500;1,600&display=swap" :rel "stylesheet")
            (:script :src "script.js"))
           (:body
            (:ul :class "cb-slideshow" ; background CSS3 slideshow
                 (:li (:span "pic1.jpg"))
                 (:li (:span "pic2.jpg"))
                 (:li (:span "pic3.jpg"))
                 (:li (:span "pic4.jpg"))
                 (:li (:span "pic5.jpg"))
                 (:li (:span "pic6.jpg")))
            (:div :class "container"
                  (:header
                   (:div :class "portfolio-container"
                         (:div :class "portfolio-section"
                               (:img :src "profile.jpg" :alt "Author Profile Picture"))
                         (:div :class "portfolio-section"
                               (:h1 "Hey There!") (:br)
                               (:h2 "I'm Hanshen.") (:br)
                               (:h2 "Nice to meet you.")))
                   (:hr)
                   (:br)
                   (:p "Welcome to my personal website! This website was built with" (:a :href "https://github.com/hanshenwang/" :target "_blank" "<s> Next.js") "and React </s>" (:a :href "https://edicl.github.io/hunchentoot/" :target "_blank" "Hunchentoot") "and Common Lisp. My resume can be found under the work tab. I hope you find what you're looking for, and may the wind be always at your back.")
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
    (:p "Questions, comments, death threats? Don't hesitate to reach out to me via email at:" (:a :class "cryptedmail" :data-name "hanshen" :data-domain "hanshenwang" :data-tld "com" :onclick "window.location.href = 'mailto:' + this.dataset.name + '@' + this.dataset.domain + '.' + this.dataset.tld; return false;" :href "#"))))
;;; for development
(defvar *app-dev* nil)

(defun stop-dev-server ()
  (if (ws:started-p *app-dev*)
      (ws:stop *app-dev*)))

(defun start-dev-server ()
  (setf ws:*dispatch-table*
        `(ws:dispatch-easy-handlers
          ,(ws:create-folder-dispatcher-and-handler
            "/" "/home/hanshen/.roswell/local-projects/project-isidore/static/")))
  (setf *app-dev*
        (ws:start (make-instance 'ws:easy-acceptor :port 4242))))
