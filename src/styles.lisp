;;;; SPDX-FileCopyrightText: 2021 Hanshen Wang <Hanshen@HanshenWang.com>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:project-isidore/styles
  (:use #:common-lisp)
  (:import-from #:cl-css)
  ;; No package local nicknames. See commit 1962a26.
  (:export #:index-css
           #:generate-global-css)
  (:documentation
   "Project Isidore Cascading Styles Sheets.

Generate GLOBAL.CSS from sexp notation. Provide static styling.

See VIEWS.LISP for HTML generation. "))

(in-package #:project-isidore/styles)

;;; Generate global.css static asset
(defun bg-slideshow-span (image delay)
  "Boilerplate function to specify background IMAGE and duration DELAY in seconds"
  `(:background-image ,(format nil "url(pic~a.webp)" image) :animation-delay
                      ,(format nil "~a" delay)))

(defun bg-slideshow-div (delay)
  "Boilerplate function to specify duration DELAY in seconds"
  `(:animation-delay ,(format nil "~a" delay)))

(defun index-css ()
  ;; When using cl-css, \"~~\" is needed to output a single \"~\", otherwise an
  ;; error will be thrown
  (cl-css:css
   `(;; Load fonts.
     ("@font-face"
      :font-family "Cinzel"
      :font-style "normal"
      :font-weight "400"
      :font-display "swap"
      :src "local(''), url('/cinzel-v11-latin-regular.woff2') format('woff2')")
     ;; CSS Background Slideshow
     (".slideshow,.slideshow:after" :position" fixed" :width" 100%" :height"
     100%" :top" 0px" :left" 0px" :z-index" 0")

     (".slideshow:after" :content" ''")

     (".slideshow li span" :width" 100%" :height" 100%" :position" absolute"
                           :top" 0px" :left" 0px" :color" transparent" :background-size" cover"
                           :background-position" 50% 50%" :opacity" 0"
                           :z-index" 0" :backface-visibility" hidden" :animation" imageAnimation 36s
     linear infinite 0s")

     (".slideshow li:nth-child(1) span" :background-image "url(pic1.webp)")
     (".slideshow li:nth-child(2) span" ,@(bg-slideshow-span 2 "6s"))
     (".slideshow li:nth-child(3) span" ,@(bg-slideshow-span 3 "12s"))
     (".slideshow li:nth-child(4) span" ,@(bg-slideshow-span 4 "18s"))
     (".slideshow li:nth-child(5) span" ,@(bg-slideshow-span 5 "24s"))
     (".slideshow li:nth-child(6) span" ,@(bg-slideshow-span 6 "30s"))
     (".slideshow li:nth-child(2) div" ,@(bg-slideshow-div "6s"))
     (".slideshow li:nth-child(3) div" ,@(bg-slideshow-div "12s"))
     (".slideshow li:nth-child(4) div" ,@(bg-slideshow-div "18s"))
     (".slideshow li:nth-child(5) div" ,@(bg-slideshow-div "24s"))
     (".slideshow li:nth-child(6) div" ,@(bg-slideshow-div "30s"))
     (".slideshow li:nth-child(2) div" ,@(bg-slideshow-div "6s"))

     ;; Animation for the slideshow images
     ("@keyframes imageAnimation" ("0%" :opacity" 0" :animation-timing-function"
     ease-in") ("8%" :opacity" 1" :animation-timing-function" ease-out") ("17%"
                                                                          :opacity"1") ("25%" :opacity"0") ("100%" :opacity "0"))

     ;; Show at least something when animations not supported
     (".no-cssanimations .slideshow li span" :opacity" 1")

     ;; General Demo Style
     ("html,body,div,dl,dt,dd,ul,ol,li,h1,h2,pre,form,fieldset,input,textarea,p
,blockquote,th,td" :margin 0 :padding 0)

     ("body" :background" #000" :font-weight" 400" :font-size" 15px" :color"
     #fff" :text-shadow" 1px 1px 1px rgba(0,0,0,0.2)" :overflow-y" scroll"
             :overflow-x" hidden")

     ("h1,h2" :font-family" 'Cinzel', 'Times New Roman', serif" :font-size"100%"
              :font-weight"normal")

     ("a" :color" #00d9ff" :text-decoration" none")

     ;; Black intro panel

     (".container" :position" relative" :text-align" center")

     (".container > header" :position" absolute" :left" 0" :right" 0" :top"
     5rem" :margin" auto" :max-width" 30rem" :padding" 3rem 3rem"
                            :background-color" rgba(0, 0, 0, 0.3)" :border-radius" 80px" :box-shadow" 0
     0 30px 20px rgba(0,0,0,0.3)" :text-shadow" 1px 1px 1px rgba(0,0,0,0.2)"
                            :display" block" :text-align" center")

     (".portfolio-container" :display"flex" :flex-direction"row")

     (".portfolio-section" :flex" 50%")

     (".container > header img" :width" 150px" :height" 150px" :margin" 0rem
     auto" :border-radius" 9999px")

     (".container > header h1" :font-family" 'Cinzel', 'Times New Roman', serif"
                               :font-size" 35px" :line-height" 35px" :position" relative" :font-weight"
     400" :color" #fff" :text-shadow" 1px 1px 1px rgba(0,0,0,0.3)" :padding" 0px
     0px 5px 0px")

     (".container > header h2" :font-family" 'Cinzel', 'Times New Roman', serif"
                               :font-size" 1.5em" :color" #f8f8f8" :text-shadow" 1px
     1px 1px rgba(0,0,0,0.6)")

     (".container > header p" :font-family" 'Times New Roman',serif")

     ;; Button Style
     ("p.portfolio-button" :display" block" :padding" 15px 0px")

     ( "p.portfolio-button a" :display" inline-block" :border" 1px solid #FFF"
                              :padding" 4px 10px 3px" :font-family" 'Cinzel', 'Times New Roman', serif"
                              :font-size" 13px" :line-height" 18px" :margin" 2px 3px" :font-weight" 400"
                              :box-shadow" 0px 1px 1px rgba(0,0,0,0.1)" :color"#fff" :border-radius" 5px"
                              :background"rgba(0,0,0,0)")

     ("p.portfolio-button a:hover" :background"rgba(0,0,0,0.5)")

     ("p.portfolio-button a:active" :background"rgba(0,0,0,0.5)")

     ;; Media Queries
     ("@media screen and (max-width: 767px)" (".container > header" :text-align"
     center") ("p.portfolio-button" :position" relative" :top" auto" :left"
     auto") (".portfolio-container" :flex-direction" column"))))
  )

(defun generate-global-css ()
  "Generates global.css file in the #P/project-isidore/assets/global.css"
  (cl-css:compile-css
   (merge-pathnames #P"global.css"
                    (asdf:system-relative-pathname :project-isidore "../assets/"))
   '(;; Load Fonts
     ("@font-face"
      :font-family "Cinzel"
      :font-style "normal"
      :font-weight "400"
      :font-display "swap"
      :src "local(''), url('/cinzel-v11-latin-regular.woff2') format('woff2')")
     ;; Navbar CSS
     (".header-fixed " :position" relative" :left" 0" :top" 0" :z-index" 1"
                       :width" 100%" :margin" 0" :background-color" #FDF6E3")

     (".navbar " :display" flex" :justify-content" space-between" :align-items"
     center" :color" #fff" :line-height" 60px")

     (".navbar .logo " :flex" 3")

     (".navbar .logo a " :display" block" :margin-left" 10%" :font-size" 22px"
                         :font-family" 'Cinzel'" :font-weight" 400" :color" #000" :text-decoration"
     none")

     (".navbar .logo a:hover " :color" #777777")

     (".navbar nav " :flex" 8")

     (".navbar label " :user-select" none" :cursor" pointer" :padding" 28px
     20px" :position" relative" :z-index" 3")

     (".navbar label i " :height" 2px" :position" relative" :transition"
     background .2s ease-out" :width" 18px" :font-style" normal" :font-weight"
     normal")

     (".navbar label i:before, .navbar label i:after " :content" ''" :height"
     100%" :position" absolute" :transition" all .2s ease-out" :width" 100%")

     (".navbar label i, .navbar label i:before, .navbar label i:after "
      :display" block" :background" black")

     (".navbar label i:before " :top" 5px")

     (".navbar label i:after " :top" -5px")

     (".navbar #navbar-toggle " :display" none")

     (".header #navbar-toggle:checked ~~ .menu " :visibility" visible" :opacity"
     0.99")

     (".header #navbar-toggle:checked ~~ label " :background" gray"
                                                 :border-radius" 50%")

     (".header #navbar-toggle:checked ~~ label i " :background" transparent")

     (".header #navbar-toggle:checked ~~ label i:before " :transform"
     rotate(-45deg)")

     (".header #navbar-toggle:checked ~~ label i:after " :transform"
     rotate(45deg)")

     (".header #navbar-toggle:checked ~~ label:not(.steps) i:before, .header
     #navbar-toggle:checked ~~ label:not(.steps) i:after " :top" 0")

     ("@media (max-width: 768px) "
      (".navbar nav " :visibility" hidden" :opacity" 0" :z-index" 2" :position"
      fixed" :top" 0px" :left" 0px" :width" 100%" :height" 100%" :transition"
      all 0.3s ease-out" :display" table" :background" #f5deb3")

      (".navbar nav ul " :margin" 0" :padding" 20px 0" :display" table-cell"
                         :vertical-align" middle")

      (".navbar nav li " :display" block" :text-align" center" :padding" 20px 0"
                         :text-align" center" :font-size" 50px" :min-height" 50px" :font-weight"
      bold" :cursor" pointer" :transition" all 0.3s ease-out")

      (".navbar nav li:hover " :background" #FDF6E3")

      (".navbar nav li:hover a " :color" brown" :transition" all 0.3s ease-out")

      (".navbar nav li a " :font-family" 'Cinzel', 'Times New Roman', serif"
                           :color" #212121"))

     ("@media (min-width: 768px) "
      (".navbar nav ul " :margin" 0" :padding" 0" :display" flex"
                         :justify-content" space-around" :text-align" center" :list-style" none")

      (".navbar nav li " :flex" 1")

      (".navbar nav li a " :display" block" :padding" 0 8px" :font-size" 16px"
                           :font-weight" 400" :font-family" 'Cinzel'" :line-height" 60px" :color"
      #000" :text-decoration" none")

      (".navbar nav li.active " :background" #555")

      (".navbar nav li:hover " :background" #EEE8D5")

      (".navbar label " :display" none"))

     ;; End of Navbar CSS
     ("html" :background-color" #FDF6E3")

     ("body " :width" 95%" :margin" 2% auto" :font-size" 14px" :line-height"
     1.4em" :font-family" 'Times New Roman', serif" :font-weight" 400" :color" #333")

     ("@media screen and (min-width: 600px) " ("body " :font-size" 18px"))

     ("@media screen and (min-width: 910px) " ("body " :width" 900px"))

     ("#content " :width" 100%")

     ("::selection " :background" #D6EDFF")

     ("p " :line-height "1.5em" :margin" 1em auto")

     ("ol,ul " :margin" 0 auto")

     ("dl " :margin" 0 auto")

     (".title " :text-align" center" :margin" 0.8em auto" :color" black"
                :font-family"'Cinzel'" :font-weight" 400")

     (".subtitle " :text-align" center" :font-size" 1.5em" :line-height" 1.4"
                   :font-weight" bold" :margin" 1em auto")

     (".updated " :text-align" center")

     (".abstract " :text-align" center" :margin" auto" :width" 80%" :font-style"
     italic")

     (".abstract p:last-of-type:before " :content" \"    \"" :white-space" pre")

     (".status " :font-size" 90%" :margin" 2em auto")

     ("[class^=\"section-number-\"] " :margin-right" .5em")

     ("[id^=\"orgheadline\"] " :clear" both")

     ("#footnotes " :font-size" 90%")

     (".footpara " :display" inline" :margin" .2em auto")

     (".footdef " :margin-bottom" 1em")

     (".footdef sup " :padding-right" .5em")

     ("a " :color" #609cdb" :text-decoration" none")

     ("a:hover " :color" #003355" :border-bottom" 1px dotted")

     ("figure " :padding" 0px" :margin" 1em auto" :text-align" center")

     ("img " :max-width" 100%" :vertical-align" middle")

     (".MathJax_Display " :margin" 0!important" :width" 90%!important")

     ("h1,h2,h3,h4,h5,h6 " :color" #A5573E" :line-height" 1em" :font-weight"
     400" :font-family" 'Arial', non-serif")

     ("h2 a " :color" #A5573E")

     ("h3 a " :color" #A5573E")

     ("h4 a " :color" #A5573E")

     ("h5 a " :color" #A5573E")

     ("h6 a " :color" #A5573E")

     ("h1,h2,h3 " :line-height" 1.4em")

     ("h4,h5,h6 " :font-size" 1em")

     ("@media screen and (min-width: 600px) "
      ("h1 " :font-size" 2em")
      ("h2 " :font-size" 1.5em")
      ("h3 " :font-size" 1.3em")
      ("h1,h2,h3 " :line-height" 1.4em")
      ("h4,h5,h6 " :font-size" 1.1em"))

     ("dt " :font-weight" bold")

     ("table " :margin" 1em auto" :border-top" 2px solid" :border-bottom" 2px
     solid" :border-collapse" collapse")

     ("thead " :border-bottom" 2px solid")

     ("table td + td, table th + th " :border-left" 1px solid gray")

     ("table tr " :border-top" 1px solid lightgray")

     ("td,th " :padding" 0.3em 0.6em" :vertical-align" middle")

     ("caption.t-above " :caption-side" top")

     ("caption.t-bottom " :caption-side" bottom")

     ("caption " :margin-bottom" 0.3em")

     ("figcaption " :margin-top" 0.3em")

     ("th.org-right " :text-align" center")

     ("th.org-left " :text-align" center")

     ("th.org-center " :text-align" center")

     ("td.org-right " :text-align" right")

     ("td.org-left " :text-align" left")

     ("td.org-center " :text-align" center")

     ("blockquote " :margin" 1em 2em" :padding-left" 1em" :border-left" 3px
     solid #ccc")

     ("kbd " :background-color" #f7f7f7" :font-size" 80%" :margin" 0 .1em"
             :padding" .1em .6em")

     (".todo " :background-color" red" :color" white" :padding" .1em 0.3em"
               :border-radius" 3px" :background-clip" padding-box" :font-size" 80%"
               :font-family" 'Courier New', monospace" :line-height" 1")

     (".done " :background-color" green" :color" white" :padding" .1em 0.3em"
               :border-radius" 3px" :background-clip" padding-box" :font-size" 80%"
               :font-family" 'Courier New', monospace" :line-height" 1")

     (".priority " :color" orange" :font-family" 'Courier New', monospace")

     ;; /* Because tag span is set to float. This is more like a hacking. Maybe
     ;; we need a cleaner solution. */
     ("#table-of-contents li " :clear" both")

     (".tag " :font-family" 'Courier New', monospace" :font-size" 0.7em"
              :font-weight" normal")

     (".tag span " :padding" 0.3em 0.3em" :float" right" :margin-right" .5em"
                   :border" 1px solid #bbb" :border-radius" 3px" :background-clip"
     padding-box" :color" #333" :background-color" #eee" :line-height" 1")

     (".timestamp " :color" #333" :font-size" 90%")

     (".timestamp-kwd " :color" #5f9ea0")

     (".org-right " :margin-left" auto" :margin-right" 0px" :text-align" right")

     (".org-left " :margin-left" 0px" :margin-right" auto" :text-align" left")

     (".org-center " :margin-left" auto" :margin-right" auto" :text-align"
     center")

     (".underline " :text-decoration" underline")

     ("#postamble p, #preamble p " :font-size" 90%" :margin" .2em")

     (".copyright-container " :display" flex" :justify-content" space-between"
                              :align-items" center" :flex-wrap" wrap" :padding" 0.4ex 1em")

     (".generated " :font-family" 'Courier New', monospace" :text-align"
     center")

     ("p.verse " :margin-left" 3%")

     (":not(pre) > code " :padding" 2px 5px" :margin" auto 1px" :border" 1px
     solid #DDD" :border-radius" 3px" :background-clip" padding-box"
                          :background-color "#F5F5F5" :color" #333" :font-size" 80%")

     (".org-src-container " :border" 1px solid #ccc" :box-shadow" 3px 3px 3px
     #eee" :background-color "#F5E8C4" :font-family" 'Courier New', monospace"
                            :font-size" 80%" :margin" 1em auto" :padding" 0.1em 0.5em" :position"
     relative")

     (".org-src-container>pre " :overflow" auto")

     (".org-src-container>pre:before " :display" block" :position" absolute"
                                       :background-color" #b3b3b3" :top" 0" :right" 0" :padding" 0 0.5em"
                                       :border-bottom-left-radius" 8px" :border" 0" :color" white" :font-size"
     80%")

     ;; /* from http://demo.thi.ng/org-spec/ */
     ;; perhaps we can add some boilerplate here to reduce verbosity?
     (".org-src-container>pre.src-sh:before " :content" 'sh'")
     (".org-src-container>pre.src-bash:before " :content" 'bash'")
     (".org-src-container>pre.src-emacs-lisp:before " :content" 'Emacs Lisp'")
     (".org-src-container>pre.src-R:before " :content" 'R'")
     (".org-src-container>pre.src-org:before " :content" 'Org'")
     (".org-src-container>pre.src-cpp:before " :content" 'C++'")
     (".org-src-container>pre.src-c:before " :content" 'C'")
     (".org-src-container>pre.src-html:before " :content" 'HTML'")
     (".org-src-container>pre.src-js:before " :content" 'Javascript'")
     (".org-src-container>pre.src-javascript:before " :content" 'Javascript'")
     ;; // More languages from
     ;; http://orgmode.org/worg/org-contrib/babel/languages.html
     (".org-src-container>pre.src-abc:before " :content" 'ABC'")
     (".org-src-container>pre.src-asymptote:before " :content" 'Asymptote'")
     (".org-src-container>pre.src-awk:before " :content" 'Awk'")
     (".org-src-container>pre.src-C:before " :content" 'C'")
     (".org-src-container>pre.src-calc:before " :content" 'Calc'")
     (".org-src-container>pre.src-clojure:before " :content" 'Clojure'")
     (".org-src-container>pre.src-comint:before " :content" 'comint'")
     (".org-src-container>pre.src-css:before " :content" 'CSS'")
     (".org-src-container>pre.src-D:before " :content" 'D'")
     (".org-src-container>pre.src-ditaa:before " :content" 'Ditaa'")
     (".org-src-container>pre.src-dot:before " :content" 'Dot'")
     (".org-src-container>pre.src-ebnf:before " :content" 'ebnf'")
     (".org-src-container>pre.src-forth:before " :content" 'Forth'")
     (".org-src-container>pre.src-F90:before " :content" 'Fortran'")
     (".org-src-container>pre.src-gnuplot:before " :content" 'Gnuplot'")
     (".org-src-container>pre.src-haskell:before " :content" 'Haskell'")
     (".org-src-container>pre.src-io:before " :content" 'Io'")
     (".org-src-container>pre.src-java:before " :content" 'Java'")
     (".org-src-container>pre.src-latex:before " :content" 'LaTeX'")
     (".org-src-container>pre.src-ledger:before " :content" 'Ledger'")
     (".org-src-container>pre.src-ly:before " :content" 'Lilypond'")
     (".org-src-container>pre.src-lisp:before " :content" 'Lisp'")
     (".org-src-container>pre.src-makefile:before " :content" 'Make'")
     (".org-src-container>pre.src-matlab:before " :content" 'Matlab'")
     (".org-src-container>pre.src-max:before " :content" 'Maxima'")
     (".org-src-container>pre.src-mscgen:before " :content" 'Mscgen'")
     (".org-src-container>pre.src-Caml:before " :content" 'Objective'")
     (".org-src-container>pre.src-octave:before " :content" 'Octave'")
     (".org-src-container>pre.src-org:before " :content" 'Org'")
     (".org-src-container>pre.src-perl:before " :content" 'Perl'")
     (".org-src-container>pre.src-picolisp:before " :content" 'Picolisp'")
     (".org-src-container>pre.src-plantuml:before " :content" 'PlantUML'")
     (".org-src-container>pre.src-python:before " :content" 'Python'")
     (".org-src-container>pre.src-ruby:before " :content" 'Ruby'")
     (".org-src-container>pre.src-sass:before " :content" 'Sass'")
     (".org-src-container>pre.src-scala:before " :content" 'Scala'")
     (".org-src-container>pre.src-scheme:before " :content" 'Scheme'")
     (".org-src-container>pre.src-screen:before " :content" 'Screen'")
     (".org-src-container>pre.src-sed:before " :content" 'Sed'")
     (".org-src-container>pre.src-shell:before " :content" 'shell'")
     (".org-src-container>pre.src-shen:before " :content" 'Shen'")
     (".org-src-container>pre.src-sql:before " :content" 'SQL'")
     (".org-src-container>pre.src-sqlite:before " :content" 'SQLite'")
     (".org-src-container>pre.src-stan:before " :content" 'Stan'")
     (".org-src-container>pre.src-vala:before " :content" 'Vala'")
     (".org-src-container>pre.src-axiom:before " :content" 'Axiom'")
     (".org-src-container>pre.src-browser:before " :content" 'HTML'")
     (".org-src-container>pre.src-cypher:before " :content" 'Neo4j'")
     (".org-src-container>pre.src-elixir:before " :content" 'Elixir'")
     (".org-src-container>pre.src-request:before " :content" 'http'")
     (".org-src-container>pre.src-ipython:before " :content" 'iPython'")
     (".org-src-container>pre.src-kotlin:before " :content" 'Kotlin'")
     (".org-src-container>pre.src-Flavored Erlang lfe:before " :content" 'Lisp'")
     (".org-src-container>pre.src-mongo:before " :content" 'MongoDB'")
     (".org-src-container>pre.src-prolog:before " :content" 'Prolog'")
     (".org-src-container>pre.src-rec:before " :content" 'rec'")
     (".org-src-container>pre.src-ML  sml:before " :content" 'Standard'")
     (".org-src-container>pre.src-Translate translate:before " :content" 'Google'")
     (".org-src-container>pre.src-typescript:before " :content" 'Typescript'")
     (".org-src-container>pre.src-rust:before " :content" 'Rust'")

     (".inlinetask " :background" #ffffcc" :border" 2px solid gray" :margin"
     10px" :padding" 10px")

     ("#org-div-home-and-up " :font-size" 70%" :text-align" right" :white-space"
     nowrap")

     (".linenr " :font-size" 90%")

     (".code-highlighted " :background-color" #ffff00")

     ("#bibliography " :font-size" 90%")

     ("#bibliography table " :width" 100%")

     (".creator " :display" block")

     ("@media screen and (min-width: 600px) " (".creator " :display" inline"
                                                           :float" right"))

     (".cryptedmail:after " :font-family" 'Times New Roman',serif"
     :font-size"1.2 rem" :content" attr(data-name) \"@\" attr(data-domain) \".\"
     attr(data-tld)"))))
