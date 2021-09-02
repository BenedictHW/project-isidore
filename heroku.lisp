(in-package #:cl-user)

;;; application entry point
(defvar *acceptor* nil)
;; Takes a PORT parameter as Heroku assigns a different PORT per environment
(defun initialize-application (&key port)
  (cl-css:compile-css
   "/app/static/index.css"
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
      :background-image "url(pic1.jpg)"
      )
     (".cb-slideshow li:nth-child(2) span"
      :background-image "url(pic2.jpg)"
      :-webkit-animation-delay" 6s"
      :-moz-animation-delay" 6s"
      :-o-animation-delay" 6s"
      :-ms-animation-delay" 6s"
      :animation-delay" 6s"
      )
     (".cb-slideshow li:nth-child(3) span"
      :background-image" url(pic3.jpg)"
      :-webkit-animation-delay" 12s"
      :-moz-animation-delay" 12s"
      :-o-animation-delay" 12s"
      :-ms-animation-delay" 12s"
      :animation-delay" 12s"
      )
     (".cb-slideshow li:nth-child(4) span"
      :background-image" url(pic4.jpg)"
      :-webkit-animation-delay" 18s"
      :-moz-animation-delay" 18s"
      :-o-animation-delay" 18s"
      :-ms-animation-delay" 18s"
      :animation-delay" 18s"
      )
     (".cb-slideshow li:nth-child(5) span"
      :background-image" url(pic5.jpg)"
      :-webkit-animation-delay" 24s"
      :-moz-animation-delay" 24s"
      :-o-animation-delay" 24s"
      :-ms-animation-delay" 24s"
      :animation-delay" 24s"
      )
     (".cb-slideshow li:nth-child(6) span"
      :background-image" url(pic6.jpg)"
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
  (setf hunchentoot:*dispatch-table*
        ;; for an explanation of ` and , and '. see https://stackoverflow.com/questions/60378335/quote-comma-in-common-lisp
        `(hunchentoot:dispatch-easy-handlers
          ,(hunchentoot:create-folder-dispatcher-and-handler ; Requires full system path
                                                    "/" "/app/static/"))) ; /app is the root on a heroku filesystem

  (when *acceptor*
    (hunchentoot:stop *acceptor*))

  (setf *acceptor*
        (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port port))))
