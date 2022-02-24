;;;; SPDX-FileCopyrightText: 2021 Hanshen Wang <Hanshen@HanshenWang.com>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:project-isidore/views
  (:use #:common-lisp
        #:project-isidore/styles
        #:project-isidore/model
        #:spinneret
        #:parenscript)
  ;; No package local nicknames. See commit 1962a26.
  (:export
   #:index-page #:about-page #:work-page #:contact-page #:subscribe-page
   #:bible-page #:bible-search-page)
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
  `(with-html
     (:html :lang "en"
            (:head
             (:title, title)
             (:meta :charset "utf-8")
             (:meta :name "viewport"
                    :content "width=device-width, initial-scale=1")
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
             (:div :class "main" ,@body)
             (:hr)
             (:footer
              (:div :class "copyright-container"
                    (:div :class "copyright" "Copyright &copy; 2021 Hanshen Wang.")))))))

(defun index-page ()
  (with-html
    (:html :lang "en"
           (:head
            (:title "HanshenWang.com")
            (:meta :charset "utf-8")
            (:meta :name "viewport"
                   :content "width=device-width, initial-scale=1")
            (:style (:raw (index-css))))
           (:body
            ;; background CSS3 slideshow
            (:ul :class "slideshow"
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
                   (:p "Welcome to my personal website! This website was built with"
                       (:a :href "https://nextjs.org" :target "_blank" :rel "noreferrer" (:s "Next.js and React"))
                       (:a :href "https://edicl.github.io/hunchentoot/" :target "_blank" :rel "noreferrer" " Hunchentoot") "and Common Lisp. My resume can be found under the work tab. I hope you find what you're looking for,and may the wind be always at your back.")
                   (:p :class "portfolio-button"
                       (:a :href "/about" "About")
                       (:a :href "/work" "Work")
                       (:a :href "/blog/archive.html" "Blog")
                       (:a :href "/contact" "Contact"))))
            ;; For a tutorial see: https://app.leby.org/post/fun-with-parenscript/
            (:script
             (:raw (ps-inline
                       (chain document (add-event-listener "DOMContentLoaded"
                         (lambda (event)
                           (var data-text (array "Hey there," "Bonjour." "¡Hola!" "Привет." "Hello!" "Guten Tag." "Good Day," "Welcome!" "Konnichiwa,"))
                           (defun type-writer(text i fn-callback)
                             (cond ((< i (length text))
                                    (setf (chain document(query-selector "h1")inner-h-t-m-l) (+(chain text (substring 0 (+ i 1))) "<span aria-hidden=\"true\"></span>"))
                                    (set-timeout (lambda () (type-writer text (+ i 1) fn-callback)) 100))
                                   ((equal (typeof fn-callback) "function")
                                    (set-timeout fn-callback 700))))
                           (defun start-text-animation (i)
                             (when (equal (typeof (aref data-text i)) "undefined")
                               (set-timeout (lambda () (start-text-animation 0))2000))
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
           (:input :type "submit" :value "Subscribe" :name "subscribe" :id "mc-embedded-subscribe" :class "button"))))

(defmacro bible-page-template ((&key title) &body body)
  "Template HTML for bible webpages. It removes the top banner navigation
located in the `web-page-template' macro and the copyright footer. The work is
in the public domain and frankly I think it's tacky to plaster the top navbar
with my name."
  `(with-html
     (:html :lang "en"
            (:head
             (:title, title)
             (:meta :charset "utf-8")
             (:meta :name "viewport"
                    :content "width=device-width, initial-scale=1")
             (:link :type "text/css" :href "global.css" :rel "stylesheet"))
            (:body (:div :class "main" ,@body)))))

(defun bible-page (bible-url)
  "127.0.0.1:8080/bible?verses=1-2-3-4-5-6 where BIBLE-URL \"1-2-3-4-5-6\" is a
  string with BEGINNINGbook-chapter-verse-ENDINGbook-chapter-verse."
  (bible-page-template (:title "1883 Haydock Douay Rheims Bible")
    (:h1 :class "title" "1883 Haydock Douay Rheims Bible")
    (:h4 "Presents commentary in a tabular format for ease of reading." (:a :href "/blog/tabular-douay-rheims.html" "Click to learn more."))
    (:div :style "overflow:auto"
          ;; Present links to all books of the bible.
          (loop for (link . title) in +bible-book-url-alist+
                do (:div :style "width:200px;float:left"
                         (:a :href link (:b title)))))
    (:br)
    (:div :style "overflow:auto"
          ;; Present links to all chapters of currently selected book.
          (loop for (link . title) in (make-bible-chapter-url-list bible-url)
                do (:div :style "width:200px;float:left"
                         (:a :href link (:b title)))))
    (:br)
    ;; Present search form for bible and haydock text.
    (:div :id "query-form" :style "text-align:center;"
          (:form :action "/bible-search" :method "GET"
                 (:label "Search: ")
                 (:input :name "query" :id "query" :size "50" :type "text"
                         :required "required")
                 (:input :type "submit" :value "Submit")))
    (:div :class "font-dropdown-menu"
          (:select :id "input-font" :class "input" :onchange "changeToFont(this);"
                   (:option :value "Times New Roman" :selected "selected" "Times New Roman")
                   (loop for font-name in (list "Arial" "Courier New" "Garamond" "Verdana")
                         do (:option :value font-name font-name)))
          (:script (:raw (ps-inline
                        (defun change-to-font (font)
                          (setf (chain document (get-element-by-id "main-content") style font-family) (@ font value)))))))
    (:table :id "main-content"
     ;; Present tabular view of bible text.
     (loop for bible-uid from (car (bible-url-to-uid bible-url))
             to (cadr (bible-url-to-uid bible-url))
           do (:tr :style "line-height: 1.5em;"
               (:td (:raw (get-heading-text bible-uid)))
               (:td (progn
                      (:raw (get-bible-text bible-uid))
                      (:br)
                      (:br)
                      (when (get-cross-references-text-with-links bible-uid)
                        (:raw (get-cross-references-text-with-links bible-uid)))))
               (:td :width "55%"
                    (when (get-footnotes-text-with-links bible-uid)
                      (:raw (get-footnotes-text-with-links bible-uid)))))))))

(defun bible-search-page (query)
  "127.0.0.1:8080/bible?query=chicken where QUERY \"chicken\" is a string.

DIV ID's
--------
query-syntax
query-tutorial
query-form"
  (bible-page-template (:title "1883 Haydock Douay Rheims Bible")
    (:h1 :class "title" "1883 Haydock Douay Rheims Bible")
    (:h2 :onclick "toggleDivWithId(\"query-syntax\")" :style "text-align:center;" "Query
    Syntax (click to toggle)")
    ;; Toggles HTML division form with ID "query-syntax" on click. Parenscript
    ;; compiles "toggle-syntax-help" to camel case toggleDivWithId
    (:script (ps-inline
                 (defun toggle-div-with-id (div-id)
                   (let ((syntax-help-div (chain document (get-element-by-id div-id))))
                     (if (equal (@ syntax-help-div style display) "none")
                         (setf (@ syntax-help-div style display) "block")
                         (setf (@ syntax-help-div style display) "none"))))))
    (:div :id "query-syntax" :style "display:none;"

          (:h2 "1. FIELDS")
          (:p " Each sentence of the Bible has the following metadata stored in fields,")
          (:li" (b) The book.")
          (:li" (c) The chapter number.")
          (:li" (v) The verse number.")
          (:li" (t) The text itself.")
          (:li" (f) If applicable, the footnote (Haydock) commentary.")
          (:li" (x) If applicable, the listed cross-references.")
          (:li" (implicit) All of the above fields.")

          (:h2 "2. OPERATOR")
          (:p " The fields are combined with Boolean OPERATOR(s),")
          (:li" (+) AND,")
          (:li" (implicit) OR")
          (:li" (!) NOT")
          (:p "Lastly, a particular predefined set of operations,")
          (:li " (*) TRUNCATE")
          (:p " The asterisk serves as the truncation (or wildcard) operator. Unlike the other operators, it should be appended to the word to be affected. Words match if they begin with the word preceding the * operator.")

          (:h2 "3. FORM")
          (:p " 3. A query takes the form of,")
          (:p "[OPERATOR FIELD : EXPRESSION]")
          (:p "where EXPRESSION may be affixed with an asterisk character. Mixing of explicit and implicit forms as well as multiple forms are allowed. E.g.")
          (:li"charity")
          (:li"charity cymbal")
          (:li"loved much")
          (:li"mercy +b:matthew")
          (:li"broad path destruction narrow")
          (:li"principalities !c:2")
          (:li"compare 'exact*' with 'exacteth'")
          (:li"no greater love lay down")
          (:li"+b:john +c:3 +v:15")
          (:h2 :onclick "toggleDivWithId(\"query-tutorial\")" :style "text-align:center;" "Tutorial: (click to toggle)")
          (:div :id "query-tutorial" :style "display:none;"

                (:p " What is the Bible if not the (his)story between God and his people? And so to reveal the new covenant, we must look to the old. How far back must we go for our journey of prefigurement? All the way back to the time when Moses lifted the serpent? Nope, that's too far back, my eyes are already glazing over. Let's flash back to 1972.")
                (:p "The Israeli government has enacted Operation Wrath of God in response to the Munich massacre. It was before my time, so I'll have to rely on Spielberg's dramatization (Munich 2005). The circle is closing in on the Mossad assassins, and they are forewarned:")
                (:blockquote "The race is not for the swift, nor the battle for the strong, But time and chance happens to them all. Fate's hand falls suddenly, who can say when it falls?")
                (:p " It's certainly pithy. Though I had a vague feeling I had heard it somewhere once upon a time.")
                (:code "Query> battle")
                (:br)
                (:code "Result>" (:a :target "_blank" :href "https://www.hanshenwang.com/bible-search?query=battle" "https://www.hanshenwang.com/bible-search?query=battle"))

                (:p " The first column indicates the scoring of the result where a higher score is ranked as a better match. The rows are arranged in decreasing order of said score. Note the top result is Isaiah 22:2 with a score of 129.")
                (:p " I see that my query has no explicit FIELD, so 'battle' was implicitly matched against all five fields as noted above under section 1. Explicit fields and operators are optional.")
                (:p " N.B. The top results all include 'battle' in both the verse and commentary. There are results that include 'battle' in either the verse or commentary, but these will be scored lower. This is why the OPERATOR OR is marked as implicit under section 2.")
                (:p " Say I would like to narrow the search to only the (t) text field.")

                (:code " Query> +t:battle")
                (:br)
                (:code " Result>" (:a :target "_blank" :href "https://www.hanshenwang.com/bible-search?query=%2Bt%3Abattle" "https://www.hanshenwang.com/bible-search?query=%2Bt%3Abattle"))

                (:p " Now the top result is now I Maccabees 2:35. There are still too many results. Let's filter her down a bit more. It sounds like the quotation would belong to the Wisdom books, and as of now 'battle' is pulling in too many results from the Historical books. To exclude all results from the book of I and II Maccabees,")
                (:code " Query> +t:battle !b:maccabees")
                (:br)
                (:code " Result>" (:a :target "_blank" :href "https://www.hanshenwang.com/bible-search?query=%2Bt%3Abattle+%21b%3Amaccabees" "https://www.hanshenwang.com/bible-search?query=%2Bt%3Abattle+%21b%3Amaccabees"))

                (:p " We can use the wildcard operator to make our lives a little bit easier. We can remove Judges, and Judith as they all begin with the pattern 'Ju'. Another very common use case for the wildcard operator is plurals.")

                (:code " Query> +t:battle !b:maccabees !b:ju*")
                (:br)
                (:code " Result>" (:a :target "_blank" :href "https://www.hanshenwang.com/bible-search?query=%2Bt%3Abattle+%21b%3Amaccabees+%21b%3Aj*" "https://www.hanshenwang.com/bible-search?query=%2Bt%3Abattle+%21b%3Amaccabees+%21b%3Aj*"))

                (:p " Let us finish the search by adding another term: '+t:race'. Note queries are whitespace sensitive.")

                (:code " Query> +t:battle !b:maccabees !b:ju* +t:race")
                (:br)
                (:code " Result>" (:a :target "_blank" :href "https://www.hanshenwang.com/bible-search?query=%2Bt%3Abattle+%21b%3Amaccabees+%21b%3Aju*+%2Bt%3Arace" "https://www.hanshenwang.com/bible-search?query=%2Bt%3Abattle+%21b%3Amaccabees+%21b%3Aju*+%2Bt%3Arace"))

                (:p "Deo gratias, we have found our verse: Ecclesiastes 9:11.")
                (:blockquote "I turned me to another thing, and I saw that under the sun, the race is not to the swift, nor the battle to the strong, nor bread to the wise, nor riches to the learned, nor favour to the skilful: but time and chance in all.")
                (:p " With accompanying commentary from Antoine Augustin Calmet (1757),")
                (:blockquote "All. Thus it appears to the inattentive, and to the wicked. For Solomon frequently inculcates that Providence directs all wisely. Human industry is not always attended with success. Deut. xxix. 19. This is a fresh proof of the vanity of all things. C.")
                (:p " How can Providence, lady fortuna, and human free will co-exist? It reminds me of an analogy I first heard from a Dominican Father, \"that if you meet you friend at a grocery store, that is indeed chance, but you had both intended to go the grocery store in the first place.\" Ahh, forgive me, my memory fails me. I'll have to go digging through old notes. Point is, there was no real contradiction. I guess this distinction was too darn subtle for me hah.")
                (:p " Anyways, as the old cliche goes: it's not possible to not have a philosophy, you'll just end up with a bad one. And as much as I wish questions like these stayed in the ivory tower, Life has a funny way of exacting a response from us in the day we least expect. Both in word and in deed, how willingly will we bear the cross of injustice? Especially when 'time and chance' combined with the misuse of free will ceases to be just text but become the blood and anguish of our loved ones.")
                (:p " Will we hearken and take to heart that 'Providence directs all wisely'? Can we swallow the fact that to deny it would make us the 'inattentive and wicked'?")
                (:p " Perhaps we will use our own free will to deny Providence and give in, for after all, 'who is willing to destroy a piece of his own heart'? Perhaps we renounce our free will -- given in His image -- blind to the Lover's pleas, no, insistence that we come to Him of our volition or not at all. Or will we decide to replace He who knew us from the beginning for that idol, that fickle mistress?")
                (:p "Ask not be 'tested beyond our strength'. Ask to be 'delivered from evil'. Ask your Father for mercy, unless somehow man can forgive his own transgressions, settle his own conscience, and assure his own salvation. Ask for a 'heart of flesh', to remember the obligation every creature owes to its Creator, every son to every Father.")
                (:p" In any case, I hope you find this search functionality useful! It is my hope that one day Microsoft Word and Adobe Acrobat Reader DC will support searching that is less primitive. To see what I mean, look at this" (:a :href "https://raw.githubusercontent.com/ShingoFukuyama/images/master/helm-swoop.gif" "GIF."))

                (:p" Some parting notes:")
                (:p" I. Ctrl+F is well known as a shortcut to search a word, and I have found the 'Highlight All' option in the popup user interface to be helpful when scanning the results. See if your particular browser supports this feature on your platform.")
                (:p "II. If you've stumbled upon the Tabular Douay Rheims and are curious about the faith, don't do the foolish thing I did by starting from Genesis. Now I can only testify what worked for myself, which was starting from the New Testament: choose one of the Gospels.")
                (:p "III. It seems only fitting to wrap up this tutorial with an epilogue from whence we started.")
                (:blockquote" Avner: If these people committed crimes we should have arrested them. Like" (:a :href "https://en.wikipedia.org/wiki/Adolf_Eichmann" "Eichmann."))
                (:blockquote" Ephraim: If these guys live, Israelis die. Whatever doubts you have Avner, you know this is true.")
                (:blockquote" [Avner walks away]")
                (:blockquote" Ephraim: You did well but you're unhappy.")
                (:blockquote" Avner: I killed seven men.")
                (:blockquote" Ephraim: Not" (:a :href "https://en.wikipedia.org/wiki/Ali_Hassan_Salameh" "Salameh.") "We'll get him of course.")
                (:blockquote" [Avner continues to walk away]")
                (:blockquote" Ephraim: You think you were the only team? It's a big operation, you were only a part. Does that assuage your guilt?")
                (:blockquote" Avner: Did we accomplish anything at all? Every man we killed has been replaced by worse.")
                (:blockquote" Ephraim: Why cut my finger nails? They'll grow back.")
                (:blockquote" Avner: Did we kill to replace the terrorist leadership or the Palestinian leadership? You tell me what we've done!")
                (:blockquote" Ephraim: You killed them for the sake of a country you now choose to abandon. The country your mother and father built, that you were born into. You killed them for Munich, for the future, for peace.")
                (:blockquote" Avner: There's no peace at the end of this no matter what you believe. You know this is true.")
                (:p " \"Live by the sword, die by the sword.\" I know for sure that's in the Bible somewhere. The proof is left as an exercise to the reader. More importantly, have a great rest of the day!")))
    (:p :style "text-align:center;"
        (:a :href "/bible?verses=1-1-1-1-1-31" "Return to tabular view."))
    (:div :class "font-dropdown-menu"
          (:select :id "input-font" :class "input" :onchange "changeToFont(this);"
            (:option :value "Times New Roman" :selected "selected" "Times New Roman")
            (loop for font-name in (list "Arial" "Courier New" "Garamond" "Verdana")
                  do (:option :value font-name font-name)))
          (:script (:raw (ps-inline
                             (defun change-to-font (font)
                               (setf (chain document (get-element-by-id "main-content") style font-family) (@ font value)))))))
    (:div :id "query-form" :style "text-align:center;"
          (:form :action "/bible-search" :method "GET"
                 (:label "Search: ")
                 (:input :name "query" :id "query" :size "50" :type "text"
                         :value query :required "required")
                 (:input :type "submit" :value "Submit")))
    (:table :id "main-content"
            ;; 37199 includes all verses of the bible. The extra are from chapter/book
            ;; descriptions etc. BIBLE-UID is a lie here, it ought to be named
            ;; MONTEZUMA-UID. They should be the same, but be careful with behaviour.
            (loop for (bible-uid . score) in (search-bible query '(:num-docs 37199))
                  do (:tr :style "line-height: 1.5em;"
                          ;; HACK Score of 1.37 > 137.
                          ;; Coerce double float to string with precision of 2.
                          (:td (write-to-string (floor score 0.01)))
                          (:td (:raw (get-heading-text bible-uid)))
                          (:td (progn
                                 (:raw (get-bible-text bible-uid))
                                 (:br) (:br)
                                 (when (get-cross-references-text-with-links bible-uid)
                                   (:raw (get-cross-references-text-with-links bible-uid)))))
                          (:td :width "50%" (when (get-footnotes-text-with-links bible-uid)
                                              (:raw (get-footnotes-text-with-links bible-uid)))))))))

