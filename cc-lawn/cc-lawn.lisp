;;;; cc-lawn.lisp

(in-package #:cc-lawn)

(in-readtable glyphs:syntax)

(defun html-base (page-name title content)
  "Print out the default HTML page"
  (let ((canonical-name (format nil "http://lawninordercc.com~a" page-name)))
    (with-html-output-to-string (s nil :prologue "<!DOCTYPE html>" :indent t)
      (setf (cl-who:html-mode) :sgml)
      (:html
       (:head
        (:title (str title) " | Lawn In Order Corpus Christi, TX")
        (:meta :name "viewport" :content "width=device-width, initial-scale=1")
        (:meta :name "description" :content "Since 2009, Lawn In Order Lawn Care in Corpus Christi has provided lawn care, landscaping and yard maintenance services to many satisfied clients.")
        (:meta :name "keywords" :content "Mowing, Edging, Blowing, Landscaping, Weed Trimming, Tilling, Fertilization, Garden Work, Bug Treatments, Shrub Shaping, Tree Cutting, Sod Laying, Grass Repair, Sprinkler Repair")
        (:link :rel "canonical" :href canonical-name)
        (:link :rel "stylesheet" :href "http://fonts.googleapis.com/css?family=Lustria" :type "text/css")
        (:link :rel "stylesheet" :href "http://fonts.googleapis.com/css?family=Dancing+Script" :type "text/css")
        (:link :rel "stylesheet" :href "/main.css" :type "text/css")
        (:link :rel "stylesheet" :href "/css/mobile.css" :type "text/css")
        (:script :src "/js/jquery-1.11.0.min.js" :type "text/javascript")
        (:script :src "/main.js" :type "text/javascript"))
       (:body
        (:div :itemscope t :itemtype "http://schema.org/LocalBusiness"
              (:img :src "/img/logo.png" :id "logo")
              (:div :id "header"
                    (:h2 :itemprop "name" "Lawn In Order")
                    (:p :id "phone" "Call today " (:span :itemprop "telephone" "361-232-7479")))
              (:div :id "menu"
                    (:a :href "/" "Home")
                    (:a :href "/tools/" "Tools")
                    (:a :href "/about/" "About")
                    (:a :href "/gallery/" "Gallery")
                    (:a :href "/pricing/" "Services")
                    (:a :href "/reviews/" "Reviews")
                    (:a :href "/blog/" "Blog")
                    (:a :href "/contact/" "Contact"))
              (:div :id "content"
                    (:h1 (str title))
                    (:div :id "copy" (str content)))
              (str "<script>
  (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
  (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
  m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
  })(window,document,'script','//www.google-analytics.com/analytics.js','ga');

  ga('create', 'UA-51500457-1', 'lawninordercc.com');
  ga('send', 'pageview');

</script>")
              (:div :id "footer"
                    (:div :itemprop "address"
                          :itemscope t
                          :itemtype "http://schema.org/PostalAddress"
                          "Copyright &copy; Lawn In Order 2014-2016 - powered by "
                          (:a :href "http://ahungry.com" "Ahungry")
                          (:p (:span :itemprop "streetAddress" "3662 Austin")
                              (:br)
                              (:span :itemprop "addressLocality" "Corpus Christi") ", "
                              (:span :itemprop "addressRegion" "TX") ", 78411"
                              (:br) "Ph: 361-232-7479"))
                    (:date :itemprop "foundingDate" :date "2009-01-01")
                    (:time :itemprop "openingHours" :datetime "Mo-Su" "Call for availability!"))
              ))))))

(defparameter *services*
  '(("Residential and Commercial Year Round Lawn Care" . "Starting at $35")
    ("Tree and Palm Trimming/Removal with Haul Off" . "
<ul>
<li>- $75 job minimum</li>
<li>- Palm Skinning</li>
<li>- Tree pruning, thinning, topping</li>
</ul>")
    ("Firewood and Cooking Wood Sales" . "Starting at $5 per bundle up to $130 per regular truck bed load.")
    ("Fertilizing, Aerating, and Dethatching Service" . "Starting at $175 for front and back yards")
    ("Stump Grinding" . "Stumps start at $75")
    ("OTHER SERVICES" . "<ul>
<li>- Hedge trimming</li>
<li>- Mulching</li>
<li>- Weeding</li>
<li>- Tilling</li>
<li>- Leaf cleanup</li>
<li>- Monthly landscape maintenance programs</li>
<li>- Land clearing</li>
</ul>
")
    ))

(defun html-service-list ()
  "Just print out the services we need"
  (with-html-output-to-string (s)
    (:div :id "services"
          (:ul ;; Display all the services in a list
           (mapcar (lambda (service)
                     (htm (:li (:h3 (str (car service)))
                               (str (cdr service))))) *services*)))))

(defun page-home ()
  (html-base
   ""
   "Corpus Christi Lawn Care"
   (with-html-output-to-string (s)
     (htm
      (:h2 "Delivering Justice to Lawns Since 2009")
      (:div :id "slideshow"
            (:div (:p "Perfectly Cut and Maintained Lawns!")
                  (:img :src "/img/yellow-house.jpg"))
            (:div (:p "Aerating, Dethatching, and Fertilizing are essential to a healthy lawn!")
                  (:img :src "/img/red-machines.jpg"))
            (:div (:p "Tree Trimming and Removal Plus Stump Grinding Available")
                  (:img :src "/img/trim-oak-cropped.jpg")))
      (:div :id "blocks"
            (:div (:h4 "Lawn Care Services")
                  (:p :class "lh28" "We offer year round lawn care service, both
residential and commercially.  This includes mowing, weedeating,
edging and blowing the front and back of properties.
")
                  (:a :href "/contact/" "CLICK HERE!"))
            (:div (:h4 "Aerating / Dethatching / Fertilizing")
                  (:p :class "lh22" "This service is essential for a thick, green, healthy lawn and may
be performed twice a year during spring and fall. If your lawn is to be the envy
of the block, don't even think about skipping out on this service!")
                  (:a :href "/blog/?blog=2014-11-08-Aerating-Dethatching-Fertilizing.html" :id "aeration" "CLICK HERE!"))
            (:div (:h4 "Other Services")
                  (:p "
Our company has the expertise and know how to spruce up your lawn and
keep it looking great year round. Enjoy the convenience and worry free
benefits of a weekly, 10 day, or bi weekly lawn maintenance schedule
today!

From lawn and tree care, to stump grinding, and everything in-between
our dedicated and insured company can take care of all your needs.

For a full list of services we offer,")
                  (:a :href "/pricing/" "CLICK HERE!"))
            (:br :style "clear:both")
            (:br :style "clear:both")
            )
      (:p
       "Lawn In Order is "
       (:span :itemprop "description" "the premier landscaping and lawn servicing company
in Corpus Christi, Texas.  No matter what level of project you need to
accomplish, we have a plan for you.")
       )
      (:p "You can count on our top-notch efficiency, dedication, and
quality to providing you a cutting edge lawn - afterall, we've been
delivering justice to lawns since 2009!")
      (:p "If you need lawn care in Corpus Christi, TX then look no
further, you've found it!")  ))))

;; http://plantscience.psu.edu/research/centers/turf/extension/factsheets/calculations-turfgrass-fertilization
(defun page-tools ()
  (html-base
   "/tools/"
   "Mulch Calculator Fertilizer Calculator"
   (with-html-output-to-string (s)
     (htm
      (:h2 "Tools Provided by Lawn In Order (Corpus Christi, Texas)")
      (:p "Lawn In Order wants you to succeed in your personal projects,
as well as those projects we assist you with.  As such, please feel free to use
any of the provided tools/calculators to help you in your DIY (do it yourself)
lawn care and projects.")
      (:h4 "Mulch Calculator")
      (:form
       :method "post"
       :action "#"
       :id "mulch-calc"
       :name "mulch-calc"
       (:p "Mulch is great for conserving moisture, lowering soil temperatures around
plant roots, preventing erosion and reducing weed growth.")
       (:p "The mulch calculator will help you figure out how many bags of mulch
will be needed for your project.")
       (:label "Width (ft.):" (:input :type "text" :class "width" :name "width" :value "10"))
       (:label "Height (ft.):" (:input :type "text" :class "height" :name "height" :value "10"))
       (:label "Area (sq. ft.):" (:input :type "text" :class "area" :name "area" :value "100" :readonly "true"))
       (:label "Mulch Depth (in.)"
               (:input :type "text" :class "saturation" :name "saturation" :value "3"))
       (:label "Bag Size:"
               (:label :class "bag-span" "1 sq.ft."
                       (:input :type "radio" :class "bag-size" :name "bag-size" :value "1"))
               (:label :class "bag-span" "2 sq.ft."
                       (:input :type "radio" :class "bag-size" :name "bag-size" :value "2" :checked "true"))
               (:label :class "bag-span" "3 sq.ft."
                       (:input :type "radio" :class "bag-size" :name "bag-size" :value "3")))
       (:label "Bags Required:"
               (:input :type "text" :class "bags" :name "bags" :value "1"))
       (:label (:input :type "submit" :class "calc" :name "calc" :value "Calculate"))
       )
      (:h4 "Fertilizer Calculator")
      (:form
       :method "post"
       :action "#"
       :id "fert-calc"
       :name "fert-calc"
       (:p "By inputting your yard's width and height, as well as you're
typical fertilizer bag size, we'll assist you in calculating how many
bags of fertilizer you'll need to cover a certain area in a standard 1.0 lb.
of nitrogen per 1000 sq. ft.")
       (:p "For most users, leaving a 1 lb. nitrogen saturation level should
be adequate")
       (:label "Width (ft.):" (:input :type "text" :class "width" :name "width" :value "100"))
       (:label "Height (ft.):" (:input :type "text" :class "height" :name "height" :value "10"))
       (:label "Area (sq. ft.):" (:input :type "text" :class "area" :name "area" :value "1000" :readonly "true"))
       (:label "Nitrogen Saturation (lb./1000 sq.ft.) "
               (:input :type "text" :class "saturation" :name "saturation" :value "1"))
       (:label "Bag Size (lbs.):" (:input :type "text" :class "bag-size" :name "bag-size" :value "50"))
       (:label "Bag Nitrogen % (in 26-5-10 this is 26%):"
               (:input :type "text" :class "fert-type" :name "fert-type" :value "26"))
       (:label "Bags Required:" (:input :type "text" :class "bags" :name "bags" :value "1"))
       (:label (:input :type "submit" :class "calc" :name "calc" :value "Calculate"))
       )
      (:p "Give us a call today to schedule a free estimate/consultation
at " (:h4 "361-232-7479"))))))

(defun page-about ()
  (html-base
   "/about/"
   "Aerating Dethatching Fertilizing Corpus Christi"
   (with-html-output-to-string (s)
     (htm
      (:h2 "About Lawn In Order")
      (:p "Lawn In Order strives for excellence in all we do.
Unlike our competitors, you will not be left feeling unsatisfied.
Each client has a special place in our heart - and we want you
to have a special place in yours, for us, as well.")
      (:p "You can count on us for giving your lawn everything it needs,
including the essentials such as aerating, dethatching, and fertilizing.")
      (:p "Give us a call today to schedule a free estimate/consultation
at " (:h4 "361-232-7479") " - don't delay, you could be missing the
opportunity of a lifetime!")))))

(defun gallery/get-images ()
  "Build the list of gallery images for displaying."
   (let* ((2016-images (mapcar #'file-namestring
                          (directory #P"~/src/lisp/cc-lawn/www/img/gallery/2016*.*")))
          (images (mapcar #'file-namestring
                          (directory #P"~/src/lisp/cc-lawn/www/img/gallery/*.*"))))
     (append
      2016-images
      (remove-if (lambda (name)
                   (string= name "2016" :end1 (min (length name) 4))) images))))

(defun page-gallery (&optional big-pic)
  (html-base
   "/gallery/"
   "Corpus Christi Lawn Service"
   (let ((images (gallery/get-images)))
     (with-html-output-to-string (s)
       (htm
        (:h2 "Check out some of our great work!")
        (when big-pic
          (let ((big-pic (format nil "/img/gallery/~a" big-pic)))
            (htm (:img :src big-pic))))
        (:div :id "thumbs"
              (loop for image in images
                 unless (string= image "")
                 do (let ((img (format nil "/img/gallery/thumbs/~a" image))
                          (link (format nil "?big-pic=~a" image)))
                      (htm (:div :class "container"
                                 (:a :href link
                                     (:img :src img))))))
              (:br :style "clear:both")
              ))))))

(defun page-pricing ()
  (html-base
   "/pricing/"
   "Corpus Christi Lawn Maintenance"
   (with-html-output-to-string (s)
     (htm
      (:h2 "The most affordable and competitive pricing around!")
      (:p "Lawn In Order is the premier landscaping and lawn servicing
company in Corpus Christi, Texas.  No matter what level of project you
need to accomplish, we have a plan for you.")
      (:p "You can count on our top-notch efficiency, dedication, and
quality to providing you a cutting edge lawn - afterall, we've been
delivering justice to lawns since 2009!")
      (:p "We offer a variety of services to keep your lawn in
      top-notch condition, including, but not limited to:")
      (str (html-service-list))))))

(defun page-reviews ()
  (html-base
   "/reviews/"
   "Corpus Christi Professional Lawn Care"
   (with-html-output-to-string (s)
     (htm
      (:h2 "See what our customers are saying about us...")
      (:div :class "review"
            "I have been using Matt to cut my lawn for the last year. He
is always here when he says he will. He gets in and gets the job
 done. My wife and I are extremely pleased with his service and all
his employees are very cordial and polite. The yard is always neat
and manicured when they leave."
            (:div :class "sig" "---Gregory R. Gamble"))
      (:div :class "review"
            "I am writing a review about Matt Parks and his lawn service
.  Matt has been handling my yard work and also trims my 3 palm
 trees, and also trims my fruit trees in my backyard. He is absolutely
the BEST. He does wonderful work, always prompt and professional,
allows me to make monthly payments on the more expensive jobs that he
does at my house. He has been taking care of my yard for over 3
years.  I would highly recommend him."
            (:div :class "sig" "---Kathy Robinson"))
      (:div :class "review"
            "I have personally known Mr. Matt Parks for about three
years and have found him to be a reliable and skillful landscaper.
 He has worked in an outstanding manner the many landscaping and tree
trimming projects for me.  His skills are impeccable and will
professionally complete projects in a timely manner."
            (:div :class "sig" "---ÁNGEL ZÚÑIGA"))
      (:div :class "review"
            "Looking for excellent lawn care service? Matt Parks is the
one you should call!!! After years of service I have gotten steady,
reliable and excellent service. More recently Matt ran an aerator over
my lawn and improved our lawn by 100%. It is now the best looking lawn
on our street. My wife and I couldn't more proud!!!!"
            (:div :class "sig" "---Robert Pena"))
      (:div :class "review"
            "I live in the Haas Middle School area off of McArdle Road
and have been using the mowing services of Matt Parks with Lawn in
Order for the past couple of months.  He has always been punctual in
taking care of my lawn according to the arranged schedule.  The
mowing, weed eating, and edging have been done with the care and the
expertise that you rely on from a landscaping company.  I would highly
recommend Lawn in Order for your mowing needs."
            (:div :class "sig" "---Jan Geyer"))
      (:div :class "review"
            "Lawn In Order is a very dependable company! I have
had multiple trees cut down and stump grinded at different times and
 they have helped me out each time. They have offered payments on MY
pay schedule. Lawn In Order also worked with me to  have them cut
down at my brush pick up schedule. I have never witnessed a hard
working lawn company before this one! I recommend them to all my
friends constantly!"
            (:div :class "sig" "---Corie Kaminski"))
      (:div :class "review"
            "Matt's Lawn Service (Lawn In Order) did a super job of
cutting nasty trees and clearing vines out of the easement behind our
house. The job was completed in a timely and careful manner and all
debris was cleared off the property as they agreed to do. The estimate
was reasonable."
            (:div :class "sig" "---Glena Sue, Morningside Addition"))
      ))))

(ƒ strip-html-extension
   ~"(.*)\\.html"~ → |"\\1"|)

(defun get-blog (article)
  (with-open-file (s (format nil "~a~a"
                             "~/src/lisp/cc-lawn/blog/"
                             (string-trim "./" article)))
    (format nil "~{~a~%~}"
            (loop for line = (read-line s nil 'eof)
               until (eq line 'eof)
               collect line))))

(defun page-blog (&optional blog)
  (html-base
   "/blog/"
   "Blog entries"
   (let ((articles (mapcar #'file-namestring
                           (directory #P"~/src/lisp/cc-lawn/blog/*.*"))))
     (with-html-output-to-string (s)
       (htm
        (if blog
            (htm
             (:a :href "/blog/" "Back to blog index")
             (str (get-blog blog)))
            (htm
             (:p "Choose one of our great blog articles to learn more!")))
        (:hr :style "clear:both;")
        (:h3 "Blog articles:")
        (loop for article in (nreverse articles)
           unless (string= article "")
           do (let* ((title (strip-html-extension article))
                     (link (format nil "?blog=~a" article)))
                (htm
                 (:div
                  (:a :href link (str title))))))
        )))))

(defun page-contact ()
  (html-base
   "/contact/"
   "Contact a Representative Today!<br> to schedule your FREE estimate"
   (with-html-output-to-string (s)
     (htm
      (:p "Reach us via phone at " (:h4 "361-232-7479")
          "for the most direct method of contact, or send us an email
by filling out the form below:"))
     (:form :id "contact-form" :name "contact" :method "post" :action "http://static.lawninordercc.com/mail.php"
            (:label "Your name: " (:input :type "text" :name "name"))
            (:label "Your email: " (:input :type "text" :name "email"))
            (:label "Your phone: " (:input :type "text" :name "phone"))
            (:label "Message:" (:textarea :name "message" :value "Enter a message here"))
            (:input :type "submit")))))

(defun main ()
  "Main entry point"
  (print ""))

;;; "cc-lawn" goes here. Hacks and glory await!

(defparameter *toot* (make-instance 'hunchentoot:easy-acceptor
                                    :port 14001
                                    :message-log-destination nil
                                    :access-log-destination nil))
(setf (hunchentoot:acceptor-document-root *toot*) #P"~/src/lisp/cc-lawn/www/")
(hunchentoot:start *toot*)

(hunchentoot:define-easy-handler (home-route :uri "/") ()
  (setf (hunchentoot:content-type*) "text/html")
  (page-home))

(hunchentoot:define-easy-handler (js-route :uri "/main.js") ()
  (setf (hunchentoot:content-type*) "text/javascript")
  (web-js))

(hunchentoot:define-easy-handler (css-route :uri "/main.css") ()
  (setf (hunchentoot:content-type*) "text/css")
  (web-css))

(hunchentoot:define-easy-handler (tools-route :uri "/tools/") ()
  (setf (hunchentoot:content-type*) "text/html")
  (page-tools))

(hunchentoot:define-easy-handler (about-route :uri "/about/") ()
  (setf (hunchentoot:content-type*) "text/html")
  (page-about))

(hunchentoot:define-easy-handler (gallery-route :uri "/gallery/") (big-pic)
  (setf (hunchentoot:content-type*) "text/html")
  (page-gallery big-pic))

(hunchentoot:define-easy-handler (pricing-route :uri "/pricing/") ()
  (setf (hunchentoot:content-type*) "text/html")
  (page-pricing))

(hunchentoot:define-easy-handler (reviews-route :uri "/reviews/") ()
  (setf (hunchentoot:content-type*) "text/html")
  (page-reviews))

(hunchentoot:define-easy-handler (blog-route :uri "/blog/") (blog)
  (setf (hunchentoot:content-type*) "text/html")
  (page-blog blog))

(hunchentoot:define-easy-handler (contact-route :uri "/contact/") ()
  (setf (hunchentoot:content-type*) "text/html")
  (page-contact))
