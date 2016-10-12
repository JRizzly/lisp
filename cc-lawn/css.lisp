;;;; css.lisp

(in-package #:cc-lawn)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (css-lite:make-css-func
      gradient (start end)
      ;; Common browser compatible gradient
      (list :background end
            :background (format nil "-moz-linear-gradient(top, ~a 0%, ~a 100%)" start end)
            :background (format nil "-webkit-gradient(linear, left top, left bottom,
                                                      color-stop(0%, ~a),
                                                      color-stop(100%, ~a))" start end)
            :background (format nil "-webkit-linear-gradient(top, ~a 0%, ~a 100%)" start end)
            :background (format nil "-o-linear-gradient(top, ~a 0%, ~a 100%)" start end)
            :background (format nil "-ms-linear-gradient(top, ~a 0%, ~a 100%)" start end)
            :background (format nil "linear-gradient(to bottom, ~a 0%, ~a 100%)" start end)
            )))

(defun web-css ()
  "The relevant CSS"
  (css-lite:css
    (("body")
     (:background "url(/img/sunset.jpg) top center no-repeat fixed #fff"
                  :font-size "1em"
                  :font-family "'Lustria', serif"
                  :margin "0px"
                  :padding "0px"))
    (("#content")
     (:background "rgba(255,255,255,.9)"
                  ;;(gradient "rgba(255,255,255,.9)" "rgba(255,255,255,1)")
                  :color "#333"
                  :font-size "1.2em"
                  :padding "20px"))
    (("h2")
     (:background "transparent"
                  :color "#000"
                  :font-family "'Dancing Script', monospace"
                  :font-size "2.5em"
                  :line-height "70px"
                  :margin "8px"
                  :max-width "80%"
                  :text-align "left"
                  :padding "0px"))
    (("h1")
     (:font-family "'Dancing Script', monospace"
                   :margin "0px"
                   :max-width "80%"))
    (("h4")
     (:font-family "'Dancing Script', monospace"
                   :font-size "1.3em"
                   :margin "0px"
                   :max-width "80%"))
    (("#header")
     (:background "#fff"
                  (gradient "#ccc" "#fff")
                  :color "#000"
                  ;;:font-family "'Pablo impallari', 'Lustria' "
                  :padding "2px"
                  :text-align "center"))
    (("#menu")
     (:background "#8E0000" ;; rgba(0,190,50,.3)"
                  (gradient "maroon" "crimson")
                  :border "3px solid #000"
                  :border-radius "5px"
                  :box-shadow "3px 3px 6px #000"
                  :color "#fff"
                  :text-decoration "none")
     (("a")
      (:background "transparent"
                   :border-radius "10px"
                   :color "#fff"
                   :display "inline-block"
                   :font-weight "bold"
                   :margin-right "10px"
                   :padding "12px"
                   :text-decoration "none"
                   :transition "1.0s ease all"))
     (("a:hover")
      (:background "url(/img/mower.png) 2px -24px rgba(0,255,55,.4)"
                   :font-weight "bold"
                   :text-shadow "0px 0px 9px #fff"
                   :text-decoration "none")))
    (("#copy")
     (:text-align "justify"
                  :line-height "2em"
                  :padding "2%"))
    (("#grass")
     (:float "right"
             :margin-left "14px"
             :margin-top "10px"
             :width "20%"))
    (("#slideshow")
     (:border "2px solid rgba(0,0,0,.3)"
              :box-shadow "4px 4px 7px #000"
              :height "300px"
              :margin "auto"
              :margin-bottom "50px"
              :overflow "hidden"
              :position "relative"
              :width "98%")
     (("div")
      (:position "absolute"
                 :left "0px"
                 :top "0px"
                 :width "100%")
      (("p")
       (:color "#fff"
               :background "rgba(0,0,0,.5)"
               :display "block"
               :font-size "1.5em"
               :font-style "italic"
               :left "50%"
               :line-height "40px"
               :margin-left "-50%"
               :padding "3%"
               :position "absolute"
               :text-align "center"
               :text-shadow "3px 3px 6px #000"
               :width "94%"))
      (("img")
       (:width "100%"))))
    (("#phone")
     (:font-size "1.5em"
                 :font-family "'Dancing Script', monospace"
                 :font-weight "bold"
                 :padding "0px"
                 :margin "0px"))
    (("#logo")
     (:position "absolute"
                :top "0px"
                :right "0px"
                :z-index "9"
                :width "300px"))
    (("#services")
     (:color "#666"
             :display "block"
             :font-style "italic")
     (("ul")
      (:margin "0px" :padding "0px"))
     (("ul li")
      (:list-style-type "none"
                        :margin "0px" :padding "0px"))
     (("h3")
      (:background "#009955"
                   :border-radius "3px"
                   :box-shadow "3px 3px 6px #000"
                   :color "#fff"
                   :font-style "normal"
                   :margin "0px"
                   :padding "0px"
                   :padding-left "9px")))
    (("#contact-form")
     (:background "#666"
                  :color "#fff"
                  :padding "30px"
                  :text-align "center")
     (("label")
      (:display "block"))
     (("input, textarea")
      (:display "block"
                :border-radius "3px"
                :margin "auto"
                :width "90%")))
    (("#blocks")
     (:border "0px solid #000"
              :margin "auto"
              :width "100%")
     (("div")
      (:display "block"
                :float "left"
                :border "4px solid rgba(0,0,0,.3)"
                :border-radius "30px 3px 30px 3px"
                :box-shadow "3px 3px 16px #000"
                :color "#fff"
                :line-height "20px"
                (gradient "rgba(155,125,40,.7)" "rgba(00,120,50,.8)")
                :font-size "1em"
                :margin-right "1%"
                :padding "12px"
                :overflow "hidden"
                :width "28%")
      (("p")
       (:height "auto"))
      (("h2")
       (:text-align "right"
                    :display "block"))
      (("a")
       (:background "#fff"
                    :border "2px solid #666"
                    :border-radius "8px 2px 8px 2px"
                    (gradient "#fff" "#bbb")
                    :display "block"
                    :font-size "1em"
                    :padding "3px"
                    :text-align "center"
                    :text-decoration "none"
                    :color "#000"))
      (("a:hover")
       (:box-shadow "inset 3px 3px 8px #000"))
      ))
    ((".lh28")
     (:line-height "38px"))
    ((".lh22")
     (:line-height "32px"))
    (("#footer")
     (:background "#333"
                  :padding "50px"
                  :color "#fff")
     (("a")
      (:color "lime")))
    (("#thumbs")
     (:background "transparent")
     ((".container")
      (:height "120px"
               :display "block"
               :float "left"
               :box-shadow "3px 3px 6px #000"
               :border "2px solid rgba(0,0,0,.8)"
               :width "150px"
               :margin "20px"
               :overflow "hidden"))
     (("img")
      (:width "200px")))
    ((".review")
     (:background "rgba(100,250,150,.7)"
                  :border "2px solid #111"
                  :box-shadow "4px 4px 6px #000"
                  :color "#000"
                  :margin "2%"
                  :padding "20px"
                  :text-align "right")
     ((".sig")
      (:font-size "2em")))
    (("label")
     (:display "block"
               :text-align "right"
               :max-width "60%")
     (("input, select")
      (:margin-left "10px"
                     :width "200px")))
    ((".bag-size")
     (:padding-left "10px"
                    :width "20px"))
    ((".bag-span")
     (:border "1px solid #999999"
              :border-radius "8px"
              :cursor "pointer"
              :display "inline-block"))
    ))
