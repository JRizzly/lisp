;;;; css.lisp

(in-package #:cc-lawn)

(defun web-js ()
  "The relevant CSS"
  (ps

    (defvar +slide-duration+ 3000)
    (defvar *slide* nil)

    (defun slideshow ()
      "Image slideshow to view"
      (setf *slide* (chain ($ "#slideshow div") (last)))
      (chain *slide* (animate (create "padding-top" "300px") +slide-duration+))
      (set-timeout
       (λλ α → (progn
                 (chain ($ "#slideshow")
                        (prepend (chain *slide*
                                        (css (create "padding-top" "0px")))))
                 (set-timeout #'slideshow +slide-duration+)))
       (* 2 +slide-duration+))
      )

    (defun fertilizer-calculator ()
      "Calculate fertilizer needed for a given area"
      (let ((w (chain ($ "#fert-calc .width") (val)))
            (h (chain ($ "#fert-calc .height") (val)))
            (area 0)
            (ft (/ (chain ($ "#fert-calc .fert-type") (val)) 100))
            (s (chain ($ "#fert-calc .saturation") (val)))
            (p1000 0)
            (bs (chain ($ "#fert-calc .bag-size") (val)))
            (bags 1)
            (a (chain ($ "#fert-calc .area"))))

        ;; First find lbs./1000 sq.ft.
        (setf area (* w h))
        (chain a (val area))

        ;; 3.8 lbs. per 1000 sq.ft. at 1 lbs. saturation
        (setf p1000 (/ s ft))

        (setf bags (/ (* p1000 (/ area 1000)) bs))

        ;;(when (< bags 1) (setf bags 1))

        (chain ($ "#fert-calc .bags") (val bags))

        false

      ))

    (defun mulch-calculator ()
      "Calculate mulch needed for a given area"
      (let ((w (chain ($ "#mulch-calc .width") (val)))
            (h (chain ($ "#mulch-calc .height") (val)))
            (area 0)
            (s (chain ($ "#mulch-calc .saturation") (val)))
            (bags 1)
            (bag-size (chain ($ "#mulch-calc .bag-size:checked") (val)))
            (a (chain ($ "#mulch-calc .area"))))

        (setf area (* w h))
        (chain a (val area))

        ;; Standard bag covers 4 sq.ft. @ 3 inch depth
        ;; or covers 8 sq.ft. @ 2 inch depth
        ;; or covers 16 sq.ft. @ 1 inch depth
        (setf bags (* (/ area (* bag-size 12)) s))

        ;;(when (< bags 1) (setf bags 1))

        (chain ($ "#mulch-calc .bags") (val bags))

        false

      ))

    (chain ($ document)
           (ready
            (lambda ()
              (slideshow)

              (chain ($ "#fert-calc input") (change (lambda () (fertilizer-calculator))))
              (chain ($ "#fert-calc .calc") (click (lambda () (fertilizer-calculator))))
              (chain ($ "#fert-calc") (submit (lambda () (fertilizer-calculator))))

              (chain ($ "#mulch-calc input") (change (lambda () (mulch-calculator))))
              (chain ($ "#mulch-calc .calc") (click (lambda () (mulch-calculator))))
              (chain ($ "#mulch-calc") (submit (lambda () (mulch-calculator))))

              (mulch-calculator)
              )))
    ))
