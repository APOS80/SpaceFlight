#lang racket
 
(require pict3d
         pict3d/universe
         math/flonum)
 

;Structs
(struct item (type position direction roll speed) #:mutable #:transparent) 

;Mutables
(define mouse-x 0)
(define mouse-y 0)
(define key-ad 0)
(define key-ws 0)

(define camOne (item 'camera (pos 1000 1000 994) (dir 0 0 1) 0 0))

;Helpers

(define (angles->dir-yp ang1 ang2 axis) 
  (let ([ang1  (fl (degrees->radians ang1))]
        [ang2  (fl (degrees->radians ang2))])
        (define ca1 (flcos ang1))
        (define sa1 (flsin ang1))
        (define ca2 (flcos ang2))
        (define sa2 (flsin ang2))
    (cond
      ;Yaw around Z axis
      [(equal? 'zx axis) (dir (*  1 ca1 ca2) (* 1 sa1 ca2) (* -1 sa2))] ;Yaw from x, Pitch counterclockwise.
      [(equal? 'zy axis) (dir (* -1 sa1 ca2) (* 1 ca1 ca2) (*  1 sa2))] ;Yaw from y, Pitch counterclockwise.
      ;Yaw around Y axis
      [(equal? 'yz axis) (dir (* 1 sa1 ca2) (* -1 sa2) (* 1 ca1 ca2))] ;Yaw from z, Pitch counterclockwise.
      [(equal? 'yx axis) (dir (* 1 ca1 ca2) (* 1 sa2) (* -1 sa1 ca2))] ;Yaw from x, Pitch counterclockwise.
      ;Yaw around X axis
      [(equal? 'xy axis) (dir (* -1 sa2) (* 1 ca1 ca2) (* 1 sa1 ca2))] ;Yaw from y, Pitch counterclockwise.
      [(equal? 'xz axis) (dir (* 1 sa2) (* -1 sa1 ca2) (* 1 ca1 ca2)) ] ;Yaw from z, Pitch counterclockwise.
      ;else
      [else (printf "(angles->dir-yp real real symbol)\n")] 
      )
    ))

(define (dir->angles-yp dv axis)
    (let* ([x (fl(dir-dx dv))]
           [y (fl(dir-dy dv))]
           [z (fl(dir-dz dv))]
           [r (flsqrt (fl+ (fl* x x) (fl+ (fl* y y) (fl* z z))))])
      (cond
        ;Yaw around Z axis
        [(equal? 'zx axis) (list (radians->degrees (atan y x))
                            (radians->degrees (asin (fl/ z r))))] ;Yaw from x, Pitch counterclockwise.
        [(equal? 'zy axis) (list (radians->degrees (atan x y))
                            (radians->degrees (asin (fl/ z r))))] ;Yaw from y, Pitch counterclockwise.
        ;Yaw around y axis
        [(equal? 'yz axis) (list (radians->degrees (atan x z))
                            (radians->degrees (asin (fl/ y r))))] ;Yaw from z, Pitch counterclockwise.
        [(equal? 'yx axis) (list (radians->degrees (atan z x))
                            (radians->degrees (asin (fl/ y r))))] ;Yaw from x, Pitch counterclockwise.
        ;Yaw around x axis
        [(equal? 'xy axis) (list (radians->degrees (atan z y))
                            (radians->degrees (asin (fl/ x r))))] ;Yaw from y, Pitch counterclockwise.
        [(equal? 'xz axis) (list (radians->degrees (atan y z))
                            (radians->degrees (asin (fl/ x r))))] ;Yaw from z, Pitch counterclockwise.
        [else (list 0.0 0.0)]
         )
  ))

(define (recalc-cam cam mx my ad ws)
  (let*
      ([di (dir-normalize (dir+ (item-direction cam)
                                (transform-dir
                                 (angles->dir-yp ad (/ (* -1 my) 20) 'yz)
                                 
                                  (point-at
                                   (item-position camOne)
                                   (item-direction camOne)
                                   #:angle (item-roll camOne)
                                   #:up -y)
                                 )))]
       [po (pos+ (item-position cam) (dir-scale di ws))]
       [ro (+ (item-roll cam) (/ mx 20))])
    (item 'camera
          po
          di
          ro
          ws)))

;stuff
(current-material (material #:ambient 0.01
                            #:diffuse 0.39
                            #:specular 0.6
                            #:roughness 0.5))

(current-color (rgba "orange"))
;(current-emitted (emitted "orange" 0.1))

(define lights
  (combine ;(light (pos 1020 1020 1010) (emitted "Thistle" 5))
           ;(light (pos 1020 1021 9990) (emitted "PowderBlue" 3))
           (with-color (rgba "white")
             (with-emitted (emitted "white" 5)
             (sphere (pos 1000 1000 1000) 1)))
           (with-color (rgba "red")
             (with-emitted (emitted "red" 5)
             (sphere (pos 1020 1020 1020) 1)))
   
           ))

(define objects
  (combine (cube (pos 1010 1010 1010) 1/2)
           
           (cube (pos 1012 1010 1010) 1/2)
           (cube (pos 1014 1010 1010) 1/2)
           
           (cube (pos 1010 1012 1010) 1/2)
           (cube (pos 1010 1014 1010) 1/2)
           ))

(define orientation
  (combine (with-color (rgba "red")
             (with-emitted (emitted "red" 2)
             (arrow (pos 1000 1000 1000) (pos 1010 1000 1000))))
           (with-color (rgba "green")
             (with-emitted (emitted "green" 2)
             (arrow (pos 1000 1000 1000) (pos 1000 1010 1000))))
           (with-color (rgba "blue")
             (with-emitted (emitted "blue" 2)
             (arrow (pos 1000 1000 1000) (pos 1000 1000 1010))))
           ))

;BigBang stuff
(define (on-mouse  s n t x y e) ;Get directions
  (let ([mx (- x 250)]
        [my (- y 250)])
    (if (or (> mx -250) (< mx 250) (> my -250)(< my 250))
        (if (and (< (abs mx) 10) (< (abs my) 10))
            (begin
         (set! mouse-x 0)
         (set! mouse-y 0))
        (begin
         (set! mouse-x mx)
         (set! mouse-y my)))
        
        (begin
         (set! mouse-x 0)
         (set! mouse-y 0))
        )
    ))

(define (on-key s n t k) ;Respond to key events
  (begin
  (case k
    [("a") (set! key-ad (- key-ad 1))]
    [("d") (set! key-ad (+ key-ad 1))]
    [("w") (set! key-ws (+ key-ws 0.1))]
    [("s") (set! key-ws (- key-ws 0.1))]
    )
  ))


(define (on-draw s n t)
  (begin
    (set! camOne (recalc-cam camOne mouse-x mouse-y key-ad key-ws))
    
    (combine lights
             orientation
             objects
             (basis 'camera (point-at (item-position camOne)
                                      (item-direction camOne)
                                      #:angle (item-roll camOne) #:up -y )))          
  ))
 
(big-bang3d 0  #:width 500 #:height 500 #:on-mouse on-mouse #:on-key on-key #:on-draw on-draw)