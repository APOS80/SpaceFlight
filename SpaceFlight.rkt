#lang racket
 
(require pict3d
         pict3d/universe
         math/flonum)
 

;values
(define mouse-x 0)
(define mouse-y 0)

(define cam-angle 0) ;Roll
(define cam-dir (dir 0 0 1)) ;Direction
(define cam-pos (pos 1000 1000 994)) ;position
(define cam-speed 0) ;cam speed

;Helpers
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
    [("a") (set! cam-angle (- cam-angle 0.5))]
    [("d") (set! cam-angle (+ cam-angle 0.5))]
    [("w") (set! cam-speed (+ cam-speed 0.1))]
    [("s") (set! cam-speed (- cam-speed 0.1))]
    )
  (printf "cam-speed: ~a\n" cam-speed)
  ))

(define (corr-angles a)
  (cond
    [(> a 360) (- a 360)]
    [(< a 0) (- 360 a)]
    [else a]))

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
             (sphere (pos 2000 2000 2000) 1)))
   
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
           

;Draw stuff
(define (on-draw s n t)
  (begin
    
    (set! cam-angle (corr-angles cam-angle))
    (set! cam-dir (dir-normalize (dir+ cam-dir
                                       (dir-scale
                                       (transform-dir
                                        (dir-normalize
                                         (dir mouse-x mouse-y 1))
                                        (point-at cam-pos cam-dir #:angle cam-angle #:up -y ))
                                       0.01)
                                       
                                       )))
    

    (set! cam-pos (pos+ cam-pos (dir-scale cam-dir cam-speed)))

    (combine lights
             orientation
             objects
             (basis 'camera (point-at cam-pos cam-dir #:angle cam-angle #:up -y ))
             )          
  ))
 
(big-bang3d 0  #:on-mouse on-mouse #:on-key on-key #:on-draw on-draw)