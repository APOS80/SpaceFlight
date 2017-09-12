#lang racket
 
(require pict3d
         pict3d/universe)
 

;values
(define mouse-x 0)
(define mouse-y 0)

(define cam-rot '(0 0 90)) ;rotations in degrees yaw pitch roll
(define cam-pos (dir 5000 5000 5000)) ;position

(define speed 0)

;Helpers
(define (on-mouse  s n t x y e) ;Get directions
  (begin
    (set! mouse-x (/ (* (- x 250) -2) 360) )
    (set! mouse-y (/ (* (* (- y 250) -2)) 360))
    ;(printf "x: ~a Y: ~a\n" x y)
    ))

(define (on-key s n t k) ;Respond to key events
  (case k
    [("a") (set! cam-rot (list-set cam-rot 2 (+ (list-ref cam-rot 2) 0.5)))]
    [("d") (set! cam-rot (list-set cam-rot 2 (- (list-ref cam-rot 2) 0.5)))]
    [("w") (set! speed (+ speed 0.1))]
    [("s") (set! speed (- speed 0.1))]
    )
  )

(define (corr-angles l)
  (map (lambda (a) (cond
                     [(> a 360) (- a 360)]
                     [(< a 0) (- 360 a)]
                     [else a]))
       l))

(define (RAMP p rot pos);Rotate and move pict, pict list-of-rotation dir.
  (move
  (rotate-z/center ;Yaw
  (rotate-y/center ;Pitch
  (rotate-x/center ;Roll
             p
  (list-ref rot 2))
  (list-ref rot 1))
  (list-ref rot 0))
  pos)
  )


;stuff
(current-material (material #:ambient 0.01
                            #:diffuse 0.39
                            #:specular 0.6
                            #:roughness 0.5))

(define lights
  (combine (light (pos 5000 5001 5004) (emitted "Thistle" 3))
           (light (pos 5000 4992 5000) (emitted "PowderBlue" 2))
           ))

(define objects
  (combine (cube (pos 5000 5000 5000) 1/2)
           (cube (pos 5002 5000 5000) 1/2)
           (cube (pos 5004 5000 5000) 1/2)
           (cube (pos 5006 5000 5000) 1/2)
           (cube (pos 5000 5002 5000) 1/2)
           (cube (pos 5000 5004 5000) 1/2)
           (cube (pos 5000 5006 5000) 1/2)
           (cube (pos 5000 5008 5000) 1/2)))

;Draw stuff
(define (on-draw s n t)
  (begin
    (let ([ yaw (+ (list-ref cam-rot 0) mouse-y)]
          [ pitch (+ (list-ref cam-rot 1) mouse-x)]
          )
      (set! cam-rot (corr-angles (list yaw pitch (list-ref cam-rot 2))))
      )

    (set! cam-pos
          (dir+
           cam-pos
          (dir-scale
          (angles->dir (list-ref cam-rot 0) (list-ref cam-rot 1))
           speed)
          ))
    
  (printf "speed: ~a\n" speed)
    (combine lights
             objects
             (RAMP (basis 'camera (point-at origin (dir 1 0 0 ) #:up -z)) cam-rot cam-pos))          
  ))
 
(big-bang3d 0  #:on-mouse on-mouse #:on-key on-key #:on-draw on-draw)