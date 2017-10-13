#lang racket
 
(require pict3d
         pict3d/universe
         math/flonum
         quaternion)
 

;Structs
(struct item (type position pitchaxis yawaxis rollaxis speed) #:mutable #:transparent) 

;Mutables
(define mouse-x 0.0)
(define mouse-y 0.0)
(define key-ad 0.0)
(define key-ws 0.0)

(define camOne (item 'camera
                     (pos 1000.0 1000.0 994.0)
                     (dir 1.0 0.0 0.0)
                     (dir 0.0 1.0 0.0)
                     (dir 0.0 0.0 1.0)
                     0.0))

;Helpers

(define (angle-360 a)
  (cond [(> a 360.0) (- a 360.0)]
        [(< a 0.0)   (+ 360.0 a)]
        [else a])
  )

(define (QvToDir qv)
  (let ([x (qvector-x qv)]
        [y (qvector-y qv)]
        [z (qvector-z qv)])
    (dir x y z))
  )

(define (DirToQv d)
  (let ([x (dir-dx d)]
        [y (dir-dy d)]
        [z (dir-dz d)])
    (qvector x y z))
)

(define (recalc object p y r s)
  (let* ([Pi (q-rotation (* p (/ pi 180.0)) (DirToQv (item-pitchaxis object)))]
         [Ya (q-rotation (* y (/ pi 180.0)) (DirToQv (item-yawaxis object)))]
         [Ro (q-rotation (* r (/ pi 180.0)) (DirToQv (item-rollaxis object)))]
         [ROTS (q-multiply-qq (q-multiply-qq Pi Ya) Ro)]
         [X (QvToDir (quaternion-v (q-rotate ROTS (quaternion 0.0 (DirToQv (item-pitchaxis object))))))] 
         [Y (QvToDir (quaternion-v (q-rotate ROTS (quaternion 0.0 (DirToQv (item-yawaxis object))))))]
         [Z (QvToDir (quaternion-v (q-rotate ROTS (quaternion 0.0 (DirToQv (item-rollaxis object))))))]
         [S s]
         [P (pos+ (item-position object) (dir-scale (dir-normalize Z) S))])
         (item (item-type object) P (dir-normalize X) (dir-normalize Y) (dir-normalize Z) S))
  )

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
           (with-color (rgba "blue")
             (with-emitted (emitted "blue" 5)
             (sphere (pos 1000 1004 994) 0.5)))
   
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
           ))

;BigBang stuff
(define (on-mouse  s n t x y e) ;Get directions
  (let ([mx (- x 250.0)]
        [my (- y 250.0)])
    (if (or (> mx -250.0) (< mx 250.0) (> my -250.0)(< my 250.0))
        (if (and (< (abs mx) 10.0) (< (abs my) 10.0))
            (begin
         (set! mouse-x 0.0)
         (set! mouse-y 0.0))
        (begin
         (set! mouse-x mx)
         (set! mouse-y my)))
        
        (begin
         (set! mouse-x 0.0)
         (set! mouse-y 0.0))
        )
    ))

(define (on-key s n t k) ;Respond to key events
  (begin
  (case k
    [("a") (set! key-ad (- key-ad 1.0))]
    [("d") (set! key-ad (+ key-ad 1.0))]
    [("w") (set! key-ws (+ key-ws 0.05))]
    [("s") (set! key-ws (- key-ws 0.05))]

    )
  ))


(define (on-draw s n t)
  (begin
    (set! camOne (recalc camOne (fl* -1.0 (fl/ mouse-y 100.0)) key-ad (fl/ mouse-x 100.0) key-ws))
                       
    (combine lights
             orientation
             objects
     (basis 'camera (affine (item-pitchaxis camOne) (item-yawaxis camOne) (item-rollaxis camOne) (item-position camOne)))
     )
  
  ))
 
(big-bang3d 0  #:width 500 #:height 500 #:on-mouse on-mouse #:on-key on-key #:on-draw on-draw)