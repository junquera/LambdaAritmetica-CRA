#lang racket
; Booleanos

(define true (lambda (x y) x))

(define false (lambda (x y) y))

(define neg (lambda (x) (x false true)))

(define and (lambda (x y) (x y false)))

(define or (lambda (x y) (x true y)))

; Pares ordenados

(define par (lambda (x)
              (lambda (y)
                (lambda (f) (f x y)))))

(define primero (lambda (p) (p true)))

(define segundo (lambda (p) (p false)))

;;;;; Combinador de punto fijo

(define Y
  (lambda (f)
    ((lambda (x) (f (lambda (v) ((x x) v))))
     (lambda (x) (f (lambda (v) ((x x) v)))))))

;;;;;; Orden en naturales y test de nulidad

(define esmenoroigualnat (lambda (n)
                             (lambda (m)
                                (escero ((restanat n) m)))))

(define esmayoroigualnat (lambda (n)
                            (lambda (m)
                               (escero ((restanat m) n)))))

(define esmenornat (lambda (n)
                     (lambda (m)
                       (and ((esmenoroigualnat n) m) (noescero ((restanat m) n))))))

(define esmayornat (lambda (n)
                     (lambda (m)
                       (and ((esmayoroigualnat n) m) (noescero ((restanat n) m))))))

(define esigualnat (lambda (n)
                     (lambda (m)
                       (and ((esmayoroigualnat n) m) ((esmenoroigualnat n) m)))))

(define escero (lambda (n)
                 ((n (lambda (x) false)) true)))

(define noescero (lambda (n)
                    (neg (escero n))))

; Aritmética natural. Se define también comprobar para verificar que la cosa va bien. Defino algunos naturales para hacer comprobaciones. Los escribo en francés para distinguirlos de los enteros
; que escribiré en español.

(define zero (lambda (f)
               (lambda (x) x)))

(define sucesor (lambda (n)
                  (lambda (f)
                    (lambda (x)
                     (f((n f) x))))))

(define un (sucesor zero))

(define deux (sucesor un))

(define trois (sucesor deux))

(define quatre (sucesor trois))

(define cinq (sucesor quatre))

(define six (sucesor cinq))

(define sept (sucesor six))

(define huit (sucesor sept))

(define neuf (sucesor huit))

(define dix (sucesor neuf))

(define onze (sucesor dix))

(define douze (sucesor onze))

(define treize (sucesor douze))

(define quatorze (sucesor treize))

(define quinze (sucesor quatorze))

(define seize (sucesor quinze))

(define dix-sept (sucesor seize))

(define dix-huit (sucesor dix-sept))

(define dix-neuf (sucesor dix-huit))

(define vingt (sucesor dix-neuf))

(define comprobar (lambda (n)
                    ((n (lambda (x) (+ 1 x))) 0)))

(define sumnat (lambda (n)
                 (lambda (m)
                   ((n (lambda (x) (sucesor x))) m))))

(define prodnat (lambda (n)
                   (lambda (m)
                     (lambda (f)
                       (lambda (x) ((m (n f)) x))))))

(define prefn (lambda (f)
                (lambda (p)
                  ((par (f (primero p))) (primero p)))))

(define predecesor (lambda (n)
                     (lambda (f)
                       (lambda (x)
                            (segundo ((n ((lambda (g)
                                             (lambda (p) ((prefn g) p))) f)) ((par x) x)))))))

(define restanat (lambda (n)
                     (lambda (m)
                        ((m (lambda (x) (predecesor x))) n))))

(define restonataux
    (lambda (n)
        (lambda (m)
            ((Y (lambda (f)
                 (lambda (x)
                    ((((esmayoroigualnat x) m)
                        (lambda (no_use)
                            (f ((restanat x) m))
                        )
                        (lambda (no_use)
                            x
                        )
                    )
                        zero)    ; Pasa zero como argumento de no_use
                )
            ))
                n)  ; Pasa n como el valor inicial de x.
        )
))

(define restonat (lambda (n)
                      (lambda (m)
                        (((escero m) (lambda (no_use) false) (lambda (no_use) ((restonataux n) m))) zero))))


(define cocientenataux
    (lambda (n)
        (lambda (m)
            ((Y (lambda (f)
                (lambda (x)
                    ((((esmayoroigualnat x) m)
                        (lambda (no_use)
                            (sucesor (f ((restanat x) m)))
                        )
                        (lambda (no_use)
                            zero
                        )
                    )
                        zero)    ; Pasa zero como argumento de no_use
                )
            ))
                n)  ; Pasa n como el valor inicial de x.
        )
    )
)

(define cocientenat (lambda (n)
                      (lambda (m)
                        (((escero m) (lambda (no_use) false) (lambda (no_use) ((cocientenataux n) m))) zero))))


(define mcdnat
    (lambda (n)
        (lambda (m)
            (((Y (lambda (f)
                   (lambda (x)
                     (lambda(y)
                      (((escero y)
                       (lambda (no_use)
                            x
                        )
                       (lambda (no_use)
                            ((f y)((restonat x) y))
                        )

                    )
                        zero)    ; Pasa zero como argumento de no_use
                ))
            ))
                n) ; Pasa n como el valor inicial de x.
          m)       ; Pasa m como el valor inicial de y.
    )
))

;;;;;; Definición de algunos enteros

(define cero ((par zero) zero))

(define -uno ((par zero) un))

(define -dos ((par zero) deux))

(define -tres ((par zero) trois))

(define -cuatro ((par zero) quatre))

(define -cinco ((par zero) cinq))

(define -seis ((par zero) six))

(define -siete ((par zero) sept))

(define -ocho ((par zero) huit))

(define -nueve ((par zero) neuf))

(define -diez ((par zero) dix))

(define -once ((par zero) onze))

(define -doce ((par zero) douze))

(define -trece ((par zero) treize))

(define -catorce ((par zero) quatorze))

(define -quince ((par zero) quinze))

(define -dieciseis ((par zero) seize))

(define -diecisiete ((par zero) dix-sept))

(define -dieciocho ((par zero) dix-huit))

(define -diecinueve ((par zero) dix-neuf))

(define -veinte ((par zero) vingt))

(define uno ((par un) zero))

(define dos ((par deux) zero))

(define tres ((par trois) zero))

(define cuatro ((par quatre) zero))

(define cinco ((par cinq) zero))

(define seis ((par six) zero))

(define siete ((par sept) zero))

(define ocho ((par huit) zero))

(define nueve ((par neuf) zero))

(define diez ((par dix) zero))

(define once ((par onze) zero))

(define doce ((par douze) zero))

(define trece ((par treize) zero))

(define catorce ((par quatorze) zero))

(define quince ((par quinze) zero))

(define dieciseis ((par seize) zero))

(define diecisiete ((par dix-sept) zero))

(define dieciocho ((par dix-huit) zero))

(define diecinueve ((par dix-neuf) zero))

(define veinte ((par vingt) zero))

;;;;; Orden, valor absoluto y tests de nulidad, positividad y negatividad.
;;;
;;; m-n > m'-n' si y solo si m+n' > m'+n e igual con el resto

(define esmayoroigualent (lambda (r)
                           (lambda (s)
                             ((esmayoroigualnat ((sumnat (primero r)) (segundo s))) ((sumnat (primero s)) (segundo r))))))

(define esmenoroigualent (lambda (r)
                           (lambda (s)
                             ((esmenoroigualnat ((sumnat (primero r)) (segundo s))) ((sumnat (primero s)) (segundo r))))))

(define esmayorent (lambda (r)
                           (lambda (s)
                             ((esmayornat ((sumnat (primero r)) (segundo s))) ((sumnat (primero s)) (segundo r))))))

(define esmenorent (lambda (r)
                           (lambda (s)
                             ((esmenornat ((sumnat (primero r)) (segundo s))) ((sumnat (primero s)) (segundo r))))))

(define esigualent (lambda (r)
                           (lambda (s)
                             ((esigualnat ((sumnat (primero r)) (segundo s))) ((sumnat (primero s)) (segundo r))))))

(define absoluto (lambda (r)
                    (((esmayoroigualnat (primero r)) (segundo r)) ((par ((restanat (primero r)) (segundo r))) zero) ((par ((restanat (segundo r)) (primero r))) zero))))

(define negativo (lambda (r)
                   ((esmenorent r) cero)))

(define positivo (lambda (r)
                   ((esmayorent r) cero)))

(define esceroent (lambda (r)
                     ((esigualnat (primero r)) (segundo r))))

(define noesceroent (lambda (r)
                       (neg (esceroent r))))

;;;;; Reducción a representante canónico de la clase de equivalencia.

(define reducir (lambda (r)
                  (((esmayoroigualnat (primero r)) (segundo r))
                        ((par ((restanat (primero r)) (segundo r))) zero)
                        ((par zero) ((restanat (segundo r)) (primero r))))))

;;;;; Aritmética entera. La respuesta está siempre dada por el representante canónico de la clase de equivalencia.

(define testenteros (lambda (r)
                      (- (comprobar (primero r)) (comprobar (segundo r)))))

(define sument (lambda (r)
                  (lambda (s)
                    (reducir ((par ((sumnat (primero r)) (primero s))) ((sumnat (segundo r)) (segundo s)))))))

(define prodent (lambda (r)
                  (lambda (s)
                    (reducir ((par ((sumnat ((prodnat (primero r)) (primero s))) ((prodnat (segundo r)) (segundo s))))
                          ((sumnat ((prodnat (primero r)) (segundo s))) ((prodnat (segundo r)) (primero s))))))))

(define restaent (lambda (r)
                   (lambda (s)
                     (reducir ((par ((sumnat (primero r)) (segundo s))) ((sumnat (segundo r)) (primero s)))))))

;; Lo siguiente reduce la división de enteros a división de naturales. Si m \ge 0 y n> 0, y si q y r son cociente y resto de la división de m entre n, se tiene
;;  m  = q       * n        + r
;;  m  = (-q)    * (-n)     + r
;; -m  = (-(q+1))* n        + (n-r)
;; -m  = (q+1)   * (-n)     + (n-r),
;; siempre y cuando el resto no sea cero. Cuando el divisor es cero, la función cocienteent devuelve false.

(define cocienteent_aux (lambda (r)
                          (lambda (s)
                            ((cocientenat (primero (absoluto r))) (primero (absoluto s))))))

; Caso1: resto cero. Si m= q*n, entonces -m= (-q)*n, -m = q* (-n) y m= (-q)*(-n).

(define cocienteentaux-caso1 (lambda (r)
                               (lambda (s)
                                  ((or (and ((esmayoroigualent r) cero) (positivo s)) (and (negativo r) (negativo s))) ((par ((cocientenat (primero (absoluto r))) (primero (absoluto s)))) zero)
                                                                                                                       ((par zero) ((cocientenat (primero (absoluto r))) (primero (absoluto s))))))))

; Caso 2: resto no nulo

(define cocienteentaux-caso2 (lambda (r)
                                (lambda (s)
                                    (((esmayoroigualent r) cero) ((positivo s) ((par ((cocienteent_aux r) s)) zero) ((par zero) ((cocienteent_aux r) s)))
                                                                 ((positivo s) ((par zero) (sucesor ((cocienteent_aux r) s))) ((par (sucesor ((cocienteent_aux r) s))) zero))))))
; Cociente cuando no hay división por cero

(define cocienteentaux (lambda (r)
                         (lambda (s)
                           ((escero ((restonat (primero (absoluto r))) (primero (absoluto s)))) ((cocienteentaux-caso1 r) s) ((cocienteentaux-caso2 r) s)))))

; Cociente considerando la división por cero

(define cocienteent (lambda (r)
                      (lambda (s)
                        (((esceroent s) (lambda (no_use) false) (lambda (no_use) ((cocienteentaux r) s))) zero))))

; Resto. Si se divide por cero, devuelve false

(define restoentaux1 (lambda (r)
                        (lambda (s)
                          ((or (and ((esmayoroigualent r) cero) (positivo s)) (and ((esmayoroigualent r) cero) (negativo s))) ((par ((restonat (primero (absoluto r))) (primero (absoluto s)))) zero)
                                                                                                           ((par ((restanat (primero (absoluto s)))((restonat (primero (absoluto r))) (primero (absoluto s))))) zero)))))

(define restoentaux (lambda (r)
                       (lambda (s)
                          ((escero ((restonat (primero (absoluto r))) (primero (absoluto s)))) cero ((restoentaux1 r) s)))))

(define restoent (lambda (r)
                      (lambda (s)
                        (((esceroent s) (lambda (no_use) false) (lambda (no_use) ((restoentaux r) s))) zero))))

;; Como mcd (r,s)=mcd(|r|,|s|), se tiene

(define mcdent (lambda (r)
                 (lambda (s)
                   ((par ((mcdnat (primero (absoluto r))) (primero (absoluto s)))) zero))))

;;; Base canónica Zp n <- Entero, p <- Base
(define nmodp (lambda (n)
                    (lambda (p)
                        ((restoent n) p))))

(define enteromodp (lambda (n)
                    (lambda (p)
                        ((basemod ((nmodp n) p)) p))))

(define basemod (lambda (n)
                    (lambda (p)
                        ((par n) p))))

(define redumod (lambda (n)
                  ((nmodp (primero n)) (segundo n))))

(define testmod (lambda (n)
                (testenteros (primero n))))

;; Tanto en sumamod como en prodmod habría que comrpobar si la base es igual
;; pero no sé cómo devolver error. Tomo como base la base del primer miembro
(define sumamod (lambda (n)
                    (lambda (m)
                        ((basemod ((nmodp ((sument (primero n)) (primero m))) (segundo n))) (segundo n))
                    )))
(define restamod (lambda (n)
                    (lambda (m)
                        ((basemod ((nmodp ((restaent (primero n)) (primero m))) (segundo n))) (segundo n))
                    )))

(define prodmod (lambda (n)
                    (lambda (m)
                        ((basemod ((nmodp ((prodent (primero n)) (primero m))) (segundo n))) (segundo n))
                    )))

(define divmod (lambda (n)
                    (lambda (m)
                        ((basemod ((nmodp ((cocienteent (primero n)) (primero m))) (segundo n))) (segundo n))
                    )))

; INVERSO (Devuelve el inverso en la base canónica de Zp)
(define inverso (lambda (n)
                        ((basemod
                        ((positivo (primero n))
                            ((restaent (redumod n)) (segundo n))
                            (redumod n)
                        )) (segundo n))))

(define negativomod (lambda (n)
        ((enteromodp ((restaent cero) (primero n))) (segundo n))
    ))

; (testmod ((sumamod ((basemod dos) cinco)) ((basemod tres) cinco)))
; (testmod ((restamod ((basemod dos) cinco)) ((basemod tres) cinco)))
; (testmod ((prodmod ((basemod dos) cinco)) ((basemod tres) cinco)))
; (testmod (inverso ((basemod -dos) cinco)))
; (testmod (inverso ((basemod tres) cinco)))

;Definición de  matrices
(define testmatrices (lambda (m)
      (list (list (testenteros (primero (primero m))) (testenteros (segundo (primero m))))
            (list (testenteros (primero (segundo m))) (testenteros (segundo (segundo m))))
       )
    )
)

(define testmatrizmod (lambda (m)
      (list (list (testmod (primero (primero m))) (testmod (segundo (primero m))))
            (list (testmod (primero (segundo m))) (testmod (segundo (segundo m))))
       )
    )
)
(define matriz (lambda (a)
       (lambda (b)
           (lambda (c)
             (lambda (d)
               ((par ((par a) b)) ((par c) d)))))))

(define identidad ((((matriz uno) cero) cero) uno))
(define matriz_nula ((((matriz cero) cero) cero) cero))
(define matriz_prueba1 ((((matriz dos) cuatro) -uno) cinco))
(define matriz_prueba2 ((((matriz uno) dos) dos) tres))
(define matriz_prueba3 ((((matriz dos) uno) -tres) dos))
(define matriz_prueba4 ((((matriz uno) -tres) cero) cuatro))

(define matrizmod (lambda (a)
        (lambda (b)
            (lambda (c)
                (lambda (d)
                    (lambda (p)
                    ((par ((par ((enteromodp a) p)) ((enteromodp b) p)))
                    ((par ((enteromodp c) p)) ((enteromodp d) p)))))))))

(define matrizentmod (lambda (m)
        (lambda (p)
            (((((matrizmod  (primero (primero m))) (segundo (primero m)))
                            (primero (segundo m))) (segundo (segundo m))) p))))

(define summatrizmod (lambda (m)
        (lambda (n)
            ((((matriz
                ((sumamod  (primero (primero m))) (primero (primero n))))
                ((sumamod  (segundo (primero m))) (segundo (primero n))))
                ((sumamod  (primero (segundo m))) (primero (segundo n))))
                ((sumamod  (segundo (segundo m))) (segundo (segundo n))))
        )))

(define prodmatrizmod (lambda (m)
        (lambda (n)
        ((((matriz
            ((sumamod   ((prodmod (primero (primero m)))  (primero (primero n))))
                        ((prodmod (segundo (primero m)))  (primero (segundo n)))))
            ((sumamod   ((prodmod (primero (primero m)))  (segundo (primero n))))
                        ((prodmod (segundo (primero m)))  (segundo (segundo n)))))
            ((sumamod   ((prodmod (primero (segundo m)))  (primero (primero n))))
                        ((prodmod (segundo (segundo m)))  (segundo (primero n)))))
            ((sumamod   ((prodmod (primero (segundo m)))  (segundo (primero n))))
                        ((prodmod (segundo (segundo m)))  (segundo (segundo n)))))
        )))

(define detmatrizmod (lambda (m)
        ((restamod ((prodmod (primero (primero m))) (segundo (segundo m))))
        ((prodmod (segundo (primero m))) (primero (segundo m))))
    ))

;; Hay que buscar una forma menos cutre de meter el determinante en todos los elementos
(define inversaaux (lambda (m)
            ((((matriz
                ((divmod  (segundo (segundo m))) (detmatrizmod m)))
                ((divmod  (negativomod (segundo (primero m)))) (detmatrizmod m)))
                ((divmod  (negativomod (primero (segundo m)))) (detmatrizmod m)))
                ((divmod  (primero (primero m))) (detmatrizmod m)))
    ))

(define inversa (lambda (m)
        ((esceroent (primero (detmatrizmod m))) m (inversaaux m))
    ))

(testmatrizmod (inversa ((matrizentmod identidad) cinco)))
;((lambda (m) (testmod m))(detmatrizmod ((matrizentmod matriz_prueba2) cinco)))
