#lang racket
;#lang racket
;(require test-engine/racket-tests)

;/////////////////////////////////////////////////////////////////////////////////////////////////
; PUNTO 1
;calcularArea: number number symbol --> number
;Calcula el area de un cuadrado, triangulo o rectangulo

(define calcularArea
  (lambda (base altura forma)                             
    (cond [(equal? forma 'cuadrado) (expt base 2)]
          [(equal? forma 'triangulo) (/ (* base altura) 2)]
          [(equal? forma 'rectangulo) (* base altura)]
          [else "Se produjo un error, verifique que la forma corresponda a un triangulo, cuadrado o rectangulo"]
          )
    )
  )

;/////////////////////////////////////////////////////////////////////////////////////////////////
; PUNTO 2
;calcularFibonacci: number --> number
;Retorna el valor correspondiente de la serie fibonacci en la posición que se le paso

(define calcularFibonacci
  (lambda (numero)
    (if (> numero 1) (+ (calcularFibonacci (- numero 1))
                        (calcularFibonacci (- numero 2)))
        (if (= numero 0) 0
            (if (= numero 1) 1
                (printf "error"))))))


;/////////////////////////////////////////////////////////////////////////////////////////////////
; PUNTO 3

;calculadoa: list --> number
;ejecuta varias operaciones a partir de una lista, el primer elemento de la lista debe corresponder
;al simbolo relacionado a la operacion que se desea realizar , '+,'-,'*,'/

(define calculadora
  (lambda (lista)
    (let ([operacion (car lista)])
      (cond [(null? lista) 0]
            [(null? (rest lista)) 0]
            [(equal? operacion '+) (operar + (rest lista) 0)]
            [(equal? operacion '-) (operar - (rest lista) 0)]
            [(equal? operacion '*) (operar * (rest lista) 1)]
            [(equal? operacion '/) (operar / (rest lista) 1)]
            [else "Error de escritura"])
      )
    )    
  )

;operar: procedure list number --> number
;Dada una operacion, una lista de numeros y un numero que corresponde al modulo de la operacion que se desea organizar
;retorna un numeo que corresponde a la operacion realizada sobre los numeros de la lista

(define operar
  (lambda (operacion lista modulo)
      
    (if (empty? lista)
        modulo
        
        (let ([primerElemento (car lista)])
          (operacion
           (if (list? primerElemento)
               (calculadora primerElemento)
               primerElemento)
           (operar operacion (rest lista) modulo))
          )
        )
    )
  )


;/////////////////////////////////////////////////////////////////////////////////////////////////
; PUNTO 4

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PRIMERA PARTE (PUNTO 4)

;buscar: var list --> boolean
;Dada una variable y una lista retorna #t si la variable hace parte de la lista y #f en caso contrario

(define buscar
  (lambda (variable lista)
    (if (null? lista)
        #f
        (cond [(equal? variable (car lista)) #t]
              [(list? (car lista)) (buscar variable (car lista))]
              [else (buscar variable (rest lista))])
        )
    )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SEGUNDA PARTE (PUNTO 4)

;sustraer: var list --> list
;Dada una variable y una lista retorna la lista sin el elemento que sea equivalente a la variable ingresada

(define sustraer
  (lambda (variable lista)
    (if (null? lista)
        empty
        (let ([primerElementoLista (car lista)])
          (cond [(equal? variable primerElementoLista) (sustraer variable (rest lista))]
                [(list? primerElementoLista) (list (sustraer variable primerElementoLista))]
                [else (cons primerElementoLista (sustraer variable (rest lista)))])
          )
      )
    )
  )


;/////////////////////////////////////////////////////////////////////////////////////////////////
; EQUIPOS DE FUTBOL

;/////////////////////////////////////////////////////////////////////////////////////////////////
; PUNTO 1


;partido es una estructura
;(make-partido symbol symbol number number number number number)

(define-struct partido (equipo1 equipo2 puntos1 puntos2 goles1 goles2))

;ejemplos
(define america-cali (make-partido 'america 'cali 3 0 5 0))
(define medellin-nacional (make-partido 'nacional 'medellin 1 1 2 2))
(define america-nacional (make-partido 'america 'nacional 0 3 2 3))

(define ejeListPartidos (list america-cali medellin-nacional america-nacional))

;/////////////////////////////////////////////////////////////////////////////////////////////////
; PUNTO 2


;listarEquipos: list --> list

;Dada una lista de partidos se crea una lista de listas internas,
;las listas internas tienen la forma (cons symbol (cons number empty))
;donde el simbolo es el nombre del equipo y el numero los puntos, por partido
;En esta lista pueden ser retornados elementos en las listas con equipos repetidos
;y se retornan con la forma (list (list symbol number) (list symbol number) ...)

(define listarEquipos
  (lambda (listaPartidos)
    (if (null? listaPartidos)
        empty
        (let ([partido (car listaPartidos)])
          (if (partido? partido)
              (cons (list (partido-equipo1 partido) (partido-puntos1 partido))
                    (cons (list (partido-equipo2 partido) (partido-puntos2 partido))
                          (listarEquipos (rest listaPartidos))))
              "Error, se encontró al menos un dato inesperado")
          )
        )
    
    )
  )

;sumarPuntos: symbol list --> number

;Se ingresa un simbolo que representa el nombre de un equipo y una lista con listas
;internas donde hay equipos con sus respectivos puntos por partido, la función toma
;el simbolo dado y retorna la suma de todos los puntos que corresponden al mismo simbolo
;la lista debe tener la forma (list (list symbol number) (list symbol number) ...)

(define sumarPuntos
  (lambda (equipo listaEquipos)
    (if (null? listaEquipos)
        0
        (if (equal? equipo (car (car listaEquipos)))
            (+ (cadr (car listaEquipos)) (sumarPuntos equipo (rest listaEquipos)))
            (sumarPuntos equipo (rest listaEquipos))
            )
        )
    )
  )

;resumirLista: list list --> list

;Dada una lista de la forma (list (list symbol number) (list symbol number) ...)
;retorna otra de la misma forma donde no hay simbolos repetidos y el numero que
;corresponde a cada simbolo resultante es la suma de los puntajes originales

(define resumirLista
  (lambda (lista auxLista)
    (if (null? lista)
        empty
        (if (buscar (car (car lista)) auxLista)
            (resumirLista (rest lista) auxLista)
            (cons (list (car (car lista)) (sumarPuntos (car (car lista)) lista))
                  (resumirLista (rest lista) (cons (car (car lista)) auxLista))))
        )
    )
  )

;elegirMayor: list --> symbol

;Dada  una lista de partidos retorna el equipo que obtuvo más puntos en total

(define elegirMayor
  (lambda (lista auxMejorEquipo)
    (if (null? lista)
        (car auxMejorEquipo)
        (if (< (cadr auxMejorEquipo) (cadr (car lista)))
            (elegirMayor (rest lista) (car lista))
            (elegirMayor (rest lista) auxMejorEquipo)
            )
        )
    )
  )


;equipoMayor : list --> symbol
;Dada una lista de partidos retorna el nombre del equipo con mayor puntaje,
;en caso de que hayan varios con la misma cantidad de puntos, retorna solo uno

(define equipoMayor
  (lambda (lista)
    (elegirMayor
     (resumirLista (listarEquipos lista) empty)
     (list 'noHayEquipoMayor 0)
     )
    )
  )

;/////////////////////////////////////////////////////////////////////////////////////////////////
; PUNTO 3

;numeroEmpates: list --> num
;
;

(define numeroEmpates
  (lambda (listPartidos)
    (cond
      [(null? listPartidos) 0]
      [(= (partido-puntos1(car listPartidos)) 1)
       (+ 1 (numeroEmpates (cdr listPartidos)))]
      [else (numeroEmpates (cdr listPartidos))]
      )
    )
  )


;PUNTO 4

(define numeroGoles
  (lambda (listPartidos)
    (cond
      [(null? listPartidos) 0]
      [(partido? (car listPartidos))
       (+ (partido-goles1(car listPartidos)) (partido-goles2(car listPartidos)) (numeroGoles (cdr listPartidos)))]
      [else "algo raro pasó"]
      )
    )
  )


;PUNTO 5

(define puntosEquipo
  (lambda (listPartidos equipo)
    (cond
      [(null? listPartidos) 0]
      [(equal? (partido-equipo1 (car listPartidos)) equipo) (+ (partido-puntos1 (car listPartidos)) (puntosEquipo (cdr listPartidos) equipo))]
      [(equal? (partido-equipo2 (car listPartidos)) equipo) (+ (partido-puntos2 (car listPartidos)) (puntosEquipo (cdr listPartidos) equipo))]
      [else (puntosEquipo (cdr listPartidos) equipo)]
      )
    )
  )


;PUNTO 6
;numero mas grande de goles entre todos los partidos de parte de un equipo

(define auxMasGoles
  (lambda (listPartidos num) 
    (if (null?  listPartidos)
        num
        ( let ((goles1 (partido-goles1 (car listPartidos))) (goles2 (partido-goles2 (car listPartidos))))
          (if (>=  goles1 goles2)
              (if (> goles1 num)
                  (auxMasGoles (cdr listPartidos) goles1)
                  (auxMasGoles (cdr listPartidos) num)
                  )
              (if (> goles2 num)
                  (auxMasGoles (cdr listPartidos) goles2)
                  (auxMasGoles (cdr listPartidos) num))
                  )
           )
        )
    )
  )

(define masGoles
  (lambda (listPartidos)
  (auxMasGoles listPartidos 0)))
