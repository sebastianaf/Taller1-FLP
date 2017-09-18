#lang racket
(require test-engine/racket-tests)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;[PUNTO 1];;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;calcularArea: number number symbol --> number
;Calcula el area de un cuadrado, triangulo o rectangulo
(define calcularArea
  (lambda (base altura forma)                             
    (cond [(equal? forma 'cuadrado) (expt base 2)]
          [(equal? forma 'triangulo) (/ (* base altura) 2)]
          [(equal? forma 'rectangulo) (* base altura)]
          [else 0])))

;;Pruebas
(check-expect (calcularArea 2 0 'cuadrado) 4)
(check-expect (calcularArea 4 5 'triangulo) 10)
(check-expect (calcularArea 10 2 'rectangulo) 20)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;[PUNTO 2];;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;calcularFibonacci: number --> number
;Retorna el valor correspondiente de la serie fibonacci en la posición que se le paso supiendo que es un numero
(define calcularFibonacci
  (lambda (numero)
    (if (> numero 1) (+ (calcularFibonacci (- numero 1))
                        (calcularFibonacci (- numero 2)))
        (if (= numero 0) 0
            (if (= numero 1) 1
                (printf "error"))))))

;;Pruebas
(check-expect (calcularFibonacci 0) 0)
(check-expect (calcularFibonacci 6) 8)
(check-expect (calcularFibonacci 9) 34)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;[PUNTO 3];;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
            [else "Error de escritura"]))))

;;Pruebas
(check-expect (calculadora (list '* 10 (list '+ 2 3) 4)) 200)
(check-expect (calculadora (list '/ (list '+ 5 2 3) 5)) 2)
(check-expect (calculadora (list '- (list '* 3 3 2) 8)) 10)
(check-expect (calculadora (list - 18 8)) "Error de escritura")

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
           (operar operacion (rest lista) modulo))))))

;;Pruebas
(check-expect (operar + (list 2 3 4) 0) 9)
(check-expect (operar + (list 1 (list '- 5 2) 1) 0) 5)
(check-expect (operar * (list 3 4) 1) 12)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;[PUNTO 4A];;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;buscar: var list --> boolean
;Dada una variable y una lista retorna #t si la variable hace parte de la lista y #f en caso contrario

(define buscar
  (lambda (variable lista)
    (if (null? lista)
        #f
        (cond [(equal? variable (car lista)) #t]
              [(list? (car lista)) (buscar variable (car lista))]
              [else (buscar variable (rest lista))]))))

;;Pruebas
(check-expect (buscar 'hola (list 'hola 'chao 'buenas)) #t)
(check-expect (buscar 2 (list 10 20 55 10 15 2)) #t)
(check-expect (buscar ":)" (list ":v" ":')" "xD")) #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;[PUNTO 4B];;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;sustraer: var list --> list
;Dada una variable y una lista retorna la lista sin el elemento que sea equivalente a la variable ingresada

(define sustraer
  (lambda (variable lista)
    (if (null? lista)
        empty
        (let ([primerElementoLista (car lista)])
          (cond [(equal? variable primerElementoLista) (sustraer variable (rest lista))]
                [(list? primerElementoLista) (list (sustraer variable primerElementoLista))]
                [else (cons primerElementoLista (sustraer variable (rest lista)))])))))

;;Pruebas
(check-expect (sustraer 'hola (list 'hola 'chao 'buenas)) (list 'chao 'buenas))
(check-expect (sustraer 2 (list 10 20 55 10 15 2)) (list 10 20 55 10 15))
(check-expect (sustraer ":)" (list ":v" ":')" "xD")) (list ":v" ":')" "xD"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;[EQUIPOS DE FUTBOL];;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;[PUNTO 1];;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;partido es una estructura
;(make-partido symbol symbol number number number number number)

(define-struct partido (equipo1 equipo2 puntos1 puntos2 goles1 goles2))

;ejemplos de partidos
(define america-cali (make-partido 'america 'cali 3 0 5 0))
(define medellin-nacional (make-partido 'medellin 'nacional 1 1 2 2))
(define america-nacional (make-partido 'america 'nacional 0 3 2 3))

;definicion de una lista de partidos
(define partidos (list america-cali medellin-nacional america-nacional))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;[PUNTO 2];;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
              "Error, se encontró al menos un dato inesperado")))))

;;Pruebas
(check-expect (listarEquipos partidos)
              (list (list 'america 3) (list 'cali 0)
                    (list 'medellin 1) (list 'nacional 1)
                    (list 'america 0) (list 'nacional 3)))

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
            (sumarPuntos equipo (rest listaEquipos))))))

;;Pruebas
(check-expect (sumarPuntos 'america (listarEquipos partidos))
              3)
(check-expect (sumarPuntos 'cali (listarEquipos partidos))
              0)
(check-expect (sumarPuntos 'nacional (listarEquipos partidos))
              4)

;resumirLista: list list --> list

;Dada una lista de la forma (list (list symbol number) (list symbol number) ...)
;retorna otra de la misma forma donde no hay simbolos repetidos y el numero que
;corresponde a cada simbolo resultante es la suma de los puntajes originales
;Se usa un parametro como auxiliar, debe ser una lista empty

(define resumirLista
  (lambda (lista auxLista)
    (if (null? lista)
        empty
        (if (buscar (car (car lista)) auxLista)
            (resumirLista (rest lista) auxLista)
            (cons (list (car (car lista)) (sumarPuntos (car (car lista)) lista))
                  (resumirLista (rest lista) (cons (car (car lista)) auxLista)))))))


;;Pruebas
(check-expect (resumirLista
               (list (list 'america 3) (list 'cali 0)
                     (list 'medellin 1) (list 'nacional 1)
                     (list 'america 0) (list 'nacional 3)) empty)

              (list (list 'america 3) (list 'cali 0)
                    (list 'medellin 1) (list 'nacional 4)))


;elegirMayor: list --> symbol

;Dada  una lista de partidos retorna el equipo que obtuvo más puntos en total
;Se usa un parametro como auxiliar, debe tener la forma (cons 'symbol (cons 0 empty))

(define elegirMayor
  (lambda (lista auxMejorEquipo)
    (if (null? lista)
        (car auxMejorEquipo)
        (if (< (cadr auxMejorEquipo) (cadr (car lista)))
            (elegirMayor (rest lista) (car lista))
            (elegirMayor (rest lista) auxMejorEquipo)))))


;;Pruebas
(check-expect (elegirMayor (list (list 'barcelona 1) (list 'real 3) (list 'cali 4))
                           (list 'equiposAux 0))
              'cali)
(check-expect (elegirMayor (resumirLista (listarEquipos partidos) empty) (list 'Aux 0))
              'nacional)

;equipoMayor : list --> symbol
;Dada una lista de partidos retorna el nombre del equipo con mayor puntaje,
;en caso de que hayan varios con la misma cantidad de puntos, retorna solo uno

(define equipoMayor
  (lambda (lista)
    (elegirMayor
     (resumirLista (listarEquipos lista) empty)
     (list 'noHayEquipoMayor 0))))


;;Pruebas
(check-expect (equipoMayor partidos) 'nacional)
(check-expect (equipoMayor (list (make-partido 'real 'barcelona 3 0 6 0)
                                 (make-partido 'manchester 'liverpool 1 1 2 2)
                                 (make-partido 'chelsea 'real 3 0 2 1)
                                 (make-partido 'liverpool 'barcelona 3 0 3 0)))
              'liverpool)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;[PUNTO 3];;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;numeroEmpates: list --> num
;;comprueba apartir de una lista de partidos en cuales el primer equipo tuvo punto

(define numeroEmpates
  (lambda (listPartidos)
    (cond
      [(null? listPartidos) 0]
      [(= (partido-puntos1(car listPartidos)) 1)
       (+ 1 (numeroEmpates (cdr listPartidos)))]
      [else (numeroEmpates (cdr listPartidos))])))


;;Pruebas
(check-expect (numeroEmpates partidos)
              1)
(check-expect (numeroEmpates (list (make-partido 'asenal 'chelsea 1 1 0 0)
                                   (make-partido 'manchester 'bayer 1 1 3 3)
                                   (make-partido 'barcelona 'juventus 3 0 4 1)
                                   (make-partido 'PSG 'real 3 0 3 0)
                                   (make-partido 'liverpool 'barcelona 0 3 1 2)))
              2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;[PUNTO 4];;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;numeroGoles: list --> num
;calcula el numero de goles totales que se hicieron en una lista de partidos
(define numeroGoles
  (lambda (listPartidos)
    (cond
      [(null? listPartidos) 0]
      [(partido? (car listPartidos))
       (+ (partido-goles1(car listPartidos)) (partido-goles2(car listPartidos)) (numeroGoles (cdr listPartidos)))]
      [else 0])))

;;Pruebas
(check-expect (numeroGoles partidos)
              14)
(check-expect (numeroGoles (list (make-partido 'asenal 'chelsea 1 1 0 0)
                                   (make-partido 'manchester 'bayer 1 1 3 3)
                                   (make-partido 'barcelona 'juventus 3 0 5 1)
                                   (make-partido 'PSG 'real 3 0 3 0)
                                   (make-partido 'liverpool 'chelsea 0 3 1 3)
                                   (make-partido 'bayer 'barcelona 0 3 0 2)
                                   (make-partido 'liverpool 'PSG 0 3 1 4)))
              26)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;[PUNTO 5];;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;puntosEquipo: list symbol --> num
;retorna los puntos que hizo un equipo apartir de una lista de partidos

(define puntosEquipo
  (lambda (listaPartidos equipo)
    (sumarPuntos equipo (listarEquipos listaPartidos))))

;;Pruebas
(check-expect (puntosEquipo partidos 'cali)0)
(check-expect(puntosEquipo partidos 'nacional)4)
(check-expect (puntosEquipo
               (list (make-partido 'real 'barcelona 0 3 1 5)
                     (make-partido 'manchester 'liverpool 1 1 4 4)
                     (make-partido 'chelsea 'real 1 1 1 1)
                     (make-partido 'liverpool 'barcelona 0 3 0 3))
               'barcelona)
              6)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;[PUNTO 6];;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;auxMasGoles: list num --> num
;funcion auxiliar de masGoles que usa un num como carry de comparacion
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
                  (auxMasGoles (cdr listPartidos) num)))))))

;calcula el numero mas grande de goles entre todos los partidos de parte de un equipo
;mas Goles: list --> num
(define masGoles
  (lambda (listPartidos)
  (auxMasGoles listPartidos 0)))

;;Pruebas
(check-expect (masGoles (list (make-partido 'asenal 'chelsea 1 1 0 0)
                              (make-partido 'juventus 'barcelona  3 0 7 1)
                              (make-partido 'liverpool 'chelsea 0 3 1 3)
                              (make-partido 'arsenal 'PSG 0 3 1 4)))
              7)


;Correr Pruebas
(test)