#lang racket

;Revisar donde no es necesario "remove-duplicates"
;goVecindad2, libres2, libres1 (no se usó)


;Este programa juega en dos modalidades: humano vs com, com vs com

(require racket/draw)
(require slideshow)
(require racket/trace)

;memberq: elemento, lista-> si es elemento esta en la lista
;Objetivo: derterminar si un elemento pertenece a una lista
;(memberq 5 '(1 2 3 4 5 6))-> #t
;Algoritmo: si la lista es vacia, entonces #f, si no el primero es igual al elemento o el elemento esta en el resto de la lista
;nota: Modificamos a member de racket de maera que de falso o verdadero.
(define memberq (lambda(e lis) (if (list? (member e lis)) #t #f)))
;(memberq 7 '(1 2 3 4 5))


;suma->lis m->numero
;objetivo: conocer la suma de todos los elementos de una lista.
;algoritmo: si lis es nula entonces m si no suma del resto de lis y m+ el primero de la lista.
(define suma (λ(lis m) (if (null? lis) m (suma (cdr lis) (+ m (car lis))))))
  
;tablero:n-> da todas las posiciones posibles en un tablero de nxn
;objetivo: tener todas las posiciones posibles de un tablero de nxn
;build-list: n f-> lista de f(i) de i desde 0 hasta n-1
;(builld-list 3 (λ(m) (+ m 1)))-> '(1 2 3)
(define tablero (λ(n) (cartesian-product (build-list n (λ(m) (+ m 1))) (build-list n (λ(m) (+ m 1))))))

;producto: lis-> da el producto de todos los elementos de una lista.
;algoritmo: si la lista es vacia da 1, si no multiplica el primer elemento por el producto del resto de la lista.
;Hacer
;(producto '(1 2 3 4))-> 24
(define producto (λ(lis m) (if (null? lis) m (producto (cdr lis) (* m (car lis))))))

;vxy: n m-> vecindad de radio 1 de (n m) en ZxZ
;objetivo: dados n y m coloca en una lista todos los puntos vecinos de (n m).
;algoritmo: crea listas de la posición del vecino de arriba, del vecino de abajo, del vecino de la izquierda, del vecino de la derecha y de si mismo. Esto de hace creando listas que le suman o restan un uno al vertice x o y.
;ejemplo: (vxy 1 2)-> '((2 2) (0 2) (1 3) (1 1) (1 2))
(define vxy (λ(n m) (list (list (+ n 1) m) (list (- n 1) m) (list n (+ m 1)) (list n (- m 1)) (list n m))))

;goVecindad1: pos n-> vecindad de pos de radio 1 en tablero de nxn
;objetivo: tomar las coordenadas que corresponden a un tablero de tamaño nxn
;algoritmo: para cada elemento de la vecindad de una posición, toma aquellas vecindades dentro del tablero.
;(goVecindad1 '(1 2) 9)->'((2 2) (1 3) (1 1) (1 2)) 
(define goVecindad1 (λ(pos n) (filter (λ(pos) (and (< (car pos) (+ n 1)) (< (car (cdr pos)) (+ n 1)) (< 0 (car pos)) (< 0 (car (cdr pos))))) (vxy (car pos) (car (cdr pos))))))

;union: lista de listas-> unión de los elementos de la lista
;(union '((1 2 3) (4 5 6) (8 '() 10)))->'(1 2 3 4 5 6 8 '() 10)
;Objetivo: obtener en una sola lista todos los elementos de varias listas.
;algoritmo: si la lista es vacia, entoces da la lista vacia, si no une el primer elemento con la unión del resto de la lista.
;(define union (λ(lp) (remove-duplicates (if (null? lp) lp (append (car lp) (union (cdr lp)))))))
(define union (λ(lp) (remove-duplicates (apply append lp))))


;goVecindad2: pos n-> vecindad de la posición pos de radio 2 en un tablero de nxn
;objetivo: obtener la vecindad de radio 2 en una posición de un tablero de nxn
;algoritmo: quita duplicados de la union de las vecindades de radio 1 la posición dada y de cada uno de los elementos de la vecindad de radio uno de la misma posición todo en un tablero de nxn.
;(goVecindad2 '(1 2) 9)->'((2 2) (1 3) (1 1) (1 2) (3 2) (2 3) (2 1) (1 4))
;(remove-duplicates '(1 2 3 5 6 8 5 3))->'(1 2 3 5 6 8)
;old version: (define goVecindad2 (λ(pos n) (remove-duplicates (union (cons (goVecindad1 pos n) (map (λ(po) (goVecindad1 po n)) (goVecindad1 pos n)))))))
;No se ha usado
(define goVecindad2 (λ(pos n) (remove-duplicates (union  (map (λ(po) (goVecindad1 po n)) (goVecindad1 pos n))))))

;quita: lis1 lis2-> quita los elementos de la lista2 a la lista1.
;objetivo: quitar los elementos de una lista en otra lista.
;algoritmo: si la segunda lista es vacia, da la primera, si no quita el primer elemento de la segunda lista a la primera lista sin el resto de la segunda lista.
;(remove 5 '(1 5 6 9))->'(1 6 9)
;(quita  '(5 3 6 9 8 1 1) '(1 2 3))->'(5 6 9 8)
(define quita (λ(lis1 lis2) (if (null? lis2) lis1 (remove (car lis2) (quita lis1 (cdr lis2))))))

;libres2:pos bl ne  n-> de una posición toma su vecindad de radio 2 y regresa solo los espacios libres en un tablero de nxn.
;objetivo: obtener las posiciones libres en una vecindad de radio dos de una posición.
;algoritmo: a la vecindad de radio 2 quita las posiciones ocupadas por blancos, luego quita las posiciones ocupadas por los negros.
;(libres2 '(2 1) '((1 1) (5 8)) '((3 1) (4 5)) 9)->'((4 1) (2 1) (3 2) (1 2) (2 3) (2 2))
(define libres2 (λ(pos bl ne n) (quita (quita (goVecindad2 pos n) bl) ne)))

;libres1:pos bl ne  n-> de una posición toma su vecindad de radio 1 y regresa solo los espacios libres en un tablero de nxn
;objetivo: obtener las posiciones libres en una vecindad de radio uno de una posición.
;algoritmo: a la vecindad de radio 1 quita las posiciones ocupadas por blancos, luego quita las posiciones ocupadas por los negros.
;(libres1 '(2 1) '((1 1) (6 5)) '((1 2) (7 5)) 9)->'((3 1) (2 2) (2 1))
(define libres1 (λ(pos bl ne n) (quita (quita (goVecindad1 pos n) bl) ne)))

;wob: pos bl ne-> Si el elemento en la poscicion es blanco entonces da 2, si es negro da 3 y si la posición es libre entonces da 1, si es blano y negro dará 6.
;objetivo: relacionar el estado de una posicion con un número entero.
;Motivación: la idea de relacionar los estados con los números 2 o 3 es porque estos son primos y de esta manera solo será 6 cuando la posición forme parte de ambos colores.
;algoritmo(woba): si la posición esta entre los blancos, entonces da 2, si esta entre los negros, entonces da 3 , si no entonces da 1.
;algoritmo(wob): multiplica los valores de woba con la configuración de negros vacia con el woba con la configuración de blancos vacia.
;(woba '(2 1) '((1 1) (6 5)) '((2 1) (7 5)))->3
;(wob '(2 1) '((1 1) (6 5)) '((2 1) (7 5)))->3
(define woba (λ(pos bl ne) (cond ((memberq pos bl) 2) ((memberq pos ne) 3) (else 1))))
(define wob (λ(pos bl ne) (* (woba pos bl '()) (woba pos '() ne))))

;wobsig: pos bl ne n-> recibe una posición y da de el color que será en la siguiente generación de evaluación en un tablero de nxn.
;objetivo: conocer el valor que toma una posición en la siguiente generación de la evaluación.
;algoritmo: toma el producto de todos los valores de wob en la vecindad de radio 1 de la posición, se pregunta si el producto es multiplo de 2, si lo es pregunta si es multiplo de 6, si es da 6 si no sa 2, si no es múltiplo de 2 se pregunta si es múltiplo de 3 si lo es da 3 si no da 1.
;(wobsig '(2 1) '((1 1) (6 5)) '((2 1) (7 5)) 9)->6
;(wobsig '(2 1) '((1 1) (6 5)) '((7 5)) 9)->2
;(wobsig '(2 1) '((6 5)) '((7 5)) 9)->1
;los estados se refieren a 2->blanco, 3->negro, 6->de ninguno (blanco y negro a la vez), 1->espacio libre
(define wobsig (λ(pos bl ne n) ((λ(m) (cond ((integer? (/ m 2)) (if (integer? (/ m 6)) 6 2)) ((integer? (/ m 3)) 3) (else 1)))
                                (producto (map (λ(p) (wob p bl ne)) (goVecindad1 pos n)) 1))))

;generabl: genera la configuración siguiente de blancos "fantasma"
;objetivo: conocer que espacios serán blancos "fantasma" en la siguiente generación
;algoritmo: filtra a todas aquellas posiciones que no sean fichas iniciales y que su valor de wobsig sean multipo de 2.
;(generabl '((3 4)) '((1 2) (2 1)) '() '() 4)->'((2 4) (3 3) (4 4))
;(generabl '((3 4)) '((1 2) (2 1)) '((2 4) (3 3) (4 4)) '() 4)->'((1 4) (2 3) (2 4) (3 2) (3 3) (4 3) (4 4))
(define generabl (λ(bl ne esbl esne n) (filter (λ(pos) (integer? (/ (wobsig pos (append bl esbl) (append ne esne) n) 2))) (quita (tablero n) (append bl ne)))))
;generabl: genera la configuración siguiente de negros "fantasma"
;objetivo: conocer que espacios serán negros "fantasma" en la siguiente generación
;algoritmo: filtra a todas aquellas posiciones que no sean fichas iniciales y que su valor de wobsig sean multipo de 3
;(generane '((3 4)) '((1 2) (2 1)) '() '() 4)->'((1 1) (1 3) (2 2) (3 1))
(define generane (λ(bl ne esbl esne n) (filter (λ(pos) (integer? (/ (wobsig pos (append bl esbl) (append ne esne) n) 3))) (quita (tablero n) (append bl ne)))))

;evaluaaux: bl ne esbl esne n-> punto fijo del automata
;objetivo: Conocer la configuración que mantenga fija la evaluación
;algoritmo: si la siguiente generación de blancas es igual a las blancas dadas, entoces si la siguiente generación de negras es igual a las negras dadas entonces da las negras dadas y las blancas, si no evaluaaux de las blancas y la siguiente generacion de negras, si no evalua la siguiente generación de blancas y la siguiente generación de negras.
;(evaluaaux '((1 2) (2 1)) '((3 1)) '() '() 4)->'(((1 1) (1 3) (1 4) (2 2) (2 3) (2 4) (3 2) (3 3) (3 4) (4 1) (4 2) (4 3) (4 4)) ((1 3) (1 4) (2 2) (2 3) (2 4) (3 2) (3 3) (3 4) (4 1) (4 2) (4 3) (4 4)))
;bl->blancos fijos, ne->negros fijos, esbl->blancos fantasma,esne->negros fantasma, n->tamaño de tablero
(define evaluaaux (λ(bl ne esbl esne n) (let ((gbl (generabl bl ne esbl esne n)) (gne (generane bl ne esbl esne n))) (if (equal? esbl gbl) (if (equal? esne gne) (list esbl esne) (evaluaaux bl ne esbl gne n))
                                        (evaluaaux bl ne gbl gne n)))))

;evaluaaxu2:bl ne eval n-> dar los conjuntos de espacios que corresponden a cada color
;objetivo: Obtener los espacios dominados por cada jugador
;algoritmo: hace una lista con: la unión de las blancas fijas y de la primera entrada de la evaluacion quitando la segunda entrada de la evaluación, la unión de las negras fijas y la segunda entrada de la evaluación sin la primera entrada.
;(evaluaaux2 '((1 2) (2 1)) '((3 1)) '(((1 1) (1 3) (1 4) (2 2) (2 3) (2 4) (3 2) (3 3) (3 4) (4 1) (4 2) (4 3) (4 4)) ((1 3) (1 4) (2 2) (2 3) (2 4) (3 2) (3 3) (3 4) (4 1) (4 2) (4 3) (4 4))) 4)
;->'(((1 2) (2 1) (1 1)) ((3 1)))
;(define evaluaaux2 (λ(bl ne eval n) (list (append bl (quita (car eval) (second eval))) (append ne (quita (second eval)(car eval))))))


;evalua: bl ne n-> evaluación del tablero
;objetivo: conocer los espacios que corresponden a cada jugador al final de una partida
;algoritmo: hace evaluaaux2 de las blancas, negras, la evaluación se toma como el evalaux de blancas negras vacio vacio y n, y con tamaño n.
;(evalua '((1 2) (2 1)) '((3 1)) 4)->'(((1 2) (2 1) (1 1)) ((3 1)))
;(define evalua (λ(bl ne n) (evaluaaux2 bl ne (evaluaaux bl ne '() '() n) n)))

(define evalua (λ(bl ne n esbl esne) (let ((eval (evaluaaux bl ne esbl esne n))) (list (append bl (quita (car eval) (second eval))) (append ne (quita (second eval)(car eval)))))) )



;(evalua '((1 2) (2 1)) '((3 1)) 4 '() '())

(define evaluador (λ(bl ne n) (map length (evalua bl ne n '() '()))))

;(map length (evalua '((1 3) (1 4) (1 5) (2 1) (2 2) (2 4) (3 1) (3 3) (3 4) (3 6) (3 9) (4 1) (4 3) (4 4) (4 5) (4 6) (4 8) (4 9) (5 3) (5 8) (6 4) (6 5) (6 7) (6 8) (7 5) (7 6) (7 7) (8 8) (8 6) (9 7)) '((1 6) (1 8) (2 9) (2 7) (2 6) (2 5) (3 2) (3 5) (3 7) (3 8) (4 2) (4 7) (5 1) (5 2) (5 4) (5 5) (5 6) (5 7) (6 1) (6 3) (6 6) (7 2) (7 3) (7 4) (8 4) (8 5) (9 3) (9 5) (9 6)) 9))

;ILUSTRATIVO DE LA EVALUACIÓN
;orbita: bl ne esbl esne n m-> las primeras m generaciones
;objetivo: conocer las primeras m generaciones de evaluación del tablero
;algortitmo: si m es cero entonces da la lista con la unión de blancas y espacios blancos y la union de negras y espacios negros, si no construye la lista con la lista de la union de blancas y espacios blancos y la union de negros y espacios negros y la orbita de blancos, negros la siguiente evaluacion de blancos, la siguiente evaluacion de negros n y m-1
;(orbitago '((1 2) (2 1)) '((3 1)) '() '() 4 2)->'((((1 2) (2 1)) ((3 1))) (((1 2) (2 1) (1 1) (1 3) (2 2)) ((3 1) (3 2) (4 1))) ((1 2) (2 1) (1 1) (1 3) (1 4) (2 2) (2 3) (3 2)) ((3 1) (2 2) (3 2) (3 3) (4 1) (4 2)))
(define orbitago (λ(bl ne esbl esne n m) (if (equal? m 0) (list (append bl esbl) (append ne esne)) (cons (list (append bl esbl) (append ne esne)) (orbitago bl ne (generabl bl ne esbl esne n) (generane bl ne esbl esne n) n (- m 1))))))

;cadena: con cad n-> la cadena en que estan las posiciones de cad
;objetivo: conocer la cadena en que esta una ficha en una configuración
;algoritmo: elimina duplicados de si es nulo la union de las vecindades de la cadena intersectadas con la configuracion al quitar la cadena, entonces la cadena, si no da la cadena de la configuración la interseccion de la vecindad de la cadena con la intersección.
;(cadena '((3 7) (4 7) (5 7) (6 6) (7 6) (8 6) (6 7)) '((6 7)) 9)->'((5 7) (6 6) (6 7) (3 7) (4 7) (8 6) (7 6))
;(define cadena (λ(con cad n) (remove-duplicates (if (null? (quita (remove-duplicates (filter (λ(pos) (memberq pos con)) (union (map (λ(pos) (goVecindad1 pos n)) cad)))) cad)) cad (cadena con (filter (λ(pos) (memberq pos con)) (union (map (λ(pos) (goVecindad1 pos n)) cad))) n)))))
(define cadena (λ(con cad n) (let ((nuevos (filter (λ(pos) (memberq pos con)) (union (map (λ(pos) (goVecindad1 pos n)) cad))))) (remove-duplicates (if (null? (quita (remove-duplicates nuevos) cad)) cad (cadena con nuevos n))))))
;(time (cadena '((3 4) (4 3) (4 5) (5 5) (5 6) (1 9)) '((5 5)) 9))


;veccad: cad n->vecindaddecadena
;objetivo: conocer la vecindad de radio 1 de una cadena.
;algoritmo: union del map de la funcion sobre pos goVecindad1 pos n, con la lista cad.
;(veccad '((1 1) (1 2) (2 2)) 9)->'((1 1) (2 1) (1 2) (1 3) (2 2) (2 3) (3 2))
(define veccad (λ(cad n) (union (map (λ(pos) (goVecindad1 pos n)) cad))))

;libertad: bl ne cad n->
;objetivo: conocer las libertades de una cadena
;algoritmo: quita ne a quita bl de elimina duplicados de la unión de las vecindades de la cadena.
;(libertad '((1 2) (2 1) (2 2)) '((1 3) (3 1)) '((1 2) (2 1) (2 2)) 9)->'((1 1) (3 2) (2 3))
(define libertad (λ(bl ne cad n) (quita (quita (union (map (λ(pos) (goVecindad1 pos n)) cad)) bl) ne)))


;libertadp: bl ne pos n-> libertades de un punto
;objetivo: conocer las libertades de una vecindad dado un punto
;algoritmo: si la posición está en los blancos entonces haz la libertad de bl ne la cadena de plancos con la posición en n, n, si la posición esta en los negros entonces la libertad de bl ne la cadena de negros con la posición en n,n, en orto caso '().
(define libertadp (λ(bl ne pos n) (cond ((memberq pos bl) (libertad bl ne (cadena bl (list pos) n) n)) ((memberq pos ne) (libertad bl ne (cadena ne (list pos) n) n)) (else '()))))
;(libertadp '((1 2) (2 1) (2 2)) '((1 3) (3 1)) '(2 1) 9)


;confcad: conf n-> configuración en cadenas
;objetivo: dada una configuracón de piezas obtener la lista de cadenas en la configuración.
;algoritmo: si conf es nulo entonces conf, si no, sea caden la cadena de cond con la primera pieza, y construye la lista con caden y confcad de quita caden a conf
(define confcad (λ(conf n) (if (null? conf) conf (let ((caden (cadena conf (list (car conf)) n))) (cons caden (confcad (quita conf caden) n))))))
;(confcad '((8 7) (9 7) (9 5) (9 3) (9 2) (5 1) (6 3) (8 3) (6 2) (7 2) (9 6) (1 2) (6 1) (7 9) (9 4) (7 7) (3 2) (8 8) (8 2) (5 2) (8 5)) 9)
;'(((9 7) (7 7) (8 8) (8 7) (9 5) (9 6) (8 5) (9 3) (9 4) (8 2) (8 3) (9 2) (6 2) (7 2) (5 1) (5 2) (6 3) (6 1)) ((1 2)) ((7 9)) ((3 2)))

;ataque: enecv pos n m-> numero
;enecv: vecindades de cadenas enemigas; pos:posición a jugar; n:tamaño del tablero; m:contador
;Objetivo: conocer el numero de grados de libertad que quita una jugada
;algoritmo: si enecv es vacio da el contador, si pos esta en el primero de enecv entonces ataque del resto con la posición y agrega uno al contador, en otro caso ataque de el resto de las vecindades.
(define ataque (λ(enecv pos n m) (cond ((null? enecv) m) ((memberq pos (car enecv)) (ataque (cdr enecv) pos n (+ m 1))) (else (ataque (cdr enecv) pos n m)))))
;(ataque '(((2 1)) ((5 4) (3 4) (4 5) (4 3)) ((3 3) (2 4))) '(2 1) 9 0)

;mata: ami ene pos n-> las piedras que mueren al hacer una jugada
;objetivo: conocer que piezas morirían si se hace una jugada
;algoritmo: quita duplicados de la unión del map de cadena ene la posicion n, de el filtro que recibe una posición y filtra con la posición es enemiga y su libertad es cero, de la lista vecindad de radio uno de pos.
(define mata (λ(ami ene pos n) (remove-duplicates (union (map (λ(po) (cadena ene (list po) n))  (filter (λ(po) (and (memberq po ene) (null? (libertadp (cons pos ami) ene po n)))) (goVecindad1 pos n)))))))
;(mata '((1 3) (2 2)) '((1 1) (1 2)) '(2 1) 9)

;matacad: enecv pos n m-> lista de muertes
;enecv: vecindades de cadenas enemigas; pos:posición a jugar; n:tamaño del tablero; m:lista parcial de muertes
;objetivo: conocer la lista de piedras que mueren al jugar la posición pos.
;algoritmo: si enecv es vacia entonces m, si pos esta en la primera vecindad de enecv y es nulo el quita pos a la primera vecindad entonces mata cad del resto de las vecindades la posicion y a m agrega los elementos de la primer cadena
;REVISAR
(define matacad (λ(enecad enecv pos n m) (cond ((or (null? enecv) (null? enecad)) m) ((and (memberq pos (car enecv)) (null? (quita (car enecv) (list pos)))) (matacad (cdr enecad) (cdr enecv) pos n (append m (car enecad)))) (else (matacad (cdr enecad) (cdr enecv) pos n m)))))

;(print "hola")

;intento fallido
(define asignnlis (λ(lis1 lis2 e m) (if (null? lis1) m (if (memberq e (car lis1)) (asignnlis (cdr lis1) (cdr lis2) e (+ m (car lis2))) (asignnlis (cdr lis1) (cdr lis2) e m)))))
;(asignnlis '(((1 2) 2 3) (2 3 4) (3 4 5)) '(1 5 9) 5 0)

;intento fallido
(define cadposj (λ(concad pos n) (let ((veccad (map (λ(cade) (quita (remove-duplicates (union (map (λ(po) (goVecindad1 po n)) cade))) cade)) concad)) (longcad (map length concad)))
                                   (asignnlis veccad longcad pos 0))))



;juegacadena; ami ene n-> lista de libertades y tamaño de cadena
;objetivo: obtener una lista que contenga listas con el tamaño y las libertades de cada cadena de la configuración amiga
;algoritmo: map de la función lista de libertades de una cadena y tamaño de la cadena, aplicado a la configuración amiga en forma de cadenas.
(define juegacadena (λ(ami ene n) (map (λ(cad) (list (libertad ami ene cad n) (length cad))) (confcad ami n))))
;(juegacadena '((1 1) (1 2) (4 4) (2 3)) '((2 2) (1 3) (5 2)) 9)->'((((2 1)) 2) (((5 4) (3 4) (4 5) (4 3)) 1) (((3 3) (2 4)) 1))


;calculo de enecv
;(map car (juegacadena '((1 1) (1 2) (4 4) (2 3)) '((2 2) (1 3) (5 2)) 9))

;'(((libertades1) n1) ((libertades2) n2) ...)

;(cadposj '(((9 7) (7 7) (8 8) (8 7) (9 5) (9 6) (8 5) (9 3) (9 4) (8 2) (8 3) (9 2) (6 2) (7 2) (5 1) (5 2) (6 3) (6 1)) ((1 2)) ((7 9)) ((3 2))) '(2 2) 9)

;intento fallido
(define jugada2 (λ(ami ene pos n) (let ((vpos (goVecindad1 pos n))) (list (cadposj (confcad ami n) pos n) (- (length (libertadp (cons pos ami) ene pos n)) (length (libertad ami ene (remove pos (cadena ami (list pos) n)) n))) (- (length (remove-duplicates (union (map (λ(po) (libertadp ami ene po n)) (filter (λ(po) (memberq po ene)) vpos)))))
                                                                                                                               (length (remove-duplicates (union (map (λ(po) (libertadp (cons pos ami) ene po n)) (filter (λ(po) (memberq po ene)) vpos))))))
                                       (length (mata ami ene pos n))))))



;jugcad: cedenn pos m-> numero
;objetvo: saber el tamaño de la cadena a la que pertenecera una piedra al jugarla
;algoritmo: si cadenn es nulo entonces m, si no si la posición perteneca al primero del primero de cadenn entonces jugcad del resto de cdr cadenn pos y m+ el segundo del primero de cadenn
;           si no jugcad del resto de cadenn pos y m.
;cadenn: '((cadena1 tamañodecadena1) (cadena2 tamañodecadena1) ...)
;(jugcad '((((1 2) (2 2) (1 1)) 3) ((3 3) 1)) '(1 1) 0)->3
;(jugcad '((((1 2) (2 2) (1 1)) 3) ((3 3) 1)) '(2 3) 0)->5
(define jugcad (λ(cadenn pos m) (if (null? cadenn) m (if (memberq pos (car (car cadenn))) (jugcad (cdr cadenn) pos (+ m (second (car cadenn)))) (jugcad (cdr cadenn) pos m)))))

;Fallido
(define jugada3 (λ(ami ene pos n) (let ((vpos (goVecindad1 pos n)) (jcad (juegacadena ami ene n)) (jcad2 (juegacadena ene ami n))) (list (jugcad jcad pos n) (- (suma (map second (juegacadena (cons pos ami) ene n)) 0) (suma (map second jcad) 0)) (- (suma (map second (juegacadena ene (cons pos ami) n)) 0) (suma (map second jcad2) 0))  (length (mata ami ene pos n))) )))

;jugadacad: ami ene pos n-> vector de jugada
;objetivo: conocer el vector de jugada para un tablero en una posición
(define jugadacad (λ(ami ene pos n enevc jcad) (let ((vpos (goVecindad1 pos n)) (jjj (jugcad jcad pos n))) (list (- jjj 1) (if (equal? jjj 1) 1 (- (suma (map second (juegacadena (cons pos ami) ene n)) 0) (suma (map second jcad) 0)))
                                                                                                                                            (ataque enevc pos n 0) (length (matacad (confcad ene n) enevc pos n '()))))))

;jugada: ami ene pos n-> vector de la jugada
;objetivo: conocer, mediante un vector, como afecta una jugada.
;vector: (tamaño de la cadena en que esta la pieza jugada; grados de libertad que aporta la jugada; grados de libertad que quita al oponente la jugada; número de muertes en la jugada)
;(jugada '((1 1) (1 2) (4 4) (2 3)) '((2 2) (1 3) (5 2)) '(2 1) 9)->'(3 0 1 0)
(define jugada (λ(ami ene pos n) (let ((vpos (goVecindad1 pos n))) (list (length (cadena (cons pos ami) (list pos) n)) (- (length (libertadp (cons pos ami) ene pos n)) (length (libertad ami ene (remove pos (cadena ami (list pos) n)) n))) (- (length (remove-duplicates (union (map (λ(po) (libertadp ami ene po n)) (filter (λ(po) (memberq po ene)) vpos)))))
                                                                                                                               (length (remove-duplicates (union (map (λ(po) (libertadp (cons pos ami) ene po n)) (filter (λ(po) (memberq po ene)) vpos))))))
                                       (length (mata ami ene pos n))))))



;juega: ami ene pos n-> tablero al jugar pos
;objetivo: conocer como será el tablero luego de hacer una jugada.
;algoritmo: si la posición el '(0 0) entonces hace la lista de ami ene, si no agrega la jugada a ami y le quita a ene el mata al jugar pos.
;(juega '((1 1) (1 2) (2 3) (3 2)) '((2 2) (1 3) (1 4) (1 5)) '(2 1) 9)->'(((2 1) (1 1) (1 2) (2 3) (3 2)) ((1 3) (1 4) (1 5)))
;Nunca usado
(define juega (λ(ami ene pos n) (if (equal? pos '(0 0)) (list ami ene) (list (cons pos ami) (quita ene (mata ami ene pos n))))))


;juegaal: ami ene pos n-> tablero al jugar
;objetivo: saber como será el tablero al hacer una jugada, pero obtenerlo en orden inverso.
;algoritmo: si la posición es '(0 0) entonces da la lista de ene ami, si no la lista de quita a ene las mata al jugar pos y agrega pos a ami.
(define juegaal (λ(ami ene pos n) (if (equal? pos '(0 0)) (list ene ami) (list (quita ene (mata ami ene pos n)) (cons pos ami)))))

;Partida: jug n-> tablero
;objetivo: dado una lista de jugadas conocer el estado del tablero al realizar las jugadas.
;Algoritmo (partidaaux): si jug es nula entonces si m/2 es entero entonces tab si no lista del segundo del tablero y el primero, en otro caso, partidaaux de partida aux el tablero al jugar el primero de jug, el resto de jug, n y m+1.
;Algoritmo (partida): llama a partidaaux con la lista de vacío y vacíom jug n y 0.
(define partidaaux (λ(tab jug n m) (if (null? jug) (if (integer? (/ m 2)) tab (list (second tab) (car tab))) (partidaaux (juegaal (first tab) (second tab) (car jug) n) (cdr jug) n (+ m 1)))))

(define partida (λ(jug n) (partidaaux '(() ()) jug n 0)))

;(partida '((1 2) (1 1) (2 1) (2 2) (2 3) (3 2)) 9)
;(partida '((0 0) (1 2) (1 1) (2 1) (2 2) (2 3) (3 2)) 9)

;maxf: lis f-> lista de máximos de la función.
;objetivo: conocer los puntos donde una función toma sus puntos máximos.
;algoritmo (maxfaux): si lis es nula entonces laux, si la imagen del primero de la lis es menor que la imagen de la 
;FALLIDO
(define maxfaux (λ(lis f laux) (cond ((null? lis) laux) ((< (f (car lis)) (f (car laux))) (maxfaux (cdr lis) f laux)) (else (if (equal? (f (car lis)) (f (car laux))) (maxfaux (cdr lis) f (cons (car lis) laux))
                                                                                                                                (maxfaux (cdr lis) f (list (car lis))))))))
;FALLIDO
(define maxf (λ(lis f) (if (null? lis) lis (time (maxfaux (cdr lis) f (list (car lis)))))))

;maxfun: lis f-> lista de máximos de la función.
;objetivo: conocer los puntos donde una función toma sus puntos máximos
;algoritmo (maxpair): Si lia es nula entonces laux, si laux es vacía entonces maxpair del resto de lia, el resto de lii la lista con el primero de lia y la lista con el primero de lii, si el primero de lii es menor que el primero de lauxi entonces maxpair del resto de lia, el resto, laux y lauxi
;si el primero de lii es igual al primero de lauxi, entoces maxpair del resto de lia, el resto de lii, laux al agregarle el primero de lia y lauxi al agregarle el primero de lii, en otro caso maxpair del resto de lia, el resto de lii, la lista con el primero de lia y la lista con el primero de lii.
;Algoritmo (maxfun): maxpair con los argumentos lis, el mapeo de f en lis, vacío y vacío.

;(maxfun (quita (quita (tablero n) (append proh ami)) ene) (λ(pos) (provec vec (jugadacad ami ene pos n enecv jcad1) 0)))
;Con el ejemplo anterior dados ami(posiciones de amigos) y ene(posiciones enemigas) un vector de juego, la lista de vecindades de las cadenas enemigas (enecv) y jcad1 (el tablero en cadenas)
;además de proh (posiciones prohibidas) y el tamaño del tablero n, da las posiciones jugables con mayor valoración.
(define maxpair (λ(lia lii laux lauxi) (cond ((null? lia) laux) ((null? laux) (maxpair (cdr lia) (cdr lii) (list (car lia)) (list (car lii)))) ((< (car lii) (car lauxi)) (maxpair (cdr lia) (cdr lii) laux lauxi))
                                             ((equal? (car lii) (car lauxi)) (maxpair (cdr lia) (cdr lii) (cons (car lia) laux) (cons (car lii) lauxi))) (else (maxpair (cdr lia) (cdr lii) (list (car lia)) (list (car lii)))))))

(define maxfun (λ(lis f) (maxpair lis (map f lis) '() '())))





;Hacer en T-recurtion
;(define provec (λ(v1 v2) (if (or (null? v2) (null? v1)) 0 (+ (* (car v1) (car v2)) (provec (cdr v1) (cdr v2))))))

;provec: v1 v2 m-> numero
;objetivo: Conocer el producto vectorial entre dos vectores
;algoritmo: si v1 o v2 son vacios entonces m, si no provec del resto de v1 v2 y m+el primero de v1 por el primero de v2.
(define provec (λ(v1 v2 m) (if (or (null? v2) (null? v1)) m (provec (cdr v1) (cdr v2) (+ m (* (car v1) (car v2)))))))
;(provec '(1 2 3) '(1 2 3) 0)

;(define maxfunaux (λ(lis f) (indexes-where lis (λ(x) (equal? (f x) (apply max (map f lis)))))))
;(define maxfun (λ(lis f) (map last (map (λ(pos) (take lis (+ pos 1))) (maxfunaux lis f)))))

;(maxfun '(-3 -2 -1 0 1 2 3) (λ(x) (* x x)))


;(define valorjugada (λ(ami ene vec n pos jcad) (provec vec (jugadacad ami ene pos n (map car jcad) jcad) 0)))


;mejorjX: ami ene vec n-> mejores jugadas
;objetivo: dadas las posiciones de las piedras y el vector de juego, conocer las mejores jugadas.
;Aloritmo: maxf de quita a quita a el tablero n ami, ene, y la función provec con argumentos vec y jugadaX de ami ene pos n.
(define mejorj (λ(ami ene vec n proh) (maxf (quita (quita (tablero n) ami) ene) (λ(pos) (provec vec (jugada ami ene pos n) 0)))))
(define mejorj2 (λ(ami ene vec n) (maxf (quita (quita (tablero n) ami) ene) (λ(pos) (provec vec (jugada2 ami ene pos n) 0)))))
(define mejorj3 (λ(ami ene vec n) (maxf (quita (quita (tablero n) ami) ene) (λ(pos) (provec vec (jugada3 ami ene pos n) 0)))))
(define mejorjcad (λ(ami ene vec n) (let ((jcad1 (juegacadena ami ene n))) (maxf (quita (quita (tablero n) ami) ene) (λ(pos) (provec vec (jugadacad ami ene pos n (map car jcad1) jcad1) 0))))))
;Cuarta entrada de jugadacad
;(define mejorjcad2 (λ(ami ene vec n proh) (let ((jcad1 (juegacadena ami ene n))) (maxfun (quita (quita (tablero n) (append proh ami)) ene) (λ(pos) (provec vec (jugadacad ami ene pos n (map car jcad1) jcad1) 0))))))
;Revisar
(define mejorjcad2 (λ(ami ene vec n proh) (let ((jcad1 (juegacadena ami ene n)) (enecv (map (λ(cad) (remove-duplicates (quita (veccad cad n) (append ami ene)))) (confcad ene n)))) (maxfun (quita (quita (tablero n) (append proh ami)) ene) (λ(pos) (provec vec (jugadacad ami ene pos n enecv jcad1) 0))))))

;(juegacadena '((1 1) (1 2) (3 2) (5 5) (6 5) (9 9)) '((1 9) (1 8) (2 5) (2 6) (3 5) (9 1)) 9 )
;(let ((jcad1 (juegacadena '((1 1) (1 2) (3 2) (5 5) (6 5) (9 9)) '((1 9) (1 8) (2 5) (2 6) (3 5) (9 1)) 9 ))) (map car jcad1))
;(map (λ(cad) (remove-duplicates (quita (quita (veccad cad 9) '((1 1) (1 2) (3 2) (5 5) (6 5) (9 9))) '((1 9) (1 8) (2 5) (2 6) (3 5) (9 1))))) (confcad '((1 9) (1 8) (2 5) (2 6) (3 5) (9 1)) 9))



;'(((2 8) (1 9) (1 7) (1 8) (2 9) (1 8) (1 9)) ((4 5) (2 5) (3 6) (3 4) (3 5) (3 6) (1 6) (2 7) (2 5) (2 6) (3 5) (1 5) (2 6) (2 4) (2 5)) ((8 1) (9 2) (9 1)))


;Intentos fallidos de "Mejorjuegocad2"
(define mejorjuego (λ(ami ene vec n) (car (shuffle (mejorj ami ene vec n)))))
(define mejorjuego2 (λ(ami ene vec n) (car (shuffle (mejorj2 ami ene vec n)))))
(define mejorjuego3 (λ(ami ene vec n) (car (shuffle (mejorj3 ami ene vec n)))))
(define mejorjuegocad (λ(ami ene vec n) (car (shuffle (mejorjcad ami ene vec n)))))

;mejorjuegocad2: ami ene vec n proh -> pos
;objetivo: obtener una posición que tenga evaluación máxima, si hay varios toma uno al azar
;algoritmo: el primero del shuffle de mejorcad2 de ami ene vec n proh
(define mejorjuegocad2 (λ(ami ene vec n proh) (car (shuffle (mejorjcad2 ami ene vec n proh)))))

;Intentos fallidos del programa "mejorCOLORauxcad2"
(define mejornegraaux (λ(parti vec n) (mejorjuego (car parti) (second parti) vec n)))
(define jueganegra (λ(jug vec n) (mejornegraaux (partida jug n) vec n)))
(define mejorblancaaux (λ(parti vec n) (mejorjuego (second parti) (car parti) vec n)))
(define juegablanca (λ(jug vec n) (mejorblancaaux (partida jug n) vec n)))

(define mejornegraaux2 (λ(parti vec n) (mejorjuego2 (car parti) (second parti) vec n)))
(define jueganegra2 (λ(jug vec n) (mejornegraaux2 (partida jug n) vec n)))
(define mejorblancaaux2 (λ(parti vec n) (mejorjuego2 (second parti) (car parti) vec n)))
(define juegablanca2 (λ(jug vec n) (mejorblancaaux2 (partida jug n) vec n)))

(define mejornegraaux3 (λ(parti vec n) (mejorjuego3 (car parti) (second parti) vec n)))
(define jueganegra3 (λ(jug vec n) (mejornegraaux3 (partida jug n) vec n)))
(define mejorblancaaux3 (λ(parti vec n) (mejorjuego3 (second parti) (car parti) vec n)))
(define juegablanca3 (λ(jug vec n) (mejorblancaaux3 (partida jug n) vec n)))

(define mejornegraauxcad (λ(parti vec n) (mejorjuegocad (car parti) (second parti) vec n)))
(define jueganegracad (λ(jug vec n) (mejornegraauxcad (partida jug n) vec n)))
(define mejorblancaauxcad (λ(parti vec n) (mejorjuegocad (second parti) (car parti) vec n)))
(define juegablancacad (λ(jug vec n) (mejorblancaauxcad (partida jug n) vec n)))

;mejorCOLORauxcad2: parti vec n proh-> pos
;mejorCOLORcad2: jug vec n proh->pos
;objetivo dada el tablero como historial de jugada y un vector obener una jugada de valor máximo.
;algortimo (aux): mejorcad del primero de parti, el segundo de parti (<- en el caso de las blancas estos se invierten) el vector n y proh
;algoritmo: mejorCOLORauxcad2 partida de jug n, vector n y proh
(define mejornegraauxcad2 (λ(parti vec n proh) (mejorjuegocad2 (car parti) (second parti) vec n proh)))
(define jueganegracad2 (λ(jug vec n proh) (mejornegraauxcad2 (partida jug n) vec n proh)))
(define mejorblancaauxcad2 (λ(parti vec n proh) (mejorjuegocad2 (second parti) (car parti) vec n proh)))
(define juegablancacad2 (λ(jug vec n proh) (mejorblancaauxcad2 (partida jug n) vec n proh)))

;Ejemplo:
;(mejorblancaauxcad2 '(((1 3)(1 3) (8 6)(1 9)(7 4)(3 9)(5 9) (4 8) (2 2) (2 8) (1 7) (4 2) (2 1) (6 8) (2 9) (3 3) (3 7) (4 1) (4 7) (1 5) (2 4) (2 6) (7 3)(1 6) (8 4) (4 5) (3 5) (5 7)
;  (4 6) (5 3) (7 5) (6 6) (4 4) (6 4) (5 5))
;   ((8 7) (9 7) (9 5) (9 3) (9 2) (5 1) (6 3) (8 3) (6 2) (7 2) (9 6) (1 2) (6 1) (7 9) (9 4) (7 7) (3 2) (8 8) (8 2) (5 2) (8 5))) '(3 4 4.5 7) 9 '())
;->'(7 6)

;(juegablancacad2 '((5 5) (6 5) (6 4) (5 6) (4 4) (7 4) (6 6) (3 4) (7 5) (8 5) (5 3) (5 2) (4 6)
;                   (3 6) (5 7) (5 8) (3 5) (2 5) (4 5) (8 2) (8 4) (2 7) (1 6) (8 8) (7 3) (3 2)
;                   (2 6) (7 7) (2 4) (2 3) (1 5) (3 8) (4 7) (4 3) (4 1) (9 4) (3 7) (7 9) (3 3) (1 8)
;                   (2 9) (6 1) (6 8) (1 2) (2 1) (9 6) (4 2) (4 9) (1 7) (7 2) (2 8) (6 2) (2 2) (8 3)
;                   (4 8) (6 3) (5 9) (5 1) (3 9) (9 2) (7 4) (9 3) (1 9) (9 5) (8 6)  (9 7) (1 3)  (8 7) (1 3)) '(3 4 4.5 7) 9 '())
;->'(7 6)







;(time (juegablanca2 '((4 4) (3 4) (3 5) (2 5) (3 3) (5 4) (2 4) (3 6) (2 6) (2 7) (1 5) (3 2) (2 8) (3 8) (1 7) (4 5) (3 7) (4 7) (4 6)
;                     (5 6) (5 5) (6 5) (3 9) (4 5) (3 6) (2 3) (3 4) (4 9) (4 8) (5 8) (5 9) (4 3) (1 3) (1 2) (2 2) (6 9) (5 7) (4 9) (5 5)
;                     (5 3)  (7 6) (4 2) (6 4) (5 2) (6 3) (7 5) (6 2) (6 1) (6 7) (7 4)) '(1 0.5 5 5) 9))


;(time (juegablanca2 '((5 5) (6 5) (6 4) (5 6) (4 4) (7 4) (6 6) (3 4) (7 5) (8 5) (5 3) (5 2) (4 6)
;                   (3 6) (5 7) (5 8) (3 5) (2 5) (4 5) (8 2) (8 4) (2 7) (1 6) (8 8) (7 3) (3 2)
;                   (2 6) (7 7) (2 4) (2 3) (1 5) (3 8) (4 7) (4 3) (4 1) (9 4) (3 7) (7 9) (3 3) (1 8)
;                   (2 9) (6 1) (6 8) (1 2) (2 1) (9 6) (4 2) (4 9) (1 7) (7 2) (2 8) (6 2) (2 2) (8 3)
;                   (4 8) (6 3) (5 9) (5 1) (3 9) (9 2) (7 4) (9 3) (1 9) (9 5) (8 6)) '(3 4 4.5 7) 9))

;(time (juegablanca '((5 5) (6 5) (6 4) (5 6) (4 4) (7 4) (6 6) (3 4) (7 5) (8 5) (5 3) (5 2) (4 6)
;                   (3 6) (5 7) (5 8) (3 5) (2 5) (4 5) (8 2) (8 4) (2 7) (1 6) (8 8) (7 3) (3 2)
;                   (2 6) (7 7) (2 4) (2 3) (1 5) (3 8) (4 7) (4 3) (4 1) (9 4) (3 7) (7 9) (3 3) (1 8)
;                   (2 9) (6 1) (6 8) (1 2) (2 1) (9 6) (4 2) (4 9) (1 7) (7 2) (2 8) (6 2) (2 2) (8 3)
;                   (4 8) (6 3) (5 9) (5 1) (3 9) (9 2) (7 4) (9 3) (1 9) (9 5) (8 6)  (9 7) (1 3)) '(3 4 4.5 7) 9))

;(time (juegablanca2 '((5 5) (6 5) (6 4) (5 6) (4 4) (7 4) (6 6) (3 4) (7 5) (8 5) (5 3) (5 2) (4 6)
;                   (3 6) (5 7) (5 8) (3 5) (2 5) (4 5) (8 2) (8 4) (2 7) (1 6) (8 8) (7 3) (3 2)
;                   (2 6) (7 7) (2 4) (2 3) (1 5) (3 8) (4 7) (4 3) (4 1) (9 4) (3 7) (7 9) (3 3) (1 8)
;                   (2 9) (6 1) (6 8) (1 2) (2 1) (9 6) (4 2) (4 9) (1 7) (7 2) (2 8) (6 2) (2 2) (8 3)
;                   (4 8) (6 3) (5 9) (5 1) (3 9) (9 2) (7 4) (9 3) (1 9) (9 5) (8 6)  (9 7) (1 3)) '(3 4 4.5 7) 9))


;cpu time: 74906 real time: 75005 gc time: 25204
;'(8 7)

;(time (juegablancacad '((5 5) (6 5) (6 4) (5 6) (4 4) (7 4) (6 6) (3 4) (7 5) (8 5) (5 3) (5 2) (4 6)
;                   (3 6) (5 7) (5 8) (3 5) (2 5) (4 5) (8 2) (8 4) (2 7) (1 6) (8 8) (7 3) (3 2)
;                   (2 6) (7 7) (2 4) (2 3) (1 5) (3 8) (4 7) (4 3) (4 1) (9 4) (3 7) (7 9) (3 3) (1 8)
;                   (2 9) (6 1) (6 8) (1 2) (2 1) (9 6) (4 2) (4 9) (1 7) (7 2) (2 8) (6 2) (2 2) (8 3)
;                   (4 8) (6 3) (5 9) (5 1) (3 9) (9 2) (7 4) (9 3) (1 9) (9 5) (8 6)  (9 7) (1 3)) '(3 4 4.5 7) 9))

;(time (juegablancacad2 '((5 5) (6 5) (6 4) (5 6) (4 4) (7 4) (6 6) (3 4) (7 5) (8 5) (5 3) (5 2) (4 6)
;                   (3 6) (5 7) (5 8) (3 5) (2 5) (4 5) (8 2) (8 4) (2 7) (1 6) (8 8) (7 3) (3 2)
;                   (2 6) (7 7) (2 4) (2 3) (1 5) (3 8) (4 7) (4 3) (4 1) (9 4) (3 7) (7 9) (3 3) (1 8)
;                   (2 9) (6 1) (6 8) (1 2) (2 1) (9 6) (4 2) (4 9) (1 7) (7 2) (2 8) (6 2) (2 2) (8 3)
;                   (4 8) (6 3) (5 9) (5 1) (3 9) (9 2) (7 4) (9 3) (1 9) (9 5) (8 6)  (9 7) (1 3)  (8 7) (1 3)) '(3 4 4.5 7) 9))

;cpu time: 95125 real time: 93066 gc time: 27991
;'(8 7)

;(juegablanca3 '((5 5) (6 5) (6 4) (5 6) (4 4) (7 4) (6 6) (3 4) (7 5) (8 5) (5 3) (5 2) (4 6)
;                   (3 6) (5 7) (5 8) (3 5) (2 5) (4 5) (8 2) (8 4) (2 7) (1 6) (8 8) (7 3) (3 2)
;                   (2 6) (7 7) (2 4) (2 3) (1 5) (3 8) (4 7) (4 3) (4 1) (9 4) (3 7) (7 9) (3 3) (1 8)
;                   (2 9) (6 1) (6 8) (1 2) (2 1) (9 6) (4 2) (4 9) (1 7) (7 2) (2 8) (6 2) (2 2) (8 3)
;                   (4 8) (6 3) (5 9) (5 1) (3 9) (9 2) (7 4) (9 3) (1 9) (9 5) (8 6) (9 7) (1 3) (8 7) (3 1)) '(3 4 4.5 7) 9)

;(define par (partida '((5 5) (6 5) (6 4) (5 6) (4 4) (7 4) (6 6) (3 4) (7 5) (8 5) (5 3) (5 2) (4 6)
;                   (3 6) (5 7) (5 8) (3 5) (2 5) (4 5) (8 2) (8 4) (2 7) (1 6) (8 8) (7 3) (3 2)
;                   (2 6) (7 7) (2 4) (2 3) (1 5) (3 8) (4 7) (4 3) (4 1) (9 4) (3 7) (7 9) (3 3) (1 8)
;                   (2 9) (6 1) (6 8) (1 2) (2 1) (9 6) (4 2) (4 9) (1 7) (7 2) (2 8) (6 2) (2 2) (8 3)
;                   (4 8) (6 3) (5 9) (5 1) (3 9) (9 2) (7 4) (9 3) (1 9) (9 5) (8 6) (9 7) (1 3)) 9))

;(juegacadena (car par) (cdr par) 9)

;(juegacadena (car par) (second par) 9)

;(define libres (quita (quita (tablero 9) (car par)) (second par)))

;(map (λ(po) (jugcad '((((2 3) (1 4)) 1)
;  (((7 6) (8 7)) 1)
;  (((4 9) (3 8) (1 8) (2 7)) 4)
;  (((5 4) (6 5) (7 6)) 5)
;  (((6 9) (4 9) (5 8)) 1)
;  (((6 7) (5 8) (5 6) (2 7) (3 8) (3 6) (4 9) (6 5) (5 4) (2 5) (3 4) (4 3)) 9)
;  (((3 1) (1 1) (2 3)) 2)
;  (((3 6) (2 7) (2 5) (1 8) (1 4)) 4)
;  (((3 1) (4 3)) 2)
;  (((7 8) (5 8) (6 9) (6 7)) 1)
;  (((4 3) (2 3) (3 4)) 1)
;  (((3 4) (1 4) (2 5) (2 3)) 1)
;  (((4 3) (5 4)) 1)
;  (((7 6) (5 6) (6 7) (6 5)) 1)) po 9)) libres)

;(map (λ(po) (- (length (libertadp (cons po (car par)) (second par) po 9)) (length (libertad (car par) (second par) (remove po (cadena (car par) (list po) 9)) 9)))) libres)

;(map (λ(po) (length (mata (car par) (second par) po 9))) libres)

;(map (λ(po) (- (suma (map second (juegacadena (second par) (cons po (car par)) 9)) 0) (suma (map second (juegacadena (second par) (car par) 9)) 0))) libres)

;(define libres (quita (quita (tablero 9) (car par)) (second par)))
;(map (λ(po) (jugada (second par) (car par) po 9)) libres)


;(confcad (second par) 9)
;(quita (quita (tablero 9) (car par)) (second par))
;(time (map (λ(po) (cadposj (confcad (second par) 9) po 9)) (quita (quita (tablero 9) (car par)) (second par))))

;(trace jueganegra mejornegraaux mejorjuego mejorj maxf maxfaux quita provec jugada mata cadena memberq union libertadp goVecindad1 vxy)
;(jueganegra '((5 5) (6 5) (6 4) (5 6) (4 4) (7 4) (6 6) (3 4) (7 5) (8 5) (5 3) (5 2) (4 6)
 ;                  (3 6) (5 7) (5 8) (3 5) (2 5) (4 5) (8 2) (8 4) (2 7) (1 6) (8 8) (7 3) (3 2)
 ;                  (2 6) (7 7) (2 4) (2 3) (1 5) (3 8) (4 7) (4 3) (4 1) (9 4) (3 7) (7 9) (3 3) (1 8)
 ;                  (2 9) (6 1) (6 8) (1 2) (2 1) (9 6) (4 2) (4 9) (1 7) (7 2) (2 8) (6 2) (2 2) (8 3)
 ;                  (4 8) (6 3) (5 9) (5 1) (3 9) (9 2) (7 4) (9 3) (1 9) (9 5) (8 6) (9 7) (1 3)) '(3 4 4.5 7) 9)


;(trace jueganegra mejornegraaux mejorjuego mejorj maxf maxfaux quita provec jugada mata cadena memberq union libertadp goVecindad1 vxy)
;(jueganegra '((5 5) (6 5) (6 4) (5 6) (4 4) (7 4) (6 6) (3 4) (7 5) (8 5) (5 3) (5 2) (4 6)
;                   (3 6) (5 7) (5 8) (3 5) (2 5) (4 5) (8 2) (8 4) (2 7) (1 6) (8 8) (7 3) (3 2)
;                   (2 6) (7 7) (2 4) (2 3) (1 5) (3 8) (4 7) (4 3) (4 1) (9 4) (3 7) (7 9) (3 3) (1 8)
;                   (2 9) (6 1) (6 8) (1 2) (2 1) (9 6) (4 2) (4 9) (1 7) (7 2) (2 8) (6 2) (2 2) (8 3)
;                   (4 8) (6 3) (5 9) (5 1) (3 9) (9 2) (7 4) (9 3) (1 9) (9 5) (8 6) (9 7) (1 3) (8 7)) '(3 4 4.5 7) 9)


;(time (map (λ(po) (length (cadena (cons po (second par)) (list po) 9))) (quita (quita (tablero 9) (car par)) (second par))))
;(time (map (λ(po) (- (length (libertadp (cons po (second par)) (car par) po 9)) (length (libertad (second par) (car par) (remove po (cadena (second par) (list po) 9)) 9)))) (quita (quita (tablero 9) (car par)) (second par))))
;(time (map (λ(po1) (- (length (remove-duplicates (union (map (λ(po) (libertadp (second par) (car par) po 9)) (filter (λ(po) (memberq po (car par))) (goVecindad1 po1 9))))))
;             (length (remove-duplicates (union (map (λ(po) (libertadp (cons po1 (second par)) (car par) po 9)) (filter (λ(po) (memberq po (car par))) (goVecindad1 po1 9)))))))) (quita (quita (tablero 9) (car par)) (second par))))
;(time (map (λ(po) (length (mata (second par) (car par) po 9))) (quita (quita (tablero 9) (car par)) (second par))))

;agregar suicidio y ku al juego

;suicidio: ami ene pos n-> bol
;objetivo: saber si una jugada es suicidio
;algoritmo: es nula libertasp de la lista de ami con por, ene pos y n, y es nulo el mata de ami ene pos n.
;si no tiene libertades y no mata es suicidio.
;(suicidio '((1 2) (8 9)) '((2 3) (6 8)) '(9 9) 9)
(define suicidio (λ(ami ene pos n) (and (null? (libertadp (cons pos ami) ene pos n)) (null? (mata ami ene pos n)))))

;pasa: ami ene pos vec n-> bol
;objetivo: saber si la mejor jugada es una buena jugada.
;algoritmo: sea jcad1 el juegacadena de ami ene n, si el provec de vec y jugadacad de ami ene pos n el map car de jcad1, jcad1, es negativo, entonces '(0 0) si no pos
;la mejor jugada es una buena jugada si su valor de jugada con el vector es positivo, lo que implica que esta haciendo defensa o esta atacando.
;(pasa '((1 2) (8 9)) '((2 3) (6 8)) '(9 9) '(1 0.5 9 11) 9)
(define pasa (λ(ami ene pos vec n) (let ((jcad1 (juegacadena ami ene n))) (if (< (provec vec (jugadacad ami ene pos n (map car jcad1) jcad1) 0) 0) '(0 0) pos))))

;(define koaux (λ(tab pos n) (if (not (memberq pos jug)) #f (memberq (juega ())))))

;listasluegode: lis e-> listani
;objetivo: conocer todas las sub listas luego de la aparición de un elemento.
;algoritmo sea k el index-of de lis e, map de la función take lis m, con variable m, en la lista desde la longitud de la lista menos k más uno hasta la longitud de la lista.
;la idea de hacer esto es conocer todos los tableros ocurridos luego de jugar una posición especifica.
(define listasluegode (λ(lis e) (let ((k (index-of lis e))) (map (λ(m) (take lis m)) (build-list (- (length lis) k) (λ(x) (+ x k 1)))))))
;(listasluegode '(1 2 4 5 6 3) 4)

;ko:jug pos n-> bol
;objetivo: saber si una jugada será ko.
;algoritmo: si la posicion no esta en jug entonces #f, si no verificar si partida de jug al agregar pos en n esta en el map de la función partida de lisj en la lista listasluegode con jug y pos.
;una jugada es ko si el tablero resultante al jugarlo ya había aparecido.
;argumentos: jug->partida, pos->posición a evaluar, n->tamaño de tablero.
;(ko '((1 2) (2 3) (8 9) (6 8)) '(9 9) 9)
(define ko (λ(jug pos n) (if (not (memberq pos jug)) #f (memberq (partida (append jug (list pos)) n) (map (λ(lisj) (partida lisj n)) (listasluegode jug pos))))))


;yajuegacolor: jug vec n proh-> piedra
;objetivo: conocer una jugada con valor máximo dado un vector y evitando suicidios, ko y con la posibilidad de pasar.
;algoritmo: sea posi juegacolorcad2 con jug vec n proh, y tab partida de jug n; si pasa de amigas (second de tab en blancas o car de tab en negras), enemigas, posi vec y n es igual a '(0 0) entonces '(0 0);
;           si suicidio de amigas enemigas posi n entonces yajuegacolor de jug vec n y la lista proh al agregar posi;
;           si ko de jug posi n, entonces yajuegacolor de jug vec n y la lista proh al agregar posi; en otro caso posi.
;argumentos: jug->partida actual, vec-> vector de juego, n->tamaño de tablero, proh->posiciones no jugables.
;(yajuegablanca '((1 2) (2 3) (8 9) (6 8)) '(1 0.5 8 11) 9 '())

(define yajuegablanca (λ(jug vec n proh) (let ((posi (juegablancacad2 jug vec n proh)) (tab (partida jug n)))
                                           (cond ((equal? (pasa (second tab) (car tab) posi vec n) '(0 0)) '(0 0))
                                                 ((suicidio (second tab) (car tab) posi n) (yajuegablanca jug vec n (cons posi proh)))
                                                 ((ko jug posi n) (yajuegablanca jug vec n (cons posi proh)))
                                                 (else posi)))))

(define yajueganegra (λ(jug vec n proh) (let ((posi (jueganegracad2 jug vec n proh)) (tab (partida jug n)))
                                           (cond ((equal? (pasa (car tab) (second tab) posi vec n) '(0 0)) '(0 0))
                                                 ((suicidio (car tab) (second tab) posi n) (yajueganegra jug vec n (cons posi proh)))
                                                 ((ko jug posi n) (yajueganegra jug vec n (cons posi proh)))
                                                 (else posi)))))

;versus: vecn vecb n m jug->partida
;objetivo: obtener m jugadas por color a partir de dos vectores comenzando en un tablero dado
;algoritmo: si m es cero entonces jug; si la longitud de jug es par entonces versus de vecn vecb n m y agrega al final de jug yajueganegra de jug vecn n '();
;           en otro casi versus de vecn vecb n m-1 y agregar al final de jug yajuegablanca de jug vecb n '().
;los agumentos de vesrsus son: vecn-> vector de jugada de negros, vecb-> vector de jugada de blancos, n-> tamaño de tablero, m->número de jugadas a ejecutar, jug-> jugadas iniciales.
;(versus '(1 0.5 9 11) '(1 0.5 8 11) 9 50 '())
;(versus '(1 0.5 9 11) '(1 0.5 8 11) 9 20 '((1 2) (2 3) (8 9) (6 8)))

(define versus (λ(vecn vecb n m jug) (cond ((equal? m 0) jug) ((integer? (/ (length jug) 2)) (versus vecn vecb n m (append jug (list (yajueganegra jug vecn n '())))))
                                           (else (versus vecn vecb n (- m 1) (append jug (list (yajuegablanca jug vecb n '()))))))))


;enfrenta: vecn vecb n m jug-> vec
;objetivo: conocer que vector avanza mas al jugar m jugadas dado un tablero
;algoritmo: sea tabini el tablero iniial y tabfin el tablero final, punini el puntaje inicial y punfin el final. Si la diferencia del puntaje final e inicial negro es menor que la diferencia blanca, entonces vecb, si no si las diferencias son iguales, entonces uno de los
;           vectores al azar, en otro caso el vector negro
(define enfrenta (λ(vecn vecb n m jug) (let ((tabini (partida jug n)) (tabfin (partida (versus vecn vecb n m jug) n))) (let ((punini (evaluador (car tabini) (second tabini) n)) (punfin (evaluador (car tabfin) (second tabfin) n)))
                                                                                                                       (if (< (- (car punfin) (car punini)) (- (second punfin) (second punini))) vecb
                                                                                                                           (if (equal? (- (car punfin) (car punini)) (- (second punfin) (second punini))) (car (shuffle (list vecb vecn)))
                                                                                                                               vecn))) ) ))
;(enfrenta '(1 0.5 9 11) '(2 3 4 5) 9 7 '())
(define ^ (λ(a b) (apply * (build-list b (λ(x) a)))))

;parejas: dada una lista da la lista dividida en parejas
(define parejas (λ(lis) (if (< (length lis) 2) '() (cons (list (car lis) (second lis)) (parejas (cdr (cdr lis)))))))

;inoperante
(define torneo (λ(par n m jug lisa) (let ((win (map (λ(ppar) (enfrenta (car ppar) (second ppar) n m jug)) (parejas (shuffle par)))))
                                       (if (equal? (length par) 1) (cons (car par)lisa) (torneo (shuffle win) n m jug (append lisa par))))))
;Generacion de poblacion de tamano 8
;(define fit (torneo (build-list 8 (lambda(x) (build-list 4 (lambda(y) (random))))) 9 8 '() '()))
;(define individuos '((0.136846060506995 0.7130907334673388 0.021492005435362723 0.650972674461621)
;  (0.42448216613668266 0.6906910123451917 0.7640324341875376 0.9279794967779275)
;  (0.6363033110627646 0.04966063851709776 0.7285830007272922 0.15449067906803948)
;  (0.49750798858740874 0.4517661554197223 0.21444785003670325 0.8060908193390096)))

;(torneo individuos 9 5 '() '())

;(take (shuffle fit) 3)

;cruve: v1 v2 v3-> vector
;objetivo: conocer el cruce de tres vectores
;algoritmo: sea x una constante dada por los vectores, entonces crea la lista de x* v1[[1]], x*v1[[2]], v2[[3]], v2[[4]]
(define cruce (lambda (v1 v2 v3) (let ((x (/ (* (second v3) (third v2)) (* (second v1) (third v3))))) (list (* x (car v1)) (* x (second v1)) (third v2) (fourth v2)))))
;'((0.1797938052092473 0.24843227087378325 0.6801315426051991 0.40661085014582077)
;  (0.7321007457275306 0.29637333858889864 0.254152864651707 0.8627335702647881)
;  (0.6622429803820653 0.040396129806152314 0.1941628957600059 0.7339652002939866))
;(cruce '(0.1797938052092473 0.24843227087378325 0.6801315426051991 0.40661085014582077) '(0.7321007457275306 0.29637333858889864 0.254152864651707 0.8627335702647881)
;       '(0.6622429803820653 0.040396129806152314 0.1941628957600059 0.7339652002939866))


;INOPERANTE
(define geneaux (lambda(pob n m g jug) (let ((pun (torneo pob n m jug '())))
                                         (if (equal? g 0) pob (geneaux (map (lambda(pad) (cruce (car pad) (second pad) (third pad))) (build-list (length pob) (lambda(x) (take (shuffle pun) 3)))) n m (- g 1) jug)))))


;INOPERANTE
(define gene (lambda(n m g jug pob) (geneaux (build-list pob (lambda(x) (build-list 4 (lambda(y) (random))))) n m g jug)))

;(gene 9 10 6 '() 8)

;valorablanca: vecb vecn n m jug: numero
;objetivo: conocer cuanto avanza el vector blanco al competir contra uno negro
;Algoritmo: se calculan las puntuaciones al inicio y al final del juego, luego se hace la diferencia.
(define valorablanca (λ(vecb vecn n m jug) (let ((tabini (partida jug n)) (tabfin (partida (versus vecn vecb n m jug) n))) (let ((punini (evaluador (car tabini) (second tabini) n)) (punfin (evaluador (car tabfin) (second tabfin) n)))
                                                                                                                             (- (second punfin) (second punini))))))
(define valoranegra (λ(vecb vecn n m jug) (let ((tabini (partida jug n)) (tabfin (partida (versus vecn vecb n m jug) n))) (let ((punini (evaluador (car tabini) (second tabini) n)) (punfin (evaluador (car tabfin) (second tabfin) n)))
                                                                                                                             (- (car punfin) (car punini))))))


;pruebaCOLOR vecC lisv n m jug-> numero
;objetivo: se suma los valoraColor de vecC contra cada vecctor de la lista de vectores lisv.
(define pruebablanca (λ(vecb lisv n m jug) (suma (map (λ(vecn) (valorablanca vecb vecn n m jug)) lisv) 0)))
(define pruebanegra (λ(vecn lisv n m jug) (suma (map (λ(vecb) (valoranegra vecb vecn n m jug)) lisv) 0)))

;torneoCOLOR1: lisC lisv n m jug: lista de vectores
;objetivo: obtener una lista donde cada vector ocurra el numero que obtuvo en prueba
;algoritmo: justa las listas de los vectores ocurriendo el numero de lugares que gana en el prueba,
(define torneoblanco1 (λ(lisb lisv n m jug) (let ((pru (map (λ(vecb) (pruebablanca vecb lisv n m jug)) lisb))) (let ((suma (if (< (apply max pru) 1) (+ (apply max pru) 1) 0))) (apply append (map (λ(k) (let ((fit (last (take pru k)))) (if (< fit 1) '() (build-list fit (λ(x) (last (take lisb k)))))) ) (build-list (length lisb) (λ(t) (+ t 1)))))))))
(define torneonegro1 (λ(lisn lisv n m jug) (let ((pru (map (λ(vecn) (pruebanegra vecn lisv n m jug)) lisn))) (let ((suma (if (< (apply max pru) 1) (+ (apply max pru) 1) 0))) (apply append (map (λ(k) (let ((fit (last (take pru k)))) (if (< fit 1) '() (build-list fit (λ(x) (last (take lisn k)))))) ) (build-list (length lisn) (λ(t) (+ t 1)))))))))


;geneauxCOLOR1: pob n m g jug->poblacion
;objetivo: conocer la g-ésima generación del algoritmo genético
;algoritmo: si g es cero, entonces pob, si no hace el llamado recursivo evolucionando la población una generacion y actualizando g a g-1
(define geneauxblanco1 (lambda(pob n m g jug) (let ((pun1 (torneoblanco1 pob (build-list 4 (lambda(x) (build-list 4 (lambda(y) (random))))) n m jug)))
                                         (let ((pun (append pun1 pun1 pun1))) (if (equal? g 0) pob (geneauxblanco1 (map (lambda(pad) (cruce (car pad) (second pad) (third pad))) (build-list (length pob) (lambda(x) (take (shuffle pun) 3)))) n m (- g 1) jug))))))



;geneCOLOR1: n m g jug pob-> pob
;objetivo: comocer la g-ésima generacion a partir de una población aleatoria
;algoritmo: hace el geneauxCOLOR de una población aleatora
(define geneblanco1 (lambda(n m g jug pob) (geneauxblanco1 (build-list pob (lambda(x) (build-list 4 (lambda(y) (random))))) n m g jug)))


(define geneauxnegro1 (lambda(pob n m g jug) (let ((pun1 (torneonegro1 pob (build-list 4 (lambda(x) (build-list 4 (lambda(y) (random))))) n m jug)))
                                         (let ((pun (append pun1 pun1 pun1))) (if (equal? g 0) pob (geneauxnegro1 (map (lambda(pad) (cruce (car pad) (second pad) (third pad))) (build-list (length pob) (lambda(x) (take (shuffle pun) 3)))) n m (- g 1) jug))))))

(define genenegro1 (lambda(n m g jug pob) (geneauxnegro1 (build-list pob (lambda(x) (build-list 4 (lambda(y) (random))))) n m g jug)))
;(geneblanco1 9 8 5 '() 4)

;tornvecCO: lisb n jug -> vector
;objetivo: de un grupo de vectores conocer los más aptos
;algoritmo: hace el maxfun de lisb con la función bruebaCOLOR
(define tornvecbl (λ(lisb n jug) (let ((lisn (build-list 5 (lambda(x) (build-list 4 (lambda(y) (random))))))) (maxfun lisb (λ(vec) (pruebablanca vec lisn n 4 jug))))))

(define tornvecne (λ(lisb n jug) (let ((lisn (build-list 5 (lambda(x) (build-list 4 (lambda(y) (random))))))) (maxfun lisb (λ(vec) (pruebanegra vec lisn n 4 jug))))))


;juegagenCO: n m g jug pob -> pos
;onbjetivo: conocer una jugada
;aloritmo: Si, el la el torneo el nulo entonces pasa, si no juega usando un vector de los ganador(es) del torneo.
(define juegagenbl (λ(n m g jug pob) (let ((vecs (shuffle (tornvecbl (geneblanco1 n m g jug pob) 9 jug)))) (if (null? vecs) '(0 0) (yajuegablanca jug (car vecs) n '())))))

(define borde (lambda(n) (append (cartesian-product (build-list n (lambda(x) (+ x 1))) (list 1 n 2 (- n 1))) (cartesian-product (list 1 2 (- n 1) n) (build-list n (lambda(x) (+ x 1)))))))



(define juegagenne (λ(n m g jug pob) (let ((vec (car (shuffle (tornvecne (genenegro1 n m g jug pob) 9 jug))))) (if (null? jug) (yajueganegra jug vec n (borde n)) (yajueganegra jug vec n '())))))



(define versusgen (lambda(n m g jug pob j) (if (equal? j 0) jug (if (even? (length jug)) (versusgen n m g (append jug (list (juegagenne n m g jug pob))) pob (- j 1))
                                                                    (versusgen n m g (append jug (list (juegagenbl n m g jug pob))) pob (- j 1))))))
;Juego contra humano
;Humano juega negras maquina juega blancas
;Programa piix
;localidad: kaaj
;Game of Go: A Cellular Automata and Genetic algorthm approach

;Juego contra Amybot(14k) (blanca)
;(define jugg '((3 6) (5 5) (5 4) (4 4) (2 6) (6 4) (2 5) (6 7) (4 5) (5 3) (3 5) (4 7)
;                     (3 4) (3 3) (4 6) (5 6) (3 7) (4 8) (3 8) (2 3)))
;((partida jugg 9))


;(evaluador   9)
;(define uljug (juegagenne 9 4 6 jugg 6))
;uljug

(define enfrentablanco (λ(v1 v2 n m jug) (let ((pun1 (valorablanca v1 v2 n m jug)) (pun2 (valorablanca v2 v1 n m jug))) (if (< pun1 pun2) v2 (if (equal? pun1 pun2)
                                                                                                                                                 (car (shuffle (list v1 v2))) v1)))))
(define enfrentanegro (λ(v1 v2 n m jug) (let ((pun1 (valoranegra v1 v2 n m jug)) (pun2 (valoranegra v2 v1 n m jug))) (if (< pun1 pun2) v2 (if (equal? pun1 pun2)
                                                                                                                                                 (car (shuffle (list v1 v2))) v1)))))


(define torneoblanco2 (λ(par n m jug lisa) (let ((win (map (λ(ppar) (enfrentablanco (car ppar) (second ppar) n m jug)) (parejas (shuffle par)))))
                                       (if (equal? (length par) 1) (cons (car par)lisa) (torneo (shuffle win) n m jug (append lisa par))))))
(define torneonegro2 (λ(par n m jug lisa) (let ((win (map (λ(ppar) (enfrentanegro (car ppar) (second ppar) n m jug)) (parejas (shuffle par)))))
                                       (if (equal? (length par) 1) (cons (car par)lisa) (torneo (shuffle win) n m jug (append lisa par))))))


(define individuos '((0.136846060506995 0.7130907334673388 0.021492005435362723 0.650972674461621)
  (0.42448216613668266 0.6906910123451917 0.7640324341875376 0.9279794967779275)
  (0.6363033110627646 0.04966063851709776 0.7285830007272922 0.15449067906803948)
  (0.49750798858740874 0.4517661554197223 0.21444785003670325 0.8060908193390096)))

;(torneo individuos 9 5 '() '())


;(define jugarblanca (λ(jug vec n) (let ((posi (juegablancacad2 jug vec n)) (tab (partida jug n))) (cond (sui)))))


;(suicidio '((2 2)) '((1 2) (2 1)) '(1 1) 9)

;Con pruebas se ha notado que es mas conveniente ser agresivo en tableros pequeños. Al futuro podría probarse con buscar el vector de juego con algoritmos genéticos para cada tablero.

;(mejorj '((2 2) (3 3) (2 4) (2 5) (2 6) (4 5) (5 5) (6 6) (7 5)) '((3 2) (3 4) (3 5) (4 2) (4 3) (4 4) (5 4) (6 3) (6 5)) '(1 0.5 5 5) 7)
;(mejorj '((2 2) (3 3) (2 4) (2 5) (2 6) (4 5) (5 5) (6 6) (7 5)) '((3 2) (3 4) (3 5) (4 2) (4 3) (4 4) (5 4) (6 3) (6 5)) '(3 4 3 5) 7)
;(mejorj '((1 4) (1 6) (2 4) (3 3) (3 4) (3 5) (3 6) (3 7) (4 3) (5 1) (5 2) (5 3) (6 3) (7 3) (7 5) (7 6)) '((1 3) (2 1) (2 3) (2 5) (2 6) (3 2) (4 1) (4 2) (4 5) (4 6) (4 7) (5 4) (5 5) (6 4) (6 6) (6 7) (7 4)) '(3 4 3 5) 7)
;(mejorj '((1 4) (1 6) (2 4) (3 3) (3 4) (3 5) (3 6) (3 7) (4 3) (5 1) (5 2) (5 3) (6 3) (7 3) (7 5) (7 6)) '((1 3) (2 1) (2 3) (2 5) (2 6) (3 2) (4 1) (4 2) (4 5) (4 6) (4 7) (5 4) (5 5) (6 4) (6 6) (6 7) (7 4)) '(1 0.5 5 5) 7)
;(mejorj '((2 3) (2 4) (3 5) (3 6) (3 7) (4 2) (4 3) (4 4) (5 1)) '((4 5) (4 6) (4 7) (5 2) (5 4) (5 5) (6 1) (6 2)) '(1 0.5 5 5) 7)

(define negra (car (partida '((1 2) (1 1) (2 1) (2 2) (2 3) (3 2)) 9)))
(define blanca (second (partida '((1 2) (1 1) (2 1) (2 2) (2 3) (3 2)) 9)))


;(map (λ(pos) (provec '(3 4 3 6) (jugada negra blanca pos 9))) (mejorj negra blanca '(3 4 3 6) 9))
;(mejorj negra blanca '(2 4 3 5) 9)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; tablero ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define reemplaza (lambda(ren col ele mat)
                (cond ((null? mat) '())
                     ((equal? ren 1) (append  (list (reemplazacol col ele (car mat))) (reemplaza (- ren 1) col ele (cdr mat) )))
                     (else (append (list (car mat)) (reemplaza (- ren 1) col ele (cdr mat) )))
                    )))


(define reemplazacol (lambda(col ele renglon)
            (cond ((null? renglon) '())
                 ((equal? col 1) (cons ele (reemplazacol (- col 1) ele (cdr renglon))))
                  (else (cons (car renglon) (reemplazacol (- col 1) ele (cdr renglon))))
                   )))

(define reemplazavertices (lambda(vertices ele mat)
         (if (null? vertices) mat 
              (reemplazavertices (cdr vertices) ele  (reemplaza (car (car vertices)) (car (cdr (car vertices))) ele mat))
            ))) 

(define primeros (lambda(m) (map (lambda(x) (car x)) m)))
(define restos (lambda(m) (map (lambda(x) (cdr x)) m)))

(define transpuesta (lambda(mat)
       (if (null? (car mat)) '()
             (append (list (primeros mat))  (transpuesta (restos mat)))
                      )))

(define (bordered-square n)
  (filled-rectangle n n #:draw-border? #t))

(define (draw-row lst)
  (apply hc-append 2 (map (λ (x) (colorize (disk 10)
                                           (cond((= x 0) "red")
                                                ((= x 1) "black")
                                                ((= x 2) "gray")
                                                (else "gray"))))
                          lst)))


(define showtablero (lambda(b w n)
       (map (lambda(space) (apply vc-append 2 (map draw-row
                       space))) (list (reverse (transpuesta (reemplazavertices b 1 (reemplazavertices w 0 (build-list n (lambda(x) (build-list n (lambda(x) 2)))))))))
                         )))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;REPRESENTACIÓN DE LA EVALUACIÓN

;(define tablerofinal1 (partida '((5 5) (6 5) (6 4) (5 6) (4 4) (7 4) (6 6) (3 4) (7 5) (8 5) (5 3) (5 2) (4 6)
;                   (3 6) (5 7) (5 8) (3 5) (2 5) (4 5) (8 2) (8 4) (2 7) (1 6) (8 8) (7 3) (3 2)
;                   (2 6) (7 7) (2 4) (2 3) (1 5) (3 8) (4 7) (4 3) (4 1) (9 4) (3 7) (7 9) (3 3) (1 8)
;                   (2 9) (6 1) (6 8) (1 2) (2 1) (9 6) (4 2) (4 9) (1 7) (7 2) (2 8) (6 2) (2 2) (8 3)
;                   (4 8) (6 3) (5 9) (5 1) (3 9) (9 2) (7 4) (9 3) (1 9) (9 5) (8 6) (9 7) (1 3) (8 7)) 9))


;(showtablero (second tablerofinal1) (car tablerofinal1) 9)
;(showtablero (append (second tablerofinal1) (second (evaluaaux (car tablerofinal1) (second tablerofinal1) '() '() 9))) (car tablerofinal1) 9)
;(showtablero (second tablerofinal1) (append (car tablerofinal1) (car (evaluaaux (car tablerofinal1) (second tablerofinal1) '() '() 9)))  9)
;(let ((ev (evalua (car tablerofinal1) (second tablerofinal1) 9 '() '()))) (showtablero (second ev) (car ev) 9))

;(showtablero negra blanca 9)

;Ver archivos .sgf

(define hispartidaux (λ(jug n) (if (null? jug) '() (cons (partida jug n) (hispartidaux (take jug (- (length jug) 1)) n)))))
(define hispartida (λ(jug n) (reverse (hispartidaux jug n))))
(define plotpartida (λ(jug n) (map (λ(tab) (showtablero (car tab) (second tab) n)) (hispartida jug n))))


;(last (plotpartida (append jugg (list uljug)) 9))
;(last (plotpartida jugg 9))


;CORRER EN LA NOCHE versus tamañodetablero numerodejugaadasentorneo numerodegeneracionesgenetico tablero tamañodepoblacion numerodejugadas
;(define jugg (versusgen 9 4 5 '() 6 100))
;jugg
;(plotpartida jugg 9)

;(plotpartida jugg 9)

;(plotpartida '((5 5) (6 5) (6 4) (5 6) (4 4) (7 4) (6 6) (3 4) (7 5) (8 5) (5 3) (5 2) (4 6)
;                   (3 6) (5 7) (5 8) (3 5) (2 5) (4 5) (8 2) (8 4) (2 7) (1 6) (8 8) (7 3) (3 2)
;                   (2 6) (7 7) (2 4) (2 3) (1 5) (3 8) (4 7) (4 3) (4 1) (9 4) (3 7) (7 9) (3 3) (1 8)
;                   (2 9) (6 1) (6 8) (1 2) (2 1) (9 6) (4 2) (4 9) (1 7) (7 2) (2 8) (6 2) (2 2) (8 3)
;                   (4 8) (6 3) (5 9) (5 1) (3 9) (9 2) (7 4) (9 3) (1 9) (9 5) (8 6) (9 7) (1 3) (8 7)) 9)


;(define jugg '((4 4) (4 5) (5 4) (5 5) (6 5) (5 6) (6 4) (6 6) (7 6) (5 7) (3 5)))
;(yajuegablanca jugg '(1.4532673631395943 0.8211656917515014 0.6916404247426448 0.6021625842549414) 9 '())

;(define jugg '((4 4) (4 5) (5 4) (5 5) (6 5) (5 6) (6 4) (6 6) (7 6) (5 7) (3 5) (6 7) (8 6) (7 7) (9 7) (8 7) (8 8) (7 8) (7 9) (4 7) (4 6)))
;(yajuegablanca jugg '(0.05612068727281038 0.060478576797741324 0.10086152771934816 0.510968525493856) 9 '())


;(define jugg '((4 4) (4 5) (5 4) (5 5) (6 5) (5 6) (6 4) (6 6) (7 6) (5 7) (3 5) (6 7) (8 6) (7 7) (9 7) (8 7) (8 8) (7 8) (7 9) (4 7) (4 6) (3 6) (8 9) (9 6) (9 5) (8 5) (6 8) (5 8)
;                     (6 9) (5 9) (7 5) (3 4) (8 4)))
;(yajuegablanca jugg ' (0.032289007946788795 0.20539806434389057 0.23314438585518682 0.529170807699563) 9 '())



;(define jugg '((4 4) (4 5) (5 4) (5 5) (6 5) (5 6) (6 4) (6 6) (7 6) (5 7) (3 5) (6 7) (8 6) (7 7) (9 7) (8 7) (8 8) (7 8) (7 9) (4 7) (4 6) (3 6) (8 9) (9 6) (9 5) (8 5) (6 8) (5 8)
              ;       (6 9) (5 9) (7 5) (3 4) (8 4) (4 6) (9 8) (4 9) (2 5) (2 6) (3 3) (4 8) (2 4) (3 9) (1 6)))
;(yajuegablanca jugg '(0.015045196311961327 0.040163921435024744 0.049013956960966595 0.13203845486603646) 9 '())



;(define jugg '((4 4) (4 5) (5 4) (5 5) (6 5) (5 6) (6 4) (6 6) (7 6) (5 7) (3 5) (6 7) (8 6) (7 7) (9 7) (8 7) (8 8) (7 8) (7 9) (4 7) (4 6) (3 6) (8 9) (9 6) (9 5) (8 5) (6 8) (5 8)
    ;                 (6 9) (5 9) (7 5) (3 4) (8 4) (4 6) (9 8) (4 9) (2 5) (2 6) (3 3) (4 8) (2 4) (3 9) (1 6) (3 7) (2 7) (2 9) (3 8) (2 8) (1 9) (1 8) (1 7) (3 8)))
;(yajuegablanca jugg '(0.368270852879602 0.3365935771285098 0.26083156053278705 0.5480233184035985) 9 '())


;(print "Wait I'm Thinking....")
;(geneblanco1 9 8 6 jugg 6)


;(plotpartida jugg 9)

;(define part (versus '(1 0.5 9 11) '(3 0.5 8 11) 9 15 '()))
;part
;(plotpartida part 9)


;Juego de 07/05/2019 Contra Will (negras)
;(define lisjug '((5 5) (2 8) (6 5) (3 8) (6 4) (1 8) (7 5) (2 7) (4 4) (2 6) (5 3) (2 9) (7 3) (3 7) (8 4) (1 6) (3 3) (4 7) (4 2) (2 5) (6 2) (4 8)
;                       (2 3) (5 7) (2 4) (1 9) (8 6) (6 7) (7 7) (3 9) (6 6) (3 5) (4 5) (3 4) (3 6) (5 6) (1 4) (4 9) (8 8) (4 6) (9 7) (5 8) (6 8) (1 7)
;                       (7 9) (1 5) (5 9) (6 9) (7 8) (5 9) (9 9)))
 
;(define piedra (juegablancacad2 lisjug '(1 0.5 8 10) 9))
;piedra
;(last (plotpartida (append lisjug (list piedra)) 9))

;En este juego se vio que conforme aumenta el tamaño de la cadena el programa se vuelve mas defensivo por lo que hay que hacer que dependa del tamaño de la cadena.

;Segundo juego de 07/05/2019 Contra Will (negras)

;(define lisjug '((6 5) (5 9) (7 6) (4 9) (5 6) (5 8) (6 7) (3 9) (5 4) (4 8) (7 4) (6 8) (8 5) (7 8) (4 5) (5 7) (6 3) (8 8)
;                       (8 7) (7 7) (9 6) (9 8) (6 6) (3 8) (4 7) (8 9) (3 6) (9 9) (2 5) (2 8) (2 7) (1 8) (1 6) (1 9) (3 4) (7 9)
;                       (4 3) (2 9) (5 2) (3 7) (4 6) (1 7) (2 6) (6 9) (9 4)))
;
;(define maxcad (let ((q (map length (confcad (second (partida lisjug 9)) 9)))) (if (null? q) 1 (apply max q))))

;(define piedra (juegablancacad2 lisjug (list (/ 1 maxcad) (/ 0.5 maxcad) (* 8 maxcad) (* 9.5 maxcad)) 9))
;piedra
;(last (plotpartida (append lisjug (list piedra)) 9))


;Juego 1 de 14/05/2019 contra Will (negras)
;(define lisjug '((4 6) (5 6) (5 7) (4 7) (4 8) (5 8) (3 7) (3 6) (6 6) (6 7) (5 5) (4 5) (6 8) (3 8) (5 9) (6 5) (7 7) (4 9) (3 9) (7 8) (3 5) (6 9)
;                       (2 6) (7 6) (2 8) (2 7) (1 7) (2 9) (4 4) (5 4) (7 9) (3 4) (7 5) (1 8) (1 9) (1 6) (8 6) (8 7) ()))

;(define piedra (yajuegablanca lisjug '(1 0.5 9 11) 9 '()))
;piedra
;(last (plotpartida (append lisjug (list piedra)) 9))


;Juego 2 de 14/05/2019 contra Will (negras)
;(define lisjug '((4 6) (6 4) (6 5) (5 5) (5 4) (6 6) (5 6) (7 5) (7 4) (4 4) (4 5) (5 3) (7 6) (7 3) (6 3) (8 4) (8 3) (3 7) (8 5) (3 5) (3 6) (2 6) (2 5) (8 6)
;                 (3 4) (9 5) (2 7) (6 2) (1 6) (7 7) (7 2) (8 2) (5 2) (9 3) (6 7) (7 1) (9 2) (3 3) (4 3) (2 4) (5 4) (4 2) (6 5) (4 4) (4 3) (6 5) (5 6) (5 5)))

;(define piedra (yajueganegra lisjug '(1 7 9 11) 9 '()))
;piedra
;(plotpartida (append lisjug (list piedra)) 9)

;(+ 1 2)


;(plotpartida '((5 5) (6 5) (6 4) (5 6) (4 4) (7 4) (6 6) (3 4) (7 5) (8 5) (5 3) (5 2) (4 6)
;                   (3 6) (5 7) (5 8) (3 5) (2 5) (4 5) (8 2) (8 4) (2 7) (1 6) (8 8) (7 3) (3 2)
;                   (2 6) (7 7) (2 4) (2 3) (1 5) (3 8) (4 7) (4 3) (4 1) (9 4) (3 7) (7 9) (3 3) (1 8)
;                   (2 9) (6 1) (6 8) (1 2) (2 1) (9 6) (4 2) (4 9) (1 7) (7 2) (2 8) (6 2) (2 2) (8 3)
;                   (4 8) (6 3) (5 9) (5 1) (3 9) (9 2) (7 4) (9 3) (1 9) (9 5) (8 6) (9 7) (1 3) (8 7)) 9)

;(plotpartida '((5 5) (6 5) (6 4) (5 6) (4 4) (7 4) (6 6) (3 4) (7 5) (8 5) (5 3) (5 2) (4 6)
;                     (3 6) (5 7) (5 8) (3 5) (2 5) (4 5) (8 2) (8 4) (2 7) (1 6) (8 8) (7 3) (3 2)
;                     (2 6) (7 7) (2 4) (2 3) (1 5) (3 8) (4 7) (4 3) (4 1) (9 4) (3 7) (7 9) (3 3) (1 8)
;                     (2 9) (6 1) (6 8) (1 2) (2 1) (9 6) (4 2) (4 9) (1 7) (7 2) (2 8) (6 2) (2 2) (8 3)
;                     (4 8) (6 3) (5 9) (5 1) (3 9) (9 2) (7 4) (9 3) (1 9) (9 5) (8 6) (9 7) (1 3) (8 7) (1 3)) 9)

;(partida '((5 5) (6 5) (6 4) (5 6) (4 4) (7 4) (6 6) (3 4) (7 5) (8 5) (5 3) (5 2) (4 6)
;                     (3 6) (5 7) (5 8) (3 5) (2 5) (4 5) (8 2) (8 4) (2 7) (1 6) (8 8) (7 3) (3 2)
;                     (2 6) (7 7) (2 4) (2 3) (1 5) (3 8) (4 7) (4 3) (4 1) (9 4) (3 7) (7 9) (3 3) (1 8)
;                     (2 9) (6 1) (6 8) (1 2) (2 1) (9 6) (4 2) (4 9) (1 7) (7 2) (2 8) (6 2) (2 2) (8 3)
;                     (4 8) (6 3) (5 9) (5 1) (3 9) (9 2) (7 4) (9 3) (1 9) (9 5) (8 6) (9 7) (1 3) (8 7) (1 3))  9)

;(hispartida '((1 2) (1 1) (2 1) (2 2) (2 3) (3 2)) 9)

;(map (λ(tab) tab) (hispartida '((1 2) (1 1) (2 1) (2 2) (2 3) (3 2)) 9))
;((λ(tab) (showtablero (car tab) (second tab) 9)) '(((2 1) (1 2)) ((2 2)))) 
;(map (λ(tab) (showtablero (car tab) (second tab) 9)) (hispartida '((1 2) (1 1) (2 1) (2 2) (2 3) (3 2)) 9))
;(plotpartida '((1 2) (1 1) (2 1) (2 2) (2 3) (3 2)) 9)


;juego 1
;-----------------------------------------------------------------------------------------------
;(define blanca '((7 3) (8 3) (8 2) (8 1) (9 1) (9 3) (6 2) (7 1) (7 4) (8 5) (9 4) (7 5) (8 6) (4 2) (5 1) (5 3) (6 4) (9 5) (3 1) (4 1)))

;(define negra '((5 5) (6 5) (5 6) (4 5) (5 4) (7 6) (8 7) (4 3) (3 2) (2 1) (9 8) (6 7) (7 8) (8 9) (3 4) (1 2) (2 4) (1 3) (3 6) (4 8) (2 8) (1 7) (5 8) (2 6) (9 6) (1 5) (9 7) (2 3)))

;(evaluador blanca negra 9)
;(mata negra blanca '(3 9) 9)
;------------------------------------------------------------------------------------------------


;juego 2
;----------------------------------------------------------------------------

;(define negra '((5 5) (4 4) (4 6) (7 7) (3 3) (7 5) (8 6) (4 5) (8 4) (8 5) (6 6) (7 6) (8 8) (9 6) (5 8) (6 9) (9 8) (4 7) (6 6) (4 2) (2 2) (5 1) (6 2) (7 3) (6 2) (3 8) (1 1) (1 8) (1 2) (2 7) (1 2) (1 3) (2 8) (8 2) (1 7) (7 1) (3 9) (5 9) (9 1)))
;(define blanca '((3 1) (5 3) (8 7) (9 4)))

;(evalua blanca negra 9)
;(quita blanca (mata negra blanca '(1 5) 9))
;(mata negra blanca '(1 5) 9)
;(jugada negra blanca '(8 6) 9)

;--------------------------------------------------------------------------

;Juego











;pendientes: *calcular libertades de cadenas*, evaluar jugadas
;suicidio, quitar muertas

;(filter (λ(pos) (memberq pos '((1 1) (1 2) (2 1)))) (union (map (λ(pos) (goVecindad1 pos 4)) '((2 1) (1 2) (1 1)))))

;(evalua '((1 1)(1 2)(1 4)(1 7)(1 8)(1 11)(1 17)(1 18)(1 19)(2 6)(2 7)(2 10)(2 15)(2 18)(3 1)(3 2)(3 5)(3 9)(3 12)(3 14)(3 16)(4 1)(4 9)(4 11)(4 13)(4 17)(4 18)(5 3)(5 4)(5 8)(5 9)(5 11)(5 13)(5 16)(5 17)(6 1)(6 6)(6 7)(6 10)(6 12)(6 13)(7 1)(7 6)(7 13)(7 15)(7 18)(8 1)(8 2)(8 4)(8 10)(8 13)(8 15)(8 16)(8 17)(8 19)(9 1)(9 7)(9 11)(9 12)(9 14)(9 15)(9 16)(10 1)(10 6)(10 7)(10 9)(10 12)(10 13)(10 15)(10 18)(11 1)(11 5)(11 7)(11 8)(11 9)(11 10)(11 14)(11 18)(12 3)(12 4)(12 6)(12 7)(12 8)(12 12)(12 19)(13 3)(13 5)(13 8)(13 15)(13 17)(14 8)(14 10)(14 11)(14 15)(14 18)(14 19)(15 3)(15 5)(15 8)(15 12)(15 13)(15 18)(16 3)(16 4)(16 9)(16 13)(16 16)(17 1)(17 5)(17 19)(18 1)(18 4)(18 6)(18 9)(18 11)(18 16)(18 17)(18 19)(19 5)(19 6)(19 7)(19 13))  '((1 5)(1 12)(1 13)(1 14)(1 15)(1 16)(2 4)(2 5)(2 8)(2 11)(2 13)(2 16)(2 17)(3 3)(3 4)(3 7)(3 11)(3 15)(3 17)(4 3)(4 6)(4 7)(4 10)(4 12)(4 14)(4 15)(5 1)(5 2)(5 7)(5 15)(5 18)(6 5)(6 8)(6 9)(6 16)(6 18)(7 3)(7 5)(7 7)(7 8)(7 10)(7 11)(7 14)(7 19)(8 3)(8 6)(8 7)(8 8)(8 12)(9 2)(9 3)(9 4)(9 8)(9 18)(9 19)(10 4)(10 8)(10 10)(10 11)(10 17)(11 2)(11 11)(11 15)(11 16)(11 17)(11 19)(12 13)(12 15)(13 4)(13 11)(13 12)(13 13)(14 1)(14 5)(14 6)(14 7)(14 17)(15 1)(15 7)(15 10)(15 11)(15 14)(15 17)(15 19)(16 6)(16 7)(16 12)(16 14)(16 18)(17 2)(17 6)(17 8)(17 9)(17 10)(17 11)(17 12)(17 14)(17 15)(17 16)(17 17)(17 18)(18 2)(18 3)(18 10)(18 12)(19 9)(19 10)(19 15)(19 16)(19 17)(19 19)) 19)


;'((((2 3) (2 1) (1 2)) ((3 2) (2 2))) (((2 3) (2 1) (1 2)) ((2 2))) (((2 1) (1 2)) ((2 2))) (((2 1) (1 2)) ()) (((1 2)) ((1 1))) (((1 2)) ()))

;'((((1 2)) ()) (((1 2)) ((1 1))) (((2 1) (1 2)) ()) (((2 1) (1 2)) ((2 2))) (((2 3) (2 1) (1 2)) ((2 2))) (((2 3) (2 1) (1 2)) ((3 2) (2 2))))

;(define (f x) (if (zero? x) 0 (add1 (f (sub1 x)))))
;(trace f)
;(f 10)


;Juego para prueba de Turing 1 (blanco humano)
(define jugg '((7 7) (7 6) (7 8) (6 7) (6 8) (5 8) (8 7) (9 6) (8 6) (8 5) (9 7) (6 9) (9 5) (4 9) (7 9) (9 4) (5 9) (9 9)))



;(evaluador   9)
(define uljug (juegagenne 9 4 6 jugg 6))
uljug

(last (plotpartida (append jugg (list uljug)) 9))



;Juego para prueba de Turing 2 (negro humano)
;(define jugg '((7 5) (7 6) (7 7) (6 6) (6 4) (6 5) (5 5) (5 6) (5 7) (6 7) (4 5) (4 6) (4 7) (8 6) (7 3) (8 5) (8 4) (8 7) (7 8) (6 8)))



;(evaluador   9)
;(define uljug (juegagenbl 9 4 6 jugg 6))
;uljug

;(last (plotpartida (append jugg (list uljug)) 9))

;Juego para prueba de Turing 3 (Blanco humano)

;(define jugg '((4 9) (4 8) (3 9) (2 8) (3 8) (1 7) (2 9) (1 9) (1 8) (2 7) (1 9) (3 7) (5 9) (5 7) (4 7)))



;(evaluador   9)
;(define uljug (juegagenbl 9 4 6 jugg 6))
;uljug

;(last (plotpartida (append jugg (list uljug)) 9))