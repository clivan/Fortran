!Autor: Claudio Iván Esparza Castañeda
!Título: Evaluar polinomios y su derivada
!Descripción: Se encarga de evaluar polinomios mediante la anidación de los términos, así como la evaluación de la derivada
!Fecha: 19/01/2020

program eval !Inicio del programa
  implicit none !Sin variables implícitas
  REAL, allocatable, dimension(:)::a !Vector tipo REAL con dimensión dinámica
  REAL::p, d, x !Varibles tipo REAL
  INTEGER::i, n !Variables enteras
  d=0.0 !Inicializar valor de derivada
  p=0.0 !Inicializar valor de polinomio
  WRITE(*, *) "Grado del polinomio" !Preguntar por el grado del polinomio
  READ(*, *) n
  allocate(a(0:n)) !Asignar valor a variable dinámica
  WRITE(*, *) "Coeficientes del polinomio a0+a1x+a2x²+...anx^n" !Pregunta coeficientes del polinomio
  do i=0, n 
     READ(*, *) a(i)
  end do
  WRITE(*, *) "Valor para evaluar polinomio" !Preguntar valor para evaluar polinomio
  READ(*, *) x 
  do i=n, 0, -1 !Recorrer el arreglo al revés
     d=d*x+p !Derivada=Derivada*valor+Polinomio
     p=p*x+a(i) !Polinomio=Polinomio*valor+elemento del vector
  end do
  deallocate(a)
  WRITE(*, 1) "F(x)=", p, "F'(x)=", d !Imprimir resultado con formato 1
  1 format(' ', a5, f10.3, 5x, a6, f10.3) !
end program eval


