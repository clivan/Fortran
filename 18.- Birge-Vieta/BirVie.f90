!Autor: Claudio Iván Esparza Castañeda
!Título: Birge-Vieta
!Descripción: El método se basa en el método de Newton-Raphson implementando el esquema de Horner para la evaluación del polinomio y su derivada
!Fecha: 22/03/2020
  
Program birvie
  implicit none
  REAL::e, er, x, xe, b, c, r
  INTEGER::n, i, j
  REAL, allocatable, dimension(:)::A
  WRITE(*, *) "Grado del polinomio"
  READ(*, *) n
  allocate(A(0:n))
  WRITE(*, *) "Coeficientes del polinomio a0+a1x+a2x²+...anx^n"
  do i=0, n
     READ(*, *) A(i)
  end do
  WRITE(*, *) "Valor inicial" !Preguntar final del intervalo
  READ(*, *) x !Ingresar final del intervalo
  WRITE(*, *) "Error esperado" !Preguntar el error
  READ(*, *) er !Ingresar el error
  i=0
  do
     i=i+1
     xe=x
     b=A(n)
     c=A(n)
     do j=n-1, 1, -1
        b=b*x+a(j)
        c=c*x+b
     end do
     b=b*x+A(0)
     r=x-b/c
     x=r
     if (x .NE. 0) then
        e=abs((x-xe)/x)*100
     end if
     WRITE(*, *) x
     if (e<er) then
        EXIT
     end if
  end do
  WRITE(*, *) "Hola"
  deallocate(A)
end Program birvie
