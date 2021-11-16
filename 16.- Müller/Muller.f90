!Autor: Claudio Iván Esparza Castañeda
!Título: Müller
!Descripción: Se encarga de calcular las raíces de un polinomio a partir del método de Müller
!Fecha: 22/03/2020
Program mul
  implicit none
  REAL::x0, x1, x2, xr, e, er, h, A, B, C, s, f
  INTEGER::i
  CALL graf1()
  h=0.5
  WRITE(*, *) "Raíz aproximada:"
  READ(*, *) xr
  x2=xr
  x1=xr+h*xr
  x0=xr-h*xr
  WRITE(*, *) "Error esperado:"
  READ(*, *) er
  i=0
  do
     A=((f(x0)-f(x1))/(x0-x1)-(f(x1)-f(x2))/(x1-x2))/(x0-x2)
     B=A*(x0-x1)+(f(x0)-f(x1))/(x0-x1)
     C=f(x0)
     if (B<0) then
        s=-1
     else
        s=1
     end if
     x2=x1
     x1=x0
     x0=x0+(-2*C)/(B+s*sqrt(B**2-4*A*C))
     e=abs((x0-x1)/x0)
     i=i+1
     if (e<=er) then
        EXIT
     end if
  end do
  write(*, *) "       x                 f(x)            e               i"
  write(*, *) x0, f(x0), e, i
  open(unit=3, file="Punto.dat", status="unknown") !Archivo para graficar el punto de intersección
  write(3, *) x0, f(x0) !Escribir en archivo
  close(3)
  CALL graf2()
end Program mul

subroutine graf1() !subrutina que grafica
  implicit none !Quitar variables implícitas
  REAL::x, y, f, h, a, b !Variables de tipo real
  INTEGER::N, i !Variables de tipo entero
  open(unit=1, file="Datos.dat", status="unknown") !Abrir archivo para guardar puntos
  open(unit=2, file="Graf.plt", status="unknown") !Abrir archivo para guardar información de la gŕafica
  WRITE(*, *) "Inicio de la gráfica" !Preguntar inicio de gráfica
  READ(*, *) a !Ingresar inicio de la gráfica
  WRITE(*, *) "Final de la gráfica" !Preguntar final de la gráfica
  READ(*, *) b !Ingresar final de la gráfica
  WRITE(*, *) "¿Con cuántos puntos?" !Preguntar número de puntos
  READ(*, *) N !Ingresar número de puntos
  h=(b-a)/N !Tamaño de paso
  do i=0, N !Construir datos
     x=a+i*h 
     y=f(x)
     WRITE(1, *) x, y !Escribir datos en archivo
  end do
  close(1)
  !Establecer todos los parámetros de la gráfica
  WRITE(2, *) "set title 'y=x³-3x-12'"
  WRITE(2, *) "set xlabel 'x'"
  WRITE(2, *) "set ylabel 'y'"
  WRITE(2, *) "m='./Datos.dat'"
  WRITE(2, *) "set nokey"
  WRITE(2, *) "set grid"
  WRITE(2, *) "p m w l"
  WRITE(2, *) "replot"
  close(2)
  CALL SYSTEM("gnuplot -p Graf.plt") !Llamar a GNUPlot desde la terminal
end subroutine graf1

subroutine graf2() !subrutina que grafica
  implicit none !Quitar variables implícitas
  REAL::x, y, f, h, a, b !Variables de tipo real
  INTEGER::N, i !Variables de tipo entero
  open(unit=1, file="Datos.dat", status="unknown") !Abrir archivo para guardar puntos
  open(unit=2, file="Graf.plt", status="unknown") !Abrir archivo para guardar información de la gŕafica
  WRITE(*, *) "Inicio de la gráfica" !Preguntar inicio de gráfica
  READ(*, *) a !Ingresar inicio de la gráfica
  WRITE(*, *) "Final de la gráfica" !Preguntar final de la gráfica
  READ(*, *) b !Ingresar final de la gráfica
  WRITE(*, *) "¿Con cuántos puntos?" !Preguntar número de puntos
  READ(*, *) N !Ingresar número de puntos
  h=(b-a)/N !Tamaño de paso
  do i=0, N !Construir datos
     x=a+i*h 
     y=f(x)
     WRITE(1, *) x, y !Escribir datos en archivo
  end do
  close(1)
  !Establecer todos los parámetros de la gráfica
  WRITE(2, *) "set title 'y=x³-13x-12'"
  WRITE(2, *) "set xlabel 'x'"
  WRITE(2, *) "set ylabel 'y'"
  WRITE(2, *) "m='./Datos.dat'"
  WRITE(2, *) "n='./Punto.dat'"
  WRITE(2, *) "set nokey"
  WRITE(2, *) "set grid"
  WRITE(2, *) "p m w l, n"
  WRITE(2, *) "set term png"
  WRITE(2, *) "set out 'Graf.png'"
  WRITE(2, *) "replot"
  close(2)
  CALL SYSTEM("gnuplot -p Graf.plt") !Llamar a GNUPlot desde la terminal
end subroutine graf2

function f(x)
  implicit none
  REAL::f, x
  f=x**3-13*x-12
end function f



