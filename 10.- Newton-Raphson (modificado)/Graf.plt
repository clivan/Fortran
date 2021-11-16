 set title 'y=sin(x)-cos(x)'
 set xlabel 'x'
 set ylabel 'y'
 m='./Datos.dat'
 n='./Punto.dat'
 set nokey
 set grid
 p m w l, n
 set term png
 set out 'Graf.png'
 replot
