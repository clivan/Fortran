 set title 'y=sin(x)-cos(x)'
 set xlabel 'x'
 set ylabel 'y'
 m='./Datos.dat'
 set nokey
 set grid
 p m w l
 set term png
 set out 'Graf.png'
 replot
