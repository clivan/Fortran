Program oscilador
  implicit none
  double precision, parameter::ti=0.0 , tf=10.0
  integer, parameter::N=1000
  double precision, parameter::x0=6.6d0, v0=0.0
  doubleprecision::t, x, y, f, g, h
  doubleprecision::k1, k2, k3, k4, l1, l2, l3, l4
  integer :: i
  open(01, file="Datos.dat", status="unknown")
  h=(tf-ti)/N
  x=x0
  y=v0
  do i=0,1000
     t=ti+i*h
     k1=h*f(t, x, y)
     l1=h*g(t, x, y)
     k2=h*f(t+h*0.5d0, x+k1*0.5d0, y+L1*0.5d0)
     l2=h*g(t+h*0.5d0, x+k1*0.5d0, y+L1*0.5d0)
     k3=h*f(t+h*0.5d0, x+k2*0.5d0, y+L2*0.5d0 )
     l3=h*g(t+h*0.5d0, x+k2*0.5d0, y+L2*0.5d0 )
     k4=h*f(t+h , x+k3, y+L3 )
     l4=h*g(t+h , x+k3, y+L3 )
     x=x+(k1+k2+k2+k3+k3+k4)/6.0d0;
     y=y+(L1+L2+L2+L3+L3+L4)/6.0d0;
     write(01, *) t, x, y
  end do
end Program oscilador

function g(t, x, y)
  double precision::g, t, x, y, a, c
  !g=-x + t*x*y*0.0
  a=-9.932e-6
  c=1.575e-6
  g=a*sin(x)+c*sin(2*x)
end function g

function f(t, x, y)
  double precision::f, t, x, y
  f=0.0
end function f
