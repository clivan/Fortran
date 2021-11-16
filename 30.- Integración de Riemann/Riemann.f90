Program Riemann
  implicit none
  REAL::x, f, a, b, h, Int
  INTEGER::i, N
  N=10000
  a=0
  b=1
  h=(b-a)/N
  Int=0
  do i=1, N
     x=a+i*h
     Int=Int+f(x)*h
  end do
  write(*, *) Int
  
end Program Riemann

function f(x)
  implicit none
  REAL::f, x
  f=exp(-x**2)
end function f

   
