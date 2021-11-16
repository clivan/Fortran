Program bart
  implicit none
  REAL::f, x, a, b, h, Int, S1, S2
  INTEGER::i, N
  N=1000
  a=0
  b=1
  h=(b-a)/N
  S1=0
  S2=0
  Int=0
  do i=1, N-1, 2
     x=a+i*h
     S1=S1+f(x)
  end do
  do i=2, N-1, 2
     x=a+i*h
     S2=S2+f(x)
  end do
  Int=h*(f(a)+4*S1+2*S2+f(b))/3
  write(*, *) Int
end Program bart

function f(x)
  implicit none
  REAL::f, x
  f=exp(-x**2)
end function f

