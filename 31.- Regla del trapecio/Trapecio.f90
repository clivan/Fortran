Program itsatrap
  implicit none
  REAL::x, f, a, b, Int, h
  INTEGER::i, N
  N=100000
  a=0
  b=1
  Int=0
  h=(b-a)/N
  do i=1, N
     x=a+i*h
     Int=Int+f(x)
  end do
  Int=(b-a)*(f(a)+2*Int+f(b))/(2*N)
  write(*, *) Int
end Program itsatrap

function f(x)
  implicit none
  REAL::x, f
  f=exp(-x**2)
end function f
