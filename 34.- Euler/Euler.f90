Program Leonhard
  implicit none
  REAL::a, b, h, x, f, y
  INTEGER::i, N
  N=1000
  a=0
  b=10
  h=(b-a)/N
  y=1
  do i=1, N
     x=a+i*h
     write(90, *) x, y
     y=y+h*f(x, y)
  end do
  
end Program Leonhard

function f(x, y)
  implicit none
  REAL::x, y, f
  f=-y-3*exp(-x)*sin(3*x)
end function f


