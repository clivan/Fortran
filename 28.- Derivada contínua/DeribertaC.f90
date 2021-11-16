Program DeriC
  implicit none
  REAL::x, f, fp, a, b, h
  INTEGER, parameter::N=100
  REAL, dimension(1:N, 1:3)::C
  INTEGER::i
  a=0
  b=10
  h=(b-a)/N
  do i=1, N
     x=a+i*h
     C(i, 1)=x
     C(i, 2)=f(x)
  end do

  do i=1, N
     C(i, 3)=fp(C(i, 1), C(i+1, 1), C(i, 2), C(i+1, 2))
  end do
  C(N, 3)=0.0d0
  do i=1, N
     write(*, *) C(i, :)
  end do
  
  

end Program DeriC

function f(x)
  implicit none
  REAL::x, f
  f=sin(x)
end function f

function fp(xi, xf, yi, yf)
  implicit none
  REAL::xi, xf, yi, yf, fp
  fp=(yf-yi)/(xf-xi)
end function fp
