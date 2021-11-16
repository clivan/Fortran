Program MCL
  implicit none
  REAL::Sx, Sy, Sxy, Sx2, a, b, S
  REAL, allocatable, dimension(:, :)::D
  INTEGER::i, N
  open(unit=14, file="Datos.dat", status="old")
  N=0
  do i=1, 10000
     read(14, *, end=15) S
     N=N+1
  end do
15 close(14)
  write(*, *) "El archivo contiene", N, " pares de datos."
  Sx=0.0d0
  Sy=0.0d0
  Sxy=0.0d0
  Sx2=0.0d0
  allocate(D(1:N, 1:2))
  open(42, file="Datos.dat", status="old")
  do i=1, N
     read(42, *) D(i, :)
     write(*, *) D(i, :)
  end do
  do i=1, N
     Sx=Sx+D(i, 1)
     Sy=Sy+D(i, 2)
     Sxy=Sxy+D(i, 1)*D(i, 2)
     Sx2=Sx2+D(i, 1)**2
  end do
  deallocate(D)
  a=(N*Sxy-Sx*Sy)/(N*Sx2-Sx**2)
  b=(Sy-a*Sx)/N
  write(*, *)
  write(*, *) a, b
end Program MCL

  
