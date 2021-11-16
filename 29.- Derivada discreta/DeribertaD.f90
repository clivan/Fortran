Program DeriD
  implicit none
  REAL::S, P
  REAL, allocatable, dimension(:, :)::A
  REAL, allocatable, dimension(:)::B
  INTEGER::i, j, x, N
  open(unit=14, file="Datos.dat", status="old")
  N=0
  do i=1, 10000
     read(14, *, end=15) S
     N=N+1
  end do
15 close(14)
  write(*, *) "El archivo contiene", N, " pares de datos."
  write(*, *)
  allocate(A(1:N, 1:2), B(1:N))
  open(42, file="Datos.dat", status="old")
  do i=1, N
     read(42, *) A(i, :)
     write(*, *) A(i, :)
  end do

  do i=1, N
     B(i)=(A(i, 2)-A(i+1, 2))/(A(i, 1)-A(i+1, 1))
  end do
  write(*, *) 
  do i=1, N
     write(*, *) A(i, :), B(i)
  end do
  deallocate(A, B)
end Program DeriD
