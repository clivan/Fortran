Program zztop
  implicit none
  REAL::S, P
  REAL, allocatable, dimension(:, :)::A
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
  allocate(A(1:N, 1:2))
  open(42, file="Datos.dat", status="old")
  do i=1, N
     read(42, *) A(i, :)
     write(*, *) A(i, :)
  end do
  write(*, *) "¿Qué valor quieres interpolar?"
  read(*, *) x
  S=0.0d0
  do i=0, N
     P=A(i, 2)
     do j=0, N
        if (i/=j) then
           P=P*((x-A(j, 1))/(A(i, 1)-A(j, 1)))
        end if
     end do
     S=S+P
  end do
  deallocate(A)
  write(*, *) S
end Program zztop

  
     
