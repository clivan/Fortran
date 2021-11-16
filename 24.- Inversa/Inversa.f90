!Autor: Claudio Iván Esparza Castañeda
!Título: Eliminación de Gauss
!Descripción: Programa que resuelve un sistema de ecuaciones empleando el método de eliminación de  Gauss
!Fecha: 16/12/2018

Program Gaussito
  implicit none
  INTEGER::i, j, k,  N, M
  REAL::S, sum
  REAL, allocatable, dimension(:, :)::A, B
  REAL, allocatable, dimension(:)::X
  
  open(unit=14, file="Matriz.dat", status="old")
  N=0
  do i=1, 10000
     read(14, *, end=15) S
     N=N+1
  end do
15 close(14)
  write(*, 1) N, N
1 format("El archivo contiene un sistema de", I2, " X", I2)
  
  allocate(A(1:N, 1:N), B(1:N, 1:N))
  do i=1, n
     do j=1, n
        if (i==j) then
           B(i, j)=1.0
        end if
     end do
  end do
     
  WRITE(*, *) "Matriz original"
  open(42, file="Matriz.dat", status="old")
  do i=1, N
     read(42, *) A(i, :)
     write(*, *) A(i, :)
  end do
  close(42)
  
  do k=1, N-1
     do i=k+1, N
        S=A(i, k)/A(k, k)
        do j=k+1, N
           A(i, j)=A(i, j)-A(k, j)*S
           B(i, j)=A(i, j)-A(k, j)*S
        end do
        A(i, k)=0.0d0
        B(i, k)=0.0d0
        A(i, N)=A(i, N)-S*A(k, N)
        B(i, N)=A(i, N)-S*A(k, N)
     end do
  end do
  write(*, *) "Matriz invertida"
  do i=1, n
     WRITE(*, *) B(i, :)
  end do
  deallocate(A, B)
end Program Gaussito



