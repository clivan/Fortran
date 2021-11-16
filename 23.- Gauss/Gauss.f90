!Autor: Claudio Iván Esparza Castañeda
!Título: Eliminación de Gauss
!Descripción: esuelve un sistema de ecuaciones empleando el método de eliminación de  Gauss
!Fecha: 28/04/2020

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
  
  M=N+1
  
  allocate(A(1:N, 1:M), B(1:N, 1:M), X(1:N))
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
        end do
        A(i, k)=0.0d0
        A(i, M)=A(i, M)-S*A(k, M)
     end do
  end do
  
  X(N)=A(N, M)/A(N, N)
  
  do i=N-1, 1, -1
     sum=A(i, M)
     do j=i+1, N
        sum=sum-A(i, j)*X(j)
     end do
     X(i)=sum/A(i, i)
  end do

  
  write(*, *)
  write(*, *) X
  deallocate(A, B, X)
end Program Gaussito



