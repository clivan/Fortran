!Autor: Claudio Iván Esparza Castañeda
!Título: Matriz por escalar
!Descripción: Multiplica un escalar por una matriz
!Fecha: 30/03/2020

Program sumat
  implicit none
  REAL, allocatable, dimension(:, :)::A, B
  iNTEGER, dimension(0:1)::N
  REAL::r
  INTEGER::i, j, k
  WRITE(*, *) "Dimensión de la matriz (filas x columnas)"
  READ(*, *) N(0), N(1)
  allocate(A(1:N(0), 1:N(1)), B(1:N(0), 1:N(1)))
  WRITE(*, *) "Elementos de la matriz:"
  do i=1, N(0)
     do j=1, N(1)
        READ(*, *) A(i, j)
     end do
  end do
  WRITE(*, *) "Escalar"
  READ(*, *) r
  do i=1, N(0)
     do j=1, N(1)
        B(i, j)=r*A(i, j)
     end do
  end do
  WRITE(*, *) "Producto:"
  do i=1, N(0)
     WRITE(*, *) B(i, :)
  end do
  deallocate(A, B)
end Program sumat



