!Autor: Claudio Iván Esparza Castañeda
!Título: Multiplicación de matrices
!Descripción: Multiplica dos matrices
!Fecha: 30/03/2020

Program mulmat
  implicit none
  REAL, allocatable, dimension(:, :)::A, B, C
  INTEGER, dimension(0:1)::N, M
  REAL::s, t
  INTEGER::i, j, k
  WRITE(*, *) "Dimensión de la primera matriz (filas x columnas)"
  READ(*, *) N(0), N(1)
  WRITE(*, *) "Dimensión de la segunda matriz (filas x columnas)"
  READ(*, *) M(0), M(1)
  if ((N(1) .eq. M(0))) then
     allocate(A(1:N(0), 1:N(1)), B(1:M(0), 1:M(1)), C(1:N(0), 1:M(1)))
     WRITE(*, *) "Elementos de la primera matriz:"
     do i=1, N(0)
        do j=1, N(1)
           READ(*, *) A(i, j)
        end do
     end do
     WRITE(*, *) "Elementos de la segunda matriz:"
     do i=1, N(0)
        do j=1, N(1)
           READ(*, *) B(i, j)
        end do
     end do
     do i=1, N(0)
        do j=1, M(1)
           s=0
           do k=1, M(1)
              s=s+A(i, k)*B(k, j)
           end do
           C(i, j)=s
        end do
     end do
     WRITE(*, *) "Matriz C"
     do i=1, M(1)
        WRITE(*, *) C(i, :)
     end do
     deallocate(A, B, C)
  end if
end Program mulmat



