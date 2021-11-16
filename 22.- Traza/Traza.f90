!Autor: Claudio Iván Esparza Castañeda
!Título: Traza
!Descripción: Obtiene la traza de una matriz cuadrada
!Fecha: 16/04/2020

Program tra
  implicit none
  REAL, allocatable, dimension(:, :)::A, B
  INTEGER::N
  REAL::s
  INTEGER::i, j, k
  WRITE(*, *) "Dimensión de la matriz (número de filas igual al número de columnas)"
  READ(*, *) N
  allocate(A(1:N, 1:N))
  WRITE(*, *) "Elementos de la matriz:"
  do i=1, N
     do j=1, N
        READ(*, *) A(i, j)
     end do
  end do
  s=0
  do i=1, N
     do j=1, N
        if (i .eq. j) then
           s=s+A(i, j)
        end if
     end do
  end do
  WRITE(*, *) "Traza:"
  WRITE(*, *) s
  deallocate(A)
end Program tra




