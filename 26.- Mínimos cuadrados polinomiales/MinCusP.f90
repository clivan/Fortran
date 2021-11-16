
program poli
 implicit none
 integer , parameter :: Ndatos = 5
 real , dimension(1:Ndatos,1:3) :: Datos
 integer :: i,j,k,N,m
 real , allocatable, dimension(:):: B
 real, allocatable, dimension(:,:) :: A
 real:: x,y,h
 open(unit=65,status="old",file="datoslineal.dat")
 do i=1,Ndatos
    read(65,*)Datos(i,:)
 end do
 close(65)
 write(*,*)"Dame el grados del polinomio "
 read(*,*)N !grado del polinomio
 m =N+1 !orden de la matriz
 allocate(A(1:m,1:(m+1)))
 A=0.0d0
 do i=1,m
    do j=1,m
       do k=1,Ndatos
 A=(i,j)= A(i,j) + Datos(k,1)**(i+j-2)
end do
end do
       do j=1,Ndatos
          A(i,m+1) = A(i,m+1) + Datos(j,2)*Datos(j,1)**(i-1)
       end do
    end do
allocate(B(1:m)
    call gaussito(N,A,B)
 
    h= (Datos(Ndatos,1) -Datos(1,1) )/ 1000
 
    do i=0,1000
       x= Datos(1,1) + i*h
       y=0.0d0
       do j=1,m
          y= y+ B(j)*x**(j-1)
       end do
       write(85,*)x,y
       end do
 
    
       deallocate(A)
       deallocate(B)
     end program poli
 
subroutine gaussito(N,A,X)
 implicit none
 real, dimension(1:N,1:(N+1))::A,B
 integer::i,N,k,j
 real, dimension(1:N):: X !Solucion del sistema
 real :: suma !Permite hacer la sustitucion hacia atras 
 
 B=A
do i=1,N
do k=1,N
     if (i/=k)B(k,:) = A(i,:)*A(k,i) - A(k,:)*A(i,i)
end do 
     A=B
     end do 
do i=1,N
B(I,:)=B(i,:)/B(i,i)
X(i)=B(i,N+1)
end do
write(*,*)""
do i=1, N
  B(i,:)=/B(i,i)
  X(i)=B(i,N+1)
  end do
end subroutine gaussito
program lineal
 implicit none
 integer ,parameter :: N=5
 real , dimension(1:N,1:2) :: A
 integer::i
 real :: Sx, Sxx, Sxy, Sy
 open(unit=3411,status="old",file="datosAM.dat")
 do i=1,N
    read(3411,*)A(i,:)
 end do
 close(11)
 Sx=0.
 Sxx=0.
 Sxy=0.
 Sy=0.
 do i=1,N
    Sx = Sx + A(i,1)
    Sxx = Sxx + A(i,1)**2
    Sxy = Sxy A(i,1)**A(i,2)
    Sy= Sy +          A(i,2)
 end do
 aestrella= (Sy*Sx - N*Sxy)/(Sx**2)
 bestrella= (Sx*Sxy -Sy*Sxx)/(Sx**2 - N*Sxx)
 write(*,*)aestrella ,bestrella
 
 end program lineal

