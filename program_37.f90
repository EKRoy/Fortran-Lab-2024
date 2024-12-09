program matrix_det
implicit none
integer::i,j,si,n
real(8):: det
real(8),dimension(:,:),allocatable::A
do
print*,"Enter the serial no:/=0"
read*,si
if(si==0) exit
print*,"Enter the matrix size(n):"
read*,n
allocate(A(n,n))
print*,"Enter the matrix elements Row-Rise"
read*,((A(i,j),j=1,n),i=1,n)
if(n==1) then
print*,"Determinant=",A(1,1)
else if(n==2) then
det=A(1,1)*A(2,2)-A(1,2)*A(2,1)
print*,"Determinant=",det
else if(n==3) then
det=A(1,1)*(A(2,2)*A(3,3)-A(2,3)*A(3,2))-A(1,2)*(A(2,1)*A(3,3)-A(2,3)*A(3,1))+A(1,3)*(A(2,1)*A(3,2)-A(2,2)*A(3,1))
print*,"Determinant=",det
else 
print*,"Please Enter the size n=1 or 2 or 3"
end if 
deallocate(A)
end do
end program