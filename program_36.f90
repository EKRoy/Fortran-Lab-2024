program matrix_Sum_Subtraction_Multiplication
implicit none
integer::Arow,Brow,Acol,Bcol,i,j,k,si
integer,allocatable,dimension(:,:)::A,B,C,D,E
do
print*,"Enter the serial no:/=0"
read*,si
if(si==0) exit
print*,"Enter A matrix size:(Arow,Acol)"
read*,Arow,Acol
print*,"Enter B matrix size:(Brow,Bcol)"
read*,Brow,Bcol

allocate(A(Arow,Acol),B(Brow,Bcol),C(Arow,Acol),D(Arow,Acol),E(Acol,Brow))

print*,"Enter the value of A row-wise:"
read*,((A(i,j),j=1,Acol),i=1,Arow)

print*,"Enter the value of B row-wise:"
read*,((B(i,j),j=1,Bcol),i=1,Brow)

if((Arow==Brow) .AND.(Acol==Bcol)) then
do i=1,Arow
	do j=1,Acol
		C(i,j)=A(i,j)+B(i,j)
		D(i,j)=A(i,j)-B(i,j)
	end do
end do
print*,"Sum of two matrices:"
do i=1,Arow
	print*,(C(i,j),j=1,Acol)
end do
print*,"Subtraction of two matrices:"
do i=1,Arow
	print*,(D(i,j),j=1,Acol)
end do
else 
print*,"its not possible sum and subtraction."
end if

if(Acol==Brow) then
E=0
do i=1,Arow
	do j=1,Bcol
		do k=1,Acol
		E(i,j)=E(i,j)+A(i,k)*B(k,j)
		end do
	end do
end do
 print*,"Multiplication of the matrices:"
 do i=1,Arow
	print*,(E(i,j),j=1,Bcol)
 end do
 else 
 print*,"Matrix dimensions do not match for multiplication. Ensure Acol == Brow."
 end if
 deallocate(A)
 deallocate(B)
 deallocate(C)
 deallocate(D)
 deallocate(E)
 end do
 end program matrix_Sum_Subtraction_Multiplication