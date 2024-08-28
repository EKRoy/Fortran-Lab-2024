program matrix_multiplicaton
implicit none
integer::Arow,Brow,Acol,Bcol,i,j,k,si
integer,allocatable,dimension(:,:)::A,B,C
do
print*,"Enter the serial no:/=0"
read*,si
if(si==0) exit
print*,"Enter the A matrix size:(Arow,Acol)"
read*,Arow,Acol
print*,"Enter the B matrix size:(Brow,Bcol)"
read*,Brow,Bcol

allocate(A(Arow,Acol),B(Brow,Bcol),C(Arow,Bcol))

print*,"Enter the value of A row-wise:"
read*,((A(i,j),j=1,Acol),i=1,Arow)

print*,"Enter the value of B row-wise:"
read*,((B(i,j),j=1,Bcol),i=1,Brow)

if(Acol==Brow) then
C=0
do i=1,Arow
	do j=1,Bcol
		do k=1,Acol
		C(i,j)=C(i,j)+A(i,k)*B(k,j)
		end do
	end do
end do
 print*,"Multiplication of the matrices:"
 do i=1,Arow
	print*,(C(i,j),j=1,Bcol)
 end do
 else 
 print*,"Matrix dimensions do not match for multiplication. Ensure Acol == Brow."
 end if
 deallocate(A)
 deallocate(B)
 deallocate(C)
 end do
 end program matrix_multiplicaton