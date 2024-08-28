program matrix_sub
implicit none
integer::Arow,Brow,Acol,Bcol,i,j,si
integer,allocatable,dimension(:,:)::A,B,C
do
print*,"Enter the serial no:/=0"
read*,si
if(si==0) exit
print*,"Enter the matrix size:Arow,Acol,Brow,Bcol"
read*,Arow,Acol,Brow,Bcol
allocate(A(Arow,Acol),B(Brow,Bcol),C(Brow,Bcol))

print*,"Enter the value of A row wise:"
read*,((A(i,j),j=1,Acol),i=1,Arow)

print*,"Enter the value of B row wise:"
read*,((B(i,j),j=1,Bcol),i=1,Brow)

if((Arow==Brow) .AND.(Acol==Bcol)) then
do i=1,Arow
	do j=1,Acol
		C(i,j)=A(i,j)-B(i,j)
	end do
end do
 print*,"Subtraction of the Matrix:"
 do i=1,Arow
	print*,(C(i,j),j=1,Acol)
 end do
 else 
 print*,"this is not possible to subtraction"
 end if
 deallocate(A)
 deallocate(B)
 deallocate(C)
 end do
 end program matrix_sub