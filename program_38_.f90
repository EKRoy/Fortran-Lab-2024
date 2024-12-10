program matrix_sum
implicit none
integer::n,i,j,m
real(8)::detA
real(8),allocatable,dimension(:,:)::A,inv,adj,adj_trans
print*,"Enter the n,m"
read*,n,m
allocate(A(n,m),inv(n,m),adj(n,m),adj_trans(m,n))

print*,"Enter the value of A row wise:"
read*,((A(i,j),j=1,m),i=1,n)
   print*,"The transpose Matrix:"
do i=1,n
 print*,(A(j,i),j=1,m)
 end do

	if(n/=m) then
	print*,"The matrix is not square matrix.So we not find to inverse:"
	stop
	end if
	if(n==1)then
	if(A(1,1)==0.0) then
	print*,"Matrix is Singular and cannot be inverted"
	else
	print*,"The inverse matrix is:",1.0/A(1,1)
	end if
	else if(n==2) then
	detA=A(1,1)*A(2,2)-A(1,2)*A(2,1)
	  if(detA==0.0) then
	    print*,"Matrix is Singular and cannot be inverted"
	   else
	     adj(1,1)=A(2,2)
		 adj(1,2)=-A(2,1)
		 adj(2,1)=A(1,2)
		 adj(2,2)=-A(1,1)

 do i=1,n
   do j=1,m
	adj_trans(i,j)=adj(j,i)
	end do
	end do
 inv=adj_trans/detA
 print*,"The inverse Matrix is:"
 do i=1,n
	print*,(inv(i,j),j=1,m)
	end do
	 end if
	else
 detA=A(1,1)*(A(2,2)*A(3,3)-A(2,3)*A(3,2))&
	  -A(1,2)*(A(2,1)*A(3,3)-A(2,3)*A(3,1)) &
	  +A(1,3)*(A(2,1)*A(3,2)-A(2,2)*A(3,1))

if(detA==0.0) then
print*,"Matrix is Singular and cannot be inverted"
else
adj(1,1) = A(2,2)*A(3,3) - A(2,3)*A(3,2) 
adj(1,2) = -(A(2,1)*A(3,3) - A(2,3)*A(3,1)) 
adj(1,3) = A(2,1)*A(3,2) - A(2,2)*A(3,1) 
adj(2,1) = -(A(1,2)*A(3,3) - A(1,3)*A(3,2)) 
adj(2,2) = A(1,1)*A(3,3) - A(1,3)*A(3,1) 
adj(2,3) = -(A(1,1)*A(3,2) - A(1,2)*A(3,1)) 
adj(3,1) = A(1,2)*A(2,3) - A(1,3)*A(2,2) 
adj(3,2) = -(A(1,1)*A(2,3) - A(1,3)*A(2,1))
 adj(3,3) = A(1,1)*A(2,2) - A(1,2)*A(2,1)

  do i=1,n
   do j=1,m
	adj_trans(i,j)=adj(j,i)
	end do
	end do
 inv=adj_trans/detA
 print*,"The inverse Matrix is:"
 do i=1,n
	print*,(inv(i,j),j=1,m)
  end do 
	end if
	end if
 end program matrix_sum
