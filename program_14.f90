program expression_product
implicit none
integer::si,n,i
real::prod
do
print*,"Enter the seral no:/=0"
read*,si
if(si==0) exit
print*,"Enter the value of n"
read*,n
prod=1
do i=1,n
 prod=prod*((2.0*i-1)/(2.0*i))
 end do
 print*,"Product=",prod
 end do
 end program expression_product
