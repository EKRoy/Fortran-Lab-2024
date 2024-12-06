program product_of_expression_02
implicit none
integer::si,i,n
real::prod,a,b,r
do
prInt*,"Enter the serial no:/=0"
read*,si
if(si==0) exit
print*,"Enter the value of n,a,b,r"
read*,n,a,b,r
prod=1.0
do i=1,n
prod=prod*(i*b)/(r+(i-1)*a)**i
end do
print*,"Product=",prod
end do
end program product_of_expression_02