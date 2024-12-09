program product_of_expression_02
implicit none
integer::si,i,n
real::prod,a,b,r
logical::flag
do
print*,"Enter the serial no:/=0"
read*,si
if(si==0) exit
print*,"Enter the value of n,a,b,r"
read*,n,a,b,r
 flag=.true.
prod=1.0
do i=1,n
if((r+(i-1)*a)==0) then
flag=.false.
exit
else
prod=prod*(i*b)/(r+(i-1)*a)**i
end if
end do
if(flag) then
print*,"Product=",prod
else
print*,"It is Undefined"
end if
end do
end program product_of_expression_02