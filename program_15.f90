program product_of_expression_02
implicit none
integer::i,n,si
real::prod,a,b
logical::flag
do
print*,"Enter the serial no:/=0"
read*,si
if(si==0) exit
print*,"Enter the value of n,a,b"
read*,n,a,b
flag=.true.
prod=1.0
do i=1,n
if((a+i*b)==0) then
flag=.false.
exit
end if
prod=prod*(i/(a+i*b))
end do
if(flag) then
print*,"Product=",prod
else
print*,"This is undefined."
end if
end do
end program product_of_expression_02