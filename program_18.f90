program sum_of_expression_02
implicit none
integer::si,i,n
real::sum,a,b
logical::flag
do
print*,"Enter the serial no:/=0"
read*,si
if(si==0) exit
print*,"Enter the value of n,a,b"
read*,n,a,b
flag=.true.
sum=0.0
do i=1,n
if((a+(i-1)*b)==0) then
flag=.false.
exit
else
sum=sum+i/(a+(i-1)*b)
end if
end do
if(flag) then
print*,"Summation=",sum
else 
print*,"It is undefined"
end if
end do
end program sum_of_expression_02