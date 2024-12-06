program sum_of_expression_02
implicit none
integer::si,i,n
real::sum
do
print*,"Enter the serial no:/=0"
read*,si
if(si==0) exit
print*,"Enter the value of n"
read*,n
sum=0.0
do i=1,n
sum=sum+(1.0/i**2)
end do
print*,"Summation=",sum
end do
end program sum_of_expression_02