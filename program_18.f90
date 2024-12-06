program sum_of_expression_02
implicit none
integer::si,i,n
real::sum,a,b
do
print*,"Enter the serial no:/=0"
read*,si
if(si==0) exit
print*,"Enter the value of n,a,b"
read*,n,a,b
sum=0.0
do i=1,n
sum=sum+i/(a+(i-1)*b)
end do
print*,"Summation=",sum
end do
end program sum_of_expression_02