program sum_of_exp_05
implicit none
integer::i,si,n
real::sum,x
do
print*,"Enter the serial no:/=0"
read*,si
if(si==0) exit
print*,"Enter the value of x,n"
read*,x,n
if(abs(x)>0.5) then
print*,"Its Undefined,Because the value of x is greater than 1/2"
else
sum=0.0
do i=1,n
sum=sum+(2*x)**(i-1)
end do
print*,"Sum of series:",sum
end if
end do
end program sum_of_exp_05