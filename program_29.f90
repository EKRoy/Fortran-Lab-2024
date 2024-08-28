program sum_of_series_09
implicit none
integer::si,i,n
real::sum,x,b
do
print*,"Enter the serial no:/=0"
read*,si
if(si==0) exit
print*,"Enter the value of x,b,n"
read*,x,b,n
if(abs(x)>(1/abs(b))) then
print*,"It is not exist.Because absolute value of x is greater than 1"
else
sum=0.0
do i=1,n
sum=(-1)**(i+1)*(b*x)**(i-1)+sum
end do
print*,"Summation=",sum
end if
end do
end program sum_of_series_09
