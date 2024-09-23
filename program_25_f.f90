program sum_of_series_05
implicit none
integer::si,i,n,fact
real::sum,x
do
print*,"Enter the serial no:/=0"
read*,si
if(si==0) exit
print*,"Enter the value of x,n"
read*,x,n
if(n==0) then
Print*,"Sum=0"
else
sum=1.0
fact=1
do i=1,n-1
fact=fact*i
sum=sum+x**i/fact
end do
print*,"Sum of series:",sum
end if
end do
end program sum_of_series_05
