program sum_of_series_03
implicit none
integer::si,i,n
real::sum,x,fact
real,parameter ::pi=3.1416
do
print*,"Enter the serial no:\=0"
read*,si
if(si==0) exit
print*,"Enter the value of x,n"
read*,x,n
if(n==0) then
print*,"Sum=0"
else
x=(pi*x)/180
fact=1.0
sum=1
do i=1,n-1
fact=fact*(2*i)*(2*i-1)
sum=sum+((-1)**i)*(x**(2*i))/fact
end do
print*,"Sum of series:",sum	
end if 
end do
end program sum_of_series_03