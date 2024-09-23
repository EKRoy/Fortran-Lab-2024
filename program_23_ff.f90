program sum_of_series_03
implicit none
integer::si,i,n
real::sum,x
integer,allocatable,dimension(:)::fact
do
print*,"Enter the serial no:/=0"
read*,si
if(si==0) exit
print*,"Enter the value of x,n"
read*,x,n
allocate(fact(n))
if(n==0) then
print*,"Sum=0"
else
sum=1.0
fact(0)=1
do i=1,n-1
fact(i)=fact(i-1)*(2*i)*(2*i-1)
end do
do i=1,n-1
sum=sum+((-1)**i)*(x**(2*i))/fact(i)
end do
print*,"Sum of series:",sum	
end if
deallocate(fact) 
end do
end program sum_of_series_03