program sum_of_series_03
implicit none
integer::si,i,n
real::sum1,sum2,sum,x
integer,allocatable,dimension(:)::fact1,fact2
do
print*,"Enter the serial no:/=0"
read*,si
if(si==0) exit
print*,"Enter the value of x,n"
read*,x,n
allocate(fact1(n))
allocate(fact2(n))
if(n==0) then
print*,"Sum=0"
else
sum1=x
fact1(0)=1
do i=1,n-1
fact1(i)=fact1(i-1)*(2*i)*(2*i+1)
end do
do i=1,n-1
sum1=sum1+((-1)**i)*(x**(2*i+1))/fact1(i)
end do
sum2=1
fact2(0)=1
do i=1,n-1
fact2(i)=fact2(i-1)*(2*i)*(2*i-1)
end do
do i=1,n-1
sum2=sum2+((-1)**i)*(x**(2*i))/fact2(i)
end do
sum=sum1/sum2
end if
print*,"Sum of series:",sum
print*,"sum1:",sum1
print*,"sum2:",sum2
deallocate(fact1)
deallocate(fact2) 
end do
end program sum_of_series_03