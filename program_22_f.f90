program sum_of_series_02
implicit none
integer::si,i,n
real::sum,x
real,parameter ::pi=3.1414
integer,allocatable,dimension(:)::a
do
print*,"Enter the serial no:\=0"
read*,si
if(si==0) exit
print*,"Enter the value of x,n"
read*,x,n
allocate(a(n))
if(n==0) then
print*,"Sum=0"
else
x=(pi*x)/180
sum=x
a(0)=1
do i=1,n-1
a(i)=a(i-1)*(2*i)*(2*i+1)
end do
do i=1,n-1
sum=sum+((-1)**i)*(x**(2*i+1))/a(i)
end do
print*,"Sum of series:",sum	
end if
deallocate(a) 
end do
end program sum_of_series_02