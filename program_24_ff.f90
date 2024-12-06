program sum_of_series_tanx
implicit none
integer::si,i,n
real::sum1,sum2,fact,ans,x
real,parameter:: pi=3.1416
do
print*,"Enter the serial no:/=0"
read*,si
if(si==0) exit
 print*,"Enter the value of x(degree)"
 read*,x
 print*,"Enter the value of n"
 read*,n
 if(n==0) then
 print*,"Sum=0"
 else
 x=pi*(x/180)
 sum1=x
 fact=1.0
 do i=1,n-1
	fact=fact*(2*i)*(2*i+1)
	sum1=((-1)**(i)*x**(2*i+1))/fact+sum1
 end do
  sum2=1
  fact=1.0
 do i=1,n-1
  fact=fact*(2*i)*(2*i-1)
  sum2=((-1)**(i)*x**(2*i))/fact+sum2
 end do
 ans=sum1/sum2
 print*,"Sum of series :",ans
 end if
 end do
end program sum_of_series_tanx
