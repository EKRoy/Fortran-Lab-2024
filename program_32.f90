program straight_line_03
implicit none
integer::i,si,n
real::m,c
real,dimension(:),allocatable::x,y
do
print*,"Enter the serial no:/=0"
read*,si
if(si==0) exit
print*,"Enter the value of n points"
read*,n
allocate(x(n))
allocate(y(n))
print*,"Enter the set of points xi,yi"
read*,(x(i),y(i),i=1,n)
call st_line(x,y,n,m,c)
print*,"Equation of straight line:","y=",m,"x+",c
deallocate(x)
deallocate(y)
end do
end program straight_line_03 

subroutine st_line(x,y,n,m,c)
implicit none
integer,intent(in)::n
integer::i
real,intent(in)::x(n),y(n)
real,intent(out)::m,c
real::sumx,sumy,sumxy,sqrtx,upper,lower
sumx=0.0
sumy=0.0
sumxy=0.0
sqrtx=0.0
do i=1,n
sumx=sumx+x(i)
sumy=sumy+y(i)
sumxy=sumxy+x(i)*y(i)
sqrtx=sqrtx+x(i)*x(i)
end do
upper=n*sumxy-sumx*sumy
lower=n*sqrtx-sumx*sumx
m=upper/lower
c=(sumy-m*sumx)/n
end subroutine st_line