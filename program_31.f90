program straight_line_02
implicit none
integer::i,si,n
real::m,c,slope,intersect
real,allocatable,dimension(:)::x,y
do
print*,"Enter the serial no:/=0"
read*,si
if(si==0) exit
print*,"Enter the value of n points"
read*,n
allocate(x(n))
allocate(y(n))
print*,"Enter the set of points xi,yi"
do i=1,n
read*,x(i),y(i)
end do
m=slope(x,y,n)
c=intersect(x,y,n,m)
print*,"Equation of straight line:","Y=",m," x+",c
deallocate(x)
deallocate(y)
end do
end program straight_line_02
 
real function slope(x,y,n)
implicit none
integer::i
integer,intent(in)::n
real,intent(in)::x(n),y(n)
real::sumx,sumy,sumxy,sqrtx,upper,lower,m
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
slope=m
end function slope

real function intersect(x,y,n,m)
implicit none
integer::i
integer,intent(in)::n
real,intent(in)::x(n),y(n),m
real::sumx,sumy,c
sumx=0.0
sumy=0.0
do i=1,n
sumx=sumx+x(i)
sumy=sumy+y(i)
end do
c=(sumy-m*sumx)/n
intersect=c
end function intersect

 