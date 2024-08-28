real function mm(arr_x,arr_y,n)
implicit none
integer::i,n
real::arr_x(n),arr_y(n)
real::sumx,sumy,sumxy,sqrtx,upper,lower,m
sumx=0.0
sumy=0.0
sumxy=0.0
sqrtx=0.0
do i=1,n
sumx=sumx+arr_x(i)
sumy=sumy+arr_y(i)
sumxy=sumxy+arr_x(i)*arr_y(i)
sqrtx=sqrtx+arr_x(i)*arr_x(i)
end do
upper=n*sumxy-sumx*sumy
lower=n*sqrtx-sumx*sumx
m=upper/lower
mm=m
end function mm

real function cc(arr_x,arr_y,n,m)
implicit none
integer::i,n
real::arr_x(n),arr_y(n)
real::sumx,sumy,c,m
sumx=0.0
sumy=0.0
do i=1,n
sumx=sumx+arr_x(i)
sumy=sumy+arr_y(i)
end do
c=(sumy-m*sumx)/n
cc=c
end function cc

program straight_line_02
implicit none
integer::i,si,n
real::m1,c1,mm,cc
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
m1=mm(x,y,n)
c1=cc(x,y,n,m1)
print*,"Equation of straight line:","Y=",m1,"x+",c1
deallocate(x)
deallocate(y)
end do
end program straight_line_02  