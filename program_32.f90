program straight_line_03
implicit none
integer::i,si,n
real::m,c
real,dimension(100)::x,y
do
print*,"Enter the serial no:/=0"
read*,si
if(si==0) exit
print*,"Enter the value of n points"
read*,n
print*,"Enter the set of points xi,yi"
read*,(x(i),y(i),i=1,n)
call st_line(x,y,n,m,c)
print*,"Equation of straight line:","y=",m,"x+",c
end do
end program straight_line_03 

subroutine st_line(arr_x,arr_y,n,m,c)
implicit none
integer::i,n
real::arr_x(n),arr_y(n)
real::sumx,sumy,sumxy,sqrtx,upper,lower,m,c
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
c=(sumy-m*sumx)/n
end subroutine st_line