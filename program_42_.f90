program evaluate_y
implicit none
integer::si
real::y,ans,x
do
print*,"Enter the serial no:/=0"
read*,si
if(si==0) exit
print*,"Enter the value of x"
read*,x
ans=y(x)
print*,"The value of y=",ans
!print 50,ans
!50 format(X,f7.2)
end do
end program evaluate_y

real function y(x)
implicit none
real::x
if(x<2) then
y=2*x**2+3*x+4
else if(x==2) then
y=0
else 
y=2*x**2+3*x-4
end if
end function y	     