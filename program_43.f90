program quadratic
implicit none
real::a,b,c,d,e,xx1,xx2,Ex1,x1,x2,im1,r
print*,"Enter the value of a,b,c,d"
read*,a,b,c,d
if((a+b)==0) then
print*,"This is not quadratic equation."
else
e=c**2-4*(a+b)*d
if(e>0) then
print*,"This Equation has real and unequal roots"
xx1=x1(a,b,c,e)
xx2=x2(a,b,c,e)
print*,"x1=",xx1
print*,"x2=",xx2
else if(e==0) then
print*,"This equation has real and equal roots"
xx1=Ex1(a,b,c)
print*,"x1=",xx1
print*,"x2=",xx1
else 
print*,"This equation has imaginary roots."
xx1=im1(a,b,e)
xx2=r(a,b,c)
print*,"x1=",xx2," +i",xx1
print*,"x2=",xx2," -i",xx1
end if
end if
end program quadratic

real function x1(a,b,c,e)
implicit none
real,intent(in)::a,b,c,e
real::x
x=(-c+sqrt(e))/(2*(a+b))
x1=x
end function x1

real function x2(a,b,c,e)
implicit none
real,intent(in)::a,b,c,e
real::x
x=(-c-sqrt(e))/(2*(a+b))
x2=x
end function x2

real function Ex1(a,b,c)
implicit none
real,intent(in)::a,b,c
real::x
x=-c/(2*(a+b))
Ex1=x
end function Ex1

real function im1(a,b,e)
implicit none
real,intent(in)::a,b,e
real::x
x=sqrt(abs(e))/(2*(a+b))
im1=x
end function im1

real function r(a,b,c)
implicit none
real,intent(in)::a,b,c
real::x
x=-c/(2*(a+b))
r=x
end function r


 