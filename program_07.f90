program quadratic_equation
implicit none 
integer::si
real::a,b,c,d,e,r,x1,x2,im1,im2
do
print*,"Enter the serial no:/=0"
read*,si
if(si==0) exit
print*,"Enter the value of a,b,c and d"
read*,a,b,c,d
if((a+b)==0) then
print*,"This is not quadratic equation."
else
e=c**2-4*(a+b)*d
if(e>0) then 
x1=(-c+sqrt(e))/(2*(a+b))
x2=(-c-sqrt(e))/(2*(a+b))											    
print*,"the equation has two real and unequal roots"
print*,"x1=",x1
print*,"x2=",x2
else if(e==0)then 
x1=-c/(2*(a+b))
x2=x1
print*,"this equation has two equal and real roots"
print*,"x1=",x1
print*,"x2=",x2
else
im1=sqrt(abs(e))/(2*(a+b))
im2=im1
r=-c/(2*(a+b))
print*,"the equation has complex roots"
print*,"x1=",r,"+i",im1
print*,"x2=",r,"+i",im2
end if 
end if
end do
end program quadratic_equation
