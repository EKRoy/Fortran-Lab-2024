program triangle_02
implicit none
real::a,b,c,theta,rad_theta,area,perimeter
integer::si
real,parameter::pi=3.1416
do
print*,"Enter the serial no:/0"
read*,si
 if(si==0)exit

print*,"Enter the value of a,b and theta"
read*,a,b,theta
if(theta/=0) then
rad_theta=theta*(pi/180.0)
c=sqrt(a**2+b**2-2.0*a*b*cos(rad_theta))
area=0.5*a*b*sin(rad_theta)
perimeter=a+b+c
print*,"Area of triangle=",area
print*,"perimeter=",perimeter
else 
 print*,"this is not a triangle"
end if
end do
end program triangle_02 
