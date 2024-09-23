program inner_circle
implicit none
integer::si
real::x1,x2,x3,y1,y2,y3,a,b,c,s,r,area_triangle,perimeter,area_circle,circum
real,parameter:: pi=3.141592
do 
print*,"Enter the serial no:/=0"
read*,si
if(si==0) exit
print*,"Enter the value of x1,y1,x2,y2,x3,y3"
read*,x1,y1,x2,y2,x3,y3
a=sqrt((x1-x2)**2.0+(y1-y2)**2.0)
b=sqrt((x2-x3)**2.0+(y2-y3)**2.0)
c=sqrt((x3-x1)**2.0+(y3-y1)**2.0)
if((a+b)>c .and. (b+c)>a .and. (a+c)>b) then 
perimeter=a+b+c
s=(a+b+c)/2.0
area_triangle=sqrt(s*(s-a)*(s-b)*(s-c))
r=(2.0*area_triangle)/perimeter
area_circle=pi*(r**2)
circum=2*pi*r
print*,"Area of Circle=",area_circle
print*,"Circumference=",circum
else 
print*,"Does not from a triangle"
end if
end do
end program inner_circle