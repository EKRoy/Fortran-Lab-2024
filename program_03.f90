program rectangle
implicit none
integer::si
real::x1,x2,x3,x4,y1,y2,y3,y4,a,b,c,d,e,f,perimeter,area
do
print*,"Enter the serial no:/=0"
read*,si
if(si==0) exit
print*,"Enter the value of all vertexs:x1,y1,x2,y2,x3,y3,x4,y4"
read*,x1,y1,x2,y2,x3,y3,x4,y4
a=sqrt((x1-x2)**2.0 +(y1-y2)**2.0)
b=sqrt((x2-x3)**2.0 +(y2-y3)**2.0)
c=sqrt((x3-x4)**2.0 +(y3-y4)**2.0)
d=sqrt((x4-x1)**2.0 +(y4-y1)**2.0)
e=sqrt((x3-x1)**2.0 +(y3-y1)**2.0)
f=sqrt((x4-x2)**2.0 +(y4-y2)**2.0)
if(a==c .and. b==d .and. e==f) then
perimeter=2*(a+b)
area=a*b
Print*,"Area of Rectangle=",area
print*,"Perimeter=",perimeter
else 
print*,"Does not from a rectangle."
end if 
end do
end program rectangle