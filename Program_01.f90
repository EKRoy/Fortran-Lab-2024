Program triangle
implicit none
integer::si
real::x1,x2,x3,y1,y2,y3,a,b,c,perimeter,area,s
do
print*,"Enter the serial no:\=0"
read*,si
if(si==0) exit
print*,"Enter the first vertex(x1,y1)"
read*,x1,y1
print*,"Enter the second vertex(x2,y2)"
read*,x2,y2
print*,"Enter the Third vertex(x3,y3)"
read*,x3,y3
a=sqrt((x1-x2)**2.0+(y1-y2)**2.0)
b=sqrt((x2-x3)**2.0+(y2-y3)**2.0)
c=sqrt((x3-x1)**2.0+(y3-y1)**2.0)
if((a+b)>c .and. (b+c)>a .and. (c+a)>b )  then
perimeter=a+b+c
s=perimeter/2.0
area=sqrt(s*(s-a)*(s-b)*(s-c))
print*,"Area of Triangle:",area
print*,"Perimeter:",perimeter
else 
print*,"Does not from a triangle"
end if 
end do
end program triangle

