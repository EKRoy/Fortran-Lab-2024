program expression
implicit none
integer::si
real::a,b,c,d,r,x,u,v
do
print*,"Enter the serial no:/=0"
read*,si
if(si==0) exit
print*,"Enter the value of a,b,c,d,r,x"
read*,a,b,c,d,r,x
if((r/=d) .and. (b/=0) ) then
u=abs(a+b**(-1))/sin(r-d)
print*,"the value of u is:",u
else
print*,"The value of u and v is undefined"
end if
if(u/=0) then
v=(((c*u**(-1))-(u*cos(x)))/b
print*,"The value of v is:",v
else 
print*,"The value of v is undefined"
end if
end do
end program empression
