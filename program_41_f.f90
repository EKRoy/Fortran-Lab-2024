 program quadratic_equation_Subroutine_Subprogram
implicit none 
integer::si,ok1,ok2
real::a,b,c,d,r,x1,x2,im1,im2
do
print*,"Enter the serial no:/=0"
read*,si
if(si==0) exit
print*,"Enter the value of a,b,c and d"
read*,a,b,c,d
ok1=0
ok2=0
call quadratic(a,b,c,d,x1,x2,im1,im2,r,ok1,ok2)
if(ok1==1) then
 print*,"x1 = ", x1
 print*,"x2 = ", x2
end if
if(ok2==1) then
print*,"x1=",r,"+i",im1
print*,"x2=",r,"-i",im2
end if
end do
end program quadratic_equation_Subroutine_Subprogram

subroutine quadratic(a,b,c,d,x1,x2,im1,im2,r,ok1,ok2)
implicit none
integer::ok1,ok2
real::a,b,c,d,x1,x2,im1,im2,e,r
if((a+b)==0) then
print*,"This is not quadratic equation."
else
e=c**2-4*(a+b)*d
if(e>0) then 
x1=(-c+sqrt(e))/(2*(a+b))
x2=(-c-sqrt(e))/(2*(a+b))											    
print*,"the equation has two real and unequal roots"
ok1=1
else if(e==0)then 
ok1=1
x1=-c/(2*(a+b))
x2=x1
print*,"this equation has two equal and real roots"
else
ok2=1
im1=sqrt(abs(e))/(2*(a+b))
im2=im1
r=-c/(2*(a+b))
print*,"the equation has complex roots"
end if 
end if
end subroutine quadratic
