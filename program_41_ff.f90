program quadratic_equation_Subroutine_Subprogram
implicit none 
integer::si,ok
real::a,b,c,d,r,x1,x2,im1,im2
do
print*,"Enter the serial no:/=0"
read*,si
if(si==0) exit
print*,"Enter the value of a,b,c and d"
read*,a,b,c,d
!ok=0
call quadratic(a,b,c,d,x1,x2,im1,im2,r,ok)
if(ok==1) then
 print*,"x1 = ", x1
 print*,"x2 = ", x2
end if
if(ok==2) then
print*,"x1=",r," +i",im1
print*,"x2=",r," -i",im2
end if
end do
end program quadratic_equation_Subroutine_Subprogram

subroutine quadratic(a,b,c,d,x1,x2,im1,im2,r,ok)
implicit none
real,intent(in)::a,b,c,d
real,intent(out)::x1,x2,im1,im2
integer,intent(out)::ok
real::disc,r
if((a+b)==0) then
print*,"This is not quadratic equation."
else
disc=c**2-4*(a+b)*d
if(disc>0) then 
print*,"the equation has two real and unequal roots"
x1=(-c+sqrt(disc))/(2*(a+b))
x2=(-c-sqrt(disc))/(2*(a+b))											    
ok=1
else if(disc==0)then 
print*,"this equation has two equal and real roots"
ok=1
x1=-c/(2*(a+b))
x2=x1
else
print*,"the equation has complex roots"
ok=2
im1=sqrt(abs(disc))/(2*(a+b))
im2=im1
r=-c/(2*(a+b))
end if 
end if
end subroutine quadratic
