program sum_of_exp_04
implicit none
integer::si,i,n
real::sum,a,b,r
logical::flag
do 
print*,"Enter the serial no:/=0"
read*,si
if(si==0) exit
print*,"Enter the value of a,b,r,n"
read*,a,b,r,n
flag=.true.
sum=0.0
do i=1,n
if(r+(i-1)*a==0) then
flag=.false.
exit
end if
sum=sum+(i*b/(r+(i-1)*a)**i)
end do
if(flag) then
print*,"Sum of expression:",sum
else 
print*,"Sum is Undefined"
end if
end do 
end program sum_of_exp_04