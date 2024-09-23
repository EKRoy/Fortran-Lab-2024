program sum_of_exp_04
implicit none
integer::si,i,n
real::sum,a,b,r
do 
print*,"Enter the serial no:/=0"
read*,si
if(si==0) exit
print*,"Enter the value of a,b,r,n"
read*,a,b,r,n
if(r+(n-1)*a==0) then
print*,"Sum is Undefined"
else
sum=0.0
do i=1,n
sum=sum+(i*b/(r+(i-1)*a)**i)
end do
print*,"Sum of expression:",sum
end if
end do 
end program sum_of_exp_04