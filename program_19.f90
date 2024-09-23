program sum_of_exp_03
implicit none
integer::i,si,n,fact
real::sum,x
do 
print*,"Enter the value of serial no:/=0"
read*,si
if(si==0) exit
print*,"Enter the value of x,n"
read*,x,n
sum=0.0	
fact=1.0
do i=1,n
fact=fact*i
sum=sum+((x**i)/real(fact))
end do
print*,"summation:",sum
end do
end program sum_of_exp_03
