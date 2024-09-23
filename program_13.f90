program sum_of_digit
implicit none
integer::si,sum=0,n,digit
do
print*,"Enter the serial no:/=0"
read*,si
if(si==0) exit
print*,"Enter the value of number(n)"
read*,n
print*,"Reverse of the number:"
  do
  if(n==0) exit
  digit=mod(n,10)
  print*,digit
  sum=sum+digit
  n=n/10
  end do
  print*,"Summation:",sum
  end do
  end program sum_of_digit