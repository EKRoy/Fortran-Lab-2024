program prime_number
implicit none
integer:: si,n,m,i,j,flag,cnt
do
print*," Enter the serial no:/=0"
read*,si
if(si==0) exit
cnt=0
print*,"Enter the values of m and n"
read*,m,n
Print*,"The prime number are:"
do i=m,n
if(i<=1) cycle
flag=1
do j=2,i-1
if(mod(i,j)==0) then
  flag=0
  exit
  end if
 end do
  if(flag==1) then
  cnt=cnt+1
  print*,i
  end if
 end do
 print*,"Total Prime Number:",cnt
end do
  end program prime_number