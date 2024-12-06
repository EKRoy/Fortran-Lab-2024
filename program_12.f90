program fibbonacci_sequence
implicit none
integer::si,i,n
integer,allocatable,dimension(:)::fib
do 
print*,"Enter the serial no:"
read*,si
if(si==0) exit
print*,"Enter the value of n"
read*,n
allocate(fib(n))               
fib(1)=0
fib(2)=1
do i=3,n
fib(i)=fib(i-1)+fib(i-2)
end do
print*,"The Fibonacci Sequence:"
do i=1,n
print*,fib(i)
end do
deallocate(fib)
end do
end program fibbonacci_sequence


