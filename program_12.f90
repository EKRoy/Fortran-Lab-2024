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
allocate(fib(0:n))               
fib(0)=0
fib(1)=1
do i=2,n
fib(i)=fib(i-1)+fib(i-2)
end do
print*,"The Fibonacci Sequence:"
do i=0,n-1
print*,fib(i)
end do
deallocate(fib)
end do
end program fibbonacci_sequence


