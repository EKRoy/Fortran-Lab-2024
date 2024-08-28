program experiment_40
implicit none
integer::i,n
real::sm,am,gm,sd
integer,allocatable,dimension(:)::array
print*,"Enter the size of elements:"
read*,n
allocate(array(n))
print*,"Enter the elements:"
read*,(array(i),i=1,n)
call area(array,n,sm,am,gm,sd)
print*,"Sum=",sm
print*,"AM=",am
print*,"GM=",gm
print*,"SD=",sd
deallocate(array)
end program experiment_40

subroutine area(arr,n,sm1,am1,gm1,sd1)
implicit none
integer::i,n,arr(n)
real::sm1,am1,gm1,sd1,dv,prod,sum,var
sum=0.0
do i=1,n
sum=sum+arr(i)
end do
sm1=sum
am1=sum/real(n)

prod=1.0
do i=1,n
prod=prod*arr(i)
end do
gm1=prod**(1/real(n))

dv=0.0
do i=1,n
dv=dv+(arr(i)-am1)**2.0
end do
var=dv/real(n)
sd1=sqrt(var)
end subroutine area

