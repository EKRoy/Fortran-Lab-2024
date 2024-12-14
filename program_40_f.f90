program experiment_40
implicit none
integer::i,n
real::sm,am,gm,sd
real,allocatable,dimension(:)::A
print*,"Enter the size of elements:"
read*,n
allocate(A(n))
print*,"Enter the elements:"
read*,(A(i),i=1,n)
call area(A,n,sm,am,gm,sd)
print*,"Sum=",sm
print*,"AM=",am
print*,"GM=",gm
print*,"SD=",sd
deallocate(A)
end program experiment_40

subroutine area(A,n,sm,am,gm,sd)
implicit none
integer::i
integer,intent(in)::n
real,intent(in)::A(n)
real,intent(out)::sm,am,gm,sd
real::dv,prod,sum
sum=0.0
do i=1,n
sum=sum+A(i)
end do
sm=sum
am=sum/real(n)

prod=1.0
do i=1,n
prod=prod*A(i)
end do
gm=prod**(1/real(n))

dv=0.0
do i=1,n
dv=dv+(A(i)-am)**2.0
end do
dv=dv/real(n)
sd=sqrt(dv)
end subroutine area

