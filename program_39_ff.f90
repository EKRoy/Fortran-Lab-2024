program experiment_39
implicit none
integer::n,i
real::am,gm,sm,sd,am1,gm1,sm1,sd1
integer,allocatable,dimension(:)::A
print*,"Enter the value of array size:"
read*,n
allocate(A(n))
print*,"Enter the elements:"
read*,(A(i),i=1,n)
sm1=sm(A,n)
gm1=gm(A,n)
am1=am(A,n)
sd1=sd(A,n,am1)
print*,"sum=",sm1
print*,"GM=",gm1
print*,"AM=",am1
print*,"SD=",sd1
deallocate(A)
end program experiment_39 

real function sm(A,n)
implicit none
integer::i
integer,intent(in)::n,A(n)
real::sum
sum=0.0
do i=1,n
sum=sum+A(i)
end do
sm=sum
end function sm

real function gm(A,n)
implicit none
integer::i
integer,intent(in)::n,A(n)
real::product
product=1.0
do i=1,n
product=product*A(i)
end do
gm=product**(1/real(n))
end function gm

real function am(A,n)
implicit none
integer::i
integer,intent(in)::n,A(n)
real::sum
sum=0.0
do i=1,n 
sum=sum+A(i)
end do
am=sum/real(n)
end function am

real function sd(A,n,am1)
implicit none
integer::i
integer,intent(in)::n,A(n)
real::dv,am1
dv=0.0
do i=1,n
dv=dv+(A(i)-am1)**2.0
end do
dv=dv/real(n)
sd=sqrt(dv)
end function sd


