real function sm(arr,n)
implicit none
integer::i,n,arr(n)
real::sum
sum=0.0
do i=1,n
sum=sum+arr(i)
end do
sm=sum
end function sm

real function gm(arr,n)
implicit none
integer::i,n,arr(n)
real::product,gm1
product=1.0
do i=1,n
product=product*arr(i)
end do
gm1=product**(1/real(n))
gm=gm1
end function gm

real function am(arr,n)
implicit none
integer::i,n,arr(n)
real::sum,arithm
sum=0.0
do i=1,n 
sum=sum+arr(i)
end do
arithm=sum/real(n)
am=arithm
end function am

real function sd(arr,n)
implicit none
integer::i,n,arr(n)
real::dv,sum,amm,val
sum=0.0
do i=1,n
sum=sum+arr(i)
end do
amm=sum/real(n)
dv=0.0
do i=1,n
dv=dv+(arr(i)-amm)**2.0
end do
val=dv/real(n)
sd=sqrt(val)
end function sd

program experiment_39
implicit none
integer::n,i
real::am,gm,sm,sd,am1,gm1,sm1,sd1
integer,allocatable,dimension(:)::array
print*,"Enter the value of array size:"
read*,n
allocate(array(n))
print*,"Enter the elements:"
read*,(array(i),i=1,n)
sm1=sm(array,n)
gm1=gm(array,n)
am1=am(array,n)
sd1=sd(array,n)
print*,"sum=",sm1
print*,"GM=",gm1
print*,"AM=",am1
print*,"SD=",sd1
deallocate(array)
end program experiment_39
