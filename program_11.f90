program biggest_num
implicit none
integer::si,u,v,w,big
do
print*,"Enter the serial no:/=0"
read*,si
if(si==0) exit
print*,"Enter the values of u,v,w"
read*,u,v,w
big=u
if(v>big) big=v
if(w>big) big=w
print*,"The Biggest Number:",big
end do
end program biggest_num


