program ascending_decending_order
implicit none
integer::i,j,n,temp,si
integer,allocatable,dimension(:)::arr
do
print*,"Enter the value of si:/=0"
read*,si
if(si==0) exit
print*,"Enter the value of n"
read*,n
allocate(arr(n))
print*,"Enter the values of array:arr(i)"
read*,(arr(i),i=1,n)
 do i = 1, n-1
    do j = i+1, n
      if (arr(i) > arr(i+1)) then
        temp = arr(i)
        arr(i) = arr(j)
        arr(j) = temp
      end if
    end do
  end do
 print*,"Ascending Array:"
do i=1,n
print*,arr(i)
end do


 do i = 1, n-1
    do j = i+1, n
      if (arr(i) < arr(j)) then
        temp = arr(i)
        arr(i) = arr(j)
        arr(j) = temp
      end if
    end do
  end do

 print*,"Decending Array:"
do i=1,n
print*,arr(i)
end do
deallocate(arr)
 end do
end program ascending_decending_order