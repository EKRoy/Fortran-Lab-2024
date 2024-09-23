program ascending_decending_order
implicit none
integer::i,j,n,temp
integer,allocatable,dimension(:)::arr
print*,"Enter the value of n."
read*,n
allocate(arr(n))
print*,"Enter the values of array"
read*,(arr(i),i=1,n)
!bubble sort algorithm
 do i = 1, n-1
    do j = 1, n-i
      if (arr(j) > arr(j+1)) then
        temp = arr(j)
        arr(j) = arr(j+1)
        arr(j+1) = temp
      end if
    end do
  end do
 print*,"Ascending Array:"
do i=1,n
print*,arr(i)
end do
!decending sort
 do i = 1, n-1
    do j = 1, n-i
      if (arr(j) < arr(j+1)) then
        temp = arr(j)
        arr(j) = arr(j+1)
        arr(j+1) = temp
      end if
    end do
  end do

 print*,"Decending Array:"
do i=1,n
print*,arr(i)
end do

end program ascending_decending_order