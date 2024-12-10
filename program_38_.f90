program matrix_sum
    implicit none
    integer::n, i, j, m
    real(8)::detA
    real(8), allocatable, dimension(:,:)::A, inv, adj

    print *, "Enter the number of rows (n) and columns (m):"
    read *, n, m
    allocate(A(n,m), inv(n,m), adj(n,m))

    print *, "Enter the values of matrix A row-wise:"
    read *, ((A(i,j), j=1,m), i=1,n)

    print *, "The transpose of the matrix:"
    do i = 1, n
        print *, (A(j, i), j=1, m)
    end do

    if (n /= m) then
        print *, "The matrix is not square. Cannot find the inverse."
        stop
    end if

    if (n == 1) then
        if (A(1,1) == 0.0) then
            print *, "Matrix is singular and cannot be inverted."
        else
            print *, "The inverse matrix is:", 1.0 / A(1,1)
        end if
    
    else if (n == 2) then
        detA = A(1,1) * A(2,2) - A(1,2) * A(2,1)
        if (detA == 0.0) then
            print *, "Matrix is singular and cannot be inverted."
        else
      		!adj and transpose
            adj(1,1) = A(2,2)
            adj(2,1) = -A(2,1)
            adj(1,2) = -A(1,2)
            adj(2,2) = A(1,1)

            inv = adj / detA
            print *, "The inverse matrix is:"
            do i = 1, n
                print *, (inv(i,j), j=1, m)
            end do
        end if
    else
        detA = A(1,1) * (A(2,2) * A(3,3) - A(2,3) * A(3,2)) &
             - A(1,2) * (A(2,1) * A(3,3) - A(2,3) * A(3,1)) &
             + A(1,3) * (A(2,1) * A(3,2) - A(2,2) * A(3,1))

        if (detA == 0.0) then
            print *, "Matrix is singular and cannot be inverted."
        else
            adj(1,1) = A(2,2) * A(3,3) - A(2,3) * A(3,2)
            adj(2,1) = -(A(2,1) * A(3,3) - A(2,3) * A(3,1))
            adj(3,1) = A(2,1) * A(3,2) - A(2,2) * A(3,1)
            adj(1,2) = -(A(1,2) * A(3,3) - A(1,3) * A(3,2))
            adj(2,2) = A(1,1) * A(3,3) - A(1,3) * A(3,1)
            adj(3,2) = -(A(1,1) * A(3,2) - A(1,2) * A(3,1))
            adj(1,3) = A(1,2) * A(2,3) - A(1,3) * A(2,2)
            adj(2,3) = -(A(1,1) * A(2,3) - A(1,3) * A(2,1))
            adj(3,3) = A(1,1) * A(2,2) - A(1,2) * A(2,1)


            inv = adj / detA

            print *, "The inverse matrix is:"
            do i = 1, n
                print *, (inv(i,j), j=1, m)
            end do
        end if
    end if
end program matrix_sum
