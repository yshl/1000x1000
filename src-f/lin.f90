program lin
    implicit none
    integer, parameter :: N=1000
    double precision, allocatable :: a(:,:), b(:)
    integer :: i
    
    allocate(a(1:N,1:N))
    allocate(b(1:N))
    
    a(1:N,1:N)=1.0
    b(1:N)=1000.0
    do i=1, N
        a(i,i)=1001.0
    enddo
    call solve(a,b)
    do i=1, N
        write(*,'(f8.6)') b(i)
    enddo
    deallocate(a)
    deallocate(b)
contains
subroutine swap(a, b)
    implicit none
    double precision, intent(inout):: a,b
    double precision :: tmp
    tmp=a
    a=b
    b=tmp
end subroutine
subroutine pivot(a, b, i)
    implicit none
    double precision, intent(inout):: a(:,:), b(:)
    integer, intent(in) :: i
    double precision :: aji, ajimax
    integer :: j, maxj
    ajimax=abs(a(i,i))
    maxj=i
    do j=i+1, ubound(a,1)
        aji=abs(a(j,i))
        if(aji>ajimax)then
            ajimax=aji
            maxj=j
        endif
    enddo
    ! swap
    if(i/=maxj)then
        do j=i, ubound(a,2)
            call swap(a(i,j), a(maxj,j))
        enddo
        call swap(b(i), b(maxj))
    endif
end subroutine
subroutine solve(a, b)
    implicit none
    double precision, intent(inout):: a(:,:), b(:)
    double precision :: factor
    integer :: i,j,N
    N=ubound(a,1)
    do i=1, N
        ! pivot
        call pivot(a,b,i)
        ! forward
        factor=1.0/a(i,i)
        a(i,i+1:N)=factor*a(i,i+1:N)
        b(i)=factor*b(i)
    
        do j=i+1, N
            a(i+1:N,j)=a(i+1:N,j)-a(i+1:N,i)*a(i,j)
        enddo
        b(i+1:N)=b(i+1:N)-a(i+1:N,i)*b(i)
    enddo
    ! backard
    do i=N, 1, -1
        b(1:i-1)=b(1:i-1)-a(1:i-1,i)*b(i)
    enddo
end subroutine
end
