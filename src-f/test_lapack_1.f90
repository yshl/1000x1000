program test_1
    implicit none
    integer, parameter :: N=1000
    double precision, allocatable :: a(:,:), b(:)
    integer, allocatable :: ipiv(:)
    double precision :: maxerr
    integer :: i, info
    
    allocate(a(1:N,1:N))
    allocate(b(1:N))
    allocate(ipiv(1:N))
    
    a(1:N,1:N)=1.0
    b(1:N)=1000.0
    do i=1, N
        a(i,i)=1001.0
    enddo
    call dgesv(N,1,a,N,ipiv,b,N,info)
    maxerr=0.0
    do i=1, N
        if(abs(b(i)-0.5)>abs(maxerr))then
            maxerr=b(i)-0.5
        endif
    enddo
    write(*,'(e13.6)') maxerr
    if(abs(maxerr)>1.0e-8)then
        write(0,'(A)') "Large error"
        stop 1
    endif
    deallocate(a)
    deallocate(b)
end
