program test_1
    use lin
    implicit none
    integer, parameter :: N=1000
    double precision, allocatable :: a(:,:), b(:)
    double precision :: maxerr
    integer :: i
    
    allocate(a(1:N,1:N))
    allocate(b(1:N))
    
    a(1:N,1:N)=1.0
    b(1:N)=1000.0
    do i=1, N
        a(i,i)=1001.0
    enddo
    call solve(a,b)
    maxerr=0.0
    do i=1, N
        if(abs(b(i)-0.5)>abs(maxerr))then
            maxerr=b(i)-0.5
        endif
    enddo
    write(*,'(e13.6)') maxerr
    if(abs(maxerr)>1.0e-8)then
        stop 1
    endif
    deallocate(a)
    deallocate(b)
end
