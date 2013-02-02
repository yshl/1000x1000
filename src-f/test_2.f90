program test_2
    use lin_complete
    implicit none
    integer, parameter :: N=1000
    double precision, allocatable :: a(:,:), b(:)
    double precision :: maxerr
    integer :: i
    
    allocate(a(1:N,1:N))
    allocate(b(1:N))
    
    a(1:N,1:N)=0.0
    do i=1, N-1
        a(i,i)=1.0
        a(i+1:N,i)=-1.0
    enddo
    a(1:N,N)=1.0
    b(1:N)=0.0
    do i=1, N
        b(1:N)=b(1:N)+a(1:N,i)
    enddo
    call solve(a,b)
    maxerr=0.0
    do i=1, N
        if(.not.(abs(b(i)-1.0)<=abs(maxerr)))then
            maxerr=b(i)-1.0
        endif
    enddo
    write(*,'(e13.6)') maxerr
    if(.not.(abs(maxerr)<=1.0e-8))then
        write(0,'(A)') "Large error"
        stop 1
    endif
    deallocate(a)
    deallocate(b)
end
