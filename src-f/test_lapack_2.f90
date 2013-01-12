program test_2
    implicit none
    integer, parameter :: N=1000
    double precision, allocatable :: a(:,:), b(:)
    integer, allocatable :: ipiv(:), jpiv(:)
    double precision :: scal, maxerr
    integer :: i, info
    
    allocate(a(1:N,1:N))
    allocate(b(1:N))
    allocate(ipiv(1:N))
    allocate(jpiv(1:N))
    
    ! matrix from http://www.cs.yale.edu/homes/spielman/BAP/lect6.pdf
    a(1:N,1:N)=0.0
    do i=1, N-1
        a(i,i)=1.0
        a(i+1:N,i)=-1.0
    enddo
    a(1:N,N)=1.0
    do i=1, N
        b(i)=sum(a(i,1:N))
    enddo
    call dgetc2(N,a,N,ipiv,jpiv,info)
    if(info>0)then
        write(*,'(a,i4,a,i4,a,e13.6)') "A(",info,",",info,")=",a(info,info)
        stop 1
    endif
    call dgesc2(N,a,N,b,ipiv,jpiv,scal)
    if(scal/=1.0)then
        b(1:N)=b(1:N)*scal
    endif
    maxerr=0.0
    do i=1, N
        if(abs(b(i)-1.0)>abs(maxerr))then
            maxerr=b(i)-1.0
        endif
    enddo
    write(*,'(e13.6)') maxerr
    if(abs(maxerr)>1.0e-8)then
        write(0,'(A)') "Large error"
        stop 1
    endif
    deallocate(a)
    deallocate(b)
    deallocate(ipiv)
    deallocate(jpiv)
end
