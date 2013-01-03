module lin_complete
    implicit none
contains
subroutine swap(a, b)
    implicit none
    double precision, intent(inout):: a,b
    double precision :: tmp
    tmp=a
    a=b
    b=tmp
end subroutine
subroutine scalearray(a, b, scaleb)
    implicit none
    double precision, intent(inout):: a(:,:), b(:)
    double precision, intent(out):: scaleb(:)
    double precision:: aijmax, factor
    integer :: i, j, N

    N=ubound(a,1)
    do i=1, N
        aijmax=abs(a(i,1))
        do j=2, N
            aijmax=max(aijmax,abs(a(i,j)))
        enddo
        factor=1.0d0/aijmax
        a(i,1:N)=a(i,1:N)*factor
        b(i)=b(i)*factor
    enddo
    do i=1, N
        aijmax=abs(a(1,i))
        do j=2, N
            aijmax=max(aijmax,abs(a(j,i)))
        enddo
        factor=1.0d0/aijmax
        a(1:N,i)=a(1:N,i)*factor
        scaleb(i)=factor
    enddo
end subroutine
integer function pivot(a, b, i)
    implicit none
    double precision, intent(inout):: a(:,:), b(:)
    integer, intent(in) :: i
    double precision :: ajk, ajkmax
    integer :: j, k, maxj, maxk
    ajkmax=abs(a(i,i))
    maxj=i
    maxk=i
    do k=i, ubound(a,2)
        do j=i, ubound(a,1)
            ajk=abs(a(j,k))
            if(ajk>ajkmax)then
                ajkmax=ajk
                maxj=j
                maxk=k
            endif
        enddo
    enddo
    ! swap
    if(i/=maxj)then
        do j=i, ubound(a,2)
            call swap(a(i,j), a(maxj,j))
        enddo
        call swap(b(i), b(maxj))
    endif
    if(i/=maxk)then
        do j=1, ubound(a,1)
            call swap(a(j,i), a(j,maxk))
        enddo
    endif
    pivot=maxk
end function
subroutine solve(a, b)
    implicit none
    double precision, intent(inout):: a(:,:), b(:)
    double precision :: factor
    double precision, allocatable:: scaleb(:)
    integer, allocatable:: swapcol(:)
    integer :: i,j,N
    N=ubound(a,1)
    allocate(scaleb(1:N))
    allocate(swapcol(1:N))
    ! scale
    call scalearray(a,b,scaleb)
    do i=1, N
        ! pivot
        swapcol(i)=pivot(a,b,i)
        ! forward
        factor=1.0/a(i,i)
        a(i,i+1:N)=factor*a(i,i+1:N)
        b(i)=factor*b(i)
    
        do j=i+1, N
            a(i+1:N,j)=a(i+1:N,j)-a(i+1:N,i)*a(i,j)
        enddo
        b(i+1:N)=b(i+1:N)-a(i+1:N,i)*b(i)
    enddo
    ! backward
    do i=N, 1, -1
        b(1:i-1)=b(1:i-1)-a(1:i-1,i)*b(i)
    enddo
    ! swap
    do i=N, 1, -1
        if(swapcol(i)/=i)then
            call swap(b(i),b(swapcol(i)))
        endif
    enddo
    ! scale
    do i=1, N
        b(i)=b(i)*scaleb(i)
    enddo
    deallocate(swapcol)
    deallocate(scaleb)
end subroutine
end module
