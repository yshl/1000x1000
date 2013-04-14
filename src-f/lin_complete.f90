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
subroutine pivot(a, i, maxj, maxk)
    implicit none
    double precision, intent(inout):: a(:,:)
    integer, intent(in) :: i
    integer, intent(out) :: maxj, maxk
    double precision :: ajk, ajkmax
    integer :: j, k
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
end subroutine
subroutine swappivot(a, b, i, maxj, maxk)
    double precision, intent(inout) :: a(:,:), b(:)
    integer, intent(in) :: i,maxj,maxk
    integer :: j
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
end subroutine
subroutine solve(a, b)
    implicit none
    double precision, intent(inout):: a(:,:), b(:)
    double precision :: factor, ajkmax, ajk
    double precision, allocatable:: scaleb(:)
    integer, allocatable:: swapcol(:)
    integer :: i,j,k,N,maxj,maxk
    N=ubound(a,1)
    allocate(scaleb(1:N))
    allocate(swapcol(1:N))
    ! scale
    call scalearray(a,b,scaleb)
    call pivot(a,1,maxj,maxk)
    do i=1, N
        ! pivot
        swapcol(i)=maxk
        call swappivot(a,b,i,maxj,maxk)
        ! forward
        factor=1.0/a(i,i)
        a(i,i+1:N)=factor*a(i,i+1:N)
        b(i)=factor*b(i)
    
        ajkmax=0.0
        maxj=i+1
        maxk=i+1
        do j=i+1, N
            do k=i+1, N
                a(k,j)=a(k,j)-a(k,i)*a(i,j)
                ajk=abs(a(k,j))
                if(ajk>ajkmax)then
                    ajkmax=ajk
                    maxj=k
                    maxk=j
                endif
            enddo
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
