module lin
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

double precision function abs_max_array(x,maxi)
    implicit none
    double precision, intent(inout):: x(:)
    integer, intent(out):: maxi
    double precision:: xi
    integer i

    abs_max_array=abs(x(1))
    maxi=1
    do i=2, ubound(x,1)
        xi=abs(x(i))
        if(xi>abs_max_array)then
            abs_max_array=xi
            maxi=i
        endif
    enddo
end function

subroutine scale_matrix_row(a, b)
    implicit none
    double precision, intent(inout):: a(:,:), b(:)
    double precision :: factor
    integer :: i,N,dummy

    N=ubound(a,2)
    do i=1, ubound(a,1)
        factor=1.0d0/abs_max_array(a(i,1:N),dummy)
        a(i,1:N)=a(i,1:N)*factor
        b(i)=b(i)*factor
    enddo
end subroutine

subroutine pivot(a, b, i)
    implicit none
    double precision, intent(inout):: a(:,:), b(:)
    integer, intent(in) :: i
    double precision :: dummy
    integer :: N, j, maxj
    N=ubound(a,1)
    dummy=abs_max_array(a(i:N,i),maxj)
    maxj=maxj+i-1
    ! swap
    if(i/=maxj)then
        do j=i, ubound(a,2)
            call swap(a(i,j), a(maxj,j))
        enddo
        call swap(b(i), b(maxj))
    endif
end subroutine

subroutine update_lower_col(a, b, i, blend)
    implicit none
    double precision, intent(inout):: a(:,:), b(:)
    integer, intent(in):: i, blend
    double precision:: factor
    integer:: N,i1,j

    N=ubound(a,1)
    do i1=i, blend
        call pivot(a,b,i1)
        factor=1.0d0/a(i1,i1)
        a(i1,i1+1:blend)=a(i1,i1+1:blend)*factor
        b(i1)=b(i1)*factor

        do j=i1+1, blend
            a(i1+1:N,j)=a(i1+1:N,j)-a(i1+1:N,i1)*a(i1,j)
        enddo
        b(i1+1:N)=b(i1+1:N)-a(i1+1:N,i1)*b(i1)
    enddo
end subroutine

subroutine update_upper_row(a, i, blend)
    implicit none
    double precision,intent(inout):: a(:,:)
    integer, intent(in):: i, blend
    integer:: k,i1

    do k=blend+1, ubound(a,2)
        do i1=i, blend
            a(i1,k)=a(i1,k)/a(i1,i1)
            a(i1+1:blend,k)=a(i1+1:blend,k)-a(i1+1:blend,i1)*a(i1,k)
        enddo
    enddo
end subroutine

subroutine forward_elimination(a, i, blend)
    implicit none
    double precision,intent(inout):: a(:,:)
    integer, intent(in):: i, blend
    integer N,j,k,l,blsize
    integer jend

    blsize=48
    N=ubound(a,1)
    do j=blend+1, N, blsize
        jend=min(j+blsize-1,N)
        do k=blend+1, N
            do l=i, blend
                a(j:jend,k)=a(j:jend,k)-a(j:jend,l)*a(l,k)
            enddo
        enddo
    enddo
end subroutine

subroutine back_substitution(a, b)
    implicit none
    double precision, intent(in):: a(:,:)
    double precision, intent(inout):: b(:)
    integer:: i
    do i=ubound(b,1), 1, -1
        b(1:i-1)=b(1:i-1)-a(1:i-1,i)*b(i)
    enddo
end subroutine

subroutine solve(a, b)
    implicit none
    double precision, intent(inout):: a(:,:), b(:)
    integer :: i,N,blocksize, blend
    blocksize=48
    N=ubound(a,1)
    ! scale
    call scale_matrix_row(a,b)
    do i=1, N, blocksize
        blend=min(i+blocksize-1,N)
        ! forward
        call update_lower_col(a,b,i,blend)
        call update_upper_row(a,i,blend)
        call forward_elimination(a,i,blend)
    enddo
    ! backward
    call back_substitution(a,b)
end subroutine
end module
