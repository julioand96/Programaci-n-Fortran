program taylor_exp
    implicit none                   
    real :: x, exp_true, y, dx
    integer :: nmax, n, j

 open (11, file = 'sal.dat', status = 'unknown')   
 dx=0.1

    do j =-40,40 
       x = float(j) * dx                     
       exp_true = exp(x) 
       write(11,*) x, exp_true, 0
       enddo 
write(11,*) " "
       do nmax=1,8
	do j = -40,40
       	 x = float(j) * dx               
       	 call exptaylor(x,nmax,y,n) !defined below 
 open (11+nmax, status = 'unknown')  
       	write(11+nmax,*) x, y, nmax
       	enddo
	!write(11,*) " "
       enddo

close (11)
end program taylor_exp
!==================================== 
subroutine exptaylor(x,nmax,y,n) 
    implicit none 
   ! subroutine arguments: 
    real, intent(in) :: x 
    integer, intent(in) :: nmax 
    real, intent(out) :: y 
    integer, intent(out) :: n 
    ! local variables: 
    real :: term, partial_sum
    integer :: j

    term = 1.
    partial_sum = term

    do j=1,nmax
        term = term*x/j   
        partial_sum = partial_sum + term   
        enddo
     y = partial_sum  ! this is the value returned
end subroutine exptaylor 
