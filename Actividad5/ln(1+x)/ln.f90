program taylor_ln
    implicit none                   
    real :: x, ln_true, y, dx
    integer :: nmax, n, j

 open (11, file = 'sal.dat', status = 'unknown')   
 dx=0.1

    do j =-20,20 
       x = float(j) * dx                     
       ln_true = log(1+x) 
       write(11,*) x, ln_true, 0
       enddo 
write(11,*) " "
       do nmax=4,7,3
	do j = -20,20
       	 x = float(j) * dx               
       	 call logtaylor(x,nmax,y,n) !defined below 
       	write(11,*) x, y, nmax
       	enddo
	write(11,*) " "
       enddo

       do nmax=11,16,5
	do j = -20,20
       	 x = float(j) * dx               
       	 call logtaylor(x,nmax,y,n) !defined below 
       	write(11,*) x, y, nmax
       	enddo
	write(11,*) " "
       enddo
close (11)
end program taylor_ln
!==================================== 
subroutine logtaylor(x,nmax,y,n) 
    implicit none 
   ! subroutine arguments: 
    real, intent(in) :: x 
    integer, intent(in) :: nmax 
    real, intent(out) :: y 
    integer, intent(out) :: n 
    ! local variables: 
    real :: term, partial_sum

    partial_sum = 0
     do n=1,nmax      
        term = (x**n)*((-1)**(n+1))/n
        partial_sum = partial_sum + term    
     enddo 
     y = partial_sum  ! this is the value returned 
end subroutine logtaylor 
