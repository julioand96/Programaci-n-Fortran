program taylor_seno
    implicit none                   
    real :: x, sin_true, y, dx
    integer :: nmax, n, j

 open (11, file = 'sal.dat', status = 'unknown')   
 dx=0.1

    do j =-40,40 
       x = float(j) * dx                      ! convert to a real 
       sin_true = sin(x) 
       write(11,*) x, sin_true, 0
       enddo 

write(11,*) " "
    
       do nmax=1,6
	do j = -40,40
       	 x = float(j) * dx                    ! convert to a real 
       	 call sintaylor(x,nmax,y,n)   ! defined below 
       	write(11,*) x, y, n
       	enddo
	write(11,*) " "
       enddo

close (11)
end program taylor_seno
!==================================== 
subroutine sintaylor(x,nmax,y,n) 
    implicit none 
   ! subroutine arguments: 
    real, intent(in) :: x 
    integer, intent(in) :: nmax 
    real, intent(out) :: y 
    integer, intent(out) :: n 
    real, external :: factorial
    ! local variables: 
    real :: term, partial_sum, nf
    integer :: np

    partial_sum = 0
     do n=1,nmax
	np=2 * n - 1 
	nf=factorial(np)         
        term = (x**np)*((-1)**n)/nf
        partial_sum = partial_sum - term    
     enddo 
     y = partial_sum  ! this is the value returned 
end subroutine sintaylor 
!===================================================================================
function factorial(np)
  implicit none  
   integer, intent(in) :: np 
   integer :: m
   integer :: nfact  
   real :: factorial 
   
 nfact = 1  
   do m = 1, np      
      nfact = nfact * m
   end do 
factorial = float(nfact) !this is the value returned 
end function factorial
