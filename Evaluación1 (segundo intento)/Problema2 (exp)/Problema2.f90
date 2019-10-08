program aprox_sin 
    implicit none                   
    real:: x, exp_true, y, dt, Er
    integer :: l
    real, external :: pade02, pade11, pade20

 open (11, file = 'padexp.dat', status = 'unknown')   
dt=0.1

    do l = 0,32
       	 x = float(l) * dt           
         exp_true = exp(x)        	 
         y = pade02(x)   !función definida abajo
         Er = abs(y - exp_true) / exp_true
        ! write(11,*) x, y, 1
         write(11,*) x, Er, 1
    enddo
write(11,*) " "
    do l = 0,32
       	 x = float(l) * dt           
         exp_true = exp(x)        	 
         y = pade11(x)   !función definida abajo
         Er = abs(y - exp_true) / exp_true
         !write(11,*) x, y, 2
         write(11,*) x, Er, 2
    enddo
write(11,*) " "
 do l = 0,32
       	 x = float(l) * dt           
         exp_true = exp(x)        	 
         y = pade20(x)   !función definida abajo
         Er = abs(y - exp_true) / exp_true
       !  write(11,*) x, y, 3
         write(11,*) x, Er, 3
    enddo
close (11)
end program aprox_sin
!========================================================================================================
function pade02(x)  
  implicit none  
   ! define variables
   real, intent(in) :: x         !único input 
   real :: pade02                  !variable de salida
   real :: p                      !variable de salida

    p = 1.0/(1.0 - x + (0.5 * (x**2)))

pade02 = p
end function pade02
!=======================================================================================
function pade11(x)  
  implicit none  
   ! define variables
   real, intent(in) :: x        
   real :: pade11                  
   real :: p 

     p = (1.0 + (0.5 * (x**2)))/(1.0 - (0.5 * (x**2)))

pade11 = p
end function pade11
!=======================================================================================
function pade20(x)  
  implicit none  
   ! define variables
   real, intent(in) :: x         
   real :: pade20                 
   real :: p 

    p = (1.0 + x + (0.5 * (x**2)))

pade20 = p
end function pade20
