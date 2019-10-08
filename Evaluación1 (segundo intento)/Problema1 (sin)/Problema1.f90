program aprox_sin 
    implicit none                   
    real:: x, sin_true, y, dt, Er
    integer :: j, k
    real, external :: pade
 open (11, file = 'padsin.dat', status = 'unknown')   
 dt=0.1
    do j = -32,32
       	 x = float(j) * dt           
         sin_true = sin(x)        	 
         write(11,*) x, sin_true, 1
    enddo
 write(11,*) " "
    do k = -32,32
       	 x = float(k) * dt                	 
         y = pade(x)   !función definida abajo
         write(11,*) x, y, 2
    enddo
 close (11)
end program aprox_sin
!========================================================================================================
function pade(x)  
  implicit none  
   ! define variables
   real, intent(in) :: x         !único input 
   real :: pade                  !variable de salida
   real :: p, np                 !variables locales
   real (kind=16) dp 
   !generando la aproximación
  np=(12671.0/4363920.0)*(x**5)-(2363.0/18183.0)*(x**3)+x                        !numerador
  dp=1+(445.0/12122.0)*(x**2)+(601.0/872784.0)*(x**4)+(121.0/16662240.0)*(x**6)  !denominador
  p= np / dp
pade = p
end function pade
