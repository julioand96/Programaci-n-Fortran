program aprox_sin 
    implicit none                   
    real:: x, sin_true, y, dt, Er
    integer :: l
    real, external :: pade
 open (11, file = 'ersin.dat', status = 'unknown')   
dt=0.1

    do l = 0,32
       	 x = float(l) * dt           
         sin_true = sin(x)        	 
         y = pade(x)   !función definida abajo
         Er = abs(y - sin_true) / sin_true
         write(11,*) x, Er
    enddo
close (11)
end program aprox_sin
!========================================================================================================
function pade(x)
  implicit none  
   ! define variables
   real, intent(in) :: x         !único input 
   real :: pade                  !variable de salida
   ! variables locales
   real :: p, np 
   real (kind=16) dp 

   !generando la aproximación
  np=(12671.0/4363920.0)*(x**5)-(2363.0/18183.0)*(x**3)+x
  dp=1+(445.0/12122.0)*(x**2)+(601.0/872784.0)*(x**4)+(121.0/16662240.0)*(x**6)
  p= np / dp
pade = p

end function pade
