program aprox_sin 

    implicit none                   
    real:: x, sin_true, y, dt, Er
    integer :: l
    real, external :: pade

 open (11, file = 'padsin.dat', status = 'unknown')   
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
function pade(x)  !necesita dos argumentos? como en exptaylor(x,n)
  implicit none  
   ! define variables
   real, intent(in) :: x         !único input 
   real :: pade                  !variable de salida
   
   ! variables locales
   real :: p 
 
   !generando la aproximación
    p = ((12671/4363920 * (x**5)) - (2363/18183 * (x**3)) + x) & !numerador
    / (1 + (445/12122 * (x**2)) + (601/872784 * (x**4)) + (121/16662240 * (x**6))) !denominador

pade = p
  
end function pade
