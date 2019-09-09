program projectile
  implicit none

  ! definimos constantes
  real, parameter :: g = 9.8
  real, parameter :: pi = 3.1415927

  ! definimos las variables
  real :: a, u, x, y, t, dt, ToF, dangulo
integer :: n, npasos, m, mpasos

open (11, file = 'salida.dat', status = 'unknown')
  
! Leer valores para el ángulo a, el tiempo t, y la velocidad inicial u desde la terminal
  write(*,*) 'Dame la rapidez inicial y el número de pasos'
  read(*,*) u, npasos

  ! convirtiendo ángulo a radianes
  a = a * pi / 180.0
  
  ! Obteniendo tiempo de vuelo (ToF)
  ToF = 2.0 * u * sin(a) / g

dt = 0.1
dangulo = 15.0 * pi / 180.0

! LOOP
  do m = 1, 6
     a = float(m) * dangulo
	do n = 0, npasos
	   t = float(n) * dt
	  ! las ecuaciones de la posición en "x" y "y"
	   x = u * cos(a) * t
	   y = u * sin(a) * t - 0.5 * g * t * t
	   write(11,*) x, y, m
	 ! write(*,*) x, y 
	   if (y<0) exit
	end do
   write(11,*) " "
  end do
close (11)
end program projectile
 
