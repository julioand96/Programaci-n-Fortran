program projectile
  implicit none

  ! definimos constantes
  real, parameter :: g = 9.8
  real, parameter :: pi = 3.1415927

  ! definimos las variables
  real :: a, u, x, y, t, dt, ToF
integer :: n, npasos

  ! Leer valores para el ángulo a, el tiempo t, y la velocidad inicial u desde la terminal
  write(*,*) 'Dame el ángulo, la rapidez inicial y el número de pasos'
  read(*,*) a, u, npasos

  ! convirtiendo ángulo a radianes
  a = a * pi / 180.0
  
  ! Obteniendo tiempo de vuelo (ToF)
  ToF = 2.0 * u * sin(a) / g

dt = 0.1

! LOOP
do n = 0, npasos
   t = float(n) * dt
  ! las ecuaciones de la posición en "x" y "y"
   x = u * cos(a) * t
   y = u * sin(a) * t - 0.5 * g * t * t
   if (y<0) exit
  ! escribiendo el resultado en la pantalla
   print*, t, " ", x, y
end do


end program projectile
 
