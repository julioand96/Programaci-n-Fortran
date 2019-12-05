program projectile
  implicit none
  ! definimos constantes
  real, parameter :: e = 2.7182818, g = 9.81, pi = 3.1415927
  real, parameter :: m = 0.145, a = 45.0 * pi / 180.0, k = 0.0431, u = 44.7
  
  ! definimos las variables
  real :: ToF, hmax, xmax, vt, x, y, t, dt, vx, vy, vf, ux, uy, th
  integer :: n, npasos

open (11, file = 'posiciones4.dat', status = 'unknown')

! Leer valores para número de pasos desde la terminal
  write(*,*) 'Dame el número de pasos'
  read(*,*) npasos

  dt = 0.1
  ux = u * cos(a)
  uy = u * sin(a)

  ! LOOP MOV. CON ROZAMIENTO
  do n = 0, npasos
     t = float(n) * dt
      ! Posiciones
       x = m / k * ux * (1 - e ** (- k * t / m))
       y = (-m * g * t / k) + (m / k) * (uy + m * g / k) * (1 - e ** (- k * t / m))
      ! Velocidades
       vx = ux * e ** (-k * t / m)
       vy = (-m * g / k) + (uy + (m * g / k)) * (e ** (-k * t / m))
       write(11,*) x, y, 1
      if (y<0) exit
  end do

!Se obtienen y escriben los parámetros para el mov. CON ROZAMIENTO
  vf = sqrt((vx*vx)+(vy*vy))
  th = (-m/k)*alog(g/(g+(k*uy/m)))
  hmax = (-m * g * th / k) + (m / k) * (uy + m * g / k) * (1 - e ** (- k * th / m))

  write(*,*) 'Parámetros de movimiento con rozamiento:'
  write(*,*) 'Altura máxima:' ,hmax, 'metros.' 
  write(*,*) 'A los:' ,th, 'segundos.'
  write(*,*) 'Alcance:' ,x, 'metros.' 
  write(*,*) 'A los:' ,t, 'segundos.'
  write(*,*) 'Con velocidad:' ,vf, 'm/s.'

  write(11,*) " "
  write(*,*) " "

  ! LOOP MOV. SIN ROZAMIENTO
  do n = 0, npasos
	   t = float(n) * dt
	  ! las ecuaciones de la posición en "x" y "y"
	   x = u * cos(a) * t
	   y = u * sin(a) * t - 0.5 * g * t * t
	   write(11,*) x, y, 2
	   if (y<0) exit
	end do

  ! Se obtienen y escriben los parámetros del mov. SIN ROZAMIENTO
  ToF = 2 * u * sin(a) / g
  hmax = u * u * sin(a) * sin(a) / (2 * g)
  xmax = ToF * u * cos(a)

  write(*,*) 'Parámetros de movimiento sin rozamiento:'  
  write(*,*) 'Altura máxima:',hmax,'metros.' 
  write(*,*) 'A los:' ,ToF/2.0, 'segundos.'
  write(*,*) 'Alcance:' ,xmax, 'metros.' 
  write(*,*) 'A los:' ,ToF, 'segundos.'
  write(*,*) 'Con velocidad:' ,u, 'm/s.'

close (11) 
end program projectile
