program eulerpendulo
    implicit none 
    real, parameter :: l = 9.81     
    real, parameter :: g = 9.81
    real, parameter :: m = 1          
    real :: A, h, wo, t, y, B 
    integer :: j, k
    real, dimension(2) :: resp

wo = sqrt(g / l)           !donde ω0=sqrt(g/l), cuando θ0 ≪ 1
print*, 'Ángulo y tamaño de pasos'
read(*,*) A, h

open (11, file = 'pendulodata.dat', status = 'unknown') 

       do k = 0, 7000
       t = float(k) * h 
       if (t>6.3) exit 
       y = A * cos(wo * t)
       write(11,*) t, y, 1
       end do

write(11,*) " "
B = A

       do k = 0, 7000
       t = float(k) * h
       if (t>6.3) exit 
       call eulermethod(A,wo,h,g,l,resp)    
       write(11,*) t, resp(1), 2
       A = resp(1)
       wo = resp(2)
       end do
close (11)

print*, abs((B-A)/B)    !relative_error
end program eulerpendulo

!========================================
subroutine eulermethod(A,wo,h,g,l,resp)
    implicit none
    real, dimension(2) :: prev
    real, dimension(2) :: nex
    real, intent(in) :: A, wo, h, g, l
    real, dimension(2), intent(out) :: resp
    real :: ap, w, a2, w2
    ap = A
    w = wo
    a2 = h * w
    w2 = -h * g / l * a
    prev = (/a, w /)
    nex = (/a2, w2/) 

resp = prev + nex

end subroutine eulermethod
