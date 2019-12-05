PROGRAM Triangle
     IMPLICIT NONE
     REAL :: a, b, c, Area, Vol
     PRINT *, 'Welcome, please enter the&
              & lengths of the 3 sides.'
     READ *, a, b, c 
     PRINT *, 'Triangle''s area:  ', Area(a,b,c)
     PRINT *, 'Parallelepiped''s volume:  ', Vol(a,b,c)
END PROGRAM Triangle
    FUNCTION Area(x,y,z)
     IMPLICIT NONE
     REAL :: Area            ! function type
     REAL, INTENT( IN ) :: x, y, z
     REAL :: theta, height
     theta = ACOS((x**2+y**2-z**2)/(2.0*x*y))
     height = x*SIN(theta); Area = 0.5*y*height
    END FUNCTION Area

    FUNCTION Vol(x,y,z)
    IMPLICIT NONE
    REAL :: Vol
    REAL, INTENT (IN) :: x,y,z
    Vol = x*y*z
    END FUNCTION Vol

 
