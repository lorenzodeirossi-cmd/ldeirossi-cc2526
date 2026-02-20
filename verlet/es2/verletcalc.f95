MODULE verletcalc
  USE kinds, ONLY:  wp => dp
  USE potential  !modul containing potential derivatives
  IMPLICIT NONE

  CONTAINS

   SUBROUTINE distancecalc(x,r,n)
   !creates distance matrix, where every elements contains the distance beetween particle i  and j

      INTEGER, INTENT(IN) :: n     !particle number
      REAL (KIND=wp), DIMENSION(n,3), INTENT(IN) :: x    !particle Z-matrix
      REAL (KIND=wp), DIMENSION(n,n), INTENT(OUT) :: r   !distance matrix

      INTEGER :: a, b
      REAL (KIND=wp) :: distance

      ! matrix element calculation

      DO a=1, n
       r(a,a) = 0.0_wp
         DO b=a+1, n
           distance = SQRT((x(a,1)-x(b,1))**2.0_wp + (x(a,2)-x(b,2))**2.0_wp + (x(a,3)-x(b,3))**2.0_wp)
           r(a,b) = distance
           r(b,a) = distance
         ENDDO
      ENDDO

    END SUBROUTINE distancecalc

    SUBROUTINE forcecalc (sigma, epsilon, x, n, fn, V, dV)    
     !calculates  f_k+1 for each component and for each particle, in order to calculate the velocities v_k+1=v_k+dt/2m*(f_k+f_k+1)

      INTEGER, INTENT(IN) :: n
      REAL (KIND=wp), INTENT(IN) :: sigma, epsilon
      REAL (KIND=wp), DIMENSION(n,3), INTENT(IN) :: x !Z-matrix
      REAL (KIND=wp), DIMENSION(n,3), INTENT(OUT) :: fn  !forcese
      REAL (KIND=wp), DIMENSION(n,n), INTENT(OUT) :: V, dV  ! potential and its derivative

      INTEGER :: a, b, i      !a,b indices for the particles, i represents the 3 axis
      REAL (KIND=wp) :: sum
      REAL (KIND=wp), DIMENSION(n,n) :: r

      CALL distancecalc(x,r,n)  !import distance matrix

      CALL Lennard_Jones(sigma, epsilon,n,r,dV,V)    !import Lennard-Jones potential

      DO i=1, 3 !iteration for each axis
        DO a=1, n    !iteration for each particle 
           sum = 0.0_wp
            DO b=1, n      !iteration over particle a interacting particles
              IF (b/=a) THEN
                sum = sum - ((x(a,i)-x(b,i))/r(a,b))*dV(a,b)
              END IF
            ENDDO
          fn(a,i) = sum    !aggiorna la componente i (x,y,z) della particella a (1,2...,n)
        ENDDO
      ENDDO

    END SUBROUTINE forcecalc


    SUBROUTINE energy_calc(n, m, vel, V, ecin, epot, etot)

      INTEGER, INTENT(IN) :: n
      REAL (KIND=wp), DIMENSION(:), INTENT(IN) :: m !masses 
      REAL (KIND=wp), DIMENSION(:,:), INTENT(IN) :: vel !velocities
      REAL (KIND=wp), DIMENSION(:,:), INTENT(IN) :: V !potential
      REAL (KIND=wp), INTENT(OUT) :: ecin, epot, etot !energies

      INTEGER :: a, b, i      !a,b indices for the particles, i represents the 3 axis

      ecin = 0.0_wp   
      epot = 0.0_wp

      !kinetic energy

      DO i=1, n

        ecin = ecin + ((m(i)/2.0_wp) * ((vel(i,1)**2.0_wp) + (vel(i,2)**2.0_wp) + (vel(i,3)**2.0_wp)))

      ENDDO

      !potential energy

      DO a=1, n
        DO b=a+1, n

          epot = epot + V(a,b)

        ENDDO
      ENDDO

      etot = ecin + epot


    END SUBROUTINE energy_calc

END MODULE verletcalc

