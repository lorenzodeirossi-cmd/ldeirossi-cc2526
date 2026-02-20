MODULE potential

        !Creates n x n matrix where each cell contains the potential derivatives between the two particles 

        USE kind, ONLY: wp => dp
        
        IMPLICIT NONE

        CONTAINS

          SUBROUTINE Lennard_Jones(sigma, epsilon, n, r, dV, V)
            INTEGER, INTENT (IN) :: n
            REAL(KIND=wp) :: epsilon, sigma
            REAL(KIND=wp), DIMENSION(n,n), INTENT(IN) :: r
            REAL(KIND=wp), DIMENSION(n,n), INTENT(OUT) :: dV, V

            INTEGER :: a, b
            REAL(KIND=wp), :: derivative, potential

            DO a=1, n 
                
                V (a,a) = 0.0_wp
                dV(a,a) = 0.0_wp

                DO b=a+1,n

                  derivative = 4.0_wp*epsilon*(-12.0_wp*((sigma**12.0_wp)/(r(a,b)**13.0_wp))+6*((sigma**6.0_wp)/(r(a,b)**7.0_wp)))
         
                  dV(a,b) = derivative
                  dV(b,a) = derivative

                  potential = 4.0_wp*epsilon*(((sigma/r(a,b))**12.0_wp) - ((sigma/r(a,b))**6.0_wp))
                  V(a,b) = potential
                  V(b,a) = potential

                ENDDO
           ENDDO

        END SUBROUTINE Lennard_Jones

END MODULE potential
