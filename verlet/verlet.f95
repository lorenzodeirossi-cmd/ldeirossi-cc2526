PROGRAM verlet
        IMPLICIT NONE !tells the complier not to consider anything implicit
         
        !Variables declaration

        !First let's declare the parameter wp need to use double precision for real variables 
        !parameter means that the value of the variable cannot change
        INTEGER, PARAMETER :: wp = SELECTED_REAL_KIND (p=13, r=300) 

        INTEGER :: k, n, m
        REAL (KIND=wp) :: tau, f_x, f_y, f_z 
        REAL, DIMENSION(:), ALLOCATABLE :: x, y, z, v_x, v_y, v_z ! 1-index array

        k = 0
        n = 10          ! number of iteration
        tau = 0.2       ! time-step    
        m = 1
        f_x = 0.0
        f_y = 0.1
        f_z = 0.0

        ALLOCATE( x(n), y(n), z(n), v_x(n), v_y(n), v_z(n) )

        !Initialization of positions and velocities
        x(1) = 0.0
        y(1) = 0.0
        z(1) = 0.0
        v_x(1) = 0.0
        v_y(1) = 0.0
        v_z(1) = 0.0
        
        PRINT*, "STEP", 0
        PRINT*, x(1), y(1), z(1)
        PRINT*, v_x(1), v_y(1), v_z(1)

        !Calculations of the positions and the velocities
        DO k = 1, n
                x(k+1) = x(k) + tau*v_x(k) + tau**2 * (f_x/2*m)
                v_x(k+1) = v_x(k) + (tau/2*m)*(f_x + f_x)
                
                y(k+1) = y(k) + tau*v_y(k) + tau**2 * (f_y/2*m)
                v_y(k+1) = v_y(k) + (tau/2*m)*(f_y + f_y)

                z(k+1) = z(k) + tau*v_z(k) + tau**2 * (f_z/2*m)
                v_z(k+1) = v_z(k) + (tau/2*m)*(f_z + f_z)

                PRINT*, "STEP", k 
                PRINT*, x(k+1), y(k+1), z(k+1)
                PRINT*, v_x(k+1), v_y(k+1),v_z(k+1)
        ENDDO

END PROGRAM verlet       
