PROGRAM verlet
        IMPLICIT NONE !tells the complier not to consider anything implicit
         
        !Variables declaration

        !First let's declare the parameter wp need to use double precision for real variables 
        !parameter means that the value of the variable cannot change
        INTEGER, PARAMETER :: wp = SELECTED_REAL_KIND (p=13, r=300) 

        INTEGER :: k, i, n, m, time
        REAL (KIND=wp) :: tau, f_x, f_y, f_z 
        REAL (KIND=wp) :: y_final_prev, y_final_curr, error
        REAL, DIMENSION(:), ALLOCATABLE :: x, y, z, v_x, v_y, v_z ! 1-index array

        k = 0 
        time = 120       ! total time of the simulation
        tau = 12         ! time-step       
        m = 1            ! mass
        f_x = 0.0        ! force in the x direction       
        f_y = 0.1        ! force in the y direction
        f_z = 0.0        ! force in the z direction
        y_final_prev = 0.0 

        DO i = 0, 15   

                n = INT(time / tau)
                ALLOCATE( x(n+1), y(n+1), z(n+1), v_x(n+1), v_y(n+1), v_z(n+1) )
                
                !Initialization of positions and velocities
                x(1) = 0.0
                y(1) = 0.0
                z(1) = 0.0
                v_x(1) = 0.0
                v_y(1) = 0.0
                v_z(1) = 0.0
                

                !Calculations of the positions and the velocities
                DO k = 1, n
                        x(k+1) = x(k) + tau*v_x(k) + tau**2 * (f_x/(2.0*m))
                        v_x(k+1) = v_x(k) + (tau/(2.0*m))*(f_x + f_x)
                        
                        y(k+1) = y(k) + tau*v_y(k) + tau**2 * (f_y/(2.0*m))
                        v_y(k+1) = v_y(k) + (tau/(2.0*m))*(f_y + f_y)
                        
                       ! PRINT*, y(k+1)

                        z(k+1) = z(k) + tau*v_z(k) + tau**2 * (f_z/(2.0*m))
                        v_z(k+1) = v_z(k) + (tau/(2.0*m))*(f_z + f_z)

                ENDDO
                y_final_curr = y(n+1)
                error = ABS(y_final_curr - y_final_prev)

                ! Check with the previous iteration
                PRINT*, "TIMESTEP" , tau                   
                PRINT*, "STEP", n
                PRINT*, "FINAL TIME", n*tau
                PRINT*, x(n+1), y(n+1), z(n+1)
                PRINT*, v_x(n+1), v_y(n+1),v_z(n+1)
                
                IF ( error  < 0.01 ) THEN
                        PRINT*, "CONVERGED !"
                ELSE
                        PRINT*, "NOT CONVERGED"
                ENDIF

                        
                tau = tau / 2
                y_final_prev = y_final_curr     
                DEALLOCATE (x, y, z, v_x, v_y, v_z)
        ENDDO

END PROGRAM verlet       
