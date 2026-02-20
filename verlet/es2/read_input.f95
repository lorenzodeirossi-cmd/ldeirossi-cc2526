MODULE read_input

        USE kind, ONLY: wp => dp
        USE unit
        
        IMPLICIT NONE

        !Declares variables used in the module 
        INTEGER :: n, k
        REAL (KIND=wp) :: dt, sigma, epsilon
        REAL (KIND=wp), DIMENSION (:,:), ALLOCATABLE :: x, vel 
        REAL (KIND=wp), DIMENSION (:), ALLOCATABLE :: m
        
        CONTAINS

        SUBROUTINE input_reading()

              INTEGER :: a, u1     !dummy variable used in the for cicle, unit number 
               
              u1 = get_free_unit() !call the function in the unit module
              OPEN (UNIT=u1, FILE="inp.txt", STATUS="old", ACTION="read")   !open file

                READ (UNIT=u1, FMT=*) k, dt             !number of steps, timestep
                READ (UNIT=u1, FMT=*) sigma, epsilon    !Lennard-Jones parameter
                READ (UNIT=u1, FMT=*) n                 !number of particle

                ALLOCATE( x(n,3), vel(n,3), m(n))
                
                !read masses, initial positions and velocities of n particles
                DO a = 1,n

                  READ (UNIT=u1, FMT=*) m(a), x(a,1), x(a,2), x(a,3), vel(a,1), vel(a,2), vel(a,3)

                ENDDO 

              CLOSE (UNIT=u1)

       END SUBROUTINE input_reading

END MODULE read_input

