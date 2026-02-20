PROGRAM verletNparticle

        !======================================================================

        !Modules: 
        !-kinds: for variables precision 
        !-unit: for opening w/r units
        !-read_input: read the input.txt file and assigns the variables 
        !-potential: calculates the forces as the potential's derivatives 
        !-verletcalc: updates positions and velocities using verlet algorithm
        
        !Input:
        !k, dt: number of steps, timestep
        !sigma, epsilon: Lennard-Jones parameters
        !n: number of particles
        !m(n): array storing particles masses
        !x(n,3): matrix storing initial positions
        !vel(n,3): matrix storing initial velocities

        !Output:
        !

        !=======================================================================
        
        USE kinds, ONLY: wp => dp
        USE unit
        USE read_input
        
        
        IMPLICIT NONE 
        


