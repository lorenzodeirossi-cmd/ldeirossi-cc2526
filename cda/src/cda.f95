PROGRAM cda
  USE kinds, ONLY: wp => dp
  USE cubes
  IMPLICIT NONE
  
  TYPE (cube) :: cubeA, cubeB, cubeAB, cubeREF, DELTAcube
  !REAL (KIND=wp), DIMENSION (:,:,:), ALLOCATABLE :: array3D_A
  REAL (KIND=wp), DIMENSION (:), ALLOCATABLE ::z_axis, cubeintegral, cubeCDZ
  REAL (KIND=wp) :: start, finish
  INTEGER :: i
  
  ! 1. Insantiate cube objects with data from cube files ../test/CuCO+/ab.cube, ../test/CuCO+/a.cube, and ../test/CuCO+/b.cube
  CHARACTER (LEN=*), PARAMETER :: file_a  = '../test/CuCO+/a.cube'
  CHARACTER (LEN=*), PARAMETER :: file_b  = '../test/CuCO+/b.cube'
  CHARACTER (LEN=*), PARAMETER :: file_ab = '../test/CuCO+/ab.cube'

  CALL cube_get(cubeA, file_a)
  CALL cube_get(cubeB, file_b)
  CALL cube_get(cubeAB, file_ab)

  cubeREF = cubeA + cubeB
  
  ! 2. Generate a cube object containing the charge redistribution \Delta \rho = \rho_{AB} - \rho_{A} - \rho_{B}
  DELTAcube = cubeAB - cubeREF
  
  ! 3. Use an ad hoc defined external procedures in module cubes for extracting the charge-displacement function from the charge redistribution
  cubeintegral = cube_int(DELTAcube)
  cubeCDZ = cube_cdz(DELTAcube)

  ! 4. Create z axis 
  
  z_axis = [ (DELTAcube%zmin + (i-1)*DELTAcube%dz, i = 1, DELTAcube%nz) ]

  ! 5. Writing the output file
 OPEN(UNIT=12, FILE="output_cda.txt", STATUS='replace', ACTION="WRITE")
        WRITE(UNIT=12,FMT='(3(ES14.6, 2X))') (z_axis(i), cubeintegral(i), cubeCDZ(i), i=1, DELTAcube%nz)  
 CLOSE(12)
END PROGRAM cda
