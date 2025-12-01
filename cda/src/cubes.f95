MODULE cubes
  USE kinds, ONLY: wp => dp
  IMPLICIT NONE

  TYPE, PUBLIC :: cube
    PRIVATE
    CHARACTER (LEN=72) :: str1
    CHARACTER (LEN=72) :: str2
    REAL (KIND=wp) :: xmin, ymin, zmin, dx, dy, dz
    INTEGER :: nx, ny, nz, natoms
    INTEGER, DIMENSION(:), POINTER :: zahl      !atomic number array
    REAL (KIND=wp), DIMENSION(:), POINTER :: chrg, x, y, z, array 
  END TYPE cube

  PUBLIC :: cube_get, &
            cube_add, &
            cube_sub, &
            cube_int, &
            cube_cdz, &
            cube_del

  INTERFACE OPERATOR(+)
    MODULE PROCEDURE cube_add
  END INTERFACE
  INTERFACE OPERATOR(-)
    MODULE PROCEDURE cube_sub
  END INTERFACE


  CONTAINS


  SUBROUTINE cube_get (mycube, infile)
    CHARACTER(LEN=*), INTENT(IN) :: infile
    TYPE (cube), INTENT(OUT) :: mycube
    ! ...
    ! access attributes as follows:
    ! mycube%str1 = ...

    INTEGER :: i
    REAL(KIND=wp) :: d1 !dummy variable to store zeros in the dx,dy,dz matrix
    
    OPEN (UNIT=11, FILE=infile, STATUS='old', ACTION='write')

    READ(UNIT=11, FMT=*) mycube%str1
    READ(UNIT=11, FMT=*) mycube%str2
    READ(UNIT=11, FMT=*) mycube%natoms, mycube%xmin, mycube%ymin, mycube%zmin
    READ(UNIT=11, FMT=*) mycube%nx, mycube%dx, mycube%d1, mycube%d1
    READ(UNIT=11, FMT=*) mycube%ny, mycube%d1, mycube%dy, mycube%d1
    READ(UNIT=11, FMT=*) mycube%nz, mycube%d1, mycube%d1, mycube%dz
    
    ALLOCATE(mycube%zahl(mycube%natoms), mycube%chrg(mycube%natoms), mycube%x(mycube%natoms), mycube%y(mycube%natoms),
    mycube%z(mycube%natoms))

    DO i=1, mycube%natoms
        READ(UNIT=11, FMT=*) mycube%zahl(i), mycube%chrg(i), mycube%x(i), mycube%y(i), mycube%z(i)

    ENDDO

    ALLOCATE(mycube%array(mycube%nx*mycube%ny*mycube%nz))
    READ(UNIT=11, FMT=*) mycube%array

    CLOSE(11)

  END SUBROUTINE cube_get


  FUNCTION cube_add (mycube1, mycube2)
    TYPE(cube) :: cube_add
    TYPE(cube), INTENT(IN) :: mycube1, mycube2
    ! ...
  END FUNCTION cube_add


  FUNCTION cube_sub (mycube1, mycube2)
    TYPE(cube) :: cube_sub
    TYPE(cube), INTENT(IN) :: mycube1, mycube2
    ! ...
  END FUNCTION cube_sub


  FUNCTION cube_int (mycube)
    REAL (KIND=wp) :: cube_int
    TYPE (cube), INTENT(IN) :: mycube
    ! ...
  END FUNCTION cube_int


  SUBROUTINE cube_del (mycube)
    TYPE (cube), INTENT(IN) :: mycube
    ! ...
  END SUBROUTINE cube_del


END MODULE cubes
