MODULE cubes
  USE kinds, ONLY: wp => dp
  IMPLICIT NONE

  TYPE, PUBLIC :: cube
    ! PRIVATE
    CHARACTER (LEN=256) :: str1
    CHARACTER (LEN=256) :: str2
    REAL (KIND=wp) :: xmin, ymin, zmin, dx, dy, dz 
    INTEGER :: nx, ny, nz, natoms
    INTEGER, DIMENSION(:), POINTER :: zahl      !atomic number array
    REAL (KIND=wp), DIMENSION(:), POINTER :: chrg, x, y, z, array 
  END TYPE cube

  PUBLIC :: cube_get, &
            cube_add, &
            cube_sub, &
            cube_unroll, &
            cube_int, &
            cube_cdz, &
            cube_del, &
            cube_write

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
    
    OPEN (UNIT=11, FILE=infile, STATUS='old', ACTION='read')

    READ(UNIT=11, FMT=*) mycube%str1
    READ(UNIT=11, FMT=*) mycube%str2
    READ(UNIT=11, FMT=*) mycube%natoms, mycube%xmin, mycube%ymin, mycube%zmin
    READ(UNIT=11, FMT=*) mycube%nx, mycube%dx, d1, d1
    READ(UNIT=11, FMT=*) mycube%ny, d1, mycube%dy, d1
    READ(UNIT=11, FMT=*) mycube%nz, d1, d1, mycube%dz
    
    ALLOCATE(mycube%zahl(mycube%natoms), mycube%chrg(mycube%natoms), mycube%x(mycube%natoms), mycube%y(mycube%natoms), mycube%z(mycube%natoms))

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
    INTEGER :: i, j 

    !
    cube_add%str1 = mycube1%str1 
    cube_add%str2 = mycube1%str2 
    cube_add%natoms = mycube1%natoms + mycube2%natoms 
    cube_add%xmin = mycube1%xmin
    cube_add%ymin = mycube1%ymin
    cube_add%zmin = mycube1%zmin
    cube_add%nx = mycube1%nx
    cube_add%ny = mycube1%ny
    cube_add%nz = mycube1%nz
    cube_add%dx = mycube1%dx
    cube_add%dy = mycube1%dy
    cube_add%dz = mycube1%dz
    
    ALLOCATE(cube_add%zahl(cube_add%natoms), cube_add%chrg(cube_add%natoms), cube_add%x(cube_add%natoms), cube_add%y(cube_add%natoms), cube_add%z(cube_add%natoms))

    DO i=1, mycube1%natoms

        cube_add%zahl(i) = mycube1%zahl(i) 
        cube_add%chrg(i) = mycube1%chrg(i) 
        cube_add%x(i) = mycube1%x(i) 
        cube_add%y(i) = mycube1%y(i) 
        cube_add%z(i) = mycube1%z(i) 

    ENDDO

    
    DO j=1, mycube2%natoms

        cube_add%zahl(mycube1%natoms + j) = mycube2%zahl(j)
        cube_add%chrg(mycube1%natoms + j) = mycube2%chrg(j)
        cube_add%x(mycube1%natoms + j) = mycube2%x(j)
        cube_add%y(mycube1%natoms + j) = mycube2%y(j)
        cube_add%z(mycube1%natoms + j) = mycube2%z(j)
    
    ENDDO

    ALLOCATE(cube_add%array(cube_add%nx*cube_add%ny*cube_add%nz))
    
    cube_add%array = mycube1%array + mycube2%array

  END FUNCTION cube_add


  FUNCTION cube_sub (mycube1, mycube2)
    TYPE(cube) :: cube_sub
    TYPE(cube), INTENT(IN) :: mycube1, mycube2
    INTEGER :: i

    !we assume that the 2 cubes contains yet infos of the two fragments
    !all info in cube_sub will be the copy of the first cube unless the array

    cube_sub%str1 = mycube1%str1
    cube_sub%str2 = "Subtruction of the two cubes"
    cube_sub%natoms = mycube1%natoms
    cube_sub%xmin = mycube1%xmin
    cube_sub%ymin = mycube1%ymin
    cube_sub%zmin = mycube1%zmin
    cube_sub%nx = mycube1%nx
    cube_sub%ny = mycube1%ny
    cube_sub%nz = mycube1%nz
    cube_sub%dx = mycube1%dx
    cube_sub%dy = mycube1%dy
    cube_sub%dz = mycube1%dz
    
    ALLOCATE(cube_sub%zahl(cube_sub%natoms), cube_sub%chrg(cube_sub%natoms), cube_sub%x(cube_sub%natoms), cube_sub%y(cube_sub%natoms), cube_sub%z(cube_sub%natoms))

    DO i=1, mycube1%natoms

        cube_sub%zahl(i) = mycube1%zahl(i) 
        cube_sub%chrg(i) = mycube1%chrg(i) 
        cube_sub%x(i) = mycube1%x(i) 
        cube_sub%y(i) = mycube1%y(i) 
        cube_sub%z(i) = mycube1%z(i) 

    ENDDO


    ALLOCATE(cube_sub%array(cube_sub%nx*cube_sub%ny*cube_sub%nz))
    
    cube_sub%array = mycube1%array - mycube2%array

  END FUNCTION cube_sub


  SUBROUTINE  cube_unroll (mycube, array3d)
    TYPE (cube), INTENT(IN) :: mycube
    REAL (KIND=wp), DIMENSION(:,:,:), ALLOCATABLE, INTENT(OUT) :: array3d
    INTEGER :: i, i_x, i_y, i_z

    ALLOCATE(array3d(mycube%nx, mycube%ny, mycube%nz))
    i = 1

    DO i_x = 1, mycube%nx
        DO i_y = 1, mycube%ny
            DO i_z = 1, mycube%nz
                array3d(i_x, i_y, i_z) = mycube%array(i)
                i = i + 1
            ENDDO
        ENDDO
    ENDDO 
  END SUBROUTINE cube_unroll


  FUNCTION cube_int (mycube)
    REAL (KIND=wp), DIMENSION(:), ALLOCATABLE :: cube_int
    REAL (KIND=wp), DIMENSION(:,:,:), ALLOCATABLE :: array3d
    TYPE (cube), INTENT(IN) :: mycube
    INTEGER :: i_x, i_y, i_z
    
    ALLOCATE(cube_int(mycube%nz))

    CALL cube_unroll(mycube, array3d)
        
    DO i_z = 1, mycube%nz
      cube_int(i_z) = 0.0_wp
        DO i_x = 1, mycube%nx
          DO i_y = 1, mycube%ny
            cube_int(i_z) = cube_int(i_z) + (array3d(i_x, i_y, i_z) * mycube%dx * mycube%dy)
          ENDDO
        ENDDO
    ENDDO

  END FUNCTION cube_int

  FUNCTION cube_cdz (mycube)
    REAL (KIND=wp), DIMENSION(:), ALLOCATABLE :: cube_integral, cube_cdz
    TYPE (cube), INTENT(IN) :: mycube
    INTEGER :: i, j

    cube_integral = cube_int(mycube)
    ALLOCATE(cube_cdz(mycube%nz))

    DO i = 1, mycube%nz
        DO j = 1, i
                cube_cdz(i) = cube_cdz(i) + (cube_integral(j) * mycube%dz) 
        ENDDO
    ENDDO
 END FUNCTION cube_cdz


  SUBROUTINE cube_del (mycube)
    TYPE (cube), INTENT(IN) :: mycube
    ! ...
  END SUBROUTINE cube_del

  SUBROUTINE cube_write (mycube, out_file)
    TYPE (cube), INTENT(IN) :: mycube
    CHARACTER (LEN=*), INTENT(IN) :: out_file
    INTEGER :: i
    
    OPEN(UNIT=12, FILE=out_file, STATUS='replace', ACTION="WRITE")
    
    WRITE(UNIT=12,FMT='(A)') mycube%str1
    WRITE(UNIT=12,FMT='(A)') '! Density difference cube file'
    WRITE(UNIT=12,FMT='(I5, 3ES14.6)') mycube%natoms, mycube%xmin, mycube%ymin, mycube%zmin
    WRITE(UNIT=12,FMT='(I5, 3ES14.6)') mycube%nx, mycube%dx, 0.0_wp, 0.0_wp
    WRITE(UNIT=12,FMT='(I5, 3ES14.6)') mycube%ny, 0.0_wp, mycube%dy, 0.0_wp
    WRITE(UNIT=12,FMT='(I5, 3ES14.6)') mycube%nz, 0.0_wp, 0.0_wp, mycube%dz

    DO i = 1, mycube%natoms
        WRITE(UNIT=12,FMT='(I5, 3ES14.6)') mycube%zahl(i), mycube%x(i), mycube%y(i), mycube%z(i)
    ENDDO

    WRITE(UNIT=12,FMT='(I5, 3ES14.6)') mycube%array
    CLOSE(12)

  END SUBROUTINE cube_write

END MODULE cubes