Program cube_tools
   !!
   !! Contains tools to manipulate cube objects
   !! Written by L. Dei Rossi, February 2026
   !!
   use kinds, only: wp => dp
   use cubes
   implicit none

   type (cube) :: cubeA, cubeB, cubeOUT
   character (len=256) :: flag, fileA, fileB, fileOUT
   !integer :: num_args

   ! should we check the number of arguments? 

   call get_command_argument(1, flag)

   if (flag == "add") then
      call get_command_argument(2, fileA)
      call get_command_argument(3, fileB)
      call get_command_argument(4, fileOUT)
      call cube_get(cubeA, fileA)
      call cube_get(cubeB, fileB)
      cubeOUT = cubeA + cubeB
      if (fileOUT(LEN_TRIM(fileOUT)-4 : LEN_TRIM(fileOUT)) == ".cube") then
         call cube_write(cubeOUT, fileOUT)
      else 
         fileOUT = "output_add.cube"
         call cube_write(cubeOUT, fileOUT)
         print *, "Output file name not correct. Writing to output_add.cube"
      end if
      
   else if (flag == "diff") then
      call get_command_argument(2, fileA)
      call get_command_argument(3, fileB)
      call get_command_argument(4, fileOUT)
      call cube_get(cubeA, fileA)
      call cube_get(cubeB, fileB)
      cubeOUT = cubeA - cubeB
       if (fileOUT(LEN_TRIM(fileOUT)-4 : LEN_TRIM(fileOUT)) == ".cube") then
         call cube_write(cubeOUT, fileOUT)
      else 
         fileOUT = "output_diff.cube"
         call cube_write(cubeOUT, fileOUT)
         print *, "Output file name not correct. Writing to output_diff.cube"
      end if
   
   else 
      print *, "ERROR: Incorrect flag. "
      print *, "Usage: cube_tools <flag> <fileA> <fileB> <fileOUT>"
      print *, "Available flags: add, diff, int, cdz"
      stop
   end if   

end program cube_tools