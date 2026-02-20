MODULE unit
  IMPLICIT NONE

  CONTAINS !assign w/r units

  FUNCTION get_free_unit() RESULT(unit_num)
    INTEGER :: unit_num
    LOGICAL :: is_open

    DO unit_num = 10, 999
      INQUIRE(UNIT=unit_num, OPENED=is_open)
      IF (.NOT. is_open) RETURN
    END DO

  STOP

  END FUNCTION get_free_unit

END MODULE unit
