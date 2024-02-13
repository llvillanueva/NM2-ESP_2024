REAL FUNCTION integrate_ft(dt, lam) RESULT(y)
    REAL, INTENT(IN) :: dt, lam
    y = 1 - lam*dt
END FUNCTION integrate_ft

REAL FUNCTION integrate_bt(dt, lam) RESULT(y)
    REAL, INTENT(IN) :: dt, lam
    y = 1/((1 + lam*dt)*1.0)
END FUNCTION integrate_bt

REAL FUNCTION exact_sol(t, y0, lam) RESULT(y)
    REAL, INTENT(IN) :: t, y0, lam
    y = y0* EXP(-lam*t)
END FUNCTION exact_sol

PROGRAM exer1
    IMPLICIT NONE
    REAL :: lam, y0, end_time, dt_list(3), dt, holder
    REAL, ALLOCATABLE :: y_values(:), t_values(:)
    INTEGER :: ios, num_steps, i, n
    REAL, EXTERNAL :: exact_sol
    CHARACTER(LEN=20) :: filename

    lam = 0.1
    y0 = 1.0
    end_time = 200

    dt_list = (/0.01, 15.0, 25.0/)

    DO n = 1, 3
        !Forward FT
        dt = dt_list(n)
        num_steps = FLOOR(end_time/dt) + 1
        ALLOCATE(y_values(num_steps), t_values(num_steps))
        y_values(1) = y0
        t_values(1) = 0.0
        WRITE(filename, '(A, I2.2, A)') "FT_Error", n, ".txt"
        OPEN(UNIT=10, IOSTAT = ios, FILE = filename, ACTION ='WRITE')       
            IF (ios == 0) THEN
                    CALL forward_fd(lam, dt, num_steps, y_values, t_values)
                    DO i = 1, num_steps
                        WRITE(10, *) t_values(i), y_values(i) - exact_sol(t_values(i), y0, lam) 
                    END DO
                    CLOSE(10)
                    DEALLOCATE(y_values, t_values)
            ELSE
                    PRINT '(a25)', 'Error: file not opened.'
            END IF

        !Backward FT
        ALLOCATE(y_values(num_steps), t_values(num_steps))
        y_values(1) = y0
        t_values(1) = 0.0
        WRITE(filename, '(A, I2.2, A)') "BT_Error", n, ".txt"
        OPEN(UNIT=10, IOSTAT = ios, FILE = filename, ACTION ='WRITE')       
            IF (ios == 0) THEN
                    CALL backward_fd(lam, dt, num_steps, y_values, t_values)
                    DO i = 1, num_steps
                        WRITE(10, *) t_values(i), y_values(i) - exact_sol(t_values(i), y0, lam) 
                    END DO
                    CLOSE(10)
                    DEALLOCATE(y_values, t_values)
            ELSE
                    PRINT '(a25)', 'Error: file not opened.'
            END IF
    END DO

CONTAINS
    SUBROUTINE forward_fd(lam, dt, num_steps, y_values, t_values)
        REAL, INTENT(IN) :: lam, dt
        REAL, INTENT(OUT) :: y_values(:), t_values(:)
        INTEGER :: num_steps, i
        REAL, EXTERNAL :: integrate_ft
        DO i = 1, num_steps -1 
            y_values(i+1) = y_values(i) * integrate_ft(dt, lam) 
            t_values(i+1) = i*dt
        END DO
    END SUBROUTINE forward_fd

    SUBROUTINE backward_fd(lam, dt, num_steps, y_values, t_values)
        REAL, INTENT(IN) :: lam, dt
        REAL, INTENT(OUT) :: y_values(:), t_values(:)
        INTEGER :: num_steps, i
        REAL, EXTERNAL :: integrate_bt
        DO i = 1, num_steps - 1
            y_values(i+1) = y_values(i) * integrate_bt(dt, lam) 
            t_values(i+1) = i*dt
        END DO
    END SUBROUTINE backward_fd
    
END PROGRAM exer1
