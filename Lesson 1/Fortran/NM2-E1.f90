REAL FUNCTION integrate_ft(dt, lam) RESULT(y)
    REAL, INTENT(IN) :: dt, lam
    y = 1 - lam*dt
END FUNCTION integrate_ft

REAL FUNCTION exact_sol(t, y0, lam) RESULT(y)
    REAL, INTENT(IN) :: t, y0, lam
    y = y0* EXP(-lam*t)
END FUNCTION exact_sol

PROGRAM exer1
    IMPLICIT NONE
    REAL :: lam, y0, end_time, dt
    REAL, ALLOCATABLE :: y_values(:), t_values(:)
    INTEGER :: ios, num_steps, i
    REAL, EXTERNAL :: exact_sol

    lam = 0.1
    y0 = 1.0
    end_time = 200

    dt = 0.01


    num_steps = FLOOR(end_time/dt) + 1
    ALLOCATE(y_values(num_steps), t_values(num_steps))
    y_values(1) = y0
    t_values(1) = 0.0

    OPEN(UNIT=10, IOSTAT = ios, FILE = 'FT.txt', ACTION ='WRITE')       
        IF (ios == 0) THEN
                CALL forward_fd(lam, dt, num_steps, y_values, t_values)
                DO i = 1, num_steps
                    WRITE(10, *) t_values(i), y_values(i) - exact_sol(t_values(i), y0, lam) 
                END DO
                CLOSE(10)
        ELSE
                PRINT '(a25)', 'Error: file not opened.'
        END IF


CONTAINS
    SUBROUTINE forward_fd(lam, dt, num_steps, y_values, t_values)
        REAL, INTENT(IN) :: lam, dt
        REAL, INTENT(OUT) :: y_values(:), t_values(:)
        INTEGER :: num_steps, i
        REAL, EXTERNAL :: integrate_ft
        DO i = 1, num_steps
            y_values(i+1) = y_values(i) * integrate_ft(dt, lam) 
            t_values(i+1) = i*dt
        END DO
    END SUBROUTINE forward_fd

    
END PROGRAM exer1