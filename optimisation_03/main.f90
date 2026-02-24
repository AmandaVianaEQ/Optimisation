PROGRAM plus_grande_pente
     IMPLICIT NONE
    INTEGER i, NN, NNmax
    PARAMETER(NNmax=5)
    DOUBLE PRECISION w
    DOUBLE PRECISION, DIMENSION(NNmax) :: x

    OPEN(10, FILE='donnees.txt',STATUS='OLD')
        READ(10,*) W,NN
        DO i=1,NN
            READ(10,*) x(i)
        END DO
    CLOSE(10)

    CALL methode_grande_pente(w,NN,x)

END PROGRAM

! ---------------------------------
SUBROUTINE methode_grande_pente(w,NN,x)
    IMPLICIT NONE
    INTEGER i, NN, iter, itermax
    DOUBLE PRECISION w,Fonction, EPSILON, som_derivee
    DOUBLE PRECISION, DIMENSION(NN) :: x
    DOUBLE PRECISION, DIMENSION(NN) :: DFDX

    EPSILON = 1d-04
    itermax = 100
    som_derivee = 1.0

    CALL Calc_fonction(NN,x,Fonction)
    CALL Calc_derivee(NN,x,DFDX)

    DO iter=1,itermax
        DO WHILE(som_derivee>=EPSILON)
            som_derivee = 0

            CALL Calc_derivee(NN,x,DFDX)

            DO i=1,NN
                x(i) = x(i) + w*DFDX(i)
            END DO

            DO i=1,NN
                som_derivee = som_derivee + (DFDX(i))**2
            END DO
            som_derivee = SQRT(som_derivee)
        END DO

        DO i=1,NN
            Print *, x(i)
        END DO
        STOP
    ENDDO

END SUBROUTINE

! ---------------------------------
SUBROUTINE Calc_fonction(NN,x, Fonction)
    IMPLICIT NONE
    INTEGER NN
    DOUBLE PRECISION, DIMENSION(NN) :: x
    DOUBLE PRECISION Fonction

    Fonction = (x(1)-1)**2 + (x(2)-3)**2 -4*x(1) + x(2)

    RETURN
END SUBROUTINE

! ---------------------------------
SUBROUTINE Calc_derivee(NN,x,DFDX)
    IMPLICIT NONE
    INTEGER NN
    DOUBLE PRECISION, DIMENSION(NN) :: x
    DOUBLE PRECISION, DIMENSION(NN) :: DFDX

    DFDX(1) = -(2*x(1)-6)
    DFDX(2) = -(2*x(2)-5)

    RETURN
END SUBROUTINE

