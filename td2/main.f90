PROGRAM intervalles_cste
    IMPLICIT NONE
    DOUBLE PRECISION a,b,x1,x2,f1,f2
    DOUBLE PRECISION Calc_fonction
    INTEGER k

    k=0
    x1=0
    x2=0

    PRINT *, "A:"
    READ *, a
    PRINT *, "B:"
    READ *, b

    CALL methode_intervalles(x1,x2,f1,f2,a,b,k)

    PRINT *, "Interation:",k,"Valeur min:",a

END PROGRAM

!------------------------------------------------------

SUBROUTINE methode_intervalles(x1,x2,f1,f2,a,b,k)
    DOUBLE PRECISION a,b,x1,x2,f1,f2,epsilon
    DOUBLE PRECISION Calc_fonction
    INTEGER k

    epsilon = 0.0001

    DO WHILE((b-a)>=epsilon)
        k=k+1
        x1= a+(b-a)/3
        x2= b-(b-a)/3

        f1=Calc_fonction(x1)
        f2=Calc_fonction(x2)

        IF(f1>f2) THEN
            a=x1
        ELSEIF(f1<f2) THEN
            b=x2
        ELSEIF(f1==f2) THEN
            a=x1
            b=x2
        END IF
    END DO
END SUBROUTINE

!------------------------------------------------------

DOUBLE PRECISION FUNCTION Calc_fonction(x)
        DOUBLE PRECISION x
        Calc_fonction = x**2+10*COS(5*x+1)
END FUNCTION

