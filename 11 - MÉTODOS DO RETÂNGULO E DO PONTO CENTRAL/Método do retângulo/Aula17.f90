







 program Aula14

         implicit none

         real a, b, f





        print*, "Forne�a os valores do intervalo [a,b] de integra��o para integrar f(x): "

        read*, a, b

        print*, "A integral da fun��o no intevalo [", a, ",", b, "]�:", f(b)*(b-a)


end program Aula14



real function f(a)    ! Definimos uma fun��o para teste

    real a

     f = 3*a**2+2

       return
end function f






