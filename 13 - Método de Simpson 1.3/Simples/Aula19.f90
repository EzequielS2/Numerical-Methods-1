







 program Aula14

         implicit none

         real a, b, h, f



         h = (b-a)/2.0

        print*, "Forne�a os valores do intervalo [a,b] de integra��o para integrar f(x): "

        read*, a, b

        print*, "A integral da fun��o no intevalo [", a, ",", b, "]�:", (h/3.0)*(f(a)+4*f((a+b)/2.0))+f(b))


end program Aula14



real function f(a)    ! Definimos uma fun��o para teste

    real a

     f = 3*a**2+2

       return
end function f






