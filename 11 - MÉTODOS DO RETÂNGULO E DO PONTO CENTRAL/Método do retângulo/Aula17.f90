







 program Aula14

         implicit none

         real a, b, f





        print*, "Forneça os valores do intervalo [a,b] de integração para integrar f(x): "

        read*, a, b

        print*, "A integral da função no intevalo [", a, ",", b, "]é:", f(b)*(b-a)


end program Aula14



real function f(a)    ! Definimos uma função para teste

    real a

     f = 3*a**2+2

       return
end function f






