! Para este m�todo precisamos das seguintes varia�veis ou entradas

! Vari�veis de entrada:

! x Vetor com as coordenadas x dos pontos.

! y Vetor com as coordenada y dos pontos.









 program Aula14

         implicit none

         real Der2, f ! b(:) � o coeficiente do polinomio

         real x(3)

         integer i



         print*, "Forne�a o valore de x para derivar: "
         read*, x(3)
         
        print*, "----------------------------"

         print*, "Forne�a o valor de h para aproximar mais a deriva no ponto", x(3), ":"
         
         read*, h
         
         x(2) = x(3)-h; x(1) = x(2)-h


        print*, "O valor da derivada �: ", Der2(x, h)
        print*, "----------------------------------"



end program Aula14



real function f(a)    ! Definimos uma fun��o para teste

    real a

     f = 3*a**2+2

       return
end function f


real function Der2(a1, h)  ! essa fun��o n�o retorna valor

     real a1(3), h


      Der2 =(f(a1(1))-2*f(a1(2))+f(a1(3)))/h**2


       return


end function  Der2



