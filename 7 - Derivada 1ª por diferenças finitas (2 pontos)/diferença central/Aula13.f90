! Para este m�todo precisamos das seguintes varia�veis ou entradas

! Vari�veis de entrada:

! x Vetor com as coordenadas x dos pontos.

! y Vetor com as coordenada y dos pontos.


! O (h) � o que se aproxima mais a derivada no ponto







 program Aula13

         implicit none

         real Der, f ! b(:) � o coeficiente do polinomio

         real x(2), y(2), de


         print*, "Forne�a os valores de x para derivar: "
         
         read*, x(2)
         
         
        print*, "----------------------------"

         y(1)= f(x(1))

          print*, "Forne�a o valor de h para a derivada mai pr�xima de", x(2), ":"      ! Ou podemos pedir x(1) e fazer: h = x(2)-x(1)

         read*, h

          x(1)=x(2)-h
          
          y(1) = f(x(1))
          
        call Der(x, h, de)

        print*, "O valor da derivada �: ", de
        print*, "----------------------------------"



end program Aula13



real function f(a)    ! Definimos uma fun��o para teste

    real a

     f = 3*a**2+2

       return
end function f


subroutine Der(a1, h, de)  ! essa fun��o n�o retorna valor

     real a1(2)
     real, intent(out) :: de   ! Vamos alterar de


       de=(f(a1(2))-f(a1(1)))/h


end subroutine  Der



