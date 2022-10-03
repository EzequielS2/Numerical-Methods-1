! Para este método precisamos das seguintes variaáveis ou entradas

! Variáveis de entrada:

! x Vetor com as coordenadas x dos pontos.

! y Vetor com as coordenada y dos pontos.


! Para calcular a derivada em um ponto (a): tome tres pontos mais proximos de a...
!... Ou seja, x(1)=a-0.1, x(2)=a e x(3)=a+0.1, onde h=0.1







 program Aula14

         implicit none

         real Der2, f, h ! b(:) é o coeficiente do polinomio

         real x(3)

         integer i



        print*, "Forneça o valor de x para derivar: "
        
        read*, x(2)
        
        print*, "----------------------------"

        print*, "Diga o valor de h para achar a derivada mais p´roxima de", x(2), ":"

        read*, h

        x(1) = x(2)-h
        
        x(3) = x(2)+h
        
        

        print*, "O valor da derivada em x(2) é: ", Der2(x)
        print*, "----------------------------------"



end program Aula14



real function f(a)    ! Definimos uma função para teste

    real a

     f = 3*a**2+2

       return
end function f


real function Der2(a1, h)  ! essa função não retorna valor

     real a1(3), h


      Der2 =(f(a1(1))-2*f(a1(2))+f(a1(3)))/h**2


       return


end function  Der2



