! Para este método precisamos das seguintes variaáveis ou entradas

! Variáveis de entrada:

! x Vetor com as coordenadas x dos pontos.

! y Vetor com as coordenada y dos pontos.

! ---------------------------
! Função que definimos

! h

! A derivada segunda Der2

! Saída:

! A derivada segunda (D) pelo método de RICHARD..




! Para calcular a derivada em um ponto (a): tome tres pontos mais proximos de a...
!... Ou seja, x(1)=a-0.1, x(2)=a e x(3)=a+0.1, onde h=0.1



 program Aula14

         implicit none

         real(4) Der2, f, D, h1, h2, DerivadaCalculadora


         real x(3), x1(3)

         integer i



         print*, "Forneça os valores de x para D(h): "
        print*, "----------------------------"
        do i=1, 3

         print*, "Diga x: "

         read*, x(i)

        end do

        h1= x(2)-x(1) ! Esse é para o D(h)

        h2 = h1/2 ! É para o D(h/2) h2=0.1 significa que: x1(1)=1.8+0.1 - x1(2)=2 e x1(3)=2.2-0.1

        x1(1) = x(2) - h2; x1(2) = x(2); x1(3) = x(2)+h2
        
        

        print*, "O valor da derivada no ponto", x(1), "é: ", D(x, x1, h1, h2)
        print*, "----------------------------------"
        print*, "Diga a derivada da calculadora no ponto", x(1)

        read*, DerivadaCalculadora
        print*, "------------------------------------"
        print*, " Erro da precisão da solução: ", (D(x, x1, h1, h2)-DerivadaCalculadora)/DerivadaCalculadora, "%"


end program Aula14



real function f(a)    ! Definimos uma função para teste

    real a

     f = 2**a/a

       return
end function f

real function D(a1, a2, h1, h2)

     real h1, h2, a1(3), a2(3)


      D = (1/3.0)*(4*Der2(a2, h2)-Der2(a1, h1))  ! Não esquecer de colocar 3.0 na fração


       return


end function  D




real function Der2(a1, h)

     real(4) a1(3)

     real, intent(in) :: h


      Der2 =(f(a1(1))-2*f(a1(2))+f(a1(3)))/h**2 ! h = a1(2)-a(1)


       return


end function  Der2



