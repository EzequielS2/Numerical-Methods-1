! Para este método precisamos das seguintes variaáveis ou entradas

! Variáveis de entrada:

! x Vetor com as coordenadas x dos pontos.

! y Vetor com as coordenada y dos pontos.









 program Aula14

         implicit none

         real Der, f

         real x(3), y(3)

         integer i



         print*, "Forneça os valore de x para derivar: "
         
         read*, x(3)
         
         
        print*, "----------------------------"

        print*, "Diga o valor de h para aproximar o valor da derivada no ponto", x(3), ":"
        
        read*, h
        
        x(2) = x(3)-h; x(1) = x(2)-h
        
        y(1)= f(x(1)); y(2)= f(x(2)); y(3) = f(x(3))
        



        print*, "O valor da derivada é: ", Der(x)
        print*, "----------------------------------"



end program Aula14



real function f(a)    ! Definimos uma função para teste

    real a

     f = 3*a**2+2

       return
end function f


real function Der(a1)  ! essa função não retorna valor

     real a1(3)


      Der =(f(a1(1))-4*f(a1(2))+3*f(a1(3)))/(2*h)


       return


end function  Der



