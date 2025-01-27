! Para este m�todo precisamos das seguintes varia�veis ou entradas

! y = a1*x + a0 que melhor se ajusta aos n pontos do conjunto de dados.

! Vari�veis de entrada:

! x Vetor com as coordenadas x dos pontos.

! y Vetor com as coordenada y dos pontos.

! O numero maximo de itera��s

! Vari�veis de sa�da:

! a1 Coeficiente a1.
! a0 Coeficiente a0.


! Nesse programa, voc� acha a equa��o da reta y = a1*x + a0 que mais se adpta aos pontos dados dos vetores x e y




 program Aula10

         implicit none

         real, allocatable :: x(:), y(:)

         real Sx, Sy, Sxy, Sxx, Somarecursiva, Somarecursiva1, Somarecursiva2, a0, a1

         integer i, a, b

         print*, "Diga o tamanho de x e y"

         read*, a

         allocate(x(a), y(a))

         print*, "Forne�a os pontos para achar a qua��o: "

        do i=1, a

         print*, "Diga x e y: "

         read*, x(i), y(i)

        end do

        if(a/=a)   then

        print*, "O n�mero de elemntos de x deve ser igual ao de y - N�o tem solu��o"

        else

         Sx = Somarecursiva(x, a)

         Sy = Somarecursiva(y, a)

         Sxy = Somarecursiva1(x, y, a)

         Sxx = Somarecursiva2(x, a)

         a1 = (a*Sxy-Sx*Sy)/(a*Sxx-(Sx)**2) ! (Sx)**2 = (Sx)^2
         a0 = (Sxx*Sy - Sxy*Sx)/(a*Sxx-(Sx)**2)
        end if

        print*, "A equa��o fica: ",a1, "x + ",a0

        print*, "Sx: ", Sx

        print*, "Sy: ", Sy

        print*, "Sxy: ", Sxy

        print*, "Sxx: ", Sxx
        
end program Aula10



real function Somarecursiva(a, b)

       implicit none
       real a(b)
       integer i, b

       Somarecursiva=0


       do i=1, b

          Somarecursiva = Somarecursiva + a(i)

        end do


        return

end function Somarecursiva

real function Somarecursiva1(a, c, b)

       implicit none
       real a(b), c(b)
       integer i, b

       Somarecursiva1=0


       do i=1, b

          Somarecursiva1 = Somarecursiva1 + a(i)*c(i)

        end do


        return

end function Somarecursiva1


real function Somarecursiva2(a, b)

       implicit none
       real a(b)
       integer i, b

       Somarecursiva2=0


       do i=1, b

          Somarecursiva2 = Somarecursiva2 + a(i)**2

        end do


        return

end function Somarecursiva2

