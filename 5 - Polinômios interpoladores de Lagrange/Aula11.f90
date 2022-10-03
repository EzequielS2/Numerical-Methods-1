! Para este método precisamos das seguintes variaáveis ou entradas

! Determine o polinômio de Lagrange de quarta ordem que passa pelos pontos.

! Variáveis de entrada:

! x Vetor com as coordenadas x dos pontos.

! y Vetor com as coordenada y dos pontos.

! Xinnter é o valor a ser interpolado

! L (que é o lagrange que ajusta o valor a ser interpolado)

! Variáveis de saída:

! Yinter é o valor interpolado







 program Aula11

         implicit none

         real, allocatable :: x(:), y(:), L(:)

         real Xint, FuncLagrange

         integer i, a, b

         print*, "Diga o tamanho de x e y"

         read*, a

         allocate(x(a), y(a))

         print*, "Forneça os pontos para achar a quação: "

        do i=1, a

         print*, "Diga x e y: "

         read*, x(i), y(i)

        end do

        print*, "Diga o valor a ser interpolado: "

        read*, Xint

        print*, "O valor interpolado foi Yinter: ", FuncLagrange(x, y, a, Xint)


end program Aula11


real function FuncLagrange(v, v1, b, Xint)

      implicit none

       real v(b), v1(b), Xint, a

       integer i, j, b

        a=1; FuncLagrange = 0

       do i=1, b
          a=1
          do j=1, b

             if(j/=i) then
                a = a*(Xint-v(j))/(v(i)-v(j))

            end if


           end do
            print*, "--", a*v1(i)
           FuncLagrange = FuncLagrange + a*v1(i)
        end do

        return

end function FuncLagrange
