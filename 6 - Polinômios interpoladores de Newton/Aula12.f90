! Para este m�todo precisamos das seguintes varia�veis ou entradas

! Vari�veis de entrada:

! x Vetor com as coordenadas x dos pontos.

! y Vetor com as coordenada y dos pontos.

! Xinnter � o valor a ser interpolado

! L (que � o lagrange que ajusta o valor a ser interpolado)

! Vari�veis de sa�da:

! Yinter � o valor interpolado







 program Aula12

         implicit none

         real, allocatable :: x(:), y(:), b(:), DivDif(:,:) ! b(:) � o coeficiente do polinomio

         real Xint, Yint, Xn

         integer i, a, j

         print*, "Diga o tamanho de x e y"

         read*, a

         allocate(x(a), y(a), DivDif(a-1, a-1), b(a))

         print*, "Forne�a os pontos para achar a qua��o: "

        do i=1, a

         print*, "Diga x e y: "

         read*, x(i), y(i)

        end do

        do i=1, a-1

           DivDif(i,1)=(y(i+1)-y(i))/(x(i+1)-x(i))

        end do

        do j=2, a-1

                do i=1, a-j

                    DivDif(i,j)=(DivDif(i+1, j-1)-DivDif(i,j-1))/(x(j+i)-x(i))

                end do
       end do
       b(1)=y(1)
       do j=2, a

          b(j)= DivDif(1,j-1)
        end do

        print*, "Diga o valor a ser interpolado: "

        read*, Xint

        Yint =b(1)
        Xn = 1

        do i=2, a

           Xn=Xn*(Xint - x(i-1))

           Yint = Yint + b(i)*Xn
        end do



        print*, "O valor interpolado foi Yinter: ", Yint
        print*, "----------------------------------"
        do i=1, a

        print*, "O valor do coeficiente a(",i,")" , b(i)

        end do


end program Aula12

