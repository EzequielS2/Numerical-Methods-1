







 program Aula14

         implicit none

         real a, b, Dxy, SolExata, h, k1, k2, k3

        real, allocatable :: x(:), y(:)

        integer i, tam




        print*, "Forne�a os valores do dominio: "

        read*, a, b    ! Atribui o dom�nio �s vari�veis a e b


        print*, "Diga o passo de integra��o: "

        read*, h    ! a largura do passo de integra��o a h

        tam = (b-a)/h      ! Define o tamanho do vetor

        allocate(x(tam))
        allocate(y(tam))

        print*, "Forne�a o valor inicial de y: "

        read*, y(1)  ! Atribui a condi��o inicial ao primeiro ponto da solu��o

        x(1) = a; x(tam)=b   ! Atribui a condi��o inicial ao primeiro ponto da solu��o
        print*, "------------------------"

        print*, "Valores iniciais: ", "x:", x(1), "y:", y(1)

        print*, "------------------------"

        print*, "Quantidade de elemntos n:", tam



        do i=1, tam

                x(i+1) = x(i) + h

                k1 = Dxy(x(i), y(i))

                k2 = Dxy(x(i)+(1/2.0)*h, y(i)+(1/2.0)*k1*h)

                k3 =  Dxy(x(i)+h, y(i)-k1*h+2*k2*h)
                
                
                y(i+1) = y(i) + ((1/6.0)*k1+(4/6.0)*k2+(1/6.0)*k3)*h

       end do

       do i=1, tam

            print*, "i:", i, "x:", x(i), "y:", y(i), "Solu��o exata: ", SolExata(x(i))
        end do


end program Aula14



real function Dxy(x, y)    ! Definimos uma EDO para teste

    real x, y, y1

     Dxy = -1.2*y + 7*(exp(-0.3*x))    ! exp(x) = e^x

       return
end function Dxy



real function SolExata(x)

     SolExata = (70/9.0)*exp(-0.3*x)-(43/9.0)*exp(-1.2*x)
     
     return
     
end function SolExata




