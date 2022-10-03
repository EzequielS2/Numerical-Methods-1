







 program Aula14

         implicit none

         real a, b, Dxy, h

        real, allocatable :: x(:), y(:)

        integer i, n




        print*, "Forne�a o valor inicial de x e o �ltimo valor x: "

        read*, a, b


        print*, "Diga o passo de integra��o: "

        read*, h

        n = (b-a)/h      ! Define o tamanho do vetor

        allocate(x(n))
        allocate(y(n))

        print*, "Forne�a o valor inicial de y: "

        read*, y(1)

        x(1) = a; x(n) = b

        ! Valor inicial x(1) e y(1)

        print*, "Valor n:", n

        i=2

        do while( x(i)/=b .and. i<=n)

              x(i) = x(i-1)+h
              y(i)=y(i-1)+Dxy(x(i-1), y(i-1))*h

              i=i+1
         end do

        y(n)=y(n-1)+Dxy(x(n-1), y(n-1))*h

        print*, "Valores: "
        print*, "----------------"

        do i=1, n

           print*,"i: ", i, "x", x(i), "y: ", y(i)
        end do


end program Aula14



real function Dxy(x, y)    ! Definimos uma EDO para teste

    real x, y

     Dxy = -1.2*y + 7*exp(-0.3*x)    ! exp(x) = e^x

       return
end function Dxy









