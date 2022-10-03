







 program Aula14

         implicit none

         real a, b, Dxy, h, denom, xnew, num, x

        real, allocatable :: n(:), t(:)

        integer i, tam, j




        print*, "Forne�a os valores do dominio: "

        read*, a, b    ! Atribui o dom�nio �s vari�veis a e b


        print*, "Diga o passo de integra��o: "

        read*, h    ! a largura do passo de integra��o a h

        tam = (b-a)/h      ! Define o tamanho do vetor

        allocate(n(tam))
        allocate(t(tam))

        print*, "Forne�a o valor inicial de y: "

        read*, n(1)  ! Atribui a condi��o inicial ao primeiro ponto da solu��o

        t(1) = a   ! Atribui a condi��o inicial ao primeiro ponto da solu��o



        print*, "Valor n:", tam

        x= n(1)

        do i=1, tam

           t(i+1) = t(i) +h

           x=n(i)



           ! Tem in�cio o m�todo de Newton.

           do j=1, 20


              num = x - Dxy(x, n(1), t(i+1))*h - n(i)

              denom =  1 + 0.8*1.5*(x**(1/2.0))*h

              xnew = x - num/denom



              if(abs((xnew-x)/x)<0.001) then !Verifica se o erro � pequeno o suficiente para que as itera��es sejam interrompidas
                   exit
              else
                x = xnew

              end if

           end do

            if(j==20) then

                      print*, "A solu��o n�o pode ser encontrada em t-", t(i)

                      exit
             end if

             ! Termina o m�todo de Newton.

             n(i+1) = xnew   ! A solu��o obtida com o m�todo de Newton � atribu�da � solu��o da EDO no ponto seguinte.


        end do

        do i=1, tam

           print*, "i:", i, "x: ", t(i), "y: ", n(i)
        end do






end program Aula14



real function Dxy(x, y, y1)    ! Definimos uma EDO para teste

    real x, y, y1

     Dxy = -0.8*x**(3/2.0) + 10.0*y*(1-exp(-3.0*y1))    ! exp(x) = e^x

       return
end function Dxy









