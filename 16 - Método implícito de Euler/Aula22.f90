







 program Aula14

         implicit none

         real a, b, Dxy, h, denom, xnew, num, x

        real, allocatable :: n(:), t(:)

        integer i, tam, j




        print*, "Forneça os valores do dominio: "

        read*, a, b    ! Atribui o domínio às variáveis a e b


        print*, "Diga o passo de integração: "

        read*, h    ! a largura do passo de integração a h

        tam = (b-a)/h      ! Define o tamanho do vetor

        allocate(n(tam))
        allocate(t(tam))

        print*, "Forneça o valor inicial de y: "

        read*, n(1)  ! Atribui a condição inicial ao primeiro ponto da solução

        t(1) = a   ! Atribui a condição inicial ao primeiro ponto da solução



        print*, "Valor n:", tam

        x= n(1)

        do i=1, tam

           t(i+1) = t(i) +h

           x=n(i)



           ! Tem início o método de Newton.

           do j=1, 20


              num = x - Dxy(x, n(1), t(i+1))*h - n(i)

              denom =  1 + 0.8*1.5*(x**(1/2.0))*h

              xnew = x - num/denom



              if(abs((xnew-x)/x)<0.001) then !Verifica se o erro é pequeno o suficiente para que as iterações sejam interrompidas
                   exit
              else
                x = xnew

              end if

           end do

            if(j==20) then

                      print*, "A solução não pode ser encontrada em t-", t(i)

                      exit
             end if

             ! Termina o método de Newton.

             n(i+1) = xnew   ! A solução obtida com o método de Newton é atribuída à solução da EDO no ponto seguinte.


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









