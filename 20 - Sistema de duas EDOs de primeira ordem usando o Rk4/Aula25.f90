


  ! Podemos estender esse m�todo para 3 ou mais Edos quisermos (Vamos dar um exemplo de 3 s�)




 program Aula25


         implicit none

         real a, b, Dxy, Dxz, Dxw, h, Ky1, Ky2, Ky3, Ky4, Kz1, Kz2, Kz3, Kz4, Kw1, Kw2, Kw3, Kw4

        real, allocatable :: x(:), y(:), z(:), w(:)

        integer i, tam




        print*, "Forne�a os valores do dominio: "

        read*, a, b    ! Atribui o dom�nio �s vari�veis a e b


        print*, "Diga o passo de integra��o: "

        read*, h    ! a largura do passo de integra��o a h

        tam = (b-a)/h      ! Define o tamanho do vetor

        allocate(x(tam))
        allocate(y(tam))
        allocate(z(tam))
        allocate(w(tam))


        print*, "Forne�a o valor inicial de y: "

        read*, y(1)  ! Atribui a condi��o inicial ao primeiro ponto da solu��o

        print*, "Forne�a o valor inicial de z: "

        read*, z(1)  ! Atribui a condi��o inicial ao primeiro ponto da solu��o

         print*, "Forne�a o valor inicial de z: "

        read*, w(1)  ! Atribui a condi��o inicial ao primeiro ponto da solu��o



        x(1) = a; x(tam)=b   ! Atribui a condi��o inicial ao primeiro ponto da solu��o
        print*, "------------------------"

        print*, "Valores iniciais: ", "x:", x(1), "y:", y(1), "z:", z(1), "w:", w(1)

        print*, "------------------------"

        print*, "Quantidade de elemntos n:", tam



        do i=1, tam

                x(i+1) = x(i) + h

                Ky1 = Dxy(x(i), y(i), z(i), w(i))

                Kz1 = Dxz(x(i), y(i), z(i), w(i))

                Kw1 = Dxw(x(i), y(i), z(i), w(i))

                Ky2 = Dxy(x(i)+(1/2.0)*h, y(i)+(1/2.0)*Ky1*h, z(i)+(1/2.0)*Kz1*h, w(i)+(1/2.0)*Kw1*h)

                Kz2 = Dxz(x(i)+(1/2.0)*h, y(i)+(1/2.0)*Ky1*h, z(i)+(1/2.0)*Kz1*h, w(i)+(1/2.0)*Kw1*h)

                Kw2 = Dxw(x(i)+(1/2.0)*h, y(i)+(1/2.0)*Ky1*h, z(i)+(1/2.0)*Kz1*h, w(i)+(1/2.0)*Kw1*h)

                Ky3 = Dxy(x(i)+(1/2.0)*h, y(i)+(1/2.0)*Ky2*h, z(i)+(1/2.0)*Kz2*h, w(i)+(1/2.0)*Kw2*h)

                Kz3 = Dxz(x(i)+(1/2.0)*h, y(i)+(1/2.0)*Ky2*h, z(i)+(1/2.0)*Kz2*h, w(i)+(1/2.0)*Kw2*h)

                Kw3 = Dxw(x(i)+(1/2.0)*h, y(i)+(1/2.0)*Ky2*h, z(i)+(1/2.0)*Kz2*h, w(i)+(1/2.0)*Kw2*h)

                Ky4 = Dxy(x(i)+h, y(i)+Ky3*h, z(i)+Kz3*h, w(i)+Kw3*h)

                Kz4 = Dxz(x(i)+h, y(i)+Ky3*h, z(i)+Kz3*h, w(i)+Kw3*h)

                Kw4 = Dxw(x(i)+h, y(i)+Ky3*h, z(i)+Kz3*h, w(i)+Kw3*h)



                y(i+1) = y(i) + (1/6.0)*(Ky1 + 2*Ky2 + 2*Ky3 + Ky4)*h

                z(i+1) = z(i) + (1/6.0)*(Kz1 + 2*Kz2 + 2*Kz3 + Kz4)*h

                w(i+1) = w(i) + (1/6.0)*(Kw1 + 2*Kw2 + 2*Kw3 + Kw4)*h

       end do

       do i=1, tam

            print*, "i:", i, "x:", x(i), "y:", y(i), "z:", z(i), "w:", w(i)
        end do


end program Aula25



real function Dxy(x, y, z, w)    ! Primeira 2 EDO

    real x, y, z, w

     Dxy = (-y + z)*exp(1-x) + 0.5*y    ! exp(x) = e^x

       return
end function Dxy



real function Dxz(x, y, z, w)   ! Segunda 2 EDO

      real x, y, z, w

     Dxz = y - z**2 - w

     return

end function Dxz


real function Dxw(x, y, z, w)   ! Segunda 2 EDO

      real x, y, z, w

     Dxw = x + 3*z**2 + w

     return

end function Dxw


