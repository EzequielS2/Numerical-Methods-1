


! Exercício 2)

! Usarei aqui o Método de Gauss-Jordan.



include "tudo"

program main

    use tudo, only: GJ, InverGJ, norm_m, norm_v, Calcerro, CalcResiduo

    implicit none 
    
    integer, parameter :: n=4
    integer :: i, j
    real(8) ::  a(n,n), b(n), InvGJ(n,n), Xgj(n)
    real(8) :: r(n), e(n), Xns(n)
  
! Matriz original [a]
! Matriz [b]
! InvGJ Inverso da matriz [a]
! [Xgj] são as soluções exatas 
! [Xns] são as soluções numéricas
! [r] Rsíduo 
! [e] Erro 


! matriz A

    a(1,1)=3.14159265358979323846
    a(1,2)=-exp(1.0)
    a(1,3)=sqrt(2.0)
    a(1,4)=-sqrt(3.0)
    
    a(2,1)=(3.14159265358979323846)**2 
    a(2,2)=exp(1.0) 
    a(2,3)=-exp(2.0) 
    a(2,4)=3.0/7.0 
    
    a(3,1)=sqrt(5.0)
    a(3,2)=-sqrt(6.0)
    a(3,3)=1.0
    a(3,4)=-sqrt(2.0)
    
    a(4,1)=(3.14159265358979323846)**3
    a(4,2)=exp(2.0)
    a(4,3)=-sqrt(7.0)
    a(4,4)=1.0/9.0
    
    
    
    
!Matriz b

    b(1)=sqrt(11.0)
    b(2)=0.0
    b(3)=3.14159265358979323846
    b(4)=sqrt(2.0)
    
!solução  exata

    data (Xgj(i),   i=1,4) /  0.788,  -3.12,  0.167, 4.55 /  
    
! Imprime Matriz [A]

    write (*,200)
    do i=1,n
        write (*,201) (a(i,j),j=1,n)
    end do
    
    print*, ''
    
! Imprime Matriz [b]

    write (*,217)
    do i=1,n
      write (*,218) (b(i))
    end do
    
    

    print*, '------------------------------------------------------------------------'
    print*, '--------------------------Método Gauss-Jordan-----------------------'
    
!  Inverso+(Soluçã sistema): Gauss-JOrdan

    CALL GJ(a, b,  Xns, n) ! Acha a Soulução do sistema passando a solução para Xns
    
    call InverGJ(a, InvGJ, n) ! Acha Matriz inversa InvGJ
    
! Matriz inversa [A^-1]
    
    write (*,223)
    do i=1,n
      write (*,224) (InvGJ(i,j),j=1,n)
    end do
    
    print*, ''
! Solução  

    write (*,225)
    write (*,226) (Xns(i),i=1,n)
    
    print*, '--------------------------FIM:Gauss-Jordan-----------------------------------'
    print*, '-----------------------------------------------------------------------------'





    WRITE(*,*) "As Soluções exatas do sistema :"
    DO i = 1, n
        WRITE (*,"(3X,'X(', I2,') = ', F16.4)") i,  Xgj(i)
    END DO
    
    print*, ''
    call Calcerro(Xgj, Xns, e, n) ! Erro
    
    WRITE(*,*) "Erro [e]="
    DO i = 1, n
        WRITE (*,*),  e(i)
    END DO
    
    print*, ''
    call CalcResiduo(a, e, r, n) ! Resíduo      
       
    print*, "Resíduo [r]= "
         
    do i=1, n
        print*, r(i)
    end do


    print*, ''
    
    print*, '||A||=', norm_m(n,n,a)
    
    print*, '||A^-1||=', norm_m(n,n,InvGJ)
    
    print*, 'Cond(A)=', norm_m(n,n,a)*norm_m(n,n,InvGJ)
    
    print*, '||r||=', norm_v(n, r)
    
    print*, '||b||=', norm_v(n, b)
    
    print*, ''
    
    print*, 'limite inferior =', (1.0/(norm_m(n,n,a)*norm_m(n,n,InvGJ)))*(norm_v(n, r)/norm_v(n, b))
    
    print*, 'limite superior =', (norm_m(n,n,InvGJ)*norm_m(n,n,a))*(norm_v(n, r)/norm_v(n, b))
    

200 format ('Matriz [A]=') ! Imprime matriz [A]
201 format (6f12.2)


217 format ('Matriz [b]=') ! Imprime matriz [b]
218 format (6f12.2)


223 format ('Matriz inversa de [A]: [A^-1]=') ! Matriz inversa [A^-1]
224 format (6f12.2)

225 format ('Solução do sistema (numérica) [a][X]=[b]: [X] =') ! Imprime solução [X]
226 format (6f12.4)

print*, ''

WRITE(*,*) "*As soluções exatas e numéricas diferem bem pouco."

end program main





