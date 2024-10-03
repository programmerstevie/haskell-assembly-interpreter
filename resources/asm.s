mov ax, 555
mov bx, 378

mov cx, ax
call gcd
msg '------------'
msg 'the gcd of ', cx, ' and ', bx, ' is: ', ax
msg '------------'
end

gcd:
  push bx
  push cx
  jmp cond0
  loop0:
    call mod
    mov cx, ax
    mov ax, bx
    mov bx, cx
  cond0:
    cmp bx, 0
    jne loop0
  pop cx
  pop bx
  ret

mod:
  push bx
  jmp cond1:
  loop1:
    sub ax, bx
  cond1:
    cmp ax, bx
    jge loop1
  pop bx
  ret