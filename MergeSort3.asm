org 100h

.data 
msg1 db 'Enter No of elements: $'  
msg2 db 0ah, 0dh, '$'  ; new line
msg3 db 'Enter elements (separated by spaces): $' 
msg4 db 'All elements entered successfully.$'

start dw ?
mid dw ?
enda dw ? 

left_arr_end dw ?
right_arr_end dw ?  
array dw 200 dup(?)  ; Define an array of 200 bytes (1 byte per element)
left_array dw 100 dup(?) 
right_array dw 100 dup(?)
size dw ? 

New_line MACRO
    ; New line & go to the beginning of the line
    mov dx, offset msg2
    mov ah, 09h
    int 21h
ENDM 

.code
main PROC 
    mov AX, @DATA      ; Initialize data segment
    mov DS, AX
    
    ; Prompt user to enter size of the array
    mov dx, offset msg1
    mov ah, 09h
    int 21h
    New_line
    
    ; Get the number of elements
    mov ah, 01h 
    int 21h
    sub al, 30h        ; Convert ASCII to number
    mov cl, al         ; Store number of elements in CL
    xor ah, ah
    mov cl, al
    mov ch, al 
    cmp cl, 0
    je End_program     ; If zero elements, end the program
    New_line
    
    ; Prompt user to enter array elements
    mov dx, offset msg3
    mov ah, 09h
    int 21h 
    New_line
    call Get_array  

main ENDP    


Read_number PROC
    xor al, al         ; Clear AL
    mov bl, 10h        ; To multiply number by 10 decimal
    mov bh, 0
    xor dx, dx 
    
Read_Digit:
    mov ah, 1h         
    int 21h 
    inc bh
    cmp al, 20h        ; space
    je Read_number     ; If space, store the current number 
    
    cmp al, 0Dh        ; Enter          
    je end_num         
    
    sub al, 30h        ; Convert ASCII to integer
    cmp bh, 2
    je push_num        
    mov dl, al         
    xor ah, ah         
   
    mul bl             ; Multiply AL by 10 (result in AX)
    mov dx, ax
    
    jmp Read_Digit     ; Repeat for next digit

push_num:
    add ax, dx
    mov [di], al       ; Store the number in the array (di points to the next position)
    xor ax, ax         ; Clear DL for the next number
    inc di
    dec cl
    jmp Read_number    ; Continue reading input

end_num:
    jmp get_midd
Endp

Get_array PROC
    mov di, offset array ; Set DI to the start of the array 
    
Get_numbers:
    call Read_number   ; Read a number
    loop Get_numbers   ; Repeat for the specified number of elements
    New_line           ; Add a new line after input
    ret
Endp  

;-----------------------------------------------------mid--------------------------------------------------------
    get_midd:
    mov start, offset array       
    mov di, offset array ; di have first index of the array
    mov si, offset array
    xor ax, ax
    mov al, ch
    add si, ax
    mov enda, si          ; si have last index of the array
                                                
                                                
    ; if p(di)< r(si) then q = (di+si)/2  
    cmp si, di
    jl midd
    
    
    
    midd:
        mov bx, di
        add bx, si
        mov ax, bx
        mov bx, 2
        div bx 
        mov mid, ax 
        
        test ax, 1
        jnz odd_value
        
        odd_value:
            sub ax, 1
            mov midd, ax 
            call MergeP
    

;------------------------------------------------------Merge Sort-------------------------------------------------------
 MergeSort PROC    
    mov ax, start
    mov dx, enda
    cmp ax, dx
    je call MergeP
   
    
    ; MergeSort(A, p, q)
    push start
    push mid
    
    pop mid
    pop start
    call MergeSort
    
    ; MergeSort(A, q+1, r)
    inc start
    push start
    push dx ; endd
    
    pop dx
    pop start
    call MergeSort
    
    ;Mergep(A, p. q, r)
    push start
    push dx
    push mid  
    
    ;call Mergep
    pop mid 
    pop dx
    pop start
    
EndMergeSort:
    RET
MergeSort ENDP 
 
 
;---------------------------------------------------Merge----------------------------------------------------
MergeP PROC    
   merge:    
    ;left_arr_end = mid - start + 1
    mov ax, mid
    xor ah, ah
    xor dh, dh
    mov dx, start
    sub al, dl
    ;inc ax 
    mov [left_arr_end], ax
    
    ;right_arr_end = end - mid
    mov ax, enda
    mov dx, mid
    sub al, dl
    xor ah, ah 
    mov [right_arr_end], ax
    
    ;filling left_arr
    mov cl, 0
    lea di, left_array
    lea si, array
    xor bh, bh
    mov bx, start
    ;xor bh, bh
    ;si points to the 1st value in array 
    ;partitioning the arr into arr[start..mid] and arr[mid+1..end]
        
    ;loop creates the 1st part of arr --> arr[start..mid]
    fill_left_arr_loop:
        xor ch, ch
        cmp cx, left_arr_end
        je fill_right_arr
        ;if not
        add bl, cl
        
        ;getting value from array to left_array
        mov ax, [si] 
        xor ah, ah
        mov [di], ax
        
        inc cl
        inc si
        inc di
        jmp fill_left_arr_loop
    
    ;filling right_arr
    fill_right_arr:
        mov cl, 0
        lea di, right_array
        lea si, array
        add si, left_arr_end
        fill_right_arr_loop:
            cmp cx, right_arr_end
            je merge_arrs
            
            xor bh, bh
            mov bx, mid
            ;xor bh, bh
            add bl, cl
            inc bl
            ;mov si, bx
            mov ax, [si] 
            xor ah, ah
            mov [di], ax
            inc si
            inc cl
            inc di
            jmp fill_right_arr_loop

     merge_arrs:
            mov cx, 0
            mov dx, 0
            lea si, array
            lea di, left_array
            
            merge_loop:
                cmp cx, left_arr_end
                jge right_array_remaining
                
                cmp dx, right_arr_end
                jge left_array_remaining
                
                lea bx, left_array
                     
                add bx, cx ; where am i in left arr
                mov ax, [bx]; value where i stop
                lea bx, right_array
                add bx, dx
                mov dx, [bx]
                xor dh,dh
                cmp ax, dx 
                
                jle copy_from_left ;left< right   
                jge copy_from_right
                
                copy_from_right:
                mov dx, [bx]
                xor dh, dh
                mov si, enda 
                mov [si], dl
                inc bx
                inc si
                xor dx, dx
                inc cx 
                jmp merge_loop

                copy_from_left:
                mov ah, al
                mov [si], ah
                inc cx
                inc si
                xor dx, dx 
                jmp merge_loop    
      
       ;copy the remainging elements of left_arr 
       ;if there is any 
       left_array_remaining:
            xor dx, dx
            cmp dx, left_arr_end
            call Print_Array
            lea bx, left_array
            add bx, cx
            mov ax, [bx] 
            mov [si], ax
            inc dx
            inc si
            jmp left_array_remaining


        ;copy the remaining elements of right_arr 
        ; if there are any 
        right_array_remaining:
             cmp cx, right_arr_end
             je Print_Array
             lea bx, right_array
             add bx, cx
             mov ax, [bx] 
             xor ah, ah
             mov [si], ax
             inc dx
             inc si
             inc cx
             jmp right_array_remaining  



    quit: 
        mov ah,04ch ; function used to terminate the program  
        int 21h
MergeP ENDP


;------------------------------------------------------end---------------------------------------------------------------------



Print_Array PROC
    add si, enda
    lea si, array
    New_line
    lea si, array  ; SI points to the original array
    mov cx, size   ; Load the array size

Print_Loop:
    cmp cx, 0
    je End_program
    mov ax, [si]
    jmp Print_Digit
    mov dl, ' '
    mov ah, 02h
    int 21h
    inc si     ; Increment by 2 bytes since it's word-sized
    dec cx
    loop Print_Loop

Print_Digit:
    mov ah, 02h
    int 21h
    loop Print_Digit
        
ENDP 
;------------------------------------------------------end--------------------------------------------------------------------- 
End_program:
    mov ah, 4Ch        ; Exit program
    int 21h          
end main

    