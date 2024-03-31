; Space Invaders in 512 bytes (Boot Sector)
; Needs to be well optimised
; link https://github.com/nanochess/Invaders/blob/master/invaders.asm (Reference)
; by Ritabrata Mandal
; Start date: 14-05-2022


    %ifndef pure8088
pure8088:       equ 1

        cpu 8086
    %endif

    %ifndef com_file             ;If not defined create a boot sector
com_file:       equ 0
    %endif

base:           equ 0xfc80      ;Memory base (Same as Video Segment)

shots:          equ base+0x00   ;Contains 4 shots (2bytes each)
                                ;must be divisible by SPRITE_SIZE
old_time:       equ base+0x0c   ;old_time
level:          equ base+0x10   ;Current level number
lives:          equ base+0x11   ;Current lives
sprites:        equ base+0x12   ;Space to contain sprite table

SHIP_ROW:       equ 0x5c*OFFSET_X
X_WIDTH:        equ 0x0140
OFFSET_X:       equ X_WIDTH*2
SPRITE_SIZE:    equ 4           ;4 bytes for the sprite (each)

        ;Color table
        ;All colors must be dfferent to distinguish things

SPACESHIP_COLOR:    equ 0x1c    ;Must be below 0x20
BARRIER_COLOR:      equ 0x0b
SHIP_EXPLOSION_COLOR:       equ 0x0e
INVADER_EXPLOSION_COLOR:    equ 0x0e
BULLET_COLOR:       equ 0x0c
START_COLOR:        equ ((sprites+SPRITE_SIZE-(shots+2))/SPRITE_SIZE+0x20)

        ;Color table end

    %if com_file
        org 0x0100      ;Start position for COM files
    %else
        org 0x7c00      ;Start position for boot Sector
     %endif
        mov ax, 0x0013  ;Set mode 0x13 (320x200|256 colors|VGA)
        int 0x10
        cld
        mov ax, 0xa000  ;Point to Screen Memory
        mov ds, ax      ;Both DS
        mov es, ax      ;And ES
        mov ah, 0x04
        mov [level], ax ;Level = 0, Lives = 4

restart_game:
        xor ax, ax
        mov cx, level/2 ;Clear screen and variables (except level and lives)
        xor di, di
        rep
        stosw           ;ch is 0 from here

        ;Setup descend state

        mov ax, [di]    ;ak contains level, ah contains Lives
        inc ax          ;Increase descend by
        inc ax          ;2 for proper descent
        stosw           ;Advance Level
        mov ah, al
        xchg ax, dx     ;Shouldn't damage dx starting here

        ;Setup the spaceship

        mov ax, SPACESHIP_COLOR*0x100+0x00
        stosw
        mov ax, SHIP_ROW+0x4c*2
        stosw

        ;Setup the Invaders

        mov ax, 0x08*OFFSET_X+0x28
        mov bx, START_COLOR*0x0100+0x10
in1:    mov cl, 0x0b        ;11 Invaders per row
in5:    stosw               ;Set invader position
        add ax, 0x0b*2      ;Go to next column
        xchg ax, bx
        stosw               ;Set Invader coloe and shape
        inc ah              ;Next color
        xchg ax, bx
        loop in5
        add ax, 0x09*OFFSET_X-0x000b*0x000b*2   ;Next row
        cmp bh, START_COLOR ;Whole board finished?
        jne in1             ;No, jump

        ;Draw the barriers

        mov di, 0x55*0x280+0x10*2
        mov cl, 5
in48:
        mov ax, BARRIER_COLOR*0x011+0x04
        call draw_sprite
        add di, 0x1e*2
        loop in48

        ;ch is 0

in14:
        mov si, sprites+SPRITE_SIZE

        ; Game loop

        ; Globals:
        ; SI = Next Invader to animate
        ; DL = state
        ; DH = next state
        ; CH = dead invaders
        ; BP = frame counter

in46:
        cmp byte [si+2], 0x20  ;Current invaer is cosmic debri?
        jc in2                  ;No, jump
        inc ch                  ;Count another dead invader
        cmp ch, 55              ;All invaders defeated?
        je restart_game
        ;Todo: Add sound if fits in boot sector

in6:
        lodsw                   ;Load position in ax
        xchg ax, di
        lodsw                   ;Get the type of sprite
        cmp al, 0x28
        je in27                 ;Jump if destroyed
        cmp al, 0x20            ;Explosion?
        jne in29                ;No, Jump
        mov byte [si-2], 0x28   ;Dont draw again

in29:   call draw_sprite

in27:   cmp si, sprites+56*SPRITE_SIZE  ;Whole board revised?
        jne in46                ;No, Jump
        mov al, dh
        sub al, 2               ;Going down?
        jc in14                 ;No, presere left/right direction
        xor al, 1               ;Switch direction
        mov dl, al
        mov dh, al
        jmp in14

in2:
        xor byte [si+2], 8      ;Invader anim (before possible explosion)

        ;Sync game to 18.20648 hz of BIOS

        inc bp
        and bp, 7               ;Each 8 invaders
    %if pure8088
        push dx
        push si
        push bp
    %else
        pusha
    %endif
        jne in12

in22:
        mov ah, 0x00
        int 0x1a                ;BIOS clock read
        cmp dx, [old_time]      ;Wait for change
        je in22
        mov [old_time], dx      ;Save new current time

in12:
    %if 1

        ;Handle player BULLET

        mov si, shots       ;point to shots list
        mov cx, 4           ;4 shots at most
        lodsw               ;Read position of player
        cmp ax, X_WIDTH     ;Is it at top?
        xchg ax, di
        jc in31             ;Erase bullet
                            ;Dosent mind doing it all the time
        call zero           ;Remove bullet
        sub di, X_WIDTH+2
        mov al, [di]        ;Read pixel
        sub al, 0x20        ;Hits invader?
        jc in30             ;No, jump
    %if pure8088
        push si
        push di
    %else
        pusha
    %endif
        mov ah, SPRITE_SIZE ;The pixel indicates the
        mul ah              ;invader Hit
        add si, ax
        lodsw
        xchg ax, di
        mov byte [si], 0x20 ;Erase next time
        mov ax, INVADER_EXPLOSION_COLOR*0x0100+0x08 ;But explosion now
        call draw_sprite    ;Draw sprite
    %if pure8088
        pop di
        pop si
    %else
        popa
    %endif
        jmp in31

        ;Handle invader bullets

in24:
        lodsw                   ;Read current coordinate
        or ax, ax               ;Is it falling
        je in23                 ;No, jump
        cmp ax, 0x60*OFFSET_X   ;Pixel lower than ship?
        xchg ax, di
        jnc in31                ;Yes, remove the bullet
        call zero               ;Remove the bullet
        add di, X_WIDTH-2       ;Bullet falls down

        ;Draw BULLET
in30:
        mov ax, BULLET_COLOR*0x0100+BULLET_COLOR
        mov [si-2], di          ;Update position
        cmp byte [di+X_WIDTH], BARRIER_COLOR    ;Barrier in path?
        jne in7                 ;Yes, erase bullet and barrier pixel

        ;Remove bullet
in31:   xor ax, ax              ;AX contains 0 (DI unaffected)
        mov [si-2], ax          ;Delete bullet from table

in7:    cmp byte [di], SPACESHIP_COLOR  ;Chech collision with player
        jne in41                        ;No, jump
        mov word [sprites], SHIP_EXPLOSION_COLOR*0x0100+0x38 ;Player Explosion

in41:
        call big_pixel          ;Draw/erase bullet

in23:   loop in24
    %endif

        ;spaceship handling

        mov si, sprites         ;Point to spaceship
        lodsw                   ;load sprite frame, color
        or al, al               ;Explosion?
        je in42                 ;No, Jump
        add al, 0x08            ;Keep explosion
        jne in42                ;finished? No, jump
        mov ah, SPACESHIP_COLOR ;Restore color
        dec byte [lives]        ;Decrease one life
        js in10                 ;Exit if all used

in42:   mov [si-2], ax          ;Save new color
        mov di, [si]            ;Load position
        call draw_sprite        ;Draw sprite
        jne in43                ;Jump if still explosion

        mov ah, 0x02            ;BIOS get keyboard flags
        int 0x16

    %if com_file
        test al, 0x10           ;Test for Scroll Lock and exit
        jnz in10
    %endif
        test al, 0x04           ;Ctrk key?
        jz in17                 ;No, jump
        dec di                  ;move 2 pixels left
        dec di

in17:   test al, 0x08           ;Alt?
        jz in18                 ;No, jump
        inc di
        inc di

in18:
        test al, 0x03           ;Shift key?
        jz in35                 ;No, jump
        cmp word [shots], 0     ;Bullet Availible?
        jne in35                ;No, jump
        lea ax, [di+(0x04*2)]   ;Offset from spaceship
        mov [shots], ax         ;Start bullet

in35:
        xchg ax, di
        cmp ax, SHIP_ROW-2       ;Update if not touching border
        je in43
        cmp ax, SHIP_ROW+0x0132
        je in43

in19:   mov [si], ax            ;Update position

in43:
    %if pure8088
        pop bp
        pop si
        pop dx
    %else
        popa
    %endif

        mov ax, [si]            ;Get position of current invader
        cmp dl, 1               ;Going down?
        jbe in9                 ;No, jump
        add ax, 0x0280          ;Go down by 2 pixels
        cmp ax, 0x55*0x280      ;Reaches Earth?
        jc in8                  ;No, jump

in10:
    %if com_file
        mov ax, 0x0003          ;Text mode
        int 0x10
    %endif
        int 0x20                ;Exit

in9:    dec ax                  ;Moving left
        dec ax
        jc in20
        add ax, 4               ;Moving right

in20:   push ax
        shr ax, 1               ;Divide position by 2
        mov cl, 0xa0            ;means we can get column by dividing 0xa0
        div cl                  ;instead of 0x0140 (Longer code Must maintain efficiency)
        dec ah                  ;Convert 0x00 to 0xff
        cmp ah, 0x94            ;Border touched?
        pop ax
        jb in8                  ;No, jump
        or dh, 22               ;Goes down by 11 px (11*2) must be odd

in8:    mov [si], ax
        add ax, 0x06*0x280+0x03*2   ;Offset fir bullet
        xchg ax, bx

        mov cx, 3       ;Invader alive if ch=0
        in al, (0x40)   ;Read timer
        cmp al, 0xfc    ;Random event happening
        jc in4          ;no, Jump
        ; Doesn't work in my (original Author's) computer:
        ;
        ; mov di,shots+2
        ; xor ax,ax
        ; repne scasw
        ; mov [di-2],bx
        ;
        mov di, shots+2

in45:   cmp word [di], 0    ;Search for free slot
        je in44             ;It's free, jump
        scasw               ;Advance di
        loop in45           ;Until 3 slots searched

in44:
        mov [di], bx        ;Start invader shot (or put in ignored slot)

in4:
        jmp in6

        ;Now the bitmaps for sprites
bitmaps:
        db 0x18, 0x18, 0x3c, 0x24, 0x3c, 0x7e, 0xFf, 0x24       ;Spaceship
        db 0x00, 0x80, 0x42, 0x18, 0x10, 0x48, 0x82, 0x01       ;Explosion
        db 0x00, 0xbd, 0xdb, 0x7e, 0x24, 0x3c, 0x66, 0xc3       ;Invaders (frame1)
        db 0x00, 0x3c, 0x5a, 0xff, 0xa5, 0x3c, 0x66, 0x66       ;Invaders (frame2)
        db 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00       ;Erase

        ;Draw pixel per Carry (use AX if Carry=1 or zero if Carry=0)

bit:    jc big_pixel

zero:   xor ax, ax
        ;Draw a big pixel

big_pixel:
        mov [di+X_WIDTH], ax
        stosw
        ret

        ;ah = sprite color
        ;al = sprite (x8)
        ;di = target address

draw_sprite:
    %if pure8088
        push cx
        push di
        pushf
    %else
        pusha
    %endif

in3:    push ax
        mov bx, bitmaps
        cs xlat             ;Extract one byte from bitmap
        xchg ax, bx         ;bl contains byte, bh contains color
        mov cx, 10          ;Two exta zero pixels at left and right
        clc                 ;Left pixel as clean

in0:    mov al, bh          ;Duplicate color in al
        mov ah, bh
        call bit            ;Draw pixel
        shl bl, 1
        loop in0
        add di, OFFSET_X-20 ;Goto next video line
        pop ax
        inc ax              ;Next bitmap byte
        test al, 7          ;Sprite complete?
        jne in3             ;No, jump

    %if pure8088
        popf
        pop di
        pop cx
    %else
        popa
    %endif
        ret

    ;End
    %if com_file
    %else
        times 510-($-$$) db 0x4f
        db 0x55, 0xAA           ;Make it a bootable Sector
    %endif
