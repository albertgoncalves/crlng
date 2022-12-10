format ELF64

public main

public THREAD
public SCHED_RSP
public SCHED_RBP

public receive
public send
public stack_overflow

extrn stderr
extrn setlinebuf

extrn scheduler

extrn memory_init

extrn thread_new
extrn thread_kill

extrn channel_ready
extrn channel_push_data
extrn channel_push_wait
extrn channel_pop_data

extrn main_thread

section '.bss' writeable
    SCHED_RSP rq 1
    SCHED_RBP rq 1
    THREAD    rq 1

section '.text' executable

    macro LOAD_THREAD_STACK {
        mov     rax, [THREAD]
        mov     rsp, [rax + 8]       ; NOTE: If the `Thread` struct is
        mov     rbp, [rax + (8 * 2)] ; re-ordered, this will break. Watch out!
    }

    macro YIELD address {
        mov     rax, [THREAD]
        mov     qword [rax + 8], rsp
        mov     qword [rax + (8 * 2)], rbp ; NOTE: Stash stack pointers.
        mov     qword [rax], address       ; NOTE: Stash resume address.

        mov     rsp, qword [SCHED_RSP]
        mov     rbp, qword [SCHED_RBP]
        jmp     scheduler
    }

    macro EXIT {
        mov     rdi, 1
        mov     rax, 60
        syscall
    }


    receive:
        push    rdi
        YIELD   receive_yield
    receive_yield:
        LOAD_THREAD_STACK
        mov     rdi, [rsp]
        call    channel_ready

        test    rax, rax ; NOTE: if (channel_ready()) { ...
        jz      receive_else
    ; receive_if_then:
        mov     rdi, [rsp]
        call    channel_pop_data
        add     rsp, 8
        ret
    receive_else:
        mov     rdi, [rsp]
        mov     rsi, [THREAD]
        call    channel_push_wait
        YIELD   receive_yield


    send:
        call    channel_push_data
        YIELD   send_yield
    send_yield:
        LOAD_THREAD_STACK
        ret


    stack_overflow:
        mov     r10, rsp
        mov     rax, [THREAD]
        mov     r11, [rax + (8 * 3)]
        sub     r10, 2048
        cmp     r10, r11
        jg      stack_overflow_ret
        EXIT
    stack_overflow_ret:
        ret


    main_thread_kill:
        mov     rax, qword [THREAD]
        mov     rsp, qword [rax + 8]
        mov     rbp, qword [rax + 16]
        call    main_thread

        mov     rdi, qword [THREAD]
        call    thread_kill

        mov     rsp, qword [SCHED_RSP]
        mov     rbp, qword [SCHED_RBP]
        jmp     scheduler


    main:
        push    rbp
        mov     rbp, rsp

        mov     rdi, [stderr]
        call    setlinebuf

        call    memory_init

        mov     rdi, main_thread_kill
        call    thread_new

        mov     qword [SCHED_RSP], rsp
        mov     qword [SCHED_RBP], rbp
        jmp     scheduler
