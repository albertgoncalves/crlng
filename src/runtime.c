#include <stdint.h>
#include <stdio.h>
#include <unistd.h>

#define STATIC_ASSERT(condition) _Static_assert(condition, "!(" #condition ")")

typedef uint8_t  u8;
typedef uint32_t u32;
typedef uint64_t u64;

STATIC_ASSERT(sizeof(void*) == sizeof(u64));

typedef enum {
    FALSE = 0,
    TRUE,
} Bool;

STATIC_ASSERT(sizeof(Bool) == sizeof(u8));

#define OK    0
#define ERROR 1

#define VERBOSE 0

#define EXIT(code)      \
    do {                \
        fflush(stdout); \
        fflush(stderr); \
        _exit(code);    \
    } while (0)

#if 1
    #define EXIT_IF(condition)             \
        do {                               \
            if (condition) {               \
                fprintf(stderr,            \
                        "%s:%s:%d `%s`\n", \
                        __FILE__,          \
                        __func__,          \
                        __LINE__,          \
                        #condition);       \
                EXIT(ERROR);               \
            }                              \
        } while (0)
#else
    #define EXIT_IF(_) \
        do {           \
        } while (0)
#endif

#define CAP_BUFFER   (1 << 10)
#define CAP_STACKS   (1 << 3)
#define CAP_THREADS  CAP_STACKS
#define CAP_CHANNELS (1 << 4)
#define CAP_DATAS    (1 << 4)
#define CAP_WAITS    (1 << 3)
#define CAP_CALLS    (1 << 4)

typedef struct {
    void* buffer[CAP_BUFFER];
} Stack;

typedef struct Call Call;

struct Call {
    const char* label;
    Call*       prev;
};

typedef enum {
    DEAD = 0,
    PAUSED,
    READY,
} ThreadStatus;

typedef struct Thread Thread;

struct Thread {
    void (*resume)(void);
    void**       rsp;
    void**       rbp;
    Stack*       stack;
    Thread*      prev;
    Thread*      next;
    Call*        call;
    ThreadStatus status;
};

typedef struct {
    Thread* first;
    Thread* last;
    u32     len;
    u32     len_ready;
} ThreadQueue;

typedef struct ChannelWait ChannelWait;

struct ChannelWait {
    Thread*      thread;
    ChannelWait* next;
};

typedef struct ChannelData ChannelData;

struct ChannelData {
    void*        data;
    ChannelData* next;
};

typedef struct {
    ChannelData* data_first;
    ChannelData* data_last;
    ChannelWait* wait_first;
    ChannelWait* wait_last;
} Channel;

static Channel CHANNELS[CAP_CHANNELS];
static u32     LEN_CHANNELS = 0;

static Stack       STACKS[CAP_STACKS];
static Thread      THREADS[CAP_THREADS];
static ChannelData DATAS[CAP_DATAS];
static ChannelWait WAITS[CAP_WAITS];
static Call        CALLS[CAP_CALLS];

static Stack*       STACK_POOL[CAP_STACKS];
static Thread*      THREAD_POOL[CAP_THREADS];
static ChannelData* DATA_POOL[CAP_DATAS];
static ChannelWait* WAIT_POOL[CAP_WAITS];
static Call*        CALL_POOL[CAP_CALLS];

static u32 LEN_STACKS = CAP_STACKS;
static u32 LEN_THREADS = CAP_THREADS;
static u32 LEN_DATAS = CAP_DATAS;
static u32 LEN_WAITS = CAP_WAITS;
static u32 LEN_CALLS = CAP_CALLS;

static ThreadQueue QUEUE = {0};

extern Thread* THREAD;

__attribute__((noreturn)) void scheduler(void);

void memory_init(void);

Thread* thread_new(void (*)(void));
void    thread_kill(Thread*);
void    thread_push_stack(Thread*, void*);

Channel* channel_new(void);
Bool     channel_ready(Channel*);
void     channel_push_data(Channel*, void*);
void     channel_push_wait(Channel*, Thread*);
void*    channel_pop_data(Channel*);

void call_push(const char*);
void call_pop(void);

__attribute__((noreturn)) void panic(void);

static Channel* alloc_channel(void) {
    EXIT_IF(CAP_CHANNELS <= LEN_CHANNELS);
    return &CHANNELS[LEN_CHANNELS++];
}

void memory_init(void) {
#if VERBOSE
    fprintf(stderr, "  [ Initializing memory ]\n");
#endif
    for (u32 i = 0; i < CAP_STACKS; ++i) {
        STACK_POOL[i] = &STACKS[(CAP_STACKS - 1) - i];
    }
    for (u32 i = 0; i < CAP_THREADS; ++i) {
        THREAD_POOL[i] = &THREADS[(CAP_THREADS - 1) - i];
    }
    for (u32 i = 0; i < CAP_DATAS; ++i) {
        DATA_POOL[i] = &DATAS[(CAP_DATAS - 1) - i];
    }
    for (u32 i = 0; i < CAP_WAITS; ++i) {
        WAIT_POOL[i] = &WAITS[(CAP_WAITS - 1) - i];
    }
    for (u32 i = 0; i < CAP_CALLS; ++i) {
        CALL_POOL[i] = &CALLS[(CAP_CALLS - 1) - i];
    }
}

static Stack* alloc_stack(void) {
    EXIT_IF(LEN_STACKS == 0);
    return STACK_POOL[--LEN_STACKS];
}

static Thread* alloc_thread(void) {
    EXIT_IF(LEN_THREADS == 0);
    return THREAD_POOL[--LEN_THREADS];
}

static ChannelData* alloc_data(void) {
    EXIT_IF(LEN_DATAS == 0);
    return DATA_POOL[--LEN_DATAS];
}

static ChannelWait* alloc_wait(void) {
    EXIT_IF(LEN_WAITS == 0);
    return WAIT_POOL[--LEN_WAITS];
}

static Call* alloc_call(void) {
    EXIT_IF(LEN_CALLS == 0);
    return CALL_POOL[--LEN_CALLS];
}

static void free_stack(Stack* stack) {
    STACK_POOL[LEN_STACKS++] = stack;
}

static void free_thread(Thread* thread) {
    free_stack(thread->stack);
    THREAD_POOL[LEN_THREADS++] = thread;
}

static void free_data(ChannelData* data) {
    DATA_POOL[LEN_DATAS++] = data;
}

static void free_wait(ChannelWait* wait) {
    WAIT_POOL[LEN_WAITS++] = wait;
}

static void free_call(Call* call) {
    CALL_POOL[LEN_CALLS++] = call;
}

static void queue_push(Thread* thread) {
    EXIT_IF(!thread);
    EXIT_IF(QUEUE.len < QUEUE.len_ready);
#if VERBOSE
    fprintf(stderr, "  [ Pushing thread (%p) onto queue ]\n", (void*)thread);
#endif
    if (QUEUE.len == 0) {
        EXIT_IF(QUEUE.first);
        EXIT_IF(QUEUE.last);
        EXIT_IF(QUEUE.len_ready != 0);
        thread->prev = NULL;
        thread->next = NULL;
        QUEUE.first = thread;
        QUEUE.last = thread;
    } else {
        thread->prev = QUEUE.last;
        thread->next = NULL;
        QUEUE.last->next = thread;
        QUEUE.last = thread;
    }
    ++QUEUE.len;
    if (thread->status == READY) {
        ++QUEUE.len_ready;
    }
}

static Thread* queue_pop(void) {
    EXIT_IF(!QUEUE.first);
    EXIT_IF(!QUEUE.last);
    EXIT_IF(QUEUE.len == 0);
#if VERBOSE
    fprintf(stderr, "  [ Popping thread from queue ]\n");
#endif
    Thread* thread = QUEUE.first;
    if (QUEUE.first == QUEUE.last) {
        QUEUE.first = NULL;
        QUEUE.last = NULL;
    } else {
        QUEUE.first = thread->next;
        QUEUE.first->prev = NULL;
    }
    thread->prev = NULL;
    thread->next = NULL;
    --QUEUE.len;
    if (thread->status == READY) {
        --QUEUE.len_ready;
    }
    return thread;
}

Thread* thread_new(void (*resume)(void)) {
    EXIT_IF(!resume);
#if VERBOSE
    fprintf(stderr, "  [ Creating thread ]\n");
#endif
    Thread* thread = alloc_thread();
    thread->resume = resume;
    Stack* stack = alloc_stack();
    thread->stack = stack;
    thread->rbp = &stack->buffer[CAP_BUFFER];
    thread->rsp = &stack->buffer[CAP_BUFFER];
    thread->call = NULL;
    thread->status = READY;
    queue_push(thread);
    return thread;
}

void thread_kill(Thread* thread) {
    EXIT_IF(!thread);
    EXIT_IF(!QUEUE.first);
    EXIT_IF(!QUEUE.last);
    EXIT_IF(QUEUE.len == 0);
    EXIT_IF(thread->next);
    EXIT_IF(QUEUE.last != thread);
    EXIT_IF(thread->status != READY);
    if (!thread->prev) {
        EXIT_IF((void*)thread != &THREADS[0]);
        EXIT_IF(QUEUE.len != 1);
        EXIT_IF(QUEUE.len_ready != 1);
#if VERBOSE
        fprintf(stderr, "  [ Killing last thread (%p) ]\n", (void*)thread);
#endif
        EXIT(OK);
    }
#if VERBOSE
    fprintf(stderr, "  [ Killing thread (%p) ]\n", (void*)thread);
#endif
    QUEUE.last = thread->prev;
    QUEUE.last->next = NULL;
    --QUEUE.len;
    --QUEUE.len_ready;
    thread->status = DEAD;
    free_thread(thread);
}

void thread_push_stack(Thread* thread, void* data) {
    EXIT_IF(!thread);
#if VERBOSE
    if (data) {
        fprintf(stderr, "  [ Pushing data (%p) onto thread stack ]\n", data);
    } else {
        fprintf(stderr, "  [ Pushing data (0x0) onto thread stack ]\n");
    }
#endif
    --thread->rsp;
    *thread->rsp = data;
}

Channel* channel_new(void) {
#if VERBOSE
    fprintf(stderr, "  [ Creating channel ]\n");
#endif
    Channel* channel = alloc_channel();
    channel->data_first = NULL;
    channel->data_last = NULL;
    channel->wait_first = NULL;
    channel->wait_last = NULL;
    return channel;
}

void channel_push_wait(Channel* channel, Thread* thread) {
    EXIT_IF(!channel);
    EXIT_IF(!thread);
    EXIT_IF(QUEUE.len_ready == 0);
#if VERBOSE
    fprintf(stderr,
            "  [ Adding thread (%p) to wait list ]\n"
            "  [ Pausing thread ]\n",
            (void*)thread);
#endif
    ChannelWait* wait = alloc_wait();
    wait->thread = thread;
    wait->next = NULL;
    thread->status = PAUSED;
    --QUEUE.len_ready;
    if (!channel->wait_first) {
        EXIT_IF(channel->wait_last);
        channel->wait_first = wait;
        channel->wait_last = wait;
    } else {
        channel->wait_last->next = wait;
        channel->wait_last = wait;
    }
}

static void channel_pop_wait(Channel* channel) {
    EXIT_IF(!channel);
    EXIT_IF(!channel->data_first);
    EXIT_IF(!channel->data_last);
    EXIT_IF(!channel->wait_first);
    EXIT_IF(!channel->wait_last);
    EXIT_IF(QUEUE.len == QUEUE.len_ready);
#if VERBOSE
    fprintf(stderr, "  [ Popping thread from wait list ]\n");
#endif
    ChannelWait* channel_wait = channel->wait_first;
    channel_wait->thread->status = READY;
    ++QUEUE.len_ready;
    if (channel_wait == channel->wait_last) {
        channel->wait_first = NULL;
        channel->wait_last = NULL;
    } else {
        channel->wait_first = channel_wait->next;
    }
    free_wait(channel_wait);
}

Bool channel_ready(Channel* channel) {
    EXIT_IF(!channel);
#if VERBOSE
    fprintf(stderr, "  [ Channel (%p) ready? ]\n", (void*)channel);
#endif
    if (channel->data_first && channel->wait_first) {
        channel_pop_wait(channel);
    }
    return channel->data_first != NULL;
}

void channel_push_data(Channel* channel, void* data) {
    EXIT_IF(!channel);
#if VERBOSE
    if (data) {
        fprintf(stderr,
                "  [ Pushing message (%p) into channel (%p) ]\n",
                data,
                (void*)channel);
    } else {
        fprintf(stderr,
                "  [ Pushing message (0x0) into channel (%p) ]\n",
                (void*)channel);
    }
#endif
    ChannelData* channel_data = alloc_data();
    channel_data->data = data;
    channel_data->next = NULL;
    if (!channel->data_first) {
        EXIT_IF(channel->data_last);
        channel->data_first = channel_data;
        channel->data_last = channel_data;
    } else {
        channel->data_last->next = channel_data;
        channel->data_last = channel_data;
    }
    if (channel->wait_first) {
        EXIT_IF(!channel->wait_last);
        channel_pop_wait(channel);
    }
}

void* channel_pop_data(Channel* channel) {
    EXIT_IF(!channel);
    EXIT_IF(!channel->data_first);
    EXIT_IF(!channel->data_last);
#if VERBOSE
    fprintf(stderr, "  [ Popping data from channel (%p) ]\n", (void*)channel);
#endif
    ChannelData* channel_data = channel->data_first;
    void*        data = channel_data->data;
    if (channel_data == channel->data_last) {
        channel->data_first = NULL;
        channel->data_last = NULL;
    } else {
        channel->data_first = channel_data->next;
    }
    free_data(channel_data);
    return data;
}

void call_push(const char* label) {
    EXIT_IF(!THREAD);
#if VERBOSE
    fprintf(stderr, "  [ Pushing `%s` onto call stack ]\n", label);
#endif
    Call* call = alloc_call();
    call->label = label;
    call->prev = THREAD->call;
    THREAD->call = call;
}

void call_pop(void) {
    EXIT_IF(!THREAD);
    EXIT_IF(!THREAD->call);
    Call* call = THREAD->call;
#if VERBOSE
    fprintf(stderr, "  [ Popping `%s` from call stack ]\n", call->label);
#endif
    THREAD->call = call->prev;
    free_call(call);
}

__attribute__((noreturn)) void panic(void) {
    EXIT_IF(!THREAD);
    EXIT_IF(!THREAD->call);
    fprintf(stderr, "Panic at\n");
    for (;;) {
        fprintf(stderr, "  `%s`", THREAD->call->label);
        THREAD->call = THREAD->call->prev;
        if (!THREAD->call) {
            fputc('\n', stderr);
            break;
        }
        fprintf(stderr, " called from\n");
    }
    EXIT(ERROR);
}

__attribute__((noreturn)) void scheduler(void) {
    EXIT_IF(QUEUE.len == 0);
#if VERBOSE
    fprintf(stderr, "  [ Resuming scheduler ]\n");
#endif
    if (THREADS[0].status == DEAD) {
#if VERBOSE
        fprintf(stderr, "  [ Main thread is dead ]\n");
#endif
        EXIT(OK);
    }
    if (QUEUE.len_ready == 0) {
#if VERBOSE
        fprintf(stderr, "  [ Deadlock ]\n");
#endif
        EXIT(ERROR);
    }
#if VERBOSE
    fprintf(stderr,
            "  [ %u thread(s) alive, %u thread(s) ready ]\n",
            QUEUE.len,
            QUEUE.len_ready);
    for (Thread* thread = QUEUE.first; thread; thread = thread->next) {
        fprintf(stderr, "    > %p\n", (void*)thread);
    }
#endif
    for (u32 i = 0;; ++i) {
        THREAD = queue_pop();
        EXIT_IF(THREAD->status == DEAD);
        queue_push(THREAD);
        if (THREAD->status == READY) {
            break;
        }
        EXIT_IF(QUEUE.len < i);
#if VERBOSE
        fprintf(stderr, "  [ Thread (%p) paused, skipping ]\n", (void*)THREAD);
#endif
    }
#if VERBOSE
    fprintf(stderr,
            "  [ %u thread(s) alive, %u thread(s) ready ]\n",
            QUEUE.len,
            QUEUE.len_ready);
    for (Thread* thread = QUEUE.first; thread; thread = thread->next) {
        fprintf(stderr, "    < %p\n", (void*)thread);
    }
    fprintf(stderr, "  [ Running thread (%p) ]\n", (void*)THREAD);
#endif
    THREAD->resume();
    EXIT(ERROR);
}
