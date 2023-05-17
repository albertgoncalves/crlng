#include <stdint.h>
#include <stdio.h>
#include <time.h>
#include <unistd.h>

#define STATIC_ASSERT(condition) _Static_assert(condition, "!(" #condition ")")

typedef uint8_t  u8;
typedef uint32_t u32;
typedef uint64_t u64;

typedef struct timespec Time;

STATIC_ASSERT(sizeof(void*) == sizeof(u64));

typedef enum {
    FALSE = 0,
    TRUE,
} Bool;

STATIC_ASSERT(sizeof(Bool) == sizeof(u8));

#define U64_MAX 0xFFFFFFFFFFFFFFFF

#define NANO_PER_SECOND  1000000000llu
#define NANO_PER_MILLI   1000000llu
#define MICRO_PER_SECOND 1000000llu
#define NANO_PER_MICRO   (NANO_PER_SECOND / MICRO_PER_SECOND)

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
    #define EXIT_IF(condition) \
        do {                   \
            if (condition) {   \
            }                  \
        } while (0)
#endif

#if VERBOSE
    #define VERBOSE_FPRINTF(...) fprintf(__VA_ARGS__)
#else
    #define VERBOSE_FPRINTF(...) \
        do {                     \
        } while (0)
#endif

#define CAP_BUFFER   (1 << 10)
#define CAP_STACKS   (1 << 3)
#define CAP_THREADS  CAP_STACKS
#define CAP_CHANNELS (1 << 4)
#define CAP_DATAS    (1 << 4)
#define CAP_WAITS    (1 << 3)
#define CAP_CALLS    (1 << 4)
#define CAP_SLEEPS   (1 << 4)

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
    u64          wake_at;
    ThreadStatus status;
};

typedef struct {
    Thread* first;
    Thread* last;
    u32     len;
    u32     len_ready;
    u32     len_sleeping;
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

static Channel CHANNELS[CAP_CHANNELS];
static u64     SLEEPS[CAP_SLEEPS];

static u32 LEN_CHANNELS = 0;
static u32 LEN_SLEEPS = 0;

static ThreadQueue QUEUE = {0};

extern Thread* THREAD;

__attribute__((noreturn)) void scheduler(void);

void memory_init(void);

Thread* thread_new(void (*)(void));
void    thread_kill(Thread*);
void    thread_push_stack(Thread*, void*);
void    thread_sleep(Thread*, u64);

Channel* channel_new(void);
Bool     channel_ready(Channel*);
void     channel_push_data(Channel*, void*);
void     channel_push_wait(Channel*, Thread*);
void*    channel_pop_data(Channel*);

void call_push(const char*);
void call_pop(void);

__attribute__((noreturn)) void panic(void);

static u64 now(void) {
    Time time;
    EXIT_IF(clock_gettime(CLOCK_MONOTONIC, &time));
    return ((u64)time.tv_sec * NANO_PER_SECOND) + (u64)time.tv_nsec;
}

static void sleep_until(u64 future) {
    EXIT_IF(future < now());
    EXIT_IF(usleep(((u32)((future - now()) / NANO_PER_MICRO)) + 1));
}

static Channel* alloc_channel(void) {
    EXIT_IF(CAP_CHANNELS <= LEN_CHANNELS);
    return &CHANNELS[LEN_CHANNELS++];
}

void memory_init(void) {
    VERBOSE_FPRINTF(stderr, "  [ Initializing memory ]\n");
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
    VERBOSE_FPRINTF(stderr,
                    "  [ Pushing thread (%p) onto queue ]\n",
                    (void*)thread);
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
    VERBOSE_FPRINTF(stderr, "  [ Popping thread from queue ]\n");
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
    VERBOSE_FPRINTF(stderr, "  [ Creating thread ]\n");
    Thread* thread = alloc_thread();
    thread->resume = resume;
    Stack* stack = alloc_stack();
    thread->stack = stack;
    thread->rbp = &stack->buffer[CAP_BUFFER];
    thread->rsp = &stack->buffer[CAP_BUFFER];
    thread->call = NULL;
    thread->wake_at = U64_MAX;
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
        VERBOSE_FPRINTF(stderr,
                        "  [ Killing last thread (%p) ]\n",
                        (void*)thread);
        EXIT(OK);
    }
    VERBOSE_FPRINTF(stderr, "  [ Killing thread (%p) ]\n", (void*)thread);
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

static u32 sleep_parent(u32 i) {
    return ((i + 1) / 2) - 1;
}

static u32 sleep_left_sibling(u32 i) {
    return ((i + 1) * 2) - 1;
}

static void sleep_swap(u32 i, u32 j) {
    const u64 x = SLEEPS[i];
    SLEEPS[i] = SLEEPS[j];
    SLEEPS[j] = x;
}

static void sleep_balance_up(u32 i) {
    u32 j = sleep_parent(i);
    while (0 < i) {
        if (SLEEPS[i] < SLEEPS[j]) {
            sleep_swap(i, j);
        }
        i = j;
        j = sleep_parent(i);
    }
}

static void sleep_balance_down(u32 i) {
    for (;;) {
        const u32 l = sleep_left_sibling(i);
        const u32 r = l + 1;
        u32       m = i;
        if ((l < LEN_SLEEPS) && (SLEEPS[l] < SLEEPS[m])) {
            m = l;
        }
        if ((r < LEN_SLEEPS) && (SLEEPS[r] < SLEEPS[m])) {
            m = r;
        }
        if (i == m) {
            return;
        }
        sleep_swap(i, m);
        i = m;
    }
}

static void sleep_push(u64 wake_at) {
    EXIT_IF(CAP_SLEEPS <= LEN_SLEEPS);
    EXIT_IF(wake_at == U64_MAX);
    const u32 i = LEN_SLEEPS++;
    SLEEPS[i] = wake_at;
    sleep_balance_up(i);
}

static u64 sleep_pop(void) {
    EXIT_IF(LEN_SLEEPS == 0);
    const u64 wake_at = SLEEPS[0];
    SLEEPS[0] = SLEEPS[--LEN_SLEEPS];
    sleep_balance_down(0);
    return wake_at;
}

void thread_sleep(Thread* thread, u64 milliseconds) {
    EXIT_IF(!thread);
    EXIT_IF(QUEUE.len_ready == 0);
    VERBOSE_FPRINTF(stderr,
                    "  [ Putting thread to sleep for `%lu ms` ]\n"
                    "  [ Pausing thread ]\n",
                    milliseconds);
    THREAD->status = PAUSED;
    const u64 wake_at = now() + (NANO_PER_MILLI * milliseconds);
    THREAD->wake_at = wake_at;
    sleep_push(wake_at);
    --QUEUE.len_ready;
    ++QUEUE.len_sleeping;
}

Channel* channel_new(void) {
    VERBOSE_FPRINTF(stderr, "  [ Creating channel ]\n");
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
    VERBOSE_FPRINTF(stderr,
                    "  [ Adding thread (%p) to wait list ]\n"
                    "  [ Pausing thread ]\n",
                    (void*)thread);
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
    VERBOSE_FPRINTF(stderr, "  [ Popping thread from wait list ]\n");
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
    VERBOSE_FPRINTF(stderr, "  [ Channel (%p) ready? ]\n", (void*)channel);
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
    VERBOSE_FPRINTF(stderr,
                    "  [ Popping data from channel (%p) ]\n",
                    (void*)channel);
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
    VERBOSE_FPRINTF(stderr, "  [ Pushing `%s` onto call stack ]\n", label);
    Call* call = alloc_call();
    call->label = label;
    call->prev = THREAD->call;
    THREAD->call = call;
}

void call_pop(void) {
    EXIT_IF(!THREAD);
    EXIT_IF(!THREAD->call);
    Call* call = THREAD->call;
    VERBOSE_FPRINTF(stderr,
                    "  [ Popping `%s` from call stack ]\n",
                    call->label);
    THREAD->call = call->prev;
    free_call(call);
}

__attribute__((noreturn)) void panic(void) {
    EXIT_IF(!THREAD);
    EXIT_IF(!THREAD->call);
    fflush(stdout);
    fflush(stderr);
    fprintf(stderr, "\nPanic at\n");
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
    VERBOSE_FPRINTF(stderr, "  [ Resuming scheduler ]\n");
    if (THREADS[0].status == DEAD) {
        VERBOSE_FPRINTF(stderr, "  [ Main thread is dead ]\n");
        EXIT(OK);
    }
    if (QUEUE.len_ready == 0) {
        if (QUEUE.len_sleeping != 0) {
            EXIT_IF(LEN_SLEEPS == 0);
            EXIT_IF(LEN_SLEEPS < QUEUE.len_sleeping);
            u64 wake_at;
            do {
                VERBOSE_FPRINTF(stderr, "  [ Popping sleep ]\n");
                wake_at = sleep_pop();
            } while ((LEN_SLEEPS != 0) && (wake_at < now()));
            if (now() < wake_at) {
                VERBOSE_FPRINTF(
                    stderr,
                    "  [ "
                    "No threads ready, scheduler sleeping for `%lu ns` ]\n",
                    wake_at - now());
                sleep_until(wake_at);
            }
        } else {
            VERBOSE_FPRINTF(stderr, "  [ Deadlock ]\n");
            EXIT(ERROR);
        }
    }
    VERBOSE_FPRINTF(stderr,
                    "  [ "
                    "%u thread(s) alive, "
                    "%u thread(s) ready, "
                    "%u thread(s) sleeping ]\n",
                    QUEUE.len,
                    QUEUE.len_ready,
                    QUEUE.len_sleeping);
#if VERBOSE
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
        if (THREAD->wake_at <= now()) {
            VERBOSE_FPRINTF(stderr,
                            "  [ Waking up thread (%p) ]\n",
                            (void*)THREAD);
            THREAD->status = READY;
            THREAD->wake_at = U64_MAX;
            ++QUEUE.len_ready;
            --QUEUE.len_sleeping;
            break;
        }
        EXIT_IF(QUEUE.len < i);
        VERBOSE_FPRINTF(stderr,
                        "  [ Thread (%p) paused, skipping ]\n",
                        (void*)THREAD);
    }
    VERBOSE_FPRINTF(stderr,
                    "  [ "
                    "%u thread(s) alive, "
                    "%u thread(s) ready, "
                    "%u thread(s) sleeping ]\n",
                    QUEUE.len,
                    QUEUE.len_ready,
                    QUEUE.len_sleeping);
#if VERBOSE
    for (Thread* thread = QUEUE.first; thread; thread = thread->next) {
        fprintf(stderr, "    < %p\n", (void*)thread);
    }
#endif
    VERBOSE_FPRINTF(stderr, "  [ Running thread (%p) ]\n", (void*)THREAD);
    THREAD->resume();
    EXIT(ERROR);
}
