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

#if 1
    #define EXIT_IF(condition)            \
        do {                              \
            if (condition) {              \
                printf("%s:%s:%d `%s`\n", \
                       __FILE__,          \
                       __func__,          \
                       __LINE__,          \
                       #condition);       \
                _exit(ERROR);             \
            }                             \
        } while (0)
#else
    #define EXIT_IF(_) \
        do {           \
        } while (0)
#endif

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
    Thread*      prev;
    Thread*      next;
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

#define CAP_BUFFER   (1 << 12)
#define CAP_STACKS   (1 << 4)
#define CAP_THREADS  (1 << 4)
#define CAP_CHANNELS (1 << 4)
#define CAP_DATAS    (1 << 4)
#define CAP_WAITS    (1 << 4)

typedef struct {
    void* buffer[CAP_BUFFER];
} Stack;

static Stack       STACKS[CAP_STACKS];
static Thread      THREADS[CAP_THREADS];
static Channel     CHANNELS[CAP_CHANNELS];
static ChannelData DATAS[CAP_DATAS];
static ChannelWait WAITS[CAP_WAITS];

static u32 LEN_STACKS = 0;
static u32 LEN_THREADS = 0;
static u32 LEN_CHANNELS = 0;
static u32 LEN_DATAS = 0;
static u32 LEN_WAITS = 0;

static ThreadQueue QUEUE = {0};

extern Thread* THREAD;

__attribute__((noreturn)) void scheduler(void);

Thread* thread_new(void (*)(void));
void    thread_kill(Thread*);
void    thread_push_stack(Thread*, void*);

Channel* channel_new(void);
Bool     channel_ready(Channel*);
void     channel_push_data(Channel*, void*);
void     channel_push_wait(Channel*, Thread*);
void*    channel_pop_data(Channel*);

static void queue_push(Thread* thread) {
    EXIT_IF(!thread);
    EXIT_IF(QUEUE.len < QUEUE.len_ready);
#if VERBOSE
    printf("  [ Pushing thread (%p) onto queue ]\n", (void*)thread);
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
    printf("  [ Popping thread from queue ]\n");
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
    EXIT_IF(CAP_THREADS <= LEN_THREADS);
#if VERBOSE
    printf("  [ Creating thread ]\n");
#endif
    Thread* thread = &THREADS[LEN_THREADS++];
    thread->resume = resume;
    void** stack = &STACKS[LEN_STACKS++].buffer[CAP_BUFFER];
    thread->rbp = stack;
    thread->rsp = stack;
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
    EXIT_IF(!thread->prev);
    EXIT_IF(QUEUE.last != thread);
    EXIT_IF(thread->status != READY);
#if VERBOSE
    printf("  [ Killing thread (%p) ]\n", (void*)thread);
#endif
    QUEUE.last = thread->prev;
    QUEUE.last->next = NULL;
    --QUEUE.len;
    --QUEUE.len_ready;
    thread->status = DEAD;
}

void thread_push_stack(Thread* thread, void* data) {
    EXIT_IF(!thread);
#if VERBOSE
    if (data) {
        printf("  [ Pushing data (%p) onto thread stack ]\n", data);
    } else {
        printf("  [ Pushing data (0x0) onto thread stack ]\n");
    }
#endif
    --thread->rsp;
    *thread->rsp = data;
}

Channel* channel_new(void) {
    EXIT_IF(CAP_CHANNELS <= LEN_CHANNELS);
#if VERBOSE
    printf("  [ Creating channel ]\n");
#endif
    Channel* channel = &CHANNELS[LEN_CHANNELS++];
    channel->data_first = NULL;
    channel->data_last = NULL;
    channel->wait_first = NULL;
    channel->wait_last = NULL;
    return channel;
}

void channel_push_wait(Channel* channel, Thread* thread) {
    EXIT_IF(!channel);
    EXIT_IF(!thread);
    EXIT_IF(CAP_WAITS <= LEN_WAITS);
    EXIT_IF(QUEUE.len_ready == 0);
#if VERBOSE
    printf("  [ Adding thread (%p) to wait list ]\n"
           "  [ Pausing thread ]\n",
           (void*)thread);
#endif
    ChannelWait* wait = &WAITS[LEN_WAITS++];
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
    printf("  [ Popping thread from wait list ]\n");
#endif
    channel->wait_first->thread->status = READY;
    ++QUEUE.len_ready;
    if (channel->wait_first == channel->wait_last) {
        channel->wait_first = NULL;
        channel->wait_last = NULL;
    } else {
        channel->wait_first = channel->wait_first->next;
    }
}

Bool channel_ready(Channel* channel) {
    EXIT_IF(!channel);
#if VERBOSE
    printf("  [ Channel (%p) ready? ]\n", (void*)channel);
#endif
    if (channel->data_first && channel->wait_first) {
        channel_pop_wait(channel);
    }
    return channel->data_first != NULL;
}

void channel_push_data(Channel* channel, void* data) {
    EXIT_IF(!channel);
    EXIT_IF(CAP_DATAS <= LEN_DATAS);
#if VERBOSE
    if (data) {
        printf("  [ Pushing message (%p) into channel (%p) ]\n",
               data,
               (void*)channel);
    } else {
        printf("  [ Pushing message (0x0) into channel (%p) ]\n",
               (void*)channel);
    }
#endif
    ChannelData* channel_data = &DATAS[LEN_DATAS++];
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
    printf("  [ Popping data from channel (%p) ]\n", (void*)channel);
#endif
    void* data = channel->data_first->data;
    if (channel->data_first == channel->data_last) {
        channel->data_first = NULL;
        channel->data_last = NULL;
    } else {
        channel->data_first = channel->data_first->next;
    }
    return data;
}

__attribute__((noreturn)) void scheduler(void) {
    EXIT_IF(QUEUE.len == 0);
#if VERBOSE
    printf("  [ Resuming scheduler ]\n");
#endif
    if (THREADS[0].status == DEAD) {
#if VERBOSE
        printf("  [ Main thread is dead ]\n");
#endif
        _exit(OK);
    }
    if (QUEUE.len_ready == 0) {
#if VERBOSE
        printf("  [ Deadlock ]\n");
#endif
        _exit(ERROR);
    }
#if VERBOSE
    printf("  [ %u thread(s) alive, %u thread(s) ready ]\n",
           QUEUE.len,
           QUEUE.len_ready);
    for (Thread* thread = QUEUE.first; thread; thread = thread->next) {
        printf("    > %p\n", (void*)thread);
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
        printf("  [ Thread (%p) paused, skipping ]\n", (void*)THREAD);
#endif
    }
#if VERBOSE
    printf("  [ %u thread(s) alive, %u thread(s) ready ]\n",
           QUEUE.len,
           QUEUE.len_ready);
    for (Thread* thread = QUEUE.first; thread; thread = thread->next) {
        printf("    < %p\n", (void*)thread);
    }
    printf("  [ Running thread (%p) ]\n", (void*)THREAD);
#endif
    THREAD->resume();
    _exit(ERROR);
}
