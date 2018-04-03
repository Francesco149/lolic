/*
 * this is my little compiler/parser playground where I experiment while
 * prototyping my future programming language. once I feel like I'm ready
 * I will likely rewrite the compiler from scratch properly, unless I
 * grow attached to this code base for some reason
 *
 * # license
 * this is free and unencumbered software released into the public domain.
 * refer to the attached UNLICENSE or http://unlicense.org/
 */

#define _GNU_SOURCE

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <stdint.h>

#ifdef LOLIC_DEBUG
#define logf(fmt, ...) \
    fprintf(stderr, \
        "[%s:%d:%s] " fmt "\n", __FILE__, __LINE__, __func__, \
        __VA_ARGS__ \
    )

#define log(msg) logf("%s", msg)

#define assert(cond) \
    for (; !(cond); log("assertion '" #cond "' failed"), exit(1))

#define assertf(cond, fmt, ...) \
    for (; !(cond); \
         logf("assertion '" #cond "' failed: " fmt, __VA_ARGS__), exit(1))
#else
#define logf(fmt, ...)
#define log(msg)
#define assert(cond)
#define assertf(cond, fmt, ...)
#endif

#define errf(fmt, ...) fprintf(stderr, fmt "\n", __VA_ARGS__)
#define err(msg) errf("%s", msg)
#define lenof(a) (sizeof(a) / sizeof((a)[0]))
#define offsetof(t, f) ((size_t)&((t*)0)->f)
#define min(x, y) ((x) < (y) ? (x) : (y))
#define max(x, y) ((x) > (y) ? (x) : (y))

/* --------------------------------------------------------------------- */

struct bufhdr
{
    int len;
    int cap;
    char buf[1];
};

typedef struct bufhdr bufhdr_t;

/* sorry for this macro abomination */

#define bhdr(x) ((bufhdr_t*)((char*)(x) - offsetof(bufhdr_t, buf)))
#define blen(x) ((x) ? bhdr(x)->len : 0)
#define bcap(x) ((x) ? bhdr(x)->cap : 0)
#define bfits(x, n) (blen(x) + (n) <= bcap(x))
#define bfree(x) (x) ? (free(bhdr(x)), x = 0) : 0

#define bfit(x, n) \
    (bfits(x, n) ? 0 \
     : (*(void**)&(x) = bgrw(x, blen(x) + (n), sizeof(*(x)))))

#define bpush(x, val) \
    bfit(x, 1), \
    (x)[blen(x)] = (val), \
    ++bhdr(x)->len

void* bgrw(void* x, int len, int elem_size)
{
    int cap;
    bufhdr_t* hdr;

    cap = max(2 * bcap(x) + 1, len);
    assert(cap >= len);

    hdr = realloc(
        x ? bhdr(x) : 0,
        offsetof(bufhdr_t, buf) + cap * elem_size
    );

    if (!hdr) {
        perror("realloc");
        return 0;
    }

    if (!x) {
        hdr->len = 0;
    }

    hdr->cap = cap;

    return hdr->buf;
}

void test_buf()
{
    int i;
    int* a = 0;

    enum { N = 1024, M = 7 };

    assert(blen(a) == 0);

    for (i = 0; i < N; ++i) {
        bpush(a, i * M);
    }

    assert(blen(a) == N);

    for (i = 0; i < N; ++i) {
        assert(a[i] == i * M);
    }

    bfree(a);

    assert(!a);
    assert(!blen(a));

    log("(passed)");
}

/* --------------------------------------------------------------------- */

struct intern
{
    char* str;
    int len;
};

typedef struct intern intern_t;

intern_t* interns = 0;
char* string_shl;
char* string_shr;

char* istr(char* str);

void init_interns()
{
    string_shl = istr("<<");
    string_shr = istr(">>");
}

char* istr_r(char* start, char* end)
{
    int i;
    int len;
    intern_t new;

    len = end - start;

    for (i = 0; i < blen(interns); ++i)
    {
        if (len == interns[i].len &&
            !strncmp(start, interns[i].str, len))
        {
            return interns[i].str;
        }
    }

    new.str = strndup(start, len);
    new.len = len;
    bpush(interns, new);

    return new.str;
}

char* istr(char* str)
{
    return istr_r(str, str + strlen(str));
}

void test_istr()
{
    char a[] = "hello";
    char b[] = "hello";
    char c[] = "hello!";
    char str[] = "foo bar";
    char sub[] = "foo";

    assert(a != b);
    assert(istr(a) == istr(b));
    assert(istr(c) != istr(b));
    assert(str != sub);
    assert(istr(sub) == istr_r(str, str + 3));
    assert(strlen(istr_r(str, str + 3)) == 3);

    log("(passed)");
}

/* --------------------------------------------------------------------- */

enum
{
    TOKEN_LAST_LITERAL = 128,
    TOKEN_SHL,
    TOKEN_SHR,
    TOKEN_INT,
    TOKEN_NAME,
};

struct token
{
    int kind;
    char* start;
    char* end;

    union
    {
        uint64_t u64;
        char* name;
        /* ... */
    }
    data;
};

typedef struct token token_t;

token_t ltok;
char* ldata;

void lnext();

void linit(char* data)
{
    ldata = data;
    lnext();
}

char* lkindstr(int kind, char* buf)
{
    if (!buf)
    {
        buf = (char*)malloc(4096);
        if (!buf) {
            perror("malloc");
            return 0;
        }
    }

    if (kind <= TOKEN_LAST_LITERAL)
    {
        sprintf(buf, "'%c' '\\x%02X'", (char)kind, kind);
        goto done;
    }

#define c(x) case TOKEN_##x: strcpy(buf, #x); break
    switch (kind)
    {
    c(INT);
    c(NAME);
    c(SHL);
    c(SHR);

    default:
        sprintf(buf, "unknown token %d", kind);
    }
#undef c

done:
    return buf;
}

char* ldescribe(token_t* tok, char* buf)
{
    char *p;

    if (!buf)
    {
        buf = (char*)malloc(4096);
        if (!buf) {
            perror("malloc");
            return 0;
        }
    }

    p = buf;
    lkindstr(tok->kind, p);
    p += strlen(p);

    switch (tok->kind)
    {
    case TOKEN_INT:
        sprintf(p, ": %lu", ltok.data.u64);
        break;

    case TOKEN_NAME:
        sprintf(p, ": %s (%p)", ltok.data.name, ltok.data.name);
        break;
    }

    return buf;
}

void lnext()
{
    for (; isspace(*ldata); ++ldata);

    ltok.start = ldata;

    if (isdigit(*ldata))
    {
        ltok.kind = TOKEN_INT;
        ltok.data.u64 = 0;

        while (isdigit(*ldata))
        {
            ltok.data.u64 *= 10;
            ltok.data.u64 += *ldata++ - '0';
        }
    }

    else if (istr_r(ldata, ldata + 2) == string_shl)
    {
        ltok.kind = TOKEN_SHL;
        ldata += 2;
    }

    else if (istr_r(ldata, ldata + 2) == string_shr)
    {
        ltok.kind = TOKEN_SHR;
        ldata += 2;
    }

    else if (isalpha(*ldata) || *ldata == '_')
    {
        ltok.kind = TOKEN_NAME;
        for (; *ldata && (isalpha(*ldata) || *ldata == '_'); ++ldata);
        ltok.data.name = istr_r(ltok.start, ldata);
    }

    else {
        ltok.kind = *ldata++;
    }

    ltok.end = ldata;
}

void test_lex()
{
    char* src = "XY+(XY)_HELLO1,234+FOO!994memes";
    char buf[64];

    logf("input: %s", src);
    linit(src);

    while (ltok.kind)
    {
        ldescribe(&ltok, buf);
        log(buf);
        lnext();
    }

    log("(passed)");
}

/* --------------------------------------------------------------------- */

enum
{
    OP_HLT,
    OP_LIT,
    OP_ADD,
    OP_SUB,
    OP_XOR,
    OP_OR,
    OP_MUL,
    OP_DIV,
    OP_MOD,
    OP_AND,
    OP_SHL,
    OP_SHR,
    OP_NEG,
    OP_NOT,
};

#define VM_MAXCODE 1024 * 1024
char vm_code[VM_MAXCODE];
char* vm_ip = vm_code;

#define VM_MAXSTACK 8191
uint64_t vm_stack[VM_MAXSTACK + 1];
int vm_sp = 0;

void vm_reset()
{
    vm_ip = vm_code;
    vm_sp = 0;
}

#define vm_chkpush(n) \
    assertf(vm_sp + (n) <= VM_MAXSTACK, "%s", "stack overflow");

#define vm_chkpop(n) \
    assertf(vm_sp >= (n), "%s", "stack underflow");

#define vm_push(x) vm_stack[vm_sp++] = (x)
#define vm_pop() vm_stack[--vm_sp]

uint64_t vm_read8()
{
    uint64_t val;

    assert(vm_ip + 8 <= vm_code + VM_MAXCODE && vm_ip >= vm_code);

    val =
        ((uint64_t)vm_ip[0] <<  0) |
        ((uint64_t)vm_ip[1] <<  8) |
        ((uint64_t)vm_ip[2] << 16) |
        ((uint64_t)vm_ip[3] << 24) |
        ((uint64_t)vm_ip[4] << 32) |
        ((uint64_t)vm_ip[5] << 40) |
        ((uint64_t)vm_ip[6] << 48) |
        ((uint64_t)vm_ip[7] << 56);

    vm_ip += 8;

    return val;
}

void vm_write8(uint64_t val)
{
    assert(vm_ip + 8 <= vm_code + VM_MAXCODE && vm_ip >= vm_code);

    vm_ip[0] = (val >>  0) & 0xFF;
    vm_ip[1] = (val >>  8) & 0xFF;
    vm_ip[2] = (val >> 16) & 0xFF;
    vm_ip[3] = (val >> 24) & 0xFF;
    vm_ip[4] = (val >> 32) & 0xFF;
    vm_ip[5] = (val >> 40) & 0xFF;
    vm_ip[6] = (val >> 48) & 0xFF;
    vm_ip[7] = (val >> 56) & 0xFF;

    vm_ip += 8;
}

uint64_t vm_exec()
{
    char op;
    uint64_t lval, rval;

    while (1)
    {
        assert(vm_ip <= vm_code + VM_MAXCODE && vm_ip >= vm_code);
        op = *vm_ip++;

        switch (op)
        {
        case OP_HLT:
            log("HLT");
            vm_chkpop(1);
            return vm_pop();

        case OP_LIT:
            lval = vm_read8();
            logf("LIT %lu", lval);
            vm_chkpush(1);
            vm_push(lval);
            break;

        case OP_ADD:
        case OP_SUB:
        case OP_XOR:
        case OP_OR:
        case OP_MUL:
        case OP_DIV:
        case OP_MOD:
        case OP_AND:
        case OP_SHL:
        case OP_SHR:
            vm_chkpop(2);
            rval = vm_pop();
            lval = vm_pop();

                 if (op == OP_ADD) vm_push(lval +  rval), log("ADD");
            else if (op == OP_SUB) vm_push(lval -  rval), log("SUB");
            else if (op == OP_XOR) vm_push(lval ^  rval), log("XOR");
            else if (op == OP_OR ) vm_push(lval |  rval), log("OR");
            else if (op == OP_MUL) vm_push(lval *  rval), log("MUL");
            else if (op == OP_DIV) vm_push(lval /  rval), log("DIV");
            else if (op == OP_MOD) vm_push(lval %  rval), log("MOD");
            else if (op == OP_AND) vm_push(lval &  rval), log("AND");
            else if (op == OP_SHL) vm_push(lval << rval), log("SHL");
            else if (op == OP_SHR) vm_push(lval >> rval), log("SHR");
            break;

        case OP_NEG:
        case OP_NOT:
            vm_chkpop(1);
            lval = vm_pop();
            vm_push(op == OP_NEG ? -lval : ~lval);
            log(op == OP_NEG ? "NEG" : "NOT");
            break;

        default:
            assertf(0, "invalid opcode %02X", op);
            return -1;
        }
    }

    return -1;
}

/* --------------------------------------------------------------------- */

#define PMAXLINE 4096

void pexpect(int token)
{
    assertf(ltok.kind == token, "unexpected token. got %s, expected %s",
        ldescribe(&ltok, 0), lkindstr(token, 0));

    lnext();
}

uint64_t pexpr0(char* dst);

uint64_t pexpr3(char *dst)
{
    uint64_t val;

    switch (ltok.kind)
    {
    case '(':
        lnext();
        val = pexpr0(dst);
        pexpect(')');
        break;

    case TOKEN_INT:
        sprintf(dst, "%ld", ltok.data.u64);
        val = ltok.data.u64;
        *vm_ip++ = OP_LIT;
        vm_write8(val);
        lnext();
        break;

    default:
        assertf(0, "unexpected token %s", ldescribe(&ltok, 0));
    }

    return val;
}

uint64_t pexpr2(char *dst)
{
    char* p = dst;
    uint64_t val;
    char op = ltok.kind;

    switch (op)
    {
    case '-':
    case '~':
        *p++ = '(';
        *p++ = ltok.kind;
        *p++ = ' ';
        lnext();
        val = pexpr3(p);
        p += strlen(p);
        *p++ = ')';

        if (op == '-') {
            val = (uint64_t)-val, *vm_ip++ = OP_NEG;
        } else if (op == '~') {
            val = ~val,           *vm_ip++ = OP_NOT;
        } else {
            assertf(0, "uknown operator %s", lkindstr(op, 0));
        }

        break;

    default:
        val = pexpr3(p);
    }

    return val;
}

uint64_t pexpr1(char* dst)
{
    int op;
    uint64_t val;

    val = pexpr2(dst);
    op = ltok.kind;

more:
    switch (ltok.kind)
    {
    case TOKEN_SHR:
    case TOKEN_SHL:
    case '*':
    case '/':
    case '%':
    case '&':
    {
        uint64_t rval;

        char dstbak[PMAXLINE];
        char* p = dst;

        strcpy(dstbak, dst);

        p += sprintf(p, "(%.*s %s ",
            (int)(ltok.end - ltok.start), ltok.start, dstbak);

        lnext();

        rval = pexpr1(p);
        p += strlen(p);
        *p++ = ')';

        if (op == '*') {
            val *= rval, *vm_ip++ = OP_MUL;
        } else if (op == '/') {
            val /= rval, *vm_ip++ = OP_DIV;
        } else if (op == '%') {
            val %= rval, *vm_ip++ = OP_MOD;
        } else if (op == '&') {
            val &= rval, *vm_ip++ = OP_AND;
        } else if (op == TOKEN_SHR) {
            val >>= rval, *vm_ip++ = OP_SHR;
        } else if (op == TOKEN_SHL) {
            val <<= rval, *vm_ip++ = OP_SHL;
        } else {
            assertf(0, "uknown operator %s", lkindstr(op, 0));
        }

        goto more;
    }

    }

    return val;
}

uint64_t pexpr0(char* dst)
{
    uint64_t val;

    val = pexpr1(dst);

more:
    switch (ltok.kind)
    {
    case '+':
    case '-':
    case '^':
    case '|':
    {
        char op = (char)ltok.kind;
        char dstbak[PMAXLINE];
        char* p = dst;
        uint64_t rval;

        strcpy(dstbak, dst);
        lnext();

        p += sprintf(p, "(%c %s ", op, dstbak);
        rval = pexpr1(p);
        p += strlen(p);
        *p++ = ')';

        if (op == '+') {
            val += rval, *vm_ip++ = OP_ADD;
        } else if (op == '-') {
            val -= rval, *vm_ip++ = OP_SUB;
        } else if (op == '^') {
            val ^= rval, *vm_ip++ = OP_XOR;
        } else if (op == '|') {
            val |= rval, *vm_ip++ = OP_OR;
        } else {
            assertf(0, "uknown operator %s", lkindstr(op, 0));
        }

        goto more;
    }

    }

    return val;
}

void test_pexpr(char* expr, uint64_t expected)
{
    uint64_t val;
    char buf[PMAXLINE];

    err("");
    logf("input: %s", expr);
    memset(buf, 0, sizeof(buf));
    linit(expr);
    vm_reset();
    val = pexpr0(buf);
    *vm_ip++ = OP_HLT;
    assertf(!ltok.kind, "%s", "trailing data?");
    log(buf);

    log("");
    log("[interpreter]");
    logf("= %lu", val);
    logf("= 0x%016lX", val);
    assertf(val == expected, "wrong result, expected %lu", expected);

    log("");
    log("[vm]");
    vm_reset();
    val = vm_exec();
    logf("= %lu", val);
    logf("= 0x%016lX", val);
    assertf(val == expected, "wrong result, expected %lu", expected);

    log("(passed)");
}

void test_p()
{
#define t(e) test_pexpr(#e, e)
    t(1);
    t((1));
    t(1*2+3);
    t(1*2*3);
    t(1+2*3);
    t(12*34 + 45/56 + ~25);
    t((1 + 2 + 3) * 5 + 10 / (1 + 1));
    t(1<<0);
    t(1<<1);
    t(1<<1|1<<4|1<<30);
    t(-5+7);
    t(1+-3);
    t(1-2-3);
#undef t
}

/* --------------------------------------------------------------------- */

void init()
{
    init_interns();
}

#ifdef LOLIC_DEBUG
void tests()
{
    test_buf();
    test_istr();
    test_lex();
    test_p();
}
#else
#define tests()
#endif

int main(int argc, char* argv[])
{
    init();
    tests();

    return 0;
}
