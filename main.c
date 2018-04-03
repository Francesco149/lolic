/*
 * prototype of my programming language. will likely be completely
 * rewritten as a compiler bootstrapped from nothing once I have a good
 * idea of what I'm doing.
 *
 * # license
 * this is free and unencumbered software released into the public domain.
 * refer to the attached UNLICENSE or http://unlicense.org/
 */

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

    log("passed");
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
        /* ... */
    }
    data;
};

typedef struct token token_t;

token_t ltok;
char* ldata;

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
        sprintf(buf, "'%c'", (char)kind);
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
        sprintf(p, ": %.*s", (int)(ltok.end - ltok.start), ltok.start);
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

    else if (!strncmp(ldata, "<<", 2))
    {
        ltok.kind = TOKEN_SHL;
        ldata += 2;
    }

    else if (!strncmp(ldata, ">>", 2))
    {
        ltok.kind = TOKEN_SHR;
        ldata += 2;
    }

    else if (isalpha(*ldata) || *ldata == '_')
    {
        ltok.kind = TOKEN_NAME;
        for (; *ldata && (isalpha(*ldata) || *ldata == '_'); ++ldata);
    }

    else {
        ltok.kind = *ldata++;
    }

    ltok.end = ldata;
}

void test_lex()
{
    char* src = "+()_HELLO1,234+FOO!994memes";
    char buf[64];

    logf("input: %s", src);
    ldata = src;
    lnext();

    while (ltok.kind)
    {
        ldescribe(&ltok, buf);
        log(buf);
        lnext();
    }

    log("passed");
}

/* --------------------------------------------------------------------- */

#define PMAXLINE 4096

void pexpect(int token)
{
    assertf(ltok.kind == token, "unexpected token. got %s, expected %s",
        ldescribe(&ltok, 0), lkindstr(token, 0));

    lnext();
}

void pexpr(char* dst);

void pfactor(char *dst)
{
    char* p = dst;

    switch (ltok.kind)
    {
    case '(':
        lnext();
        pexpr(p);
        pexpect(')');
        break;

    case '-':
    case '~':
        *p++ = '(';
        *p++ = ltok.kind;
        *p++ = ' ';
        lnext();
        pfactor(p);
        p += strlen(p);
        *p++ = ')';
        break;

    case TOKEN_INT:
        sprintf(p, "%ld", ltok.data.u64);
        lnext();
        break;

    default:
        assertf(0, "unexpected token %s", ldescribe(&ltok, 0));
    }
}

void pterm(char* dst)
{
    char* operator;

    pfactor(dst);

more:
    switch (ltok.kind)
    {
    case TOKEN_SHR:
        operator = ">>";
        goto doterm;

    case TOKEN_SHL:
        operator = "<<";
        goto doterm;

    case '*':
    case '/':
    case '%':
    case '&':
        operator = (char*)&ltok.kind;
    doterm:
    {
        /* TODO: endian independent */
        char dstbak[PMAXLINE];
        char* p = dst;

        strcpy(dstbak, dst);
        p += sprintf(p, "(%s %s ", operator, dstbak);
        lnext();

        pterm(p);
        p += strlen(p);
        *p++ = ')';

        goto more;
    }

    }
}

void pexpr(char* dst)
{
    pterm(dst);

more:
    switch (ltok.kind)
    {
    case '+':
    case '-':
    case '^':
    case '|':
    {
        char operator = (char)ltok.kind;
        char dstbak[PMAXLINE];
        char* p = dst;

        strcpy(dstbak, dst);
        lnext();

        p += sprintf(p, "(%c %s ", operator, dstbak);
        pterm(p);
        p += strlen(p);
        *p++ = ')';

        goto more;
    }

    }
}

void test_p()
{
    char buf[PMAXLINE];
    char* src = "12*34 + 45/56 + ~25";

    logf("input: %s", src);
    memset(buf, 0, sizeof(buf));
    ldata = src;
    lnext();
    pexpr(buf);
    assertf(!ltok.kind, "%s", "trailing data?");
    log(buf);
    log("passed");
}

/* --------------------------------------------------------------------- */

#ifdef LOLIC_DEBUG
void tests()
{
    test_buf();
    test_lex();
    test_p();
}
#else
#define tests()
#endif

int main(int argc, char* argv[])
{
    tests();

    return 0;
}
