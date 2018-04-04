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
#define bend(x) ((x) + blen(x))
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

    assert(bcap(x) <= (0x7FFFFFFF / 2));
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
    intern_t* i;
    int len;
    intern_t new;

    len = end - start;

    for (i = interns; i < bend(interns); ++i)
    {
        if (len == i->len && !strncmp(start, i->str, len)) {
            return i->str;
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
    TOKEN_FLOAT,
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
        double f64;
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
    case 0: strcpy(buf, "EOF"); break;

    c(INT);
    c(FLOAT);
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

    case TOKEN_FLOAT:
        sprintf(p, ": %.17g", ltok.data.f64);
        break;

    case TOKEN_NAME:
        sprintf(p, ": %s (%p)", ltok.data.name, ltok.data.name);
        break;
    }

    return buf;
}

uint64_t linteger()
{
    int base = 10;
    uint64_t val;

    val = 0;

    if (*ldata == '0')
    {
        ++ldata;

        if (tolower(*ldata) == 'x') {
            base = 16;
            ++ldata;
        } else if (isdigit(*ldata)) {
            base = 8;
        } else if (tolower(*ldata) == 'b') {
            base = 2;
            ++ldata;
        }
    }

    if (base != 10) {
        assertf(isxdigit(*ldata), "%s", "integer prefix with no value");
    }

    while (isxdigit(*ldata))
    {
        int digit;

        if (*ldata <= '9') {
            digit = *ldata++ - '0';
        } else {
            digit = 10 + tolower(*ldata++) - 'a';
        }

        assert(digit < base);
        assert(val <= (UINT64_MAX - digit) / base);

        val *= base;
        val += digit;
    }

    return val;
}

double lfloat()
{
    char* start = ldata;

    for (; isdigit(*ldata); ++ldata);

    assertf(*ldata == '.', "expected '.' in float lit., got %c", *ldata);
    ++ldata;

    for (; isdigit(*ldata); ++ldata);

    if (tolower(*ldata) == 'e')
    {
        ++ldata;

        if (*ldata == '-' || *ldata == '+') {
            ++ldata;
        }

        assertf(isdigit(*ldata), "%s", "float literal missing exponent");
        for (; isdigit(*ldata); ++ldata);
    }

    return strtod(start, 0);
}

void lnext()
{
    for (; isspace(*ldata); ++ldata);

    ltok.start = ldata;

    if (isdigit(*ldata))
    {
        for (; isdigit(*ldata); ++ldata);

        if (*ldata == '.')
        {
            ldata = ltok.start;
            ltok.kind = TOKEN_FLOAT;
            ltok.data.f64 = lfloat();
        }

        else
        {
            ldata = ltok.start;
            ltok.kind = TOKEN_INT;
            ltok.data.u64 = linteger();
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

        while (isalpha(*ldata) || isdigit(*ldata) || *ldata == '_') {
            ++ldata;
        }

        ltok.data.name = istr_r(ltok.start, ldata);
    }

    else {
        ltok.kind = *ldata++;
    }

    ltok.end = ldata;
}

#define lassert_name(s) \
    assertf(ltok.kind == TOKEN_NAME && ltok.data.name == istr(s), \
        "unexpected token. got %s, expected NAME: %s (%p)", \
        ldescribe(&ltok, 0), s, s); \
    lnext()

#define lassert_int(i) \
    assertf(ltok.kind == TOKEN_INT && ltok.data.u64 == i, \
        "unexpected token. got %s, expected INT: %lu", \
        ldescribe(&ltok, 0), (uint64_t)i); \
    lnext()

#define lassert_lit(c) \
    assertf(ltok.kind == c, \
        "unexpected token. got %s, expected %s", \
        ldescribe(&ltok, 0), lkindstr(c, 0)); \
    lnext()

void test_lex()
{
    char* src = "XY+(XY)_HELLO1,234+FOO!994memes";

    logf("input: %s", src);
    linit(src);

    lassert_name("XY");
    lassert_lit('+');
    lassert_lit('(');
    lassert_name("XY");
    lassert_lit(')');
    lassert_name("_HELLO1");
    lassert_lit(',');
    lassert_int(234);
    lassert_lit('+');
    lassert_name("FOO");
    lassert_lit('!');
    lassert_int(994);
    lassert_name("memes");
    lassert_lit(0);

    log("(passed)");
}

/* --------------------------------------------------------------------- */

#define PMAXLISP 4096

struct operand
{
    int kind;

    union
    {
        uint64_t u64;
        double f64;
    }
    data;
};

typedef struct operand operand_t;

enum
{
    OPERAND_INT,
    OPERAND_FLOAT,
};

char* pkindstr(int kind)
{
    switch (kind)
    {
    case OPERAND_INT: return "uint64";
    case OPERAND_FLOAT: return "double";
    }

    return "unknown";
}

char* pdescribe(operand_t* op, char* dst)
{
    char* p;

    if (!dst)
    {
        dst = malloc(4096);
        if (!dst) {
            perror("malloc");
            return 0;
        }
    }

    p = dst;
    p += sprintf(dst, "(%s)", pkindstr(op->kind));

    switch (op->kind)
    {
    case OPERAND_INT:
        sprintf(p, "%lu", op->data.u64);
        break;

    case OPERAND_FLOAT:
        sprintf(p, "%.17g", op->data.f64);
        break;
    }

    return dst;
}

void pexpect(int token)
{
    assertf(ltok.kind == token, "unexpected token. got %s, expected %s",
        ldescribe(&ltok, 0), lkindstr(token, 0));

    lnext();
}

void pexpr0(operand_t* dst, char* lisp);

void pexpr3(operand_t* dst, char *lisp)
{
    switch (ltok.kind)
    {
    case '(':
        lnext();
        pexpr0(dst, lisp);
        pexpect(')');
        break;

    case TOKEN_INT:
        sprintf(lisp, "%ld", ltok.data.u64);
        dst->kind = OPERAND_INT;
        dst->data.u64 = ltok.data.u64;
        lnext();
        break;

    case TOKEN_FLOAT:
        sprintf(lisp, "%.17g", ltok.data.f64);
        dst->kind = OPERAND_FLOAT;
        dst->data.f64 = ltok.data.f64;
        lnext();
        break;

    default:
        assertf(0, "unexpected token %s", ldescribe(&ltok, 0));
    }
}

void pexpr2(operand_t* dst, char *lisp)
{
    char* p = lisp;
    char op = ltok.kind;

    switch (op)
    {
    case '-':
    case '~':
        *p++ = '(';
        *p++ = ltok.kind;
        *p++ = ' ';
        lnext();
        pexpr3(dst, p);
        p += strlen(p);
        *p++ = ')';

        if (dst->kind == OPERAND_FLOAT)
        {
            if (op == '-') {
                dst->data.f64 = -dst->data.f64;
            } else {
                assertf(0, "unsupported operator '%s' for %s",
                    lkindstr(op, 0), pdescribe(dst, 0));
            }
        }

        else if (dst->kind == OPERAND_INT)
        {
            if (op == '-') {
                dst->data.u64 = (uint64_t)-dst->data.u64;
            } else if (op == '~') {
                dst->data.u64 = ~dst->data.u64;
            } else {
                assertf(0, "unsupported operator '%s' for %s",
                    lkindstr(op, 0), pdescribe(dst, 0));
            }
        }

        else {
            assertf(0, "unsupported operand '%s' for operator %s",
                pdescribe(dst, 0), lkindstr(op, 0));
        }

        break;

    default:
        pexpr3(dst, p);
    }
}

void pexpr1(operand_t* dst, char* lisp)
{
    int op;
    char* opstart;
    char* opend;

    pexpr2(dst, lisp);

more:
    op = ltok.kind;
    opstart = ltok.start;
    opend = ltok.end;

    switch (op)
    {
    case TOKEN_SHR:
    case TOKEN_SHL:
    case '*':
    case '/':
    case '%':
    case '&':
    {
        operand_t rval;

        char dstbak[PMAXLISP];
        char* p = lisp;

        strcpy(dstbak, lisp);

        p += sprintf(p, "(%.*s %s ",
            (int)(ltok.end - ltok.start), ltok.start, dstbak);

        lnext();

        pexpr1(&rval, p);
        p += strlen(p);
        *p++ = ')';

        assertf(dst->kind == rval.kind, "%s %.*s %s: type mismatch",
            pdescribe(dst, 0), (int)(opend - opstart), opstart,
            pdescribe(&rval, 0));

        if (dst->kind == OPERAND_INT)
        {
                 if (op == '*') dst->data.u64 *= rval.data.u64;
            else if (op == '/') dst->data.u64 /= rval.data.u64;
            else if (op == '%') dst->data.u64 %= rval.data.u64;
            else if (op == '&') dst->data.u64 &= rval.data.u64;
            else if (op == TOKEN_SHR) dst->data.u64 >>= rval.data.u64;
            else if (op == TOKEN_SHL) dst->data.u64 <<= rval.data.u64;
            else {
                assertf(0, "uknown operator %s %.*s %s",
                    pdescribe(dst, 0), (int)(opend - opstart), opstart,
                    pdescribe(&rval, 0));
            }
        }

        else if (dst->kind == OPERAND_FLOAT)
        {
                 if (op == '*') dst->data.f64 *= rval.data.f64;
            else if (op == '/') dst->data.f64 /= rval.data.f64;
            else {
                assertf(0, "uknown operator %s %.*s %s",
                    pdescribe(dst, 0), (int)(opend - opstart), opstart,
                    pdescribe(&rval, 0));
            }
        }

        goto more;
    }

    }
}

void pexpr0(operand_t* dst, char* lisp)
{
    char* opstart;
    char* opend;
    char op;

    pexpr1(dst, lisp);

more:
    op = ltok.kind;
    opstart = ltok.start;
    opend = ltok.end;

    switch (op)
    {
    case '+':
    case '-':
    case '^':
    case '|':
    {
        char lispbak[PMAXLISP];
        char* p = lisp;
        operand_t rval;

        strcpy(lispbak, lisp);
        lnext();

        p += sprintf(p, "(%c %s ", op, lispbak);
        pexpr1(&rval, p);
        p += strlen(p);
        *p++ = ')';

        assertf(dst->kind == rval.kind, "%s %.*s %s: type mismatch",
            pdescribe(dst, 0), (int)(opend - opstart), opstart,
            pdescribe(&rval, 0));

        if (dst->kind == OPERAND_INT)
        {
                 if (op == '+') dst->data.u64 += rval.data.u64;
            else if (op == '-') dst->data.u64 -= rval.data.u64;
            else if (op == '^') dst->data.u64 ^= rval.data.u64;
            else if (op == '|') dst->data.u64 |= rval.data.u64;
            else {
                assertf(0, "uknown operator %s %.*s %s",
                    pdescribe(dst, 0), (int)(opend - opstart), opstart,
                    pdescribe(&rval, 0));
            }
        }

        else if (dst->kind == OPERAND_FLOAT)
        {
                 if (op == '+') dst->data.f64 += rval.data.f64;
            else if (op == '-') dst->data.f64 -= rval.data.f64;
            else {
                assertf(0, "uknown operator %s %.*s %s",
                    pdescribe(dst, 0), (int)(opend - opstart), opstart,
                    pdescribe(&rval, 0));
            }
        }

        goto more;
    }

    }
}

void test_pexpr(char* expr, void* pexpected, int expected_kind)
{
    operand_t res;
    char buf[PMAXLISP];

    err("");
    logf("input: %s", expr);
    memset(buf, 0, sizeof(buf));
    linit(expr);
    pexpr0(&res, buf);
    assertf(!ltok.kind, "%s", "trailing data?");
    log(buf);

    assertf(res.kind == expected_kind, "wrong result type %s, expected %s",
        pdescribe(&res, 0), pkindstr(expected_kind));

    log("");

    switch (res.kind)
    {
    case OPERAND_INT:
    {
        uint64_t val = res.data.u64;
        uint64_t expected = *(uint64_t*)pexpected;

        logf("= %lu", val);
        logf("= 0x%016lX", val);
        assertf(val == expected,
            "wrong result, expected %lu", (uint64_t)expected);
        break;
    }

    case OPERAND_FLOAT:
    {
        double val = res.data.f64;
        double expected = *(double*)pexpected;

        logf("= %.17g", val);
        assertf(val == expected,
            "wrong result, expected %.17g", (double)expected);
        break;
    }
    }

    log("(passed)");
}

void test_pexpr_i(char* expr, uint64_t expected)
{
    test_pexpr(expr, &expected, OPERAND_INT);
}

void test_pexpr_f(char* expr, double expected)
{
    test_pexpr(expr, &expected, OPERAND_FLOAT);
}

void test_p()
{
#define i(e) test_pexpr_i(#e, e)
#define f(e) test_pexpr_f(#e, e)
#define b(s, i) test_pexpr_i(s, i)
    i(1);
    i((1));
    i(1*2+3);
    i(1*2*3);
    i(1+2*3);
    i(12*34 + 45/56 + ~25);
    i((1 + 2 + 3) * 5 + 10 / (1 + 1));
    i(1<<0);
    i(1<<1);
    i(1<<1|1<<4|1<<30);
    i(-5+7);
    i(1+-3);
    i(1-2-3);
    test_pexpr_i("18446744073709551615", ~0);
    i(0xFFFF);
    i(0xFFFFFFFF);
    i(0x7FFFFFFF);
    i(0xFFFFFFFFFFFFFFFF);
    i(0755);
    i(0666);
    i(0777);
    b("0b11", 3);
    b("0b11111111", 0xFF);
    b("0b1111111111111111", 0xFFFF);
    b("0b1000000000000000", 0x8000);
    b("0b11111111111111111111111111111111", 0xFFFFFFFF);
    b("0b11111111111111111111111111111111"
        "11111111111111111111111111111111", 0xFFFFFFFFFFFFFFFF);
    f(3.14);
    f(1337.1337+420.420+69.69);
#undef i
#undef f
#undef b
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
