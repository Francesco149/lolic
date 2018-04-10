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
#include <math.h>

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
#define round_down(x, po2) ((x) & ~((po2) - 1))
#define round_up(x, po2) round_down((x) + (po2), po2)
#define roundptr_up(x, po2) (void*)round_up((uintptr_t)(x), po2)
#define roundptr_down(x, po2) (void*)round_down((uintptr_t)(x), po2)

/* --------------------------------------------------------------------- */

#define xmalloc(n) xmalloc_(n, __FILE__, __LINE__)
#define xrealloc(p, n) xrealloc_(p, n, __FILE__, __LINE__)

void* xmalloc_(int n, char* file, int line)
{
    void* res;

    res = malloc(n);
#ifdef LOLIC_MALLOC_DEBUG
    errf("[%s:%d] malloc(%d) -> %p", file, line, n, res);
#endif

    if (!res)
    {
        errf("[%s:%d] malloc failed", file, line);
        perror("malloc");
        exit(1);
    }

    return res;
}

void* xrealloc_(void* p, int n, char* file, int line)
{
    void* res;

    res = realloc(p, n);
#ifdef LOLIC_MALLOC_DEBUG
    errf("[%s:%d] realloc(%p, %d) -> %p", file, line, p, n, res);
#endif

    if (!res)
    {
        errf("[%s:%d] realloc failed", file, line);
        perror("realloc");
        exit(1);
    }

    return res;
}

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

    hdr = xrealloc(
        x ? bhdr(x) : 0,
        offsetof(bufhdr_t, buf) + cap * elem_size
    );

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

char* istr(char* str);

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

#define syntax_errorf(fmt, ...) fprintf(stderr, fmt "\n", __VA_ARGS__)
#define syntax_error(fmt) fprintf(stderr, fmt "\n")
#define syntax_assert(cond, msg) for (; !(cond); syntax_error(msg))
#define syntax_assertf(cond, ...) \
    for (; !(cond); syntax_errorf(__VA_ARGS__))

enum
{
    TOKEN_LAST_LITERAL = 127,

    TOKEN_INT,
    TOKEN_FLOAT,
    TOKEN_NAME,
    TOKEN_STRING,

    TOKEN_OROR,
    TOKEN_ANDAND,

    TOKEN_FIRST_CMP,
    TOKEN_EQEQ = TOKEN_FIRST_CMP,
    TOKEN_NE,
    TOKEN_BE,
    TOKEN_GE,
    TOKEN_GT,
    TOKEN_LT,
    TOKEN_LAST_CMP = TOKEN_LT,

    TOKEN_FIRST_ADD,
    TOKEN_ADD = TOKEN_FIRST_ADD,
    TOKEN_SUB,
    TOKEN_OR,
    TOKEN_XOR,
    TOKEN_LAST_ADD = TOKEN_XOR,

    TOKEN_FIRST_MUL,
    TOKEN_MUL = TOKEN_FIRST_MUL,
    TOKEN_DIV,
    TOKEN_AND,
    TOKEN_MOD,
    TOKEN_SHL,
    TOKEN_SHR,
    TOKEN_LAST_MUL = TOKEN_SHR,

    TOKEN_FIRST_EQ,
    TOKEN_EQ = TOKEN_FIRST_EQ,
    TOKEN_ADDEQ,
    TOKEN_SUBEQ,
    TOKEN_MULEQ,
    TOKEN_DIVEQ,
    TOKEN_MODEQ,
    TOKEN_XOREQ,
    TOKEN_ANDEQ,
    TOKEN_SHLEQ,
    TOKEN_SHREQ,
    TOKEN_OREQ,
    TOKEN_LAST_EQ = TOKEN_OREQ,

    TOKEN_NEG,
    TOKEN_NOT,

    TOKEN_INC,
    TOKEN_DEC,

    TOKEN_COUNT,
};

enum
{
    MOD_HEX = 1,
    MOD_OCT,
    MOD_BIN,
    MOD_CHR,
};

struct token
{
    int kind;
    int modifier;
    char* start;
    char* end;

    union
    {
        uint64_t u64;
        double f64;
        char* name;
        char* string;
    }
    u;
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

char* lkinds[TOKEN_COUNT] =
{
    [0] = "EOF",

    [TOKEN_INT] = "int",
    [TOKEN_FLOAT] = "float",
    [TOKEN_NAME] = "name",
    [TOKEN_STRING] = "string",

    [TOKEN_OROR] = "||",
    [TOKEN_ANDAND] = "&&",

    [TOKEN_EQEQ] = "==",
    [TOKEN_NE] = "!=",
    [TOKEN_BE] = "<=",
    [TOKEN_GE] = ">=",
    [TOKEN_GT] = "<",
    [TOKEN_LT] = ">",

    [TOKEN_ADD] = "+",
    [TOKEN_SUB] = "-",
    [TOKEN_OR] = "|",
    [TOKEN_XOR] = "^",

    [TOKEN_MUL] = "*",
    [TOKEN_DIV] = "/",
    [TOKEN_AND] = "&",
    [TOKEN_MOD] = "%",
    [TOKEN_SHL] = "<<",
    [TOKEN_SHR] = ">>",

    [TOKEN_EQ] = "=",
    [TOKEN_ADDEQ] = "+=",
    [TOKEN_SUBEQ] = "-=",
    [TOKEN_MULEQ] = "*=",
    [TOKEN_DIVEQ] = "/=",
    [TOKEN_MODEQ] = "%=",
    [TOKEN_XOREQ] = "^=",
    [TOKEN_ANDEQ] = "&=",
    [TOKEN_SHLEQ] = "<<=",
    [TOKEN_SHREQ] = ">>=",
    [TOKEN_OREQ] = "|=",

    [TOKEN_NEG] = "!",
    [TOKEN_NOT] = "~",

    [TOKEN_INC] = "++",
    [TOKEN_DEC] = "--",
};

char* lkindstr(int kind, char* buf)
{
    buf = buf ? buf : xmalloc(4096);

    if (kind <= TOKEN_LAST_LITERAL)
    {
        sprintf(buf, "'%c' '\\x%02X'", (char)kind, kind);
    }

    else if (kind >= TOKEN_COUNT)
    {
        sprintf(buf, "unknown token %d", kind);
    }

    else
    {
        sprintf(buf, lkinds[kind]);
    }

    return buf;
}

char* ldescribe(token_t* tok, char* buf)
{
    char *p;

    if (!buf) {
        buf = (char*)xmalloc(4096);
    }

    p = buf;
    lkindstr(tok->kind, p);
    p += strlen(p);

    switch (tok->kind)
    {
    case TOKEN_INT:
        sprintf(p, ": %lu", ltok.u.u64);
        break;

    case TOKEN_FLOAT:
        sprintf(p, ": %.17f", ltok.u.f64);
        break;

    case TOKEN_NAME:
        sprintf(p, ": %s (%p)", ltok.u.name, ltok.u.name);
        break;

    case TOKEN_STRING:
        sprintf(p, ": %s", ltok.u.string);
        break;
    }

    return buf;
}

uint8_t char_to_digit[255] =
{
    ['0'] = 0,
    ['1'] = 1,
    ['2'] = 2,
    ['3'] = 3,
    ['4'] = 4,
    ['5'] = 5,
    ['6'] = 6,
    ['7'] = 7,
    ['8'] = 8,
    ['9'] = 9,
    ['a'] = 10, ['A'] = 10,
    ['b'] = 11, ['B'] = 11,
    ['c'] = 12, ['C'] = 12,
    ['d'] = 13, ['D'] = 13,
    ['e'] = 14, ['E'] = 14,
    ['f'] = 15, ['F'] = 15,
};

void linteger()
{
    int base = 10;
    uint64_t val;
    int mod = 0;

    val = 0;

    if (*ldata == '0')
    {
        ++ldata;

        if (tolower(*ldata) == 'x')
        {
            base = 16;
            mod = MOD_HEX;
            ++ldata;
        }

        else if (isdigit(*ldata))
        {
            base = 8;
            mod = MOD_HEX;
        }

        else if (tolower(*ldata) == 'b')
        {
            base = 2;
            mod = MOD_BIN;
            ++ldata;
        }
    }

    syntax_assert(base == 10 || isxdigit(*ldata),
        "integer prefix with no value");

    while (1)
    {
        int digit;

        digit = char_to_digit[(uint8_t)*ldata];

        if (!digit && *ldata != '0') {
            break;
        }

        ++ldata;

        if (digit >= base)
        {
            syntax_errorf("integer literal digit is out of range for "
                "base %d", base);
            break;
        }

        if (val > (UINT64_MAX - digit) / base)
        {
            syntax_error("integer literal overflows uint64");
            for (; isxdigit(*ldata); ++ldata);
            break;
        }

        val *= base;
        val += digit;
    }

    ltok.kind = TOKEN_INT;
    ltok.modifier = mod;
    ltok.u.u64 = val;
}

void lfloat()
{
    char* start = ldata;

    for (; isdigit(*ldata); ++ldata);

    if (*ldata == '.') {
        ++ldata;
    }

    for (; isdigit(*ldata); ++ldata);

    if (tolower(*ldata) == 'e')
    {
        ++ldata;

        if (*ldata == '-' || *ldata == '+') {
            ++ldata;
        }

        syntax_assert(isdigit(*ldata), "float literal missing exponent");

        for (; isdigit(*ldata); ++ldata);
    }

    ltok.kind = TOKEN_FLOAT;
    ltok.u.f64 = strtod(start, 0);
}

void lexpect(char c)
{
    syntax_assertf(*ldata == c, "expected '%c', got '%c'", c, *ldata);
    ++ldata;
}

char escape_to_char[255] =
{
    ['n'] = '\n',
    ['r'] = '\r',
    ['t'] = '\t',
    ['v'] = '\v',
    ['b'] = '\b',
    ['a'] = '\a',
    ['\\'] = '\\',
    ['\''] = '\'',
};

char lchar()
{
    char val;

    syntax_assert(*ldata != '\n', "string or char literal cannot "
        "contain newline");

    if (*ldata == '\\')
    {
        ++ldata;
        val = escape_to_char[(int)*ldata];

        syntax_assertf(val || *ldata == '0', "invalid escape sequence %c",
            *ldata);

        ++ldata;
    }

    else {
        val = *ldata++;
    }

    return val;
}

void lchar_literal()
{
    char val;

    lexpect('\'');
    syntax_assert(*ldata != '\'', "empty char literal");
    val = lchar();
    lexpect('\'');

    ltok.kind = TOKEN_INT;
    ltok.modifier = MOD_CHR;
    ltok.u.u64 = val;
}

void lstring_literal()
{
    char* str = 0;

    lexpect('"');

    while (*ldata && *ldata != '"')
    {
        char c;

        c = lchar();
        if (!c) {
            break;
        }

        bpush(str, c);
    }

    bpush(str, 0);

    lexpect('"');

    ltok.kind = TOKEN_STRING;
    ltok.u.string = str;
}

void lnext()
{
    for (; isspace(*ldata); ++ldata);

    ltok.modifier = 0;
    ltok.start = ldata;

    switch (*ldata)
    {
    case '.':
        lfloat();
        break;

    case '\'':
        lchar_literal();
        break;

    case '"':
        lstring_literal();
        break;

    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
        for (; isdigit(*ldata); ++ldata);

        if (*ldata == '.' || tolower(*ldata) == 'e')
        {
            ldata = ltok.start;
            lfloat();
        }

        else
        {
            ldata = ltok.start;
            linteger();
        }
        break;

    case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g':
    case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n':
    case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u':
    case 'v': case 'w': case 'x': case 'y': case 'z': case 'A': case 'B':
    case 'C': case 'D': case 'E': case 'F': case 'G': case 'H': case 'I':
    case 'J': case 'K': case 'L': case 'M': case 'N': case 'O': case 'P':
    case 'Q': case 'R': case 'S': case 'T': case 'U': case 'V': case 'W':
    case 'X': case 'Y': case 'Z': case '_':
        ltok.kind = TOKEN_NAME;

        while (isalpha(*ldata) || isdigit(*ldata) || *ldata == '_') {
            ++ldata;
        }

        ltok.u.name = istr_r(ltok.start, ldata);
        break;

    case '~':
        ltok.kind = TOKEN_NOT;
        ++ldata;
        break;

#define op1(c, t, c1, t1) \
    case c: \
        ltok.kind = t; \
        ++ldata; \
        \
        if (*ldata == c1) \
        { \
            ltok.kind = t1; \
            ++ldata; \
        } \
        break;

#define op2(c, t, c1, t1, c2, t2) \
    case c: \
        ltok.kind = t; \
        ++ldata; \
        \
        if (*ldata == c1) \
        { \
            ltok.kind = t1; \
            ++ldata; \
        } \
        \
        else if (*ldata == c2) \
        { \
            ltok.kind = t2; \
            ++ldata; \
        } \
        break;

#define shlr(c, t, c1, t1, t1eq, c2, t2) \
    case c: \
        ltok.kind = t; \
        ++ldata; \
        \
        if (*ldata == c1) \
        { \
            ltok.kind = t1; \
            ++ldata; \
            \
            if (*ldata == '=') \
            { \
                ltok.kind = t1eq; \
                ++ldata; \
            } \
        } \
        \
        else if (*ldata == c2) \
        { \
            ltok.kind = t2; \
            ++ldata; \
        } \
        break;

    op1('=', TOKEN_EQ,  '=', TOKEN_EQEQ)
    op1('!', TOKEN_NEG, '=', TOKEN_NE)
    op2('+', TOKEN_ADD, '=', TOKEN_ADDEQ, '+', TOKEN_INC)
    op2('-', TOKEN_SUB, '=', TOKEN_SUBEQ, '-', TOKEN_DEC)
    op1('*', TOKEN_MUL, '=', TOKEN_MULEQ)
    op1('/', TOKEN_DIV, '=', TOKEN_DIVEQ)
    op1('%', TOKEN_MOD, '=', TOKEN_MODEQ)
    op1('^', TOKEN_XOR, '=', TOKEN_XOREQ)
    op2('|', TOKEN_OR,  '=', TOKEN_OREQ,  '|', TOKEN_OROR)
    op2('&', TOKEN_AND, '=', TOKEN_ANDEQ, '&', TOKEN_ANDAND)
    shlr('<', TOKEN_LT, '<', TOKEN_SHL, TOKEN_SHLEQ, '=', TOKEN_BE)
    shlr('>', TOKEN_GT, '>', TOKEN_SHR, TOKEN_SHREQ, '=', TOKEN_GE)

#undef shlr
#undef op2
#undef op1

    default:
        ltok.kind = *ldata++;
    }

    ltok.end = ldata;
}

#define lassert_name(s) \
    assertf(ltok.kind == TOKEN_NAME && ltok.u.name == istr(s), \
        "unexpected token. got %s, expected NAME: %s (%p)", \
        ldescribe(&ltok, 0), s, s); \
    lnext()

#define lassert_str(s) \
    assertf(ltok.kind == TOKEN_STRING && !strcmp(ltok.u.string, s), \
        "unexpected token. got %s, expected STRING: %s (%p)", \
        ldescribe(&ltok, 0), s, s); \
    lnext()

#define lassert_int(i) \
    assertf(ltok.kind == TOKEN_INT && ltok.u.u64 == i, \
        "unexpected token. got %s, expected INT: %lu", \
        ldescribe(&ltok, 0), (uint64_t)i); \
    lnext()

#define lassert_tok(c) \
    assertf(ltok.kind == c, \
        "unexpected token. got %s, expected %s", \
        ldescribe(&ltok, 0), lkindstr(c, 0)); \
    lnext()

void test_linit(char* src)
{
    logf("input: %s", src);
    linit(src);
}

void test_lex()
{
    test_linit("! ~ + ++ - -- * *= / /= % %= & &= && | |= || ^ ^= != == "
        "+= -= <<= >>= < << <= > >> >=");
    lassert_tok(TOKEN_NEG);
    lassert_tok(TOKEN_NOT);
    lassert_tok(TOKEN_ADD);
    lassert_tok(TOKEN_INC);
    lassert_tok(TOKEN_SUB);
    lassert_tok(TOKEN_DEC);
    lassert_tok(TOKEN_MUL);
    lassert_tok(TOKEN_MULEQ);
    lassert_tok(TOKEN_DIV);
    lassert_tok(TOKEN_DIVEQ);
    lassert_tok(TOKEN_MOD);
    lassert_tok(TOKEN_MODEQ);
    lassert_tok(TOKEN_AND);
    lassert_tok(TOKEN_ANDEQ);
    lassert_tok(TOKEN_ANDAND);
    lassert_tok(TOKEN_OR);
    lassert_tok(TOKEN_OREQ);
    lassert_tok(TOKEN_OROR);
    lassert_tok(TOKEN_XOR);
    lassert_tok(TOKEN_XOREQ);
    lassert_tok(TOKEN_NE);
    lassert_tok(TOKEN_EQEQ);
    lassert_tok(TOKEN_ADDEQ);
    lassert_tok(TOKEN_SUBEQ);
    lassert_tok(TOKEN_SHLEQ);
    lassert_tok(TOKEN_SHREQ);
    lassert_tok(TOKEN_LT);
    lassert_tok(TOKEN_SHL);
    lassert_tok(TOKEN_BE);
    lassert_tok(TOKEN_GT);
    lassert_tok(TOKEN_SHR);
    lassert_tok(TOKEN_GE);
    lassert_tok(0);

#define i(x) \
    test_linit(#x); \
    lassert_int(x##llu); \
    lassert_tok(0);

    i(0)
    i(1234567890)
    i(18446744073709551615)

    i(0x0)
    i(0x1234567890ABCDEF)
    i(0xFFFFFFFFFFFFFFFF)

    i(00)
    i(0123456701234)
    i(01777777777777777777777)

#undef i

    test_linit("0b0");
    lassert_int(0);
    lassert_tok(0);

    test_linit("0b10111010110111011100101011111110"
                 "10111010101011011111000000001101");
    lassert_int(0xBADDCAFEBAADF00D);
    lassert_tok(0);

    test_linit("0b11111111111111111111111111111111"
                 "11111111111111111111111111111111");
    lassert_int(0xFFFFFFFFFFFFFFFF);
    lassert_tok(0);

    test_linit("abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ_"
        "0123456789 blah");
    lassert_name("abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ_"
        "0123456789");
    lassert_name("blah");
    lassert_tok(0);

    test_linit("\"hello\\nworld\\nthis is a test 123 321\" 123");
    lassert_str("hello\nworld\nthis is a test 123 321");
    lassert_int(123);
    lassert_tok(0);

    log("(passed)");
}

/* --------------------------------------------------------------------- */

#define CHUNK_SIZE (1024 * 1024)
#define CHUNK_ALIGN 8

struct memchunks
{
    char** chunks;
    char* p;
    char* end;
};

typedef struct memchunks memchunks_t;

void memchunks_grow(memchunks_t* m, int min_size)
{
    char* new_chunk;
    int size = round_up(min_size, CHUNK_SIZE);

    new_chunk = xmalloc(size);
    bpush(m->chunks, new_chunk);
    m->p = new_chunk;
    m->end = new_chunk + size;
}

void* memchunks_alloc(memchunks_t* m, int n)
{
    void* res;

    if (m->p + n >= m->end)
    {
        memchunks_grow(m, n);
        assert(m->end - m->p >= n);
    }

    res = m->p;
    m->p += n;
    m->p = roundptr_up(m->p, CHUNK_ALIGN);
    assert(m->p == roundptr_down(m->p, CHUNK_ALIGN));

    return res;
}

/* --------------------------------------------------------------------- */

memchunks_t ast_allocator;

void* ast_alloc(int size)
{
    return memchunks_alloc(&ast_allocator, size);
}

#define ast_dup(x, n) ast_dup_n(x, n * sizeof(*x))

void* ast_dup_n(void* p, int n)
{
    void* res;

    if (n <= 0) {
        return 0;
    }

    res = ast_alloc(n);
    memcpy(res, p, n);
    return res;
}

/* --------------------------------------------------------------------- */

struct typespec;
typedef struct typespec typespec_t;

enum
{
    TYPE_NONE,
    TYPE_NAME,
    TYPE_ARRAY,
    TYPE_PTR,
};

struct typespec
{
    int kind;

    union
    {
        char* name;

        struct
        {
            typespec_t* element_type;
            int len;
        }
        array;

        typespec_t* pointed;
    }
    u;
};

typespec_t* typespec(int kind)
{
    typespec_t* res;

    res = (typespec_t*)ast_alloc(sizeof(typespec_t));
    res->kind = kind;

    return res;
}

typespec_t* typespec_name(char* name)
{
    typespec_t* res;

    res = typespec(TYPE_NAME);
    res->u.name = name;

    return res;
}

typespec_t* typespec_array(typespec_t* element_type, int len)
{
    typespec_t* res;

    res = typespec(TYPE_ARRAY);
    res->u.array.element_type = element_type;
    res->u.array.len = len;

    return res;
}

typespec_t* typespec_ptr(typespec_t* pointed)
{
    typespec_t* res;

    res = typespec(TYPE_PTR);
    res->u.pointed = pointed;

    return res;
}

/* --------------------------------------------------------------------- */

struct expr;
struct stmt;

typedef struct expr expr_t;
typedef struct stmt stmt_t;

struct stmt_block
{
    stmt_t** stmts;
    int nstmts;
};

typedef struct stmt_block stmt_block_t;

enum
{
    DECL_NONE,
    DECL_VAR,
    DECL_STRUCT,
    DECL_UNION,
    DECL_ENUM,
    DECL_FUNC,
};

struct enum_item
{
    char* name;
    expr_t* value;
};

struct aggregate_field
{
    char* name;
    typespec_t* type;
};

struct func_param
{
    char* name;
    typespec_t* type;
    expr_t* default_val;
};

typedef struct enum_item enum_item_t;
typedef struct aggregate_field aggregate_field_t;
typedef struct func_param func_param_t;

struct decl
{
    int kind;
    char* name;

    union
    {
        struct
        {
            expr_t* expr;
            typespec_t* type;
        }
        var;

        struct
        {
            aggregate_field_t* fields;
            int nfields;
        }
        aggregate;

        struct
        {
            enum_item_t* items;
            int nitems;
        }
        enum_;

        struct
        {
            typespec_t* ret_type;
            func_param_t* params;
            int nparams;
            stmt_block_t body;
        }
        func;
    }
    u;
};

typedef struct decl decl_t;

decl_t* decl(int kind, char* name)
{
    decl_t* res;

    res = (decl_t*)ast_alloc(sizeof(decl_t));
    res->kind = kind;
    res->name = name;

    return res;
}

decl_t* decl_var(char* name, expr_t* expr, typespec_t* type)
{
    decl_t* res;

    res = decl(DECL_VAR, name);
    res->u.var.expr = expr;
    res->u.var.type = type;

    return res;
}

decl_t* decl_aggregate(int kind, char* name, aggregate_field_t* fields,
    int nfields)
{
    decl_t* res;

    assert(kind == DECL_STRUCT || kind == DECL_UNION);
    res = decl(kind, name);
    res->u.aggregate.fields = ast_dup(fields, nfields);
    res->u.aggregate.nfields = nfields;

    return res;
}

decl_t* decl_enum(enum_item_t* items, int nitems)
{
    decl_t* res;

    res = decl(DECL_ENUM, 0);
    res->u.enum_.items = ast_dup(items, nitems);
    res->u.enum_.nitems = nitems;

    return res;
}

decl_t* decl_func(char* name, typespec_t* ret_type, func_param_t* params,
    int nparams, stmt_block_t body)
{
    decl_t* res;

    res = decl(DECL_FUNC, name);
    res->u.func.ret_type = ret_type;
    res->u.func.params = ast_dup(params, nparams);
    res->u.func.nparams = nparams;
    res->u.func.body = body;

    return res;
}

/* --------------------------------------------------------------------- */

enum
{
    EXPR_NONE,
    EXPR_INT,
    EXPR_FLOAT,
    EXPR_STR,
    EXPR_NAME,
    EXPR_TERNARY,
    EXPR_BINARY,
    EXPR_UNARY,
    EXPR_CALL,
    EXPR_AGGREGATE,
    EXPR_TOINT,
    EXPR_TOFLOAT,
    EXPR_SIZEOF,
    EXPR_OFFSETOF,
    EXPR_PUSH8,
    EXPR_PUSH16,
    EXPR_PUSH32,
    EXPR_PUSH64,
};

struct aggregate_init_item
{
    char* name;
    expr_t* value;
};

typedef struct aggregate_init_item aggregate_init_item_t;

struct expr
{
    int kind;

    union
    {
        uint64_t u64;
        double f64;
        char* str;

        struct
        {
            expr_t* cond;
            expr_t* then;
            expr_t* else_;
        }
        ternary;

        struct
        {
            int operator;
            expr_t* left;
            expr_t* right;
        }
        binary;

        struct
        {
            int operator;
            expr_t* operand;
        }
        unary;

        struct
        {
            expr_t* function;
            expr_t** params;
            int nparams;
        }
        call;

        struct
        {
            aggregate_init_item_t* items;
            int nitems;
        }
        aggregate;

        expr_t* expr;
        typespec_t* sizeof_type;

        struct
        {
            typespec_t* type;
            char* field;
        }
        offsetof_;

        struct
        {
            expr_t* dst;
            expr_t* src;
        }
        push;
    }
    u;
};

expr_t* expr(int kind)
{
    expr_t* res;

    res = (expr_t*)ast_alloc(sizeof(expr_t));
    res->kind = kind;

    return res;
}

expr_t* expr_int(uint64_t u64)
{
    expr_t* res;

    res = expr(EXPR_INT);
    res->u.u64 = u64;

    return res;
}

expr_t* expr_float(double f64)
{
    expr_t* res;

    res = expr(EXPR_FLOAT);
    res->u.f64 = f64;

    return res;
}

expr_t* expr_str(char* str)
{
    expr_t* res;

    res = expr(EXPR_STR);
    res->u.str = str;

    return res;
}

expr_t* expr_name(char* name)
{
    expr_t* res;

    res = expr(EXPR_NAME);
    res->u.str = name;

    return res;
}

expr_t* expr_ternary(expr_t* cond, expr_t* then, expr_t* else_)
{
    expr_t* res;

    res = expr(EXPR_TERNARY);
    res->u.ternary.cond = cond;
    res->u.ternary.then = then;
    res->u.ternary.else_ = else_;

    return res;
}

expr_t* expr_binary(int operator, expr_t* left, expr_t* right)
{
    expr_t* res;

    res = expr(EXPR_BINARY);
    res->u.binary.operator = operator;
    res->u.binary.left = left;
    res->u.binary.right = right;

    return res;
}

expr_t* expr_unary(int operator, expr_t* operand)
{
    expr_t* res;

    res = expr(EXPR_UNARY);
    res->u.unary.operator = operator;
    res->u.unary.operand = operand;

    return res;
}

expr_t* expr_call(expr_t* function, expr_t** params, int nparams)
{
    expr_t* res;

    res = expr(EXPR_CALL);
    res->u.call.function = function;
    res->u.call.params = ast_dup(params, nparams);
    res->u.call.nparams = nparams;

    return res;
}

expr_t* expr_aggregate(aggregate_init_item_t* items, int nitems)
{
    expr_t* res;

    res = expr(EXPR_AGGREGATE);
    res->u.aggregate.items = ast_dup(items, nitems);
    res->u.aggregate.nitems = nitems;

    return res;
}

expr_t* expr_toint(expr_t* exp)
{
    expr_t* res;

    res = expr(EXPR_TOINT);
    res->u.expr = exp;

    return res;
}

expr_t* expr_tofloat(expr_t* exp)
{
    expr_t* res;

    res = expr(EXPR_TOFLOAT);
    res->u.expr = exp;

    return res;
}

expr_t* expr_sizeof(typespec_t* type)
{
    expr_t* res;

    res = expr(EXPR_SIZEOF);
    res->u.sizeof_type = type;

    return res;
}

expr_t* expr_offsetof(typespec_t* type, char* field)
{
    expr_t* res;

    res = expr(EXPR_OFFSETOF);
    res->u.offsetof_.type = type;
    res->u.offsetof_.field = field;

    return res;
}

expr_t* expr_push(int bits, expr_t* src, expr_t* dst)
{
    expr_t* res;
    int kind;

    switch (bits)
    {
    case 8:  kind = EXPR_PUSH8;  break;
    case 16: kind = EXPR_PUSH16; break;
    case 32: kind = EXPR_PUSH32; break;
    case 64: kind = EXPR_PUSH64; break;

    default:
        assertf(0, "cannot push %d bits", bits);
        break;
    }

    res = expr(kind);
    res->u.push.dst = dst;
    res->u.push.src = src;

    return res;
}

/* --------------------------------------------------------------------- */

enum
{
    STMT_NONE,
    STMT_DECL,
    STMT_BREAK,
    STMT_CONTINUE,
    STMT_FALLTHROUGH,
    STMT_EXPR,
    STMT_RETURN,
    STMT_BLOCK,
    STMT_IF,
    STMT_WHILE,
    STMT_FOR,
    STMT_DO,
    STMT_SWITCH,
    STMT_PICK,
};

struct elseif
{
    expr_t* cond;
    stmt_block_t body;
};

struct switch_case
{
    expr_t* exprs;
    int nexprs;
    stmt_block_t body;
};

typedef struct switch_case switch_case_t;
typedef struct elseif elseif_t;

struct stmt
{
    int kind;

    union
    {
        decl_t* decl;
        expr_t* expr;
        stmt_block_t block;

        struct
        {
            expr_t* cond;
            stmt_block_t then;
            elseif_t* elseifs;
            int nelseifs;
            stmt_block_t else_;
        }
        if_;

        struct
        {
            stmt_t* init;
            expr_t* cond;
            stmt_t* iter;
            stmt_block_t body;
        }
        loop;

        struct
        {
            expr_t* expr;
            switch_case_t* cases;
            int ncases;
        }
        switch_;
    }
    u;
};

stmt_block_t stmt_block(stmt_t** stmts, int nstmts)
{
    return (stmt_block_t){ast_dup(stmts, nstmts), nstmts};
}

stmt_t* stmt(int kind)
{
    stmt_t* res;

    res = (stmt_t*)ast_alloc(sizeof(stmt_t));
    res->kind = kind;

    return res;
}

stmt_t* stmt_decl(decl_t* decl)
{
    stmt_t* res;

    res = stmt(STMT_DECL);
    res->u.decl = decl;

    return res;
}

stmt_t* stmt_expr(expr_t* expr)
{
    stmt_t* res;

    res = stmt(STMT_EXPR);
    res->u.expr = expr;

    return res;
}

stmt_t* stmt_return(expr_t* expr)
{
    stmt_t* res;

    res = stmt(STMT_RETURN);
    res->u.expr = expr;

    return res;
}

stmt_t* stmt_if(expr_t* cond, stmt_block_t then, elseif_t* elseifs,
    int nelseifs, stmt_block_t else_)
{
    stmt_t* res;

    res = stmt(STMT_IF);
    res->u.if_.cond = cond;
    res->u.if_.then = then;
    res->u.if_.elseifs = ast_dup(elseifs, nelseifs);
    res->u.if_.nelseifs = nelseifs;
    res->u.if_.else_ = else_;

    return res;
}

stmt_t* stmt_loop(int kind, expr_t* cond, stmt_block_t body)
{
    stmt_t* res;

    assert(kind == STMT_WHILE || kind == STMT_DO);
    res = stmt(kind);
    res->u.loop.cond = cond;
    res->u.loop.body = body;

    return res;
}

stmt_t* stmt_for(stmt_t* init, expr_t* cond, stmt_t* iter,
    stmt_block_t body)
{
    stmt_t* res;

    res = stmt(STMT_FOR);
    res->u.loop.init = init;
    res->u.loop.cond = cond;
    res->u.loop.iter = iter;
    res->u.loop.body = body;

    return res;
}

stmt_t* stmt_switch(int kind, expr_t* expr, switch_case_t* cases,
    int ncases)
{
    stmt_t* res;

    assert(kind == STMT_SWITCH || kind == STMT_PICK);
    res = stmt(kind);
    res->u.switch_.expr = expr;
    res->u.switch_.cases = ast_dup(cases, ncases);
    res->u.switch_.ncases = ncases;

    return res;
}

/* --------------------------------------------------------------------- */

void print_indent(int indent)
{
    for (; indent; --indent) {
        printf("    ");
    }
}

void print_typespec(typespec_t* type)
{
    switch (type->kind)
    {
    case TYPE_NAME:
        printf(type->u.name);
        break;

    case TYPE_ARRAY:
        printf("(array ");
        print_typespec(type->u.array.element_type);
        printf(" %d)", type->u.array.len);
        break;

    case TYPE_PTR:
        printf("(ptr ");
        print_typespec(type->u.pointed);
        printf(")");
        break;
    }
}

void print_expr(expr_t* expr, int indent);
void print_stmt_block(stmt_block_t block, int indent);

void print_decl(decl_t* decl, int indent)
{
    int i;

    print_indent(indent);

    switch (decl->kind)
    {
    case DECL_VAR:
        printf("(");
        print_typespec(decl->u.var.type);
        printf(" %s ", decl->name);

        if (decl->u.var.expr) {
            print_expr(decl->u.var.expr, indent);
        } else {
            printf("nil");
        }

        printf(")");
        break;

    case DECL_STRUCT:
    case DECL_UNION:
        if (decl->kind == DECL_STRUCT) {
            printf("(struct %s", decl->name);
        } else {
            printf("(union %s", decl->name);
        }

        for (i = 0; i < decl->u.aggregate.nfields; ++i)
        {
            aggregate_field_t* it = &decl->u.aggregate.fields[i];

            printf("\n");
            print_indent(indent + 1);
            printf("(");
            print_typespec(it->type);
            printf(" %s)", it->name);
        }

        printf(")");
        break;

    case DECL_ENUM:
        printf("(enum ");

        for (i = 0; i < decl->u.enum_.nitems; ++i)
        {
            enum_item_t* it = &decl->u.enum_.items[i];

            printf("\n");
            print_indent(indent + 1);
            printf("(%s ", it->name);
            print_expr(it->value, indent);
            printf(")");
        }

        printf(")");
        break;

    case DECL_FUNC:
        printf("(func (");
        print_typespec(decl->u.func.ret_type);
        printf(" %s) (", decl->name);

        for (i = 0; i < decl->u.func.nparams; ++i)
        {
            func_param_t* it = &decl->u.func.params[i];

            if (i) {
                printf(" ");
            }

            printf("(");
            print_typespec(it->type);
            printf(" %s ", it->name);

            if (it->default_val) {
                print_expr(it->default_val, indent);
            } else {
                printf("nil");
            }

            printf(")");
        }

        printf(")\n");

        print_stmt_block(decl->u.func.body, indent + 1);
        printf(")");
        break;

    default:
        assertf(0, "unknown decl kind %d", decl->kind);
    }
}

void print_operator(int op)
{
    char* s;

    if (op < TOKEN_LAST_LITERAL)
    {
        printf("%c", op);
        return;
    }

    switch (op)
    {
    case TOKEN_INC:    s = "++";  break;
    case TOKEN_DEC:    s = "--";  break;
    case TOKEN_ADDEQ:  s = "+=";  break;
    case TOKEN_SUBEQ:  s = "-=";  break;
    case TOKEN_MULEQ:  s = "*=";  break;
    case TOKEN_DIVEQ:  s = "/=";  break;
    case TOKEN_MODEQ:  s = "%=";  break;
    case TOKEN_XOREQ:  s = "^=";  break;
    case TOKEN_OREQ:   s = "|=";  break;
    case TOKEN_OROR:   s = "||";  break;
    case TOKEN_ANDEQ:  s = "&=";  break;
    case TOKEN_ANDAND: s = "&&";  break;
    case TOKEN_SHL:    s = "<<";  break;
    case TOKEN_SHLEQ:  s = "<<="; break;
    case TOKEN_BE:     s = "<=";  break;
    case TOKEN_SHR:    s = ">>";  break;
    case TOKEN_SHREQ:  s = ">>="; break;
    case TOKEN_GE:     s = ">=";  break;
    }

    printf(s);
}

void print_expr(expr_t* expr, int indent)
{
    int i;

    switch (expr->kind)
    {
    case EXPR_INT:
        printf("%lu", expr->u.u64);
        break;

    case EXPR_FLOAT:
        printf("%f", expr->u.f64);
        break;

    case EXPR_STR:
        printf("\"%s\"", expr->u.str);
        break;

    case EXPR_NAME:
        printf("%s", expr->u.str);
        break;

    case EXPR_BINARY:
        printf("(");
        print_operator(expr->u.binary.operator);
        printf(" ");
        print_expr(expr->u.binary.left, indent);
        printf(" ");
        print_expr(expr->u.binary.right, indent);
        printf(")");
        break;

    case EXPR_UNARY:
        printf("(");
        print_operator(expr->u.unary.operator);
        printf(" ");
        print_expr(expr->u.unary.operand, indent);
        printf(")");
        break;

    case EXPR_CALL:
        printf("(");
        print_expr(expr->u.call.function, indent);

        for (i = 0; i < expr->u.call.nparams; ++i)
        {
            printf(" ");
            print_expr(expr->u.call.params[i], indent);
        }

        printf(")");
        break;

    case EXPR_AGGREGATE:
        printf("(aggregate\n");

        for (i = 0; i < expr->u.aggregate.nitems; ++i)
        {
            aggregate_init_item_t* it = &expr->u.aggregate.items[i];

            printf("\n");
            print_indent(indent + 1);
            printf("(%s ", it->name ? it->name : "nil");
            print_expr(it->value, indent);
            printf(")");
        }
        break;

    case EXPR_TOINT:
        printf("(int ");
        print_expr(expr->u.expr, indent);
        printf(")");
        break;

    case EXPR_TOFLOAT:
        printf("(float ");
        print_expr(expr->u.expr, indent);
        printf(")");
        break;

    case EXPR_SIZEOF:
        printf("(sizeof ");
        print_typespec(expr->u.sizeof_type);
        printf(")");
        break;

    case EXPR_OFFSETOF:
        printf("(offsetof ");
        print_typespec(expr->u.offsetof_.type);
        printf(" %s)", expr->u.offsetof_.field);
        break;

    case EXPR_PUSH8:
    case EXPR_PUSH16:
    case EXPR_PUSH32:
    case EXPR_PUSH64:
        printf("(push%d ", 2 << (expr->kind - EXPR_PUSH8 + 3));
        print_expr(expr->u.push.dst, indent);
        printf(" ");
        print_expr(expr->u.push.src, indent);
        printf(")");
        break;

    default:
        assertf(0, "unknown expr kind %d", expr->kind);
    }
}

void print_stmt(stmt_t* stmt, int indent)
{
    int i, j;

    print_indent(indent);

    switch (stmt->kind)
    {
    case STMT_DECL:
        print_decl(stmt->u.decl, indent);
        break;

    case STMT_BREAK:
        printf("(break)");
        break;

    case STMT_CONTINUE:
        printf("(continue)");
        break;

    case STMT_FALLTHROUGH:
        printf("(fallthrough)");
        break;

    case STMT_EXPR:
        print_expr(stmt->u.expr, indent);
        break;

    case STMT_RETURN:
        printf("(return ");

        if (stmt->u.expr) {
            print_expr(stmt->u.expr, indent);
        } else {
            printf("nil");
        }

        printf(")");
        break;

    case STMT_BLOCK:
        printf("(do\n");
        print_stmt_block(stmt->u.block, indent + 1);
        printf(")");
        break;

    case STMT_IF:
        printf("(if ");
        print_expr(stmt->u.if_.cond, indent);
        printf("\n");
        print_indent(indent);

        printf("(then\n");
        print_stmt_block(stmt->u.if_.then, indent + 1);
        printf(")");

        for (i = 0; i < stmt->u.if_.nelseifs; ++i)
        {
            elseif_t* it = &stmt->u.if_.elseifs[i];

            printf("(elsif ");
            print_expr(it->cond, indent);
            printf("\n");
            print_indent(indent);
            printf("(then\n");
            print_stmt_block(it->body, indent + 1);
            printf("))");
        }

        if (stmt->u.if_.else_.nstmts)
        {
            printf("(else\n");
            print_stmt_block(stmt->u.if_.else_, indent + 1);
            printf(")");
        }

        printf(")");
        break;

    case STMT_WHILE:
        printf("(while ");
        print_expr(stmt->u.loop.cond, indent);
        printf("\n");
        printf("(do\n");
        print_stmt_block(stmt->u.loop.body, indent + 1);
        printf("))");
        break;

    case STMT_DO:
        printf("(loop\n");
        printf("(do\n");
        print_stmt_block(stmt->u.loop.body, indent + 1);
        printf(")\n");
        printf("(while ");
        print_expr(stmt->u.loop.cond, indent);
        printf("))");
        break;

    case STMT_FOR:
        printf("(for ");
        print_stmt(stmt->u.loop.init, indent);
        printf(" ");
        print_expr(stmt->u.loop.cond, indent);
        printf(" ");
        print_stmt(stmt->u.loop.iter, indent);
        printf("\n");
        printf("(do\n");
        print_stmt_block(stmt->u.loop.body, indent + 1);
        printf("))");
        break;

    case STMT_SWITCH:
    case STMT_PICK:
        if (stmt->kind == STMT_SWITCH) {
            printf("(switch ");
        } else {
            printf("(pick ");
        }

        print_expr(stmt->u.switch_.expr, indent);
        printf("\n");

        for (i = 0; i < stmt->u.switch_.ncases; ++i)
        {
            switch_case_t* it = &stmt->u.switch_.cases[i];

            printf("(case (");

            for (j = 0; j < it->nexprs; ++j) {
                print_expr(&it->exprs[j], indent);
            }

            printf(")\n");
            printf("(do\n");
            print_stmt_block(it->body, indent + 1);
            printf("))\n");
        }
        break;

    default:
        assertf(0, "unknown stmt kind %d", stmt->kind);
    }
}

void print_stmts(stmt_t** stmts, int nstmts, int indent)
{
    int i;

    for (i = 0; i < nstmts; ++i)
    {
        if (i) {
            printf("\n");
            print_indent(indent);
        }

        print_stmt(stmts[i], indent);
    }
}

void print_stmt_block(stmt_block_t block, int indent)
{
    print_stmts(block.stmts, block.nstmts, indent);
}

/* --------------------------------------------------------------------- */

int pmatch(int token)
{
    if (ltok.kind == token)
    {
        lnext();
        return 1;
    }

    return 0;
}

int prange(int first, int last)
{
    return ltok.kind >= first && ltok.kind < last;
}

void pexpect(int kind)
{
    syntax_assertf(pmatch(kind), "unexpected token. got %s, "
        "expected %s", ldescribe(&ltok, 0), lkindstr(kind, 0));
}

expr_t* pexpr_add()
{
    return 0;
}

/* expr_add (("==" | ">=" | "<=" | '<' | '>') expr_add)* */
expr_t* pexpr_cmp()
{
    expr_t* res;

    res = pexpr_add();

    while (prange(TOKEN_FIRST_CMP, TOKEN_LAST_CMP))
    {
        int operator;

        operator = ltok.kind;
        lnext();
        res = expr_binary(operator, res, pexpr_add());
    }

    return res;
}

/* expr_cmp ("&&" expr_cmp)* */
expr_t* pexpr_andand()
{
    expr_t* res;

    res = pexpr_cmp();

    while (pmatch(TOKEN_ANDAND))
    {
        res = expr_binary(TOKEN_ANDAND, res, pexpr_cmp());
    }

    return res;
}

/* expr_andand ("||" expr_andand)* */
expr_t* pexpr_oror()
{
    expr_t* res;

    res = pexpr_andand();

    while (pmatch(TOKEN_OROR))
    {
        res = expr_binary(TOKEN_OROR, res, pexpr_andand());
    }

    return res;
}

/* expr_oror ('?' expr_ternary ':' expr_ternary)? */
expr_t* pexpr_ternary()
{
    expr_t* res;

    res = pexpr_oror();

    if (pmatch('?'))
    {
        expr_t* then;
        expr_t* else_;

        then = pexpr_ternary();
        pexpect(':');
        else_ = pexpr_ternary();

        return expr_ternary(res, then, else_);
    }

    return res;
}

expr_t* pexpr()
{
    return pexpr_ternary();
}

void test_p()
{

}

/* --------------------------------------------------------------------- */

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

int main()
{
    tests();

    return 0;
}
