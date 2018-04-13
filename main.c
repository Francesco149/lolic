/*
 * this is my little compiler/parser playground where I experiment while
 * prototyping my future programming language. once I feel like I'm ready
 * I will likely rewrite the compiler from scratch properly, unless I
 * grow attached to this code base for some reason
 *
 * this is heavily inspired by bitwise, which taught me a lot of parsing
 * tricks https://bitwise.handmade.network/
 *
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

#define bpush(x, ...) \
    bfit(x, 1), \
    (x)[blen(x)] = (__VA_ARGS__), \
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

memchunks_t intern_allocator;

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

    new.str = memchunks_alloc(&intern_allocator, len + 1);
    memcpy(new.str, start, len);
    new.str[len] = 0;
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
}

/* --------------------------------------------------------------------- */

#define syntax_errorf(fmt, ...) fprintf(stderr, fmt "\n", __VA_ARGS__)
#define syntax_error(fmt) fprintf(stderr, fmt "\n")
#define syntax_assert(cond, msg) ((cond) ? 0 : syntax_error(msg))
#define syntax_assertf(cond, ...) \
    ((cond) ? 0 :  syntax_errorf(__VA_ARGS__))

enum
{
    TOKEN_LAST_LITERAL = 127,

    TOKEN_INT,
    TOKEN_FLOAT,
    TOKEN_NAME,
    TOKEN_STRING,
    TOKEN_KWORD,

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

char** kwords;
char* first_kword;
char* last_kword;

#define init_kword(kword) init_kword_(&kword_##kword, #kword)
char* init_kword_(char** pkword, char* str)
{
    *pkword = istr(str);
    bpush(kwords, *pkword);
    return *pkword;
}

int is_kword(char* str)
{
    return str >= first_kword && str <= last_kword;
}

char* kword_toint;
char* kword_tofloat;
char* kword_sizeof;
char* kword_offsetof;
char* kword_push8;
char* kword_push16;
char* kword_push32;
char* kword_push64;

char* kword_if;
char* kword_else;
char* kword_while;
char* kword_for;
char* kword_do;
char* kword_switch;
char* kword_pick;
char* kword_break;
char* kword_continue;
char* kword_fallthrough;
char* kword_return;

char* kword_struct;
char* kword_union;

void linit()
{
    int nblocks;

    nblocks = blen(intern_allocator.chunks);

    if (!nblocks) {
        nblocks = 1;
    }

    init_kword(toint);
    init_kword(tofloat);
    init_kword(sizeof);
    init_kword(offsetof);
    init_kword(push8);
    init_kword(push16);
    init_kword(push32);
    init_kword(push64);

    init_kword(if);
    init_kword(else);
    init_kword(while);
    init_kword(for);
    init_kword(do);
    init_kword(switch);
    init_kword(pick);
    init_kword(break);
    init_kword(continue);
    init_kword(fallthrough);
    init_kword(return);

    init_kword(struct);
    init_kword(union);

    assert(blen(intern_allocator.chunks) == nblocks);

    first_kword = kword_toint;
    last_kword = kword_union;
}

void lnext();

void lreset(char* data)
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
    [TOKEN_KWORD] = "keyword",

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

    if (kind <= TOKEN_LAST_LITERAL) {
        sprintf(buf, "'%c' '\\x%02X'", (char)kind, kind);
    }

    else if (kind >= TOKEN_COUNT) {
        sprintf(buf, "unknown token %d", kind);
    }

    else {
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
    case TOKEN_KWORD:
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
    ['"'] = '"',
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

    while (*ldata && *ldata != '"') {
        bpush(str, lchar());
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
        if (isdigit(ldata[1])) {
            lfloat();
        } else {
            ltok.kind = *ldata++;
        }
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

        if (is_kword(ltok.u.name)) {
            ltok.kind = TOKEN_KWORD;
        }
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

#define lassert_kword(s) \
    assertf(ltok.kind == TOKEN_KWORD && ltok.u.name == istr(s), \
        "unexpected token. got %s, expected KWORD: %s (%p)", \
        ldescribe(&ltok, 0), s, s); \
    lnext()

#define lassert_int(i) \
    assertf(ltok.kind == TOKEN_INT && ltok.u.u64 == i, \
        "unexpected token. got %s, expected INT: %lu", \
        ldescribe(&ltok, 0), (uint64_t)i); \
    lnext()

#define lassert_float(i) \
    assertf(ltok.kind == TOKEN_FLOAT && ltok.u.f64 == i, \
        "unexpected token. got %s, expected FLOAT: %lu", \
        ldescribe(&ltok, 0), (uint64_t)i); \
    lnext()

#define lassert_tok(c) \
    assertf(ltok.kind == c, \
        "unexpected token. got %s, expected %s", \
        ldescribe(&ltok, 0), lkindstr(c, 0)); \
    lnext()

void test_lex()
{
    lreset("! ~ + ++ - -- * *= / /= % %= & &= && | |= || ^ ^= != == "
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
    lreset(#x); \
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

#define f(x) \
    lreset(#x); \
    lassert_float(x); \
    lassert_tok(0);

    f(3.14) f(1e10) f(1e+10) f(1e-10) f(.1e10) f(.1e+10) f(.1e-10)
    f(1.5e10) f(1.5e+10) f(1.5e-10) f(.1) f(1.)

#undef f

    lreset("0b0");
    lassert_int(0);
    lassert_tok(0);

    lreset("0b10111010110111011100101011111110"
                 "10111010101011011111000000001101");
    lassert_int(0xBADDCAFEBAADF00D);
    lassert_tok(0);

    lreset("0b11111111111111111111111111111111"
                 "11111111111111111111111111111111");
    lassert_int(0xFFFFFFFFFFFFFFFF);
    lassert_tok(0);

    lreset("abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ_"
        "0123456789 blah");
    lassert_name("abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ_"
        "0123456789");
    lassert_name("blah");
    lassert_tok(0);

    lreset("\"hello\\nworld\\nthis is a test 123 321 \\\\\\\"\" 123");
    lassert_str("hello\nworld\nthis is a test 123 321 \\\"");
    lassert_int(123);
    lassert_tok(0);

    lreset("toint tofloat sizeof offsetof push8 push16 push32 push64");
    lassert_kword("toint");
    lassert_kword("tofloat");
    lassert_kword("sizeof");
    lassert_kword("offsetof");
    lassert_kword("push8");
    lassert_kword("push16");
    lassert_kword("push32");
    lassert_kword("push64");
}

/* --------------------------------------------------------------------- */

memchunks_t ast_allocator;

void* ast_alloc(int size) {
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

struct expr;
struct decl;
struct typespec;

typedef struct expr expr_t;
typedef struct decl decl_t;
typedef struct typespec typespec_t;

enum
{
    TYPE_NONE,
    TYPE_NAME,
    TYPE_ARRAY,
    TYPE_PTR,
    TYPE_STRUCT,
    TYPE_UNION,
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
            expr_t* len;
        }
        array;

        typespec_t* pointed;

        struct
        {
            char* name;
            decl_t** fields;
            int nfields;
        }
        aggregate;
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

typespec_t* typespec_array(typespec_t* element_type, expr_t* len)
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

typespec_t* typespec_aggregate(int kind, char* name, decl_t** fields,
    int nfields)
{
    typespec_t* res;

    assert(kind == TYPE_STRUCT || kind == TYPE_UNION);
    res = typespec(kind);
    res->u.aggregate.name = name;
    res->u.aggregate.fields = ast_dup(fields, nfields);
    res->u.aggregate.nfields = nfields;

    return res;
}

/* --------------------------------------------------------------------- */

struct stmt;
typedef struct stmt stmt_t;

enum
{
    DECL_NONE,
    DECL_VARS,
    DECL_ENUM,
    DECL_FUNC,
};

struct decl_name
{
    char* name;
    expr_t* value;
};

struct func_param
{
    char* name;
    typespec_t* type;
    expr_t* default_val;
};

typedef struct decl_name decl_name_t;
typedef struct func_param func_param_t;

struct decl
{
    int kind;
    char* name;

    union
    {
        struct
        {
            typespec_t* type;
            decl_name_t* names;
            int nnames;
        }
        vars;

        struct
        {
            decl_name_t* items;
            int nitems;
        }
        enum_;

        struct
        {
            char* name;
            typespec_t* ret_type;
            func_param_t* params;
            int nparams;
            stmt_t* body;
        }
        func;
    }
    u;
};

decl_t* decl(int kind)
{
    decl_t* res;

    res = (decl_t*)ast_alloc(sizeof(decl_t));
    res->kind = kind;

    return res;
}

decl_t* decl_vars(decl_name_t* names, int nnames, typespec_t* type)
{
    decl_t* res;

    res = decl(DECL_VARS);
    res->u.vars.names = ast_dup(names, nnames);
    res->u.vars.nnames = nnames;
    res->u.vars.type = type;

    return res;
}

decl_t* decl_enum(decl_name_t* items, int nitems)
{
    decl_t* res;

    res = decl(DECL_ENUM);
    res->u.enum_.items = ast_dup(items, nitems);
    res->u.enum_.nitems = nitems;

    return res;
}

decl_t* decl_func(char* name, typespec_t* ret_type, func_param_t* params,
    int nparams, stmt_t* body)
{
    decl_t* res;

    res = decl(DECL_FUNC);
    res->u.func.name = name;
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
    EXPR_COMPOUND,
    EXPR_FIELD,
    EXPR_INDEX,
    EXPR_TOINT,
    EXPR_TOFLOAT,
    EXPR_SIZEOF_TYPE,
    EXPR_SIZEOF_EXPR,
    EXPR_OFFSETOF,
    EXPR_PUSH8,
    EXPR_PUSH16,
    EXPR_PUSH32,
    EXPR_PUSH64,
};

enum
{
    COMPOUND_NONE,
    COMPOUND_INDEX,
    COMPOUND_FIELD,
};

struct compoundlit_item
{
    int kind;
    expr_t* value;

    union
    {
        expr_t* index;
        char* name;
    }
    u;
};

typedef struct compoundlit_item compoundlit_item_t;

compoundlit_item_t* compoundlit_item(int kind, expr_t* value)
{
    compoundlit_item_t* res;

    res = ast_alloc(sizeof(compoundlit_item_t));
    res->kind = kind;
    res->value = value;

    return res;
}

compoundlit_item_t* compoundlit_index(expr_t* index, expr_t* value)
{
    compoundlit_item_t* res;

    res = compoundlit_item(COMPOUND_INDEX, value);
    res->u.index = index;

    return res;
}

compoundlit_item_t* compoundlit_field(char* name, expr_t* value)
{
    compoundlit_item_t* res;

    res = compoundlit_item(COMPOUND_FIELD, value);
    res->u.name = name;

    return res;
}

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
            compoundlit_item_t** items;
            int nitems;
        }
        compound;

        struct
        {
            expr_t* object;
            char* field_name;
        }
        field;

        struct
        {
            expr_t* array;
            expr_t* index;
        }
        index;

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

expr_t* expr_compound(compoundlit_item_t** items, int nitems)
{
    expr_t* res;

    res = expr(EXPR_COMPOUND);
    res->u.compound.items = ast_dup(items, nitems);
    res->u.compound.nitems = nitems;

    return res;
}

expr_t* expr_field(expr_t* object, char* field_name)
{
    expr_t* res;

    res = expr(EXPR_FIELD);
    res->u.field.object = object;
    res->u.field.field_name = field_name;

    return res;
}

expr_t* expr_index(expr_t* array, expr_t* index)
{
    expr_t* res;

    res = expr(EXPR_INDEX);
    res->u.index.array = array;
    res->u.index.index = index;

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

expr_t* expr_sizeof_type(typespec_t* type)
{
    expr_t* res;

    res = expr(EXPR_SIZEOF_TYPE);
    res->u.sizeof_type = type;

    return res;
}

expr_t* expr_sizeof_expr(expr_t* exp)
{
    expr_t* res;

    res = expr(EXPR_SIZEOF_EXPR);
    res->u.expr = exp;

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

expr_t* expr_push(int bits, expr_t* dst, expr_t* src)
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
    STMT_NOOP,
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

struct switch_case
{
    expr_t* exprs;
    int nexprs;
    stmt_t* body;
};

typedef struct switch_case switch_case_t;

struct stmt
{
    int kind;

    union
    {
        decl_t* decl;
        expr_t* expr;

        struct
        {
            stmt_t** stmts;
            int nstmts;
        }
        block;

        struct
        {
            expr_t* cond;
            stmt_t* then;
            stmt_t* else_;
        }
        if_;

        struct
        {
            stmt_t* init;
            expr_t* cond;
            expr_t* iter;
            stmt_t* body;
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

stmt_t* stmt(int kind)
{
    stmt_t* res;

    res = (stmt_t*)ast_alloc(sizeof(stmt_t));
    res->kind = kind;

    return res;
}

stmt_t* stmt_block(stmt_t** stmts, int nstmts)
{
    stmt_t* res;

    res = stmt(STMT_BLOCK);
    res->u.block.stmts = ast_dup(stmts, nstmts);
    res->u.block.nstmts = nstmts;

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

stmt_t* stmt_if(expr_t* cond, stmt_t* then, stmt_t* else_)
{
    stmt_t* res;

    res = stmt(STMT_IF);
    res->u.if_.cond = cond;
    res->u.if_.then = then;
    res->u.if_.else_ = else_;

    return res;
}

stmt_t* stmt_loop(int kind, expr_t* cond, stmt_t* body)
{
    stmt_t* res;

    assert(kind == STMT_WHILE || kind == STMT_DO);
    res = stmt(kind);
    res->u.loop.cond = cond;
    res->u.loop.body = body;

    return res;
}

stmt_t* stmt_for(stmt_t* init, expr_t* cond, expr_t* iter,
    stmt_t* body)
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

void print_expr(expr_t* expr, int indent);
void print_decl(decl_t* decl, int indent);

void print_typespec(typespec_t* type, int indent)
{
    int i;

    switch (type->kind)
    {
    case TYPE_NAME:
        printf(type->u.name);
        break;

    case TYPE_ARRAY:
        printf("(array ");
        print_typespec(type->u.array.element_type, indent);
        printf(" ");
        print_expr(type->u.array.len, indent);
        printf(")");
        break;

    case TYPE_PTR:
        printf("(ptr ");
        print_typespec(type->u.pointed, indent);
        printf(")");
        break;

    case TYPE_STRUCT:
    case TYPE_UNION:
        if (type->kind == TYPE_STRUCT) {
            printf("(struct ");
        } else {
            printf("(union ");
        }

        if (type->u.aggregate.name) {
            printf("%s", type->u.aggregate.name);
        } else {
            printf("nil");
        }

        for (i = 0; i < type->u.aggregate.nfields; ++i)
        {
            decl_t* it = type->u.aggregate.fields[i];

            printf("\n");
            print_indent(indent + 1);
            print_decl(it, indent + 1);
        }

        printf(")");
        break;
    }
}

void print_stmt(stmt_t* stmt, int indent);

void print_decl(decl_t* decl, int indent)
{
    int i;

    switch (decl->kind)
    {
    case DECL_VARS:
        printf("(");
        print_typespec(decl->u.vars.type, indent);

        if (!decl->u.vars.nnames) {
            printf(" nil");
        }

        else for (i = 0; i < decl->u.vars.nnames; ++i)
        {
            decl_name_t* name = &decl->u.vars.names[i];

            printf(" (%s ", name->name);

            if (name->value) {
                print_expr(name->value, indent);
            } else {
                printf("nil");
            }

            printf(")");
        }

        printf(")");
        break;

    case DECL_ENUM:
        printf("(enum ");

        for (i = 0; i < decl->u.enum_.nitems; ++i)
        {
            decl_name_t* it = &decl->u.enum_.items[i];

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
        print_typespec(decl->u.func.ret_type, indent);
        printf(" %s) (", decl->name);

        for (i = 0; i < decl->u.func.nparams; ++i)
        {
            func_param_t* it = &decl->u.func.params[i];

            if (i) {
                printf(" ");
            }

            printf("(");
            print_typespec(it->type, indent);
            printf(" %s ", it->name);

            if (it->default_val) {
                print_expr(it->default_val, indent);
            } else {
                printf("nil");
            }

            printf(")");
        }

        printf(")\n");

        print_stmt(decl->u.func.body, indent + 1);
        printf(")");
        break;

    default:
        assertf(0, "unknown decl kind %d", decl->kind);
    }
}

void print_expr(expr_t* expr, int indent)
{
    int i;

    switch (expr->kind)
    {
    case EXPR_INT:
        /* TODO preserve the base that was specified in the code */
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

    case EXPR_TERNARY:
        printf("(? ");
        print_expr(expr->u.ternary.cond, indent);
        printf(" ");
        print_expr(expr->u.ternary.then, indent);
        printf(" ");
        print_expr(expr->u.ternary.else_, indent);
        printf(")");
        break;

    case EXPR_BINARY:
        printf("(%s ", lkinds[expr->u.binary.operator]);
        print_expr(expr->u.binary.left, indent);
        printf(" ");
        print_expr(expr->u.binary.right, indent);
        printf(")");
        break;

    case EXPR_UNARY:
        printf("(%s ", lkinds[expr->u.unary.operator]);
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

    case EXPR_COMPOUND:
        printf("(compound");

        for (i = 0; i < expr->u.compound.nitems; ++i)
        {
            compoundlit_item_t* it = expr->u.compound.items[i];

            printf("\n");
            print_indent(indent + 1);

            if (it->kind == COMPOUND_FIELD) {
                printf("(field %s ", it->u.name ? it->u.name : "nil");
            }

            else if (it->kind == COMPOUND_INDEX)
            {
                printf("(index ");

                if (it->u.index) {
                    print_expr(it->u.index, indent);
                } else {
                    printf("nil");
                }

                printf(" ");
            }

            print_expr(it->value, indent);
            printf(")");
        }

        printf(")");
        break;

    case EXPR_FIELD:
        printf("(get ");
        print_expr(expr->u.field.object, 0);
        printf(" %s)", expr->u.field.field_name);
        break;

    case EXPR_TOINT:
        printf("(toint ");
        print_expr(expr->u.expr, indent);
        printf(")");
        break;

    case EXPR_TOFLOAT:
        printf("(tofloat ");
        print_expr(expr->u.expr, indent);
        printf(")");
        break;

    case EXPR_SIZEOF_TYPE:
        printf("(sizeof-type ");
        print_typespec(expr->u.sizeof_type, indent);
        printf(")");
        break;

    case EXPR_SIZEOF_EXPR:
        printf("(sizeof-expr ");
        print_expr(expr->u.expr, indent);
        printf(")");
        break;

    case EXPR_OFFSETOF:
        printf("(offsetof ");
        print_typespec(expr->u.offsetof_.type, indent);
        printf(" %s)", expr->u.offsetof_.field);
        break;

    case EXPR_PUSH8:
    case EXPR_PUSH16:
    case EXPR_PUSH32:
    case EXPR_PUSH64:
        printf("(push%d ", 1 << (expr->kind - EXPR_PUSH8 + 3));
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
    case STMT_NOOP:
        break;

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
        printf("(do");

        for (i = 0; i < stmt->u.block.nstmts; ++i)
        {
            printf("\n");
            print_stmt(stmt->u.block.stmts[i], indent + 1);
        }

        printf(")");
        break;

    case STMT_IF:
        printf("(if ");
        print_expr(stmt->u.if_.cond, indent);
        printf("\n");
        print_stmt(stmt->u.if_.then, indent);

        if (stmt->u.if_.else_)
        {
            printf("\n");
            print_indent(indent);
            printf("(else\n");
            print_stmt(stmt->u.if_.else_, indent);
            printf(")");
        }

        printf(")");
        break;

    case STMT_WHILE:
        printf("(while ");
        print_expr(stmt->u.loop.cond, indent);
        printf("\n");
        printf("(do\n");
        print_stmt(stmt->u.loop.body, indent + 1);
        printf("))");
        break;

    case STMT_DO:
        printf("(loop\n");
        print_stmt(stmt->u.loop.body, indent);
        printf("\n");
        print_indent(indent);
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
        print_expr(stmt->u.loop.iter, indent);
        printf("\n");
        printf("(do\n");
        print_stmt(stmt->u.loop.body, indent + 1);
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
            print_stmt(it->body, indent + 1);
            printf("))\n");
        }
        break;

    default:
        assertf(0, "unknown stmt kind %d", stmt->kind);
    }
}

/* --------------------------------------------------------------------- */

typespec_t* find_type(char* name)
{
    (void)name;
    return 0; /* TODO */
}

int ppeek(int kind)
{
    return ltok.kind == kind;
}

int pmatch(int kind)
{
    if (ppeek(kind))
    {
        lnext();
        return 1;
    }

    return 0;
}

int prange(int first, int last)
{
    return ltok.kind >= first && ltok.kind <= last;
}

int ppeek_kword(char* kword)
{
    return ppeek(TOKEN_KWORD) && ltok.u.name == kword;
}

int pmatch_kword(char* kword)
{
    if (ppeek_kword(kword))
    {
        lnext();
        return 1;
    }

    return 0;
}

char ebuf[8192];

#define pexpect_kword(kind) pexpect_kword_(__FILE__, __LINE__, kind)

int pexpect_kword_(char* file, int line, char* kword)
{
    if (!pmatch_kword(kword))
    {
        syntax_errorf("[%s:%d] unexpected token. got %s, "
            "expected keyword %s",file, line, ldescribe(&ltok, ebuf),
            kword);
        exit(1);
        return 0;
    }

    return 1;
}

#define pexpect(kind) pexpect_(__FILE__, __LINE__, kind)

int pexpect_(char* file, int line, int kind)
{
    if (!pmatch(kind))
    {
        syntax_errorf("[%s:%d] unexpected token. got %s, expected %s",
            file, line, ldescribe(&ltok, ebuf),
            lkindstr(kind, ebuf + 4096));
        exit(1);
        return 0;
    }

    return 1;
}

expr_t* pexpr();
decl_t* pdecl_vars();

/*
 * (("struct" | "union") name? '{' decl_vars ';')* '}')
 * | (name ('*' | '[' expr ']')*)
 */
typespec_t* ptype()
{
    typespec_t* res;
    char* name = 0;

    if (ppeek_kword(kword_struct) || ppeek_kword(kword_union))
    {
        int kind;
        decl_t** decls = 0;

        if (pmatch_kword(kword_struct)) {
            kind = TYPE_STRUCT;
        }

        else if (pmatch_kword(kword_union)) {
            kind = TYPE_UNION;
        }

        else {
            assert(0);
        }

        name = ltok.u.name;

        if (!pmatch(TOKEN_NAME)) {
            name = 0;
        }

        if (!pmatch('{')) {
            res = 0;
            goto cleanup;
        }

        while (!ppeek(0) && !ppeek('}'))
        {
            decl_t* decl;

            decl = pdecl_vars();
            if (!decl) {
                res = 0;
                goto cleanup;
            }

            bpush(decls, decl);
        }

        if (!pmatch('}')) {
            res = 0;
            goto cleanup;
        }

        res = typespec_aggregate(kind, name, decls, blen(decls));

    cleanup:
        bfree(decls);

        if (!res) {
            return 0;
        }
    }

    else
    {
        name = ltok.u.name;

        if (!pmatch(TOKEN_NAME)) {
            name = 0;
        }

        res = typespec_name(name);
    }

    for (;;)
    {
        if (pmatch('*')) {
            res = typespec_ptr(res);
        }

        else if (pmatch('['))
        {
            expr_t* len;

            len = pexpr();
            if (!pmatch(']')) {
                return 0;
            }

            res = typespec_array(res, len);
        }

        else {
            break;
        }
    }

    return res;
}

/* "sizeof" '(' (expr | type) ')' */
expr_t* pexpr_sizeof()
{
    pexpect_kword(kword_sizeof);

    pexpect('(');

    if (ppeek(TOKEN_NAME) && find_type(ltok.u.name))
    {
        typespec_t* type;

        type = ptype();
        pexpect(')');

        return expr_sizeof_type(type);
    }

    else
    {
        expr_t* expr;

        expr = pexpr();
        pexpect(')');

        if (expr->kind != EXPR_NAME && expr->kind != EXPR_FIELD) {
            syntax_error("sizeof only applies to names and fields");
        }

        return expr_sizeof_expr(expr);
    }
}

/* "offsetof" '(' type ',' field_name ')' */
expr_t* pexpr_offsetof()
{
    typespec_t* type;
    char* field;

    pexpect_kword(kword_offsetof);

    pexpect('(');
    type = ptype();
    pexpect(',');

    field = ltok.u.name;

    if (!pexpect(TOKEN_NAME)) {
        field = 0;
    }

    pexpect(')');

    return expr_offsetof(type, field);
}

/*
 *   "push8"  '(' expr ',' expr ')'
 * | "push16" '(' expr ',' expr ')'
 * | "push32" '(' expr ',' expr ')'
 * | "push64" '(' expr ',' expr ')'
 */
expr_t* pexpr_push()
{
    int bits = 0;
    expr_t* dst;
    expr_t* src;

         if (pmatch_kword(kword_push8 )) bits =  8;
    else if (pmatch_kword(kword_push16)) bits = 16;
    else if (pmatch_kword(kword_push32)) bits = 32;
    else if (pmatch_kword(kword_push64)) bits = 64;

    pexpect('(');
    dst = pexpr();
    pexpect(',');
    src = pexpr();
    pexpect(')');

    return expr_push(bits, dst, src);
}

/*
 * compoundlit_item = (('.' field_name) | ('[' expr ']') '=')? expr
 *
 * '{' (compoundlit_item (',' compoundlit_item)* ','?)? '}'
 */
expr_t* pexpr_compound()
{
    compoundlit_item_t** items = 0;

    pexpect('{');

    do
    {
        expr_t* value;

        if (ppeek('}')) {
            break;
        }

        if (pmatch('.'))
        {
            char* name;

            name = ltok.u.name;
            pexpect(TOKEN_NAME);
            pexpect(TOKEN_EQ);
            value = pexpr();

            bpush(items, compoundlit_field(name, value));
        }

        else if (pmatch('['))
        {
            expr_t* index;

            index = pexpr();
            pexpect(']');
            pexpect(TOKEN_EQ);
            value = pexpr();

            bpush(items, compoundlit_index(index, value));
        }

        else {
            bpush(items, compoundlit_field(0, pexpr()));
        }
    }
    while(pmatch(','));

    pexpect('}');

    return expr_compound(items, blen(items));
}

expr_t* pexpr_primitive()
{
    if (ppeek(TOKEN_INT))
    {
        uint64_t u64;

        u64 = ltok.u.u64;
        lnext();

        return expr_int(u64);
    }

    if (ppeek(TOKEN_FLOAT))
    {
        double f64;

        f64 = ltok.u.f64;
        lnext();

        return expr_float(f64);
    }

    if (ppeek(TOKEN_STRING))
    {
        char* str;

        str = ltok.u.string;
        lnext();

        return expr_str(str);
    }

    if (ppeek(TOKEN_NAME))
    {
        char* name;

        name = ltok.u.string;
        lnext();

        return expr_name(name);
    }

    if (pmatch_kword(kword_toint)) {
        return expr_toint(pexpr());
    }

    if (pmatch_kword(kword_tofloat)) {
        return expr_tofloat(pexpr());
    }

    if (ppeek_kword(kword_sizeof)) {
        return pexpr_sizeof();
    }

    if (ppeek_kword(kword_offsetof)) {
        return pexpr_offsetof();
    }

    if (ppeek_kword(kword_push8) ||
        ppeek_kword(kword_push16) ||
        ppeek_kword(kword_push32) ||
        ppeek_kword(kword_push64))
    {
        return pexpr_push();
    }

    if (ppeek('{')) {
        return pexpr_compound();
    }

    if (pmatch('('))
    {
        expr_t* expr;

        expr = pexpr();
        pexpect(')');

        return expr;
    }

    syntax_errorf("unexpected token %s in expression",
        ldescribe(&ltok, ebuf));

    exit(1);

    return 0;
}

/*
 * expr_primitive ('(' expr? (',' expr)* ','? ')'
 *               | '[' expr ']'
 *               | '.' field_name)?
 */
expr_t* pexpr_special()
{
    expr_t* res;

    res = pexpr_primitive();

    while (ppeek('(') || ppeek('{') || ppeek('.'))
    {
        if (pmatch('('))
        {
            expr_t** params = 0;

            do
            {
                if (ppeek(')')) {
                    break;
                }

                bpush(params, pexpr());
            }
            while (pmatch(','));

            pexpect(')');

            res = expr_call(res, params, blen(params));
            bfree(params);
        }

        else if (pmatch('['))
        {
            expr_t* index;

            index = pexpr();
            pexpect(']');
            res = expr_index(res, index);
        }

        else if (pmatch('.'))
        {
            char* name;

            name = ltok.u.name;
            pexpect(TOKEN_NAME);
            res = expr_field(res, name);
        }

        else
        {
            syntax_errorf("unexpected token %s", ldescribe(&ltok, ebuf));
        }
    }

    return res;
}

/* expr_primitive (('~' | '!' | '+' | '-' | '*' | '&') expr_unary)? */
expr_t* pexpr_unary()
{
    if (ppeek(TOKEN_NOT) || ppeek(TOKEN_NEG) || ppeek(TOKEN_ADD) ||
        ppeek(TOKEN_SUB) || ppeek(TOKEN_MUL) || ppeek(TOKEN_AND))
    {
        int operator;

        operator = ltok.kind;
        lnext();
        return expr_unary(operator, pexpr_unary());
    }

    return pexpr_special();
}

/* expr_unary (('*' | '/' | '&' | '%' | "<<" | ">>") expr_unary)* */
expr_t* pexpr_mul()
{
    expr_t* res;

    res = pexpr_unary();

    while (prange(TOKEN_FIRST_MUL, TOKEN_LAST_MUL))
    {
        int operator;

        operator = ltok.kind;
        lnext();
        res = expr_binary(operator, res, pexpr_unary());
    }

    return res;
}

/* expr_mul (('+' | '-' | '|' | '^') expr_mul)* */
expr_t* pexpr_add()
{
    expr_t* res;

    res = pexpr_mul();

    while (prange(TOKEN_FIRST_ADD, TOKEN_LAST_ADD))
    {
        int operator;

        operator = ltok.kind;
        lnext();
        res = expr_binary(operator, res, pexpr_mul());
    }

    return res;
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

    while (pmatch(TOKEN_OROR)) {
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

/*
 * expr_ternary (('=' | "+=" | "-=" | "*=" |  "/=" | "%=" | "^=" | "&="
 *                | "<<=" | ">>=" | "|=") expr_assignment)?
 *
 * note the right-associativity
 * a = b = c = d;
 * a = (b = (c = d));
 */
expr_t* pexpr_assignment()
{
    expr_t* res;

    res = pexpr_ternary();

    if (prange(TOKEN_FIRST_EQ, TOKEN_LAST_EQ))
    {
        int operator;

        operator = ltok.kind;
        lnext();
        return expr_binary(operator, res, pexpr_assignment());
    }

    return res;
}

expr_t* pexpr()
{
    return pexpr_assignment();
}

/*
 * decl_name = (name ('=' expr)?
 * typespec (':' decl_name (',' decl_name)*)*)? ';'
 */
decl_t* pdecl_vars()
{
    decl_t* res;
    typespec_t* type;
    decl_name_t* names = 0;

    type = ptype();

    if (!type) {
        return 0;
    }

    if (pmatch(':'))
    {
        do
        {
            char* name;
            expr_t* expr = 0;

            name = ltok.u.name;

            if (!pexpect(TOKEN_NAME)) {
                name = 0;
            }

            if (pmatch(TOKEN_EQ)) {
                expr = pexpr();
            }

            bpush(names, (decl_name_t){name, expr});
        }
        while (pmatch(','));
    }

    if (!pmatch(';')) {
        return 0;
    }

    res = decl_vars(names, blen(names), type);
    bfree(names);

    return res;
}

/*
 * TODO:
 *
 * stmt_switch = "switch" '(' expr ')' '{'
 *               ("case" expr ':') | ("default" ':') | stmt
 *               '}'
 * stmt_pick = "pick" '(' expr ')' '{'
 *             ("case" expr ':') | ("default" ':') | stmt
 *             '}'
 *
 * stmt_base = decl | "break" | "continue" | "fallthrough" | expr
 *           | ("return" expr)
 */

stmt_t* pstmt_base()
{
    if (pmatch_kword(kword_break)) {
        return stmt(STMT_BREAK);
    }

    else if (pmatch_kword(kword_continue)) {
        return stmt(STMT_CONTINUE);
    }

    else if (pmatch_kword(kword_fallthrough)) {
        return stmt(STMT_FALLTHROUGH);
    }

    else if (pmatch_kword(kword_return)) {
        return stmt_return(pexpr());
    }

    return stmt_expr(pexpr());
}

/*
 * decl | '' | stmt_block | stmt_if | stmt_while | stmt_for | stmt_do |
 * stmt_switch | stmt_pick (stmt_base ';')
 */
stmt_t* pstmt()
{
    token_t start_tok;
    char* start;
    decl_t* decl;

    /*
     * this is a ugly hack to distinguish var decl's from expressions
     * we look ahead and see if we have a valid decl, otherwise we rewind
     *
     * TODO: how can I make this less shitty?
     */

    start_tok = ltok;
    start = ldata;
    decl = pdecl_vars();

    if (decl) {
        return stmt_decl(decl);
    }

    ltok = start_tok;
    ldata = start;

    if (pmatch(';')) {
        return stmt(STMT_NOOP);
    }

    /* '{' stmt* '}' ';'* */
    else if (pmatch('{'))
    {
        stmt_t** stmts = 0;

        while (!ppeek(0) && !ppeek('}')) {
            bpush(stmts, pstmt());
        }

        pexpect('}');

        return stmt_block(stmts, blen(stmts));
    }

    /* "if" '(' expr ')' stmt ("else" stmt)? */
    if (pmatch_kword(kword_if))
    {
        expr_t* cond;
        stmt_t* then;
        stmt_t* else_ = 0;

        pexpect('(');
        cond = pexpr();
        pexpect(')');
        then = pstmt();

        if (pmatch_kword(kword_else)) {
            else_ = pstmt();
        }

        return stmt_if(cond, then, else_);
    }

    /* "while" '(' expr ')' stmt */
    if (pmatch_kword(kword_while))
    {
        expr_t* cond;
        stmt_t* body;

        pexpect('(');
        cond = pexpr();
        pexpect(')');
        body = pstmt();

        return stmt_loop(STMT_WHILE, cond, body);
    }

    /* "do" '{' stmt '}' "while" '(' expr ')' ';' */
    if (pmatch_kword(kword_do))
    {
        stmt_t* body;
        expr_t* cond;

        body = pstmt();
        pexpect_kword(kword_while);
        pexpect('(');
        cond = pexpr();
        pexpect(')');
        pexpect(';');

        return stmt_loop(STMT_DO, cond, body);
    }

    /* "for" '(' stmt expr? ';' expr? ')' stmt */
    if (pmatch_kword(kword_for))
    {
        stmt_t* init = 0;
        expr_t* cond = 0;
        expr_t* iter = 0;
        stmt_t* body;

        pexpect('(');

        init = pstmt();

        if (!pmatch(';'))
        {
            cond = pexpr();
            pexpect(';');
        }

        if (!pmatch(')'))
        {
            iter = pexpr();
            pexpect(')');
        }

        body = pstmt();

        return stmt_for(init, cond, iter, body);
    }

    /* stmt_base ';' */
    else
    {
        stmt_t* res;

        res = pstmt_base();
        pexpect(';');
        return res;
    }

    return 0;
}

void test_expr(char* code)
{
    printf("code:\n%s\n\n", code);
    lreset(code);
    printf("syntax tree:\n");
    print_expr(pexpr(), 0);
    printf("\n\n\n");
}

void test_stmt(char* code)
{
    printf("code:\n%s\n\n", code);
    lreset(code);
    printf("syntax tree:\n");
    print_stmt(pstmt(), 0);
    printf("\n\n\n");
}

void test_p()
{
    test_expr("(a + b + sizeof(c) + (flags & SOME_FLAG ? x : y))");
    test_expr("1 * 2 + 3 + 4 / 2");
    test_expr("buf = malloc(cap * elem_size + offsetof(header, buf))");
    test_expr(
        "weak = (\n"
        "    e1 & FIRE  && e2 & (WATER | ROCK) ||\n"
        "    e1 & ROCK  && e2 & (WATER | GRASS) ||\n"
        "    e1 & WATER && e2 & GRASS ||\n"
        "    e1 & GRASS && e2 & FIRE\n"
        ")"
    );

    test_expr("push8(p, 0755)");
    test_expr("push16(p, 0b1000000000000000)");
    test_expr("push32(p, 0xAABBCCDD)");
    test_expr("push64(p, 6000000000)");
    test_expr("push64(p, push8(p, 10))");
    test_expr("a = b = meme_memes + 10");
    test_expr(
        "long_ass_function(\n"
        "    long_ass_param_1,\n"
        "    long_ass_param_2,\n"
        "    long_ass_param_3,\n"
        "    long_ass_param_4,\n"
        ")"
    );
    test_expr("foo()");
    test_expr("(((1+1)))");
    test_expr("3.14 * 1e10 + 1");
    test_expr("a || !b ? x * -z : a + +y");
    test_expr("p = &var");
    test_expr("value = *p");
    test_expr("point = {}");
    test_expr("point = { x, y }");
    test_expr("point = { .x = .37, .37 }");
    test_expr("point = { .37, .37 }");
    test_expr("point = { .x = 13.37, 13.37 }");
    test_expr("point = { 13.37, 13.37 }");
    test_expr(
        "rect = {\n"
        "   x, y,\n"
        "   x + width,\n"
        "   y + height,\n"
        "}"
    );
    test_expr(
        "rect = {\n"
        "   .left = x,\n"
        "   .top = y,\n"
        "   .right = x + width,\n"
        "   .bottom = y + height,\n"
        "}"
    );
    test_expr(
        "char_to_digit =\n"
        "{\n"
        "    ['0'] = 0,\n"
        "    ['1'] = 1,\n"
        "    ['2'] = 2,\n"
        "    ['3'] = 3,\n"
        "    ['4'] = 4,\n"
        "    ['5'] = 5,\n"
        "    ['6'] = 6,\n"
        "    ['7'] = 7,\n"
        "    ['8'] = 8,\n"
        "    ['9'] = 9,\n"
        "    ['a'] = 10, ['A'] = 10,\n"
        "    ['b'] = 11, ['B'] = 11,\n"
        "    ['c'] = 12, ['C'] = 12,\n"
        "    ['d'] = 13, ['D'] = 13,\n"
        "    ['e'] = 14, ['E'] = 14,\n"
        "    ['f'] = 15, ['F'] = 15,\n"
        "}"
    );
    test_expr("b = pt.x");
    test_expr("left = rect.lt.x");

    test_stmt(
        "{\n"
        "    float: b = 42.42;\n"
        "    int: a = toint(b);\n"
        "    struct point { int: x; int: y; };\n"
        "    point: pt = { 10 + a, 20 };\n"
        "    struct { int: x, y, z; }: v3;\n"
        "\n"
        "    struct shape\n"
        "    {\n"
        "        int: kind;\n"
        "        union {\n"
        "            struct { float: x, y; }: pt;\n"
        "            struct { float: l, t, r, b; }: rect;\n"
        "            float: circle_radius;\n"
        "        }: u;\n"
        "    };\n"
        "\n"
        "    do\n"
        "    {\n"
        "        do_stuff();\n"
        "        a *= 2;\n"
        "        if (a > 500) {\n"
        "            b /= 10;\n"
        "        } else {\n"
        "            b += 10;\n"
        "        }\n"
        "    }\n"
        "    while (keep_going);\n"
        "}"
    );
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
    linit();
    tests();

    return 0;
}
