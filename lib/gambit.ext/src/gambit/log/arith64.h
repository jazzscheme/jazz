/* File: "arith64.h"   (c) 1997-1998, Marc Feeley */

/* Routines for 64 bit arithmetic. */

#ifndef ARITH64_H
#define ARITH64_H

/*---------------------------------------------------------------------------*/

/* Various builtin integer types */


/* U8 corresponds to a builtin unsigned 8 bit integer type */

typedef unsigned char U8;


/* U16 corresponds to a builtin unsigned 16 bit integer type */

typedef unsigned short U16;


/* U32 corresponds to a builtin unsigned 32 bit integer type */

typedef unsigned int U32;


/*---------------------------------------------------------------------------*/

#ifdef __GNUC__

/* U64 represents an unsigned 64 bit integer type */

typedef unsigned long long U64;

#define U64_to_double(n) ((double)(n))
#define U64_to_U32(n) ((U32)(n))
#define U64_init(hi32,lo32) (((U64)(U32)(hi32)<<32)+(U64)(U32)(lo32))
#define U64_less_than_U64(n,m) ((U64)(n)<(U64)(m))
#define U64_less_than_U32(n,m) ((U64)(n)<(U32)(m))
#define U64_add_U64(n,m) ((U64)(n)+(U64)(m))
#define U64_add_U32(n,m) ((U64)(n)+(U32)(m))
#define U64_sub_U64(n,m) ((U64)(n)-(U64)(m))
#define U64_sub_U32(n,m) ((U64)(n)-(U32)(m))
#define U64_mul_U64(n,m) ((U64)(n)*(U64)(m))
#define U64_mul_U32(n,m) ((U64)(n)*(U32)(m))
#define U64_div_U64(n,m) ((U64)(n)/(U64)(m))
#define U64_div_U32(n,m) ((U64)(n)/(U32)(m))
#define U64_mod_U32(n,m) ((U32)((U64)(n)%(U32)(m)))

static U32 U64_shift_right (dest, count)
U64 *dest;
int count;
{
  U32 result = (*dest) & ~(~0ULL<<count);
  *dest >>= count;
  return result;
}

#else

/* U64 represents an unsigned 64 bit integer type */

typedef struct { U32 lo32, hi32; } U64;


static double U64_to_double (n)
U64 n;
{
  return n.hi32*4294967296.0 + n.lo32;
}


static U32 U64_to_U32 (n)
U64 n;
{
  return n.lo32;
}


static U64 U64_init (hi32, lo32)
U32 hi32;
U32 lo32;
{
  U64 result;
  result.lo32 = lo32;
  result.hi32 = hi32;
  return result;
}


static int U64_less_than_U64 (n, m)
U64 n;
U64 m;
{
  U32 x = n.hi32;
  U32 y = m.hi32;
  return (x < y) || ((x == y) && (n.lo32 < m.lo32));
}


static int U64_less_than_U32 (n, m)
U64 n;
U32 m;
{
  return (n.hi32 == 0) && (n.lo32 < m);
}


static U64 U64_add_U64 (n, m)
U64 n;
U64 m;
{
  U32 x = n.lo32;
  U32 y = n.hi32;
  U32 a = x + m.lo32;
  U32 b = y + m.hi32;
  if (a < x)
    b++;
  n.lo32 = a;
  n.hi32 = b;
  return n;
}


static U64 U64_add_U32 (n, m)
U64 n;
U32 m;
{
  U32 x = n.lo32;
  U32 a = x + m;
  if (a < x)
    n.hi32++;
  n.lo32 = a;
  return n;
}


static U64 U64_sub_U64 (n, m)
U64 n;
U64 m;
{
  U32 x = n.lo32;
  U32 y = n.hi32;
  U32 a = x - m.lo32;
  U32 b = y - m.hi32;
  if (a > x)
    b--;
  n.lo32 = a;
  n.hi32 = b;
  return n;
}


static U64 U64_sub_U32 (n, m)
U64 n;
U32 m;
{
  U32 x = n.lo32;
  U32 a = x - m;
  if (a > x)
    n.hi32--;
  n.lo32 = a;
  return n;
}


static U64 U64_mul_U32 (n, m)
U64 n;
U32 m;
{
  U16 xlo16 = n.lo32;
  U16 xhi16 = n.lo32>>16;
  U16 ylo16 = n.hi32;
  U16 yhi16 = n.hi32>>16;
  U16 m16 = m;
  U32 a = m16 * xlo16;
  U32 b = m16 * xhi16 + (a>>16);
  U32 c = m16 * ylo16 + (b>>16);
  U32 d = m16 * yhi16 + (c>>16);
  m16 = m>>16;
  if (m16 > 0)
    {
      U32 e = m16 * xlo16;
      U32 f = m16 * xhi16 + (e>>16);
      U32 g = m16 * ylo16 + (f>>16);
      b = (b & ~(~0<<16)) + (e & ~(~0<<16));
      c = (c & ~(~0<<16)) + (f & ~(~0<<16)) + (b>>16);
      d = (d & ~(~0<<16)) + (g & ~(~0<<16)) + (c>>16);
    }
  n.lo32 = (a & ~(~0<<16)) + (b<<16);
  n.hi32 = (c & ~(~0<<16)) + (d<<16);
  return n;
}


static U64 U64_mul_U64 (n, m)
U64 n;
U64 m;
{
  U64 result = U64_mul_U32 (n, m.lo32);
  result.hi32 += n.lo32 * m.hi32;
  return result;
}


static U64 U64_div_U64 (n, m)
U64 n;
U64 m;
{
  double r = U64_to_double (n) / U64_to_double (m);
  U32 b = r / 4294967296.0;
  U32 a = r - b*4294967296.0;
  n.lo32 = a;
  n.hi32 = b;
  return n;
}


static U64 U64_div_U32 (n, m)
U64 n;
U32 m;
{
  U32 x = n.lo32;
  U32 y = n.hi32;
  n.lo32 = x/m + (U32)((y%m)*4294967296.0/m);
  n.hi32 = y/m;
  return n;
}


static U32 U64_mod_U32 (n, m)
U64 n;
U32 m;
{
  U64 result = U64_sub_U64 (n, U64_mul_U32 (U64_div_U32 (n, m), m));
  return result.lo32;
}


static U32 U64_shift_right (dest, count)
U64 *dest;
int count;
{
  U32 x = dest->lo32;
  U32 y = dest->hi32;
  if (count < 32)
    {
      dest->lo32 = (x>>count) + (y<<(32-count));
      dest->hi32 = y>>count;
      return x & ~(~0UL<<count);
    }
  else
    {
      dest->lo32 = y;
      dest->hi32 = 0;
      return x;
    }
}

#endif

/*---------------------------------------------------------------------------*/

#endif
