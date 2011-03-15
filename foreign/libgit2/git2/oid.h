/*
 * This file is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License, version 2,
 * as published by the Free Software Foundation.
 *
 * In addition to the permissions in the GNU General Public License,
 * the authors give you unlimited permission to link the compiled
 * version of this file into combinations with other programs,
 * and to distribute those combinations without any restriction
 * coming from the use of this file.  (The General Public License
 * restrictions do apply in other respects; for example, they cover
 * modification of the file, and distribution when not linked into
 * a combined executable.)
 *
 * This file is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file COPYING.  If not, write to
 * the Free Software Foundation, 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 */
#ifndef INCLUDE_git_oid_h__
#define INCLUDE_git_oid_h__

/**
 * @file git2/oid.h
 * @brief Git object id routines
 * @defgroup git_oid Git object id routines
 * @ingroup Git
 * @{
 */
GIT_BEGIN_DECL

/** Size (in bytes) of a raw/binary oid */
#define GIT_OID_RAWSZ 20

/** Size (in bytes) of a hex formatted oid */
#define GIT_OID_HEXSZ (GIT_OID_RAWSZ * 2)

/** Unique identity of any object (commit, tree, blob, tag). */
typedef struct {
	/** raw binary formatted id */
	unsigned char id[GIT_OID_RAWSZ];
} git_oid;

/**
 * Parse a hex formatted object id into a git_oid.
 * @param out oid structure the result is written into.
 * @param str input hex string; must be pointing at the start of
 *        the hex sequence and have at least the number of bytes
 *        needed for an oid encoded in hex (40 bytes).
 * @return GIT_SUCCESS if valid; GIT_ENOTOID on failure.
 */
GIT_EXTERN(int) git_oid_mkstr(git_oid *out, const char *str);

/**
 * Copy an already raw oid into a git_oid structure.
 * @param out oid structure the result is written into.
 * @param raw the raw input bytes to be copied.
 */
GIT_EXTERN(void) git_oid_mkraw(git_oid *out, const unsigned char *raw);

/**
 * Format a git_oid into a hex string.
 * @param str output hex string; must be pointing at the start of
 *        the hex sequence and have at least the number of bytes
 *        needed for an oid encoded in hex (40 bytes).  Only the
 *        oid digits are written; a '\\0' terminator must be added
 *        by the caller if it is required.
 * @param oid oid structure to format.
 */
GIT_EXTERN(void) git_oid_fmt(char *str, const git_oid *oid);

/**
 * Format a git_oid into a loose-object path string.
 * <p>
 * The resulting string is "aa/...", where "aa" is the first two
 * hex digitis of the oid and "..." is the remaining 38 digits.
 *
 * @param str output hex string; must be pointing at the start of
 *        the hex sequence and have at least the number of bytes
 *        needed for an oid encoded in hex (41 bytes).  Only the
 *        oid digits are written; a '\\0' terminator must be added
 *        by the caller if it is required.
 * @param oid oid structure to format.
 */
GIT_EXTERN(void) git_oid_pathfmt(char *str, const git_oid *oid);

/**
 * Format a gid_oid into a newly allocated c-string.
 * @param oid the oid structure to format
 * @return the c-string; NULL if memory is exhausted.  Caller must
 *         deallocate the string with free().
 */
GIT_EXTERN(char *) git_oid_allocfmt(const git_oid *oid);

/**
 * Format a git_oid into a buffer as a hex format c-string.
 * <p>
 * If the buffer is smaller than GIT_OID_HEXSZ+1, then the resulting
 * oid c-string will be truncated to n-1 characters. If there are
 * any input parameter errors (out == NULL, n == 0, oid == NULL),
 * then a pointer to an empty string is returned, so that the return
 * value can always be printed.
 *
 * @param out the buffer into which the oid string is output.
 * @param n the size of the out buffer.
 * @param oid the oid structure to format.
 * @return the out buffer pointer, assuming no input parameter
 *         errors, otherwise a pointer to an empty string.
 */
GIT_EXTERN(char *) git_oid_to_string(char *out, size_t n, const git_oid *oid);

/**
 * Copy an oid from one structure to another.
 * @param out oid structure the result is written into.
 * @param src oid structure to copy from.
 */
GIT_EXTERN(void) git_oid_cpy(git_oid *out, const git_oid *src);

/**
 * Compare two oid structures.
 * @param a first oid structure.
 * @param b second oid structure.
 * @return <0, 0, >0 if a < b, a == b, a > b.
 */
GIT_EXTERN(int) git_oid_cmp(const git_oid *a, const git_oid *b);

/** @} */
GIT_END_DECL
#endif
