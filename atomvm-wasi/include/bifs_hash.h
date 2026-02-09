/* C code produced by gperf version 3.0.3 */
/* Command-line: /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/gperf -t --output-file=/Users/main/elixir-workers/atomvm-wasi/include/bifs_hash.h /Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf  */
/* Computed positions: -k'8-11,$' */

#if !((' ' == 32) && ('!' == 33) && ('"' == 34) && ('#' == 35) \
      && ('%' == 37) && ('&' == 38) && ('\'' == 39) && ('(' == 40) \
      && (')' == 41) && ('*' == 42) && ('+' == 43) && (',' == 44) \
      && ('-' == 45) && ('.' == 46) && ('/' == 47) && ('0' == 48) \
      && ('1' == 49) && ('2' == 50) && ('3' == 51) && ('4' == 52) \
      && ('5' == 53) && ('6' == 54) && ('7' == 55) && ('8' == 56) \
      && ('9' == 57) && (':' == 58) && (';' == 59) && ('<' == 60) \
      && ('=' == 61) && ('>' == 62) && ('?' == 63) && ('A' == 65) \
      && ('B' == 66) && ('C' == 67) && ('D' == 68) && ('E' == 69) \
      && ('F' == 70) && ('G' == 71) && ('H' == 72) && ('I' == 73) \
      && ('J' == 74) && ('K' == 75) && ('L' == 76) && ('M' == 77) \
      && ('N' == 78) && ('O' == 79) && ('P' == 80) && ('Q' == 81) \
      && ('R' == 82) && ('S' == 83) && ('T' == 84) && ('U' == 85) \
      && ('V' == 86) && ('W' == 87) && ('X' == 88) && ('Y' == 89) \
      && ('Z' == 90) && ('[' == 91) && ('\\' == 92) && (']' == 93) \
      && ('^' == 94) && ('_' == 95) && ('a' == 97) && ('b' == 98) \
      && ('c' == 99) && ('d' == 100) && ('e' == 101) && ('f' == 102) \
      && ('g' == 103) && ('h' == 104) && ('i' == 105) && ('j' == 106) \
      && ('k' == 107) && ('l' == 108) && ('m' == 109) && ('n' == 110) \
      && ('o' == 111) && ('p' == 112) && ('q' == 113) && ('r' == 114) \
      && ('s' == 115) && ('t' == 116) && ('u' == 117) && ('v' == 118) \
      && ('w' == 119) && ('x' == 120) && ('y' == 121) && ('z' == 122) \
      && ('{' == 123) && ('|' == 124) && ('}' == 125) && ('~' == 126))
/* The character set is not based on ISO-646.  */
error "gperf generated tables don't work with this execution character set. Please report a bug to <bug-gnu-gperf@gnu.org>."
#endif

#line 23 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"

#include <string.h>
#include <stdbool.h>
typedef struct BifNameAndPtr BifNameAndPtr;
#line 28 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
struct BifNameAndPtr
{
  const char *name;
  union
  {
    struct Bif bif;
    struct GCBif gcbif;
  };
};

#define TOTAL_KEYWORDS 71
#define MIN_WORD_LENGTH 10
#define MAX_WORD_LENGTH 32
#define MIN_HASH_VALUE 10
#define MAX_HASH_VALUE 148
/* maximum key range = 139, duplicates = 0 */

#ifdef __GNUC__
__inline
#else
#ifdef __cplusplus
inline
#endif
#endif
static unsigned int
hash (str, len)
     register const char *str;
     register unsigned int len;
{
  static const unsigned char asso_values[] =
    {
      149, 149, 149, 149, 149, 149, 149, 149, 149, 149,
      149, 149, 149, 149, 149, 149, 149, 149, 149, 149,
      149, 149, 149, 149, 149, 149, 149, 149, 149, 149,
      149, 149, 149, 149, 149, 149, 149, 149, 149, 149,
      149, 149,  10,  70, 149,   5, 149,   0,   0,   5,
        0,   0, 149, 149, 149, 149, 149, 149,  20, 149,
       30,  40,  20, 149, 149, 149, 149, 149, 149, 149,
      149, 149, 149, 149, 149, 149, 149, 149, 149, 149,
      149, 149, 149, 149, 149, 149, 149, 149, 149, 149,
      149, 149, 149, 149, 149,  10, 149,  25,   0,  40,
       40,  45,  10,  40,  25,   5, 149, 149,   0,  30,
       10,  40,  35,  55,  10,   5,   0,   0,  50, 149,
       15,  35,  10, 149, 149, 149, 149, 149, 149, 149,
      149, 149, 149, 149, 149, 149, 149, 149, 149, 149,
      149, 149, 149, 149, 149, 149, 149, 149, 149, 149,
      149, 149, 149, 149, 149, 149, 149, 149, 149, 149,
      149, 149, 149, 149, 149, 149, 149, 149, 149, 149,
      149, 149, 149, 149, 149, 149, 149, 149, 149, 149,
      149, 149, 149, 149, 149, 149, 149, 149, 149, 149,
      149, 149, 149, 149, 149, 149, 149, 149, 149, 149,
      149, 149, 149, 149, 149, 149, 149, 149, 149, 149,
      149, 149, 149, 149, 149, 149, 149, 149, 149, 149,
      149, 149, 149, 149, 149, 149, 149, 149, 149, 149,
      149, 149, 149, 149, 149, 149, 149, 149, 149, 149,
      149, 149, 149, 149, 149, 149, 149, 149, 149, 149,
      149, 149, 149, 149, 149, 149
    };
  register unsigned int hval = len;

  switch (hval)
    {
      default:
        hval += asso_values[(unsigned char)str[10]];
      /*FALLTHROUGH*/
      case 10:
        hval += asso_values[(unsigned char)str[9]];
      /*FALLTHROUGH*/
      case 9:
        hval += asso_values[(unsigned char)str[8]];
      /*FALLTHROUGH*/
      case 8:
        hval += asso_values[(unsigned char)str[7]];
        break;
    }
  return hval + asso_values[(unsigned char)str[len - 1]];
}

const struct BifNameAndPtr *
in_word_set (str, len)
     register const char *str;
     register unsigned int len;
{
  static const struct BifNameAndPtr wordlist[] =
    {
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 77 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang://2", {.gcbif.base.type = GCBIFFunctionType, .gcbif.gcbif2_ptr = bif_erlang_fdiv_2}},
      {""}, {""}, {""}, {""},
#line 75 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:-/2", {.gcbif.base.type = GCBIFFunctionType, .gcbif.gcbif2_ptr = bif_erlang_sub_2}},
      {""},
#line 91 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:bsl/2", {.gcbif.base.type = GCBIFFunctionType, .gcbif.gcbif2_ptr = bif_erlang_bsl_2}},
      {""}, {""},
#line 76 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:*/2", {.gcbif.base.type = GCBIFFunctionType, .gcbif.gcbif2_ptr = bif_erlang_mul_2}},
#line 95 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:tl/1", {.bif.base.type = BIFFunctionType, .bif.bif1_ptr = bif_erlang_tl_1}},
      {""}, {""}, {""},
#line 80 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:-/1", {.gcbif.base.type = GCBIFFunctionType, .gcbif.gcbif1_ptr = bif_erlang_neg_1}},
      {""},
#line 92 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:bsr/2", {.gcbif.base.type = GCBIFFunctionType, .gcbif.gcbif2_ptr = bif_erlang_bsr_2}},
      {""}, {""},
#line 70 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:>/2", {.bif.base.type = BIFFunctionType, .bif.bif2_ptr = bif_erlang_greater_than_2}},
      {""}, {""}, {""}, {""}, {""},
#line 105 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:list_to_atom/1", {.gcbif.base.type = GCBIFFunctionType, .gcbif.gcbif1_ptr = bif_erlang_list_to_atom_1}},
#line 42 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:bit_size/1", {.gcbif.base.type = GCBIFFunctionType, .gcbif.gcbif1_ptr = bif_erlang_bit_size_1}},
      {""},
#line 86 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:trunc/1", {.gcbif.base.type = GCBIFFunctionType, .gcbif.gcbif1_ptr = bif_erlang_trunc_1}},
#line 71 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:</2", {.bif.base.type = BIFFunctionType, .bif.bif2_ptr = bif_erlang_less_than_2}},
#line 53 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:is_list/1", {.bif.base.type = BIFFunctionType, .bif.bif1_ptr = bif_erlang_is_list_1}},
#line 58 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:is_tuple/1", {.bif.base.type = BIFFunctionType, .bif.bif1_ptr = bif_erlang_is_tuple_1}},
#line 47 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:is_binary/1", {.bif.base.type = BIFFunctionType, .bif.bif1_ptr = bif_erlang_is_binary_1}},
#line 48 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:is_boolean/1", {.bif.base.type = BIFFunctionType, .bif.bif1_ptr = bif_erlang_is_boolean_1}},
#line 106 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:list_to_existing_atom/1", {.gcbif.base.type = GCBIFFunctionType, .gcbif.gcbif1_ptr = bif_erlang_list_to_existing_atom_1}},
#line 46 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:is_bitstring/1", {.bif.base.type = BIFFunctionType, .bif.bif1_ptr = bif_erlang_is_binary_1}},
#line 82 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:abs/1", {.gcbif.base.type = GCBIFFunctionType, .gcbif.gcbif1_ptr = bif_erlang_abs_1}},
#line 59 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:is_record/2",{.bif.base.type = BIFFunctionType, .bif.bif2_ptr = bif_erlang_is_record_2}},
#line 52 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:is_integer/1", {.bif.base.type = BIFFunctionType, .bif.bif1_ptr = bif_erlang_is_integer_1}},
#line 51 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:is_function/2", {.bif.base.type = BIFFunctionType, .bif.bif2_ptr = bif_erlang_is_function_2}},
#line 67 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:/=/2", {.bif.base.type = BIFFunctionType, .bif.bif2_ptr = bif_erlang_not_equal_to_2}},
#line 49 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:is_float/1", {.bif.base.type = BIFFunctionType, .bif.bif1_ptr = bif_erlang_is_float_1}},
#line 54 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:is_number/1", {.bif.base.type = BIFFunctionType, .bif.bif1_ptr = bif_erlang_is_number_1}},
      {""},
#line 50 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:is_function/1", {.bif.base.type = BIFFunctionType, .bif.bif1_ptr = bif_erlang_is_function_1}},
#line 57 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:is_reference/1", {.bif.base.type = BIFFunctionType, .bif.bif1_ptr = bif_erlang_is_reference_1}},
#line 102 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:min/2", {.bif.base.type = BIFFunctionType, .bif.bif2_ptr = bif_erlang_min_2}},
      {""},
#line 97 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:tuple_size/1", {.bif.base.type = BIFFunctionType, .bif.bif1_ptr = bif_erlang_tuple_size_1}},
#line 43 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:binary_part/3", {.gcbif.base.type = GCBIFFunctionType, .gcbif.gcbif3_ptr = bif_erlang_binary_part_3}},
#line 64 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:or/2", {.bif.base.type = BIFFunctionType, .bif.bif2_ptr = bif_erlang_or_2}},
#line 88 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:bor/2", {.gcbif.base.type = GCBIFFunctionType, .gcbif.gcbif2_ptr = bif_erlang_bor_2}},
#line 107 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:binary_to_atom/2", {.gcbif.base.type = GCBIFFunctionType, .gcbif.gcbif2_ptr = bif_erlang_binary_to_atom_2}},
      {""}, {""},
#line 45 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:is_atom/1", {.bif.base.type = BIFFunctionType, .bif.bif1_ptr = bif_erlang_is_atom_1}},
#line 62 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:not/1", {.bif.base.type = BIFFunctionType, .bif.bif1_ptr = bif_erlang_not_1}},
#line 93 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:bnot/1", {.gcbif.base.type = GCBIFFunctionType, .gcbif.gcbif1_ptr = bif_erlang_bnot_1}},
#line 61 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:is_map_key/2", {.bif.base.type = BIFFunctionType, .bif.bif2_ptr = bif_erlang_is_map_key_2}},
#line 60 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:is_map/1", {.bif.base.type = BIFFunctionType, .bif.bif1_ptr = bif_erlang_is_map_1}},
#line 73 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:>=/2", {.bif.base.type = BIFFunctionType, .bif.bif2_ptr = bif_erlang_greater_than_or_equal_2}},
#line 108 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:binary_to_existing_atom/2", {.gcbif.base.type = GCBIFFunctionType, .gcbif.gcbif2_ptr = bif_erlang_binary_to_existing_atom_2}},
#line 38 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:self/0", {.bif.base.type = BIFFunctionType, .bif.bif0_ptr = bif_erlang_self_0}},
      {""},
#line 55 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:is_pid/1", {.bif.base.type = BIFFunctionType, .bif.bif1_ptr = bif_erlang_is_pid_1}},
#line 56 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:is_port/1", {.bif.base.type = BIFFunctionType, .bif.bif1_ptr = bif_erlang_is_port_1}},
#line 65 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:xor/2", {.bif.base.type = BIFFunctionType, .bif.bif2_ptr = bif_erlang_xor_2}},
#line 90 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:bxor/2", {.gcbif.base.type = GCBIFFunctionType, .gcbif.gcbif2_ptr = bif_erlang_bxor_2}},
#line 85 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:round/1", {.gcbif.base.type = GCBIFFunctionType, .gcbif.gcbif1_ptr = bif_erlang_round_1}},
#line 74 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:+/2", {.gcbif.base.type = GCBIFFunctionType, .gcbif.gcbif2_ptr = bif_erlang_add_2}},
#line 72 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:=</2", {.bif.base.type = BIFFunctionType, .bif.bif2_ptr = bif_erlang_less_than_or_equal_2}},
#line 103 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:max/2", {.bif.base.type = BIFFunctionType, .bif.bif2_ptr = bif_erlang_max_2}},
#line 104 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:size/1", {.gcbif.base.type = GCBIFFunctionType, .gcbif.gcbif1_ptr = bif_erlang_size_1}},
      {""}, {""},
#line 94 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:hd/1", {.bif.base.type = BIFFunctionType, .bif.bif1_ptr = bif_erlang_hd_1}},
#line 63 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:and/2", {.bif.base.type = BIFFunctionType, .bif.bif2_ptr = bif_erlang_and_2}},
#line 89 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:band/2", {.gcbif.base.type = GCBIFFunctionType, .gcbif.gcbif2_ptr = bif_erlang_band_2}},
      {""},
#line 81 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:+/1", {.gcbif.base.type = GCBIFFunctionType, .gcbif.gcbif1_ptr = bif_erlang_plus_1}},
#line 66 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:==/2", {.bif.base.type = BIFFunctionType, .bif.bif2_ptr = bif_erlang_equal_to_2}},
#line 69 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:=/=/2", {.bif.base.type = BIFFunctionType, .bif.bif2_ptr = bif_erlang_exactly_not_equal_to_2}},
#line 100 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:unique_integer/0", {.bif.base.type = BIFFunctionType, .bif.bif0_ptr = bif_erlang_unique_integer_0}},
#line 87 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:float/1", {.gcbif.base.type = GCBIFFunctionType, .gcbif.gcbif1_ptr = bif_erlang_float_1}},
      {""}, {""},
#line 79 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:rem/2", {.gcbif.base.type = GCBIFFunctionType, .gcbif.gcbif2_ptr = bif_erlang_rem_2}},
#line 101 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:unique_integer/1", {.bif.base.type = BIFFunctionType, .bif.bif1_ptr = bif_erlang_unique_integer_1}},
      {""}, {""}, {""},
#line 44 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:get/1", {.bif.base.type = BIFFunctionType, .bif.bif1_ptr = bif_erlang_get_1}},
#line 41 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:byte_size/1", {.gcbif.base.type = GCBIFFunctionType, .gcbif.gcbif1_ptr = bif_erlang_byte_size_1}},
      {""}, {""}, {""},
#line 78 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:div/2", {.gcbif.base.type = GCBIFFunctionType, .gcbif.gcbif2_ptr = bif_erlang_div_2}},
#line 83 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:ceil/1", {.gcbif.base.type = GCBIFFunctionType, .gcbif.gcbif1_ptr = bif_erlang_ceil_1}},
#line 84 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:floor/1", {.gcbif.base.type = GCBIFFunctionType, .gcbif.gcbif1_ptr = bif_erlang_floor_1}},
      {""}, {""},
#line 68 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:=:=/2", {.bif.base.type = BIFFunctionType, .bif.bif2_ptr = bif_erlang_exactly_equal_to_2}},
      {""}, {""},
#line 40 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:length/1", {.gcbif.base.type = GCBIFFunctionType, .gcbif.gcbif1_ptr = bif_erlang_length_1}},
#line 99 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:map_get/2", {.bif.base.type = BIFFunctionType, .bif.bif2_ptr = bif_erlang_map_get_2}},
      {""}, {""}, {""}, {""}, {""},
#line 98 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:map_size/1", {.gcbif.base.type = GCBIFFunctionType, .gcbif.gcbif1_ptr = bif_erlang_map_size_1}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""},
#line 96 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:element/2", {.bif.base.type = BIFFunctionType, .bif.bif2_ptr = bif_erlang_element_2}},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 39 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/bifs.gperf"
      {"erlang:node/0", {.bif.base.type = BIFFunctionType, .bif.bif0_ptr = bif_erlang_node_0}}
    };

  if (len <= MAX_WORD_LENGTH && len >= MIN_WORD_LENGTH)
    {
      unsigned int key = hash (str, len);

      if (key <= MAX_HASH_VALUE)
        {
          register const char *s = wordlist[key].name;

          if (*str == *s && !strcmp (str + 1, s + 1))
            return &wordlist[key];
        }
    }
  return 0;
}
