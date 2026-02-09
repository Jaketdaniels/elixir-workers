/* C code produced by gperf version 3.0.3 */
/* Command-line: /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/gperf -t --output-file=/Users/main/elixir-workers/atomvm-wasi/include/nifs_hash.h /Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf  */
/* Computed positions: -k'7-8,10,16,18,$' */

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

#line 24 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"

#include <string.h>
typedef struct NifNameAndNifPtr NifNameAndNifPtr;
#line 28 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
struct NifNameAndNifPtr
{
  const char *name;
  const struct Nif *nif;
};

#define TOTAL_KEYWORDS 196
#define MIN_WORD_LENGTH 9
#define MAX_WORD_LENGTH 40
#define MIN_HASH_VALUE 20
#define MAX_HASH_VALUE 715
/* maximum key range = 696, duplicates = 0 */

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
  static const unsigned short asso_values[] =
    {
      716, 716, 716, 716, 716, 716, 716, 716, 716, 716,
      716, 716, 716, 716, 716, 716, 716, 716, 716, 716,
      716, 716, 716, 716, 716, 716, 716, 716, 716, 716,
      716, 716, 716,   0, 716, 716, 716, 716, 716, 716,
      716, 716, 716,  30, 716,  20, 716,  70,  90,   5,
        0,  25,  10,  50, 716, 716, 716, 716,  10, 716,
      716, 716, 716, 716, 716, 716, 716, 716, 716, 716,
      716, 716, 716, 716, 716, 716, 716, 716, 716, 716,
      716, 716, 716, 716, 716, 716, 716, 716, 716, 716,
      716, 716, 716, 716, 716,   5, 135,  45,  25, 215,
       20,   5,  90,  85,  15, 155, 155,  90,   0,  45,
      100,  30,  20,  65,   0,   0,  15, 110,  35,  20,
       15,   5, 716, 716, 716, 716, 716, 716, 716, 716,
      716, 716, 716, 716, 716, 716, 716, 716, 716, 716,
      716, 716, 716, 716, 716, 716, 716, 716, 716, 716,
      716, 716, 716, 716, 716, 716, 716, 716, 716, 716,
      716, 716, 716, 716, 716, 716, 716, 716, 716, 716,
      716, 716, 716, 716, 716, 716, 716, 716, 716, 716,
      716, 716, 716, 716, 716, 716, 716, 716, 716, 716,
      716, 716, 716, 716, 716, 716, 716, 716, 716, 716,
      716, 716, 716, 716, 716, 716, 716, 716, 716, 716,
      716, 716, 716, 716, 716, 716, 716, 716, 716, 716,
      716, 716, 716, 716, 716, 716, 716, 716, 716, 716,
      716, 716, 716, 716, 716, 716, 716, 716, 716, 716,
      716, 716, 716, 716, 716, 716, 716, 716, 716, 716,
      716, 716, 716, 716, 716, 716, 716
    };
  register unsigned int hval = len;

  switch (hval)
    {
      default:
        hval += asso_values[(unsigned char)str[17]+1];
      /*FALLTHROUGH*/
      case 17:
      case 16:
        hval += asso_values[(unsigned char)str[15]];
      /*FALLTHROUGH*/
      case 15:
      case 14:
      case 13:
      case 12:
      case 11:
      case 10:
        hval += asso_values[(unsigned char)str[9]];
      /*FALLTHROUGH*/
      case 9:
      case 8:
        hval += asso_values[(unsigned char)str[7]];
      /*FALLTHROUGH*/
      case 7:
        hval += asso_values[(unsigned char)str[6]];
        break;
    }
  return hval + asso_values[(unsigned char)str[len - 1]];
}

const struct NifNameAndNifPtr *
nif_in_word_set (str, len)
     register const char *str;
     register unsigned int len;
{
  static const struct NifNameAndNifPtr wordlist[] =
    {
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 129 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:!/2", &send_nif},
      {""},
#line 151 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"ets:delete/2", &ets_delete_nif},
      {""},
#line 40 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"binary:split/2", &binary_split_nif},
#line 199 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"lists:reverse/2", &lists_reverse_nif},
      {""},
#line 149 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"ets:delete/1", &ets_delete_nif},
#line 38 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"binary:last/1", &binary_last_nif},
#line 61 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:error/2", &error_nif},
#line 198 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"lists:reverse/1", &lists_reverse_nif},
      {""},
#line 146 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"ets:insert/2", &ets_insert_nif},
      {""},
#line 60 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:error/1", &error_nif},
#line 187 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"console:print/1", &console_print_nif},
      {""}, {""}, {""}, {""}, {""},
#line 136 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:setnode/2", &setnode_2_nif},
#line 185 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"code_server:type_resolver/2", IF_HAVE_JIT(&code_server_type_resolver_nif)},
      {""},
#line 121 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:throw/1", &throw_nif},
#line 100 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:system_flag/2", &system_flag_nif},
      {""},
#line 218 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"math:floor/1", &math_floor_nif},
      {""},
#line 41 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"binary:split/3", &binary_split_nif},
#line 215 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"math:cos/1", &math_cos_nif},
#line 200 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"maps:from_keys/2", &maps_from_keys_nif},
      {""}, {""},
#line 62 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:error/3", &error_nif},
#line 217 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"math:exp/1", &math_exp_nif},
#line 65 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:display/1", &display_nif},
#line 116 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:put/2", &put_nif},
      {""}, {""},
#line 223 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"math:pow/2", &math_pow_nif},
#line 101 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:whereis/1", &whereis_nif},
      {""},
#line 180 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"code_server:is_loaded/1", &code_server_is_loaded_nif},
#line 182 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"code_server:code_chunk/1", IF_HAVE_JIT(&code_server_code_chunk_nif)},
      {""},
#line 43 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"binary:replace/4", &binary_replace_nif},
#line 171 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"atomvm:posix_opendir/1", IF_HAVE_OPENDIR_READDIR_CLOSEDIR(&atomvm_posix_opendir_nif)},
#line 39 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"binary:part/3", &binary_part_nif},
      {""},
#line 184 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"code_server:literal_resolver/2", IF_HAVE_JIT(&code_server_literal_resolver_nif)},
#line 125 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:port_to_list/1", &port_to_list_nif},
#line 213 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"math:atan2/2", &math_atan2_nif},
      {""}, {""}, {""}, {""},
#line 139 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:dist_ctrl_get_data/1", &dist_ctrl_get_data_nif},
#line 203 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"unicode:characters_to_list/2", &unicode_characters_to_list_nif},
#line 59 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:erase/1", &erase_1_nif},
#line 205 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"unicode:characters_to_binary/2", &unicode_characters_to_binary_nif},
#line 166 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"atomvm:posix_select_stop/1", IF_HAVE_OPEN_CLOSE(&atomvm_posix_select_stop_nif)},
#line 183 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"code_server:atom_resolver/2", IF_HAVE_JIT(&code_server_atom_resolver_nif)},
#line 202 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"unicode:characters_to_list/1", &unicode_characters_to_list_nif},
#line 44 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"binary:match/2", &binary_match_nif},
#line 204 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"unicode:characters_to_binary/1", &unicode_characters_to_binary_nif},
#line 169 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"atomvm:posix_unlink/1", IF_HAVE_UNLINK(&atomvm_posix_unlink_nif)},
      {""},
#line 170 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"atomvm:posix_clock_settime/2", IF_HAVE_CLOCK_SETTIME_OR_SETTIMEOFDAY(&atomvm_posix_clock_settime_nif)},
#line 195 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"lists:member/2", &lists_member_nif},
#line 138 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:dist_ctrl_get_data_notification/1", &dist_ctrl_get_data_notification_nif},
#line 137 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:setnode/3", &setnode_3_nif},
#line 212 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"math:atanh/1", &math_atanh_nif},
#line 77 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:list_to_binary/1", &list_to_binary_nif},
#line 186 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"code_server:set_native_code/3", IF_HAVE_JIT(&code_server_set_native_code_nif)},
#line 229 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"zlib:compress/1", &zlib_compress_nif},
#line 42 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"binary:replace/3", &binary_replace_nif},
      {""},
#line 90 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:demonitor/1", &demonitor_nif},
#line 145 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"ets:new/2", &ets_new_nif},
#line 175 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"code:load_abs/1", &code_load_abs_nif},
#line 164 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"atomvm:posix_select_read/3", IF_HAVE_OPEN_CLOSE(&atomvm_posix_select_read_nif)},
#line 165 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"atomvm:posix_select_write/3", IF_HAVE_OPEN_CLOSE(&atomvm_posix_select_write_nif)},
#line 84 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:open_port/2", &open_port_nif},
      {""},
#line 206 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"unicode:characters_to_binary/3", &unicode_characters_to_binary_nif},
#line 201 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"maps:next/1", &maps_next_nif},
#line 173 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"atomvm:posix_readdir/1", IF_HAVE_OPENDIR_READDIR_CLOSEDIR(&atomvm_posix_readdir_nif)},
#line 119 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:term_to_binary/1", &term_to_binary_nif},
#line 45 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"binary:match/3", &binary_match_nif},
      {""},
#line 103 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:--/2", &erlang_lists_subtract_nif},
      {""},
#line 97 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:spawn_opt/2", &spawn_fun_opt_nif},
      {""},
#line 153 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"ets:update_counter/4", &ets_update_counter_nif},
#line 216 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"math:cosh/1", &math_cosh_nif},
#line 81 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:list_to_tuple/1", &list_to_tuple_nif},
#line 91 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:demonitor/2", &demonitor_nif},
#line 37 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"binary:first/1", &binary_first_nif},
#line 88 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:memory/1", &memory_nif},
#line 102 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:++/2", &concat_nif},
#line 140 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:dist_ctrl_put_data/2", &dist_ctrl_put_data_nif},
#line 95 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:send/2", &send_nif},
      {""},
#line 192 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"lists:flatten/1", &lists_flatten_nif},
#line 50 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:atom_to_list/1", &atom_to_list_nif},
      {""},
#line 76 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:link/1", &link_nif},
      {""},
#line 152 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"ets:update_counter/3", &ets_update_counter_nif},
#line 120 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:split_binary/2", &split_binary_nif},
      {""},
#line 158 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"atomvm:read_priv/2", &atomvm_read_priv_nif},
      {""},
#line 220 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"math:log/1", &math_log_nif},
#line 34 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"binary:at/2", &binary_at_nif},
#line 141 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:module_loaded/1",&module_loaded_nif},
#line 176 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"code:load_binary/3", &code_load_binary_nif},
      {""},
#line 123 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:unlink/1", &unlink_nif},
#line 155 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"atomvm:add_avm_pack_file/2", &atomvm_add_avm_pack_file_nif},
      {""},
#line 154 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"atomvm:add_avm_pack_binary/2", &atomvm_add_avm_pack_binary_nif},
#line 159 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"atomvm:posix_open/2", IF_HAVE_OPEN_CLOSE(&atomvm_posix_open_nif)},
      {""},
#line 211 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"math:atan/1", &math_atan_nif},
#line 80 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:list_to_float/1", &list_to_float_nif},
#line 98 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:spawn_opt/4", &spawn_opt_nif},
      {""},
#line 179 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"code:ensure_loaded/1", &code_ensure_loaded_nif},
#line 226 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"math:sqrt/1", &math_sqrt_nif},
#line 147 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"ets:lookup/2", &ets_lookup_nif},
#line 57 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:delete_element/2", &delete_element_nif},
#line 167 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"atomvm:subprocess/4", IF_HAVE_EXECVE(&atomvm_subprocess_nif)},
#line 99 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:system_info/1", &system_info_nif},
#line 219 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"math:fmod/2", &math_fmod_nif},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 49 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:atom_to_binary/2", &atom_to_binary_nif},
#line 58 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:erase/0", &erase_0_nif},
#line 227 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"math:tan/1", &math_tan_nif},
      {""}, {""},
#line 48 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:atom_to_binary/1", &atom_to_binary_nif},
#line 160 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"atomvm:posix_open/3", IF_HAVE_OPEN_CLOSE(&atomvm_posix_open_nif)},
      {""},
#line 89 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:monitor/2", &monitor_nif},
      {""},
#line 113 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:processes/0", &processes_nif},
      {""},
#line 161 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"atomvm:posix_close/1", IF_HAVE_OPEN_CLOSE(&atomvm_posix_close_nif)},
#line 143 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:list_to_bitstring/1", &list_to_bitstring_nif},
#line 178 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"code:all_loaded/0", &code_all_loaded_nif},
#line 172 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"atomvm:posix_closedir/1", IF_HAVE_OPENDIR_READDIR_CLOSEDIR(&atomvm_posix_closedir_nif)},
      {""}, {""}, {""},
#line 93 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:register/2", &register_nif},
#line 64 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:exit/2", &exit_nif},
#line 162 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"atomvm:posix_read/2", IF_HAVE_OPEN_CLOSE(&atomvm_posix_read_nif)},
      {""}, {""},
#line 210 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"math:asinh/1", &math_asinh_nif},
#line 63 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:exit/1", &exit_nif},
      {""}, {""},
#line 47 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"os:getenv/1", &os_getenv_nif},
      {""}, {""}, {""}, {""},
#line 111 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:process_flag/2", &process_flag_nif},
      {""}, {""}, {""}, {""},
#line 222 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"math:log2/1", &math_log2_nif},
      {""},
#line 110 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:timestamp/0", &timestamp_nif},
#line 122 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:raise/3", &raise_nif},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 115 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:get/0", &get_0_nif},
      {""}, {""}, {""}, {""}, {""},
#line 51 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:binary_to_atom/1", &binary_to_atom_1_nif},
      {""}, {""},
#line 112 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:process_flag/3", &process_flag_nif},
#line 221 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"math:log10/1", &math_log10_nif},
#line 142 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:nif_error/1",&nif_error_nif},
      {""},
#line 193 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"lists:keyfind/3", &lists_keyfind_nif},
#line 168 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"atomvm:posix_mkfifo/2", IF_HAVE_MKFIFO(&atomvm_posix_mkfifo_nif)},
      {""}, {""}, {""},
#line 181 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"code_server:resume/2", &code_server_resume_nif},
#line 228 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"math:tanh/1", &math_tanh_nif},
#line 106 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:tuple_to_list/1", &tuple_to_list_nif},
      {""}, {""}, {""}, {""}, {""},
#line 55 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:binary_to_list/1", &binary_to_list_nif},
#line 94 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:unregister/1", &unregister_nif},
      {""},
#line 209 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"math:asin/1", &math_asin_nif},
      {""}, {""}, {""},
#line 46 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"calendar:system_time_to_universal_time/2", &system_time_to_universal_time_nif},
#line 214 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"math:ceil/1", &math_ceil_nif},
      {""}, {""}, {""},
#line 188 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"base64:encode/1", &base64_encode_nif},
      {""},
#line 194 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"lists:keymember/3", &lists_keymember_nif},
#line 109 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:localtime/1", &localtime_nif},
#line 87 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:make_tuple/2", &make_tuple_nif},
      {""},
#line 174 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"atomvm:get_creation/0", &atomvm_get_creation_nif},
#line 85 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:make_fun/3", &make_fun_nif},
#line 36 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"binary:copy/2", &binary_copy_nif},
#line 96 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:setelement/3", &setelement_nif},
      {""},
#line 128 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:function_exported/3", &function_exported_nif},
      {""},
#line 35 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"binary:copy/1", &binary_copy_nif},
      {""},
#line 189 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"base64:decode/1", &base64_decode_nif},
      {""}, {""}, {""},
#line 135 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:get_module_info/2", &get_module_info_nif},
#line 177 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"code:all_available/0", &code_all_available_nif},
      {""}, {""}, {""},
#line 134 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:get_module_info/1", &get_module_info_nif},
#line 224 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"math:sin/1", &math_sin_nif},
      {""},
#line 208 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"math:acosh/1", &math_acosh_nif},
      {""},
#line 52 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:binary_to_float/1", &binary_to_float_nif},
#line 105 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:system_time/1", &system_time_nif},
#line 133 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:group_leader/2", &group_leader_nif},
      {""}, {""}, {""}, {""}, {""},
#line 70 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:fun_info/2", &fun_info_nif},
      {""}, {""}, {""}, {""},
#line 56 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:binary_to_existing_atom/1", &binary_to_existing_atom_1_nif},
      {""}, {""},
#line 163 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"atomvm:posix_write/2", IF_HAVE_OPEN_CLOSE(&atomvm_posix_write_nif)},
      {""}, {""},
#line 118 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:binary_to_term/2", &binary_to_term_nif},
#line 79 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:list_to_integer/2", &list_to_integer_nif},
#line 148 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"ets:lookup_element/3", &ets_lookup_element_nif},
      {""},
#line 144 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erts_debug:flat_size/1", &flat_size_nif},
#line 117 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:binary_to_term/1", &binary_to_term_nif},
#line 78 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:list_to_integer/1", &list_to_integer_nif},
#line 190 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"base64:encode_to_string/1", &base64_encode_to_string_nif},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 69 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:float_to_list/2", &float_to_list_nif},
#line 71 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:insert_element/3", &insert_element_nif},
#line 67 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:float_to_binary/2", &float_to_binary_nif},
      {""}, {""},
#line 68 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:float_to_list/1", &float_to_list_nif},
      {""},
#line 66 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:float_to_binary/1", &float_to_binary_nif},
#line 191 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"base64:decode_to_string/1", &base64_decode_to_string_nif},
#line 114 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:process_info/2", &process_info_nif},
#line 86 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:make_ref/0", &make_ref_nif},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 207 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"math:acos/1", &math_acos_nif},
      {""}, {""}, {""},
#line 92 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:is_process_alive/1", &is_process_alive_nif},
      {""}, {""}, {""},
#line 150 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"file:get_cwd/0", IF_HAVE_GETCWD_PATHMAX(&file_get_cwd_nif)},
#line 124 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:pid_to_list/1", &pid_to_list_nif},
#line 225 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"math:sinh/1", &math_sinh_nif},
      {""},
#line 108 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:localtime/0", &localtime_nif},
      {""}, {""},
#line 54 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:binary_to_integer/2", &binary_to_integer_nif},
      {""}, {""}, {""}, {""},
#line 53 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:binary_to_integer/1", &binary_to_integer_nif},
      {""}, {""},
#line 75 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:integer_to_list/2", &integer_to_list_nif},
      {""},
#line 73 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:integer_to_binary/2", &integer_to_binary_nif},
      {""},
#line 197 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"jit:variant/0", IF_HAVE_JIT(&jit_variant_nif)},
#line 74 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:integer_to_list/1", &integer_to_list_nif},
      {""},
#line 72 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:integer_to_binary/1", &integer_to_binary_nif},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 157 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"atomvm:get_start_beam/1", &atomvm_get_start_beam_nif},
      {""}, {""},
#line 132 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:group_leader/0", &group_leader_nif},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""},
#line 131 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:garbage_collect/1", &garbage_collect_nif},
      {""}, {""}, {""},
#line 156 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"atomvm:close_avm_pack/2", &atomvm_close_avm_pack_nif},
      {""},
#line 126 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:ref_to_list/1", &ref_to_list_nif},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 82 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:iolist_size/1", &iolist_size_nif},
      {""}, {""}, {""}, {""},
#line 83 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:iolist_to_binary/1", &iolist_to_binary_nif},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 130 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:garbage_collect/0", &garbage_collect_nif},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""},
#line 127 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:fun_to_list/1", &fun_to_list_nif},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
#line 104 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:monotonic_time/1", &monotonic_time_nif},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""},
#line 107 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"erlang:universaltime/0", &universaltime_nif},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {""},
#line 196 "/Users/main/elixir-workers/vendor/AtomVM/src/libAtomVM/nifs.gperf"
      {"jit:backend_module/0", IF_HAVE_JIT(&jit_backend_module_nif)}
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
