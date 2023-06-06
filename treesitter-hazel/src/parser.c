#include <tree_sitter/parser.h>

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#define LANGUAGE_VERSION 14
#define STATE_COUNT 57
#define LARGE_STATE_COUNT 2
#define SYMBOL_COUNT 62
#define ALIAS_COUNT 0
#define TOKEN_COUNT 47
#define EXTERNAL_TOKEN_COUNT 0
#define FIELD_COUNT 0
#define MAX_ALIAS_SEQUENCE_LENGTH 6
#define PRODUCTION_ID_COUNT 1

enum {
  sym_ident = 1,
  anon_sym_Int = 2,
  anon_sym_String = 3,
  anon_sym_Float = 4,
  anon_sym_Bool = 5,
  anon_sym_LPAREN = 6,
  anon_sym_COMMA = 7,
  anon_sym_RPAREN = 8,
  anon_sym_DASH_GT = 9,
  sym_int_lit = 10,
  sym_float_lit = 11,
  anon_sym_true = 12,
  anon_sym_false = 13,
  anon_sym_DQUOTE = 14,
  aux_sym_string_token1 = 15,
  aux_sym_string_token2 = 16,
  anon_sym_PLUS = 17,
  anon_sym_DASH = 18,
  anon_sym_STAR = 19,
  anon_sym_SLASH = 20,
  anon_sym_STAR_STAR = 21,
  anon_sym_STAR_STAR_DOT = 22,
  anon_sym_EQ = 23,
  anon_sym_EQ_EQ = 24,
  anon_sym_DOLLAR_EQ_EQ = 25,
  anon_sym_DOLLAR_EQ = 26,
  anon_sym_DOLLAR = 27,
  anon_sym_LT = 28,
  anon_sym_GT = 29,
  anon_sym_GT_EQ = 30,
  anon_sym_LT_EQ = 31,
  anon_sym_PLUS_DOT = 32,
  anon_sym_DASH_DOT = 33,
  anon_sym_STAR_DOT = 34,
  anon_sym_SLASH_DOT = 35,
  anon_sym_EQ_EQ_DOT = 36,
  anon_sym_GT_DOT = 37,
  anon_sym_LT_DOT = 38,
  anon_sym_GT_EQ_DOT = 39,
  anon_sym_LT_EQ_DOT = 40,
  anon_sym_EQ_DOT = 41,
  anon_sym_AMP = 42,
  anon_sym_AMP_AMP = 43,
  anon_sym_let = 44,
  anon_sym_in = 45,
  anon_sym_COLON = 46,
  sym_program = 47,
  sym__expression = 48,
  sym__pat = 49,
  sym__type = 50,
  sym_tuple_type = 51,
  sym_arrow_type = 52,
  sym_bool_lit = 53,
  sym_string = 54,
  sym_var = 55,
  sym_tuple_exp = 56,
  sym_let = 57,
  sym_typeann = 58,
  aux_sym_tuple_type_repeat1 = 59,
  aux_sym_string_repeat1 = 60,
  aux_sym_tuple_exp_repeat1 = 61,
};

static const char * const ts_symbol_names[] = {
  [ts_builtin_sym_end] = "end",
  [sym_ident] = "ident",
  [anon_sym_Int] = "Int",
  [anon_sym_String] = "String",
  [anon_sym_Float] = "Float",
  [anon_sym_Bool] = "Bool",
  [anon_sym_LPAREN] = "(",
  [anon_sym_COMMA] = ",",
  [anon_sym_RPAREN] = ")",
  [anon_sym_DASH_GT] = "->",
  [sym_int_lit] = "int_lit",
  [sym_float_lit] = "float_lit",
  [anon_sym_true] = "true",
  [anon_sym_false] = "false",
  [anon_sym_DQUOTE] = "\"",
  [aux_sym_string_token1] = "string_token1",
  [aux_sym_string_token2] = "string_token2",
  [anon_sym_PLUS] = "+",
  [anon_sym_DASH] = "-",
  [anon_sym_STAR] = "*",
  [anon_sym_SLASH] = "/",
  [anon_sym_STAR_STAR] = "**",
  [anon_sym_STAR_STAR_DOT] = "**.",
  [anon_sym_EQ] = "=",
  [anon_sym_EQ_EQ] = "==",
  [anon_sym_DOLLAR_EQ_EQ] = "$==",
  [anon_sym_DOLLAR_EQ] = "$=",
  [anon_sym_DOLLAR] = "$",
  [anon_sym_LT] = "<",
  [anon_sym_GT] = ">",
  [anon_sym_GT_EQ] = ">=",
  [anon_sym_LT_EQ] = "<=",
  [anon_sym_PLUS_DOT] = "+.",
  [anon_sym_DASH_DOT] = "-.",
  [anon_sym_STAR_DOT] = "*.",
  [anon_sym_SLASH_DOT] = "/.",
  [anon_sym_EQ_EQ_DOT] = "==.",
  [anon_sym_GT_DOT] = ">.",
  [anon_sym_LT_DOT] = "<.",
  [anon_sym_GT_EQ_DOT] = ">=.",
  [anon_sym_LT_EQ_DOT] = "<=.",
  [anon_sym_EQ_DOT] = "=.",
  [anon_sym_AMP] = "&",
  [anon_sym_AMP_AMP] = "&&",
  [anon_sym_let] = "let",
  [anon_sym_in] = "in",
  [anon_sym_COLON] = ":",
  [sym_program] = "program",
  [sym__expression] = "_expression",
  [sym__pat] = "_pat",
  [sym__type] = "_type",
  [sym_tuple_type] = "tuple_type",
  [sym_arrow_type] = "arrow_type",
  [sym_bool_lit] = "bool_lit",
  [sym_string] = "string",
  [sym_var] = "var",
  [sym_tuple_exp] = "tuple_exp",
  [sym_let] = "let",
  [sym_typeann] = "typeann",
  [aux_sym_tuple_type_repeat1] = "tuple_type_repeat1",
  [aux_sym_string_repeat1] = "string_repeat1",
  [aux_sym_tuple_exp_repeat1] = "tuple_exp_repeat1",
};

static const TSSymbol ts_symbol_map[] = {
  [ts_builtin_sym_end] = ts_builtin_sym_end,
  [sym_ident] = sym_ident,
  [anon_sym_Int] = anon_sym_Int,
  [anon_sym_String] = anon_sym_String,
  [anon_sym_Float] = anon_sym_Float,
  [anon_sym_Bool] = anon_sym_Bool,
  [anon_sym_LPAREN] = anon_sym_LPAREN,
  [anon_sym_COMMA] = anon_sym_COMMA,
  [anon_sym_RPAREN] = anon_sym_RPAREN,
  [anon_sym_DASH_GT] = anon_sym_DASH_GT,
  [sym_int_lit] = sym_int_lit,
  [sym_float_lit] = sym_float_lit,
  [anon_sym_true] = anon_sym_true,
  [anon_sym_false] = anon_sym_false,
  [anon_sym_DQUOTE] = anon_sym_DQUOTE,
  [aux_sym_string_token1] = aux_sym_string_token1,
  [aux_sym_string_token2] = aux_sym_string_token2,
  [anon_sym_PLUS] = anon_sym_PLUS,
  [anon_sym_DASH] = anon_sym_DASH,
  [anon_sym_STAR] = anon_sym_STAR,
  [anon_sym_SLASH] = anon_sym_SLASH,
  [anon_sym_STAR_STAR] = anon_sym_STAR_STAR,
  [anon_sym_STAR_STAR_DOT] = anon_sym_STAR_STAR_DOT,
  [anon_sym_EQ] = anon_sym_EQ,
  [anon_sym_EQ_EQ] = anon_sym_EQ_EQ,
  [anon_sym_DOLLAR_EQ_EQ] = anon_sym_DOLLAR_EQ_EQ,
  [anon_sym_DOLLAR_EQ] = anon_sym_DOLLAR_EQ,
  [anon_sym_DOLLAR] = anon_sym_DOLLAR,
  [anon_sym_LT] = anon_sym_LT,
  [anon_sym_GT] = anon_sym_GT,
  [anon_sym_GT_EQ] = anon_sym_GT_EQ,
  [anon_sym_LT_EQ] = anon_sym_LT_EQ,
  [anon_sym_PLUS_DOT] = anon_sym_PLUS_DOT,
  [anon_sym_DASH_DOT] = anon_sym_DASH_DOT,
  [anon_sym_STAR_DOT] = anon_sym_STAR_DOT,
  [anon_sym_SLASH_DOT] = anon_sym_SLASH_DOT,
  [anon_sym_EQ_EQ_DOT] = anon_sym_EQ_EQ_DOT,
  [anon_sym_GT_DOT] = anon_sym_GT_DOT,
  [anon_sym_LT_DOT] = anon_sym_LT_DOT,
  [anon_sym_GT_EQ_DOT] = anon_sym_GT_EQ_DOT,
  [anon_sym_LT_EQ_DOT] = anon_sym_LT_EQ_DOT,
  [anon_sym_EQ_DOT] = anon_sym_EQ_DOT,
  [anon_sym_AMP] = anon_sym_AMP,
  [anon_sym_AMP_AMP] = anon_sym_AMP_AMP,
  [anon_sym_let] = anon_sym_let,
  [anon_sym_in] = anon_sym_in,
  [anon_sym_COLON] = anon_sym_COLON,
  [sym_program] = sym_program,
  [sym__expression] = sym__expression,
  [sym__pat] = sym__pat,
  [sym__type] = sym__type,
  [sym_tuple_type] = sym_tuple_type,
  [sym_arrow_type] = sym_arrow_type,
  [sym_bool_lit] = sym_bool_lit,
  [sym_string] = sym_string,
  [sym_var] = sym_var,
  [sym_tuple_exp] = sym_tuple_exp,
  [sym_let] = sym_let,
  [sym_typeann] = sym_typeann,
  [aux_sym_tuple_type_repeat1] = aux_sym_tuple_type_repeat1,
  [aux_sym_string_repeat1] = aux_sym_string_repeat1,
  [aux_sym_tuple_exp_repeat1] = aux_sym_tuple_exp_repeat1,
};

static const TSSymbolMetadata ts_symbol_metadata[] = {
  [ts_builtin_sym_end] = {
    .visible = false,
    .named = true,
  },
  [sym_ident] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_Int] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_String] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_Float] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_Bool] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_LPAREN] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_COMMA] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_RPAREN] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_DASH_GT] = {
    .visible = true,
    .named = false,
  },
  [sym_int_lit] = {
    .visible = true,
    .named = true,
  },
  [sym_float_lit] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_true] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_false] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_DQUOTE] = {
    .visible = true,
    .named = false,
  },
  [aux_sym_string_token1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_string_token2] = {
    .visible = false,
    .named = false,
  },
  [anon_sym_PLUS] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_DASH] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_STAR] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_SLASH] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_STAR_STAR] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_STAR_STAR_DOT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_EQ] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_EQ_EQ] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_DOLLAR_EQ_EQ] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_DOLLAR_EQ] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_DOLLAR] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_LT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_GT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_GT_EQ] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_LT_EQ] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_PLUS_DOT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_DASH_DOT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_STAR_DOT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_SLASH_DOT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_EQ_EQ_DOT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_GT_DOT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_LT_DOT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_GT_EQ_DOT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_LT_EQ_DOT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_EQ_DOT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_AMP] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_AMP_AMP] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_let] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_in] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_COLON] = {
    .visible = true,
    .named = false,
  },
  [sym_program] = {
    .visible = true,
    .named = true,
  },
  [sym__expression] = {
    .visible = false,
    .named = true,
  },
  [sym__pat] = {
    .visible = false,
    .named = true,
  },
  [sym__type] = {
    .visible = false,
    .named = true,
  },
  [sym_tuple_type] = {
    .visible = true,
    .named = true,
  },
  [sym_arrow_type] = {
    .visible = true,
    .named = true,
  },
  [sym_bool_lit] = {
    .visible = true,
    .named = true,
  },
  [sym_string] = {
    .visible = true,
    .named = true,
  },
  [sym_var] = {
    .visible = true,
    .named = true,
  },
  [sym_tuple_exp] = {
    .visible = true,
    .named = true,
  },
  [sym_let] = {
    .visible = true,
    .named = true,
  },
  [sym_typeann] = {
    .visible = true,
    .named = true,
  },
  [aux_sym_tuple_type_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_string_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_tuple_exp_repeat1] = {
    .visible = false,
    .named = false,
  },
};

static const TSSymbol ts_alias_sequences[PRODUCTION_ID_COUNT][MAX_ALIAS_SEQUENCE_LENGTH] = {
  [0] = {0},
};

static const uint16_t ts_non_terminal_alias_map[] = {
  0,
};

static const TSStateId ts_primary_state_ids[STATE_COUNT] = {
  [0] = 0,
  [1] = 1,
  [2] = 2,
  [3] = 2,
  [4] = 4,
  [5] = 5,
  [6] = 4,
  [7] = 5,
  [8] = 8,
  [9] = 9,
  [10] = 10,
  [11] = 11,
  [12] = 12,
  [13] = 13,
  [14] = 14,
  [15] = 15,
  [16] = 16,
  [17] = 17,
  [18] = 18,
  [19] = 19,
  [20] = 20,
  [21] = 21,
  [22] = 22,
  [23] = 18,
  [24] = 20,
  [25] = 22,
  [26] = 26,
  [27] = 27,
  [28] = 28,
  [29] = 29,
  [30] = 30,
  [31] = 31,
  [32] = 32,
  [33] = 33,
  [34] = 34,
  [35] = 35,
  [36] = 36,
  [37] = 28,
  [38] = 38,
  [39] = 39,
  [40] = 39,
  [41] = 41,
  [42] = 42,
  [43] = 43,
  [44] = 42,
  [45] = 27,
  [46] = 26,
  [47] = 35,
  [48] = 32,
  [49] = 31,
  [50] = 33,
  [51] = 30,
  [52] = 13,
  [53] = 53,
  [54] = 53,
  [55] = 55,
  [56] = 56,
};

static bool ts_lex(TSLexer *lexer, TSStateId state) {
  START_LEXER();
  eof = lexer->eof(lexer);
  switch (state) {
    case 0:
      if (eof) ADVANCE(31);
      if (lookahead == '"') ADVANCE(56);
      if (lookahead == '$') ADVANCE(71);
      if (lookahead == '&') ADVANCE(86);
      if (lookahead == '(') ADVANCE(46);
      if (lookahead == ')') ADVANCE(48);
      if (lookahead == '*') ADVANCE(62);
      if (lookahead == '+') ADVANCE(60);
      if (lookahead == ',') ADVANCE(47);
      if (lookahead == '-') ADVANCE(61);
      if (lookahead == '/') ADVANCE(63);
      if (lookahead == ':') ADVANCE(91);
      if (lookahead == '<') ADVANCE(72);
      if (lookahead == '=') ADVANCE(67);
      if (lookahead == '>') ADVANCE(73);
      if (lookahead == 'B') ADVANCE(16);
      if (lookahead == 'F') ADVANCE(12);
      if (lookahead == 'I') ADVANCE(15);
      if (lookahead == 'S') ADVANCE(25);
      if (lookahead == '\\') ADVANCE(29);
      if (lookahead == 'f') ADVANCE(3);
      if (lookahead == 'i') ADVANCE(13);
      if (lookahead == 'l') ADVANCE(7);
      if (lookahead == 't') ADVANCE(19);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(0)
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(50);
      END_STATE();
    case 1:
      if (lookahead == '"') ADVANCE(56);
      if (lookahead == '\\') ADVANCE(29);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') ADVANCE(58);
      if (lookahead != 0) ADVANCE(57);
      END_STATE();
    case 2:
      if (lookahead == '>') ADVANCE(49);
      END_STATE();
    case 3:
      if (lookahead == 'a') ADVANCE(10);
      END_STATE();
    case 4:
      if (lookahead == 'a') ADVANCE(24);
      END_STATE();
    case 5:
      if (lookahead == 'e') ADVANCE(52);
      END_STATE();
    case 6:
      if (lookahead == 'e') ADVANCE(54);
      END_STATE();
    case 7:
      if (lookahead == 'e') ADVANCE(23);
      END_STATE();
    case 8:
      if (lookahead == 'g') ADVANCE(43);
      END_STATE();
    case 9:
      if (lookahead == 'i') ADVANCE(14);
      END_STATE();
    case 10:
      if (lookahead == 'l') ADVANCE(21);
      END_STATE();
    case 11:
      if (lookahead == 'l') ADVANCE(45);
      END_STATE();
    case 12:
      if (lookahead == 'l') ADVANCE(17);
      END_STATE();
    case 13:
      if (lookahead == 'n') ADVANCE(90);
      END_STATE();
    case 14:
      if (lookahead == 'n') ADVANCE(8);
      END_STATE();
    case 15:
      if (lookahead == 'n') ADVANCE(22);
      END_STATE();
    case 16:
      if (lookahead == 'o') ADVANCE(18);
      END_STATE();
    case 17:
      if (lookahead == 'o') ADVANCE(4);
      END_STATE();
    case 18:
      if (lookahead == 'o') ADVANCE(11);
      END_STATE();
    case 19:
      if (lookahead == 'r') ADVANCE(26);
      END_STATE();
    case 20:
      if (lookahead == 'r') ADVANCE(9);
      END_STATE();
    case 21:
      if (lookahead == 's') ADVANCE(6);
      END_STATE();
    case 22:
      if (lookahead == 't') ADVANCE(42);
      END_STATE();
    case 23:
      if (lookahead == 't') ADVANCE(88);
      END_STATE();
    case 24:
      if (lookahead == 't') ADVANCE(44);
      END_STATE();
    case 25:
      if (lookahead == 't') ADVANCE(20);
      END_STATE();
    case 26:
      if (lookahead == 'u') ADVANCE(5);
      END_STATE();
    case 27:
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(27)
      if (('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(41);
      END_STATE();
    case 28:
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(51);
      END_STATE();
    case 29:
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(59);
      END_STATE();
    case 30:
      if (eof) ADVANCE(31);
      if (lookahead == '"') ADVANCE(56);
      if (lookahead == '(') ADVANCE(46);
      if (lookahead == ')') ADVANCE(48);
      if (lookahead == ',') ADVANCE(47);
      if (lookahead == '-') ADVANCE(2);
      if (lookahead == ':') ADVANCE(91);
      if (lookahead == '=') ADVANCE(66);
      if (lookahead == 'f') ADVANCE(32);
      if (lookahead == 'l') ADVANCE(33);
      if (lookahead == 't') ADVANCE(37);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(30)
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(50);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(41);
      END_STATE();
    case 31:
      ACCEPT_TOKEN(ts_builtin_sym_end);
      END_STATE();
    case 32:
      ACCEPT_TOKEN(sym_ident);
      if (lookahead == 'a') ADVANCE(36);
      if (lookahead == '\'' ||
          ('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('b' <= lookahead && lookahead <= 'z')) ADVANCE(41);
      END_STATE();
    case 33:
      ACCEPT_TOKEN(sym_ident);
      if (lookahead == 'e') ADVANCE(39);
      if (lookahead == '\'' ||
          ('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(41);
      END_STATE();
    case 34:
      ACCEPT_TOKEN(sym_ident);
      if (lookahead == 'e') ADVANCE(53);
      if (lookahead == '\'' ||
          ('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(41);
      END_STATE();
    case 35:
      ACCEPT_TOKEN(sym_ident);
      if (lookahead == 'e') ADVANCE(55);
      if (lookahead == '\'' ||
          ('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(41);
      END_STATE();
    case 36:
      ACCEPT_TOKEN(sym_ident);
      if (lookahead == 'l') ADVANCE(38);
      if (lookahead == '\'' ||
          ('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(41);
      END_STATE();
    case 37:
      ACCEPT_TOKEN(sym_ident);
      if (lookahead == 'r') ADVANCE(40);
      if (lookahead == '\'' ||
          ('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(41);
      END_STATE();
    case 38:
      ACCEPT_TOKEN(sym_ident);
      if (lookahead == 's') ADVANCE(35);
      if (lookahead == '\'' ||
          ('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(41);
      END_STATE();
    case 39:
      ACCEPT_TOKEN(sym_ident);
      if (lookahead == 't') ADVANCE(89);
      if (lookahead == '\'' ||
          ('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(41);
      END_STATE();
    case 40:
      ACCEPT_TOKEN(sym_ident);
      if (lookahead == 'u') ADVANCE(34);
      if (lookahead == '\'' ||
          ('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(41);
      END_STATE();
    case 41:
      ACCEPT_TOKEN(sym_ident);
      if (lookahead == '\'' ||
          ('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(41);
      END_STATE();
    case 42:
      ACCEPT_TOKEN(anon_sym_Int);
      END_STATE();
    case 43:
      ACCEPT_TOKEN(anon_sym_String);
      END_STATE();
    case 44:
      ACCEPT_TOKEN(anon_sym_Float);
      END_STATE();
    case 45:
      ACCEPT_TOKEN(anon_sym_Bool);
      END_STATE();
    case 46:
      ACCEPT_TOKEN(anon_sym_LPAREN);
      END_STATE();
    case 47:
      ACCEPT_TOKEN(anon_sym_COMMA);
      END_STATE();
    case 48:
      ACCEPT_TOKEN(anon_sym_RPAREN);
      END_STATE();
    case 49:
      ACCEPT_TOKEN(anon_sym_DASH_GT);
      END_STATE();
    case 50:
      ACCEPT_TOKEN(sym_int_lit);
      if (lookahead == '.') ADVANCE(28);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(50);
      END_STATE();
    case 51:
      ACCEPT_TOKEN(sym_float_lit);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(51);
      END_STATE();
    case 52:
      ACCEPT_TOKEN(anon_sym_true);
      END_STATE();
    case 53:
      ACCEPT_TOKEN(anon_sym_true);
      if (lookahead == '\'' ||
          ('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(41);
      END_STATE();
    case 54:
      ACCEPT_TOKEN(anon_sym_false);
      END_STATE();
    case 55:
      ACCEPT_TOKEN(anon_sym_false);
      if (lookahead == '\'' ||
          ('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(41);
      END_STATE();
    case 56:
      ACCEPT_TOKEN(anon_sym_DQUOTE);
      END_STATE();
    case 57:
      ACCEPT_TOKEN(aux_sym_string_token1);
      END_STATE();
    case 58:
      ACCEPT_TOKEN(aux_sym_string_token1);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') ADVANCE(58);
      if (lookahead != 0 &&
          lookahead != '"' &&
          lookahead != '\\') ADVANCE(57);
      END_STATE();
    case 59:
      ACCEPT_TOKEN(aux_sym_string_token2);
      END_STATE();
    case 60:
      ACCEPT_TOKEN(anon_sym_PLUS);
      if (lookahead == '.') ADVANCE(76);
      END_STATE();
    case 61:
      ACCEPT_TOKEN(anon_sym_DASH);
      if (lookahead == '.') ADVANCE(77);
      if (lookahead == '>') ADVANCE(49);
      END_STATE();
    case 62:
      ACCEPT_TOKEN(anon_sym_STAR);
      if (lookahead == '*') ADVANCE(64);
      if (lookahead == '.') ADVANCE(78);
      END_STATE();
    case 63:
      ACCEPT_TOKEN(anon_sym_SLASH);
      if (lookahead == '.') ADVANCE(79);
      END_STATE();
    case 64:
      ACCEPT_TOKEN(anon_sym_STAR_STAR);
      if (lookahead == '.') ADVANCE(65);
      END_STATE();
    case 65:
      ACCEPT_TOKEN(anon_sym_STAR_STAR_DOT);
      END_STATE();
    case 66:
      ACCEPT_TOKEN(anon_sym_EQ);
      END_STATE();
    case 67:
      ACCEPT_TOKEN(anon_sym_EQ);
      if (lookahead == '.') ADVANCE(85);
      if (lookahead == '=') ADVANCE(68);
      END_STATE();
    case 68:
      ACCEPT_TOKEN(anon_sym_EQ_EQ);
      if (lookahead == '.') ADVANCE(80);
      END_STATE();
    case 69:
      ACCEPT_TOKEN(anon_sym_DOLLAR_EQ_EQ);
      END_STATE();
    case 70:
      ACCEPT_TOKEN(anon_sym_DOLLAR_EQ);
      if (lookahead == '=') ADVANCE(69);
      END_STATE();
    case 71:
      ACCEPT_TOKEN(anon_sym_DOLLAR);
      if (lookahead == '=') ADVANCE(70);
      END_STATE();
    case 72:
      ACCEPT_TOKEN(anon_sym_LT);
      if (lookahead == '.') ADVANCE(82);
      if (lookahead == '=') ADVANCE(75);
      END_STATE();
    case 73:
      ACCEPT_TOKEN(anon_sym_GT);
      if (lookahead == '.') ADVANCE(81);
      if (lookahead == '=') ADVANCE(74);
      END_STATE();
    case 74:
      ACCEPT_TOKEN(anon_sym_GT_EQ);
      if (lookahead == '.') ADVANCE(83);
      END_STATE();
    case 75:
      ACCEPT_TOKEN(anon_sym_LT_EQ);
      if (lookahead == '.') ADVANCE(84);
      END_STATE();
    case 76:
      ACCEPT_TOKEN(anon_sym_PLUS_DOT);
      END_STATE();
    case 77:
      ACCEPT_TOKEN(anon_sym_DASH_DOT);
      END_STATE();
    case 78:
      ACCEPT_TOKEN(anon_sym_STAR_DOT);
      END_STATE();
    case 79:
      ACCEPT_TOKEN(anon_sym_SLASH_DOT);
      END_STATE();
    case 80:
      ACCEPT_TOKEN(anon_sym_EQ_EQ_DOT);
      END_STATE();
    case 81:
      ACCEPT_TOKEN(anon_sym_GT_DOT);
      END_STATE();
    case 82:
      ACCEPT_TOKEN(anon_sym_LT_DOT);
      END_STATE();
    case 83:
      ACCEPT_TOKEN(anon_sym_GT_EQ_DOT);
      END_STATE();
    case 84:
      ACCEPT_TOKEN(anon_sym_LT_EQ_DOT);
      END_STATE();
    case 85:
      ACCEPT_TOKEN(anon_sym_EQ_DOT);
      END_STATE();
    case 86:
      ACCEPT_TOKEN(anon_sym_AMP);
      if (lookahead == '&') ADVANCE(87);
      END_STATE();
    case 87:
      ACCEPT_TOKEN(anon_sym_AMP_AMP);
      END_STATE();
    case 88:
      ACCEPT_TOKEN(anon_sym_let);
      END_STATE();
    case 89:
      ACCEPT_TOKEN(anon_sym_let);
      if (lookahead == '\'' ||
          ('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(41);
      END_STATE();
    case 90:
      ACCEPT_TOKEN(anon_sym_in);
      END_STATE();
    case 91:
      ACCEPT_TOKEN(anon_sym_COLON);
      END_STATE();
    default:
      return false;
  }
}

static const TSLexMode ts_lex_modes[STATE_COUNT] = {
  [0] = {.lex_state = 0},
  [1] = {.lex_state = 30},
  [2] = {.lex_state = 30},
  [3] = {.lex_state = 30},
  [4] = {.lex_state = 30},
  [5] = {.lex_state = 30},
  [6] = {.lex_state = 30},
  [7] = {.lex_state = 30},
  [8] = {.lex_state = 30},
  [9] = {.lex_state = 0},
  [10] = {.lex_state = 0},
  [11] = {.lex_state = 0},
  [12] = {.lex_state = 0},
  [13] = {.lex_state = 30},
  [14] = {.lex_state = 30},
  [15] = {.lex_state = 30},
  [16] = {.lex_state = 30},
  [17] = {.lex_state = 30},
  [18] = {.lex_state = 1},
  [19] = {.lex_state = 1},
  [20] = {.lex_state = 27},
  [21] = {.lex_state = 30},
  [22] = {.lex_state = 1},
  [23] = {.lex_state = 1},
  [24] = {.lex_state = 27},
  [25] = {.lex_state = 1},
  [26] = {.lex_state = 0},
  [27] = {.lex_state = 0},
  [28] = {.lex_state = 0},
  [29] = {.lex_state = 30},
  [30] = {.lex_state = 0},
  [31] = {.lex_state = 0},
  [32] = {.lex_state = 0},
  [33] = {.lex_state = 0},
  [34] = {.lex_state = 0},
  [35] = {.lex_state = 0},
  [36] = {.lex_state = 30},
  [37] = {.lex_state = 0},
  [38] = {.lex_state = 0},
  [39] = {.lex_state = 0},
  [40] = {.lex_state = 0},
  [41] = {.lex_state = 0},
  [42] = {.lex_state = 30},
  [43] = {.lex_state = 0},
  [44] = {.lex_state = 30},
  [45] = {.lex_state = 0},
  [46] = {.lex_state = 0},
  [47] = {.lex_state = 0},
  [48] = {.lex_state = 0},
  [49] = {.lex_state = 0},
  [50] = {.lex_state = 0},
  [51] = {.lex_state = 0},
  [52] = {.lex_state = 0},
  [53] = {.lex_state = 0},
  [54] = {.lex_state = 0},
  [55] = {.lex_state = 0},
  [56] = {.lex_state = 0},
};

static const uint16_t ts_parse_table[LARGE_STATE_COUNT][SYMBOL_COUNT] = {
  [0] = {
    [ts_builtin_sym_end] = ACTIONS(1),
    [anon_sym_Int] = ACTIONS(1),
    [anon_sym_String] = ACTIONS(1),
    [anon_sym_Float] = ACTIONS(1),
    [anon_sym_Bool] = ACTIONS(1),
    [anon_sym_LPAREN] = ACTIONS(1),
    [anon_sym_COMMA] = ACTIONS(1),
    [anon_sym_RPAREN] = ACTIONS(1),
    [anon_sym_DASH_GT] = ACTIONS(1),
    [sym_int_lit] = ACTIONS(1),
    [sym_float_lit] = ACTIONS(1),
    [anon_sym_true] = ACTIONS(1),
    [anon_sym_false] = ACTIONS(1),
    [anon_sym_DQUOTE] = ACTIONS(1),
    [aux_sym_string_token2] = ACTIONS(1),
    [anon_sym_PLUS] = ACTIONS(1),
    [anon_sym_DASH] = ACTIONS(1),
    [anon_sym_STAR] = ACTIONS(1),
    [anon_sym_SLASH] = ACTIONS(1),
    [anon_sym_STAR_STAR] = ACTIONS(1),
    [anon_sym_STAR_STAR_DOT] = ACTIONS(1),
    [anon_sym_EQ] = ACTIONS(1),
    [anon_sym_EQ_EQ] = ACTIONS(1),
    [anon_sym_DOLLAR_EQ_EQ] = ACTIONS(1),
    [anon_sym_DOLLAR_EQ] = ACTIONS(1),
    [anon_sym_DOLLAR] = ACTIONS(1),
    [anon_sym_LT] = ACTIONS(1),
    [anon_sym_GT] = ACTIONS(1),
    [anon_sym_GT_EQ] = ACTIONS(1),
    [anon_sym_LT_EQ] = ACTIONS(1),
    [anon_sym_PLUS_DOT] = ACTIONS(1),
    [anon_sym_DASH_DOT] = ACTIONS(1),
    [anon_sym_STAR_DOT] = ACTIONS(1),
    [anon_sym_SLASH_DOT] = ACTIONS(1),
    [anon_sym_EQ_EQ_DOT] = ACTIONS(1),
    [anon_sym_GT_DOT] = ACTIONS(1),
    [anon_sym_LT_DOT] = ACTIONS(1),
    [anon_sym_GT_EQ_DOT] = ACTIONS(1),
    [anon_sym_LT_EQ_DOT] = ACTIONS(1),
    [anon_sym_EQ_DOT] = ACTIONS(1),
    [anon_sym_AMP] = ACTIONS(1),
    [anon_sym_AMP_AMP] = ACTIONS(1),
    [anon_sym_let] = ACTIONS(1),
    [anon_sym_in] = ACTIONS(1),
    [anon_sym_COLON] = ACTIONS(1),
  },
  [1] = {
    [sym_program] = STATE(56),
    [sym__expression] = STATE(55),
    [sym_bool_lit] = STATE(55),
    [sym_string] = STATE(55),
    [sym_var] = STATE(55),
    [sym_tuple_exp] = STATE(55),
    [sym_let] = STATE(55),
    [sym_ident] = ACTIONS(3),
    [anon_sym_LPAREN] = ACTIONS(5),
    [sym_int_lit] = ACTIONS(7),
    [sym_float_lit] = ACTIONS(9),
    [anon_sym_true] = ACTIONS(11),
    [anon_sym_false] = ACTIONS(11),
    [anon_sym_DQUOTE] = ACTIONS(13),
    [anon_sym_let] = ACTIONS(15),
  },
};

static const uint16_t ts_small_parse_table[] = {
  [0] = 9,
    ACTIONS(3), 1,
      sym_ident,
    ACTIONS(5), 1,
      anon_sym_LPAREN,
    ACTIONS(13), 1,
      anon_sym_DQUOTE,
    ACTIONS(15), 1,
      anon_sym_let,
    ACTIONS(17), 1,
      anon_sym_RPAREN,
    ACTIONS(19), 1,
      sym_int_lit,
    ACTIONS(21), 1,
      sym_float_lit,
    ACTIONS(11), 2,
      anon_sym_true,
      anon_sym_false,
    STATE(40), 6,
      sym__expression,
      sym_bool_lit,
      sym_string,
      sym_var,
      sym_tuple_exp,
      sym_let,
  [34] = 9,
    ACTIONS(3), 1,
      sym_ident,
    ACTIONS(5), 1,
      anon_sym_LPAREN,
    ACTIONS(13), 1,
      anon_sym_DQUOTE,
    ACTIONS(15), 1,
      anon_sym_let,
    ACTIONS(23), 1,
      anon_sym_RPAREN,
    ACTIONS(25), 1,
      sym_int_lit,
    ACTIONS(27), 1,
      sym_float_lit,
    ACTIONS(11), 2,
      anon_sym_true,
      anon_sym_false,
    STATE(39), 6,
      sym__expression,
      sym_bool_lit,
      sym_string,
      sym_var,
      sym_tuple_exp,
      sym_let,
  [68] = 8,
    ACTIONS(29), 1,
      sym_ident,
    ACTIONS(31), 1,
      anon_sym_LPAREN,
    ACTIONS(33), 1,
      sym_int_lit,
    ACTIONS(35), 1,
      sym_float_lit,
    ACTIONS(39), 1,
      anon_sym_DQUOTE,
    ACTIONS(41), 1,
      anon_sym_let,
    ACTIONS(37), 2,
      anon_sym_true,
      anon_sym_false,
    STATE(53), 6,
      sym__expression,
      sym_bool_lit,
      sym_string,
      sym_var,
      sym_tuple_exp,
      sym_let,
  [99] = 8,
    ACTIONS(3), 1,
      sym_ident,
    ACTIONS(5), 1,
      anon_sym_LPAREN,
    ACTIONS(13), 1,
      anon_sym_DQUOTE,
    ACTIONS(15), 1,
      anon_sym_let,
    ACTIONS(43), 1,
      sym_int_lit,
    ACTIONS(45), 1,
      sym_float_lit,
    ACTIONS(11), 2,
      anon_sym_true,
      anon_sym_false,
    STATE(31), 6,
      sym__expression,
      sym_bool_lit,
      sym_string,
      sym_var,
      sym_tuple_exp,
      sym_let,
  [130] = 8,
    ACTIONS(29), 1,
      sym_ident,
    ACTIONS(31), 1,
      anon_sym_LPAREN,
    ACTIONS(39), 1,
      anon_sym_DQUOTE,
    ACTIONS(41), 1,
      anon_sym_let,
    ACTIONS(47), 1,
      sym_int_lit,
    ACTIONS(49), 1,
      sym_float_lit,
    ACTIONS(37), 2,
      anon_sym_true,
      anon_sym_false,
    STATE(54), 6,
      sym__expression,
      sym_bool_lit,
      sym_string,
      sym_var,
      sym_tuple_exp,
      sym_let,
  [161] = 8,
    ACTIONS(29), 1,
      sym_ident,
    ACTIONS(31), 1,
      anon_sym_LPAREN,
    ACTIONS(39), 1,
      anon_sym_DQUOTE,
    ACTIONS(41), 1,
      anon_sym_let,
    ACTIONS(51), 1,
      sym_int_lit,
    ACTIONS(53), 1,
      sym_float_lit,
    ACTIONS(37), 2,
      anon_sym_true,
      anon_sym_false,
    STATE(49), 6,
      sym__expression,
      sym_bool_lit,
      sym_string,
      sym_var,
      sym_tuple_exp,
      sym_let,
  [192] = 8,
    ACTIONS(3), 1,
      sym_ident,
    ACTIONS(5), 1,
      anon_sym_LPAREN,
    ACTIONS(13), 1,
      anon_sym_DQUOTE,
    ACTIONS(15), 1,
      anon_sym_let,
    ACTIONS(55), 1,
      sym_int_lit,
    ACTIONS(57), 1,
      sym_float_lit,
    ACTIONS(11), 2,
      anon_sym_true,
      anon_sym_false,
    STATE(43), 6,
      sym__expression,
      sym_bool_lit,
      sym_string,
      sym_var,
      sym_tuple_exp,
      sym_let,
  [223] = 4,
    ACTIONS(61), 1,
      anon_sym_LPAREN,
    ACTIONS(63), 1,
      anon_sym_RPAREN,
    STATE(21), 3,
      sym__type,
      sym_tuple_type,
      sym_arrow_type,
    ACTIONS(59), 4,
      anon_sym_Int,
      anon_sym_String,
      anon_sym_Float,
      anon_sym_Bool,
  [241] = 3,
    ACTIONS(61), 1,
      anon_sym_LPAREN,
    STATE(16), 3,
      sym__type,
      sym_tuple_type,
      sym_arrow_type,
    ACTIONS(65), 4,
      anon_sym_Int,
      anon_sym_String,
      anon_sym_Float,
      anon_sym_Bool,
  [256] = 3,
    ACTIONS(61), 1,
      anon_sym_LPAREN,
    STATE(36), 3,
      sym__type,
      sym_tuple_type,
      sym_arrow_type,
    ACTIONS(67), 4,
      anon_sym_Int,
      anon_sym_String,
      anon_sym_Float,
      anon_sym_Bool,
  [271] = 3,
    ACTIONS(61), 1,
      anon_sym_LPAREN,
    STATE(29), 3,
      sym__type,
      sym_tuple_type,
      sym_arrow_type,
    ACTIONS(69), 4,
      anon_sym_Int,
      anon_sym_String,
      anon_sym_Float,
      anon_sym_Bool,
  [286] = 1,
    ACTIONS(71), 5,
      ts_builtin_sym_end,
      anon_sym_COMMA,
      anon_sym_RPAREN,
      anon_sym_EQ,
      anon_sym_COLON,
  [294] = 1,
    ACTIONS(73), 5,
      anon_sym_COMMA,
      anon_sym_RPAREN,
      anon_sym_DASH_GT,
      anon_sym_EQ,
      anon_sym_COLON,
  [302] = 1,
    ACTIONS(75), 5,
      anon_sym_COMMA,
      anon_sym_RPAREN,
      anon_sym_DASH_GT,
      anon_sym_EQ,
      anon_sym_COLON,
  [310] = 1,
    ACTIONS(77), 5,
      anon_sym_COMMA,
      anon_sym_RPAREN,
      anon_sym_DASH_GT,
      anon_sym_EQ,
      anon_sym_COLON,
  [318] = 1,
    ACTIONS(79), 5,
      anon_sym_COMMA,
      anon_sym_RPAREN,
      anon_sym_DASH_GT,
      anon_sym_EQ,
      anon_sym_COLON,
  [326] = 4,
    ACTIONS(81), 1,
      anon_sym_DQUOTE,
    ACTIONS(83), 1,
      aux_sym_string_token1,
    ACTIONS(85), 1,
      aux_sym_string_token2,
    STATE(19), 1,
      aux_sym_string_repeat1,
  [339] = 4,
    ACTIONS(87), 1,
      anon_sym_DQUOTE,
    ACTIONS(89), 1,
      aux_sym_string_token1,
    ACTIONS(92), 1,
      aux_sym_string_token2,
    STATE(19), 1,
      aux_sym_string_repeat1,
  [352] = 2,
    ACTIONS(95), 1,
      sym_ident,
    STATE(44), 3,
      sym__pat,
      sym_var,
      sym_typeann,
  [361] = 4,
    ACTIONS(97), 1,
      anon_sym_COMMA,
    ACTIONS(99), 1,
      anon_sym_RPAREN,
    ACTIONS(101), 1,
      anon_sym_DASH_GT,
    STATE(34), 1,
      aux_sym_tuple_type_repeat1,
  [374] = 4,
    ACTIONS(103), 1,
      anon_sym_DQUOTE,
    ACTIONS(105), 1,
      aux_sym_string_token1,
    ACTIONS(107), 1,
      aux_sym_string_token2,
    STATE(23), 1,
      aux_sym_string_repeat1,
  [387] = 4,
    ACTIONS(83), 1,
      aux_sym_string_token1,
    ACTIONS(85), 1,
      aux_sym_string_token2,
    ACTIONS(109), 1,
      anon_sym_DQUOTE,
    STATE(19), 1,
      aux_sym_string_repeat1,
  [400] = 2,
    ACTIONS(95), 1,
      sym_ident,
    STATE(42), 3,
      sym__pat,
      sym_var,
      sym_typeann,
  [409] = 4,
    ACTIONS(111), 1,
      anon_sym_DQUOTE,
    ACTIONS(113), 1,
      aux_sym_string_token1,
    ACTIONS(115), 1,
      aux_sym_string_token2,
    STATE(18), 1,
      aux_sym_string_repeat1,
  [422] = 1,
    ACTIONS(117), 3,
      ts_builtin_sym_end,
      anon_sym_COMMA,
      anon_sym_RPAREN,
  [428] = 1,
    ACTIONS(119), 3,
      ts_builtin_sym_end,
      anon_sym_COMMA,
      anon_sym_RPAREN,
  [434] = 3,
    ACTIONS(121), 1,
      anon_sym_COMMA,
    ACTIONS(123), 1,
      anon_sym_RPAREN,
    STATE(41), 1,
      aux_sym_tuple_exp_repeat1,
  [444] = 2,
    ACTIONS(101), 1,
      anon_sym_DASH_GT,
    ACTIONS(125), 2,
      anon_sym_EQ,
      anon_sym_COLON,
  [452] = 1,
    ACTIONS(127), 3,
      ts_builtin_sym_end,
      anon_sym_COMMA,
      anon_sym_RPAREN,
  [458] = 1,
    ACTIONS(129), 3,
      ts_builtin_sym_end,
      anon_sym_COMMA,
      anon_sym_RPAREN,
  [464] = 1,
    ACTIONS(131), 3,
      ts_builtin_sym_end,
      anon_sym_COMMA,
      anon_sym_RPAREN,
  [470] = 1,
    ACTIONS(133), 3,
      ts_builtin_sym_end,
      anon_sym_COMMA,
      anon_sym_RPAREN,
  [476] = 3,
    ACTIONS(97), 1,
      anon_sym_COMMA,
    ACTIONS(135), 1,
      anon_sym_RPAREN,
    STATE(38), 1,
      aux_sym_tuple_type_repeat1,
  [486] = 1,
    ACTIONS(137), 3,
      ts_builtin_sym_end,
      anon_sym_COMMA,
      anon_sym_RPAREN,
  [492] = 2,
    ACTIONS(101), 1,
      anon_sym_DASH_GT,
    ACTIONS(139), 2,
      anon_sym_COMMA,
      anon_sym_RPAREN,
  [500] = 3,
    ACTIONS(121), 1,
      anon_sym_COMMA,
    ACTIONS(141), 1,
      anon_sym_RPAREN,
    STATE(41), 1,
      aux_sym_tuple_exp_repeat1,
  [510] = 3,
    ACTIONS(139), 1,
      anon_sym_RPAREN,
    ACTIONS(143), 1,
      anon_sym_COMMA,
    STATE(38), 1,
      aux_sym_tuple_type_repeat1,
  [520] = 3,
    ACTIONS(121), 1,
      anon_sym_COMMA,
    ACTIONS(146), 1,
      anon_sym_RPAREN,
    STATE(28), 1,
      aux_sym_tuple_exp_repeat1,
  [530] = 3,
    ACTIONS(121), 1,
      anon_sym_COMMA,
    ACTIONS(148), 1,
      anon_sym_RPAREN,
    STATE(37), 1,
      aux_sym_tuple_exp_repeat1,
  [540] = 3,
    ACTIONS(150), 1,
      anon_sym_COMMA,
    ACTIONS(153), 1,
      anon_sym_RPAREN,
    STATE(41), 1,
      aux_sym_tuple_exp_repeat1,
  [550] = 2,
    ACTIONS(155), 1,
      anon_sym_EQ,
    ACTIONS(157), 1,
      anon_sym_COLON,
  [557] = 1,
    ACTIONS(153), 2,
      anon_sym_COMMA,
      anon_sym_RPAREN,
  [562] = 2,
    ACTIONS(157), 1,
      anon_sym_COLON,
    ACTIONS(159), 1,
      anon_sym_EQ,
  [569] = 1,
    ACTIONS(119), 1,
      anon_sym_in,
  [573] = 1,
    ACTIONS(117), 1,
      anon_sym_in,
  [577] = 1,
    ACTIONS(137), 1,
      anon_sym_in,
  [581] = 1,
    ACTIONS(131), 1,
      anon_sym_in,
  [585] = 1,
    ACTIONS(129), 1,
      anon_sym_in,
  [589] = 1,
    ACTIONS(133), 1,
      anon_sym_in,
  [593] = 1,
    ACTIONS(127), 1,
      anon_sym_in,
  [597] = 1,
    ACTIONS(71), 1,
      anon_sym_in,
  [601] = 1,
    ACTIONS(161), 1,
      anon_sym_in,
  [605] = 1,
    ACTIONS(163), 1,
      anon_sym_in,
  [609] = 1,
    ACTIONS(165), 1,
      ts_builtin_sym_end,
  [613] = 1,
    ACTIONS(167), 1,
      ts_builtin_sym_end,
};

static const uint32_t ts_small_parse_table_map[] = {
  [SMALL_STATE(2)] = 0,
  [SMALL_STATE(3)] = 34,
  [SMALL_STATE(4)] = 68,
  [SMALL_STATE(5)] = 99,
  [SMALL_STATE(6)] = 130,
  [SMALL_STATE(7)] = 161,
  [SMALL_STATE(8)] = 192,
  [SMALL_STATE(9)] = 223,
  [SMALL_STATE(10)] = 241,
  [SMALL_STATE(11)] = 256,
  [SMALL_STATE(12)] = 271,
  [SMALL_STATE(13)] = 286,
  [SMALL_STATE(14)] = 294,
  [SMALL_STATE(15)] = 302,
  [SMALL_STATE(16)] = 310,
  [SMALL_STATE(17)] = 318,
  [SMALL_STATE(18)] = 326,
  [SMALL_STATE(19)] = 339,
  [SMALL_STATE(20)] = 352,
  [SMALL_STATE(21)] = 361,
  [SMALL_STATE(22)] = 374,
  [SMALL_STATE(23)] = 387,
  [SMALL_STATE(24)] = 400,
  [SMALL_STATE(25)] = 409,
  [SMALL_STATE(26)] = 422,
  [SMALL_STATE(27)] = 428,
  [SMALL_STATE(28)] = 434,
  [SMALL_STATE(29)] = 444,
  [SMALL_STATE(30)] = 452,
  [SMALL_STATE(31)] = 458,
  [SMALL_STATE(32)] = 464,
  [SMALL_STATE(33)] = 470,
  [SMALL_STATE(34)] = 476,
  [SMALL_STATE(35)] = 486,
  [SMALL_STATE(36)] = 492,
  [SMALL_STATE(37)] = 500,
  [SMALL_STATE(38)] = 510,
  [SMALL_STATE(39)] = 520,
  [SMALL_STATE(40)] = 530,
  [SMALL_STATE(41)] = 540,
  [SMALL_STATE(42)] = 550,
  [SMALL_STATE(43)] = 557,
  [SMALL_STATE(44)] = 562,
  [SMALL_STATE(45)] = 569,
  [SMALL_STATE(46)] = 573,
  [SMALL_STATE(47)] = 577,
  [SMALL_STATE(48)] = 581,
  [SMALL_STATE(49)] = 585,
  [SMALL_STATE(50)] = 589,
  [SMALL_STATE(51)] = 593,
  [SMALL_STATE(52)] = 597,
  [SMALL_STATE(53)] = 601,
  [SMALL_STATE(54)] = 605,
  [SMALL_STATE(55)] = 609,
  [SMALL_STATE(56)] = 613,
};

static const TSParseActionEntry ts_parse_actions[] = {
  [0] = {.entry = {.count = 0, .reusable = false}},
  [1] = {.entry = {.count = 1, .reusable = false}}, RECOVER(),
  [3] = {.entry = {.count = 1, .reusable = false}}, SHIFT(13),
  [5] = {.entry = {.count = 1, .reusable = true}}, SHIFT(2),
  [7] = {.entry = {.count = 1, .reusable = false}}, SHIFT(55),
  [9] = {.entry = {.count = 1, .reusable = true}}, SHIFT(55),
  [11] = {.entry = {.count = 1, .reusable = false}}, SHIFT(30),
  [13] = {.entry = {.count = 1, .reusable = true}}, SHIFT(25),
  [15] = {.entry = {.count = 1, .reusable = false}}, SHIFT(24),
  [17] = {.entry = {.count = 1, .reusable = true}}, SHIFT(33),
  [19] = {.entry = {.count = 1, .reusable = false}}, SHIFT(40),
  [21] = {.entry = {.count = 1, .reusable = true}}, SHIFT(40),
  [23] = {.entry = {.count = 1, .reusable = true}}, SHIFT(50),
  [25] = {.entry = {.count = 1, .reusable = false}}, SHIFT(39),
  [27] = {.entry = {.count = 1, .reusable = true}}, SHIFT(39),
  [29] = {.entry = {.count = 1, .reusable = false}}, SHIFT(52),
  [31] = {.entry = {.count = 1, .reusable = true}}, SHIFT(3),
  [33] = {.entry = {.count = 1, .reusable = false}}, SHIFT(53),
  [35] = {.entry = {.count = 1, .reusable = true}}, SHIFT(53),
  [37] = {.entry = {.count = 1, .reusable = false}}, SHIFT(51),
  [39] = {.entry = {.count = 1, .reusable = true}}, SHIFT(22),
  [41] = {.entry = {.count = 1, .reusable = false}}, SHIFT(20),
  [43] = {.entry = {.count = 1, .reusable = false}}, SHIFT(31),
  [45] = {.entry = {.count = 1, .reusable = true}}, SHIFT(31),
  [47] = {.entry = {.count = 1, .reusable = false}}, SHIFT(54),
  [49] = {.entry = {.count = 1, .reusable = true}}, SHIFT(54),
  [51] = {.entry = {.count = 1, .reusable = false}}, SHIFT(49),
  [53] = {.entry = {.count = 1, .reusable = true}}, SHIFT(49),
  [55] = {.entry = {.count = 1, .reusable = false}}, SHIFT(43),
  [57] = {.entry = {.count = 1, .reusable = true}}, SHIFT(43),
  [59] = {.entry = {.count = 1, .reusable = true}}, SHIFT(21),
  [61] = {.entry = {.count = 1, .reusable = true}}, SHIFT(9),
  [63] = {.entry = {.count = 1, .reusable = true}}, SHIFT(14),
  [65] = {.entry = {.count = 1, .reusable = true}}, SHIFT(16),
  [67] = {.entry = {.count = 1, .reusable = true}}, SHIFT(36),
  [69] = {.entry = {.count = 1, .reusable = true}}, SHIFT(29),
  [71] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_var, 1),
  [73] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_tuple_type, 2),
  [75] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_tuple_type, 4),
  [77] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_arrow_type, 3),
  [79] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_tuple_type, 3),
  [81] = {.entry = {.count = 1, .reusable = false}}, SHIFT(35),
  [83] = {.entry = {.count = 1, .reusable = true}}, SHIFT(19),
  [85] = {.entry = {.count = 1, .reusable = false}}, SHIFT(19),
  [87] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_string_repeat1, 2),
  [89] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_string_repeat1, 2), SHIFT_REPEAT(19),
  [92] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_string_repeat1, 2), SHIFT_REPEAT(19),
  [95] = {.entry = {.count = 1, .reusable = true}}, SHIFT(13),
  [97] = {.entry = {.count = 1, .reusable = true}}, SHIFT(11),
  [99] = {.entry = {.count = 1, .reusable = true}}, SHIFT(17),
  [101] = {.entry = {.count = 1, .reusable = true}}, SHIFT(10),
  [103] = {.entry = {.count = 1, .reusable = false}}, SHIFT(45),
  [105] = {.entry = {.count = 1, .reusable = true}}, SHIFT(23),
  [107] = {.entry = {.count = 1, .reusable = false}}, SHIFT(23),
  [109] = {.entry = {.count = 1, .reusable = false}}, SHIFT(47),
  [111] = {.entry = {.count = 1, .reusable = false}}, SHIFT(27),
  [113] = {.entry = {.count = 1, .reusable = true}}, SHIFT(18),
  [115] = {.entry = {.count = 1, .reusable = false}}, SHIFT(18),
  [117] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_tuple_exp, 3),
  [119] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_string, 2),
  [121] = {.entry = {.count = 1, .reusable = true}}, SHIFT(8),
  [123] = {.entry = {.count = 1, .reusable = true}}, SHIFT(48),
  [125] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_typeann, 3),
  [127] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_bool_lit, 1),
  [129] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_let, 6),
  [131] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_tuple_exp, 4),
  [133] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_tuple_exp, 2),
  [135] = {.entry = {.count = 1, .reusable = true}}, SHIFT(15),
  [137] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_string, 3),
  [139] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_tuple_type_repeat1, 2),
  [141] = {.entry = {.count = 1, .reusable = true}}, SHIFT(32),
  [143] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_tuple_type_repeat1, 2), SHIFT_REPEAT(11),
  [146] = {.entry = {.count = 1, .reusable = true}}, SHIFT(46),
  [148] = {.entry = {.count = 1, .reusable = true}}, SHIFT(26),
  [150] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_tuple_exp_repeat1, 2), SHIFT_REPEAT(8),
  [153] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_tuple_exp_repeat1, 2),
  [155] = {.entry = {.count = 1, .reusable = true}}, SHIFT(4),
  [157] = {.entry = {.count = 1, .reusable = true}}, SHIFT(12),
  [159] = {.entry = {.count = 1, .reusable = true}}, SHIFT(6),
  [161] = {.entry = {.count = 1, .reusable = true}}, SHIFT(5),
  [163] = {.entry = {.count = 1, .reusable = true}}, SHIFT(7),
  [165] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_program, 1),
  [167] = {.entry = {.count = 1, .reusable = true}},  ACCEPT_INPUT(),
};

#ifdef __cplusplus
extern "C" {
#endif
#ifdef _WIN32
#define extern __declspec(dllexport)
#endif

extern const TSLanguage *tree_sitter_Hazel(void) {
  static const TSLanguage language = {
    .version = LANGUAGE_VERSION,
    .symbol_count = SYMBOL_COUNT,
    .alias_count = ALIAS_COUNT,
    .token_count = TOKEN_COUNT,
    .external_token_count = EXTERNAL_TOKEN_COUNT,
    .state_count = STATE_COUNT,
    .large_state_count = LARGE_STATE_COUNT,
    .production_id_count = PRODUCTION_ID_COUNT,
    .field_count = FIELD_COUNT,
    .max_alias_sequence_length = MAX_ALIAS_SEQUENCE_LENGTH,
    .parse_table = &ts_parse_table[0][0],
    .small_parse_table = ts_small_parse_table,
    .small_parse_table_map = ts_small_parse_table_map,
    .parse_actions = ts_parse_actions,
    .symbol_names = ts_symbol_names,
    .symbol_metadata = ts_symbol_metadata,
    .public_symbol_map = ts_symbol_map,
    .alias_map = ts_non_terminal_alias_map,
    .alias_sequences = &ts_alias_sequences[0][0],
    .lex_modes = ts_lex_modes,
    .lex_fn = ts_lex,
    .primary_state_ids = ts_primary_state_ids,
  };
  return &language;
}
#ifdef __cplusplus
}
#endif
