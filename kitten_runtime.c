#include <kitten_runtime.h>

#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
  bool is_right;
  ktn_value value;
} ktn_either_data;

typedef struct {
  bool is_some;
  ktn_value value;
} ktn_option_data;

typedef struct {
  size_t length;
  ktn_value values[1]; /* Variadic. */
} ktn_vector_data;

static void ktn_not_yet_implemented(const char *funcName) {
  fprintf(stderr, "error: not yet implemented: %s\n", funcName);
  abort();
}

#define KTN_NYI() \
  ktn_not_yet_implemented(__func__);

#define KTN_UNARY_OP(name, type, op) \
  ktn_value ktn_builtin_##name(ktn_value x) { \
    return ktn_##type(op ktn_get_##type(x)); \
  }

#define KTN_BINARY_OP(name, type, op) \
  ktn_value ktn_builtin_##name(ktn_value x, ktn_value y) { \
    return ktn_##type(ktn_get_##type(x) op ktn_get_##type(y)); \
  }

static ktn_either_data *ktn_get_either(ktn_value) KTN_WARN_UNUSED_RESULT;
static ktn_option_data *ktn_get_option(ktn_value) KTN_WARN_UNUSED_RESULT;
static ktn_vector_data *ktn_get_vector(ktn_value) KTN_WARN_UNUSED_RESULT;
static ktn_vector_data *ktn_vector_create_from_string(const char *) KTN_WARN_UNUSED_RESULT;
static ktn_vector_data *ktn_vector_create_unallocated(size_t length) KTN_WARN_UNUSED_RESULT;

/*******************************************************************************
 * Builtins.
 */

KTN_BINARY_OP(addFloat, float, +)
KTN_BINARY_OP(addInt, int, +)

ktn_value ktn_builtin_addVector(ktn_value xs, ktn_value ys) {
  ktn_vector_data *left = ktn_get_vector(xs);
  ktn_vector_data *right = ktn_get_vector(ys);

  size_t length = left->length + right->length;
  ktn_vector_data *vector = ktn_vector_create_unallocated(length);
  size_t outputIndex = 0;
  for (size_t leftIndex = 0; leftIndex < left->length; ++leftIndex) {
    vector->values[outputIndex] = left->values[leftIndex];
    ++outputIndex;
  }
  for (size_t rightIndex = 0; rightIndex < right->length; ++rightIndex) {
    vector->values[outputIndex] = right->values[rightIndex];
    ++outputIndex;
  }
  assert(outputIndex == length);
  return vector;
}

KTN_BINARY_OP(andBool, bool, &&)

KTN_BINARY_OP(andInt, int, &)

ktn_value ktn_builtin_charToInt(ktn_value x) {
  return ktn_int(ktn_get_char(x));
}

void ktn_builtin_close(ktn_value x) {
  KTN_NYI();
}

KTN_BINARY_OP(divFloat, float, /)
KTN_BINARY_OP(divInt, int, /)
KTN_BINARY_OP(eqFloat, float, ==)
KTN_BINARY_OP(eqInt, int, ==)

void ktn_builtin_exit(ktn_value code) {
  exit(ktn_get_int(code));
}

ktn_value ktn_builtin_first(ktn_value pair) {
  KTN_NYI();
  return NULL;
}

ktn_value ktn_builtin_fromLeft(ktn_value either) {
  KTN_NYI();
  return NULL;
}

ktn_value ktn_builtin_fromRight(ktn_value either) {
  KTN_NYI();
  return NULL;
}

ktn_value ktn_builtin_fromSome(ktn_value option) {
  KTN_NYI();
  return NULL;
}

KTN_BINARY_OP(geFloat, float, >=)
KTN_BINARY_OP(geInt, int, >=)

ktn_value ktn_builtin_get(ktn_value xs, ktn_value index) {
  ktn_vector_data *vector = ktn_get_vector(xs);
  ktn_int_type i = ktn_get_int(index);
  if (0 <= i && (size_t)i < vector->length) {
    return ktn_builtin_some(vector->values[i]);
  } else {
    return ktn_builtin_none();
  }
}

ktn_value ktn_builtin_getLine(ktn_value handle) {
  KTN_NYI();
  return NULL;
}

KTN_BINARY_OP(gtFloat, float, >)
KTN_BINARY_OP(gtInt, int, >)

ktn_value ktn_builtin_init(ktn_value vector) {
  KTN_NYI();
  return NULL;
}

ktn_value ktn_builtin_intToChar(ktn_value x) {
  return ktn_char(ktn_get_int(x));
}

KTN_BINARY_OP(leFloat, float, <=)
KTN_BINARY_OP(leInt, int, <=)

ktn_value ktn_builtin_left(ktn_value x) {
  ktn_either_data *either = malloc(sizeof(*either));
  *either = (ktn_either_data) { .is_right = false, .value = x };
  return either;
}

ktn_value ktn_builtin_length(ktn_value vector) {
  KTN_NYI();
  return NULL;
}


KTN_BINARY_OP(ltFloat, float, <)
KTN_BINARY_OP(ltInt, int, <)

ktn_value ktn_builtin_modFloat(ktn_value x, ktn_value y) {
  KTN_NYI();
  return NULL;
}

KTN_BINARY_OP(modInt, int, %)
KTN_BINARY_OP(mulFloat, float, *)
KTN_BINARY_OP(mulInt, int, *)
KTN_BINARY_OP(neFloat, float, !=)
KTN_BINARY_OP(neInt, int, !=)

KTN_UNARY_OP(negFloat, float, -)
KTN_UNARY_OP(negInt, int, -)

ktn_value ktn_builtin_none(void) {
  ktn_option_data *option = malloc(sizeof(*option));
  *option = (ktn_option_data) { .is_some = false, .value = NULL };
  return option;
}

KTN_UNARY_OP(notBool, bool, !)
KTN_UNARY_OP(notInt, int, ~)

ktn_value ktn_builtin_openIn(void) {
  KTN_NYI();
  return NULL;
}

ktn_value ktn_builtin_openOut(void) {
  KTN_NYI();
  return NULL;
}

ktn_value ktn_builtin_option(ktn_value option, ktn_value ifSome) {
  KTN_NYI();
  return NULL;
}

ktn_value ktn_builtin_optionElse(ktn_value option, ktn_value ifSome, ktn_value ifNone) {
  KTN_NYI();
  return NULL;
}

KTN_BINARY_OP(orBool, bool, ||)
KTN_BINARY_OP(orInt, int, |)

ktn_value ktn_builtin_pair(ktn_value x, ktn_value y) {
  KTN_NYI();
  return NULL;
}

void ktn_builtin_print(ktn_value string, ktn_value handle) {
  FILE *file = ktn_get_handle(handle);
  ktn_vector_data *vector = ktn_get_vector(string);

  for (size_t i = 0; i < vector->length; ++i) {
    /* TODO(strager): Unicode code points. */
    ktn_char_type c = ktn_get_char(vector->values[i]);
    fputc(c >= 0x80 ? '?' : c, file);
  }
}

ktn_value ktn_builtin_rest(ktn_value vector) {
  KTN_NYI();
  return NULL;
}

ktn_value ktn_builtin_right(ktn_value x) {
  ktn_either_data *either = malloc(sizeof(*either));
  *either = (ktn_either_data) { .is_right = true, .value = x };
  return either;
}

ktn_value ktn_builtin_set(ktn_value vector, ktn_value index, ktn_value value) {
  KTN_NYI();
  return NULL;
}

ktn_value ktn_builtin_showFloat(ktn_value x) {
  /* FIXME(strager) */
  char buffer[64];
  int rc = snprintf(buffer, sizeof(buffer), "%e", ktn_get_float(x));
  assert(rc >= 1);
  assert((size_t)rc < sizeof(buffer) - 1);
  return ktn_vector_create_from_string(buffer);
}

ktn_value ktn_builtin_showInt(ktn_value x) {
  char buffer[32];
  int rc = snprintf(buffer, sizeof(buffer), "%lld", ktn_get_int(x));
  assert(rc >= 1);
  assert((size_t)rc < sizeof(buffer) - 1);
  return ktn_vector_create_from_string(buffer);
}

ktn_value ktn_builtin_some(ktn_value x) {
  ktn_option_data *option = malloc(sizeof(*option));
  *option = (ktn_option_data) { .is_some = true, .value = x };
  return option;
}

ktn_value ktn_builtin_stderr(void) {
  return ktn_handle(stderr);
}

ktn_value ktn_builtin_stdin(void) {
  return ktn_handle(stdin);
}

ktn_value ktn_builtin_stdout(void) {
  return ktn_handle(stdout);
}

KTN_BINARY_OP(subFloat, float, -)
KTN_BINARY_OP(subInt, int, -)

ktn_value ktn_builtin_tail(ktn_value vector) {
  ktn_vector_data *input = ktn_get_vector(vector);
  size_t length = input->length - 1;
  ktn_vector_data *output = ktn_vector_create_unallocated(length);
  for (size_t i = 0; i < length; ++i) {
    output->values[i] = input->values[i + 1];
  }
  return output;
}

ktn_value ktn_builtin_unsafePurify11(ktn_value x) {
  KTN_NYI();
  return NULL;
}


KTN_BINARY_OP(xorBool, bool, !=)
KTN_BINARY_OP(xorInt, int, ^)

/*******************************************************************************
 * Runtime functions.
 */

ktn_value ktn_act(ktn_closure_function closure, size_t closed, ...) {
  size_t size
    = sizeof(ktn_activation)
    - sizeof(((ktn_activation *)NULL)->values)
    + sizeof(ktn_value) * closed;

  ktn_activation *act = malloc(size);
  act->function = closure;

  va_list va;
  va_start(va, closed);
  for (size_t i = 0; i < closed; ++i) {
    act->values[i] = va_arg(va, ktn_value);
  }
  va_end(va);

  return act;
}

ktn_value ktn_bool(ktn_bool_type x) {
  ktn_bool_type *ret = malloc(sizeof(*ret));
  *ret = x;
  return ret;
}

ktn_value ktn_char(ktn_char_type x) {
  ktn_char_type *ret = malloc(sizeof(*ret));
  *ret = x;
  return ret;
}

ktn_value ktn_false(void) {
  return ktn_bool(false);
}

ktn_value ktn_float(double x) {
  ktn_float_type *ret = malloc(sizeof(*ret));
  *ret = x;
  return ret;
}

ktn_value ktn_handle(FILE *file) {
  ktn_handle_type *ret = malloc(sizeof(*ret));
  *ret = file;
  return ret;
}

ktn_value ktn_int(int64_t x) {
  ktn_int_type *ret = malloc(sizeof(*ret));
  *ret = x;
  return ret;
}

ktn_value ktn_pair(ktn_value x, ktn_value y) {
  KTN_NYI();
  return NULL;
}

ktn_value ktn_true(void) {
  return ktn_bool(true);
}

ktn_value ktn_vector(size_t length, ...) {
  ktn_vector_data *vector = ktn_vector_create_unallocated(length);

  va_list va;
  va_start(va, length);
  for (size_t i = 0; i < length; ++i) {
    vector->values[i] = va_arg(va, ktn_value);
  }
  va_end(va);

  return vector;
}

ktn_activation *ktn_get_activation(ktn_value x) {
  return (ktn_activation *) x;
}

ktn_bool_type ktn_get_bool(ktn_value x) {
  return *(ktn_bool_type *) x;
}

ktn_char_type ktn_get_char(ktn_value x) {
  return *(ktn_char_type *) x;
}

ktn_float_type ktn_get_float(ktn_value x) {
  return *(ktn_float_type *) x;
}

ktn_handle_type ktn_get_handle(ktn_value x) {
  return *(ktn_handle_type *) x;
}

ktn_int_type ktn_get_int(ktn_value x) {
  return *(ktn_int_type *) x;
}

ktn_value ktn_either_get_value(ktn_value x) {
  return ktn_get_either(x)->value;
}

bool ktn_either_is_left(ktn_value x) {
  return !ktn_either_is_right(x);
}

bool ktn_either_is_right(ktn_value x) {
  return ktn_get_either(x)->is_right;
}

ktn_value ktn_option_get_value(ktn_value x) {
  return ktn_get_option(x)->value;
}

bool ktn_option_is_some(ktn_value x) {
  return ktn_get_option(x)->is_some;
}

bool ktn_option_is_none(ktn_value x) {
  return !ktn_option_is_some(x);
}

/*******************************************************************************
 * Utility functions.
 */

static ktn_either_data *ktn_get_either(ktn_value x) {
  return (ktn_either_data *) x;
}

static ktn_option_data *ktn_get_option(ktn_value x) {
  return (ktn_option_data *) x;
}

static ktn_vector_data *ktn_get_vector(ktn_value x) {
  return (ktn_vector_data *) x;
}

static ktn_vector_data *ktn_vector_create_from_string(const char *s) {
  size_t length = strlen(s);
  ktn_vector_data *vector = ktn_vector_create_unallocated(length);
  for (size_t i = 0; i < length; ++i) {
    vector->values[i] = ktn_char(s[i]);
  }
  return vector;
}

static ktn_vector_data *ktn_vector_create_unallocated(size_t length) {
  size_t size
    = sizeof(ktn_vector_data)
    - sizeof(((ktn_vector_data *)NULL)->values)
    + sizeof(ktn_value) * length;

  ktn_vector_data *vector = malloc(size);
  vector->length = length;
  /* Don't initialize values; the caller will do that. */

  return vector;
}
