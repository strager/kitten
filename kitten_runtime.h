#ifndef KITTEN_RUNTIME_H_61C99F37_60E6_4C69_9E81_0822EB7CDBDB
#define KITTEN_RUNTIME_H_61C99F37_60E6_4C69_9E81_0822EB7CDBDB

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

#if defined(__GNUC__) || defined(__clang__)
#define KTN_WARN_UNUSED_RESULT __attribute__((warn_unused_result))
#else
#define KTN_WARN_UNUSED_RESULT /* */
#endif

typedef bool ktn_bool_type;
typedef uint32_t ktn_char_type;
typedef double ktn_float_type;
typedef FILE *ktn_handle_type;
typedef int64_t ktn_int_type;

typedef void *ktn_value;

/*
 * A closure function accepts a ktn_activation as its first
 * argument and its formal parameters as the remaining
 * arguments.
 */
typedef void (*ktn_closure_function)(void);

/*
 * Transparent closure activation, containing the closure
 * function and the captured values. Coercable to ktn_value.
 */
typedef struct {
  ktn_closure_function function;
  ktn_value values[1]; /* Variadic. */
} ktn_activation;

#define KTN_DEFINE_TUPLE(size) \
  typedef struct { \
    ktn_value values[size]; \
  } ktn_tuple##size

KTN_DEFINE_TUPLE(2);
KTN_DEFINE_TUPLE(3);
KTN_DEFINE_TUPLE(4);
KTN_DEFINE_TUPLE(5);
KTN_DEFINE_TUPLE(6);
KTN_DEFINE_TUPLE(7);
KTN_DEFINE_TUPLE(8);
KTN_DEFINE_TUPLE(9);

#undef KTN_DEFINE_TUPLE

#if defined(__cplusplus)
extern "C" {
#endif

ktn_value ktn_builtin_addFloat       (ktn_value, ktn_value            ) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_builtin_addInt         (ktn_value, ktn_value            ) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_builtin_addVector      (ktn_value, ktn_value            ) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_builtin_andBool        (ktn_value, ktn_value            ) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_builtin_andInt         (ktn_value, ktn_value            ) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_builtin_charToInt      (ktn_value                       ) KTN_WARN_UNUSED_RESULT;
void      ktn_builtin_close          (ktn_value                       );
ktn_value ktn_builtin_divFloat       (ktn_value, ktn_value            ) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_builtin_divInt         (ktn_value, ktn_value            ) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_builtin_eqFloat        (ktn_value, ktn_value            ) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_builtin_eqInt          (ktn_value, ktn_value            ) KTN_WARN_UNUSED_RESULT;
void      ktn_builtin_exit           (ktn_value                       );
ktn_value ktn_builtin_first          (ktn_value                       ) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_builtin_fromLeft       (ktn_value                       ) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_builtin_fromRight      (ktn_value                       ) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_builtin_fromSome       (ktn_value                       ) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_builtin_geFloat        (ktn_value, ktn_value            ) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_builtin_geInt          (ktn_value, ktn_value            ) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_builtin_get            (ktn_value, ktn_value            ) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_builtin_getLine        (ktn_value                       ) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_builtin_gtFloat        (ktn_value, ktn_value            ) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_builtin_gtInt          (ktn_value, ktn_value            ) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_builtin_init           (ktn_value                       ) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_builtin_intToChar      (ktn_value                       ) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_builtin_leFloat        (ktn_value, ktn_value            ) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_builtin_leInt          (ktn_value, ktn_value            ) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_builtin_left           (ktn_value                       ) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_builtin_length         (ktn_value                       ) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_builtin_ltFloat        (ktn_value, ktn_value            ) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_builtin_ltInt          (ktn_value, ktn_value            ) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_builtin_modFloat       (ktn_value, ktn_value            ) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_builtin_modInt         (ktn_value, ktn_value            ) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_builtin_mulFloat       (ktn_value, ktn_value            ) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_builtin_mulInt         (ktn_value, ktn_value            ) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_builtin_neFloat        (ktn_value, ktn_value            ) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_builtin_neInt          (ktn_value, ktn_value            ) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_builtin_negFloat       (ktn_value                       ) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_builtin_negInt         (ktn_value                       ) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_builtin_none           (void                            ) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_builtin_notBool        (ktn_value                       ) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_builtin_notInt         (ktn_value                       ) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_builtin_openIn         (void                            ) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_builtin_openOut        (void                            ) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_builtin_orBool         (ktn_value, ktn_value            ) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_builtin_orInt          (ktn_value, ktn_value            ) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_builtin_pair           (ktn_value, ktn_value            ) KTN_WARN_UNUSED_RESULT;
void      ktn_builtin_print          (ktn_value, ktn_value            );
ktn_value ktn_builtin_rest           (ktn_value                       ) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_builtin_right          (ktn_value                       ) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_builtin_set            (ktn_value, ktn_value, ktn_value ) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_builtin_showFloat      (ktn_value                       ) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_builtin_showInt        (ktn_value                       ) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_builtin_some           (ktn_value                       ) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_builtin_stderr         (void                            ) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_builtin_stdin          (void                            ) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_builtin_stdout         (void                            ) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_builtin_subFloat       (ktn_value, ktn_value            ) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_builtin_subInt         (ktn_value, ktn_value            ) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_builtin_tail           (ktn_value                       ) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_builtin_unsafePurify11 (ktn_value                       ) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_builtin_xorBool        (ktn_value, ktn_value            ) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_builtin_xorInt         (ktn_value, ktn_value            ) KTN_WARN_UNUSED_RESULT;

ktn_value ktn_act(ktn_closure_function, size_t closed, ...) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_bool(ktn_bool_type) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_char(ktn_char_type) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_false(void) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_float(ktn_float_type) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_handle(FILE *) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_int(ktn_int_type) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_pair(ktn_value, ktn_value) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_true(void) KTN_WARN_UNUSED_RESULT;
ktn_value ktn_vector(size_t, ...) KTN_WARN_UNUSED_RESULT;

ktn_activation *ktn_get_activation(ktn_value) KTN_WARN_UNUSED_RESULT;
ktn_bool_type ktn_get_bool(ktn_value) KTN_WARN_UNUSED_RESULT;
ktn_char_type ktn_get_char(ktn_value) KTN_WARN_UNUSED_RESULT;
ktn_float_type ktn_get_float(ktn_value) KTN_WARN_UNUSED_RESULT;
ktn_handle_type ktn_get_handle(ktn_value) KTN_WARN_UNUSED_RESULT;
ktn_int_type ktn_get_int(ktn_value) KTN_WARN_UNUSED_RESULT;

ktn_value ktn_either_get_value(ktn_value) KTN_WARN_UNUSED_RESULT;
bool ktn_either_is_left(ktn_value) KTN_WARN_UNUSED_RESULT;
bool ktn_either_is_right(ktn_value) KTN_WARN_UNUSED_RESULT;

ktn_value ktn_option_get_value(ktn_value) KTN_WARN_UNUSED_RESULT;
bool ktn_option_is_some(ktn_value) KTN_WARN_UNUSED_RESULT;
bool ktn_option_is_none(ktn_value) KTN_WARN_UNUSED_RESULT;

/* WARNING: GNU extensions ahead! */

#define KTN_DROP_ARG(x, ...) \
  __VA_ARGS__

/*
 * Taken from P99:
 * http://p99.gforge.inria.fr/
 */
#define KTN_PP_HAS_ARGS(...) \
  KTN_PP_HAS_ARGS_IMPL(ignore_me, ##__VA_ARGS__, KTN_PP_HAS_ARGS_SOURCE())
#define KTN_PP_HAS_ARGS_IMPL(ignore_me, ...) \
  KTN_PP_HAS_ARGS_IMPL2(__VA_ARGS__)

#define KTN_PP_HAS_ARGS_SOURCE() \
  MULTI, MULTI, ONE, ZERO
#define KTN_PP_HAS_ARGS_IMPL2(_1, _2, _3, N, ...) \
  N

/*
 * Taken from StackOverflow (Creative Commons CC BY-SA 3.0):
 * http://stackoverflow.com/a/3868404/39992
 */
#define KTN_MAKE_VALUE(...) \
  KTN_MAKE_VALUE_DISAMBIGUATE(KTN_PP_HAS_ARGS(__VA_ARGS__), ##__VA_ARGS__)
#define KTN_MAKE_VALUE_DISAMBIGUATE(has_args, ...) \
  KTN_MAKE_VALUE_DISAMBIGUATE2(has_args, ##__VA_ARGS__)
#define KTN_MAKE_VALUE_DISAMBIGUATE2(has_args, ...) \
  KTN_MAKE_VALUE_##has_args(__VA_ARGS__)

#define KTN_MAKE_VALUE_ZERO() 0
#define KTN_MAKE_VALUE_ONE(x) x
#define KTN_MAKE_VALUE_MULTI(...) { { __VA_ARGS__ } }

/*
 * ktn_tuple2 result = KTN_APPLY(
 *   ktn_tuple2,
 *   (ktn_activation *, ktn_value),
 *   my_activation,
 *   my_parameter);
 */
#define KTN_APPLY(return_type, parameter_types, activation, ...) \
  ({ \
    ktn_activation *_activation \
      = ktn_get_activation(activation); \
    ((return_type (*)parameter_types) _activation->function)( \
      _activation, ##__VA_ARGS__); \
   })

/*
 * ktn_tuple2 result = KTN_IF(
 *   ktn_tuple2,
 *   (ktn_activation *, ktn_value),
 *   true_activation,
 *   my_parameter);
 */
#define KTN_IF(return_type, parameter_types, cond, true_activation, ...) \
  ({ \
    ktn_activation *_activation; \
    ktn_get_bool((cond)) \
      ? (_activation = ktn_get_activation(true_activation)), \
        ((return_type (*)parameter_types) _activation->function)( \
          _activation, ##__VA_ARGS__) \
      : (return_type) KTN_MAKE_VALUE(__VA_ARGS__); \
  })

/*
 * ktn_tuple2 result = KTN_IF_ELSE(
 *   ktn_tuple2,
 *   (ktn_activation *, ktn_value),
 *   true_activation,
 *   false_activation,
 *   my_parameter);
 */
#define KTN_IF_ELSE(return_type, parameter_types, cond, true_activation, false_activation, ...) \
  ({ \
    ktn_activation *_activation \
      = ktn_get_activation(ktn_get_bool((cond)) \
        ? true_activation : false_activation); \
    ((return_type (*)parameter_types) _activation->function)( \
      _activation, ##__VA_ARGS__); \
   })

#define KTN_CHOICE(return_type, parameter_types, either, left_activation, ...) \
  ({ \
    ktn_activation *_activation; \
    ktn_either_is_left((either)) \
      ? (_activation = ktn_get_activation(left_activation)), \
        ((return_type (*)parameter_types) _activation->function)( \
          _activation, ktn_either_get_value((either)), ##__VA_ARGS__) \
      : (return_type) KTN_MAKE_VALUE(__VA_ARGS__); \
  })

#define KTN_CHOICE_ELSE(return_type, parameter_types, either, left_activation, right_activation, ...) \
  ({ \
    ktn_activation *_activation \
      = ktn_get_activation(ktn_either_is_left((either)) \
        ? left_activation : right_activation); \
    ((return_type (*)parameter_types) _activation->function)( \
      _activation, ktn_either_get_value((either)), ##__VA_ARGS__); \
   })

#define KTN_OPTION(return_type, parameter_types, option, some_activation, ...) \
  ({ \
    ktn_activation *_activation; \
    ktn_option_is_some((option)) \
      ? (_activation = ktn_get_activation(some_activation)), \
        ((return_type (*)parameter_types) _activation->function)( \
          _activation, ktn_option_get_value((option)), ##__VA_ARGS__) \
      : (return_type) KTN_MAKE_VALUE(__VA_ARGS__); \
  })

#define KTN_OPTION_ELSE(return_type, some_parameter_types, none_parameter_types, option, some_activation, none_activation, ...) \
  (ktn_option_is_some((option)) \
    ? ({ \
      ktn_activation *_activation \
        = ktn_get_activation(some_activation); \
      ((return_type (*)some_parameter_types) _activation->function)( \
        _activation, ktn_option_get_value((option)), ##__VA_ARGS__); \
    }) : ({ \
      ktn_activation *_activation \
        = ktn_get_activation(none_activation); \
      ((return_type (*)none_parameter_types) _activation->function)( \
        _activation, ##__VA_ARGS__); \
    }))

#if defined(__cplusplus)
}
#endif

#endif
