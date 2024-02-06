#include <raylib.h>
#include <raymath.h>
/* #include <raygui.h> */
#include <stdbool.h>

#ifndef WIN32
#include <err.h>
#endif

#include "tinyscheme/scheme.h"
#include "tinyscheme/scheme-private.h"

#include "cxr.h"

/* #define SCREEN_WIDTH 800 */
/* #define SCREEN_HEIGHT 600 */

#define MAX(x,y) (((x)>(y))?(x):(y))
#define MIN(x,y) (((x)<(y))?(x):(y))
#define MAX_FONT_SIZE 1024

#define _vec0 Vector2
#define _vec1(a) ((Vector2){a,a})
#define _vec2(a,b)((Vector2){a,b})
#define _vec3(a,b,c)((Vector3){a,b,c})
#define _vec_dispatch(_1, _2, _3, nam, ...) nam
#define vec(...) _vec_dispatch(__VA_ARGS__, _vec3, _vec2, _vec1)(__VA_ARGS__)

// lol
#define Cons(a,b) cons(sc, (a), (b))

#define ctg(x) (pow(tan((x)),-1))

#define SCHEME_FF(f,sym) \
	scheme_define(&scm, scm.global_env, mk_symbol(&scm, sym), \
	  mk_foreign_func(&scm, f)); TraceLog(LOG_INFO, "defined " sym);

#define expect_args(func,n) \
  if (list_length(sc, args) != n) { \
    TraceLog(LOG_WARNING, \
        func " called with invalid n of args (expected " #n ")"); \
    return sc->F; }

#ifdef PROD
#define panic(fmt,...) (void)(0);
#else
// TODO: zrob tak zeby nie trzeba bylo miec  __VA_OPT__ bo chcialbym zeby dalo
// sie to skompilowac uzywajac tcc lol
#define panic(fmt, ...) errx(1, "PANIC: " #fmt __VA_OPT__(,) __VA_ARGS__)
#endif

#define TODO(s) panic("TODO: %s", s)

typedef enum {
  state_running = 0,
  state_stopped = 1
} sim_state_t;

struct window_conf_t {
  Color bgcolor;
  Color mirror_color;

  sim_state_t state;
};

struct mouse_information_t {
  bool first_click;
  bool pressed_moving;
  bool left;
  bool right;
  Vector2 pos;

  int _dx, _dy; /* the javascript way */
  void *_currently_moving;
};

typedef enum {
  B_MIRROR,
  B_LENS
} bounceable_type_t;

typedef struct {
  Vector2 p1, p2;
  float r1, r2, d, n, opacity;

  /* reszta jest obliczana przez program */
  Vector2 focal_point1, focal_point2, center;
  float f;
} lens_data_t;

typedef struct {
  Vector2 p1, p2;
} mirror_data_t;

typedef struct {
  bounceable_type_t t;
  union {
    mirror_data_t *mirror;
    lens_data_t   *lens;

    void          *p;
    // p dla jakiegokolwiek wskaźnika, żeby nie było mi źle na duszy
  } data;
} bounceable_t;

typedef struct {
  bool mouse_reactive;
  int size;
  Vector2 target; /* imaginary target */
  Vector2 pt;
  Color color;
  float thickness;
  float angle; // 0-359

  int n_beam;
} source_t;

typedef struct {
  pointer *hooks;
  int n_hooks;
} hookable_event_t;

float normalize_angle(float f);
float absf(float x);
void add_bounceable(bounceable_type_t t, void *data);
void add_mirror(Vector2 p1, Vector2 p2);
void add_lens(Vector2 p1, Vector2 p2, float r1, float r2, float d, float n, float opacity);
void add_source(source_t s);
Font get_font_with_size(int size);

void initialize_scheme(void);
void load_rc(void);
pointer scheme_click_info(struct mouse_information_t *mi);
void do_hooks(hookable_event_t *he, pointer args);
pointer ncdr(int n, pointer x);
void update_screen_size_variables(void);

void load_compiled_scripts(void);
