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

#define Cons(a,b) cons(sc, (a), (b))
#define MKI(n) mk_integer(sc, (n))
#define MR(f) mk_real(sc, (f))
#define vec2cons(v) Cons(MKI((v).x), MKI((v).y))
#define cons2vec(s) ((Vector2){rvalue(car(s)), rvalue(cdr(s))})

#define ctg(x) (pow(tan((x)),-1))

#define SCHEME_FF(f,sym) \
	scheme_define(&scm, scm.global_env, mk_symbol(&scm, sym), \
	  mk_foreign_func(&scm, f)); TraceLog(LOG_INFO, "defined " sym);

#define expect_args(func,n) \
  if (list_length(sc, args) != n) { \
    TraceLog(LOG_WARNING, \
        func " called with invalid n of args (expected " #n ")"); \
    return sc->F; }

/*
  typedef struct {
    TYPE *v;
    int n;
    int size;
  } Arr;
*/
#define DYN_BUMP_SIZE 512
#define dyn_add_ptr_sized(d, val, s)                            \
  if (d->n + 1 > d->size) {                                     \
    d->v = realloc(d->v, (s) * (d->size + DYN_BUMP_SIZE));      \
    d->size += DYN_BUMP_SIZE;                                   \
  }                                                             \
  d->v[d->n++] = (val);

#define dyn_add_ptr(d, val) dyn_add_ptr_sized((d), (val), (sizeof(d->v[0])))
#define dyn_add(d, val) dyn_add_ptr((&d), (val))

#ifdef PROD
#define panic(fmt,...) (void)(0);
#else
#define panic(...) errx(1, __VA_ARGS__);
#endif

#define TODO(s) panic("TODO: %s", s)

typedef enum {
  state_running = 0,
  state_stopped = 1
} sim_state_t;

struct window_conf_t {
  Color bgcolor;
  Color mirror_color;
  Color prism_outline_color;

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
  B_LENS,
  B_PRISM,
  B_CUSTOM,
} bounceable_type_t;

typedef struct {
  pointer draw;
  pointer remap;

  Vector2 *poly;
  int poly_pts;
} customb_data_t;

typedef struct {
  Vector2 center;
  int vert_len;
  float phi;
  float n;

  /* obliczane przez program */
  Vector2 p1, p2, p3;
} prism_data_t;

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
  bool removed;
  union {
    mirror_data_t  *mirror;
    lens_data_t    *lens;
    prism_data_t   *prism;
    customb_data_t *custom;

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

// tego sluchalem piszac ten kod:
//   https://open.spotify.com/track/5X42tJfGZcsEWQTMtJaa19?si=db1c99b60b364e5a
// ~ kpm
// PROSZE NIE BRAC ZE MNIE PRZYKLADU I KOD NAPISaNY W TYM COMMICIE CO TA WIADOMOSC CZYTAC NA WLASNA ODPOWIEDZIALNOSC
struct _teleport {
  bool serio;
  Vector2 luzik;
};
// XDD D JAK CI ZOSTANIE CZAS KRZYSZTOF TO PROSZE ZROB PORTALE
// PROSZE PROSZE PROSZE PROSZE
// ~ KPM

/* dyn arrs */
typedef struct {
  bounceable_t *v;
  int n;
  int size;
} Bounceables;

typedef struct {
  pointer *v;
  int n;
  int size;
} hookable_event_t;

// main.c
float normalize_angle(float f);
void add_bounceable(bounceable_type_t t, void *data);
void add_mirror(Vector2 p1, Vector2 p2);
void add_lens(Vector2 p1, Vector2 p2, float r1, float r2, float d, float n, float opacity);
void add_prism(Vector2 center, int vert_len, float n);
void add_source(source_t s);
Font get_font_with_size(int size);
Vector2 create_target(Vector2 a, float angle);
bool cast_light(Vector2 target, Vector2 source, Vector2 *ret, bounceable_t *hit_bounceable);
bool collision_point_poly(Vector2 point, Vector2 *points, int pointCount);

// scheme-interop.c
void initialize_scheme(void);
void load_rc(void);
pointer scheme_click_info(struct mouse_information_t *mi);
void do_hooks(hookable_event_t *he, pointer args);
pointer ncdr(int n, pointer x);
void update_screen_size_variables(void);

void load_compiled_scripts(void);

// prism.c
void draw_prism(bounceable_t *b);
void calc_prism_pts(prism_data_t *pd);
Vector2 prism_create_target(bounceable_t *b, Vector2 cur, Vector2 next, struct _teleport *tp);

// custom.c
void draw_custom(bounceable_t *b);
void custom_get_light_remap(customb_data_t *cd, Vector2 pt, float ang, struct _teleport *tp, float *ret_ang);
