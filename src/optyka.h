#include <raylib.h>
#include <raymath.h>
#include <raygui.h>
#include <err.h>

#include "tinyscheme/scheme.h"
#include "tinyscheme/scheme-private.h"

#include "cxr.h"

#define SCREEN_WIDTH 800
#define SCREEN_HEIGHT 600

#define MAX(x,y) (((x)>(y))?(x):(y))
#define MIN(x,y) (((x)<(y))?(x):(y))

#define ctg(x) (pow(tan((x)),-1))

#ifdef PROD
#define panic(fmt,...) (void)(0);
#else
#define panic(fmt, ...) errx(1, "PANIC: " #fmt __VA_OPT__(,) __VA_ARGS__)
#endif

#define TODO(s) panic("TODO: %s", s)

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
  Vector2 p;
  float r1, r2, d, opacity;
} lens_data_t;

typedef struct {
  Vector2 p1, p2;
} mirror_data_t;

typedef struct {
  bounceable_type_t t;
  union {
    mirror_data_t *mirror;
    lens_data_t   *lens;

    void          *p; // dla jakiegokolwiek wskaźnika, żeby nie było mi źle na duszy
  } data;
  //void *data;
} bounceable_t;

typedef struct {
  bool mouse_reactive;
  int size;
  Vector2 target; /* imaginary target */
  Vector2 pt;
  Color color;
  float thickness;
  float angle; // 0-359
} source_t;

typedef struct {
  pointer *hooks;
  int n_hooks;
} hookable_event_t;

float normalize_angle(float f);
float absf(float x);
void add_bounceable(bounceable_type_t t, void *data);
void add_mirror(Vector2 p1, Vector2 p2);
void add_lens(Vector2 p, float r1, float r2, float d, float opacity);
void add_source(source_t s);

void initialize_scheme(void);
pointer scheme_click_info(struct mouse_information_t *mi);
void do_hooks(hookable_event_t *he, pointer args);

void load_compiled_scripts(void);
