#include <raylib.h>
#include <raymath.h>
#include <raygui.h>

#include "tinyscheme/scheme.h"
#include "tinyscheme/scheme-private.h"

#include "cxr.h"

#define ctg(x) (pow(tan((x)),-1))

struct mouse_information_t {
  bool first_click;
  bool pressed_moving;
  Vector2 pos;

  int _dx, _dy; /* the javascript way */
  void *_currently_moving;
};

typedef struct {
  Vector2 p1, p2;
} bounceable_t;

typedef struct {
  bool mouse_reactive;
  int size;
  Vector2 target; /* imaginary target */
  Vector2 pt;
  float thickness;
  float angle; // 0-359
} source_t;

typedef struct {
  pointer *hooks;
  int n_hooks;
} hookable_event_t;

float normalize_angle(float f);
float absf(float x);
void add_bounceable(Vector2 p1, Vector2 p2);
void add_source(source_t s);

void initialize_scheme(void);
void do_hooks(hookable_event_t *he, pointer args);

void load_compiled_scripts(void);
