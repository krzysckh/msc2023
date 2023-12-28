#include "optyka.h"

#include <string.h>
#include <stdlib.h>

scheme scm;
hookable_event_t keypress = {0};
hookable_event_t click    = {0};
hookable_event_t unclick  = {0};

hookable_event_t frame    = {0};
// TODO: one będą bardzo zwalniać rysowanie. zrób coś z tym krzysztof.

pointer ncdr(int n, pointer x) {
  while (n--)
    x = cdr(x);

  return x;
}

struct hlist_el {
  char *nam;
  hookable_event_t *he;
};

static struct hlist_el hookable_events_list[] = {
  {"keypress", &keypress},
  {"click",    &click},
  {"unclick",  &unclick},
  {"frame",    &frame},
};
static int n_hookable_events = sizeof(hookable_events_list)/
  sizeof(*hookable_events_list);

void do_hooks(hookable_event_t *he, pointer args)
{
  int i;
  for (i = 0; i < he->n_hooks; ++i) {
    if (he->hooks[i])
      scheme_call(&scm, he->hooks[i], args);
  }
}

#define BUF_STEP 512
static pointer scm_system(scheme *sc, pointer args)
{
  int bufsiz = 0, n;
  char *buf = NULL, *command;
  FILE *f;

  expect_args("system", 1);
  command = string_value(car(args));

  f = popen(command, "r");
  while (!feof(f)) {
    buf = realloc(buf, bufsiz + BUF_STEP);
    n = fread(buf + bufsiz, 1, BUF_STEP, f);
    buf[bufsiz + n] = 0;
    bufsiz += BUF_STEP;
  }
  pclose(f);

  return mk_string(sc, buf);
}
#undef BUF_STEP

__attribute__((noreturn)) static pointer scm_exit(scheme *sc, pointer args)
{
  int status = 0;

  // nie robi expect_args()

  if (list_length(sc, args) > 0)
    status = rvalue(car(args));

  TraceLog(LOG_INFO, "exiting with status %d from scheme script.", status);

  exit(status);
}

// (loads s) → nil
static pointer scm_loads(scheme *sc, pointer args)
{
  char *s;

  expect_args("loads", 1);
  s = string_value(car(args));

  scheme_load_string(sc, s);
  return sc->NIL;
}

// (real-draw-text text x y sz spacing r g b a) → #t
static pointer scm_draw_text(scheme *sc, pointer args)
{
  extern Font default_font;
  float x, y, sz, spacing, r, g, b, a;
  char *s;

  expect_args("real-draw-text", 9);

  s       = string_value(car(args));
  x       = rvalue(cadr(args));
  y       = rvalue(caddr(args));
  sz      = rvalue(cadddr(args));
  spacing = rvalue(cadddr(cdr(args)));
  r       = rvalue(cadddr(cddr(args)));
  g       = rvalue(cadddr(cdddr(args)));
  b       = rvalue(cadddr(cddddr(args)));
  a       = rvalue(cadddr(cddddr(cdr(args))));

  DrawTextEx(default_font, s, (Vector2){x,y}, sz, spacing, (Color){r,g,b,a});

  return sc->T;
}

// (real-measure-text text size spacing) → size
static pointer scm_measure_text(scheme *sc, pointer args)
{
  extern Font default_font;
  char *s;
  float sz, spacing;
  Vector2 ret;

  expect_args("measure-text", 3);

  s = string_value(car(args));
  sz = rvalue(cadr(args));
  spacing = rvalue(caddr(args));

  ret = MeasureTextEx(default_font, s, sz, spacing);

  return cons(sc, mk_integer(sc, ret.x), mk_integer(sc, ret.y));
}

// (real-set-source! n x y angle thickness r g b a)
static pointer scm_set_source(scheme *sc, pointer args)
{
  extern int N_SOURCES;
  extern source_t *sources;
  source_t *s;

  int n;
  float x, y, angle, thickness, r, g, b, a;

  expect_args("real-set-source!", 9);

  n         = rvalue(car(ncdr(0, args)));
  x         = rvalue(car(ncdr(1, args)));
  y         = rvalue(car(ncdr(2, args)));
  angle     = rvalue(car(ncdr(3, args)));
  thickness = rvalue(car(ncdr(4, args)));
  r         = rvalue(car(ncdr(5, args)));
  g         = rvalue(car(ncdr(6, args)));
  b         = rvalue(car(ncdr(7, args)));
  a         = rvalue(car(ncdr(8, args)));

  if (n >= N_SOURCES) {
    TraceLog(LOG_WARNING, "no such source: %d", n);
    return sc->F;
  }

  s = &sources[n];
  s->pt.x = x, s->pt.y = y, s->angle = angle,
  s->thickness = thickness, s->color = (Color){r,g,b,a};

  return sc->T;
}

// (real-get-source n) → (x y sz angle thickness r g b a)
static pointer scm_get_source(scheme *sc, pointer args)
{
  extern int N_SOURCES;
  extern source_t *sources;

  float n;
  source_t *s;

  expect_args("get-source", 1);
  n = rvalue(car(args));

  if (n > N_SOURCES) {
    TraceLog(LOG_WARNING, "get-source: no such source: %d", n);
    return sc->F;
  }
  s = &sources[(int)n];

  return cons(sc,
    cons(sc, mk_integer(sc, s->pt.x), mk_integer(sc, s->pt.y)),
      cons(sc, mk_integer(sc, s->angle),
        cons(sc, mk_integer(sc, s->thickness),
          cons(sc, cons(sc, mk_integer(sc, s->color.r),
              cons(sc, mk_integer(sc, s->color.g),
                cons(sc, mk_integer(sc, s->color.b),
                  cons(sc, mk_integer(sc, s->color.a), sc->NIL)))), sc->NIL))));
}

// (get-all-sources) → '((id x y sz...) ...)
static pointer scm_get_all_sources(scheme *sc, pointer args)
{
  extern int N_SOURCES;
  extern source_t *sources;
  (void)args;

  pointer ret, cur;
  int i;

  if (N_SOURCES < 1)
    return sc->F;

  ret = cons(sc, scm_get_source(sc, cons(sc, mk_integer(sc, 0), sc->NIL)),
    sc->NIL);
  cur = ret;

  for (i = 1; i < N_SOURCES; ++i) {
    set_cdr(cur, cons(sc, scm_get_source(sc, cons(sc, mk_integer(sc, i),
      sc->NIL)), sc->NIL));
    cur = cdr(cur);
  }

  return ret;
}

// (real-draw-line x1 y1 x2 y2 thickness r g b a) → nil
static pointer scm_draw_line(scheme *sc, pointer args)
{
  float x1, y1, x2, y2, thick, r, g, b, a;

  expect_args("real-draw-line", 9);

  x1    = rvalue(car(args));
  y1    = rvalue(cadr(args));
  x2    = rvalue(caddr(args));
  y2    = rvalue(cadddr(args));
  thick = rvalue(cadddr(cdr(args)));
  r     = rvalue(cadddr(cddr(args)));
  g     = rvalue(cadddr(cdddr(args)));
  b     = rvalue(cadddr(cddddr(args)));
  a     = rvalue(cadddr(cddddr(cdr(args)))); // swiete gowno

  DrawLineEx((Vector2){ x1, y1 }, (Vector2){ x2, y2 }, thick,
    (Color){ r, g, b, a });

  return sc->NIL;
}

static hookable_event_t *get_he_by_name(char *name)
{
  int i;
  for (i = 0; i < n_hookable_events; ++i)
    if (strcmp(hookable_events_list[i].nam, name) == 0)
      return hookable_events_list[i].he;

  return NULL;
}

// (delete-hook sym id) → nil
static pointer scm_delete_hook(scheme *sc, pointer args)
{
  char *sym;
  int id;//, i;
  hookable_event_t *he;

  expect_args("delete-hook", 2);
  sym = symname(car(args));
  id = rvalue(cadr(args));

  he = get_he_by_name(sym);
  if (!he) {
    TraceLog(LOG_WARNING, "delete-hook: no such hookable event: %s", sym);
    return sc->F;
  }

  if (id > he->n_hooks) {
    TraceLog(LOG_ERROR, "cannot delete hook %d: no such hook", id);
    return sc->F;
  }

  he->hooks[id] = NULL;

  TraceLog(LOG_INFO, "deleted hook %d for %s", id, sym);
  return sc->T;
}

// (real-add-hook 'type f) → #t | #f
static pointer scm_add_hook(scheme *sc, pointer args)
{
  char *name;
  pointer f;
  hookable_event_t *he;

  expect_args("add-hook", 2);

  name = symname(car(args));
  f = cadr(args);

  if (!is_closure(f)) {
    TraceLog(LOG_WARNING, "add-hook's 2nd arg has to be a function");
    return sc->F;
  }

  he = get_he_by_name(name);
  if (!he) {
    TraceLog(LOG_WARNING, "add-hook: no such hookable event: %s", name);
    return sc->F;
  }

  he->hooks = realloc(he->hooks, (1 + he->n_hooks) * sizeof(pointer));
  he->hooks[he->n_hooks] = f;
  he->n_hooks++;

  TraceLog(LOG_INFO, "successfully added hook %p for %s", f, name);
  return mk_integer(sc, he->n_hooks - 1);
}

// (get-mouse-position) → '(x y)
static pointer scm_get_mouse_position(scheme *sc, pointer args)
{
  Vector2 pos = GetMousePosition();
  (void)args;

  return cons(sc, mk_integer(sc, pos.x), mk_integer(sc, pos.y));
}

// (create-mirror x1 y1 x2 y2) → nil
static pointer scm_create_mirror(scheme *sc, pointer args)
{
  int x1, y1, x2, y2;

  expect_args("create-mirror", 4);

  x1 = rvalue(car(args));
  y1 = rvalue(cadr(args));
  x2 = rvalue(caddr(args));
  y2 = rvalue(cadddr(args));

  add_mirror((Vector2){x1,y1}, (Vector2){x2,y2});

  return sc->NIL;
}

// (real-create-source x y sz angle thickness reactive r g b a) → nil
// reactive? to #t | #f
static pointer scm_create_source(scheme *sc, pointer args)
{
  float x, y, sz, thickness, angle, r, g, b, a;
  bool rel;

  expect_args("real-create-source", 10);

  x = rvalue(car(args));
  y = rvalue(cadr(args));
  sz = rvalue(caddr(args));
  angle = rvalue(cadddr(args));
  thickness = rvalue(cadddr(cdr(args)));
  rel = cadddr(cddr(args)) == sc->T;
  r = rvalue(cadddr(cdddr(args)));
  g = rvalue(cadddr(cddddr(args)));
  b = rvalue(cadddr(cddddr(cdr(args))));
  a = rvalue(cadddr(cddddr(cddr(args)))); // lol

  add_source((source_t){
    .mouse_reactive = rel,
    .size = sz,
    .pt = (Vector2){x, y},
    .thickness = thickness,
    .angle = angle,
    .color = (Color){r,g,b,a}
  });

  return sc->NIL;
}

static void load_scheme_cfunctions(void)
{
  SCHEME_FF(scm_system,             "system");
  SCHEME_FF(scm_exit,               "exit");
  SCHEME_FF(scm_loads,              "loads");
  SCHEME_FF(scm_delete_hook,        "delete-hook");
  SCHEME_FF(scm_measure_text,       "real-measure-text");
  SCHEME_FF(scm_draw_text,          "real-draw-text");
  SCHEME_FF(scm_set_source,         "real-set-source!");
  SCHEME_FF(scm_get_source,         "get-source");
  SCHEME_FF(scm_get_all_sources,    "get-all-sources");
  SCHEME_FF(scm_create_mirror,      "create-mirror");
  SCHEME_FF(scm_create_source,      "real-create-source");
  SCHEME_FF(scm_add_hook,           "real-add-hook");
  SCHEME_FF(scm_get_mouse_position, "get-mouse-position");
  SCHEME_FF(scm_draw_line,          "real-draw-line");
}

void initialize_scheme(void)
{
  extern char tinyscheme_r5rs_scm[];
  FILE *rc = fopen("rc.scm", "r+");

  scheme_init(&scm);
  TraceLog(LOG_INFO, "loaded tinyscheme");

  scheme_set_input_port_file(&scm, stdin);
  scheme_set_output_port_file(&scm, stdout);

  scheme_define(&scm, scm.global_env, mk_symbol(&scm, "*version*"),
    mk_string(&scm, "0.0"));

  load_scheme_cfunctions();
  load_compiled_scripts();

  if (rc)
    scheme_load_file(&scm, rc);
}

pointer scheme_click_info(struct mouse_information_t *mi)
{
  return cons(&scm, mi->first_click ? scm.T : scm.F,
      cons(&scm, mi->left ? scm.T : scm.F,
        cons(&scm, mi->right ? scm.T : scm.F, scm.NIL)));
}
