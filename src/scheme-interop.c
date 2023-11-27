#include "optyka.h"

#include <string.h>
#include <stdlib.h>

scheme scm;
hookable_event_t keypress = {
  .hooks = NULL,
  .n_hooks = 0
};

hookable_event_t click = {
  .hooks = NULL,
  .n_hooks = 0
};

hookable_event_t unclick = {
  .hooks = NULL,
  .n_hooks = 0
};


struct hlist_el {
  char *nam;
  hookable_event_t *he;
};

static struct hlist_el hookable_events_list[] = {
  {"keypress", &keypress},
  {"click", &click},
  {"unclick", &unclick},
};
static int n_hookable_events = sizeof(hookable_events_list)/
  sizeof(*hookable_events_list);

void do_hooks(hookable_event_t *he, pointer args)
{
  int i;
  for (i = 0; i < he->n_hooks; ++i) {
    scheme_call(&scm, he->hooks[i], args);
  }
}

#define expect_args(func,n) \
  if (list_length(sc, args) != n) { \
    TraceLog(LOG_WARNING, \
        func " called with invalid n of args (expected " #n ")"); \
    return sc->F; }

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

  expect_args("measure-text", 2);

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

  // TODO: boze krzysztof napisz ncar(n,lst)
  n         = rvalue(car(args));
  x         = rvalue(cadr(args));
  y         = rvalue(caddr(args));
  angle     = rvalue(cadddr(args));
  thickness = rvalue(cadddr(cdr(args)));
  r         = rvalue(cadddr(cddr(args)));
  g         = rvalue(cadddr(cdddr(args)));
  b         = rvalue(cadddr(cddddr(args)));
  a         = rvalue(cadddr(cddddr(cdr(args))));

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

// (real-add-hook 'type f) → #t | #f
static pointer scm_add_hook(scheme *sc, pointer args)
{
  char *name;
  pointer f;
  int i;
  hookable_event_t *he;

  expect_args("add-hook", 2);

  name = symname(car(args));
  f = cadr(args);

  if (!is_closure(f)) {
    TraceLog(LOG_WARNING, "add-hook's 2nd arg has to be a function");
    return sc->F;
  }

  for (i = 0; i < n_hookable_events; ++i) {
    if (strcmp(hookable_events_list[i].nam, name) == 0) {
      he = hookable_events_list[i].he;
      he->hooks = realloc(he->hooks, (1 + he->n_hooks) * sizeof(pointer));
      he->hooks[he->n_hooks] = f;
      he->n_hooks++;

      return sc->T;
    }
  }

  TraceLog(LOG_WARNING, "add-hook: no such hookable event: %s", name);
  return sc->F;
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

#define SCHEME_FF(f,sym) \
	scheme_define(&scm, scm.global_env, mk_symbol(&scm, sym), \
	  mk_foreign_func(&scm, f)); TraceLog(LOG_INFO, "defined " sym);

static void load_scheme_cfunctions(void)
{
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

  scheme_init(&scm);
  TraceLog(LOG_INFO, "loaded tinyscheme");

  scheme_set_input_port_file(&scm, stdin);
  scheme_set_output_port_file(&scm, stdout);

  scheme_define(&scm, scm.global_env, mk_symbol(&scm, "*version*"),
    mk_string(&scm, "0.0"));

  load_scheme_cfunctions();
  load_compiled_scripts();

  scheme_load_string(&scm, "(load \"rc.scm\")");
}

pointer scheme_click_info(struct mouse_information_t *mi)
{
  return cons(&scm, mi->first_click ? scm.T : scm.F,
      cons(&scm, mi->left ? scm.T : scm.F,
        cons(&scm, mi->right ? scm.T : scm.F, scm.NIL)));
}
