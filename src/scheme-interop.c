#include "optyka.h"

scheme scm;

// (create-bounceable x1 y1 x2 y2) → nil
static pointer scm_create_bounceable(scheme *sc, pointer args)
{
  int x1, y1, x2, y2;

  if (args != sc->NIL) {
    if (list_length(sc, args) != 4) {
      TraceLog(LOG_WARNING, "create-bounceable called with invalid n of args (expected 4)");
      return sc->NIL;
    }
  }

  x1 = rvalue(car(args));
  y1 = rvalue(cadr(args));
  x2 = rvalue(caddr(args));
  y2 = rvalue(cadddr(args));

  add_bounceable((Vector2){x1,y1}, (Vector2){x2,y2});

  return sc->NIL;
}

// (create-source x y sz angle rel?) → nil
// rel? to #t | #f
static pointer scm_create_source(scheme *sc, pointer args)
{
  float x, y, sz, angle;
  bool rel;

  if (args != sc->NIL) {
    if (list_length(sc, args) != 5) {
      TraceLog(LOG_WARNING, "create-source called with invalid n of args "
          "(expected 5)");

      return sc->NIL;
    }
  }

  x = rvalue(car(args));
  y = rvalue(cadr(args));
  sz = rvalue(caddr(args));
  angle = rvalue(cadddr(args));
  rel = cadddr(cdr(args)) == sc->T;

  add_source((Vector2){x, y}, sz, angle, rel);

  return sc->NIL;
}

static void load_scheme_cfunctions(void)
{
  scheme_define(&scm, scm.global_env, mk_symbol(&scm, "create-bounceable"),
    mk_foreign_func(&scm, scm_create_bounceable));
  TraceLog(LOG_INFO, "defined create-bounceable");

  scheme_define(&scm, scm.global_env, mk_symbol(&scm, "create-source"),
    mk_foreign_func(&scm, scm_create_source));
  TraceLog(LOG_INFO, "defined create-source");
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

  scheme_load_string(&scm, tinyscheme_r5rs_scm);
  TraceLog(LOG_INFO, "loaded builtin r5rs.scm");

  load_scheme_cfunctions();

  scheme_load_string(&scm, "(load \"rc.scm\")");
}
