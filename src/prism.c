#include "optyka.h"
#include <raylib.h>
#include <raymath.h>

#define N0 1.f

void calc_prism_pts(prism_data_t *pd)
{
  float a = pd->vert_len,
    h = (sqrt(3) * a)/2.f,
    r = h/3;
  Vector2 S = pd->center;

  pd->p1 = (Vector2) {
    S.x - a/2,
    S.y + r
  };

  pd->p2 = (Vector2) {
    S.x + a/2,
    S.y + r
  };

  pd->p3 = (Vector2) {
    S.x,
    S.y - 2*r
  };
}

void draw_prism(bounceable_t *b)
{
  prism_data_t *pd = b->data.prism;
  extern struct window_conf_t winconf;

  DrawTriangleLines(pd->p1, pd->p2, pd->p3, winconf.prism_outline_color);
}

typedef struct {
  Vector2 p1, p2;
} Line;


static source_t prism_mk_source(void)
{
  return (source_t) {
    .angle = 0,
    .mouse_reactive = false,
    .n_beam = 1,
    .size = 1,
    .thickness = 1
  };
}

static void prism_cast_colors(Color base_color, Vector2 pt, float base_ang)
{
  source_t r = prism_mk_source(), g = prism_mk_source(), b = prism_mk_source();
  r.pt = g.pt = b.pt = pt;

  r.color = (Color){base_color.r, 0, 0, 255};
  r.target = create_target(pt, base_ang);

  g.color = (Color){0, base_color.g, 0, 255};
  g.target = create_target(pt, base_ang + 1);

  b.color = (Color){0, 0, base_color.b, 255};
  b.target = create_target(pt, base_ang + 2);

  if (r.color.r || r.color.g || r.color.b)
    draw_light(&r);
  if (g.color.r || g.color.g || g.color.b)
    draw_light(&g);
  if (b.color.r || b.color.g || b.color.b)
    draw_light(&b);
}

Vector2 prism_create_target(bounceable_t *b, Vector2 cur, Vector2 next, struct _teleport *tp, source_t *src)
{
  prism_data_t *pd = b->data.prism;
  float phi = pd->phi;
  float n = pd->n;
  Line
    A = ((Line){pd->p1, pd->p2}),
    B = ((Line){pd->p2, pd->p3}),
    C = ((Line){pd->p3, pd->p1}),
    *hit;

  if (CheckCollisionPointTriangle(cur, pd->p1, pd->p2, pd->p3))
    return next;

  if (CheckCollisionPointLine(next, A.p1, A.p2, 2)) hit = &A;
  if (CheckCollisionPointLine(next, B.p1, B.p2, 2)) hit = &B;
  if (CheckCollisionPointLine(next, C.p1, C.p2, 2)) hit = &C;

#ifdef COLOR_HIT_PRISM_LINE
  DrawLineV(hit->p1, hit->p2, GREEN);
#endif /* COLOR_HIT_PRISM_LINE */

  float hit_ang  = normalize_angle(Vector2Angle(hit->p1, hit->p2) * RAD2DEG);
  float line_ang = normalize_angle(Vector2Angle(cur, next) * RAD2DEG);
  /* DrawLineV(next, create_target(next, hit_ang + 90), WHITE); */

  float alpha = normalize_angle((hit_ang - 90) - line_ang);
  float beta = asinf((N0/n) * sinf(alpha));
  float delta = (n - 1) * phi;

  Vector2 end_targ = create_target(next, normalize_angle(line_ang - alpha + beta));

  int sw = GetScreenWidth();
  int sh = GetScreenHeight();


  /* DrawCircleV(end_targ, 20, PINK); */
  tp->serio = true;
  tp->luzik = next;
  do {
    tp->luzik = Vector2MoveTowards(tp->luzik, end_targ, 2);
  } while (CheckCollisionPointTriangle(tp->luzik, pd->p1, pd->p2, pd->p3)
           && tp->luzik.x > 0.f
           && tp->luzik.y > 0.f
           && tp->luzik.x < sw
           && tp->luzik.y < sh);

  tp->luzik = Vector2MoveTowards(tp->luzik, end_targ, 2);
  prism_cast_colors(src->color, tp->luzik, line_ang + delta);

  tp->do_trzeciej_warstwy_piekla = true;
  return vec(0, 0);
}
