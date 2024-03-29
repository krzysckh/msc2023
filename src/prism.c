#include "optyka.h"
#include "raylib.h"
#include "raymath.h"

#include <stdlib.h>
#include <float.h>
#include <stdint.h>

#define N0 1.f

bool is_white(Color c)
{
  int epsilon = 20;
  int min = 50;
  if ((c.r >= c.g - epsilon && c.r <= c.g + epsilon) &&
      (c.g >= c.b - epsilon && c.g <= c.b + epsilon) &&
      (c.r > min))
    return true;

  return false;
}

typedef struct {
  float len;
  Color c;
} len2color_t;

// https://www.partow.net/miscellaneous/colours.html
static len2color_t len2color_vs[] = {
  { 700.000, (Color){255, 000, 000, 255} },
  { 640.427, (Color){255, 031, 000, 255} },
  { 633.790, (Color){255, 063, 000, 255} },
  { 626.205, (Color){255,  95, 000, 255} },
  { 617.938, (Color){255, 127, 000, 255} },
  { 609.126, (Color){255, 159, 000, 255} },
  { 599.855, (Color){255, 191, 000, 255} },
  { 590.184, (Color){255, 223, 000, 255} },
  { 580.159, (Color){255, 255, 000, 255} },
  { 569.363, (Color){223, 255, 000, 255} },
  { 558.936, (Color){191, 255, 000, 255} },
  { 548.938, (Color){159, 255, 000, 255} },
  { 539.431, (Color){127, 255, 000, 255} },
  { 530.508, (Color){ 95, 255, 000, 255} },
  { 522.313, (Color){063, 255, 000, 255} },
  { 515.126, (Color){031, 255, 000, 255} },
  { 510.028, (Color){000, 255, 000, 255} },
  { 508.593, (Color){000, 255, 031, 255} },
  { 506.550, (Color){000, 255, 063, 255} },
  { 504.217, (Color){000, 255,  95, 255} },
  { 501.673, (Color){000, 255, 127, 255} },
  { 498.961, (Color){000, 255, 159, 255} },
  { 496.109, (Color){000, 255, 191, 255} },
  { 493.133, (Color){000, 255, 223, 255} },
  { 490.049, (Color){000, 255, 255, 255} },
  { 482.402, (Color){000, 223, 255, 255} },
  { 474.954, (Color){000, 191, 255, 255} },
  { 467.812, (Color){000, 159, 255, 255} },
  { 461.022, (Color){000, 127, 255, 255} },
  { 454.648, (Color){000,  95, 255, 255} },
  { 448.795, (Color){000, 063, 255, 255} },
  { 443.661, (Color){000, 031, 255, 255} },
  { 440.020, (Color){000, 000, 255, 255} },
  { 435.779, (Color){031, 000, 255, 255} },
  { 429.652, (Color){063, 000, 255, 255} },
  { 422.651, (Color){ 95, 000, 255, 255} },
  { 417.397, (Color){127, 000, 255, 255} },
  { 414.553, (Color){159, 000, 255, 255} },
  { 412.223, (Color){191, 000, 255, 255} },
  { 410.835, (Color){223, 000, 255, 255} },
  { 409.196, (Color){255, 000, 255, 255} },
  { 405.659, (Color){255, 000, 223, 255} },
  { 402.805, (Color){255, 000, 191, 255} },
  { 397.406, (Color){255, 000, 159, 255} },
};
static int len2color_vs_n = sizeof(len2color_vs)/sizeof(len2color_vs[0]);

// https://en.wikipedia.org/wiki/Type_punning
// pamięć to tylko bajty obok siebie
// a Color to struct z 4 bajtami obok siebie
// rgba
// \__/
//   \            4 * 8 =
//    -------------------vv
#define as_u32(c) (*(uint32_t*)&(c))

float color2wavelen(Color c)
{
  len2color_t *closest = &len2color_vs[0];
  float min = FLT_MAX, cur;
  int i;
  uint32_t lookup_color;

  lookup_color = as_u32(c);

  for (i = 0; i < len2color_vs_n; ++i) {
    cur = MAX(as_u32(len2color_vs[i].c), lookup_color)
      - MIN(as_u32(len2color_vs[i].c), lookup_color);

    if (cur < min)
      min = cur, closest = &len2color_vs[i];
  }

  return closest->len;
}

Color wavelen2rgb(float len)
{
  len2color_t *closest = &len2color_vs[0];
  float min = FLT_MAX, cur;
  int i;

  for (i = 0; i < len2color_vs_n; ++i) {
    cur = fabsf(len2color_vs[i].len - len);
    if (cur < min) {
      min = cur;
      closest = &len2color_vs[i];
    }
  }

  return closest->c;
}

void calc_prism_pts(prism_data_t *pd)
{
  float a = pd->vert_len,
    h = (sqrt(3) * a)/2.f,
    r = h/3;
  Vector2 S = pd->center;

  pd->p1 = (Vector2) {
    S.x,
    S.y - 2*r
  };

  pd->p3 = (Vector2) {
    S.x - a/2,
    S.y + r
  };

  pd->p2 = (Vector2) {
    S.x + a/2,
    S.y + r
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

// stworzy src->thickness
// tylko gdy is_white(src->color)
static void prism_cast_colors(Vector2 prev, float first_ang, float fin_ang, source_t *src, prism_data_t *pd)
{
  int n = MAX(3, src->thickness);
  int i, step = (700 - 390) / n;
  Color cur_color;
  /* int dist = floor(src->thickness / 2.f); */
  int dist = floor(src->thickness / (float)n);
  float darken_by = 255.f - (src->color.r + src->color.g + src->color.b)/3.f;

  for (i = 0; i < n; ++i) {
    cur_color = wavelen2rgb(390 + i*step);
    source_t s = prism_mk_source();

    int sz = (((float)src->thickness/2)-((i-2)*dist))/sqrt(2);
    Vector2 rot = Vector2Rotate(vec(sz, sz), (-45 - 90 + first_ang) * PI/180);
    Vector2 pt = vec(prev.x + rot.x, prev.y + rot.y);
    Vector2 targ = create_target(pt, first_ang);

    targ = vec(targ.x - (prev.x - pt.x), targ.y - (prev.y - pt.y));

    Vector2 next = pt;

    int max_iter = 128;
    while (!CheckCollisionPointTriangle(next, pd->p1, pd->p2, pd->p3) && max_iter) {
      next = Vector2MoveTowards(next, targ, 1);
      max_iter--;
    }
    // niektóre części dużej wiązki nie są w pryzmacie XDD

    int sw = GetScreenWidth();
    int sh = GetScreenHeight();

    do {
      next = Vector2MoveTowards(next, targ, 2);
    } while (CheckCollisionPointTriangle(next, pd->p1, pd->p2, pd->p3)
             && next.x > 0.f
             && next.y > 0.f
             && next.x < sw
             && next.y < sh);

    cur_color.a = 255.f - darken_by;

    s.pt = next;
    s.color = cur_color;
    s.thickness = 1;
    s.target = create_target(next, fin_ang + i*0.2);

#ifdef DRAW_LINES_INSIDE
    DrawLineEx(pt, next, 2, dim_color(cur_color, 128));
#endif

    draw_light(&s);
  }
}

static float get_alpha(float hit_angle, float ang)
{
  float diff = normalize_angle(ang - hit_angle);
  if ((90.f - diff) > 90)
    return normalize_angle(360 - 90 - diff);

  return fabsf(90.f - diff);
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

  float alpha = get_alpha(hit_ang, line_ang);
  float beta = asinf((N0/n) * sinf(alpha*DEG2RAD))*RAD2DEG;
  float delta = (n - 1) * phi;

  Vector2 end_targ = create_target(next, normalize_angle(line_ang + alpha - beta));

  int sw = GetScreenWidth();
  int sh = GetScreenHeight();

  /* DrawCircleV(end_targ, 20, PINK); */
  tp->serio = true;
  tp->luzik = next;
  /* tp->luzik =  */

  /* DrawLineV(tp->luzik, create_target(tp->luzik, normalize_angle(line_ang + delta)), VIOLET); */
  /* return vec(0,0); */

  if (is_white(src->color)) {
    prism_cast_colors(next, normalize_angle(line_ang + alpha - beta),
                      normalize_angle(line_ang + delta), src, pd);

    tp->do_trzeciej_warstwy_piekla = true;
    return vec(0, 0);
  }

  tp->luzik = Vector2MoveTowards(tp->luzik, end_targ, 2);
  do {
    tp->luzik = Vector2MoveTowards(tp->luzik, end_targ, 2);
  } while (CheckCollisionPointTriangle(tp->luzik, pd->p1, pd->p2, pd->p3)
           && tp->luzik.x > 0.f
           && tp->luzik.y > 0.f
           && tp->luzik.x < sw
           && tp->luzik.y < sh);

  tp->luzik = Vector2MoveTowards(tp->luzik, end_targ, 2);

#ifdef DRAW_LINES_INSIDE
  DrawLineEx(next, tp->luzik, src->thickness, dim_color(src->color, 128));
#endif

  return create_target(tp->luzik, normalize_angle(line_ang + delta));
}

#define φ 60
void add_prism(Vector2 center, int vert_len, float n)
{
  prism_data_t *pd = malloc(sizeof(prism_data_t));

  pd->center = center;
  pd->vert_len = vert_len;
  pd->phi = φ;
  pd->n = n;

  calc_prism_pts(pd);

  add_bounceable(B_PRISM, pd);
}
