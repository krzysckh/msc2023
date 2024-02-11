#include "optyka.h"
#include <raylib.h>
#include <raymath.h>

#define PRISM_OUTLINE_COLOR ((Color){0xff, 0x00, 0xff, 0xff})
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

  DrawTriangleLines(pd->p1, pd->p2, pd->p3, PRISM_OUTLINE_COLOR);
}

typedef struct {
  Vector2 p1, p2;
} Line;

Vector2 prism_create_target(bounceable_t *b, Vector2 cur, Vector2 next, struct _teleport *tp)
{
  prism_data_t *pd = b->data.prism;
  float phi = pd->phi = 60; // IDK LOL
  float n = 1.31;
  Line
    A = ((Line){pd->p1, pd->p2}),
    B = ((Line){pd->p2, pd->p3}),
    C = ((Line){pd->p3, pd->p1}),
    *hit, rest[2];

  if (CheckCollisionPointTriangle(cur, pd->p1, pd->p2, pd->p3))
    return next;

  if (CheckCollisionPointLine(next, A.p1, A.p2, 2)) hit = &A;
  if (CheckCollisionPointLine(next, B.p1, B.p2, 2)) hit = &B;
  if (CheckCollisionPointLine(next, C.p1, C.p2, 2)) hit = &C;

  DrawLineV(hit->p1, hit->p2, GREEN);

  float hit_ang  = normalize_angle(Vector2Angle(hit->p1, hit->p2) * RAD2DEG);
  float line_ang = normalize_angle(Vector2Angle(cur, next) * RAD2DEG);
  /* DrawLineV(next, create_target(next, hit_ang + 90), WHITE); */

  float alpha = normalize_angle((hit_ang - 90) - line_ang);
  float beta = asinf((N0/n) * sinf(alpha));
  float delta = (n - 1) * phi;

  Vector2 end_targ = create_target(next, normalize_angle(line_ang - alpha + beta));

  DrawCircleV(end_targ, 20, PINK);
  tp->serio = true;
  tp->luzik = next;
  do {
    tp->luzik = Vector2MoveTowards(tp->luzik, end_targ, 2);
    DrawCircleV(tp->luzik, 2, PINK);
  } while (CheckCollisionPointTriangle(tp->luzik, pd->p1, pd->p2, pd->p3));
  tp->luzik = Vector2MoveTowards(tp->luzik, end_targ, 2);

  /* DrawLineV(next, create_target(next, line_ang + delta), PINK); */

  /* DrawLineV(next, create_target(next, hit_ang + alpha), VIOLET); */

  /* DrawText(TextFormat("hit_ang: %f", hit_ang), 500, 100, 30, WHITE); */
  /* DrawText(TextFormat("line_ang: %f", line_ang), 500, 300, 30, WHITE); */

  /* DrawText(TextFormat("alpha: %f", alpha), 500, 500, 30, WHITE); */

  return create_target(tp->luzik, line_ang + delta);
}
