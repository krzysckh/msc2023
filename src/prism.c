#include "optyka.h"
#include <raymath.h>

#define PRISM_OUTLINE_COLOR ((Color){0xff, 0x00, 0xff, 0xff})

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

  // next - aktualny punkt
  // bo chuj ci w dupe
  // ~ kpm
  if (CheckCollisionPointLine(next, A.p1, A.p2, 2))
    hit = &A, rest[0] = B, rest[1] = C;
  if (CheckCollisionPointLine(next, B.p1, B.p2, 2))
    hit = &B, rest[0] = A, rest[1] = C;
  if (CheckCollisionPointLine(next, C.p1, C.p2, 2))
    hit = &C, rest[0] = A, rest[1] = B;

  DrawLineV(hit->p1, hit->p2, GREEN);
  float tri_ang = normalize_angle(180 + Vector2Angle(hit->p1, hit->p2) * RAD2DEG);
  float line_ang = normalize_angle(Vector2Angle(cur, next) * RAD2DEG);
  float alpha = normalize_angle(90 - (line_ang - tri_ang));
  float epsilon = phi * (n - 1);

  Vector2 targ = create_target(next, normalize_angle(tri_ang - line_ang + alpha));
  DrawCircleV(targ, 16, VIOLET);
  tp->serio = true;
  tp->luzik = next;
  do {
    tp->luzik = Vector2MoveTowards(tp->luzik, targ, 1);
    DrawCircleV(tp->luzik, 6, PINK);
  } while (CheckCollisionPointTriangle(tp->luzik, pd->p1, pd->p2, pd->p3));

  DrawCircleV(tp->luzik, 6, PINK);

  /* return create_target(next, normalize_angle(tri_ang + line_ang + 90 + epsilon)); */
  return create_target(next, normalize_angle(line_ang + epsilon));
}
