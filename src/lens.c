#include "optyka.h"
#include "raylib.h"

#include <raymath.h>
#include <stdlib.h>

extern struct window_conf_t winconf;

bool collision_point_ellipse(Vector2 point, Vector2 center, float rH, float rV)
{
  float dx = point.x - center.x;
  float dy = point.y - center.y;

  return ((dx * dx) / (rH * rH) + (dy * dy) / (rV * rV) <= 1);
}

bool collision_point_lens(Vector2 pt, lens_data_t *ld)
{
  if (pt.x < ld->center.x)
    return collision_point_ellipse(pt, (Vector2){ld->center.x, ld->center.y}, ld->r1, ld->center.y - ld->p1.y);
  else if (pt.x > ld->center.x)
    return collision_point_ellipse(pt, (Vector2){ld->p1.x, ld->center.y}, ld->r2, ld->center.y - ld->p1.y);
  else if (pt.y >= ld->p1.y && pt.y <= ld->p2.y) // i pt.x == ld->p{1,2}.x
    return true;
  return false;
}


void draw_lens(bounceable_t *b)
{
  lens_data_t *ld = b->data.lens;
  DrawCircleV(ld->focal_point1, 2, winconf.lens_focal_pt_color);
  DrawCircleV(ld->focal_point2, 2, winconf.lens_focal_pt_color);

  BeginScissorMode(ld->p1.x - ld->r1, ld->p1.y, ld->r1, ld->p2.y - ld->p1.y);
  {
    DrawEllipseLines(ld->center.x, ld->center.y, ld->r1, ld->center.y - ld->p1.y, winconf.lens_outline_color);
  }
  EndScissorMode();

  BeginScissorMode(ld->p1.x, ld->p1.y, ld->r2, ld->p2.y - ld->p1.y);
  {
    DrawEllipseLines(ld->center.x, ld->center.y, ld->r2, ld->center.y - ld->p1.y, winconf.lens_outline_color);
  }
  EndScissorMode();

#ifdef LENS_CENTER
  DrawLineEx(p1, p2, 2, winconf.lens_center_color);
#endif // LENS_CENTER
}

void calc_lens_stuff(lens_data_t *ld)
{
  ld->f = 1.f / ((1.f / ld->r1) + (1.f / ld->r2));
  ld->center = vec(ld->p1.x + (ld->p2.x - ld->p1.x)/2,
                   ld->p1.y + (ld->p2.y - ld->p1.y)/2);

  ld->focal_point1 = vec(ld->center.x - ld->f, ld->center.y);
  ld->focal_point2 = vec(ld->center.x + ld->f, ld->center.y);
  // TODO: nieprawda!!
}

void add_lens(Vector2 p1, Vector2 p2, float r1, float r2)
{
  lens_data_t *ld = malloc(sizeof(lens_data_t));
  ld->r1 = r1, ld->r2 = r2, ld->p1 = p1, ld->p2 = p2;

  calc_lens_stuff(ld);
  // p1.x == p2.x i p1.y < p2.y
  // :3333

  add_bounceable(B_LENS, ld);
}


Vector2 lens_create_target(lens_data_t *ld, Vector2 cur, Vector2 next, struct _teleport *tp)
{
  float hit_angle = normalize_angle(Vector2Angle(ld->p1, ld->p2) * RAD2DEG);
  float rel_angle = normalize_angle(hit_angle - normalize_angle(Vector2Angle(cur, next) * RAD2DEG));
  assert(ld->p1.x <= ld->p2.x);
  assert(ld->p1.y <= ld->p2.y);

  Vector2 targ;
  if (rel_angle < 180)
    targ = create_target(ld->focal_point2, normalize_angle(Vector2Angle(next, ld->focal_point2) * RAD2DEG));
  else
    targ = create_target(ld->focal_point1, normalize_angle(Vector2Angle(next, ld->focal_point1) * RAD2DEG));

  tp->serio = true;
  tp->luzik = next;
  do {
    tp->luzik = Vector2MoveTowards(tp->luzik, targ, 5);
  } while (collision_point_lens(tp->luzik, ld));

  return targ;
}
