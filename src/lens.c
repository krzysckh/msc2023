#include "optyka.h"
#include "raylib.h"

#include <raymath.h>
#include <stdlib.h>

extern struct window_conf_t winconf;

bool collision_point_lens(Vector2 pt, lens_data_t *ld)
{
  /* Rectangle rec1 = (Rectangle) { */
  /*   ld->center.x - ld->d/2, */
  /*   ld->center.y - MAX(ld->r1, ld->r2), */
  /*   ld->d/2, */
  /*   2*MAX(ld->r1, ld->r2) */
  /* }; */

  /* Rectangle rec2 = (Rectangle) { */
  /*   ld->center.x, */
  /*   ld->center.y - MAX(ld->r1, ld->r2), */
  /*   ld->d/2, */
  /*   2*MAX(ld->r1, ld->r2) */
  /* }; */

  Rectangle rec1 = (Rectangle) {
    ld->center.x - ld->d/2,
    ld->center.y - MAX(ld->r, ld->r),
    ld->d/2,
    2*MAX(ld->r, ld->r)
  };

  Rectangle rec2 = (Rectangle) {
    ld->center.x,
    ld->center.y - MAX(ld->r, ld->r),
    ld->d/2,
    2*MAX(ld->r, ld->r)
  };

  if (CheckCollisionPointRec(pt, rec1))
    return CheckCollisionPointCircle(pt, vec(ld->center.x + ld->r - ld->d/2, ld->center.y), ld->r);

  if (CheckCollisionPointRec(pt, rec2))
    return CheckCollisionPointCircle(pt, vec(ld->center.x - ld->r + ld->d/2, ld->center.y), ld->r);

  return false;
}

void draw_lens(bounceable_t *b)
{
  lens_data_t *ld = b->data.lens;
  DrawCircleV(ld->focal_point1, 2, winconf.lens_focal_pt_color);
  DrawCircleV(ld->focal_point2, 2, winconf.lens_focal_pt_color);

  float d = ld->d;

  /* DrawRectangle(ld->center.x - d/2, ld->center.y - MAX(ld->r1, ld->r2), d, 2*MAX(ld->r1, ld->r2), WHITE); */
  /*
  BeginScissorMode(ld->center.x - d/2, ld->center.y - MAX(ld->r1, ld->r2), d/2, 2*MAX(ld->r1, ld->r2));
  {
    DrawCircleLines(ld->center.x + ld->r1 - d/2, ld->center.y, ld->r1, winconf.lens_outline_color);
  }
  EndScissorMode();

  BeginScissorMode(ld->center.x, ld->center.y - MAX(ld->r1, ld->r2), d/2, 2*MAX(ld->r1, ld->r2));
  {
    DrawCircleLines(ld->center.x - ld->r2 + d/2, ld->center.y, ld->r2, winconf.lens_outline_color);
  }
  EndScissorMode();
  */

  BeginScissorMode(ld->center.x - d/2, ld->center.y - MAX(ld->r, ld->r), d/2, 2*MAX(ld->r, ld->r));
  {
    DrawCircleLines(ld->center.x + ld->r - d/2, ld->center.y, ld->r, winconf.lens_outline_color);
  }
  EndScissorMode();

  BeginScissorMode(ld->center.x, ld->center.y - MAX(ld->r, ld->r), d/2, 2*MAX(ld->r, ld->r));
  {
    DrawCircleLines(ld->center.x - ld->r + d/2, ld->center.y, ld->r, winconf.lens_outline_color);
  }
  EndScissorMode();

#ifdef LENS_CENTER
  DrawLineEx(p1, p2, 2, winconf.lens_center_color);
#endif // LENS_CENTER
}

void calc_lens_stuff(lens_data_t *ld)
{
  ld->f = 1.f / ((1.f / ld->r) + (1.f / ld->r));
  ld->focal_point1 = vec(ld->center.x - ld->f, ld->center.y);
  ld->focal_point2 = vec(ld->center.x + ld->f, ld->center.y);

  ld->p1 = ld->p2 = ld->center;
  while (collision_point_lens(ld->p1, ld))
    ld->p1.y--;
  while (collision_point_lens(ld->p2, ld))
    ld->p2.y++;
}

void add_lens(Vector2 center, float d, float r)
{
  lens_data_t *ld = malloc(sizeof(lens_data_t));
  ld->center = center, ld->d = d, ld->r = r;

  calc_lens_stuff(ld);
  // p1.x == p2.x i p1.y < p2.y
  // :3333

  add_bounceable(B_LENS, ld);
}

static float get_theta(float ang)
{

  float theta;
  if (ang >= 0.f && ang <= 90.f)
    theta = ang;
  else if (ang > 90.f && ang <= 180.f)
    theta = 180.f - ang;
  else if (ang > 180.f && ang <= 270.f)
    theta = 90.f - (270 - ang);
  else
    theta = 360.f - ang;

  theta = normalize_angle(theta);

  return theta;
}

// via src/lens.png
Vector2 lens_create_target(lens_data_t *ld, Vector2 cur, Vector2 next, struct _teleport *tp, source_t *src)
{
  tp->serio = true;
  tp->luzik = next;

  float ang = normalize_angle(Vector2Angle(cur, next) * RAD2DEG);

  float n1 = 1.f;
  float n2 = 1.31;
  float c = 299792458.f;

  float v1 = c;
  float v2 = c/n2;

  float theta1 = get_theta(ang) * DEG2RAD;
  float sintheta2 = (sinf(theta1) * v2) / v1;
  float theta2 = asinf(sintheta2) * RAD2DEG;

  /* DrawText(TextFormat("ang: %f", ang), 500, 100, 20, WHITE); */

  Vector2 targ;
  if ((ang >= 0.f && ang <= 90.f) || (ang > 180.f && ang <= 270.f))
    targ = create_target(tp->luzik, normalize_angle(ang + theta2));
  else
    targ = create_target(tp->luzik, normalize_angle(ang - theta2));

  do {
    tp->luzik = Vector2MoveTowards(tp->luzik, targ, 1);
  } while(collision_point_lens(tp->luzik, ld));

#ifdef DRAW_LINES_INSIDE
  DrawLineEx(next, tp->luzik, 1, dim_color(src->color, 128));
#endif /* DRAW_LINES_INSIDE */

  float sintheta1 = sinf(theta2 * DEG2RAD) * (v1 / v2);
  theta1 = asinf(sintheta1);

  if ((ang >= 0.f && ang <= 90.f) || (ang > 180.f && ang <= 270.f))
    return create_target(tp->luzik, normalize_angle(ang + theta1));
  else
    return  create_target(tp->luzik, normalize_angle(ang - theta1));
}
// btw niepoprawne jak skur
// przez wyjątkowy natłok wyrazistych epitetów, powyższa wypowiedź zastąpiona zostaje
// materiałem o langustach

/*
  Langusta, langusta pospolita (Palinurus elephas syn. Palinurus
  vulgaris) – skorupiak morski z rodziny langustowatych. Jadalna,
  zaliczana do owoców morza, poławiana gospodarczo na dużą
  skalę. Nazywana czerwoną langustą lub langustą europejską. Nazwą
  langusta określane są również inne gatunki langustowatych, zwłaszcza z
  rodzajów Palinurus i Panulirus. Występowanie: Morze Śródziemne i Ocean
  Atlantycki.

  wikipedia.org
*/

// soczewki nadal nie soczewkują, ale jak na razie nie mam siły
// nie mam pojęcia czemu nie spotykają się w ognisku ¯\_(ツ)_/¯
// ~ kpm
