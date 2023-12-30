#include "optyka.h"
#include "raylib.h"
#include "raymath.h"

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <limits.h>
#include <unistd.h>
#include <assert.h>
#include <time.h>

#define DEBUG 0
#undef DEBUG

extern scheme scm;
extern hookable_event_t keypress, click, unclick, frame, clocke;

#define MAX_INPUT_BUFFER_SIZE 4096
/* static void (*input_func)(void) = NULL; */
/* static char input_buffer[MAX_INPUT_BUFFER_SIZE] = {0}; */

Font default_font;

#define MIRROR_THICKNESS 1
int N_BOUNCEABLES = 0,
    N_SOURCES = 0;
bounceable_t *bounceables = NULL;
source_t *sources = NULL;

void load_default_font(void)
{
  extern unsigned int proggy_otf_len;
  extern unsigned char proggy_otf[];

  default_font = LoadFontFromMemory(".otf", proggy_otf, proggy_otf_len,
    30, NULL, 0);
  TraceLog(LOG_INFO, "loaded default font");
}

float normalize_angle(float f)
{
  while (f >= 360) f -= 360;
  while (f < 0) f += 360;

  return f;
}

float absf(float x)
{
  return x > 0 ? x : -x;
}

// via ./notatki.ora
static Vector2 create_target(Vector2 a, float angle)
{
  if ((angle >= 180 && angle <= 360) || angle == 0)
    return (Vector2){(ctg((180-angle)*(PI/180.f))*a.y+a.x), 0};
  else
    return (Vector2){a.x + ((SCREEN_HEIGHT - a.y) / ctg((90-angle)*PI/180.f)),
      SCREEN_HEIGHT};
}

static void draw_source(source_t *s)
{
  Vector2 cur_mouse;
  Rectangle rect = {
    .x = s->pt.x,
    .y = s->pt.y,
    .width = s->size,
    .height = s->size
  };

  cur_mouse = GetMousePosition();

  if (s->mouse_reactive)
    s->angle = normalize_angle(
      Vector2Angle((Vector2){rect.x, rect.y}, cur_mouse) * 180 / PI);
  s->target = create_target((Vector2){rect.x, rect.y}, s->angle);

  DrawRectanglePro(rect, (Vector2){s->size / 2.f, s->size / 2.f}, s->angle, RED);
  DrawCircleV(s->pt, 3, VIOLET);
}

static void draw_lens(bounceable_t *b)
{
  lens_data_t *ld = b->data.lens;
  Vector2 p1 = ld->p1,
          p2 = ld->p2;

  DrawCircleV(ld->focal_point1, 2, PINK);
  DrawCircleV(ld->focal_point2, 2, PINK);

  DrawCircleV(ld->center, 2, LIME);

  DrawLineEx(p1, p2, 2, PURPLE);

  DrawLineEx(p1, ld->focal_point1, 1, PURPLE);
  DrawLineEx(p1, ld->focal_point2, 1, PURPLE);
  DrawLineEx(p2, ld->focal_point1, 1, PURPLE);
  DrawLineEx(p2, ld->focal_point2, 1, PURPLE);
}

static void draw_mirror(bounceable_t *b)
{
  Vector2 p1 = b->data.mirror->p1,
          p2 = b->data.mirror->p2;
  DrawLineEx(p1, p2, MIRROR_THICKNESS, BLACK);
}

static void draw_all_bounceables(void)
{
  int i;
  for (i = 0; i < N_BOUNCEABLES; ++i) {
    switch (bounceables[i].t) {
    case B_MIRROR:
      draw_mirror(&bounceables[i]);
      break;
    case B_LENS:
      draw_lens(&bounceables[i]);
      break;
    default:
      panic("unreachable");
    }
  }
}

// jeśli miałoby coś się ścinać, zwiększże **TROCHĘ** tą liczbę żeby zwiększyć
// wydajność kosztem dokładności
#define CAST_LIGHT_STEP_SIZE 1
static bool cast_light(Vector2 target, Vector2 source, Vector2 *ret,
    bounceable_t *hit_bounceable)
{
  int max_iter = 4096, i;
  while (max_iter) {
    source = Vector2MoveTowards(source, target, CAST_LIGHT_STEP_SIZE);

    for (i = 0; i < N_BOUNCEABLES; ++i) {
      switch (bounceables[i].t) {
      case B_MIRROR:
        if (CheckCollisionPointLine(source,
              bounceables[i].data.mirror->p1,
              bounceables[i].data.mirror->p2,
              CAST_LIGHT_STEP_SIZE))
          goto hit;
        break;
      case B_LENS:
        if (CheckCollisionPointLine(source,
              bounceables[i].data.lens->p1,
              bounceables[i].data.lens->p2,
              CAST_LIGHT_STEP_SIZE))
          goto hit;
        break;
      default:
        panic("unreachable");
      }
    }

    max_iter--;
  }

  ret->x = source.x, ret->y = source.y, hit_bounceable = NULL;
  return false;

 hit:
  *hit_bounceable = bounceables[i];
  ret->x = source.x, ret->y = source.y;
  return true;
}

Vector2 create_target_by_hit(bounceable_t *b, Vector2 cur, Vector2 next)
{
  float cur_angle, hit_angle, rel_angle;
  switch (b->t) {
  case B_MIRROR:
    hit_angle = normalize_angle(Vector2Angle(b->data.mirror->p1, b->data.mirror->p2)
                                * 180 / PI);
    rel_angle = normalize_angle(hit_angle -
                                normalize_angle(Vector2Angle(cur, next) * 180 / PI));
    cur_angle = normalize_angle(hit_angle + rel_angle);
    return create_target(next, cur_angle);
    break;
  case B_LENS: {
    lens_data_t *ld = b->data.lens;
    hit_angle = normalize_angle(Vector2Angle(b->data.lens->p1, b->data.lens->p2)
                                * 180 / PI);
    rel_angle = normalize_angle(hit_angle -
                                normalize_angle(Vector2Angle(cur, next) * 180 / PI));
    assert(ld->p1.x <= ld->p2.x);
    assert(ld->p1.y <= ld->p2.y);
    if (rel_angle < 180) {
      return create_target(ld->focal_point2,
               normalize_angle(Vector2Angle(next, ld->focal_point2) * 180 / PI));
    } else
      return create_target(ld->focal_point1,
               normalize_angle(Vector2Angle(next, ld->focal_point1) * 180 / PI));
  } break;
  }
}

// https://www.physicsclassroom.com/class/refln/Lesson-1/The-Law-of-Reflection
#define max_draw_lines 100
#define draw_single_light(source, start, s_targ) \
  _draw_single_light(source, start, s_targ, max_draw_lines)
static void _draw_single_light(source_t *source, Vector2 start, Vector2 s_target,
                        int max_depth)
{
  int i;
  Vector2 next, cur = {
    .x = start.x,
    .y = start.y
  }, cur_target = s_target;
  bounceable_t hit_bounceable = {0};
  bool bounced = true;

  for (i = 0; i < max_depth && bounced; ++i) {
    bounced = cast_light(cur_target, cur, &next, &hit_bounceable);
    DrawLineEx(cur, next, source->thickness, source->color);

    if (bounced) {
      cur_target = create_target_by_hit(&hit_bounceable, cur, next);
      //cur_target = create_target(next, cur_angle);
      cur = Vector2MoveTowards(next, cur_target, 1);
    }
  }
}

static void draw_light(source_t *src)
{
  int i, dist = floor(src->size / (1.f+(float)src->n_beam));
  for (i = 1; i <= src->n_beam; ++i) {
    /* int sz = ((float)src->size/2)/sqrt(2); */
    int sz = (((float)src->size/2)-(i*dist))/sqrt(2);
    Vector2 rot = Vector2Rotate(vec(sz), (-45 - 90 + src->angle) * PI/180);
    Vector2 pt = vec(src->pt.x + rot.x, src->pt.y + rot.y);
    // witam nazywam sie krzysztof, a te wzory wyciagnalem prosto z dupy

    draw_single_light(src, pt, vec(src->target.x - (src->pt.x - pt.x),
                                   src->target.y - (src->pt.y - pt.y)));
  }
}


void add_bounceable(bounceable_type_t t, void *data)
{
  bounceables = realloc(bounceables, sizeof(bounceable_t) *
    (1 + N_BOUNCEABLES));

  bounceables[N_BOUNCEABLES] = (bounceable_t){
    .t = t,
    .data.p = data
  };

  TraceLog(LOG_INFO, "new bounceable with T = %02x", t);
  N_BOUNCEABLES++;
}

void add_mirror(Vector2 p1, Vector2 p2)
{
  mirror_data_t *md = malloc(sizeof(mirror_data_t));
  md->p1 = (Vector2){p1.x,p1.y};
  md->p2 = (Vector2){p2.x,p2.y};

  add_bounceable(B_MIRROR, md);
}

void add_lens(Vector2 p1, Vector2 p2, float r1, float r2, float d, float n, float opacity)
{
  lens_data_t *ld = malloc(sizeof(lens_data_t));
  ld->d = d, ld->opacity = opacity, ld->r1 = r1, ld->r2 = r2, ld->n = n;
  ld->p1 = p1, ld->p2 = p2;

  ld->f = 1.f / ((1.f / ld->r1) + (1.f / ld->r2));
  ld->center = vec(ld->p1.x + (ld->p2.x - ld->p1.x)/2,
                   ld->p1.y + (ld->p2.y - ld->p1.y)/2);
  ld->focal_point1 = vec(ld->center.x - ld->f, ld->center.y);
  ld->focal_point2 = vec(ld->center.x + ld->f, ld->center.y); // TODO: nieprawda!!

  add_bounceable(B_LENS, ld);
}

void add_source(source_t s)
{
  sources = realloc(sources, sizeof(source_t) * (1 + N_SOURCES));

  sources[N_SOURCES] = (source_t){
    .color = (Color){s.color.r, s.color.g, s.color.b, s.color.a},
    .pt = (Vector2){ s.pt.x, s.pt.y },
    .angle = normalize_angle(s.angle),
    .size = s.size,
    .thickness = s.thickness,
    .mouse_reactive = s.mouse_reactive,

    // TODO: n_beam
    // TODO: assert(!(n_beam >= size))
    .n_beam = 19
  };

  TraceLog(LOG_INFO, "adding source [%f %f] ang. %f", s.pt.x, s.pt.y, s.angle);

  ++N_SOURCES;
}

static void initialize_raygui(void)
{
  load_default_font();
  GuiSetFont(default_font);
}

static void silent_tracelog_callback(__attribute__((unused))int a,
                                     __attribute__((unused))const char *b,
                                     __attribute__((unused))va_list c)
{
}

int main(int argc, char **argv)
{
  extern scheme scm;
  int i, c, k, charsize, opt;
  time_t time_prev, time_cur;
  struct mouse_information_t mi = {
    .first_click = false,
    .pos = {0,0},
    .pressed_moving = false,
    .left = false,
    .right = false,
  };

  while ((opt = getopt(argc, argv, "e:F:")) != -1) {
    switch (opt) {
    case 'e':
      initialize_scheme();
      scheme_load_string(&scm, optarg);
      exit(0);
      break;
    case 'F':
      SetTraceLogCallback(silent_tracelog_callback);
      initialize_scheme();
      FILE *f = fopen(optarg, "r");
      assert(f != NULL);
      scheme_load_file(&scm, f);
      exit(0);
      break;
    }
  }

  initialize_scheme();
  InitWindow(800, 600, "giga optyka");
  SetExitKey(-1);
  initialize_raygui();

  add_lens(vec(200, 200), vec(200, 300), 20.f, 20.f, 10.f, 1.5, 100.f);

  time_prev = time_cur = time(NULL);
  while (!WindowShouldClose()) {
    mi.pos = GetMousePosition();

    BeginDrawing();
    {
      ClearBackground(WHITE);

      c = CodepointToUTF8(GetCharPressed(), &charsize)[0];
      k = GetKeyPressed();
      if (c || k)
        do_hooks(&keypress, cons(&scm, mk_character(&scm, c),
              cons(&scm, mk_integer(&scm, k), scm.NIL)));

      if ((IsMouseButtonDown(MOUSE_BUTTON_LEFT) ||
            IsMouseButtonDown(MOUSE_BUTTON_RIGHT))
          && mi.pressed_moving == false) {
        mi.pressed_moving = true;
        mi.first_click = true;

        // bardzo nie podoba mi się fakt, że nie moge tego sprawdzić jakimiś
        // flagami typu IsMouseButtonDown(L | R);
        // (sprawdzałem implementację - nie mogę)
        if (IsMouseButtonDown(MOUSE_LEFT_BUTTON))
          mi.left = true;

        if (IsMouseButtonDown(MOUSE_RIGHT_BUTTON))
          mi.right = true;;
      }

      if (!IsMouseButtonDown(MOUSE_BUTTON_LEFT) &&
          !IsMouseButtonDown(MOUSE_BUTTON_RIGHT)
          && mi.pressed_moving) {
        mi.pressed_moving = mi.first_click = mi.left = mi.right = false;
        mi._dx = mi._dy = 0, mi._currently_moving = NULL;

        do_hooks(&unclick, scheme_click_info(&mi));
      }

      draw_all_bounceables();

      // TODO: to powinno być w osobnej funkcji. ale to kiedyś
      for (i = 0; i < N_SOURCES; ++i) {
        draw_source(&sources[i]);
        draw_light(&sources[i]);
      }

      if (mi.pressed_moving && mi._currently_moving == NULL) {
        do_hooks(&click, scheme_click_info(&mi));
      }

    }

    do_hooks(&frame, scm.NIL);
    if ((time_cur = time(NULL)) != time_prev) {
      time_prev = time_cur;
      do_hooks(&clocke, cons(&scm, mk_integer(&scm, time_cur),
                             scm.NIL));
    }
    EndDrawing();

    mi.first_click = false;
  }

  free(sources);
  free(bounceables);
  scheme_deinit(&scm);
}
