#include "optyka.h"

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <limits.h>

#define DEBUG 0
#undef DEBUG

extern scheme scm;
extern hookable_event_t keypress, click, unclick, frame;

#define MAX_INPUT_BUFFER_SIZE 4096
static void (*input_func)(void) = NULL;
static char input_buffer[MAX_INPUT_BUFFER_SIZE] = {0};

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
    .x = s->pt.x - (s->size / 2.f),
    .y = s->pt.y - (s->size / 2.f),
    .width = s->size,
    .height = s->size
  };

  cur_mouse = GetMousePosition();

  if (s->mouse_reactive)
    s->angle = normalize_angle(
      Vector2Angle((Vector2){rect.x, rect.y}, cur_mouse) * 180 / PI);
  s->target = create_target((Vector2){rect.x, rect.y}, s->angle);

  DrawRectanglePro(rect, (Vector2){s->size / 2.f, s->size / 2.f}, s->angle, RED);
}

static void draw_lens(bounceable_t *b)
{
  (void)b;
  TODO("draw_lens not implemented");
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
              CAST_LIGHT_STEP_SIZE)) {
          *hit_bounceable = bounceables[i];
          ret->x = source.x, ret->y = source.y;
          return true;
        }
        break;
      case B_LENS:
        TODO("B_LENS not implemented.");
        break;
      default:
        panic("unreachable");
      }
    }

    max_iter--;
  }

  ret->x = source.x, ret->y = source.y, hit_bounceable = NULL;
  return false;
}

Vector2 create_target_by_hit(bounceable_t *b, Vector2 cur, Vector2 next)
{
  float cur_angle, hit_angle, rel_angle;

  switch (b->t) {
  case B_MIRROR:
    hit_angle =
      normalize_angle(Vector2Angle(b->data.mirror->p1, b->data.mirror->p2)
        * 180 / PI);
    rel_angle = normalize_angle(
        hit_angle - normalize_angle(Vector2Angle(cur, next) * 180 / PI));
    cur_angle = normalize_angle(hit_angle + rel_angle);

    return create_target(next, cur_angle);
  case B_LENS:
    TODO("lens not implemented");
  }
}

// https://www.physicsclassroom.com/class/refln/Lesson-1/The-Law-of-Reflection
#define max_draw_lines 100
#define draw_light(s) _draw_light(s, max_draw_lines)
static void _draw_light(source_t *s, int max_depth)
{
  int i;
  Vector2 next, cur = {
    .x = s->pt.x - 10,
    .y = s->pt.y - 10
  }, cur_target = s->target;
  bounceable_t hit_bounceable = {0};
  bool bounced = true;

  for (i = 0; i < max_depth && bounced; ++i) {
    bounced = cast_light(cur_target, cur, &next, &hit_bounceable);
    DrawLineEx(cur, next, s->thickness, s->color);

    if (bounced) {
      cur_target = create_target_by_hit(&hit_bounceable, cur, next);
      //cur_target = create_target(next, cur_angle);
      cur = Vector2MoveTowards(next, cur_target, 1);
    }
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

void add_lens(Vector2 p, float r1, float r2, float d, float opacity)
{
  lens_data_t *ld = malloc(sizeof(lens_data_t));
  ld->d = d, ld->opacity = opacity, ld->r1 = r1, ld->r2 = r2, ld->p = p;

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
    .mouse_reactive = s.mouse_reactive
  };

  TraceLog(LOG_INFO, "adding source [%f %f] ang. %f", s.pt.x, s.pt.y, s.angle);

  ++N_SOURCES;
}

static void initialize_raygui(void)
{
  load_default_font();
  GuiSetFont(default_font);
}

static void show_and_eval_scheme(void)
{
  int res;
  extern scheme scm;

  res = GuiTextInputBox((Rectangle){100, 100, 400, 200}, "eval",
                        "ewaluuj wyrażanie tinyscheme", "ok;anuluj", input_buffer,
                        MAX_INPUT_BUFFER_SIZE, NULL);

  switch (res) {
  case 1: // ok
    scheme_load_string(&scm, input_buffer);
    /* fallthrough */
  case 2: //anuluj
    input_func = NULL;
    input_buffer[0] = 0;
  }
}

int main(void)
{
  extern scheme scm;
  int i, c, k, charsize;
  struct mouse_information_t mi = {
    .first_click = false,
    .pos = {0,0},
    .pressed_moving = false,
    .left = false,
    .right = false,
  };

  InitWindow(800, 600, "giga optyka");
  initialize_raygui();
  initialize_scheme();

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
    EndDrawing();

    mi.first_click = false;
  }

  free(sources);
  free(bounceables);
  scheme_deinit(&scm);
}
