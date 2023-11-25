#include "optyka.h"

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <limits.h>

#define DEBUG 0
#undef DEBUG

#define SCREEN_WIDTH 800
#define SCREEN_HEIGHT 600

#define MAX(x,y) (((x)>(y))?(x):(y))
#define MIN(x,y) (((x)<(y))?(x):(y))

extern scheme scm;
extern char tinyscheme_r5rs_scm[];

extern hookable_event_t keypress;
extern hookable_event_t click;
extern hookable_event_t unclick;

#define MAX_INPUT_BUFFER_SIZE 4096
static void (*input_func)(void) = NULL;
static char input_buffer[MAX_INPUT_BUFFER_SIZE] = {0};

#define BOUNCEABLE_THICKNESS 1
static int N_BOUNCEABLES = 0,
           N_SOURCES = 0;
static bounceable_t *bounceables = NULL;
static source_t *sources = NULL;

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

static void draw_all_bounceables(void)
{
  int i;
  for (i = 0; i < N_BOUNCEABLES; ++i) {
    DrawLineEx(bounceables[i].p1, bounceables[i].p2, BOUNCEABLE_THICKNESS,
      BLACK);
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
      if (CheckCollisionPointLine(source, bounceables[i].p1, bounceables[i].p2,
            CAST_LIGHT_STEP_SIZE)) {
        *hit_bounceable = bounceables[i];
        ret->x = source.x, ret->y = source.y;
        return true;
      }
    }

    max_iter--;
  }

  ret->x = source.x, ret->y = source.y, hit_bounceable = NULL;
  return false;
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
  float cur_angle = s->angle, hit_angle, rel_angle;
  bounceable_t hit_bounceable;
  bool bounced = true;

  for (i = 0; i < max_depth && bounced; ++i) {
    bounced = cast_light(cur_target, cur, &next, &hit_bounceable);

    hit_angle = MAX(
      normalize_angle(
        Vector2Angle(hit_bounceable.p2, hit_bounceable.p1) * 180 / PI),
      normalize_angle(
        Vector2Angle(hit_bounceable.p1, hit_bounceable.p2) * 180 / PI));
    rel_angle = normalize_angle(
        hit_angle - normalize_angle(Vector2Angle(cur, next) * 180 / PI));
    cur_angle = normalize_angle(hit_angle + rel_angle);

    DrawLineEx(cur, next, s->thickness, s->color);

    cur_target = create_target(next, cur_angle);
    cur = Vector2MoveTowards(next, cur_target, 1);
  }
}

/* TODO: ta funkcja nie wie o obrocie source_t */
static void handle_source_repositioning(source_t *s,
    struct mouse_information_t *mi) {
  if (!mi->pressed_moving)
    return;

  /* to +10 żeby łatwiej się łapało. jak będzie przeszkadzać można usunąć */
  Rectangle rect = {
    .x = s->pt.x - (10 + s->size),
    .y = s->pt.y - (10 + s->size),
    .width = s->size + 10,
    .height = s->size + 10
  };

  // DrawRectangleRec(rect, PURPLE);


  if ((mi->_currently_moving == s || mi->_currently_moving == NULL)
      && CheckCollisionPointRec(mi->pos, rect)) {
    if (!mi->first_click) {
      s->pt.x = mi->pos.x - mi->_dx;
      s->pt.y = mi->pos.y - mi->_dy;
    } else {
      mi->_dx = mi->pos.x - s->pt.x;
      mi->_dy = mi->pos.y - s->pt.y;
      mi->_currently_moving = s;
    }
  }
}

void add_bounceable(Vector2 p1, Vector2 p2)
{
  bounceables = realloc(bounceables, sizeof(bounceable_t) *
    (1 + N_BOUNCEABLES));
  bounceables[N_BOUNCEABLES] = (bounceable_t){ p1, p2 };

  TraceLog(LOG_INFO, "adding bounceable [%f %f] [%f %f]", p1.x, p1.y, p2.x,
    p2.y);

  N_BOUNCEABLES++;
}

void add_source(source_t s)
{
  sources = realloc(sources, sizeof(source_t) * (1 + N_SOURCES));

  sources[N_SOURCES] = (source_t){
    .color = s.color,
    .pt = s.pt,
    .angle = normalize_angle(s.angle),
    .size = s.size,
    .thickness = s.thickness,
    .mouse_reactive = s.mouse_reactive
  };

  TraceLog(LOG_INFO, "adding source [%f %f] ang. %f", s.pt.x, s.pt.y, s.angle);

  ++N_SOURCES;
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
  int i, c;
  struct mouse_information_t mi = {
    .first_click = false,
    .pos = {0,0},
    .pressed_moving = false,
    .left = false,
    .right = false,
  };

  InitWindow(800, 600, "giga optyka");
  initialize_scheme();

  while (!WindowShouldClose()) {
    mi.pos = GetMousePosition();

    if (input_func == NULL) {
      c = GetCharPressed();
      switch (c) {
      case 0: break;
      case L'e':
        input_func = show_and_eval_scheme;
        break;
      default:
        do_hooks(&keypress, cons(&scm, mk_character(&scm, c), scm.NIL));
      }
    } else {
      input_func();
    }

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

    BeginDrawing();
    {
      ClearBackground(WHITE);

      draw_all_bounceables();

      //printf("%b %b\n", mi.pressed_moving, mi.first_click);
      for (i = 0; i < N_SOURCES; ++i) {
        handle_source_repositioning(&sources[i], &mi);

        draw_source(&sources[i]);
        draw_light(&sources[i]);
      }
    }
    EndDrawing();

    if (mi.pressed_moving && mi._currently_moving == NULL) {
      do_hooks(&click, scheme_click_info(&mi));
    }

    mi.first_click = false;
  }

  free(sources);
  free(bounceables);
  scheme_deinit(&scm);
}
