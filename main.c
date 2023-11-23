#include <stdio.h>
#include <raylib.h>
#include <raymath.h>
#include <stdlib.h>
#include <math.h>
#include <limits.h>

#define DEBUG 0
#undef DEBUG

#define SCREEN_WIDTH 800
#define SCREEN_HEIGHT 600

#define MAX(x,y) (((x)>(y))?(x):(y))
#define MIN(x,y) (((x)<(y))?(x):(y))

#define ctg(x) (pow(tan((x)),-1))

struct mouse_information_t {
  bool first_click;
  bool pressed_moving;
  Vector2 pos;

  int _dx, _dy; /* the javascript way */
  void *_currently_moving;
};

typedef struct {
  Vector2 p1, p2;
} bounceable_t;

typedef struct {
  int size;
  Vector2 target; /* imaginary target */
  Vector2 pt;
  float angle; // 0-359
} source_t;

#define BOUNCEABLE_THICKNESS 1
static int N_BOUNCEABLES = 0;
bounceable_t *bounceables = NULL;

#define MAX_LINE_THICKNESS 8
static float current_line_thickness = 1.f;

float normalize_angle(float f)
{
  while (f >= 360) f -= 360;
  while (f < 0) f += 360;

  return f;
}

void init_bounceables(void)
{
  bounceables = malloc(sizeof(bounceable_t) * 4);
  bounceables[0] = (bounceable_t){(Vector2){800, 50},  (Vector2){700, 400}};
  bounceables[1] = (bounceable_t){(Vector2){200, 200}, (Vector2){0, 200}};
  bounceables[2] = (bounceable_t){(Vector2){10, 200},  (Vector2){70, 500}};
  bounceables[3] = (bounceable_t){(Vector2){300, 550}, (Vector2){500, 550}};

  N_BOUNCEABLES = 4;
}

float absf(float x)
{
  return x > 0 ? x : -x;
}

// via ./notatki.ora
Vector2 create_target(Vector2 a, Vector2 b, float angle)
{
  if ((angle >= 180 && angle <= 360) || angle == 0)
    return (Vector2){(ctg((180-angle)*(PI/180.f))*a.y+a.x), 0};
  else
    return (Vector2){a.x + ((SCREEN_HEIGHT - a.y) / ctg((90-angle)*PI/180.f)), SCREEN_HEIGHT};
}

void draw_source(source_t *s)
{
  Vector2 cur_mouse;
  Rectangle rect = {
    .x = s->pt.x - (s->size / 2),
    .y = s->pt.y - (s->size / 2),
    .width = s->size,
    .height = s->size 
  };

  cur_mouse = GetMousePosition();

  s->angle = normalize_angle(Vector2Angle((Vector2){rect.x, rect.y}, cur_mouse) * 180 / PI);
  s->target = create_target((Vector2){rect.x, rect.y}, cur_mouse, s->angle);

  DrawRectanglePro(rect, (Vector2){s->size / 2, s->size / 2}, s->angle, RED);
}

void draw_all_bounceables()
{
  int i;
  for (i = 0; i < N_BOUNCEABLES; ++i) {
    DrawLineEx(bounceables[i].p1, bounceables[i].p2, BOUNCEABLE_THICKNESS, BLACK);
  }
}

void draw_line_by_lenght_angle(Vector2 pos, int len, int angle, Color c)
{
  Rectangle rect = {
    .x = pos.x,
    .y = pos.y,
    .width = len,
    .height = 1
  };

  DrawRectanglePro(rect, (Vector2){0, 0}, angle, c);
}

// jeśli miałoby coś się ścinać, zwiększże **TROCHĘ** tą liczbę żeby zwiększyć wydajność kosztem dokładności
#define CAST_LIGHT_STEP_SIZE 1
bool cast_light(Vector2 target, Vector2 source, Vector2 *ret, bounceable_t *hit_bounceable)
{
  int max_iter = 4096, i;
  while (max_iter) {
    source = Vector2MoveTowards(source, target, CAST_LIGHT_STEP_SIZE);

    for (i = 0; i < N_BOUNCEABLES; ++i) {
      if (CheckCollisionPointLine(source, bounceables[i].p1, bounceables[i].p2, CAST_LIGHT_STEP_SIZE)) {
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
void _draw_light(source_t *s, int max_depth)
{
  int i;
  Vector2 next, cur = {
    .x = s->pt.x - 10,
    .y = s->pt.y - 10
  }, cur_target = s->target;
  float cur_angle = s->angle, hit_angle, rel_angle;
  bounceable_t hit_bounceable;
  Color colors[10] = { BLUE, RED, VIOLET, GREEN, PURPLE, BLACK, ORANGE, BLUE, RED, VIOLET };
  bool bounced = true;

  for (i = 0; i < max_depth && bounced; ++i) {
    bounced = cast_light(cur_target, cur, &next, &hit_bounceable);

    hit_angle = MAX(normalize_angle(Vector2Angle(hit_bounceable.p2, hit_bounceable.p1) * 180 / PI),
                    normalize_angle(Vector2Angle(hit_bounceable.p1, hit_bounceable.p2) * 180 / PI));
    rel_angle = normalize_angle(hit_angle - normalize_angle(Vector2Angle(cur, next) * 180 / PI));
    cur_angle = normalize_angle(hit_angle + rel_angle);

    DrawLineEx(cur, next, current_line_thickness, colors[i % 10]);

#ifdef DEBUG
    if (i == 0) {
      char t0[512] = {0};
      char t1[512] = {0};
      char t2[512] = {0};
      char t3[512] = {0};

      snprintf(t0, 512, "hit_angle: %f", hit_angle);
      DrawTextEx(GetFontDefault(), t0, (Vector2){10, 10}, 32, 10, BLACK);

      snprintf(t1, 512, "cur_angle: %f", cur_angle);
      DrawTextEx(GetFontDefault(), t1, (Vector2){10, 40}, 32, 10, BLACK);

      snprintf(t2, 512, "line angle: %f", normalize_angle(Vector2Angle(cur, next) * 180 / PI));
      DrawTextEx(GetFontDefault(), t2, (Vector2){10, 80}, 32, 10, BLACK);

      snprintf(t3, 512, "rel_angle: %f", rel_angle);
      DrawTextEx(GetFontDefault(), t3, (Vector2){10, 120}, 32, 10, BLACK);
    }
#endif

    cur_target = create_target(next, (Vector2){cur.x + (2 * (next.x - cur.x)), cur.y}, cur_angle);
    cur = Vector2MoveTowards(next, cur_target, 1);
  }
}

/* TODO: ta funkcja nie wie o obrocie source_t */
void handle_source_repositioning(source_t *s, struct mouse_information_t *mi) {
  if (!mi->pressed_moving)
    return;

  Rectangle rect = {
    .x = s->pt.x - s->size,
    .y = s->pt.y - s->size,
    .width = s->size,
    .height = s->size
  };

  if (mi->first_click) {
    mi->_dx = mi->pos.x - s->pt.x;
    mi->_dy = mi->pos.y - s->pt.y;
  }

  // DrawRectangleRec(rect, PURPLE);

  if (mi->_currently_moving == s || CheckCollisionPointRec(mi->pos, rect)) {
    if (!mi->first_click) {
      s->pt.x = mi->pos.x - mi->_dx;
      s->pt.y = mi->pos.y - mi->_dy;
    } else {
      mi->_currently_moving = s;
    }
  }
}

int main(void)
{
  int i, n_sources = 1;
  source_t *sources = malloc(sizeof(source_t) * n_sources);
  struct mouse_information_t mi = {
    .first_click = false,
    .pos = {0,0},
    .pressed_moving = false
  };

  sources[0] = (source_t){
    .size = 20,
    .pt = (Vector2){ 300.f, 300.f },
    .angle = 90
  };

  init_bounceables();

  InitWindow(800, 600, "giga optyka");

  while (!WindowShouldClose()) {
    switch (GetCharPressed()) {
    case L'+':
      current_line_thickness = MIN(current_line_thickness + 1.f, MAX_LINE_THICKNESS);
      break;
    case L'-':
      current_line_thickness = MAX(current_line_thickness - 1.f, 1);
      break;
    }

    if (IsMouseButtonDown(MOUSE_BUTTON_LEFT) && mi.pressed_moving == false) {
      mi.pressed_moving = true;
      mi.first_click = true;
    }

    if (!IsMouseButtonDown(MOUSE_BUTTON_LEFT) && mi.pressed_moving) {
      mi.pressed_moving = false, mi.first_click = false;
    }


    BeginDrawing();
    {
      ClearBackground(WHITE);
      mi.pos = GetMousePosition();

      draw_all_bounceables();

      //printf("%b %b\n", mi.pressed_moving, mi.first_click);
      for (i = 0; i < n_sources; ++i) {
        handle_source_repositioning(&sources[i], &mi);

        draw_source(&sources[i]);
        draw_light(&sources[i]);
      }
    }
    EndDrawing();

    mi.first_click = false;
  }

  free(bounceables);
}
