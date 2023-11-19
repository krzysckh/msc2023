#include <stdio.h>
#include <raylib.h>
#include <raymath.h>
#include <stdlib.h>
#include <math.h>
#include <limits.h>

#define SCREEN_WIDTH 800
#define SCREEN_HEIGHT 600

#define MAX(x,y) (((x)>(y))?(x):(y))
#define MIN(x,y) (((x)<(y))?(x):(y))

typedef struct {
  Vector2 p1, p2;
} bounceable_t;

typedef struct {
  Vector2 target; /* imaginary target */
  Vector2 pt;
  float angle; // 0-359
} source_t;

static int N_BOUNCEABLES = 0;
bounceable_t *bounceables = NULL;

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
  if (angle >= -180 && angle <= 0)
    return (Vector2){a.x + ((b.x - a.x) * a.y) / (a.y - b.y), 0};
  else
    return (Vector2){a.x + ((b.x - a.x) * (SCREEN_HEIGHT - a.y)) / (b.y - a.y), SCREEN_HEIGHT};
}

void draw_source(source_t *s)
{
  Vector2 cur_mouse;
  Rectangle rect = {
    .x = s->pt.x - 10.f,
    .y = s->pt.y - 10.f,
    .width = 20,
    .height = 20
  };

  cur_mouse = GetMousePosition();

  s->angle = Vector2Angle((Vector2){rect.x, rect.y}, cur_mouse) * 180 / PI;
  s->target = create_target((Vector2){rect.x, rect.y}, cur_mouse, s->angle);

  DrawRectanglePro(rect, (Vector2){10, 10}, s->angle, RED);
}

void draw_all_bounceables() {
  size_t i;
  for (i = 0; i < N_BOUNCEABLES; ++i) {
    DrawLineV(bounceables[i].p1, bounceables[i].p2, BLACK);
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

#define CAST_LIGHT_STEP_SIZE 1
bool cast_light(Vector2 target, Vector2 source, Vector2 *ret, bounceable_t *hit_bounceable)
{
  int max_iter = 4096;
  size_t i;
  while (max_iter) {
    source = Vector2MoveTowards(source, target, CAST_LIGHT_STEP_SIZE);

    for (i = 0; i < N_BOUNCEABLES; ++i) {
      if (CheckCollisionPointLine(source, bounceables[i].p1, bounceables[i].p2, 1)) {
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

float normalize_angle(float f)
{
  while (f >= 360) f -= 360;
  while (f <= -360) f += 360;
  if (f < -180) f = f + 360;
  if (f > 180) f = f - 360;

  return f;
}

// https://www.physicsclassroom.com/class/refln/Lesson-1/The-Law-of-Reflection
#define max_draw_lines 10
#define draw_light(s) _draw_light(s, max_draw_lines)
void _draw_light(source_t *s, int max_depth)
{
  int i;
  Vector2 next, cur = {
    .x = s->pt.x - 10,
    .y = s->pt.y - 10
  }, cur_target = s->target;
  float cur_angle = s->angle, hit_angle;
  bounceable_t hit_bounceable;
  Color colors[10] = { BLUE, RED, VIOLET, GREEN, PURPLE, BLACK, ORANGE, BLUE, RED, VIOLET };
  bool bounced = true;

  // TODO: cos tu nie dziala ale nie mam juz sily
  for (i = 0; i < max_depth && bounced; ++i) {
    bounced = cast_light(cur_target, cur, &next, &hit_bounceable);

    hit_angle = Vector2Angle(hit_bounceable.p1, hit_bounceable.p2) * 180 / PI;
    // printf("ANG: %f (for [%f %f] [%f %f]\n", hit_angle,
    //       hit_bounceable.p1.x, hit_bounceable.p1.y, 
    //       hit_bounceable.p2.x, hit_bounceable.p2.y);

    if (hit_angle == 0 || hit_angle == 180)
      cur_angle = normalize_angle(Vector2Angle(cur, next) * -180 / PI);
    else
      cur_angle = normalize_angle(Vector2Angle(cur, next) * 180 / PI);


    DrawLineV(cur, next, colors[i % 10]);

    cur_target = create_target(next, (Vector2){cur.x + (2 * (next.x - cur.x)), cur.y}, cur_angle);
    cur = Vector2MoveTowards(next, cur_target, 2);
  }
}

int main (void)
{
  source_t src = {
    .pt = (Vector2){ 300.f, 300.f },
    .angle = 90
  };

  init_bounceables();

  InitWindow(800, 600, "giga optyka");

  while (!WindowShouldClose()) {
    BeginDrawing();
    {
      ClearBackground(WHITE);

      draw_all_bounceables();

      draw_source(&src);
      draw_light(&src);
    }
    EndDrawing();
  }

  free(bounceables);
}
