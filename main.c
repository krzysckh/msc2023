#include <stdio.h>
#include <raylib.h>
#include <raymath.h>
#include <math.h>
#include <limits.h>

#define SCREEN_WIDTH 800
#define SCREEN_HEIGHT 600

#define MAX(x,y) (((x)>(y))?(x):(y))
#define MIN(x,y) (((x)<(y))?(x):(y))

typedef struct {
  Vector2 target; /* imaginary target */
  Vector2 pt;
  float angle; // 0-359
} source_t;

float absf(float x) {
  return x > 0 ? x : -x;
}

// via ./notatki.ora
Vector2 create_target(Vector2 a, Vector2 b, float angle) {
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

// TODO: tu dodaj odbijanie się od przeróżnych elementów
#define CAST_LIGHT_STEP_SIZE 1
void cast_light(Vector2 target, Vector2 source, Vector2 *ret, int angle)
{
  int max_iter = 4096;
  while (max_iter) {
    //Source.x += CAST_LIGHT_STEP_SIZE, source.y += CAST_LIGHT_STEP_SIZE;
    source = Vector2MoveTowards(source, target, CAST_LIGHT_STEP_SIZE);

    if (!CheckCollisionPointRec(source, (Rectangle){0, 0, SCREEN_WIDTH, SCREEN_HEIGHT})) {
      break;
    }

    max_iter--;
  }

  ret->x = source.x, ret->y = source.y;
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
  };
  float cur_angle = s->angle;
  Vector2 cur_target = s->target;

  Color colors[10] = { BLUE, RED, VIOLET, GREEN, PURPLE, BLACK, ORANGE, BLUE, RED, VIOLET };

  for (i = 0; i < max_depth; ++i) {
    cast_light(cur_target, cur, &next, cur_angle);
    cur_angle = Vector2Angle(cur, next);
    DrawLineV(cur, next, colors[i]);


    cur_angle = normalize_angle(cur_angle + cur_angle);
    if (next.y == 0 || next.y == SCREEN_HEIGHT) {
      cur_angle = normalize_angle(cur_angle + 180);
    }

    cur_target = create_target(next, (Vector2){cur.x + (2 * (next.x - cur.x)), cur.y}, cur_angle);
    cur = next;
  }
}

int main (void)
{
  source_t src = {
    .pt = (Vector2){ 300.f, 300.f },
    .angle = 90
  };

  InitWindow(800, 600, "giga optyka");

  while (!WindowShouldClose()) {
    BeginDrawing();
    {
      ClearBackground(WHITE);

      draw_source(&src);
      draw_light(&src);
    }
    EndDrawing();
  }
}
