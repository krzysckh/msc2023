#ifdef _WIN32

#define ICON_ID (2137)

#include <windows.h>

void *win_hinstance;

void w32_load_icon(void)
{
  HWND window = GetActiveWindow();
  /* printf("window: %p\n", window); */

  /* exit(0); */
  HICON icon = LoadIcon((HINSTANCE)win_hinstance, MAKEINTRESOURCE(ICON_ID));
  SendMessage(window, WM_SETICON, ICON_BIG, (LPARAM)icon);
  SetWindowIcon(window, icon);
}

#else // _WIN32

void w32_load_icon(void)
{
  (void)(0);
}

#endif //_WIN32
