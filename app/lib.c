// sdltest.c
#ifdef _WIN32a
#include "SDL.h"   /* All SDL App's need this */
#else
#include<linux/time.h>
#define __timespec_defined 1
#define __timeval_defined 1
#define __itimerspec_defined 1
#include "SDL2/SDL.h"
#endif
#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include "hr_time.h"


#define QUITKEY SDLK_ESCAPE
#define WIDTH 1024
#define HEIGHT 768

SDL_Window* screen = NULL;
SDL_Renderer* renderer;
SDL_Event event;
SDL_Rect source, destination, dst;

int errorCount = 0;
int keypressed;
int rectCount = 0;
stopWatch s;

/* returns a number between 1 and max */
int Random(int max) {
	return (rand() % max) + 1;
}

void LogError(char* msg) {
	printf("%s\n", msg);
	errorCount++;
}

/* Sets Window caption according to state - eg in debug mode or showing fps */
void SetCaption(char* msg) {
	SDL_SetWindowTitle(screen, msg);
}

/* Initialize all setup, set screen mode, load images etc */
void InitSetup() {
	srand((int)time(NULL));
	SDL_Init(SDL_INIT_EVERYTHING);
	SDL_CreateWindowAndRenderer(WIDTH, HEIGHT, SDL_WINDOW_SHOWN, &screen, &renderer);
	if (!screen) {
		LogError("InitSetup failed to create window");
	}
	SetCaption("Example One");
}

/* Cleans up after game over */
void FinishOff() {
	SDL_DestroyRenderer(renderer);
	SDL_DestroyWindow(screen);
	//Quit SDL
	SDL_Quit();
	exit(0);
}

/* read a character */
char getaChar() {
	int result = -1;

	while (SDL_PollEvent(&event)) {
		if (event.type == SDL_KEYDOWN)
		{
			result = event.key.keysym.sym;
			break;
		}
	}
	return result;
}

void DrawRandomRectangle() {
	char buff[20];
	SDL_Rect rect;
	SDL_SetRenderDrawColor(renderer, Random(256) - 1, Random(256) - 1, Random(256) - 1, 255);
	rect.h = 120;// Random(100) + 20;
	rect.w = 120;// Random(100) + 20;
	rect.y = Random(HEIGHT - rect.h - 1);
	rect.x = Random(WIDTH - rect.w - 1);
	SDL_RenderFillRect(renderer, &rect);

	rectCount++;
	if (rectCount % 100000 == 0) {
		SDL_RenderPresent(renderer);
		stopTimer(&s);
		snprintf(buff, sizeof(buff),"%10.6f",diff(&s));
		SetCaption(buff);
		startTimer(&s);
	}
}

/* main game loop. Handles demo mode, high score and game play */
void GameLoop() {
	int gameRunning = 1;
	startTimer(&s);
	while (gameRunning)
	{
		DrawRandomRectangle();

		while (SDL_PollEvent(&event)) {
			switch (event.type) {
			case SDL_KEYDOWN:
				keypressed = event.key.keysym.sym;
				if (keypressed == QUITKEY)
				{
					gameRunning = 0;
					break;
				}

				break;
			case SDL_QUIT: /* if mouse click to close window */
			{
				gameRunning = 0;
				break;
			}
			case SDL_KEYUP: {
				break;
			}
			} /* switch */

		} /* while SDL_PollEvent */
	}
}

int SDL_Surfaces_comparable(SDL_Surface *s1, SDL_Surface *s2) {
  return (s1->format->format == s2->format->format && s1->w == s2->w && s1->h == s2->h);
}

int SDL_Surfaces_equal(SDL_Surface *s1, SDL_Surface *s2) {
  if (!SDL_Surfaces_comparable(s1, s2)) {
    return 0;
  }
  SDL_LockSurface(s1);
  SDL_LockSurface(s2);
  // the # of bytes we want to check is bytes_per_pixel * pixels_per_row * rows
  int len = s1->format->BytesPerPixel * s1->pitch * s1->h;
  int i;
  for (i = 0; i < len; i++) {
    // check if any two pixel bytes are unequal
    if (*(uint8_t *)(s1->pixels + i) != *(uint8_t *)(s2->pixels + i)) {
      break;
    }
  }
  // return true if we finished our loop without finding non-matching data
  SDL_UnlockSurface(s1);
  SDL_UnlockSurface(s2);
  return i == len;
}

int main(int argc, char* args[])
{
	InitSetup();
	//GameLoop();
	SDL_Rect rect;
	rect.h = 10;// Random(100) + 20;
	rect.w = 10;// Random(100) + 20;
	rect.y = 55;
	rect.x = 55;

	SDL_Rect rect2;
	rect2.h = 10;// Random(100) + 20;
	rect2.w = 10;// Random(100) + 20;
	rect2.y = 60;
	rect2.x = 60;


	SDL_Surface *s1 = SDL_CreateRGBSurface(0, 100,100,32,0,0,0,0);
	SDL_Surface *s2 = SDL_CreateRGBSurface(0, 100,100,32,0,0,0,0);
	SDL_FillRect(s1, &rect, 0x00ff00);
	SDL_FillRect(s2, &rect2, 0xff0000);

	SDL_BlitSurface(s1, &rect, s2, &rect);
	int z = SDL_memcmp(s1->pixels, s2->pixels, s1->pitch*s1->h);
	int a = SDL_Surfaces_equal(s1, s2);
	//printf("%d - %d", z, a);
	//printf("%d", s1->pitch*s1->h);
	
	//SDL_Texture *tex = SDL_CreateTextureFromSurface(renderer, s2);
	//SDL_RenderCopy(renderer, tex, NULL, NULL);
	//SDL_RenderPresent(renderer);
	//SDL_DestroyTexture(tex);
	uint8_t *buffer = malloc(1920000);
	SDL_Rect rectPtr = {0, 0, 800, 600}; // check this
	int res = SDL_RenderReadPixels(renderer, &rectPtr, SDL_PIXELFORMAT_RGB888, buffer, 3200);
	printf("%d - w: %d - h: %d - pf: %d", res, rectPtr.w, rectPtr.h, SDL_GetWindowPixelFormat(screen));
	printf("rgb888: %d", SDL_PIXELFORMAT_RGB888);
	SDL_Delay(2000);
	FinishOff();

	return 0;
}
