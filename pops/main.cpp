#include <iostream>
#include <SDL2/SDL.h>
#include <SDL2/SDL_image.h>
#include <cmath>
#include <sys/time.h>
#include <sys/types.h>

#include "lmap.h"

const int screen_w = 1600;
const int screen_h = 1000;

using namespace std;

SDL_Rect plot = { .x = 64, .y = 64, .w = screen_w - 128, .h = screen_h
                  - 128 };

void drawGrid(SDL_Renderer *ren) {
  SDL_SetRenderDrawBlendMode(ren, SDL_BLENDMODE_ADD);
  SDL_SetRenderDrawColor(ren, 255, 255, 0, 128);
  SDL_RenderDrawRect(ren, NULL);
  SDL_RenderDrawLine(ren, 0, (plot.h/2), plot.w, (plot.h/2));
  SDL_RenderDrawLine(ren, (plot.w/2), 0, (plot.w/2), plot.h);
}

uint64_t getmillis() {
  struct timeval tv;
  if (gettimeofday(&tv, NULL) != -1) {
    return long(((tv.tv_sec * 1000000) + tv.tv_usec)/1000);
  }
  return 0;
}

// Population growth functions - logistic map
double popf_log(double prev, double r) {
  return r * prev * (1-prev);
}

// Population growth functions - Ricker model
double popf_ricker(double prev, double r) {
  const double k = 1;
  return prev * exp(r * (1.0 - (prev/k)));
}

void draw_convergence(SDL_Renderer *ren, int x,
                      double r_start, double r_end,
                      int iterations,
                      int fade_limit) {
  vector<pair<double,int>> vals;
  double r = r_start+(((double)x*(r_end - r_start))/((double)plot.w));

  // popfniter(popf_ricker, iterations, r, 0.5, 255, vals, 0);
  popfniter(popf_log, iterations, r, 0.5, 255, vals, 0);

  int numvals = vals.size();

  for (auto v : vals) {
    int y = plot.h - (v.first * plot.h);
    int col = (v.second > 255 ? 255 : ((v.second < 0) ? 0 : v.second));
    if ((col > 0) || (numvals > fade_limit)) {
      if (numvals > fade_limit) {
        int chaos_col = 255*numvals/iterations;
        SDL_SetRenderDrawBlendMode(ren, SDL_BLENDMODE_ADD);
        SDL_SetRenderDrawColor(ren, 255, 255, 255, 255-chaos_col);
      } else {
        SDL_SetRenderDrawBlendMode(ren, SDL_BLENDMODE_NONE);
        SDL_SetRenderDrawColor(ren, 255, 255, 255, 255);
      }
      SDL_RenderDrawPoint(ren, x, y);
    }
  }
}

void update(SDL_Renderer *ren, SDL_Texture *bb) {
  SDL_SetRenderTarget(ren, NULL);
  SDL_RenderCopy(ren, bb, NULL, NULL);
  SDL_RenderPresent(ren);
  SDL_SetRenderTarget(ren, bb);
}

void draw(SDL_Renderer *ren, SDL_Texture *bb) {
  uint64_t last_update = getmillis();
  
  SDL_RenderSetViewport(ren, &plot);
  drawGrid(ren);
  SDL_SetRenderDrawColor(ren, 255, 0, 0, 255);
  for (int i=0; i<plot.w; i++) {
    uint64_t cur = getmillis();

    draw_convergence(ren, i, 1, 4, 1500, 500);
    if ((cur - last_update) > 100) {
      SDL_RenderSetViewport(ren, NULL);
      update(ren, bb);
      SDL_RenderSetViewport(ren, &plot);
      last_update = cur;
    }
  }

  update(ren, bb);
  SDL_SetRenderDrawBlendMode(ren, SDL_BLENDMODE_NONE);
  SDL_RenderSetViewport(ren, NULL);
}

bool dirty = true;
void render(SDL_Renderer *ren, SDL_Texture *bb) {
  if (!dirty) {
    return;
  }
  SDL_RenderClear(ren);
  draw(ren, bb);
  dirty = false;
}

int show() {
  int rc = 1;
  SDL_Window *win = nullptr;
  SDL_Renderer *ren = nullptr;
  SDL_Event e;
  SDL_Texture *bb = nullptr;
  bool done = false;
  
  win = SDL_CreateWindow("Pops!", 160, 100, screen_w, screen_h, SDL_WINDOW_SHOWN);
  if (win == nullptr){
    std::cout << "SDL_CreateWindow Error: " << SDL_GetError() << std::endl;
    goto cleanup;
  }

  ren = SDL_CreateRenderer(win, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
  if (ren == nullptr){
    std::cout << "SDL_CreateRenderer Error: " << SDL_GetError() << std::endl;
    goto cleanup;
  }
  SDL_RenderPresent(ren);

  bb = SDL_CreateTexture(ren,
                         SDL_PIXELFORMAT_RGBA8888,
                         SDL_TEXTUREACCESS_TARGET,
                         screen_w, screen_h);
  if (bb == nullptr) {
    goto cleanup;
  }
  SDL_SetRenderTarget(ren, bb);

  while (!done) {
    while (SDL_WaitEventTimeout(&e, 500) && (!done)) {
      switch (e.type) {
      case SDL_QUIT:
      case SDL_MOUSEBUTTONDOWN:
        done = true;
        break;
      case SDL_KEYDOWN:
        if (e.key.keysym.sym == SDLK_q)
          done = true;
        break;
      case SDL_WINDOWEVENT:
        if (e.window.event == SDL_WINDOWEVENT_SHOWN) {
          update(ren, bb);
        }
        break;
      default:
        ;
      }
    }
    render(ren, bb);
  }
  cout << "done. " << ::endl;

  rc = 0;

 cleanup:
  if (bb != nullptr) {
    SDL_DestroyTexture(bb);
  }
  if (win != nullptr) {
    SDL_DestroyWindow(win);
  }
  if (ren != nullptr) {
    SDL_DestroyRenderer(ren);
  }
  return rc;
}

int main(int, char**){
  if (SDL_Init(SDL_INIT_VIDEO) != 0){
    cout << "SDL_Init Error: " << SDL_GetError() << ::endl;
    return 1;
  }
  show();
  SDL_Quit();
  return 0;
}
