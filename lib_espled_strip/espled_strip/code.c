/*
	Copyright 2026 VESC project

	This file is part of the VESC firmware.

	The VESC firmware is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    The VESC firmware is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

// ext-espled-*: segmented addressable-LED strip engine as a native library.
//
// Lisp sets high-level segment /
// effect state and a background render thread animates, applies
// brightness / auto-white / an adaptive current limit and pushes pixels.
// The hardware path is the firmware rgbled driver through the C interface
// (VESC_IF->rgbled_init/update), which drives one pin at a time - segments
// on different pins are transmitted sequentially each frame. The firmware
// skips re-init when the pin does not change, so single-strip setups have
// no switching overhead.
//
// Native-lib constraints shape the implementation: on the RISC-V targets
// the lib executes in place from flash, so there are no writable globals -
// all state lives in one allocated struct reached through ARG - and no
// libm, so the effect math is integer only.
//
// Colors are packed 0xWWRRGGBB ints, like the color-* extensions.

#include "vesc_c_if.h"

HEADER

#define ESPLED_SEG_MAX     8
#define ESPLED_RENDER_MS   33  // ~30 fps

// Effects
enum {
	FX_SOLID = 0,
	FX_BREATHE,
	FX_CHASE,
	FX_RAINBOW,
	FX_SPARKLE,
	FX_COMET,
	FX_GAUGE,   // fill by the level param; battery gradient when color = 0
	FX_STROBE,  // hard on/off flash
	FX_LARSON,  // bouncing eye with tail (knight rider)
	FX_FELONY,  // halves alternate red/blue
};

// Color byte layouts on the wire
enum {
	TYPE_GRB = 0,
	TYPE_RGB,
	TYPE_GRBW,
	TYPE_RGBW,
};

// A palette is 4 anchor colors, interpolated across pos 0..255.
typedef struct { uint32_t c[4]; } palette_t;
static const palette_t palettes[] = {
	{{0xFF0000, 0x00FF00, 0x0000FF, 0xFFFFFF}}, // 0 rgbw-ish
	{{0xFF0000, 0xFF8000, 0xFFFF00, 0xFF0000}}, // 1 fire
	{{0x0000FF, 0x00FFFF, 0x00FF80, 0x0000FF}}, // 2 ocean
	{{0xFF00FF, 0x8000FF, 0x0080FF, 0xFF00FF}}, // 3 neon
	{{0xFFFFFF, 0xFF4000, 0x400000, 0x000000}}, // 4 ember
	{{0x00FF00, 0xFFFF00, 0xFF0000, 0x00FF00}}, // 5 traffic
	{{0xFFFFFF, 0x000000, 0xFFFFFF, 0x000000}}, // 6 strobe
	{{0x1030FF, 0xFFFFFF, 0x1030FF, 0x001040}}, // 7 police-blue
};
#define PALETTE_COUNT ((int)(sizeof(palettes) / sizeof(palettes[0])))

typedef struct {
	bool defined;
	bool on;
	uint8_t pin;
	uint8_t type;      // TYPE_*
	uint16_t len;      // pixels
	bool reverse;
	uint8_t fx;
	uint8_t pal;
	uint8_t bri;       // per-segment brightness 0..255
	uint8_t spd;       // 0..255
	uint8_t size;      // chase head / comet tail length
	uint8_t level;     // gauge fill 0..255
	uint32_t color;    // packed 0xWWRRGGBB
	uint32_t phase;    // frames since effect start

	// Wire bytes, len * 4. Per segment: the firmware LED driver transmits
	// asynchronously from the caller's buffer, so a segment's buffer must
	// stay untouched until the next transmission waits for it.
	uint8_t *txbuf;
} seg_t;

typedef struct {
	lib_thread thread;
	lib_mutex lock;
	volatile bool running;

	seg_t seg[ESPLED_SEG_MAX];
	int seg_count;

	uint8_t master_bri;
	bool auto_white;
	uint32_t ablimit_ma; // 0 = off

	uint16_t buf_len;    // pixels the work buffer holds
	uint32_t *work;      // packed 0xWWRRGGBB, buf_len entries
} espled_t;

static espled_t *state(void) {
	return (espled_t*)ARG;
}

// ---- Color helpers ------------------------------------------------------

static uint32_t pack(uint32_t r, uint32_t g, uint32_t b, uint32_t w) {
	return (w << 24) | (r << 16) | (g << 8) | b;
}

static uint32_t scale(uint32_t c, uint32_t num) { // num 0..255
	uint32_t w = (((c >> 24) & 0xFF) * num) / 255;
	uint32_t r = (((c >> 16) & 0xFF) * num) / 255;
	uint32_t g = (((c >> 8) & 0xFF) * num) / 255;
	uint32_t b = ((c & 0xFF) * num) / 255;
	return pack(r, g, b, w);
}

static uint32_t palette_at(uint8_t pal, uint8_t pos) {
	const palette_t *p = &palettes[pal % PALETTE_COUNT];
	uint32_t a = p->c[pos / 64];
	uint32_t b = p->c[(pos / 64 + 1) % 4];
	uint32_t f = pos % 64; // 0..63 between anchors

	uint32_t r = (((a >> 16) & 0xFF) * (63 - f) + ((b >> 16) & 0xFF) * f) / 63;
	uint32_t g = (((a >> 8) & 0xFF) * (63 - f) + ((b >> 8) & 0xFF) * f) / 63;
	uint32_t bl = ((a & 0xFF) * (63 - f) + (b & 0xFF) * f) / 63;
	return pack(r, g, bl, 0);
}

// Triangle wave 0..255..0 over a 512-step period.
static uint32_t triangle(uint32_t x) {
	x &= 511;
	return x < 256 ? x : 511 - x;
}

// ---- Effect renderers ---------------------------------------------------

static void fx_render(const seg_t *s, uint32_t *work) {
	int n = s->len;
	uint32_t ph = s->phase;
	uint32_t spd = s->spd ? s->spd : 32;
	int size = s->size ? s->size : 8;

	switch (s->fx) {
	case FX_BREATHE: {
		uint32_t b = triangle((ph * spd) / 32);
		uint32_t c = scale(s->color, b);
		for (int i = 0; i < n; i++) work[i] = c;
	} break;

	case FX_CHASE: {
		int head = (int)((ph * spd / 32) % (uint32_t)(n > 0 ? n : 1));
		for (int i = 0; i < n; i++) {
			int d = i - head;
			if (d < 0) d += n;
			uint32_t b = d < size ? 255 - (d * 255) / size : 0;
			uint32_t c = s->color ? s->color
				: palette_at(s->pal, (uint8_t)((i * 255) / (n ? n : 1)));
			work[i] = scale(c, b);
		}
	} break;

	case FX_RAINBOW: {
		for (int i = 0; i < n; i++) {
			uint8_t pos = (uint8_t)((i * 255) / (n ? n : 1) + (ph * spd) / 32);
			work[i] = palette_at(s->pal, pos);
		}
	} break;

	case FX_SPARKLE: {
		uint32_t c = s->color ? s->color : 0xFFFFFF;
		for (int i = 0; i < n; i++) {
			// Deterministic twinkle from phase + index
			uint32_t h = ((uint32_t)i * 2654435761u) ^ (ph * 40503u);
			work[i] = ((h >> 8) & 0xFF) < (spd / 2 + 1) ? c : 0;
		}
	} break;

	case FX_COMET: {
		int head = (int)((ph * spd / 32) % (uint32_t)(n > 0 ? n : 1));
		for (int i = 0; i < n; i++) {
			int d = head - i;
			if (d < 0) d += n;
			uint32_t b = d < size ? 255 - (d * 255) / size : 0;
			uint32_t c = s->color ? s->color : palette_at(s->pal, (uint8_t)ph);
			work[i] = scale(c, b);
		}
	} break;

	case FX_GAUGE: {
		// Fill the first level/255 of the strip. With color 0 the fill is
		// a battery-style gradient: red when nearly empty, green when
		// full. spd > 0 pulses the fill (e.g. while charging).
		int lit = (n * s->level + 254) / 255;
		if (s->level > 0 && lit < 1) lit = 1;
		uint32_t b = s->spd ? 140 + triangle((ph * s->spd) / 32) * 115 / 255
			: 255;
		uint32_t c;
		if (s->color) {
			c = s->color;
		} else if (s->level < 51) { // < 20%: red
			c = 0xFF0000;
		} else {
			uint32_t g = ((uint32_t)s->level * 255) / 204; // level/0.8
			if (g > 255) g = 255;
			c = pack(255 - g, g, 0, 0);
		}
		c = scale(c, b);
		for (int i = 0; i < n; i++) work[i] = i < lit ? c : 0;
	} break;

	case FX_STROBE: {
		uint32_t c = s->color ? s->color : 0xFFFFFF;
		bool lit = ((ph * spd) / 64) & 1;
		for (int i = 0; i < n; i++) work[i] = lit ? c : 0;
	} break;

	case FX_LARSON: {
		int span = n > 1 ? n - 1 : 1;
		int pos = (int)((ph * spd / 16) % (uint32_t)(2 * span));
		if (pos > span) pos = 2 * span - pos;
		uint32_t c = s->color ? s->color : 0xFF0000;
		for (int i = 0; i < n; i++) {
			int d = i > pos ? i - pos : pos - i;
			uint32_t b = d < size ? 255 - (d * 255) / size : 0;
			work[i] = scale(c, b);
		}
	} break;

	case FX_FELONY: {
		bool swap = ((ph * spd) / 48) & 1;
		uint32_t c1 = swap ? 0x0000FF : 0xFF0000;
		uint32_t c2 = swap ? 0xFF0000 : 0x0000FF;
		for (int i = 0; i < n; i++) {
			work[i] = i < n / 2 ? c1 : c2;
		}
	} break;

	case FX_SOLID:
	default:
		for (int i = 0; i < n; i++) work[i] = s->color;
		break;
	}
}

// ---- Render thread ------------------------------------------------------

static void render_seg(espled_t *st, seg_t *s) {
	int n = s->len;
	uint32_t *work = st->work;
	uint8_t *tx = s->txbuf;
	int colors = s->type >= TYPE_GRBW ? 4 : 3;

	fx_render(s, work);

	// Combined per-segment and master brightness
	uint32_t bri = ((uint32_t)s->bri * st->master_bri) / 255;

	uint32_t sum = 0; // channel sum for the current estimate
	for (int i = 0; i < n; i++) {
		uint32_t c = scale(work[i], bri);

		uint32_t w = (c >> 24) & 0xFF;
		uint32_t r = (c >> 16) & 0xFF;
		uint32_t g = (c >> 8) & 0xFF;
		uint32_t b = c & 0xFF;

		// Derive white from the common RGB part on RGBW strips
		if (st->auto_white && colors == 4 && w == 0) {
			w = r < g ? (r < b ? r : b) : (g < b ? g : b);
			r -= w; g -= w; b -= w;
		}

		work[i] = pack(r, g, b, w);
		sum += r + g + b + w;
	}

	// Adaptive current limit: ~20 mA per full channel + 1 mA idle per LED
	if (st->ablimit_ma) {
		uint32_t ma = (sum * 20) / 255 + n;
		if (ma > st->ablimit_ma) {
			uint32_t num = st->ablimit_ma > (uint32_t)n ? st->ablimit_ma - n : 0;
			uint32_t den = ma - n;
			for (int i = 0; i < n; i++) {
				work[i] = scale(work[i], (num * 255) / den);
			}
		}
	}

	for (int i = 0; i < n; i++) {
		uint32_t c = work[s->reverse ? n - 1 - i : i];
		uint32_t w = (c >> 24) & 0xFF;
		uint32_t r = (c >> 16) & 0xFF;
		uint32_t g = (c >> 8) & 0xFF;
		uint32_t b = c & 0xFF;

		uint8_t *px = tx + i * colors;
		switch (s->type) {
		case TYPE_RGB:  px[0] = r; px[1] = g; px[2] = b; break;
		case TYPE_GRBW: px[0] = g; px[1] = r; px[2] = b; px[3] = w; break;
		case TYPE_RGBW: px[0] = r; px[1] = g; px[2] = b; px[3] = w; break;
		case TYPE_GRB:
		default:        px[0] = g; px[1] = r; px[2] = b; break;
		}
	}
}

static void render_thd(void *arg) {
	espled_t *st = (espled_t*)arg;

	while (!VESC_IF->should_terminate()) {
		for (int i = 0; i < st->seg_count; i++) {
			seg_t *s = &st->seg[i];

			VESC_IF->mutex_lock(st->lock);
			bool active = s->defined && s->on && s->txbuf != NULL
				&& s->len > 0 && s->len <= st->buf_len;
			int tx_bytes = 0;
			int pin = s->pin;
			uint8_t *tx = s->txbuf;
			if (active) {
				render_seg(st, s);
				s->phase++;
				tx_bytes = s->len * (s->type >= TYPE_GRBW ? 4 : 3);
			}
			VESC_IF->mutex_unlock(st->lock);

			// Hardware IO outside the lock - the firmware driver can block
			// while a previous transmission finishes. Re-init is a no-op
			// when the pin is unchanged.
			if (active && VESC_IF->rgbled_init(pin)) {
				VESC_IF->rgbled_update(tx, tx_bytes);
			}
		}

		VESC_IF->sleep_ms(ESPLED_RENDER_MS);
	}
}

// ---- Extension helpers --------------------------------------------------

static bool check_num_args(lbm_value *args, lbm_uint argn, lbm_uint n) {
	if (argn != n) {
		return false;
	}
	for (lbm_uint i = 0; i < argn; i++) {
		if (!VESC_IF->lbm_is_number(args[i])) {
			return false;
		}
	}
	return true;
}

static seg_t *seg_arg(espled_t *st, lbm_value v) {
	int i = VESC_IF->lbm_dec_as_i32(v);
	if (i < 0 || i >= ESPLED_SEG_MAX) {
		return NULL;
	}
	return &st->seg[i];
}

// ---- Extensions ---------------------------------------------------------

// (ext-espled-seg-def i pin type len) - define segment i before ext-espled-init.
// type: 0 GRB, 1 RGB, 2 GRBW, 3 RGBW
static lbm_value ext_seg_def(lbm_value *args, lbm_uint argn) {
	espled_t *st = state();
	if (!check_num_args(args, argn, 4)) return VESC_IF->lbm_enc_sym_terror;

	seg_t *s = seg_arg(st, args[0]);
	int pin = VESC_IF->lbm_dec_as_i32(args[1]);
	int type = VESC_IF->lbm_dec_as_i32(args[2]);
	int len = VESC_IF->lbm_dec_as_i32(args[3]);

	if (!s || pin < 0 || pin > 255 || type < 0 || type > TYPE_RGBW
		|| len < 1 || len > 1024) {
		return VESC_IF->lbm_enc_sym_terror;
	}

	if (st->running) {
		VESC_IF->lbm_set_error_reason(
			"Stop with ext-espled-deinit before redefining segments");
		return VESC_IF->lbm_enc_sym_eerror;
	}

	VESC_IF->mutex_lock(st->lock);
	s->defined = true;
	s->on = true;
	s->pin = (uint8_t)pin;
	s->type = (uint8_t)type;
	s->len = (uint16_t)len;
	s->reverse = false;
	s->fx = FX_SOLID;
	s->pal = 0;
	s->bri = 255;
	s->spd = 32;
	s->size = 8;
	s->color = 0;
	s->phase = 0;
	VESC_IF->mutex_unlock(st->lock);

	return VESC_IF->lbm_enc_sym_true;
}

// (ext-espled-init n) - start rendering the first n segments.
static lbm_value ext_init(lbm_value *args, lbm_uint argn) {
	espled_t *st = state();
	if (!check_num_args(args, argn, 1)) return VESC_IF->lbm_enc_sym_terror;

	int n = VESC_IF->lbm_dec_as_i32(args[0]);
	if (n < 1 || n > ESPLED_SEG_MAX) return VESC_IF->lbm_enc_sym_terror;

	if (st->running) {
		VESC_IF->lbm_set_error_reason("Already running");
		return VESC_IF->lbm_enc_sym_eerror;
	}

	uint16_t max_len = 0;
	for (int i = 0; i < n; i++) {
		if (!st->seg[i].defined) {
			VESC_IF->lbm_set_error_reason("Segment not defined");
			return VESC_IF->lbm_enc_sym_eerror;
		}
		if (st->seg[i].len > max_len) max_len = st->seg[i].len;
	}

	bool alloc_ok = true;
	st->work = VESC_IF->malloc(max_len * sizeof(uint32_t));
	alloc_ok = st->work != NULL;
	for (int i = 0; i < n && alloc_ok; i++) {
		st->seg[i].txbuf = VESC_IF->malloc(st->seg[i].len * 4);
		alloc_ok = st->seg[i].txbuf != NULL;
	}

	if (!alloc_ok) {
		if (st->work) { VESC_IF->free(st->work); st->work = NULL; }
		for (int i = 0; i < n; i++) {
			if (st->seg[i].txbuf) {
				VESC_IF->free(st->seg[i].txbuf);
				st->seg[i].txbuf = NULL;
			}
		}
		return VESC_IF->lbm_enc_sym_merror;
	}
	st->buf_len = max_len;
	st->seg_count = n;

	st->thread = VESC_IF->spawn(render_thd, 3072, "espled_render", st);
	if (!st->thread) {
		VESC_IF->free(st->work); st->work = NULL;
		for (int i = 0; i < n; i++) {
			VESC_IF->free(st->seg[i].txbuf);
			st->seg[i].txbuf = NULL;
		}
		st->seg_count = 0;
		return VESC_IF->lbm_enc_sym_eerror;
	}
	st->running = true;

	return VESC_IF->lbm_enc_sym_true;
}

static void espled_stop(espled_t *st) {
	if (!st->running) {
		return;
	}
	VESC_IF->request_terminate(st->thread);
	st->running = false;

	VESC_IF->rgbled_deinit();

	VESC_IF->free(st->work); st->work = NULL;
	for (int i = 0; i < ESPLED_SEG_MAX; i++) {
		if (st->seg[i].txbuf) {
			VESC_IF->free(st->seg[i].txbuf);
			st->seg[i].txbuf = NULL;
		}
	}
	st->buf_len = 0;
	st->seg_count = 0;
}

// (ext-espled-deinit)
static lbm_value ext_deinit(lbm_value *args, lbm_uint argn) {
	(void)args; (void)argn;
	espled_stop(state());
	return VESC_IF->lbm_enc_sym_true;
}

// (ext-espled-seg-look i fx pal color spd bri) - full appearance in one call.
static lbm_value ext_seg_look(lbm_value *args, lbm_uint argn) {
	espled_t *st = state();
	if (!check_num_args(args, argn, 6)) return VESC_IF->lbm_enc_sym_terror;

	seg_t *s = seg_arg(st, args[0]);
	if (!s) return VESC_IF->lbm_enc_sym_terror;

	VESC_IF->mutex_lock(st->lock);
	s->fx = (uint8_t)VESC_IF->lbm_dec_as_i32(args[1]);
	s->pal = (uint8_t)VESC_IF->lbm_dec_as_i32(args[2]);
	s->color = VESC_IF->lbm_dec_as_u32(args[3]);
	s->spd = (uint8_t)VESC_IF->lbm_dec_as_i32(args[4]);
	s->bri = (uint8_t)VESC_IF->lbm_dec_as_i32(args[5]);
	s->phase = 0;
	VESC_IF->mutex_unlock(st->lock);

	return VESC_IF->lbm_enc_sym_true;
}

// Setters shared by the per-segment and all-segment variants. Field ids
// keep one implementation for all the small setters.
enum { SET_FX = 0, SET_PAL, SET_BRI, SET_SPD, SET_COLOR, SET_ON, SET_REVERSE, SET_SIZE, SET_LEVEL };

static void seg_set(seg_t *s, int field, uint32_t v) {
	switch (field) {
	case SET_FX:      if (s->fx != (uint8_t)v) { s->fx = (uint8_t)v; s->phase = 0; } break;
	case SET_PAL:     s->pal = (uint8_t)v; break;
	case SET_BRI:     s->bri = (uint8_t)v; break;
	case SET_SPD:     s->spd = (uint8_t)v; break;
	case SET_COLOR:   s->color = v; break;
	case SET_ON:      s->on = v != 0; break;
	case SET_REVERSE: s->reverse = v != 0; break;
	case SET_SIZE:    s->size = (uint8_t)v; break;
	case SET_LEVEL:   s->level = (uint8_t)v; break;
	}
}

static lbm_value set_one(lbm_value *args, lbm_uint argn, int field) {
	espled_t *st = state();
	if (!check_num_args(args, argn, 2)) return VESC_IF->lbm_enc_sym_terror;

	seg_t *s = seg_arg(st, args[0]);
	if (!s) return VESC_IF->lbm_enc_sym_terror;

	VESC_IF->mutex_lock(st->lock);
	seg_set(s, field, VESC_IF->lbm_dec_as_u32(args[1]));
	VESC_IF->mutex_unlock(st->lock);
	return VESC_IF->lbm_enc_sym_true;
}

static lbm_value set_all(lbm_value *args, lbm_uint argn, int field) {
	espled_t *st = state();
	if (!check_num_args(args, argn, 1)) return VESC_IF->lbm_enc_sym_terror;

	VESC_IF->mutex_lock(st->lock);
	for (int i = 0; i < ESPLED_SEG_MAX; i++) {
		seg_set(&st->seg[i], field, VESC_IF->lbm_dec_as_u32(args[0]));
	}
	VESC_IF->mutex_unlock(st->lock);
	return VESC_IF->lbm_enc_sym_true;
}

// (ext-espled-seg-fx i fx) / (ext-espled-fx fx)
static lbm_value ext_seg_fx(lbm_value *a, lbm_uint n) { return set_one(a, n, SET_FX); }
static lbm_value ext_fx(lbm_value *a, lbm_uint n) { return set_all(a, n, SET_FX); }

// (ext-espled-seg-pal i pal) / (ext-espled-pal pal)
static lbm_value ext_seg_pal(lbm_value *a, lbm_uint n) { return set_one(a, n, SET_PAL); }
static lbm_value ext_pal(lbm_value *a, lbm_uint n) { return set_all(a, n, SET_PAL); }

// (ext-espled-seg-bri i bri)
static lbm_value ext_seg_bri(lbm_value *a, lbm_uint n) { return set_one(a, n, SET_BRI); }

// (ext-espled-seg-spd i spd)
static lbm_value ext_seg_spd(lbm_value *a, lbm_uint n) { return set_one(a, n, SET_SPD); }

// (ext-espled-seg-size i size) - chase head / comet tail length
static lbm_value ext_seg_size(lbm_value *a, lbm_uint n) { return set_one(a, n, SET_SIZE); }

// (ext-espled-seg-level i level) - gauge fill 0..255
static lbm_value ext_seg_level(lbm_value *a, lbm_uint n) { return set_one(a, n, SET_LEVEL); }

// (ext-espled-seg-col i color) / (ext-espled-col color) - packed 0xWWRRGGBB
static lbm_value ext_seg_col(lbm_value *a, lbm_uint n) { return set_one(a, n, SET_COLOR); }
static lbm_value ext_col(lbm_value *a, lbm_uint n) { return set_all(a, n, SET_COLOR); }

// (ext-espled-seg-on i on)
static lbm_value ext_seg_on(lbm_value *a, lbm_uint n) { return set_one(a, n, SET_ON); }

// (ext-espled-seg-reverse i rev)
static lbm_value ext_seg_reverse(lbm_value *a, lbm_uint n) { return set_one(a, n, SET_REVERSE); }

// (ext-espled-col-rgb r g b) / (ext-espled-col-rgbw r g b w) - solid color on
// all segments, like fled-col-rgb.
static lbm_value ext_col_rgbw(lbm_value *args, lbm_uint argn) {
	espled_t *st = state();
	if (argn != 3 && argn != 4) return VESC_IF->lbm_enc_sym_terror;
	for (lbm_uint i = 0; i < argn; i++) {
		if (!VESC_IF->lbm_is_number(args[i])) return VESC_IF->lbm_enc_sym_terror;
	}

	uint32_t r = VESC_IF->lbm_dec_as_u32(args[0]) & 0xFF;
	uint32_t g = VESC_IF->lbm_dec_as_u32(args[1]) & 0xFF;
	uint32_t b = VESC_IF->lbm_dec_as_u32(args[2]) & 0xFF;
	uint32_t w = argn == 4 ? VESC_IF->lbm_dec_as_u32(args[3]) & 0xFF : 0;
	uint32_t c = pack(r, g, b, w);

	VESC_IF->mutex_lock(st->lock);
	for (int i = 0; i < ESPLED_SEG_MAX; i++) {
		st->seg[i].color = c;
		st->seg[i].fx = FX_SOLID;
	}
	VESC_IF->mutex_unlock(st->lock);
	return VESC_IF->lbm_enc_sym_true;
}

// (ext-espled-bri b) - master brightness 0..255
static lbm_value ext_bri(lbm_value *args, lbm_uint argn) {
	espled_t *st = state();
	if (!check_num_args(args, argn, 1)) return VESC_IF->lbm_enc_sym_terror;
	st->master_bri = (uint8_t)VESC_IF->lbm_dec_as_i32(args[0]);
	return VESC_IF->lbm_enc_sym_true;
}

// (ext-espled-auto-white en) - derive W from RGB on RGBW strips
static lbm_value ext_auto_white(lbm_value *args, lbm_uint argn) {
	espled_t *st = state();
	if (!check_num_args(args, argn, 1)) return VESC_IF->lbm_enc_sym_terror;
	st->auto_white = VESC_IF->lbm_dec_as_i32(args[0]) != 0;
	return VESC_IF->lbm_enc_sym_true;
}

// (ext-espled-ablimit ma) - adaptive current cap in mA, 0 = off
static lbm_value ext_ablimit(lbm_value *args, lbm_uint argn) {
	espled_t *st = state();
	if (!check_num_args(args, argn, 1)) return VESC_IF->lbm_enc_sym_terror;
	int ma = VESC_IF->lbm_dec_as_i32(args[0]);
	st->ablimit_ma = ma < 0 ? 0 : (uint32_t)ma;
	return VESC_IF->lbm_enc_sym_true;
}

// ---- Lifecycle ----------------------------------------------------------

static void stop(void *arg) {
	espled_t *st = (espled_t*)arg;

	if (st) {
		espled_stop(st);
		VESC_IF->free(st->lock);
		VESC_IF->free(st);
	}

	VESC_IF->printf("espled-strip lib stopped");
}

INIT_FUN(lib_info *info) {
	INIT_START

	espled_t *st = VESC_IF->malloc(sizeof(espled_t));
	if (!st) {
		return false;
	}

	for (unsigned int i = 0; i < sizeof(espled_t); i++) {
		((uint8_t*)st)[i] = 0;
	}

	st->master_bri = 255;
	st->lock = VESC_IF->mutex_create();
	if (!st->lock) {
		VESC_IF->free(st);
		return false;
	}

	info->arg = st;
	info->stop_fun = stop;

	VESC_IF->lbm_add_extension("ext-espled-seg-def", ext_seg_def);
	VESC_IF->lbm_add_extension("ext-espled-init", ext_init);
	VESC_IF->lbm_add_extension("ext-espled-deinit", ext_deinit);
	VESC_IF->lbm_add_extension("ext-espled-seg-look", ext_seg_look);
	VESC_IF->lbm_add_extension("ext-espled-seg-fx", ext_seg_fx);
	VESC_IF->lbm_add_extension("ext-espled-fx", ext_fx);
	VESC_IF->lbm_add_extension("ext-espled-seg-pal", ext_seg_pal);
	VESC_IF->lbm_add_extension("ext-espled-pal", ext_pal);
	VESC_IF->lbm_add_extension("ext-espled-seg-bri", ext_seg_bri);
	VESC_IF->lbm_add_extension("ext-espled-seg-spd", ext_seg_spd);
	VESC_IF->lbm_add_extension("ext-espled-seg-size", ext_seg_size);
	VESC_IF->lbm_add_extension("ext-espled-seg-level", ext_seg_level);
	VESC_IF->lbm_add_extension("ext-espled-seg-col", ext_seg_col);
	VESC_IF->lbm_add_extension("ext-espled-col", ext_col);
	VESC_IF->lbm_add_extension("ext-espled-seg-on", ext_seg_on);
	VESC_IF->lbm_add_extension("ext-espled-seg-reverse", ext_seg_reverse);
	VESC_IF->lbm_add_extension("ext-espled-col-rgb", ext_col_rgbw);
	VESC_IF->lbm_add_extension("ext-espled-col-rgbw", ext_col_rgbw);
	VESC_IF->lbm_add_extension("ext-espled-bri", ext_bri);
	VESC_IF->lbm_add_extension("ext-espled-auto-white", ext_auto_white);
	VESC_IF->lbm_add_extension("ext-espled-ablimit", ext_ablimit);

	VESC_IF->printf("espled-strip lib loaded");

	return true;
}
