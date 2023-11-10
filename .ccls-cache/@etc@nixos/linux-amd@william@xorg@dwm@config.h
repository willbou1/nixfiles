/* See LICENSE file for copyright and license details. */

/* appearance */
static const unsigned int borderpx  = 3;        /* border pixel of windows */
static const unsigned int gappx     = 15;       /* size of the gap between windows */
static const unsigned int edgepx    = 25;
static const unsigned int snap      = 0;       /* snap pixel */
static const unsigned int swallowfloating = 1;
static const int showbar            = 1;        /* 0 means no bar */
static const int topbar             = 1;        /* 0 means bottom bar */
static const int focusonwheel       = 0;

/* tagging */
static const char *tags[] = { "🏠", "📚", "⚙️", "…", "🖥️", "Hidden" };
static const char *hiddentag = "Hidden";

const int mediaTagMask = 0b01000;
const int utilsTagMask = 0b00100;
const int vmTagMask    = 0b10000;

static const Rule rules[] = {
	/* xprop(1):
	 *	WM_CLASS(STRING) = instance, class
	 *	WM_NAME(STRING) = title
	 */
	/* class                   instance           title                       tags mask     isfloating   isterminal     noswallow      monitor */
	{ "URxvt",                 NULL,              NULL,                       0,            0,            1,            0,            -1 },
	{ "Alacritty",                 NULL,              NULL,                       0,            0,            1,            0,            -1 },
	{ "Virt-manager",          NULL,              "Virtual Machine Manager",  vmTagMask,    0,            1,            0,            -1 },
	{ "Remote-viewer",         NULL,              NULL,                       vmTagMask,    0,            0,            0,            -1 },
	{ "mpv",                   NULL,              NULL,                       mediaTagMask, 0,            0,            0,            -1 },
	{ "tidal-hifi",            NULL,              NULL,                       mediaTagMask, 0,            0,            0,            -1 },
	{ "Deluge-gtk",            NULL,              NULL,                       utilsTagMask, 0,            0,            0,            -1 },
	{ "Catia",                 NULL,              NULL,                       utilsTagMask, 0,            0,            0,            -1 },
	{ "Cadence",               NULL,              NULL,                       utilsTagMask, 0,            0,            0,            -1 },
	{ "corectrl",               NULL,              NULL,                       utilsTagMask, 0,            0,            0,            -1 },
	{ NULL,           NULL,              "ncpamixer",                       utilsTagMask, 0,            0,            0,            -1 },
	{ "FLTK",                  NULL,              NULL,                       utilsTagMask, 1,            0,            0,            -1 },
};

/* layout(s) */
static const float mfact     = 0.5; /* factor of master area size [0.05..0.95] */
static const int nmaster     = 1;    /* number of clients in master area */
static const int resizehints = 0;    /* 1 means respect size hints in tiled resizals */

static const Layout layouts[] = {
	/* symbol     arrange function */
	{ "[M]:",      tile },    /* first entry is default */
	{ "FLOAT",     NULL },    /* no layout function means floating behavior */
	{ "[]",        monocle },
	{ "[M]|",      doubledeck },
	{ ":M:",	centeredmaster },
	{ "TTT",	bstack },
	{ ">M>",	centeredfloatingmaster },
};

/* key definitions */
#define MODKEY Mod4Mask
#define TAGKEYS(KEY,TAG) \
	{ MODKEY|ControlMask,           KEY,      toggleview,     {.ui = 1 << TAG} }, \
	{ MODKEY,                       KEY,      view,           {.ui = 1 << TAG} }, \
	{ MODKEY|ShiftMask,             KEY,      tag,            {.ui = 1 << TAG} }, \
	{ MODKEY|ControlMask|ShiftMask, KEY,      toggletag,      {.ui = 1 << TAG} },

/* helper for spawning shell commands in the pre dwm-5.0 fashion */
#define SHCMD(cmd) { .v = (const char*[]){ "/bin/sh", "-c", cmd, NULL } }

/* commands */
static char dmenumon[2] = "0"; /* component of dmenucmd, manipulated in spawn() */
static const char *browsercmd[] = { "qutebrowser", NULL };
static const char *i2pbrowsercmd[] = { "i2p_chromium", NULL };
static const char *volupcmd[] = { "pactl", "set-sink-volume", "@DEFAULT_SINK@", "+5%", NULL };
static const char *voldowncmd[] = { "pactl", "set-sink-volume", "@DEFAULT_SINK@", "-5%", NULL };
static const char *mutecmd[] = { "pactl", "set-sink-mute", "@DEFAULT_SINK@", "toggle", NULL };
static const char *mediaprev[] = { "playerctl", "previous", NULL };
static const char *medianext[] = { "playerctl", "next", NULL };
static const char *mediatoggle[] = { "playerctl", "play-pause", NULL };
static const char *volstatcmd[] = { "pkill", "-RTMIN+10", "dwmblocks", NULL };
static const char *dmenu_media[] = { "/home/william/.config/dmenu/scripts/searchMedia.sh", NULL };
static const char *screencapcmd[] = { "flameshot", "gui", NULL };
static Key keys[] = {
	/* modifier                     key        function        argument */
	{ MODKEY,                       XK_p,      spawn,          {.v = dmenucmd } },
	{ MODKEY|ShiftMask,             XK_p,      spawn,          {.v = sdmenucmd } },
	{ MODKEY,                       XK_s,      spawn,          {.v = dmenu_media } },
	{ MODKEY,                       XK_Return, spawn,          {.v = termcmd } },
	{ MODKEY,                       XK_F10,      spawn,          {.v = voldowncmd } },
	{ MODKEY,                       XK_F11,      spawn,          {.v = volupcmd } },
	{ MODKEY,                       XK_F9,      spawn,          {.v = mutecmd } },
	{ MODKEY,                       XK_F10,      spawn,          {.v = volstatcmd } },
	{ MODKEY,                       XK_F11,      spawn,          {.v = volstatcmd } },
	{ MODKEY,                       XK_F9,      spawn,          {.v = volstatcmd } },
	{ 0,                            XF86XK_AudioPlay,      spawn,          {.v = mediatoggle } },
	{ 0,                            XF86XK_AudioPause,      spawn,          {.v = mediatoggle } },
	{ 0,                            XF86XK_AudioNext,      spawn,          {.v = medianext } },
	{ 0,                            XF86XK_AudioPrev,      spawn,          {.v = mediaprev } },
	{ MODKEY|ShiftMask,             XK_s,      spawn,          {.v = screencapcmd } },
	{ MODKEY,                       XK_b,      spawn,          {.v = browsercmd } },
	{ MODKEY|ShiftMask|ControlMask,                       XK_b,      spawn,          {.v = i2pbrowsercmd } },
	{ MODKEY|ShiftMask,                       XK_b,      togglebar,          NULL},
	{ MODKEY,                       XK_j,      focusstack,     {.i = +1 } },
	{ MODKEY,                       XK_k,      focusstack,     {.i = -1 } },
	{ MODKEY,                       XK_i,      incnmaster,     {.i = +1 } },
	{ MODKEY,                       XK_d,      incnmaster,     {.i = -1 } },
	{ MODKEY|ShiftMask,	XK_i,      incedge,     {.i = 25 } },
	{ MODKEY|ShiftMask,	XK_d,      decedge,     {.i = 25 } },
	{ MODKEY|ShiftMask|ControlMask,	XK_i,      incgap,     {.i = 15 } },
	{ MODKEY|ShiftMask|ControlMask,	XK_d,      decgap,     {.i = 15 } },
	{ MODKEY,                       XK_h,      setmfact,       {.f = -0.05} },
	{ MODKEY,                       XK_l,      setmfact,       {.f = +0.05} },
	{ MODKEY|ShiftMask,             XK_Return, zoom,           {0} },
	{ MODKEY,                       XK_Tab,    view,           {0} },
	{ MODKEY|ShiftMask,             XK_c,      killclient,     {0} },
	{ MODKEY,                       XK_t,      setlayout,      {.v = &layouts[0]} },
	{ MODKEY,                       XK_f,      setlayout,      {.v = &layouts[1]} },
	{ MODKEY,                       XK_m,      setlayout,      {.v = &layouts[2]} },
	{ MODKEY,                       XK_r,      setlayout,      {.v = &layouts[3]} },
	{ MODKEY,                       XK_u,      setlayout,      {.v = &layouts[4]} },
	{ MODKEY,                       XK_y,      setlayout,      {.v = &layouts[5]} },
	{ MODKEY,                       XK_e,      setlayout,      {.v = &layouts[6]} },
	{ MODKEY,                       XK_space,  setlayout,      {0} },
	{ MODKEY|ShiftMask,             XK_space,  togglefloating, {0} },
	{ MODKEY|ShiftMask,             XK_0,      tag,            {.ui = ~0 } },
	{ MODKEY,                       XK_comma,  focusmon,       {.i = -1 } },
	{ MODKEY,                       XK_period, focusmon,       {.i = +1 } },
	{ MODKEY|ShiftMask,             XK_comma,  tagmon,         {.i = -1 } },
	{ MODKEY|ShiftMask,             XK_period, tagmon,         {.i = +1 } },
	{ MODKEY, XK_o, tagswapmon, {.i = -1} },
//{ MODKEY|ShiftMask|ControlMask, XK_comma,  tagmon,         {.i = -1 } },
//	{ MODKEY|ShiftMask|ControlMask,	XK_period, tagmon,         {.i = +1 } },
	TAGKEYS(                        XK_1,                      0)
	TAGKEYS(                        XK_2,                      1)
	TAGKEYS(                        XK_3,                      2)
	TAGKEYS(                        XK_4,                      3)
	TAGKEYS(                        XK_5,                      4)
	TAGKEYS(                        XK_6,                      5)
	{ MODKEY|ShiftMask,             XK_q,      quit,           {0} },
	{ MODKEY|ControlMask|ShiftMask, XK_q,      quit,           {1} },
};

/* button definitions */
/* click can be ClkTagBar, ClkLtSymbol, ClkStatusText, ClkWinTitle, ClkClientWin, or ClkRootWin */
static Button buttons[] = {
	/* click                event mask      button          function        argument */
	{ ClkLtSymbol,          0,              Button1,        setlayout,      {0} },
	{ ClkLtSymbol,          0,              Button3,        setlayout,      {.v = &layouts[2]} },
	{ ClkWinTitle,          0,              Button2,        zoom,           {0} },
	{ ClkWinTitle,          0,              Button1,         setlayout,      {.v = &layouts[2]} },
	{ ClkStatusText,        0,              Button1,        sigdwmblocks,   {.i = 1} },
	{ ClkStatusText,        0,              Button2,        sigdwmblocks,   {.i = 2} },
	{ ClkStatusText,        0,              Button3,        sigdwmblocks,   {.i = 3} },
	{ ClkStatusText,        0,              Button4,        sigdwmblocks,   {.i = 4} },
	{ ClkStatusText,        0,              Button5,        sigdwmblocks,   {.i = 5} },
	{ ClkClientWin,         MODKEY,         Button1,        movemouse,      {0} },
	{ ClkClientWin,         MODKEY,         Button2,        togglefloating, {0} },
	{ ClkClientWin,         MODKEY,         Button3,        resizemouse,    {0} },
	{ ClkTagBar,            0,              Button1,        view,           {0} },
	{ ClkTagBar,            0,              Button3,        toggleview,     {0} },
	{ ClkTagBar,            MODKEY,         Button1,        tag,            {0} },
	{ ClkTagBar,            MODKEY,         Button3,        toggletag,      {0} },
};

