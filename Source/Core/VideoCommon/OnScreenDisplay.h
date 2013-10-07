// Copyright 2013 Dolphin Emulator Project
// Licensed under GPLv2
// Refer to the license.txt file included.

#ifndef _OSD_H_
#define _OSD_H_

#include <functional>
#include <string>

namespace OSD
{
// On-screen message display
void AddMessage(const std::string& str, u32 ms = 2000);
void DrawMessages(); // draw the current messages on the screen. Only call once per frame.
void ClearMessages();

// On-screen callbacks
enum CallbackType
{
	OSD_INIT = 0,
	OSD_ONFRAME,
	OSD_SHUTDOWN
};
typedef std::function<void()> Callback;

void AddCallback(CallbackType type, Callback cb);
void DoCallbacks(CallbackType type);
}  // namespace OSD

#endif // _OSD_H_
