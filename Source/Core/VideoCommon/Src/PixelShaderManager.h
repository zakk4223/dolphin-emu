// Copyright 2013 Dolphin Emulator Project
// Licensed under GPLv2
// Refer to the license.txt file included.

#ifndef _PIXELSHADERMANAGER_H
#define _PIXELSHADERMANAGER_H

#include "BPMemory.h"
#include "XFMemory.h"
#include "PixelShaderGen.h"
#include "ConstantManager.h"

class PointerWrap;



// The non-API dependent parts.
class PixelShaderManager
{
public:
	static void Init();
	static void Dirty();
	static void Shutdown();
	static void DoState(PointerWrap &p);

	static void SetConstants(u32 components); // sets pixel shader constants

	// constant management, should be called after memory is committed
	static void SetColorChanged(int type, int index);
	static void SetAlpha();
	static void SetDestAlpha();
	static void SetTexDims(int texmapid, u32 width, u32 height, u32 wraps, u32 wrapt);
	static void SetZTextureBias();
	static void SetViewportChanged();
	static void SetIndMatrixChanged(int matrixidx);
	static void SetTevKSelChanged(int id);
	static void SetZTextureTypeChanged();
	static void SetIndTexScaleChanged(bool high);
	static void SetTexCoordChanged(u8 texmapid);
	static void SetFogColorChanged();
	static void SetFogParamChanged();
	static void SetFogRangeAdjustChanged();
	static void InvalidateXFRange(int start, int end);
	static void SetMaterialColorChanged(int index, u32 color);
	
	static PixelShaderConstants constants;
	static bool dirty;
};


#endif // _PIXELSHADERMANAGER_H
