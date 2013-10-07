// Copyright 2013 Dolphin Emulator Project
// Licensed under GPLv2
// Refer to the license.txt file included.

#include <cmath>

#include "Common.h"
#include "Statistics.h"
#include "PixelShaderManager.h"
#include "VideoCommon.h"
#include "VideoConfig.h"

#include "RenderBase.h"
static int s_nIndTexMtxChanged;
static bool s_bFogRangeAdjustChanged;
static int nLightsChanged[2]; // min,max
static u8 s_nIndTexScaleChanged;
static int nMaterialsChanged;

PixelShaderConstants PixelShaderManager::constants;
bool PixelShaderManager::dirty;

inline void SetPSConstant4f(unsigned int const_number, float f1, float f2, float f3, float f4)
{
	float4* c = (float4*) &PixelShaderManager::constants;
	c[const_number][0] = f1;
	c[const_number][1] = f2;
	c[const_number][2] = f3;
	c[const_number][3] = f4;
	PixelShaderManager::dirty = true;
}

inline void SetPSConstant4fv(unsigned int const_number, const float *f)
{
	float4* c = (float4*) &PixelShaderManager::constants;
	c[const_number][0] = f[0];
	c[const_number][1] = f[1];
	c[const_number][2] = f[2];
	c[const_number][3] = f[3];
	PixelShaderManager::dirty = true;
}

inline void SetMultiPSConstant4fv(unsigned int const_number, unsigned int count, const float *f)
{
	float4* c = (float4*) &PixelShaderManager::constants;
	for(u32 i=0; i<count; i++)
	{
		c[const_number+i][0] = f[0 + 4*i];
		c[const_number+i][1] = f[1 + 4*i];
		c[const_number+i][2] = f[2 + 4*i];
		c[const_number+i][3] = f[3 + 4*i];
	}
	PixelShaderManager::dirty = true;
}

void PixelShaderManager::Init()
{
	memset(&constants, 0, sizeof(constants));
	Dirty();
}

void PixelShaderManager::Dirty()
{
	s_nIndTexScaleChanged = 0xFF;
	s_nIndTexMtxChanged = 15;
	s_bFogRangeAdjustChanged = true;
	nLightsChanged[0] = 0; nLightsChanged[1] = 0x80;
	nMaterialsChanged = 15;
	dirty = true;
}

void PixelShaderManager::Shutdown()
{

}

void PixelShaderManager::SetConstants(u32 components)
{
	// indirect incoming texture scales
	if (s_nIndTexScaleChanged)
	{
		// set as two sets of vec4s, each containing S and T of two ind stages.
		float f[8];

        if (s_nIndTexScaleChanged & 0x03)
		{
			for (u32 i = 0; i < 2; ++i)
			{
                f[2 * i] = bpmem.texscale[0].getScaleS(i & 1);
                f[2 * i + 1] = bpmem.texscale[0].getScaleT(i & 1);
                PRIM_LOG("tex indscale%d: %f %f\n", i, f[2 * i], f[2 * i + 1]);
            }
			SetPSConstant4fv(C_INDTEXSCALE, f);
        }

		if (s_nIndTexScaleChanged & 0x0c)
		{
            for (u32 i = 2; i < 4; ++i)
			{
                f[2 * i] = bpmem.texscale[1].getScaleS(i & 1);
                f[2 * i + 1] = bpmem.texscale[1].getScaleT(i & 1);
                PRIM_LOG("tex indscale%d: %f %f\n", i, f[2 * i], f[2 * i + 1]);
            }
			SetPSConstant4fv(C_INDTEXSCALE+1, &f[4]);
        }
		s_nIndTexScaleChanged = 0;
    }

	if (s_nIndTexMtxChanged)
	{
		for (int i = 0; i < 3; ++i)
		{
            if (s_nIndTexMtxChanged & (1 << i))
			{
                int scale = ((u32)bpmem.indmtx[i].col0.s0 << 0) |
					        ((u32)bpmem.indmtx[i].col1.s1 << 2) |
					        ((u32)bpmem.indmtx[i].col2.s2 << 4);
                float fscale = powf(2.0f, (float)(scale - 17)) / 1024.0f;

                // xyz - static matrix
                // TODO w - dynamic matrix scale / 256...... somehow / 4 works better
                // rev 2972 - now using / 256.... verify that this works
				SetPSConstant4f(C_INDTEXMTX + 2 * i,
					bpmem.indmtx[i].col0.ma * fscale,
					bpmem.indmtx[i].col1.mc * fscale,
					bpmem.indmtx[i].col2.me * fscale,
					fscale * 4.0f);
				SetPSConstant4f(C_INDTEXMTX + 2 * i + 1,
					bpmem.indmtx[i].col0.mb * fscale,
					bpmem.indmtx[i].col1.md * fscale,
					bpmem.indmtx[i].col2.mf * fscale,
					fscale * 4.0f);

                PRIM_LOG("indmtx%d: scale=%f, mat=(%f %f %f; %f %f %f)\n",
                	i, 1024.0f*fscale,
                	bpmem.indmtx[i].col0.ma * fscale, bpmem.indmtx[i].col1.mc * fscale, bpmem.indmtx[i].col2.me * fscale,
                	bpmem.indmtx[i].col0.mb * fscale, bpmem.indmtx[i].col1.md * fscale, bpmem.indmtx[i].col2.mf * fscale);

				s_nIndTexMtxChanged &= ~(1 << i);
			}
        }
    }

	if (s_bFogRangeAdjustChanged)
	{
		if(!g_ActiveConfig.bDisableFog && bpmem.fogRange.Base.Enabled == 1)
		{
			//bpmem.fogRange.Base.Center : center of the viewport in x axis. observation: bpmem.fogRange.Base.Center = realcenter + 342;
			int center = ((u32)bpmem.fogRange.Base.Center) - 342;
			// normalize center to make calculations easy
			float ScreenSpaceCenter = center / (2.0f * xfregs.viewport.wd);
			ScreenSpaceCenter = (ScreenSpaceCenter * 2.0f) - 1.0f;
			//bpmem.fogRange.K seems to be  a table of precalculated coefficients for the adjust factor
			//observations: bpmem.fogRange.K[0].LO appears to be the lowest value and bpmem.fogRange.K[4].HI the largest
			// they always seems to be larger than 256 so my theory is :
			// they are the coefficients from the center to the border of the screen
			// so to simplify I use the hi coefficient as K in the shader taking 256 as the scale
			SetPSConstant4f(C_FOG + 2, ScreenSpaceCenter, (float)Renderer::EFBToScaledX((int)(2.0f * xfregs.viewport.wd)), bpmem.fogRange.K[4].HI / 256.0f,0.0f);
		}
		else
		{
			SetPSConstant4f(C_FOG + 2, 0.0f, 1.0f, 1.0f, 0.0f); // Need to update these values for older hardware that fails to divide by zero in shaders.
		}

		s_bFogRangeAdjustChanged = false;
	}

	if (g_ActiveConfig.bEnablePixelLighting && g_ActiveConfig.backend_info.bSupportsPixelLighting)  // config check added because the code in here was crashing for me inside SetPSConstant4f
	{
		if (nLightsChanged[0] >= 0)
		{
			// lights don't have a 1 to 1 mapping, the color component needs to be converted to 4 floats
			int istart = nLightsChanged[0] / 0x10;
			int iend = (nLightsChanged[1] + 15) / 0x10;
			const float* xfmemptr = (const float*)&xfmem[0x10 * istart + XFMEM_LIGHTS];

			for (int i = istart; i < iend; ++i)
			{
				u32 color = *(const u32*)(xfmemptr + 3);
				float NormalizationCoef = 1 / 255.0f;
				SetPSConstant4f(C_PLIGHTS + 5 * i,
					((color >> 24) & 0xFF) * NormalizationCoef,
					((color >> 16) & 0xFF) * NormalizationCoef,
					((color >> 8)  & 0xFF) * NormalizationCoef,
					((color)       & 0xFF) * NormalizationCoef);
				xfmemptr += 4;

				for (int j = 0; j < 4; ++j, xfmemptr += 3)
				{
					if (j == 1 &&
						fabs(xfmemptr[0]) < 0.00001f &&
						fabs(xfmemptr[1]) < 0.00001f &&
						fabs(xfmemptr[2]) < 0.00001f)
					{
						// dist attenuation, make sure not equal to 0!!!
						SetPSConstant4f(C_PLIGHTS+5*i+j+1, 0.00001f, xfmemptr[1], xfmemptr[2], 0);
					}
					else
					{
						SetPSConstant4fv(C_PLIGHTS+5*i+j+1, xfmemptr);
					}
				}
			}

			nLightsChanged[0] = nLightsChanged[1] = -1;
		}

		if (nMaterialsChanged)
		{
			float GC_ALIGNED16(material[4]);
			float NormalizationCoef = 1 / 255.0f;

			for (int i = 0; i < 2; ++i)
			{
				if (nMaterialsChanged & (1 << i))
				{
					u32 data = *(xfregs.ambColor + i);

					material[0] = ((data >> 24) & 0xFF) * NormalizationCoef;
					material[1] = ((data >> 16) & 0xFF) * NormalizationCoef;
					material[2] = ((data >>  8) & 0xFF) * NormalizationCoef;
					material[3] = ( data        & 0xFF) * NormalizationCoef;

					SetPSConstant4fv(C_PMATERIALS + i, material);
				}
			}

			for (int i = 0; i < 2; ++i)
			{
				if (nMaterialsChanged & (1 << (i + 2)))
				{
					u32 data = *(xfregs.matColor + i);

					material[0] = ((data >> 24) & 0xFF) * NormalizationCoef;
					material[1] = ((data >> 16) & 0xFF) * NormalizationCoef;
					material[2] = ((data >>  8) & 0xFF) * NormalizationCoef;
					material[3] = ( data        & 0xFF) * NormalizationCoef;

					SetPSConstant4fv(C_PMATERIALS + i + 2, material);
				}
			}

			nMaterialsChanged = 0;
		}
	}
}

// This one is high in profiles (0.5%).
// TODO: Move conversion out, only store the raw color value
// and update it when the shader constant is set, only.
// TODO: Conversion should be checked in the context of tev_fixes..
void PixelShaderManager::SetColorChanged(int type, int num, bool high)
{
	float4* c = type ? constants.kcolors : constants.colors;
	if (!high)
	{
		c[num][0] = bpmem.tevregs[num].low.a / 255.0f;
		c[num][3] = bpmem.tevregs[num].low.b / 255.0f;
	}
	else
	{
		c[num][2] = bpmem.tevregs[num].high.a / 255.0f;
		c[num][1] = bpmem.tevregs[num].high.b / 255.0f;
	}
	dirty = true;
	
	PRIM_LOG("pixel %scolor%d: %f %f %f %f\n", type?"k":"", num, c[num][0], c[num][1], c[num][2], c[num][3]);
}

void PixelShaderManager::SetAlpha(const AlphaTest& alpha)
{
	constants.alpha[0] = alpha.ref0 / 255.0f;
	constants.alpha[1] = alpha.ref1 / 255.0f;
	dirty = true;
}

void PixelShaderManager::SetDestAlpha(const ConstantAlpha& alpha)
{
	constants.alpha[3] = alpha.alpha;
	dirty = true;
}

void PixelShaderManager::SetTexDims(int texmapid, u32 width, u32 height, u32 wraps, u32 wrapt)
{
	// TODO: move this check out to callee. There we could just call this function on texture changes
	// or better, use textureSize() in glsl
	if(constants.texdims[texmapid][0] != 1.0f/width || constants.texdims[texmapid][1] != 1.0f/height)
		dirty = true;
	
	constants.texdims[texmapid][0] = 1.0f/width;
	constants.texdims[texmapid][1] = 1.0f/height;
}

void PixelShaderManager::SetZTextureBias(u32 bias)
{
	constants.zbias[1][3] = bias/16777215.0f;
	dirty = true;
}

void PixelShaderManager::SetViewportChanged()
{
	constants.zbias[1][0] = xfregs.viewport.farZ / 16777216.0f;
	constants.zbias[1][1] = xfregs.viewport.zRange / 16777216.0f;
	dirty = true;
	
	s_bFogRangeAdjustChanged = true; // TODO: Shouldn't be necessary with an accurate fog range adjust implementation
}

void PixelShaderManager::SetIndTexScaleChanged(u8 stagemask)
{
	s_nIndTexScaleChanged |= stagemask;
}

void PixelShaderManager::SetIndMatrixChanged(int matrixidx)
{
	s_nIndTexMtxChanged |= 1 << matrixidx;
}

void PixelShaderManager::SetZTextureTypeChanged()
{
	switch (bpmem.ztex2.type)
	{
		case TEV_ZTEX_TYPE_U8:
			constants.zbias[0][0] = 0;
			constants.zbias[0][1] = 0;
			constants.zbias[0][2] = 0;
			constants.zbias[0][3] = 255.0f/16777215.0f;
			break;
		case TEV_ZTEX_TYPE_U16:
			constants.zbias[0][0] = 255.0f/16777215.0f;
			constants.zbias[0][1] = 0;
			constants.zbias[0][2] = 0;
			constants.zbias[0][3] = 65280.0f/16777215.0f;
			break;
		case TEV_ZTEX_TYPE_U24:
			constants.zbias[0][0] = 16711680.0f/16777215.0f;
			constants.zbias[0][1] = 65280.0f/16777215.0f;
			constants.zbias[0][2] = 255.0f/16777215.0f;
			constants.zbias[0][3] = 0;
			break;
		default:
			break;
        }
        dirty = true;
}

void PixelShaderManager::SetTexCoordChanged(u8 texmapid)
{
	TCoordInfo& tc = bpmem.texcoords[texmapid];
        constants.texdims[texmapid][2] = tc.s.scale_minus_1 + 1;
	constants.texdims[texmapid][3] = tc.t.scale_minus_1 + 1;
	dirty = true;
}

void PixelShaderManager::SetFogColorChanged()
{
	constants.fog[0][0] = bpmem.fog.color.r / 255.0f;
	constants.fog[0][1] = bpmem.fog.color.g / 255.0f;
	constants.fog[0][2] = bpmem.fog.color.b / 255.0f;
	dirty = true;
}

void PixelShaderManager::SetFogParamChanged()
{
	if(!g_ActiveConfig.bDisableFog)
	{
		constants.fog[1][0] = bpmem.fog.a.GetA();
		constants.fog[1][1] = (float)bpmem.fog.b_magnitude / 0xFFFFFF;
		constants.fog[1][2] = bpmem.fog.c_proj_fsel.GetC();
		constants.fog[1][3] = 1 << bpmem.fog.b_shift;
	}
	else
	{
		constants.fog[1][0] = 0;
		constants.fog[1][1] = 1;
		constants.fog[1][2] = 0;
		constants.fog[1][3] = 1;
	}
	dirty = true;
}

void PixelShaderManager::SetFogRangeAdjustChanged()
{
	s_bFogRangeAdjustChanged = true;
}

void PixelShaderManager::InvalidateXFRange(int start, int end)
{
	if (start < XFMEM_LIGHTS_END && end > XFMEM_LIGHTS)
	{
		int _start = start < XFMEM_LIGHTS ? XFMEM_LIGHTS : start-XFMEM_LIGHTS;
		int _end = end < XFMEM_LIGHTS_END ? end-XFMEM_LIGHTS : XFMEM_LIGHTS_END-XFMEM_LIGHTS;

		if (nLightsChanged[0] == -1 )
		{
			nLightsChanged[0] = _start;
			nLightsChanged[1] = _end;
		}
		else
		{
			if (nLightsChanged[0] > _start) nLightsChanged[0] = _start;
			if (nLightsChanged[1] < _end)   nLightsChanged[1] = _end;
		}
	}
}

void PixelShaderManager::SetMaterialColorChanged(int index)
{
	nMaterialsChanged  |= (1 << index);
}

void PixelShaderManager::DoState(PointerWrap &p)
{
	p.Do(constants);
	p.Do(dirty);
	
	if (p.GetMode() == PointerWrap::MODE_READ)
	{
		Dirty();
	}
}
