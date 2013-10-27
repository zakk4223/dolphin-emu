// Copyright 2013 Dolphin Emulator Project
// Licensed under GPLv2
// Refer to the license.txt file included.

#include <stdio.h>
#include <cmath>
#include <assert.h>
#include <locale.h>
#ifdef __APPLE__
	#include <xlocale.h>
#endif

#include "LightingShaderGen.h"
#include "PixelShaderGen.h"
#include "XFMemory.h"  // for texture projection mode
#include "BPMemory.h"
#include "VideoConfig.h"
#include "NativeVertexFormat.h"


//   old tev->pixelshader notes
//
//   color for this stage (alpha, color) is given by bpmem.tevorders[0].colorchan0
//   konstant for this stage (alpha, color) is given by bpmem.tevksel
//   inputs are given by bpmem.combiners[0].colorC.a/b/c/d     << could be current channel color
//   according to GXTevColorArg table above
//   output is given by .outreg
//   tevtemp is set according to swapmodetables and

static const char *tevKSelTableC[] = // KCSEL
{
	"255,255,255",  // 1   = 0x00
	"223,223,223",  // 7_8 = 0x01
	"191,191,191",  // 3_4 = 0x02
	"159,159,159",  // 5_8 = 0x03
	"127,127,127",  // 1_2 = 0x04
	"95,95,95",     // 3_8 = 0x05
	"63,63,63",     // 1_4 = 0x06
	"31,31,31",     // 1_8 = 0x07
	"ERROR1", // 0x08
	"ERROR2", // 0x09
	"ERROR3", // 0x0a
	"ERROR4", // 0x0b
	"int3(" I_KCOLORS"[0].rgb * 255.0f)", // K0 = 0x0C
	"int3(" I_KCOLORS"[1].rgb * 255.0f)", // K1 = 0x0D
	"int3(" I_KCOLORS"[2].rgb * 255.0f)", // K2 = 0x0E
	"int3(" I_KCOLORS"[3].rgb * 255.0f)", // K3 = 0x0F
	"int3(" I_KCOLORS"[0].rrr * 255.0f)", // K0_R = 0x10
	"int3(" I_KCOLORS"[1].rrr * 255.0f)", // K1_R = 0x11
	"int3(" I_KCOLORS"[2].rrr * 255.0f)", // K2_R = 0x12
	"int3(" I_KCOLORS"[3].rrr * 255.0f)", // K3_R = 0x13
	"int3(" I_KCOLORS"[0].ggg * 255.0f)", // K0_G = 0x14
	"int3(" I_KCOLORS"[1].ggg * 255.0f)", // K1_G = 0x15
	"int3(" I_KCOLORS"[2].ggg * 255.0f)", // K2_G = 0x16
	"int3(" I_KCOLORS"[3].ggg * 255.0f)", // K3_G = 0x17
	"int3(" I_KCOLORS"[0].bbb * 255.0f)", // K0_B = 0x18
	"int3(" I_KCOLORS"[1].bbb * 255.0f)", // K1_B = 0x19
	"int3(" I_KCOLORS"[2].bbb * 255.0f)", // K2_B = 0x1A
	"int3(" I_KCOLORS"[3].bbb * 255.0f)", // K3_B = 0x1B
	"int3(" I_KCOLORS"[0].aaa * 255.0f)", // K0_A = 0x1C
	"int3(" I_KCOLORS"[1].aaa * 255.0f)", // K1_A = 0x1D
	"int3(" I_KCOLORS"[2].aaa * 255.0f)", // K2_A = 0x1E
	"int3(" I_KCOLORS"[3].aaa * 255.0f)", // K3_A = 0x1F
};

static const char *tevKSelTableA[] = // KASEL
{
	"255",  // 1   = 0x00
	"223",  // 7_8 = 0x01
	"191",  // 3_4 = 0x02
	"159",  // 5_8 = 0x03
	"127",  // 1_2 = 0x04
	"95",   // 3_8 = 0x05
	"63",   // 1_4 = 0x06
	"31",   // 1_8 = 0x07
	"ERROR5", // 0x08
	"ERROR6", // 0x09
	"ERROR7", // 0x0a
	"ERROR8", // 0x0b
	"ERROR9", // 0x0c
	"ERROR10", // 0x0d
	"ERROR11", // 0x0e
	"ERROR12", // 0x0f
	"int(" I_KCOLORS"[0].r * 255.0f)", // K0_R = 0x10
	"int(" I_KCOLORS"[1].r * 255.0f)", // K1_R = 0x11
	"int(" I_KCOLORS"[2].r * 255.0f)", // K2_R = 0x12
	"int(" I_KCOLORS"[3].r * 255.0f)", // K3_R = 0x13
	"int(" I_KCOLORS"[0].g * 255.0f)", // K0_G = 0x14
	"int(" I_KCOLORS"[1].g * 255.0f)", // K1_G = 0x15
	"int(" I_KCOLORS"[2].g * 255.0f)", // K2_G = 0x16
	"int(" I_KCOLORS"[3].g * 255.0f)", // K3_G = 0x17
	"int(" I_KCOLORS"[0].b * 255.0f)", // K0_B = 0x18
	"int(" I_KCOLORS"[1].b * 255.0f)", // K1_B = 0x19
	"int(" I_KCOLORS"[2].b * 255.0f)", // K2_B = 0x1A
	"int(" I_KCOLORS"[3].b * 255.0f)", // K3_B = 0x1B
	"int(" I_KCOLORS"[0].a * 255.0f)", // K0_A = 0x1C
	"int(" I_KCOLORS"[1].a * 255.0f)", // K1_A = 0x1D
	"int(" I_KCOLORS"[2].a * 255.0f)", // K2_A = 0x1E
	"int(" I_KCOLORS"[3].a * 255.0f)", // K3_A = 0x1F
};

static const char *tevScaleTable[] = // CS
{
	"*1",     // SCALE_1
	"*2",     // SCALE_2
	"*4",     // SCALE_4
	"/ 2",  // DIVIDE_2
};

static const char *tevBiasTable[] = // TB
{
	"",       // ZERO,
	"+ 128",  // ADDHALF,
	"- 128",  // SUBHALF,
	"",
};

static const char *tevOpTable[] = { // TEV
	"+",      // TEVOP_ADD = 0,
	"-",      // TEVOP_SUB = 1,
};

static const char *tevCInputTable[] = // CC
{
	"iprev.rgb",         // CPREV,
	"iprev.aaa",         // APREV,
	"ic0.rgb",           // C0,
	"ic0.aaa",           // A0,
	"ic1.rgb",           // C1,
	"ic1.aaa",           // A1,
	"ic2.rgb",           // C2,
	"ic2.aaa",           // A2,
	"itextemp.rgb",      // TEXC,
	"itextemp.aaa",      // TEXA,
	"irastemp.rgb",      // RASC,
	"irastemp.aaa",      // RASA,
	"int3(255,255,255)", // ONE
	"int3(127,127,127)", // HALF
	"ikonsttemp.rgb",    // KONST
	"int3(0,0,0)",       // ZERO
	///added extra values to map clamped values
	"icprev.rgb",        // CPREV,
	"icprev.aaa",        // APREV,
	"icc0.rgb",          // C0,
	"icc0.aaa",          // A0,
	"icc1.rgb",          // C1,
	"icc1.aaa",          // A1,
	"icc2.rgb",          // C2,
	"icc2.aaa",          // A2,
	"itextemp.rgb",      // TEXC,
	"itextemp.aaa",      // TEXA,
	"icrastemp.rgb",     // RASC,
	"icrastemp.aaa",     // RASA,
	"int3(255,255,255)", // ONE
	"int3(127,127,127)", // HALF
	"ickonsttemp.rgb",   // KONST
	"int3(0,0,0)",       // ZERO
	"PADERROR1", "PADERROR2", "PADERROR3", "PADERROR4"
};

static const char *tevAInputTable[] = // CA
{
	"iprev",            // APREV,
	"ic0",              // A0,
	"ic1",              // A1,
	"ic2",              // A2,
	"itextemp",         // TEXA,
	"irastemp",         // RASA,
	"ikonsttemp",       // KONST,  (hw1 had quarter)
	"int4(0,0,0,0)",    // ZERO
	///added extra values to map clamped values
	"icprev",            // APREV,
	"icc0",              // A0,
	"icc1",              // A1,
	"icc2",              // A2,
	"itextemp",          // TEXA,
	"icrastemp",         // RASA,
	"ickonsttemp",       // KONST,  (hw1 had quarter)
	"int4(0,0,0,0)",     // ZERO
	"PADERROR5", "PADERROR6", "PADERROR7", "PADERROR8",
	"PADERROR9", "PADERROR10", "PADERROR11", "PADERROR12",
};

static const char *tevRasTable[] =
{
	"int4(colors_0 * 255.0f)",
	"int4(colors_1 * 255.0f)",
	"ERROR13", //2
	"ERROR14", //3
	"ERROR15", //4
	"int4(alphabump, alphabump, alphabump, alphabump)", // use bump alpha
	"int4(alphabump | (alphabump >> 5), alphabump | (alphabump >> 5), alphabump | (alphabump >> 5), alphabump | (alphabump >> 5))", //normalized
	"int4(0, 0, 0, 0)", // zero
};

static const char *tevCOutputTable[]  = { "iprev.rgb", "ic0.rgb", "ic1.rgb", "ic2.rgb", "icprev.rgb", "icc0.rgb", "icc1.rgb", "icc2.rgb", };
static const char *tevAOutputTable[]  = { "iprev.a", "ic0.a", "ic1.a", "ic2.a", "icprev.a", "icc0.a", "icc1.a", "icc2.a" };

static char text[16384];

template<class T> static inline void WriteStage(T& out, pixel_shader_uid_data& uid_data, int n, API_TYPE ApiType, const char swapModeTable[4][5]);
template<class T> static inline void SampleTexture(T& out, const char *texcoords, const char *texswap, int texmap, API_TYPE ApiType);
template<class T> static inline void WriteAlphaTest(T& out, pixel_shader_uid_data& uid_data, API_TYPE ApiType,DSTALPHA_MODE dstAlphaMode, bool per_pixel_depth);
template<class T> static inline void WriteFog(T& out, pixel_shader_uid_data& uid_data);

template<class T>
static inline void GeneratePixelShader(T& out, DSTALPHA_MODE dstAlphaMode, API_TYPE ApiType, u32 components)
{
	// Non-uid template parameters will write to the dummy data (=> gets optimized out)
	pixel_shader_uid_data dummy_data;
	pixel_shader_uid_data& uid_data = (&out.template GetUidData<pixel_shader_uid_data>() != NULL)
										? out.template GetUidData<pixel_shader_uid_data>() : dummy_data;

	out.SetBuffer(text);
	const bool is_writing_shadercode = (out.GetBuffer() != NULL);
#ifndef ANDROID
	locale_t locale;
	locale_t old_locale;
	if (is_writing_shadercode)
	{
		locale = newlocale(LC_NUMERIC_MASK, "C", NULL); // New locale for compilation
		old_locale = uselocale(locale); // Apply the locale for this thread
	}
#endif

	if (is_writing_shadercode)
		text[sizeof(text) - 1] = 0x7C;  // canary

	unsigned int numStages = bpmem.genMode.numtevstages + 1;
	unsigned int numTexgen = bpmem.genMode.numtexgens;

	const bool forced_early_z = g_ActiveConfig.backend_info.bSupportsEarlyZ && bpmem.UseEarlyDepthTest() && (g_ActiveConfig.bFastDepthCalc || bpmem.alpha_test.TestResult() == AlphaTest::UNDETERMINED);
	const bool per_pixel_depth = (bpmem.ztex2.op != ZTEXTURE_DISABLE && bpmem.UseLateDepthTest()) || (!g_ActiveConfig.bFastDepthCalc && bpmem.zmode.testenable && !forced_early_z);

	out.Write("//Pixel Shader for TEV stages\n");
	out.Write("//%i TEV stages, %i texgens, %i IND stages\n",
		numStages, numTexgen, bpmem.genMode.numindstages);

	uid_data.dstAlphaMode = dstAlphaMode;
	uid_data.genMode_numindstages = bpmem.genMode.numindstages;
	uid_data.genMode_numtevstages = bpmem.genMode.numtevstages;
	uid_data.genMode_numtexgens = bpmem.genMode.numtexgens;

	// dot product for integer vectors
	out.Write("int idot(int3 x, int3 y)\n");
	out.Write("{\n");
	out.Write("\tint3 tmp = x * y;\n");
	out.Write("\treturn tmp.x + tmp.y + tmp.z;\n");
	out.Write("}\n");

	if (ApiType == API_OPENGL)
	{
		// Fmod implementation gleaned from Nvidia
		// At http://http.developer.nvidia.com/Cg/fmod.html
		out.Write("float fmod( float x, float y )\n");
		out.Write("{\n");
		out.Write("\tfloat z = fract( abs( x / y) ) * abs( y );\n");
		out.Write("\treturn (x < 0.0) ? -z : z;\n");
		out.Write("}\n");

		// Declare samplers
		for (int i = 0; i < 8; ++i)
			out.Write("uniform sampler2D samp%d;\n", i);
	}
	else // D3D
	{
		// Declare samplers
		for (int i = 0; i < 8; ++i)
			out.Write("sampler samp%d : register(s%d);\n", i, i);

		out.Write("\n");
		for (int i = 0; i < 8; ++i)
			out.Write("Texture2D Tex%d : register(t%d);\n", i, i);
	}
	out.Write("\n");

	if (g_ActiveConfig.backend_info.bSupportsGLSLUBO)
		out.Write("layout(std140) uniform PSBlock {\n");

	DeclareUniform(out, ApiType, g_ActiveConfig.backend_info.bSupportsGLSLUBO, C_COLORS, "float4", I_COLORS"[4]");
	DeclareUniform(out, ApiType, g_ActiveConfig.backend_info.bSupportsGLSLUBO, C_KCOLORS, "float4", I_KCOLORS"[4]");
	DeclareUniform(out, ApiType, g_ActiveConfig.backend_info.bSupportsGLSLUBO, C_ALPHA, "float4", I_ALPHA"[1]");  // TODO: Why is this an array...-.-
	DeclareUniform(out, ApiType, g_ActiveConfig.backend_info.bSupportsGLSLUBO, C_TEXDIMS, "float4", I_TEXDIMS"[8]");
	DeclareUniform(out, ApiType, g_ActiveConfig.backend_info.bSupportsGLSLUBO, C_ZBIAS, "float4", I_ZBIAS"[2]");
	DeclareUniform(out, ApiType, g_ActiveConfig.backend_info.bSupportsGLSLUBO, C_INDTEXSCALE, "float4", I_INDTEXSCALE"[2]");
	DeclareUniform(out, ApiType, g_ActiveConfig.backend_info.bSupportsGLSLUBO, C_INDTEXMTX, "float4", I_INDTEXMTX"[6]");
	DeclareUniform(out, ApiType, g_ActiveConfig.backend_info.bSupportsGLSLUBO, C_FOG, "float4", I_FOG"[3]");

	// For pixel lighting - TODO: Should only be defined when per pixel lighting is enabled!
	DeclareUniform(out, ApiType, g_ActiveConfig.backend_info.bSupportsGLSLUBO, C_PLIGHTS, "float4", I_PLIGHTS"[40]");
	DeclareUniform(out, ApiType, g_ActiveConfig.backend_info.bSupportsGLSLUBO, C_PMATERIALS, "float4", I_PMATERIALS"[4]");

	if (g_ActiveConfig.backend_info.bSupportsGLSLUBO)
		out.Write("};\n");

	if (ApiType == API_OPENGL)
	{
		out.Write("COLOROUT(ocol0)\n");
		if (dstAlphaMode == DSTALPHA_DUAL_SOURCE_BLEND)
			out.Write("out vec4 ocol1;\n");

		if (per_pixel_depth)
			out.Write("#define depth gl_FragDepth\n");

		out.Write("VARYIN float4 colors_02;\n");
		out.Write("VARYIN float4 colors_12;\n");

		// compute window position if needed because binding semantic WPOS is not widely supported
		// Let's set up attributes
		if (xfregs.numTexGen.numTexGens < 7)
		{
			for (int i = 0; i < 8; ++i)
				if(i < xfregs.numTexGen.numTexGens)
					out.Write("VARYIN float3 uv%d_2;\n", i);
			out.Write("VARYIN float4 clipPos_2;\n");
			if (g_ActiveConfig.bEnablePixelLighting && g_ActiveConfig.backend_info.bSupportsPixelLighting)
			{
				out.Write("VARYIN float4 Normal_2;\n");
			}
		}
		else
		{
			// wpos is in w of first 4 texcoords
			if (g_ActiveConfig.bEnablePixelLighting && g_ActiveConfig.backend_info.bSupportsPixelLighting)
			{
				for (int i = 0; i < 8; ++i)
				{
					out.Write("VARYIN float4 uv%d_2;\n", i);
				}
			}
			else
			{
				for (unsigned int i = 0; i < xfregs.numTexGen.numTexGens; ++i)
				{
					out.Write("VARYIN float%d uv%d_2;\n", i < 4 ? 4 : 3 , i);
				}
			}
			out.Write("float4 clipPos;\n");
		}

		if (forced_early_z)
		{
			// HACK: This doesn't force the driver to write to depth buffer if alpha test fails.
			// It just allows it, but it seems that all drivers do.
			out.Write("layout(early_fragment_tests) in;\n");
		}
		else if (bpmem.UseEarlyDepthTest() && (g_ActiveConfig.bFastDepthCalc || bpmem.alpha_test.TestResult() == AlphaTest::UNDETERMINED) && is_writing_shadercode)
		{
			static bool warn_once = true;
			if (warn_once)
				WARN_LOG(VIDEO, "Early z test enabled but not possible to emulate with current configuration. Make sure to enable fast depth calculations. If this message still shows up your hardware isn't able to emulate the feature properly (a GPU with D3D 11.0 / OGL 4.2 support is required).");
			warn_once = false;
		}

		out.Write("void main()\n{\n");
	}
	else // D3D
	{
		if (forced_early_z)
		{
			out.Write("[earlydepthstencil]\n");
		}
		else if (bpmem.UseEarlyDepthTest() && (g_ActiveConfig.bFastDepthCalc || bpmem.alpha_test.TestResult() == AlphaTest::UNDETERMINED) && is_writing_shadercode)
		{
			static bool warn_once = true;
			if (warn_once)
				WARN_LOG(VIDEO, "Early z test enabled but not possible to emulate with current configuration. Make sure to enable fast depth calculations. If this message still shows up your hardware isn't able to emulate the feature properly (a GPU with D3D 11.0 / OGL 4.2 support is required).");
			warn_once = false;
		}

		out.Write("void main(\n");
		out.Write("  out float4 ocol0 : SV_Target0,%s%s\n  in float4 rawpos : SV_Position,\n",
			dstAlphaMode == DSTALPHA_DUAL_SOURCE_BLEND ? "\n  out float4 ocol1 : SV_Target1," : "",
			per_pixel_depth ? "\n  out float depth : SV_Depth," : "");

		// Use centroid sampling to make MSAA work properly
		const char* optCentroid = "centroid";

		out.Write("  in %s float4 colors_0 : COLOR0,\n", optCentroid);
		out.Write("  in %s float4 colors_1 : COLOR1", optCentroid);

		// compute window position if needed because binding semantic WPOS is not widely supported
		if (numTexgen < 7)
		{
			for (unsigned int i = 0; i < numTexgen; ++i)
				out.Write(",\n  in %s float3 uv%d : TEXCOORD%d", optCentroid, i, i);
			out.Write(",\n  in %s float4 clipPos : TEXCOORD%d", optCentroid, numTexgen);
			if(g_ActiveConfig.bEnablePixelLighting && g_ActiveConfig.backend_info.bSupportsPixelLighting)
				out.Write(",\n  in %s float4 Normal : TEXCOORD%d", optCentroid, numTexgen + 1);
			out.Write("        ) {\n");
		}
		else
		{
			// wpos is in w of first 4 texcoords
			if(g_ActiveConfig.bEnablePixelLighting && g_ActiveConfig.backend_info.bSupportsPixelLighting)
			{
				for (int i = 0; i < 8; ++i)
					out.Write(",\n  in float4 uv%d : TEXCOORD%d", i, i);
			}
			else
			{
				for (unsigned int i = 0; i < xfregs.numTexGen.numTexGens; ++i)
					out.Write(",\n  in float%d uv%d : TEXCOORD%d", i < 4 ? 4 : 3 , i, i);
			}
			out.Write("        ) {\n");
			out.Write("\tfloat4 clipPos = float4(0.0, 0.0, 0.0, 0.0);");
		}
	}

	out.Write("  int4 ic0 = int4(" I_COLORS"[1] * 255.0), ic1 = int4(" I_COLORS"[2] * 255.0), ic2 = int4(" I_COLORS"[3] * 255.0), iprev = int4(" I_COLORS"[0] * 255.0);\n"
			"  int4 irastemp = int4(0, 0, 0, 0), itextemp = int4(0, 0, 0, 0), ikonsttemp = int4(0, 0, 0, 0);\n"
			"  int3 comp16 = int3(1, 256, 0), comp24 = int3(1, 256, 256*256);\n"
			"  int alphabump=0;\n"
			"  int3 tevcoord=int3(0, 0, 0);\n"
			"  int2 wrappedcoord=int2(0,0); float2 tempcoord=float2(0.0,0.0);\n"
			"  int4 icc0=int4(0, 0, 0, 0), icc1=int4(0, 0, 0, 0);\n"
			"  int4 icc2=int4(0, 0, 0, 0), icprev=int4(0, 0, 0, 0);\n"
			"  int4 icrastemp = int4(0, 0, 0, 0), ickonsttemp = int4(0, 0, 0, 0);\n\n");

	if (ApiType == API_OPENGL)
	{
		// On Mali, global variables must be initialized as constants.
		// This is why we initialize these variables locally instead.
		out.Write("float4 rawpos = gl_FragCoord;\n");
		out.Write("float4 colors_0 = colors_02;\n");
		out.Write("float4 colors_1 = colors_12;\n");
		// compute window position if needed because binding semantic WPOS is not widely supported
		// Let's set up attributes
		if (xfregs.numTexGen.numTexGens < 7)
		{
			if(numTexgen)
				for (int i = 0; i < 8; ++i)
					if(i < xfregs.numTexGen.numTexGens)
						out.Write("float3 uv%d = uv%d_2;\n", i, i);
			out.Write("float4 clipPos = clipPos_2;\n");
			if (g_ActiveConfig.bEnablePixelLighting && g_ActiveConfig.backend_info.bSupportsPixelLighting)
			{
				out.Write("float4 Normal = Normal_2;\n");
			}
		}
		else
		{
			// wpos is in w of first 4 texcoords
			if (g_ActiveConfig.bEnablePixelLighting && g_ActiveConfig.backend_info.bSupportsPixelLighting)
			{
				for (int i = 0; i < 8; ++i)
				{
					out.Write("float4 uv%d = uv%d_2;\n", i, i);
				}
			}
			else
			{
				for (unsigned int i = 0; i < xfregs.numTexGen.numTexGens; ++i)
				{
					out.Write("float%d uv%d = uv%d_2;\n", i < 4 ? 4 : 3 , i, i);
				}
			}
		}
	}

	if (g_ActiveConfig.bEnablePixelLighting && g_ActiveConfig.backend_info.bSupportsPixelLighting)
	{
		if (xfregs.numTexGen.numTexGens < 7)
		{
			out.Write("\tfloat3 _norm0 = normalize(Normal.xyz);\n\n");
			out.Write("\tfloat3 pos = float3(clipPos.x,clipPos.y,Normal.w);\n");
		}
		else
		{
			out.Write("\tfloat3 _norm0 = normalize(float3(uv4.w,uv5.w,uv6.w));\n\n");
			out.Write("\tfloat3 pos = float3(uv0.w,uv1.w,uv7.w);\n");
		}

		out.Write("\tfloat4 mat, lacc;\n"
				"\tfloat3 ldir, h;\n"
				"\tfloat dist, dist2, attn;\n");

		out.SetConstantsUsed(C_PLIGHTS, C_PLIGHTS+39); // TODO: Can be optimized further
		out.SetConstantsUsed(C_PMATERIALS, C_PMATERIALS+3);
		uid_data.components = components;
		GenerateLightingShader<T>(out, uid_data.lighting, components, I_PMATERIALS, I_PLIGHTS, "colors_", "colors_");
	}

	if (numTexgen < 7)
		out.Write("\tclipPos = float4(rawpos.x, rawpos.y, clipPos.z, clipPos.w);\n");
	else
		out.Write("\tclipPos = float4(rawpos.x, rawpos.y, uv2.w, uv3.w);\n");

	// HACK to handle cases where the tex gen is not enabled
	if (numTexgen == 0)
	{
		out.Write("\tfloat3 uv0 = float3(0.0, 0.0, 0.0);\n");
	}
	else
	{
		out.SetConstantsUsed(C_TEXDIMS, C_TEXDIMS+numTexgen-1);
		for (unsigned int i = 0; i < numTexgen; ++i)
		{
			// optional perspective divides
			uid_data.texMtxInfo_n_projection |= xfregs.texMtxInfo[i].projection << i;
			if (xfregs.texMtxInfo[i].projection == XF_TEXPROJ_STQ)
			{
				out.Write("\tif (uv%d.z != 0.0)", i);
				out.Write("\t\tuv%d.xy = uv%d.xy / uv%d.z;\n", i, i, i);
			}

			out.Write("uv%d.xy = uv%d.xy * " I_TEXDIMS"[%d].zw;\n", i, i, i);
		}
	}

	// indirect texture map lookup
	int nIndirectStagesUsed = 0;
	if (bpmem.genMode.numindstages > 0)
	{
		for (unsigned int i = 0; i < numStages; ++i)
		{
			if (bpmem.tevind[i].IsActive() && bpmem.tevind[i].bt < bpmem.genMode.numindstages)
				nIndirectStagesUsed |= 1 << bpmem.tevind[i].bt;
		}
	}

	uid_data.nIndirectStagesUsed = nIndirectStagesUsed;
	for (u32 i = 0; i < bpmem.genMode.numindstages; ++i)
	{
		if (nIndirectStagesUsed & (1 << i))
		{
			unsigned int texcoord = bpmem.tevindref.getTexCoord(i);
			unsigned int texmap = bpmem.tevindref.getTexMap(i);

			uid_data.SetTevindrefValues(i, texcoord, texmap);
			if (texcoord < numTexgen)
			{
				out.SetConstantsUsed(C_INDTEXSCALE+i/2,C_INDTEXSCALE+i/2);
				out.Write("\ttempcoord = uv%d.xy * " I_INDTEXSCALE"[%d].%s;\n", texcoord, i/2, (i&1)?"zw":"xy");
			}
			else
				out.Write("\ttempcoord = float2(0.0, 0.0);\n");

			out.Write("\tint3 iindtex%d = ", i);
			SampleTexture<T>(out, "tempcoord", "abg", texmap, ApiType);
		}
	}

	// Uid fields for BuildSwapModeTable are set in WriteStage
	char swapModeTable[4][5];
	const char* swapColors = "rgba";
	for (int i = 0; i < 4; i++)
	{
		swapModeTable[i][0] = swapColors[bpmem.tevksel[i*2].swap1];
		swapModeTable[i][1] = swapColors[bpmem.tevksel[i*2].swap2];
		swapModeTable[i][2] = swapColors[bpmem.tevksel[i*2+1].swap1];
		swapModeTable[i][3] = swapColors[bpmem.tevksel[i*2+1].swap2];
		swapModeTable[i][4] = '\0';
	}

	for (unsigned int i = 0; i < numStages; i++)
		WriteStage<T>(out, uid_data, i, ApiType, swapModeTable); // build the equation for this stage

#define MY_STRUCT_OFFSET(str,elem) ((u32)((u64)&(str).elem-(u64)&(str)))
	bool enable_pl = g_ActiveConfig.bEnablePixelLighting && g_ActiveConfig.backend_info.bSupportsPixelLighting;
	uid_data.num_values = (enable_pl) ? sizeof(uid_data) : MY_STRUCT_OFFSET(uid_data,stagehash[numStages]);


	if (numStages)
	{
		// The results of the last texenv stage are put onto the screen,
		// regardless of the used destination register
		if(bpmem.combiners[numStages - 1].colorC.dest != 0)
		{
			out.Write("\tiprev.rgb = %s;\n", tevCOutputTable[bpmem.combiners[numStages - 1].colorC.dest]);
		}
		if(bpmem.combiners[numStages - 1].alphaC.dest != 0)
		{
			out.Write("\tiprev.a = %s;\n", tevAOutputTable[bpmem.combiners[numStages - 1].alphaC.dest]);
		}
	}
	out.Write("\tiprev = iprev & 255;\n");

	AlphaTest::TEST_RESULT Pretest = bpmem.alpha_test.TestResult();
	uid_data.Pretest = Pretest;

	// NOTE: Fragment may not be discarded if alpha test always fails and early depth test is enabled 
	// (in this case we need to write a depth value if depth test passes regardless of the alpha testing result)
	if (Pretest == AlphaTest::UNDETERMINED || (Pretest == AlphaTest::FAIL && bpmem.UseLateDepthTest()))
		WriteAlphaTest<T>(out, uid_data, ApiType, dstAlphaMode, per_pixel_depth);


	// TODO: Make more sense out of this comment
	// D3D9 doesn't support readback of depth in pixel shader, so we always have to calculate it again.
	// This shouldn't be a performance issue as the written depth is usually still from perspective division
	// but this isn't true for z-textures, so there will be depth issues between enabled and disabled z-textures fragments
	if (g_ActiveConfig.bFastDepthCalc)
		out.Write("float zCoord = rawpos.z;\n");
	else
	{
		out.SetConstantsUsed(C_ZBIAS+1, C_ZBIAS+1);
		// the screen space depth value = far z + (clip z / clip w) * z range
		out.Write("float zCoord = " I_ZBIAS"[1].x + (clipPos.z / clipPos.w) * " I_ZBIAS"[1].y;\n");
	}

	// depth texture can safely be ignored if the result won't be written to the depth buffer (early_ztest) and isn't used for fog either
	const bool skip_ztexture = !per_pixel_depth && !bpmem.fog.c_proj_fsel.fsel;

	uid_data.ztex_op = bpmem.ztex2.op;
	uid_data.per_pixel_depth = per_pixel_depth;
	uid_data.forced_early_z = forced_early_z;
	uid_data.fast_depth_calc = g_ActiveConfig.bFastDepthCalc;
	uid_data.early_ztest = bpmem.UseEarlyDepthTest();
	uid_data.fog_fsel = bpmem.fog.c_proj_fsel.fsel;

	// Note: z-textures are not written to depth buffer if early depth test is used
	if (per_pixel_depth && bpmem.UseEarlyDepthTest())
		out.Write("depth = zCoord;\n");

	// Note: depth texture output is only written to depth buffer if late depth test is used
	// theoretical final depth value is used for fog calculation, though, so we have to emulate ztextures anyway
	if (bpmem.ztex2.op != ZTEXTURE_DISABLE && !skip_ztexture)
	{
		// use the texture input of the last texture stage (itextemp), hopefully this has been read and is in correct format...
		out.SetConstantsUsed(C_ZBIAS, C_ZBIAS+1);
		out.Write("zCoord = dot(" I_ZBIAS"[0].xyzw, float4(itextemp.xyzw)/255.0f) + " I_ZBIAS"[1].w %s;\n",
									(bpmem.ztex2.op == ZTEXTURE_ADD) ? "+ zCoord" : "");

		// U24 overflow emulation
		out.Write("zCoord = zCoord * (16777215.0/16777216.0);\n");
		out.Write("zCoord = frac(zCoord);\n");
		out.Write("zCoord = zCoord * (16777216.0/16777215.0);\n");
	}

	if (per_pixel_depth && bpmem.UseLateDepthTest())
		out.Write("depth = zCoord;\n");

	if (dstAlphaMode == DSTALPHA_ALPHA_PASS)
	{
		out.SetConstantsUsed(C_ALPHA, C_ALPHA);
		out.Write("\tocol0 = float4(float3(iprev.rgb) / 255.0f, " I_ALPHA"[0].a);\n");
	}
	else
	{
		WriteFog<T>(out, uid_data);
		out.Write("\tocol0 = float4(iprev) / 255.0f;\n");
	}

	// Use dual-source color blending to perform dst alpha in a single pass
	if (dstAlphaMode == DSTALPHA_DUAL_SOURCE_BLEND)
	{
		out.SetConstantsUsed(C_ALPHA, C_ALPHA);

		// Colors will be blended against the alpha from ocol1 and
		// the alpha from ocol0 will be written to the framebuffer.
		out.Write("\tocol1 = float4(iprev) / 255.0f;\n");
		out.Write("\tocol0.a = " I_ALPHA"[0].a;\n");
	}

	out.Write("}\n");

	if (is_writing_shadercode)
	{
		if (text[sizeof(text) - 1] != 0x7C)
			PanicAlert("PixelShader generator - buffer too small, canary has been eaten!");

#ifndef ANDROID
		uselocale(old_locale); // restore locale
		freelocale(locale);
#endif
	}
}



//table with the color compare operations
static const char *TEVCMPColorOPTable[16] =
{
	"float3(0.0, 0.0, 0.0)",//0
	"float3(0.0, 0.0, 0.0)",//1
	"float3(0.0, 0.0, 0.0)",//2
	"float3(0.0, 0.0, 0.0)",//3
	"float3(0.0, 0.0, 0.0)",//4
	"float3(0.0, 0.0, 0.0)",//5
	"float3(0.0, 0.0, 0.0)",//6
	"float3(0.0, 0.0, 0.0)",//7
	"   (%s&255) + (((%s.r&255) > %s.r) ? (%s&255): int3(0,0,0))",//#define TEVCMP_R8_GT 8
	"   (%s&255) + (((%s.r&255) == %s.r) ? (%s&255): int3(0,0,0))",//#define TEVCMP_R8_EQ 9
	"   (%s&255) + ((idot((%s.rgb&255), comp16) >  idot((%s.rgb&255), comp16)) ? (%s&255): int3(0,0,0))",//#define TEVCMP_GR16_GT 10
	"   (%s&255) + ((idot((%s.rgb&255), comp16) == idot((%s.rgb&255), comp16)) ? (%s&255): int3(0,0,0))",//#define TEVCMP_GR16_EQ 11
	"   (%s&255) + ((idot((%s.rgb&255), comp24) >  idot((%s.rgb&255), comp24)) ? (%s&255): int3(0,0,0))",//#define TEVCMP_BGR24_GT 12
	"   (%s&255) + ((idot((%s.rgb&255), comp24) == idot((%s.rgb&255), comp24)) ? (%s&255): int3(0,0,0))",//#define TEVCMP_BGR24_EQ 13
	"   (%s&255) + int3(max(sign(int3((%s.rgb&255)) - int3((%s.rgb&255))), int3(0,0,0)) * (%s&255))",//#define TEVCMP_RGB8_GT  14
	"   (%s&255) + int3((int3(255,255,255) - max(sign(abs(int3((%s.rgb&255)) - int3((%s.rgb&255)))), int3(0,0,0))) * (%s&255))"//#define TEVCMP_RGB8_EQ  15
};

//table with the alpha compare operations
static const char *TEVCMPAlphaOPTable[16] =
{
	"0.0",//0
	"0.0",//1
	"0.0",//2
	"0.0",//3
	"0.0",//4
	"0.0",//5
	"0.0",//6
	"0.0",//7
	"   (%s.a&255) + (((%s.r&255) > (%s.r&255)) ? (%s.a&255) : 0)",//#define TEVCMP_R8_GT 8
	"   (%s.a&255) + (((%s.r&255) == (%s.r&255)) ? (%s.a&255) : 0)",//#define TEVCMP_R8_EQ 9
	"   (%s.a&255) + ((idot((%s.rgb&255), comp16) >  idot((%s.rgb&255), comp16)) ? (%s.a&255) : 0)",//#define TEVCMP_GR16_GT 10
	"   (%s.a&255) + ((idot((%s.rgb&255), comp16) == idot((%s.rgb&255), comp16)) ? (%s.a&255) : 0)",//#define TEVCMP_GR16_EQ 11
	"   (%s.a&255) + ((idot((%s.rgb&255), comp24) >  idot((%s.rgb&255), comp24)) ? (%s.a&255) : 0)",//#define TEVCMP_BGR24_GT 12
	"   (%s.a&255) + ((idot((%s.rgb&255), comp24) == idot((%s.rgb&255), comp24)) ? (%s.a&255) : 0)",//#define TEVCMP_BGR24_EQ 13
	"   (%s.a&255) + (((%s.a&255) >  (%s.a&255)) ? (%s.a&255) : 0)",//#define TEVCMP_A8_GT 14
	"   (%s.a&255) + (((%s.a&255) == (%s.a&255)) ? (%s.a&255) : 0)" //#define TEVCMP_A8_EQ 15
};

template<class T>
static inline void WriteStage(T& out, pixel_shader_uid_data& uid_data, int n, API_TYPE ApiType, const char swapModeTable[4][5])
{
	int texcoord = bpmem.tevorders[n/2].getTexCoord(n&1);
	bool bHasTexCoord = (u32)texcoord < bpmem.genMode.numtexgens;
	bool bHasIndStage = bpmem.tevind[n].IsActive() && bpmem.tevind[n].bt < bpmem.genMode.numindstages;
	// HACK to handle cases where the tex gen is not enabled
	if (!bHasTexCoord)
		texcoord = 0;

	out.Write("// TEV stage %d\n", n);

	uid_data.stagehash[n].hasindstage = bHasIndStage;
	uid_data.stagehash[n].tevorders_texcoord = texcoord;
	if (bHasIndStage)
	{
		uid_data.stagehash[n].tevind = bpmem.tevind[n].hex & 0x7FFFFF;

		out.Write("// indirect op\n");
		// perform the indirect op on the incoming regular coordinates using iindtex%d as the offset coords
		if (bpmem.tevind[n].bs != ITBA_OFF)
		{
			const char *tevIndAlphaSel[]   = {"", "x", "y", "z"};
			const char *tevIndAlphaMask[] = {"248", "224", "240", "248"};
			out.Write("alphabump = iindtex%d.%s & %s;\n",
					bpmem.tevind[n].bt,
					tevIndAlphaSel[bpmem.tevind[n].bs],
					tevIndAlphaMask[bpmem.tevind[n].fmt]);
		}
		else
		{
			// TODO: Should we reset alphabump to 0 here?
		}

		// format
		const char *tevIndFmtMask[]   = {"255", "31", "15", "7" };
		out.Write("int3 iindtevcrd%d = iindtex%d & %s;\n", n, bpmem.tevind[n].bt, tevIndFmtMask[bpmem.tevind[n].fmt]);

		// bias - TODO: Check if this needs to be this complicated..
		const char *tevIndBiasField[]  = {"", "x", "y", "xy", "z", "xz", "yz", "xyz"}; // indexed by bias
		const char *tevIndBiasAdd[]    = {"-128", "1", "1", "1" }; // indexed by fmt
		if (bpmem.tevind[n].bias == ITB_S || bpmem.tevind[n].bias == ITB_T || bpmem.tevind[n].bias == ITB_U)
			out.Write("iindtevcrd%d.%s += int(%s);\n", n, tevIndBiasField[bpmem.tevind[n].bias], tevIndBiasAdd[bpmem.tevind[n].fmt]);
		else if (bpmem.tevind[n].bias == ITB_ST || bpmem.tevind[n].bias == ITB_SU || bpmem.tevind[n].bias == ITB_TU)
			out.Write("iindtevcrd%d.%s += int2(%s, %s);\n", n, tevIndBiasField[bpmem.tevind[n].bias], tevIndBiasAdd[bpmem.tevind[n].fmt], tevIndBiasAdd[bpmem.tevind[n].fmt]);
		else if (bpmem.tevind[n].bias == ITB_STU)
			out.Write("iindtevcrd%d.%s += int3(%s, %s, %s);\n", n, tevIndBiasField[bpmem.tevind[n].bias], tevIndBiasAdd[bpmem.tevind[n].fmt], tevIndBiasAdd[bpmem.tevind[n].fmt], tevIndBiasAdd[bpmem.tevind[n].fmt]);

		// multiply by offset matrix and scale - calculations are likely to overflow badly,
		// yet it works out since we only care about the lower 31 bits (+1 sign bit) of the result
		if (bpmem.tevind[n].mid != 0)
		{
			if (bpmem.tevind[n].mid <= 3)
			{
				int mtxidx = 2*(bpmem.tevind[n].mid-1);
				out.SetConstantsUsed(C_INDTEXMTX+mtxidx, C_INDTEXMTX+mtxidx);

				out.Write("int2 indtevtrans%d = int2(dot(" I_INDTEXMTX"[%d].xyz, float3(iindtevcrd%d)), dot(" I_INDTEXMTX"[%d].xyz, float3(iindtevcrd%d)));\n",
							n, mtxidx, n, mtxidx+1, n);
			}
			else if (bpmem.tevind[n].mid <= 7 && bHasTexCoord)
			{ // s matrix
				_assert_(bpmem.tevind[n].mid >= 5);
				int mtxidx = 2*(bpmem.tevind[n].mid-5);
				out.SetConstantsUsed(C_INDTEXMTX+mtxidx, C_INDTEXMTX+mtxidx);
				out.Write("int2 indtevtrans%d = int2(" I_INDTEXMTX"[%d].ww * uv%d.xy * float3(iindtevcrd%d.xx));\n", n, mtxidx, texcoord, n);
			}
			else if (bpmem.tevind[n].mid <= 11 && bHasTexCoord)
			{ // t matrix
				_assert_(bpmem.tevind[n].mid >= 9);
				int mtxidx = 2*(bpmem.tevind[n].mid-9);
				out.SetConstantsUsed(C_INDTEXMTX+mtxidx, C_INDTEXMTX+mtxidx);
				out.Write("int2 indtevtrans%d = int2(" I_INDTEXMTX"[%d].ww * uv%d.xy * float3(iindtevcrd%d.yy));\n", n, mtxidx, texcoord, n);
			}
			else
			{
				out.Write("int2 indtevtrans%d = int2(0, 0);\n", n);
			}
		}
		else
		{
			out.Write("int2 indtevtrans%d = int2(0, 0);\n", n);
		}

		// ---------
		// Wrapping
		// ---------
		const char *tevIndWrapStart[]  = {"0", "(256<<7)", "(128<<7)", "(64<<7)", "(32<<7)", "(16<<7)", "1" };

		// wrap S
		if (bpmem.tevind[n].sw == ITW_OFF)
			out.Write("wrappedcoord.x = int(uv%d.x*256.0);\n", texcoord);
		else if (bpmem.tevind[n].sw == ITW_0)
			out.Write("wrappedcoord.x = 0;\n");
		else
			out.Write("wrappedcoord.x = int(uv%d.x*256.0) %% %s;\n", texcoord, tevIndWrapStart[bpmem.tevind[n].sw]);

		// wrap T
		if (bpmem.tevind[n].tw == ITW_OFF)
			out.Write("wrappedcoord.y = int(uv%d.y*256.0);\n", texcoord);
		else if (bpmem.tevind[n].tw == ITW_0)
			out.Write("wrappedcoord.y = 0;\n");
		else
			out.Write("wrappedcoord.y = int(uv%d.y*256.0) %% %s;\n", texcoord, tevIndWrapStart[bpmem.tevind[n].tw]);

		// Casting uint to int preserves bit pattern
		// comex says this works, I didn't find anything wrong about it...
		if (bpmem.tevind[n].fb_addprev) // add previous tevcoord
			out.Write("tevcoord.xy += wrappedcoord + indtevtrans%d;\n", n);
		else
			out.Write("tevcoord.xy = wrappedcoord + indtevtrans%d;\n", n);

		// Emulate s24 overflows
		out.Write("tevcoord.xy = (tevcoord.xy << 8) >> 8;\n");
	}

	TevStageCombiner::ColorCombiner &cc = bpmem.combiners[n].colorC;
	TevStageCombiner::AlphaCombiner &ac = bpmem.combiners[n].alphaC;

	uid_data.stagehash[n].cc = cc.hex & 0xFFFFFF;
	uid_data.stagehash[n].ac = ac.hex & 0xFFFFF0; // Storing rswap and tswap later

	if(cc.a == TEVCOLORARG_RASA || cc.a == TEVCOLORARG_RASC
		|| cc.b == TEVCOLORARG_RASA || cc.b == TEVCOLORARG_RASC
		|| cc.c == TEVCOLORARG_RASA || cc.c == TEVCOLORARG_RASC
		|| cc.d == TEVCOLORARG_RASA || cc.d == TEVCOLORARG_RASC
		|| ac.a == TEVALPHAARG_RASA || ac.b == TEVALPHAARG_RASA
		|| ac.c == TEVALPHAARG_RASA || ac.d == TEVALPHAARG_RASA)
	{
		const int i = bpmem.combiners[n].alphaC.rswap;
		uid_data.stagehash[n].ac |= bpmem.combiners[n].alphaC.rswap;
		uid_data.stagehash[n].tevksel_swap1a = bpmem.tevksel[i*2].swap1;
		uid_data.stagehash[n].tevksel_swap2a = bpmem.tevksel[i*2].swap2;
		uid_data.stagehash[n].tevksel_swap1b = bpmem.tevksel[i*2+1].swap1;
		uid_data.stagehash[n].tevksel_swap2b = bpmem.tevksel[i*2+1].swap2;
		uid_data.stagehash[n].tevorders_colorchan = bpmem.tevorders[n / 2].getColorChan(n & 1);

		const char *rasswap = swapModeTable[bpmem.combiners[n].alphaC.rswap];
		out.Write("irastemp = %s.%s;\n", tevRasTable[bpmem.tevorders[n / 2].getColorChan(n & 1)], rasswap);
	}

	uid_data.stagehash[n].tevorders_enable = bpmem.tevorders[n / 2].getEnable(n & 1);
	if (bpmem.tevorders[n/2].getEnable(n&1))
	{
		int texmap = bpmem.tevorders[n/2].getTexMap(n&1);
		if (!bHasIndStage)
		{
			// calc tevcord
			if(bHasTexCoord)
				out.Write("tevcoord.xy = int2(uv%d.xy*256.0);\n", texcoord);
			else
				out.Write("tevcoord.xy = int2(0, 0);\n");
		}

		const int i = bpmem.combiners[n].alphaC.tswap;
		uid_data.stagehash[n].ac |= bpmem.combiners[n].alphaC.tswap << 2;
		uid_data.stagehash[n].tevksel_swap1c = bpmem.tevksel[i*2].swap1;
		uid_data.stagehash[n].tevksel_swap2c = bpmem.tevksel[i*2].swap2;
		uid_data.stagehash[n].tevksel_swap1d = bpmem.tevksel[i*2+1].swap1;
		uid_data.stagehash[n].tevksel_swap2d = bpmem.tevksel[i*2+1].swap2;

		uid_data.stagehash[n].tevorders_texmap= bpmem.tevorders[n/2].getTexMap(n&1);

		const char *texswap = swapModeTable[bpmem.combiners[n].alphaC.tswap];
		uid_data.SetTevindrefTexmap(i, texmap);

		out.Write("itextemp = ");
		SampleTexture<T>(out, "(float2(tevcoord.xy)/256.0)", texswap, texmap, ApiType);
	}
	else
	{
		out.Write("itextemp = int4(255, 255, 255, 255);\n");
	}


	if (cc.a == TEVCOLORARG_KONST || cc.b == TEVCOLORARG_KONST || cc.c == TEVCOLORARG_KONST || cc.d == TEVCOLORARG_KONST
		|| ac.a == TEVALPHAARG_KONST || ac.b == TEVALPHAARG_KONST || ac.c == TEVALPHAARG_KONST || ac.d == TEVALPHAARG_KONST)
	{
		int kc = bpmem.tevksel[n / 2].getKC(n & 1);
		int ka = bpmem.tevksel[n / 2].getKA(n & 1);
		uid_data.stagehash[n].tevksel_kc = kc;
		uid_data.stagehash[n].tevksel_ka = ka;
		out.Write("ikonsttemp = int4(%s, %s);\n", tevKSelTableC[kc], tevKSelTableA[ka]);

		if (kc > 7)
			out.SetConstantsUsed(C_KCOLORS+((kc-0xc)%4),C_KCOLORS+((kc-0xc)%4));
		if (ka > 7)
			out.SetConstantsUsed(C_KCOLORS+((ka-0xc)%4),C_KCOLORS+((ka-0xc)%4));
	}

	if (cc.d == TEVCOLORARG_C0 || cc.d == TEVCOLORARG_A0 || ac.d == TEVALPHAARG_A0)
		out.SetConstantsUsed(C_COLORS+1,C_COLORS+1);

	if (cc.d == TEVCOLORARG_C1 || cc.d == TEVCOLORARG_A1 || ac.d == TEVALPHAARG_A1)
		out.SetConstantsUsed(C_COLORS+2,C_COLORS+2);

	if (cc.d == TEVCOLORARG_C2 || cc.d == TEVCOLORARG_A2 || ac.d == TEVALPHAARG_A2)
		out.SetConstantsUsed(C_COLORS+3,C_COLORS+3);

	if (cc.dest >= GX_TEVREG0 && cc.dest <= GX_TEVREG2)
		out.SetConstantsUsed(C_COLORS+cc.dest, C_COLORS+cc.dest);

	if (ac.dest >= GX_TEVREG0 && ac.dest <= GX_TEVREG2)
		out.SetConstantsUsed(C_COLORS+ac.dest, C_COLORS+ac.dest);

	out.Write("// color combine\n");
	out.Write("%s = clamp(", tevCOutputTable[cc.dest]);

	// combine the color channel
	if (cc.bias != TevBias_COMPARE) // if not compare
	{
		//normal color combiner goes here
		if (cc.shift > TEVSCALE_1)
			out.Write("(");

		if(!(cc.d == TEVCOLORARG_ZERO && cc.op == TEVOP_ADD))
			out.Write("%s %s ", tevCInputTable[cc.d], tevOpTable[cc.op]); // TODO: Clamp d...

		out.Write("((%s&255) * (int3(255,255,255) - (%s&255)) + (%s&255) * (%s&255)) / 255", tevCInputTable[cc.a], tevCInputTable[cc.c], tevCInputTable[cc.b], tevCInputTable[cc.c]);

		out.Write("%s", tevBiasTable[cc.bias]);

		if (cc.shift > TEVSCALE_1)
			out.Write(")%s", tevScaleTable[cc.shift]);
	}
	else
	{
		int cmp = (cc.shift<<1)|cc.op|8; // comparemode stored here
		out.Write(TEVCMPColorOPTable[cmp],//lookup the function from the op table
				tevCInputTable[cc.d],
				tevCInputTable[cc.a],
				tevCInputTable[cc.b],
				tevCInputTable[cc.c]);
	}
	if (cc.clamp)
		out.Write(", int3(0,0,0), int3(255,255,255))");
	else
		out.Write(", int3(-1024,-1024,-1024), int3(1023,1023,1023))");
	out.Write(";\n");

	out.Write("// alpha combine\n");
	if (ac.clamp)
		out.Write("%s = clamp(", tevAOutputTable[ac.dest]);
	else
		out.Write("%s = ", tevAOutputTable[ac.dest]);

	if (ac.bias != TevBias_COMPARE) // if not compare
	{
		//normal alpha combiner goes here
		if (ac.shift > 0)
			out.Write("(");

		if(!(ac.d == TEVALPHAARG_ZERO && ac.op == TEVOP_ADD))
			out.Write("%s.a %s ", tevAInputTable[ac.d], tevOpTable[ac.op]);

		out.Write("((%s.a&255) * (255 - (%s.a&255)) + (%s.a&255) * (%s.a&255)) / 255", tevAInputTable[ac.a], tevAInputTable[ac.c], tevAInputTable[ac.b], tevAInputTable[ac.c]);

		out.Write("%s",tevBiasTable[ac.bias]);

		if (ac.shift>0)
			out.Write(")%s", tevScaleTable[ac.shift]);

	}
	else
	{
		//compare alpha combiner goes here
		int cmp = (ac.shift<<1)|ac.op|8; // comparemode stored here
		out.Write(TEVCMPAlphaOPTable[cmp],
				tevAInputTable[ac.d],
				tevAInputTable[ac.a],
				tevAInputTable[ac.b],
				tevAInputTable[ac.c]);
	}
	if (ac.clamp)
		out.Write(", 0, 255)");
	out.Write(";\n\n");
	out.Write("// TEV done\n");
}

template<class T>
static inline void SampleTexture(T& out, const char *texcoords, const char *texswap, int texmap, API_TYPE ApiType)
{
	out.SetConstantsUsed(C_TEXDIMS+texmap,C_TEXDIMS+texmap);

	if (ApiType == API_D3D)
		out.Write("int4(255.0f * Tex%d.Sample(samp%d,%s.xy * " I_TEXDIMS"[%d].xy)).%s;\n", texmap,texmap, texcoords, texmap, texswap);
	else
		out.Write("int4(255.0f * texture(samp%d,%s.xy * " I_TEXDIMS"[%d].xy)).%s;\n", texmap, texcoords, texmap, texswap);
}

static const char *tevAlphaFuncsTable[] =
{
	"(false)",					// NEVER
	"(iprev.a <  %s)",			// LESS
	"(iprev.a == %s)",			// EQUAL
	"(iprev.a <= %s)",			// LEQUAL
	"(iprev.a >  %s)",			// GREATER
	"(iprev.a != %s)",			// NEQUAL
	"(iprev.a >= %s)",			// GEQUAL
	"(true)"					// ALWAYS
};

static const char *tevAlphaFunclogicTable[] =
{
	" && ", // and
	" || ", // or
	" != ", // xor
	" == "  // xnor
};

template<class T>
static inline void WriteAlphaTest(T& out, pixel_shader_uid_data& uid_data, API_TYPE ApiType, DSTALPHA_MODE dstAlphaMode, bool per_pixel_depth)
{
	static const char *alphaRef[2] =
	{
		"int(" I_ALPHA"[0].r * 255.0f)",
		"int(" I_ALPHA"[0].g * 255.0f)"
	};

	out.SetConstantsUsed(C_ALPHA, C_ALPHA);

	out.Write("\tif(!( ");

	uid_data.alpha_test_comp0 = bpmem.alpha_test.comp0;
	uid_data.alpha_test_comp1 = bpmem.alpha_test.comp1;
	uid_data.alpha_test_logic = bpmem.alpha_test.logic;

	// Lookup the first component from the alpha function table
	int compindex = bpmem.alpha_test.comp0;
	out.Write(tevAlphaFuncsTable[compindex], alphaRef[0]);

	out.Write("%s", tevAlphaFunclogicTable[bpmem.alpha_test.logic]);//lookup the logic op

	// Lookup the second component from the alpha function table
	compindex = bpmem.alpha_test.comp1;
	out.Write(tevAlphaFuncsTable[compindex], alphaRef[1]);
	out.Write(")) {\n");

	out.Write("\t\tocol0 = float4(0.0, 0.0, 0.0, 0.0);\n");
	if (dstAlphaMode == DSTALPHA_DUAL_SOURCE_BLEND)
		out.Write("\t\tocol1 = float4(0.0, 0.0, 0.0, 0.0);\n");
	if(per_pixel_depth)
		out.Write("\t\tdepth = 1.0;\n");

	// HAXX: zcomploc (aka early_ztest) is a way to control whether depth test is done before
	// or after texturing and alpha test. PC graphics APIs have no way to support this
	// feature properly as of 2012: Depth buffer and depth test are not
	// programmable and the depth test is always done after texturing.
	// Most importantly, they do not allow writing to the z-buffer without
	// writing a color value (unless color writing is disabled altogether).
	// We implement "depth test before texturing" by disabling alpha test when early-z is in use.
	// It seems to be less buggy than not to update the depth buffer if alpha test fails,
	// but both ways wouldn't be accurate.

	// OpenGL 4.2 has a flag which allows the driver to still update the depth buffer
	// if alpha test fails. The driver doesn't have to, but I assume they all do because
	// it's the much faster code path for the GPU.
	uid_data.alpha_test_use_zcomploc_hack = bpmem.UseEarlyDepthTest() && bpmem.zmode.updateenable && !g_ActiveConfig.backend_info.bSupportsEarlyZ;
	if (!uid_data.alpha_test_use_zcomploc_hack)
	{
		out.Write("\t\tdiscard;\n");
		if (ApiType != API_D3D)
			out.Write("\t\treturn;\n");
	}

	out.Write("}\n");
}

static const char *tevFogFuncsTable[] =
{
	"",																// No Fog
	"",																// ?
	"",																// Linear
	"",																// ?
	"\tfog = 1.0 - pow(2.0, -8.0 * fog);\n",						// exp
	"\tfog = 1.0 - pow(2.0, -8.0 * fog * fog);\n",				// exp2
	"\tfog = pow(2.0, -8.0 * (1.0 - fog));\n",					// backward exp
	"\tfog = 1.0 - fog;\n   fog = pow(2.0, -8.0 * fog * fog);\n"	// backward exp2
};

template<class T>
static inline void WriteFog(T& out, pixel_shader_uid_data& uid_data)
{
	uid_data.fog_fsel = bpmem.fog.c_proj_fsel.fsel;
	if(bpmem.fog.c_proj_fsel.fsel == 0)
		return; // no Fog

	uid_data.fog_proj = bpmem.fog.c_proj_fsel.proj;

	out.SetConstantsUsed(C_FOG, C_FOG+1);
	if (bpmem.fog.c_proj_fsel.proj == 0)
	{
		// perspective
		// ze = A/(B - (Zs >> B_SHF)
		out.Write("\tfloat ze = " I_FOG"[1].x / (" I_FOG"[1].y - (zCoord / " I_FOG"[1].w));\n");
	}
	else
	{
		// orthographic
		// ze = a*Zs	(here, no B_SHF)
		out.Write("\tfloat ze = " I_FOG"[1].x * zCoord;\n");
	}

	// x_adjust = sqrt((x-center)^2 + k^2)/k
	// ze *= x_adjust
	// this is completely theoretical as the real hardware seems to use a table intead of calculating the values.
	uid_data.fog_RangeBaseEnabled = bpmem.fogRange.Base.Enabled;
	if (bpmem.fogRange.Base.Enabled)
	{
		out.SetConstantsUsed(C_FOG+2, C_FOG+2);
		out.Write("\tfloat x_adjust = (2.0 * (clipPos.x / " I_FOG"[2].y)) - 1.0 - " I_FOG"[2].x;\n");
		out.Write("\tx_adjust = sqrt(x_adjust * x_adjust + " I_FOG"[2].z * " I_FOG"[2].z) / " I_FOG"[2].z;\n");
		out.Write("\tze *= x_adjust;\n");
	}

	out.Write("\tfloat fog = clamp(ze - " I_FOG"[1].z, 0.0, 1.0);\n");

	if (bpmem.fog.c_proj_fsel.fsel > 3)
	{
		out.Write("%s", tevFogFuncsTable[bpmem.fog.c_proj_fsel.fsel]);
	}
	else
	{
		if (bpmem.fog.c_proj_fsel.fsel != 2 && out.GetBuffer() != NULL)
			WARN_LOG(VIDEO, "Unknown Fog Type! %08x", bpmem.fog.c_proj_fsel.fsel);
	}

	out.Write("\tint ifog = int(fog * 256.0);\n");
	out.Write("\tiprev.rgb = (iprev.rgb * (256 - ifog) + int(" I_FOG"[0].rgb * 256.0 * ifog)) >> 8;\n");
}

void GetPixelShaderUid(PixelShaderUid& object, DSTALPHA_MODE dstAlphaMode, API_TYPE ApiType, u32 components)
{
	GeneratePixelShader<PixelShaderUid>(object, dstAlphaMode, ApiType, components);
}

void GeneratePixelShaderCode(PixelShaderCode& object, DSTALPHA_MODE dstAlphaMode, API_TYPE ApiType, u32 components)
{
	GeneratePixelShader<PixelShaderCode>(object, dstAlphaMode, ApiType, components);
}

void GetPixelShaderConstantProfile(PixelShaderConstantProfile& object, DSTALPHA_MODE dstAlphaMode, API_TYPE ApiType, u32 components)
{
	GeneratePixelShader<PixelShaderConstantProfile>(object, dstAlphaMode, ApiType, components);
}

