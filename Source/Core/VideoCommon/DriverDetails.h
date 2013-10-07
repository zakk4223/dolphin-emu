// Copyright 2013 Dolphin Emulator Project
// Licensed under GPLv2
// Refer to the license.txt file included.
#pragma once
#include "CommonTypes.h"

namespace DriverDetails
{
	// Enum of known vendors
	// Tegra and Nvidia are separated out due to such substantial differences
	enum Vendor
	{
		VENDOR_ALL = 0,
		VENDOR_NVIDIA,
		VENDOR_ATI,
		VENDOR_INTEL,
		VENDOR_ARM,
		VENDOR_QUALCOMM,
		VENDOR_IMGTEC,
		VENDOR_TEGRA,
		VENDOR_VIVANTE,
		VENDOR_MESA,
		VENDOR_UNKNOWN
	};

	// Enum of known drivers
	enum Driver
	{
		DRIVER_ALL = 0,
		DRIVER_NVIDIA, // Official Nvidia, including mobile GPU
		DRIVER_NOUVEAU, // OSS nouveau
		DRIVER_ATI, // Official ATI
		DRIVER_R600, // OSS Radeon
		DRIVER_INTEL, // Official Intel
		DRIVER_I965, // OSS Intel
		DRIVER_ARM_4XX, // Official Mali driver
		DRIVER_ARM_T6XX, // Official Mali driver
		DRIVER_LIMA, // OSS Mali driver
		DRIVER_QUALCOMM_3XX, // Official Adreno driver 3xx
		DRIVER_QUALCOMM_2XX, // Official Adreno driver 2xx
		DRIVER_FREEDRENO, // OSS Adreno driver
		DRIVER_IMGTEC, // OSS PowerVR driver
		DRIVER_VIVANTE, // Official vivante driver
		DRIVER_UNKNOWN // Unknown driver, default to official hardware driver
	};

	// Enum of known bugs
	// These can be vendor specific, but we put them all in here
	// For putting a new bug in here, make sure to put a detailed comment above the enum
	// This'll ensure we know exactly what the issue is.
	enum Bug
	{
		// Bug: No Dynamic UBO array object access
		// Affected Devices: Qualcomm/Adreno
		// Started Version: 14
		// Ended Version: -1
		// Accessing UBO array members dynamically causes the Adreno shader compiler to crash
		// Errors out with "Internal Error"
		BUG_NODYNUBOACCESS = 0,
		// Bug: Centroid is broken in shaders
		// Affected devices: Qualcomm/Adreno
		// Started Version: 14
		// Ended Version: -1
		// Centroid in/out, used in the shaders, is used for multisample buffers to get the texel correctly
		// When MSAA is disabled, it acts like a regular in/out
		// Tends to cause the driver to render full white or black
		BUG_BROKENCENTROID,
		// Bug: INFO_LOG_LENGTH broken
		// Affected devices: Qualcomm/Adreno
		// Started Version: ? (Noticed on v14)
		// Ended Version: -1
		// When compiling a shader, it is important that when it fails, 
		// you first get the length of the information log prior to grabbing it.
		// This allows you to allocate an array to store all of the log
		// Adreno devices /always/ return 0 when querying GL_INFO_LOG_LENGTH
		// They also max out at 1024 bytes(1023 characters + null terminator) for the log
		BUG_BROKENINFOLOG,
		// Bug: UBO buffer offset broken
		// Affected devices: all mesa drivers
		// Started Version: 9.0 (mesa doesn't support ubo before)
		// Ended Version: up to 9.2
		// The offset of glBindBufferRange was ignored on all Mesa Gallium3D drivers until 9.1.3
		// Nouveau stored the offset as u16 which isn't enough for all cases with range until 9.1.6
		// I965 has broken data fetches from uniform buffers which results in a dithering until 9.2.0
		BUG_BROKENUBO,
		// Bug: The hacked buffer upload method isn't working
		//      This isn't a bug as the hacked buffer itself isn't used to work.
		//      I'm still surprised that it works on so many drivers.
		// Affected devices: - amd close sourced driver
		//                   - nouveau
		//                   - maybe also some others
		// This hack is evil. It's like free(pointer); *pointer = data;
		BUG_BROKENHACKEDBUFFER,
		// Bug: The pinned memory extension isn't working for index buffers
		// Affected devices: AMD as they are the only vendor providing this extension
		// Started Version: ?
		// Ended Version: -1
		// Pinned memory is disabled for index buffer as the amd driver (the only one with pinned memory support) seems
		// to be broken. We just get flickering/black rendering when using pinned memory here -- degasus - 2013/08/20
		// Please see issue #6105 on google code. Let's hope buffer storage solves this issues.
		BUG_BROKENPINNEDMEMORY,
		// Bug: Entirely broken UBOs
		// Affected devices: Qualcomm/Adreno
		// Started Version: ? (Noticed on v45)
		// Ended Version: -1
		// Uniform buffers are entirely broken on Qualcomm drivers with v45
		// Trying to use the uniform buffers causes a malloc to fail inside the driver
		// To be safe, blanket drivers from v41 - v45
		BUG_ANNIHILATEDUBOS,
		// Bug : Can't draw on screen text and clear correctly.
		// Affected devices: Qualcomm/Adreno
		// Started Version: ?
		// Ended Version: ?
		// Current code for drawing on screen text and clearing the framebuffer doesn't work on Adreno
		// Drawing on screen text causes the whole screen to swizzle in a terrible fashion
		// Clearing the framebuffer causes one to never see a frame.
		BUG_BROKENSWAP,
		// Bug: Running on a Tegra 4 device
		// Affected devices: Nvidia Tegra
		// Started Version: 4
		// Ended Version: 5
		// Tegra 4 hardware limitations don't allow it to support OpenGL ES 3
		// This is fixed in Tegra 5
		BUG_ISTEGRA,
		// Bug: Running on a PowerVR5 device
		// Affected devices: PowerVR54x
		// Started Version: 540
		// Ended Version: 6xxx
		// PowerVR 5 hardware limitations don't allow it to support OpenGL ES 3
		// This is fixed in PowerVR6
		BUG_ISPOWERVR,
	};
	
	// Initializes our internal vendor, device family, and driver version	
	void Init(Vendor vendor, Driver driver, const double version);
	
	// Once Vendor and driver version is set, this will return if it has the applicable bug passed to it.
	bool HasBug(Bug bug);
}
