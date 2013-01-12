// Copyright (C) 2003 Dolphin Project.

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, version 2.0.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License 2.0 for more details.

// A copy of the GPL 2.0 should have been included with the program.
// If not, see http://www.gnu.org/licenses/

// Official SVN repository and contact information can be found at
// http://code.google.com/p/dolphin-emu/

#include <string>

#include "Common.h"

#include "../../HW/Memmap.h"
#include "Jit.h"
#include "../JitCommon/JitBackpatch.h"
#include "StringUtil.h"

#ifdef _M_X64
static void BackPatchError(const std::string &text, u8 *codePtr, u32 emAddress) {
	u64 code_addr = (u64)codePtr;
	disassembler disasm;
	char disbuf[256];
	memset(disbuf, 0, 256);
#ifdef _M_IX86
	disasm.disasm32(0, code_addr, codePtr, disbuf);
#else
	disasm.disasm64(0, code_addr, codePtr, disbuf);
#endif
	PanicAlert("%s\n\n"
       "Error encountered accessing emulated address %08x.\n"
	   "Culprit instruction: \n%s\nat %#llx",
	   text.c_str(), emAddress, disbuf, code_addr);
	return;
}
#endif

// This generates some fairly heavy trampolines, but:
// 1) It's really necessary. We don't know anything about the context.
// 2) It doesn't really hurt. Only instructions that access I/O will get these, and there won't be 
//    that many of them in a typical program/game.
const u8 *JitArm::BackPatch(u8 *codePtr, int accessType, u32 emAddress, void *ctx_void)
{
	// TODO: This ctx needs to be filled with our information
	CONTEXT *ctx = (CONTEXT *)ctx_void;
	
	/*
	if (info.isMemoryWrite) {
		if (!Memory::IsRAMAddress(emAddress, true)) {
			PanicAlert("Exception: Caught write to invalid address %08x", emAddress);
			return;
		}
		BackPatchError("BackPatch - determined that MOV is write, not yet supported and should have been caught before",
					   codePtr, emAddress);
	}*/

	if (accessType == OP_ACCESS_WRITE)
		PanicAlert("BackPatch : Currently only supporting reads."
		           "\n\nAttempted to write to %08x.", emAddress);

	// In the first iteration, we assume that all accesses are 32-bit. We also only deal with reads.
	if (accessType == 0)
	{
#define ARMREGOFFSET (4 * 6)
		ARMXEmitter emitter(codePtr - ARMREGOFFSET);
		
		// We need to get the destination register before we start
		u32 Value = ((u32)*(codePtr + 4)) & 0x0FFFFFFF;
		ARMReg rD = (ARMReg)(Value & 0xF);
		int accessSize = (Value & 0x30) >> 4;
		printf("AccessSize: %d\n", accessSize);
		
		switch (accessSize)
		{
			case 0: // 8bit
				emitter.ARMABI_MOVI2R(R14, (u32)&Memory::Read_U8, false); // 2	
			break;
			case 1: // 16bit
				emitter.ARMABI_MOVI2R(R14, (u32)&Memory::Read_U16, false); // 2	
			break;
			case 2: // 32bit
				emitter.ARMABI_MOVI2R(R14, (u32)&Memory::Read_U32, false); // 2	
			break;
		}
		emitter.PUSH(4, R0, R1, R2, R3); // 3
		emitter.MOV(R0, R10); // 4
		emitter.BL(R14); // 5
		emitter.MOV(R14, R0); // 6
		emitter.POP(4, R0, R1, R2, R3); // 7
		emitter.MOV(rD, R14); // 8
		emitter.NOP(2); // 9-10
		ctx->reg_pc -= ARMREGOFFSET + (4 * 4);
		emitter.Flush();
		return codePtr;
	}
/*	else if (accessType == 1)
	{
		// TODO: special case FIFO writes. Also, support 32-bit mode.
		// Also, debug this so that it actually works correctly :P
		XEmitter emitter(codePtr - 2);
		// We know it's EAX so the BSWAP before will be two byte. Overwrite it.
		const u8 *trampoline = trampolines.GetWriteTrampoline(info);
		emitter.CALL((void *)trampoline);
		emitter.NOP((int)info.instructionSize - 3);
		if (info.instructionSize < 3)
			PanicAlert("instruction too small");
		// We entered here with a BSWAP-ed EAX. We'll have to swap it back.
		ctx->Rax = Common::swap32((u32)ctx->Rax);
		return codePtr - 2;
	}*/
	return 0;
}

