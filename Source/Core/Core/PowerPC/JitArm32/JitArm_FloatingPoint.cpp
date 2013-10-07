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

#include "Common.h"

#include "../../Core.h"
#include "../PowerPC.h"
#include "../../ConfigManager.h"
#include "../../CoreTiming.h"
#include "../PPCTables.h"
#include "ArmEmitter.h"
#include "../../HW/Memmap.h"
#include "JitArm_FPUtils.h"

#include "Jit.h"
#include "JitRegCache.h"
#include "JitFPRCache.h"
#include "JitAsm.h"

void JitArm::Helper_UpdateCR1(ARMReg fpscr, ARMReg temp)
{
	UBFX(temp, fpscr, 28, 4);
	STRB(temp, R9, PPCSTATE_OFF(cr_fast[1]));
}

void JitArm::fctiwx(UGeckoInstruction inst)
{
	INSTRUCTION_START
	JITDISABLE(bJITFloatingPointOff)
	u32 b = inst.FB;
	u32 d = inst.FD;

	ARMReg vB = fpr.R0(b);
	ARMReg vD = fpr.R0(d);
	ARMReg V0 = fpr.GetReg();
	ARMReg V1 = fpr.GetReg();
	ARMReg V2 = fpr.GetReg();
	
	ARMReg rA = gpr.GetReg();
	ARMReg fpscrReg = gpr.GetReg();

	FixupBranch DoneMax, DoneMin;
	LDR(fpscrReg, R9, PPCSTATE_OFF(fpscr));
	MOVI2R(rA, (u32)minmaxFloat);

	// Check if greater than max float
	{
		VLDR(V0, rA, 8); // Load Max
		VCMPE(vB, V0);
		VMRS(_PC); // Loads in to APSR
		FixupBranch noException = B_CC(CC_LE);
		VMOV(vD, V0); // Set to max
		SetFPException(fpscrReg, FPSCR_VXCVI);
		DoneMax = B();
		SetJumpTarget(noException);
	}
	// Check if less than min float
	{
		VLDR(V0, rA, 0);
		VCMPE(vB, V0);
		VMRS(_PC);
		FixupBranch noException = B_CC(CC_GE);
		VMOV(vD, V0);
		SetFPException(fpscrReg, FPSCR_VXCVI);
		DoneMin = B();
		SetJumpTarget(noException);
	}
	// Within ranges, convert to integer
	// Set rounding mode first
	// PPC <-> ARM rounding modes
	// 0, 1, 2, 3 <-> 0, 3, 1, 2  
	ARMReg rB = gpr.GetReg();
	VMRS(rA);
	// Bits 22-23
	BIC(rA, rA, Operand2(3, 5));

	LDR(rB, R9, PPCSTATE_OFF(fpscr));
	AND(rB, rB, 0x3); // Get the FPSCR rounding bits
	CMP(rB, 1);
	SetCC(CC_EQ); // zero 
		ORR(rA, rA, Operand2(3, 5));
	SetCC(CC_NEQ);
		CMP(rB, 2); // +inf
		SetCC(CC_EQ);
			ORR(rA, rA, Operand2(1, 5));
		SetCC(CC_NEQ);
			CMP(rB, 3); // -inf
			SetCC(CC_EQ);
				ORR(rA, rA, Operand2(2, 5));
	SetCC();	
	VMSR(rA);
	ORR(rA, rA, Operand2(3, 5));
	VCVT(vD, vB, TO_INT | IS_SIGNED); 
	VMSR(rA);
	gpr.Unlock(rB);
	VCMPE(vD, vB);
	VMRS(_PC);

	SetCC(CC_EQ);
		BIC(fpscrReg, fpscrReg, FRFIMask);
		FixupBranch DoneEqual = B();
	SetCC();
	SetFPException(fpscrReg, FPSCR_XX);	
	ORR(fpscrReg, fpscrReg, FIMask);
	VABS(V1, vB);
	VABS(V2, vD);
	VCMPE(V2, V1);
	VMRS(_PC);
	SetCC(CC_GT);
		ORR(fpscrReg, fpscrReg, FRMask);
	SetCC();
	SetJumpTarget(DoneEqual);

	SetJumpTarget(DoneMax);
	SetJumpTarget(DoneMin);

	MOVI2R(rA, (u32)&doublenum);
	VLDR(V0, rA, 0);
	NEONXEmitter nemit(this);
	nemit.VORR(vD, vD, V0);

	if (inst.Rc) Helper_UpdateCR1(fpscrReg, rA);

	STR(fpscrReg, R9, PPCSTATE_OFF(fpscr));
	gpr.Unlock(rA);
	gpr.Unlock(fpscrReg);
	fpr.Unlock(V0);
	fpr.Unlock(V1);
	fpr.Unlock(V2);
}


void JitArm::fctiwzx(UGeckoInstruction inst)
{
	INSTRUCTION_START
	JITDISABLE(bJITFloatingPointOff)
	u32 b = inst.FB;
	u32 d = inst.FD;

	ARMReg vB = fpr.R0(b);
	ARMReg vD = fpr.R0(d);
	ARMReg V0 = fpr.GetReg();
	ARMReg V1 = fpr.GetReg();
	ARMReg V2 = fpr.GetReg();
	
	ARMReg rA = gpr.GetReg();
	ARMReg fpscrReg = gpr.GetReg();

	FixupBranch DoneMax, DoneMin;
	LDR(fpscrReg, R9, PPCSTATE_OFF(fpscr));
	MOVI2R(rA, (u32)minmaxFloat);

	// Check if greater than max float
	{
		VLDR(V0, rA, 8); // Load Max
		VCMPE(vB, V0);
		VMRS(_PC); // Loads in to APSR
		FixupBranch noException = B_CC(CC_LE);
		VMOV(vD, V0); // Set to max
		SetFPException(fpscrReg, FPSCR_VXCVI);
		DoneMax = B();
		SetJumpTarget(noException);
	}
	// Check if less than min float
	{
		VLDR(V0, rA, 0);
		VCMPE(vB, V0);
		VMRS(_PC);
		FixupBranch noException = B_CC(CC_GE);
		VMOV(vD, V0);
		SetFPException(fpscrReg, FPSCR_VXCVI);
		DoneMin = B();
		SetJumpTarget(noException);
	}
	// Within ranges, convert to integer
	VCVT(vD, vB, TO_INT | IS_SIGNED | ROUND_TO_ZERO); 
	VCMPE(vD, vB);
	VMRS(_PC);

	SetCC(CC_EQ);
		BIC(fpscrReg, fpscrReg, FRFIMask);
		FixupBranch DoneEqual = B();
	SetCC();
	SetFPException(fpscrReg, FPSCR_XX);	
	ORR(fpscrReg, fpscrReg, FIMask);
	VABS(V1, vB);
	VABS(V2, vD);
	VCMPE(V2, V1);
	VMRS(_PC);
	SetCC(CC_GT);
		ORR(fpscrReg, fpscrReg, FRMask);
	SetCC();
	SetJumpTarget(DoneEqual);

	SetJumpTarget(DoneMax);
	SetJumpTarget(DoneMin);

	MOVI2R(rA, (u32)&doublenum);
	VLDR(V0, rA, 0);
	NEONXEmitter nemit(this);
	nemit.VORR(vD, vD, V0);

	if (inst.Rc) Helper_UpdateCR1(fpscrReg, rA);

	STR(fpscrReg, R9, PPCSTATE_OFF(fpscr));
	gpr.Unlock(rA);
	gpr.Unlock(fpscrReg);
	fpr.Unlock(V0);
	fpr.Unlock(V1);
	fpr.Unlock(V2);
}

void JitArm::fcmpo(UGeckoInstruction inst)
{
	INSTRUCTION_START
	JITDISABLE(bJITFloatingPointOff)
	u32 a = inst.FA, b = inst.FB;
	int cr = inst.CRFD;

	ARMReg vA = fpr.R0(a);
	ARMReg vB = fpr.R0(b);
	ARMReg fpscrReg = gpr.GetReg();
	ARMReg crReg = gpr.GetReg();
	Operand2 FPRFMask(0x1F, 0xA); // 0x1F000
	Operand2 LessThan(0x8, 0xA); // 0x8000
	Operand2 GreaterThan(0x4, 0xA); // 0x4000
	Operand2 EqualTo(0x2, 0xA); // 0x2000
	Operand2 NANRes(0x1, 0xA); // 0x1000
	FixupBranch Done1, Done2, Done3;
	LDR(fpscrReg, R9, PPCSTATE_OFF(fpscr));
	BIC(fpscrReg, fpscrReg, FPRFMask);

	VCMPE(vA, vB);
	VMRS(_PC);
	SetCC(CC_LT);
		ORR(fpscrReg, fpscrReg, LessThan);	
		MOV(crReg,  8);
		Done1 = B();
	SetCC(CC_GT);
		ORR(fpscrReg, fpscrReg, GreaterThan);	
		MOV(crReg,  4);
		Done2 = B();
	SetCC(CC_EQ);
		ORR(fpscrReg, fpscrReg, EqualTo);	
		MOV(crReg,  2);
		Done3 = B();
	SetCC();
	
	ORR(fpscrReg, fpscrReg, NANRes);	
	MOV(crReg,  1);
	
	VCMPE(vA, vA);
	VMRS(_PC);
	FixupBranch NanA = B_CC(CC_NEQ);	
	VCMPE(vB, vB);
	VMRS(_PC);
	FixupBranch NanB = B_CC(CC_NEQ);
	
	SetFPException(fpscrReg, FPSCR_VXVC);
	FixupBranch Done4 = B();

	SetJumpTarget(NanA);
	SetJumpTarget(NanB);

	SetFPException(fpscrReg, FPSCR_VXSNAN);
	
	TST(fpscrReg, VEMask);
	
	FixupBranch noVXVC = B_CC(CC_NEQ);
	SetFPException(fpscrReg, FPSCR_VXVC);

	SetJumpTarget(noVXVC);
	SetJumpTarget(Done1);
	SetJumpTarget(Done2);
	SetJumpTarget(Done3);
	SetJumpTarget(Done4);
	STRB(crReg, R9, PPCSTATE_OFF(cr_fast) + cr);
	STR(fpscrReg, R9, PPCSTATE_OFF(fpscr));
	gpr.Unlock(fpscrReg, crReg);
}

void JitArm::fcmpu(UGeckoInstruction inst)
{
	INSTRUCTION_START
	JITDISABLE(bJITFloatingPointOff)
	u32 a = inst.FA, b = inst.FB;
	int cr = inst.CRFD;

	ARMReg vA = fpr.R0(a);
	ARMReg vB = fpr.R0(b);
	ARMReg fpscrReg = gpr.GetReg();
	ARMReg crReg = gpr.GetReg();
	Operand2 FPRFMask(0x1F, 0xA); // 0x1F000
	Operand2 LessThan(0x8, 0xA); // 0x8000
	Operand2 GreaterThan(0x4, 0xA); // 0x4000
	Operand2 EqualTo(0x2, 0xA); // 0x2000
	Operand2 NANRes(0x1, 0xA); // 0x1000
	FixupBranch Done1, Done2, Done3;
	LDR(fpscrReg, R9, PPCSTATE_OFF(fpscr));
	BIC(fpscrReg, fpscrReg, FPRFMask);

	VCMPE(vA, vB);
	VMRS(_PC);
	SetCC(CC_LT);
		ORR(fpscrReg, fpscrReg, LessThan);	
		MOV(crReg,  8);
		Done1 = B();
	SetCC(CC_GT);
		ORR(fpscrReg, fpscrReg, GreaterThan);	
		MOV(crReg,  4);
		Done2 = B();
	SetCC(CC_EQ);
		ORR(fpscrReg, fpscrReg, EqualTo);	
		MOV(crReg,  2);
		Done3 = B();
	SetCC();
	
	ORR(fpscrReg, fpscrReg, NANRes);	
	MOV(crReg,  1);
	
	VCMPE(vA, vA);
	VMRS(_PC);
	FixupBranch NanA = B_CC(CC_NEQ);	
	VCMPE(vB, vB);
	VMRS(_PC);
	FixupBranch NanB = B_CC(CC_NEQ);
	FixupBranch Done4 = B();

	SetJumpTarget(NanA);
	SetJumpTarget(NanB);

	SetFPException(fpscrReg, FPSCR_VXSNAN);
	
	SetJumpTarget(Done1);
	SetJumpTarget(Done2);
	SetJumpTarget(Done3);
	SetJumpTarget(Done4);
	STRB(crReg, R9, PPCSTATE_OFF(cr_fast) + cr);
	STR(fpscrReg, R9, PPCSTATE_OFF(fpscr));
	gpr.Unlock(fpscrReg, crReg);
}

void JitArm::fabsx(UGeckoInstruction inst)
{
	INSTRUCTION_START
	JITDISABLE(bJITFloatingPointOff)

	if (inst.Rc) {
		Default(inst);
		return;
	}

	ARMReg vB = fpr.R0(inst.FB);
	ARMReg vD = fpr.R0(inst.FD, false);

	VABS(vD, vB);
}

void JitArm::fnabsx(UGeckoInstruction inst)
{
	INSTRUCTION_START
	JITDISABLE(bJITFloatingPointOff)

	if (inst.Rc) {
		Default(inst);
		return;
	}

	ARMReg vB = fpr.R0(inst.FB);
	ARMReg vD = fpr.R0(inst.FD, false);

	VABS(vD, vB);
	VNEG(vD, vD);
}

void JitArm::fnegx(UGeckoInstruction inst)
{
	INSTRUCTION_START
	JITDISABLE(bJITFloatingPointOff)

	if (inst.Rc) {
		Default(inst);
		return;
	}

	ARMReg vB = fpr.R0(inst.FB);
	ARMReg vD = fpr.R0(inst.FD, false);

	VNEG(vD, vB);
}

void JitArm::faddsx(UGeckoInstruction inst)
{
	INSTRUCTION_START
	JITDISABLE(bJITFloatingPointOff)

	if (inst.Rc) {
		Default(inst);
		return;
	}

	ARMReg vA = fpr.R0(inst.FA);
	ARMReg vB = fpr.R0(inst.FB);
	ARMReg vD0 = fpr.R0(inst.FD, false);
	ARMReg vD1 = fpr.R1(inst.FD, false);

	VADD(vD0, vA, vB);
	VMOV(vD1, vD0);
}

void JitArm::faddx(UGeckoInstruction inst)
{
	INSTRUCTION_START
	JITDISABLE(bJITFloatingPointOff)

	if (inst.Rc) {
		Default(inst);
		return;
	}

	ARMReg vA = fpr.R0(inst.FA);
	ARMReg vB = fpr.R0(inst.FB);
	ARMReg vD = fpr.R0(inst.FD, false);

	VADD(vD, vA, vB);
}

void JitArm::fsubsx(UGeckoInstruction inst)
{
	INSTRUCTION_START
	JITDISABLE(bJITFloatingPointOff)

	if (inst.Rc) {
		Default(inst);
		return;
	}

	ARMReg vA = fpr.R0(inst.FA);
	ARMReg vB = fpr.R0(inst.FB);
	ARMReg vD0 = fpr.R0(inst.FD, false);
	ARMReg vD1 = fpr.R1(inst.FD, false);

	VSUB(vD0, vA, vB);
	VMOV(vD1, vD0);
}

void JitArm::fsubx(UGeckoInstruction inst)
{
	INSTRUCTION_START
	JITDISABLE(bJITFloatingPointOff)

	if (inst.Rc) {
		Default(inst);
		return;
	}

	ARMReg vA = fpr.R0(inst.FA);
	ARMReg vB = fpr.R0(inst.FB);
	ARMReg vD = fpr.R0(inst.FD, false);

	VSUB(vD, vA, vB);
}

void JitArm::fmulsx(UGeckoInstruction inst)
{
	INSTRUCTION_START
	JITDISABLE(bJITFloatingPointOff)

	if (inst.Rc) {
		Default(inst);
		return;
	}

	ARMReg vA = fpr.R0(inst.FA);
	ARMReg vC = fpr.R0(inst.FC);
	ARMReg vD0 = fpr.R0(inst.FD, false);
	ARMReg vD1 = fpr.R1(inst.FD, false);

	VMUL(vD0, vA, vC);
	VMOV(vD1, vD0);
}
void JitArm::fmulx(UGeckoInstruction inst)
{
	INSTRUCTION_START
	JITDISABLE(bJITFloatingPointOff)

	if (inst.Rc) {
		Default(inst);
		return;
	}

	ARMReg vA = fpr.R0(inst.FA);
	ARMReg vC = fpr.R0(inst.FC);
	ARMReg vD0 = fpr.R0(inst.FD, false);

	VMUL(vD0, vA, vC);
}
void JitArm::fmrx(UGeckoInstruction inst)
{
	INSTRUCTION_START
	JITDISABLE(bJITFloatingPointOff)

	if (inst.Rc) {
		Default(inst);
		return;
	}

	ARMReg vB = fpr.R0(inst.FB);
	ARMReg vD = fpr.R0(inst.FD, false);

	VMOV(vD, vB);
}

void JitArm::fmaddsx(UGeckoInstruction inst)
{
	INSTRUCTION_START
	JITDISABLE(bJITFloatingPointOff)

	if (inst.Rc) {
		Default(inst);
		return;
	}

	u32 a = inst.FA, b = inst.FB, c = inst.FC, d = inst.FD;

	ARMReg vA0 = fpr.R0(a);
	ARMReg vB0 = fpr.R0(b);
	ARMReg vC0 = fpr.R0(c);
	ARMReg vD0 = fpr.R0(d, false);
	ARMReg vD1 = fpr.R1(d, false);

	ARMReg V0 = fpr.GetReg();

	VMOV(V0, vB0);
	
	VMLA(V0, vA0, vC0);

	VMOV(vD0, V0);
	VMOV(vD1, V0);

	fpr.Unlock(V0);
}

void JitArm::fmaddx(UGeckoInstruction inst)
{
	INSTRUCTION_START
	JITDISABLE(bJITFloatingPointOff)

	if (inst.Rc) {
		Default(inst);
		return;
	}

	u32 a = inst.FA, b = inst.FB, c = inst.FC, d = inst.FD;

	ARMReg vA0 = fpr.R0(a);
	ARMReg vB0 = fpr.R0(b);
	ARMReg vC0 = fpr.R0(c);
	ARMReg vD0 = fpr.R0(d, false);

	ARMReg V0 = fpr.GetReg();

	VMOV(V0, vB0);
	
	VMLA(V0, vA0, vC0);

	VMOV(vD0, V0);

	fpr.Unlock(V0);
}

void JitArm::fnmaddx(UGeckoInstruction inst)
{
	INSTRUCTION_START
	JITDISABLE(bJITFloatingPointOff)

	u32 a = inst.FA, b = inst.FB, c = inst.FC, d = inst.FD;

	if (inst.Rc) {
		Default(inst);
		return;
	}

	ARMReg vA0 = fpr.R0(a);
	ARMReg vB0 = fpr.R0(b);
	ARMReg vC0 = fpr.R0(c);
	ARMReg vD0 = fpr.R0(d, false);

	ARMReg V0 = fpr.GetReg();

	VMOV(V0, vB0);
	
	VMLA(V0, vA0, vC0);

	VNEG(vD0, V0);

	fpr.Unlock(V0);
}
void JitArm::fnmaddsx(UGeckoInstruction inst)
{
	INSTRUCTION_START
	JITDISABLE(bJITFloatingPointOff)

	u32 a = inst.FA, b = inst.FB, c = inst.FC, d = inst.FD;

	if (inst.Rc) {
		Default(inst);
		return;
	}

	ARMReg vA0 = fpr.R0(a);
	ARMReg vB0 = fpr.R0(b);
	ARMReg vC0 = fpr.R0(c);
	ARMReg vD0 = fpr.R0(d, false);
	ARMReg vD1 = fpr.R1(d, false);

	ARMReg V0 = fpr.GetReg();

	VMOV(V0, vB0);
	
	VMLA(V0, vA0, vC0);

	VNEG(vD0, V0);
	VNEG(vD1, V0);

	fpr.Unlock(V0);
}

// XXX: Messes up Super Mario Sunshine title screen
void JitArm::fresx(UGeckoInstruction inst)
{
	INSTRUCTION_START
	JITDISABLE(bJITFloatingPointOff)

	u32 b = inst.FB, d = inst.FD;

	if (inst.Rc) {
		Default(inst);
		return;
	}

	Default(inst); return;

	ARMReg vB0 = fpr.R0(b);
	ARMReg vD0 = fpr.R0(d, false);
	ARMReg vD1 = fpr.R1(d, false);

	ARMReg V0 = fpr.GetReg();
	MOVI2R(V0, 1.0, INVALID_REG); // temp reg isn't needed for 1.0
	
	VDIV(vD1, V0, vB0);
	VDIV(vD0, V0, vB0);
	fpr.Unlock(V0);
}

void JitArm::fselx(UGeckoInstruction inst)
{
	INSTRUCTION_START
	JITDISABLE(bJITPairedOff)
	
	u32 a = inst.FA, b = inst.FB, c = inst.FC, d = inst.FD;

	if (inst.Rc) {
		Default(inst); return;
	}
	ARMReg vA0 = fpr.R0(a);
	ARMReg vB0 = fpr.R0(b);
	ARMReg vC0 = fpr.R0(c);
	ARMReg vD0 = fpr.R0(d, false);
	
	VCMP(vA0);
	VMRS(_PC);

	FixupBranch GT0 = B_CC(CC_GE);
	VMOV(vD0, vB0);
	FixupBranch EQ0 = B();
	SetJumpTarget(GT0);
	VMOV(vD0, vC0);
	SetJumpTarget(EQ0);
}

void JitArm::frsqrtex(UGeckoInstruction inst)
{
	INSTRUCTION_START
	JITDISABLE(bJITPairedOff)

	u32 b = inst.FB, d = inst.FD;
	if (inst.Rc){
		Default(inst); return;
	}
	ARMReg vB0 = fpr.R0(b);
	ARMReg vD0 = fpr.R0(d, false);
	ARMReg fpscrReg = gpr.GetReg();
	ARMReg V0 = D1;
	ARMReg rA = gpr.GetReg();

	MOVI2R(fpscrReg, (u32)&PPC_NAN);
	VLDR(V0, fpscrReg, 0);
	LDR(fpscrReg, R9, PPCSTATE_OFF(fpscr));

	VCMP(vB0);
	VMRS(_PC);
	FixupBranch Less0 = B_CC(CC_LT);
		VMOV(vD0, V0);	
		SetFPException(fpscrReg, FPSCR_VXSQRT);
		FixupBranch SkipOrr0 = B();
	SetJumpTarget(Less0);
		FixupBranch noException = B_CC(CC_EQ);
		SetFPException(fpscrReg, FPSCR_ZX);
		SetJumpTarget(noException);
	SetJumpTarget(SkipOrr0);

	VCVT(S0, vB0, 0);

	NEONXEmitter nemit(this);
	nemit.VRSQRTE(F_32, D0, D0);
	VCVT(vD0, S0, 0);

	STR(fpscrReg, R9, PPCSTATE_OFF(fpscr));
	gpr.Unlock(fpscrReg, rA);
}

