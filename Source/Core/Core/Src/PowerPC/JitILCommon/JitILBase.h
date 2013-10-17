// Copyright 2013 Dolphin Emulator Project
// Licensed under GPLv2
// Refer to the license.txt file included.

#ifndef _JITILBASE_H
#define _JITILBASE_H

#include "../PPCAnalyst.h"
#include "IR.h"
#include "../JitCommon/JitBase.h"

#define INSTRUCTION_START

#define JITDISABLE(setting) \
	if (Core::g_CoreStartupParameter.bJITOff || \
		Core::g_CoreStartupParameter.setting) \
		{Default(inst); return;}

class JitILBase : public JitBase
{
protected:
	// The default code buffer. We keep it around to not have to alloc/dealloc a
	// large chunk of memory for each recompiled block.
	PPCAnalyst::CodeBuffer code_buffer;
public:
	JitILBase() : code_buffer(32000) {}
	~JitILBase() {}

	IREmitter::IRBuilder ibuild;

	virtual JitBaseBlockCache *GetBlockCache() = 0;

	virtual void Jit(u32 em_address) = 0;

	virtual	const u8 *BackPatch(u8 *codePtr, u32 em_address, void *ctx) = 0;

	virtual const CommonAsmRoutinesBase *GetAsmRoutines() = 0;

	virtual bool IsInCodeSpace(u8 *ptr) = 0;
	
	// OPCODES
	virtual void unknown_instruction(UGeckoInstruction inst) = 0;
	virtual void Default(UGeckoInstruction inst) = 0;
	virtual void DoNothing(UGeckoInstruction inst) = 0;
	virtual void HLEFunction(UGeckoInstruction inst) = 0;

	virtual void DynaRunTable4(UGeckoInstruction _inst) = 0;
	virtual void DynaRunTable19(UGeckoInstruction _inst) = 0;
	virtual void DynaRunTable31(UGeckoInstruction _inst) = 0;
	virtual void DynaRunTable59(UGeckoInstruction _inst) = 0;
	virtual void DynaRunTable63(UGeckoInstruction _inst) = 0;

	// Branches
	void sc(UGeckoInstruction inst);
	void rfi(UGeckoInstruction inst);
	void bx(UGeckoInstruction inst);
	void bcx(UGeckoInstruction inst);
	void bcctrx(UGeckoInstruction inst);
	void bclrx(UGeckoInstruction inst);

	// LoadStore
	void lXzx(UGeckoInstruction inst);
	void lhax(UGeckoInstruction inst);
	void stXx(UGeckoInstruction inst);
	void lmw(UGeckoInstruction inst);
	void stmw(UGeckoInstruction inst);
	void stX(UGeckoInstruction inst); //stw sth stb
	void lXz(UGeckoInstruction inst);
	void lbzu(UGeckoInstruction inst);
	void lha(UGeckoInstruction inst);

	// System Registers
	void mtspr(UGeckoInstruction inst);
	void mfspr(UGeckoInstruction inst);
	void mtmsr(UGeckoInstruction inst);
	void mfmsr(UGeckoInstruction inst);
	void mftb(UGeckoInstruction inst);
	void mtcrf(UGeckoInstruction inst);
	void mfcr(UGeckoInstruction inst);
	void mcrf(UGeckoInstruction inst);
	void crXX(UGeckoInstruction inst);

	void dcbst(UGeckoInstruction inst);
	void dcbz(UGeckoInstruction inst);
	void icbi(UGeckoInstruction inst);

	void addx(UGeckoInstruction inst);
	void boolX(UGeckoInstruction inst);
	void mulli(UGeckoInstruction inst);
	void mulhwux(UGeckoInstruction inst);
	void mullwx(UGeckoInstruction inst);
	void divwux(UGeckoInstruction inst);
	void srawix(UGeckoInstruction inst);
	void srawx(UGeckoInstruction inst);
	void addex(UGeckoInstruction inst);
	void addzex(UGeckoInstruction inst);

	void extsbx(UGeckoInstruction inst);
	void extshx(UGeckoInstruction inst);
	
	void reg_imm(UGeckoInstruction inst);

	void ps_sel(UGeckoInstruction inst);
	void ps_mr(UGeckoInstruction inst);
	void ps_sign(UGeckoInstruction inst); //aggregate
	void ps_arith(UGeckoInstruction inst); //aggregate
	void ps_mergeXX(UGeckoInstruction inst);
	void ps_maddXX(UGeckoInstruction inst);
	void ps_rsqrte(UGeckoInstruction inst);
	void ps_sum(UGeckoInstruction inst);
	void ps_muls(UGeckoInstruction inst);

	void fp_arith_s(UGeckoInstruction inst);

	void fcmpx(UGeckoInstruction inst);
	void fmrx(UGeckoInstruction inst);

	void cmpXX(UGeckoInstruction inst);

	void cntlzwx(UGeckoInstruction inst);

	void lfs(UGeckoInstruction inst);
	void lfd(UGeckoInstruction inst);
	void stfd(UGeckoInstruction inst);
	void stfs(UGeckoInstruction inst);
	void stfsx(UGeckoInstruction inst);
	void psq_l(UGeckoInstruction inst);
	void psq_st(UGeckoInstruction inst);

	void fmaddXX(UGeckoInstruction inst);
	void fsign(UGeckoInstruction inst);
	void rlwinmx(UGeckoInstruction inst);
	void rlwimix(UGeckoInstruction inst);
	void rlwnmx(UGeckoInstruction inst);
	void negx(UGeckoInstruction inst);
	void slwx(UGeckoInstruction inst);
	void srwx(UGeckoInstruction inst);
	void lfsx(UGeckoInstruction inst);

	void subfic(UGeckoInstruction inst);
	void subfcx(UGeckoInstruction inst);
	void subfx(UGeckoInstruction inst);
	void subfex(UGeckoInstruction inst);	

};
#endif
