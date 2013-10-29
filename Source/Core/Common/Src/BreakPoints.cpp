// Copyright 2013 Dolphin Emulator Project
// Licensed under GPLv2
// Refer to the license.txt file included.

#include "Common.h"
#include "DebugInterface.h"
#include "BreakPoints.h"
#include "../../Core/Src/PowerPC/JitCommon/JitBase.h"

#include <sstream>
#include <algorithm>

bool BreakPoints::IsAddressBreakPoint(u32 _iAddress)
{
	for (auto& bp : m_BreakPoints)
		if (bp.iAddress == _iAddress)
			return true;
	return false;
}

bool BreakPoints::IsTempBreakPoint(u32 _iAddress)
{
	for (auto& bp : m_BreakPoints)
		if (bp.iAddress == _iAddress && bp.bTemporary)
			return true;
	return false;
}

BreakPoints::TBreakPointsStr BreakPoints::GetStrings() const
{
	TBreakPointsStr bps;
	for (const auto& bp : m_BreakPoints)
	{
		if (!bp.bTemporary)
		{
			std::stringstream ss;
			ss << std::hex << bp.iAddress << " " << (bp.bOn ? "n" : "");
			bps.push_back(ss.str());
		}
	}

	return bps;
}

void BreakPoints::AddFromStrings(const TBreakPointsStr& bps)
{
	for (const auto& bps_i : bps)
	{
		TBreakPoint bp;
		std::stringstream bpstr;
		bpstr << std::hex << bps_i;
		bpstr >> bp.iAddress;
		bp.bOn = bps_i.find("n") != bps_i.npos;
		bp.bTemporary = false;
		Add(bp);
	}
}

void BreakPoints::Add(const TBreakPoint& bp)
{
	if (!IsAddressBreakPoint(bp.iAddress))
	{
		m_BreakPoints.push_back(bp);
		if (jit)
			jit->GetBlockCache()->InvalidateICache(bp.iAddress, 4);
	}
}

void BreakPoints::Add(u32 em_address, bool temp)
{
	if (!IsAddressBreakPoint(em_address)) // only add new addresses
	{
		TBreakPoint pt; // breakpoint settings
		pt.bOn = true;
		pt.bTemporary = temp;
		pt.iAddress = em_address;

		m_BreakPoints.push_back(pt);

		if (jit)
			jit->GetBlockCache()->InvalidateICache(em_address, 4);
	}
}

void BreakPoints::Remove(u32 em_address)
{
	for (TBreakPoints::iterator i = m_BreakPoints.begin(); i != m_BreakPoints.end(); ++i)
	{
		if (i->iAddress == em_address)
		{
			m_BreakPoints.erase(i);
			if (jit)
				jit->GetBlockCache()->InvalidateICache(em_address, 4);
			return;
		}
	}
}

void BreakPoints::Clear()
{
	if (jit)
	{
		std::for_each(m_BreakPoints.begin(), m_BreakPoints.end(),
			[](const TBreakPoint& bp)
			{
				jit->GetBlockCache()->InvalidateICache(bp.iAddress, 4);
			}
		);
	}
	
	m_BreakPoints.clear();
}

MemChecks::TMemChecksStr MemChecks::GetStrings() const
{
	TMemChecksStr mcs;
	for (const auto& bp : m_MemChecks)
	{
		std::stringstream mc;
		mc << std::hex << bp.StartAddress;
		mc << " " << (bp.bRange ? bp.EndAddress : bp.StartAddress) << " " <<
			(bp.bRange ? "n" : "") << (bp.OnRead ? "r" : "") <<
			(bp.OnWrite ? "w" : "") << (bp.Log ? "l" : "") << (bp.Break ? "p" : "");
		mcs.push_back(mc.str());
	}

	return mcs;
}

void MemChecks::AddFromStrings(const TMemChecksStr& mcs)
{
	for (const auto& mcs_i : mcs)
	{
		TMemCheck mc;
		std::stringstream mcstr;
		mcstr << std::hex << mcs_i;
		mcstr >> mc.StartAddress;
		mc.bRange	= mcs_i.find("n") != mcs_i.npos;
		mc.OnRead	= mcs_i.find("r") != mcs_i.npos;
		mc.OnWrite	= mcs_i.find("w") != mcs_i.npos;
		mc.Log		= mcs_i.find("l") != mcs_i.npos;
		mc.Break	= mcs_i.find("p") != mcs_i.npos;
		if (mc.bRange)
			mcstr >> mc.EndAddress;
		else
			mc.EndAddress = mc.StartAddress;
		Add(mc);
	}
}

void MemChecks::Add(const TMemCheck& _rMemoryCheck)
{
	if (GetMemCheck(_rMemoryCheck.StartAddress) == 0)
		m_MemChecks.push_back(_rMemoryCheck);
}

void MemChecks::Remove(u32 _Address)
{
	for (TMemChecks::iterator i = m_MemChecks.begin(); i != m_MemChecks.end(); ++i)
	{
		if (i->StartAddress == _Address)
		{
			m_MemChecks.erase(i);
			return;
		}
	}
}

TMemCheck *MemChecks::GetMemCheck(u32 address)
{
	for (auto& bp : m_MemChecks)
	{
		if (bp.bRange)
		{
			if (address >= bp.StartAddress && address <= bp.EndAddress)
				return &(bp);
		}
		else if (bp.StartAddress == address)
			return &(bp);
	}

	// none found
	return 0;
}

void TMemCheck::Action(DebugInterface *debug_interface, u32 iValue, u32 addr,
						bool write, int size, u32 pc)
{
	if ((write && OnWrite) || (!write && OnRead))
	{
		if (Log)
		{
			INFO_LOG(MEMMAP, "CHK %08x (%s) %s%i %0*x at %08x (%s)",
				pc, debug_interface->getDescription(pc).c_str(),
				write ? "Write" : "Read", size*8, size*2, iValue, addr,
				debug_interface->getDescription(addr).c_str()
				);
		}
		if (Break)
			debug_interface->breakNow();
	}
}
