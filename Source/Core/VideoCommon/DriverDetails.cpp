// Copyright 2013 Dolphin Emulator Project
// Licensed under GPLv2
// Refer to the license.txt file included.

#include <map>

#include "LogManager.h"
#include "DriverDetails.h"

namespace DriverDetails
{
	struct BugInfo
	{
		Vendor m_vendor; // which vendor has the error
		Driver m_driver; // which driver has the error
		Bug m_bug; // Which bug it is
		double m_versionstart; // When it started
		double m_versionend; // When it ended
		bool m_hasbug; // Does it have it?
	};

	// Local members
	Vendor	m_vendor = VENDOR_UNKNOWN;
	Driver	m_driver = DRIVER_UNKNOWN;
	double	m_version = 0.0;

	// This is a list of all known bugs for each vendor
	// We use this to check if the device and driver has a issue
	BugInfo m_known_bugs[] = {
		{VENDOR_QUALCOMM,  DRIVER_QUALCOMM_3XX, BUG_NODYNUBOACCESS,     14.0, -1.0, true},
		{VENDOR_QUALCOMM,  DRIVER_QUALCOMM_3XX, BUG_BROKENCENTROID,     14.0, -1.0, true},
		{VENDOR_QUALCOMM,  DRIVER_QUALCOMM_3XX, BUG_BROKENINFOLOG,      -1.0, -1.0, true},
		{VENDOR_QUALCOMM,  DRIVER_QUALCOMM_3XX, BUG_ANNIHILATEDUBOS,	41.0, 46.0, true},
		{VENDOR_QUALCOMM,  DRIVER_QUALCOMM_3XX, BUG_BROKENSWAP,		-1.0, -1.0, true},
		{VENDOR_MESA,      DRIVER_NOUVEAU,      BUG_BROKENUBO,           900,  916, true},
		{VENDOR_MESA,      DRIVER_R600,         BUG_BROKENUBO,           900,  913, true},
		{VENDOR_MESA,      DRIVER_I965,         BUG_BROKENUBO,           900,  920, true},
		{VENDOR_ATI,       DRIVER_ATI,          BUG_BROKENHACKEDBUFFER, -1.0, -1.0, true},
		{VENDOR_MESA,      DRIVER_NOUVEAU,      BUG_BROKENHACKEDBUFFER, -1.0, -1.0, true},
		{VENDOR_ATI,       DRIVER_ATI,          BUG_BROKENPINNEDMEMORY, -1.0, -1.0, true},
		{VENDOR_TEGRA,     DRIVER_NVIDIA,       BUG_ISTEGRA,            -1.0, -1.0, true},
		{VENDOR_IMGTEC,    DRIVER_IMGTEC,       BUG_ISPOWERVR,          -1.0, -1.0, true},
	};

	std::map<Bug, BugInfo> m_bugs;

	void Init(Vendor vendor, Driver driver, const double version)
	{
		m_vendor = vendor;
		m_driver = driver;
		m_version = version;
		
		if (driver == DRIVER_UNKNOWN)
			switch(vendor)
			{
				case VENDOR_NVIDIA:
				case VENDOR_TEGRA:
					m_driver = DRIVER_NVIDIA;
				break;
				case VENDOR_ATI:
					m_driver = DRIVER_ATI;
				break;
				case VENDOR_INTEL:
					m_driver = DRIVER_INTEL;
				break;
				case VENDOR_IMGTEC:
					m_driver = DRIVER_IMGTEC;
				break;
				case VENDOR_VIVANTE:
					m_driver = DRIVER_VIVANTE;
				break;
				default:
				break;
			}
			
		for(unsigned int a = 0; a < (sizeof(m_known_bugs) / sizeof(BugInfo)); ++a)
		{
			if(
				( m_known_bugs[a].m_vendor == m_vendor || m_known_bugs[a].m_vendor == VENDOR_ALL ) &&
				( m_known_bugs[a].m_driver == m_driver || m_known_bugs[a].m_driver == DRIVER_ALL ) &&
				( m_known_bugs[a].m_versionstart <= m_version || m_known_bugs[a].m_versionstart == -1 ) &&
				( m_known_bugs[a].m_versionend > m_version || m_known_bugs[a].m_versionend == -1 )
			)
				m_bugs.insert(std::make_pair(m_known_bugs[a].m_bug, m_known_bugs[a]));
		}
	}

	bool HasBug(Bug bug)
	{
		auto it = m_bugs.find(bug);
		if (it == m_bugs.end())
			return false;	
		return it->second.m_hasbug;
	}
}
