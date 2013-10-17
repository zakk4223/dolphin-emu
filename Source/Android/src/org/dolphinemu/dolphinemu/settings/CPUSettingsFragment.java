/**
 * Copyright 2013 Dolphin Emulator Project
 * Licensed under GPLv2
 * Refer to the license.txt file included.
 */

package org.dolphinemu.dolphinemu.settings;

import org.dolphinemu.dolphinemu.R;

import android.os.Build;
import android.os.Bundle;
import android.preference.ListPreference;
import android.preference.PreferenceFragment;

/**
 * Responsible for the loading of the CPU preferences.
 */
public final class CPUSettingsFragment extends PreferenceFragment
{
	@Override
	public void onCreate(Bundle savedInstanceState)
	{
		super.onCreate(savedInstanceState);

		// Load the preferences from an XML resource
		addPreferencesFromResource(R.xml.cpu_prefs);

		final ListPreference cpuCores = (ListPreference) findPreference("cpuCorePref");

		//
		// Set valid emulation cores depending on the CPU architecture
		// that the Android device is running on.
		//
		if (Build.CPU_ABI.contains("x86"))
		{
			cpuCores.setEntries(R.array.emuCoreEntriesX86);
			cpuCores.setEntryValues(R.array.emuCoreValuesX86);
		}
		else if (Build.CPU_ABI.contains("arm"))
		{
			cpuCores.setEntries(R.array.emuCoreEntriesARM);
			cpuCores.setEntryValues(R.array.emuCoreValuesARM);
		}
		else
		{
			cpuCores.setEntries(R.array.emuCoreEntriesOther);
			cpuCores.setEntryValues(R.array.emuCoreValuesOther);
		}
	}
}
