/**
 * Copyright 2013 Dolphin Emulator Project
 * Licensed under GPLv2
 * Refer to the license.txt file included.
 */

package org.dolphinemu.dolphinemu.gamelist;

import android.app.Activity;
import android.app.ListFragment;
import android.content.Intent;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ListView;
import android.widget.Toast;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.dolphinemu.dolphinemu.NativeLibrary;
import org.dolphinemu.dolphinemu.R;
import org.dolphinemu.dolphinemu.emulation.EmulationActivity;


/**
 * The {@link ListFragment} responsible for displaying the game list.
 */
public final class GameListFragment extends ListFragment
{
	private GameListAdapter mGameAdapter;
	private static GameListActivity mMe;
	private OnGameListZeroListener mCallback;

	/**
	 * Interface that defines how to handle the case
	 * when there are zero games in the game list.
	 */
	public interface OnGameListZeroListener
	{
		/**
		 * This is called when there are no games
		 * currently present within the game list.
		 */
		void onZeroFiles();
	}

	/**
	 * Clears all entries from the {@link GameListAdapter}
	 * backing this GameListFragment.
	 */
	public void clearGameList()
	{
		mGameAdapter.clear();
		mGameAdapter.notifyDataSetChanged();
	}

	private void Fill()
	{
		List<GameListItem> fls = new ArrayList<GameListItem>();
		String Directories = NativeLibrary.GetConfig("Dolphin.ini", "General", "GCMPathes", "0");
		int intDirectories = Integer.parseInt(Directories);

		// Extensions to filter by.
		Set<String> exts = new HashSet<String>(Arrays.asList(".dff", ".dol", ".elf", ".gcm", ".gcz", ".iso", ".wad", ".wbfs"));

		for (int a = 0; a < intDirectories; ++a)
		{
			String BrowseDir = NativeLibrary.GetConfig("Dolphin.ini", "General", "GCMPath" + a, "");
			File currentDir = new File(BrowseDir);
			File[] dirs = currentDir.listFiles();
			try
			{
				for (File entry : dirs)
				{
					String entryName = entry.getName();

					if (!entry.isHidden() && !entry.isDirectory())
					{
						if (exts.contains(entryName.toLowerCase().substring(entryName.lastIndexOf('.'))))
							fls.add(new GameListItem(mMe, entryName, String.format(getString(R.string.file_size), entry.length()), entry.getAbsolutePath()));
					}
				}
			}
			catch (Exception ignored)
			{
			}
		}
		Collections.sort(fls);

		mGameAdapter = new GameListAdapter(mMe, R.layout.gamelist_folderbrowser_list, fls);
		setListAdapter(mGameAdapter);

		if (fls.isEmpty())
		{
			mCallback.onZeroFiles();
		}
	}

	@Override
	public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState)
	{
		View rootView = inflater.inflate(R.layout.gamelist_listview, container, false);
		ListView mMainList = (ListView) rootView.findViewById(R.id.gamelist);

		Fill();

		return mMainList;
	}
	
	@Override
	public void onListItemClick(ListView listView, View view, int position, long id)
	{
		GameListItem item = mGameAdapter.getItem(position);

		// Show a toast indicating which game was clicked.
		Toast.makeText(mMe, String.format(getString(R.string.file_clicked), item.getPath()), Toast.LENGTH_SHORT).show();

		// Start the emulation activity and send the path of the clicked ROM to it.
		Intent intent = new Intent(mMe, EmulationActivity.class);
		intent.putExtra("SelectedGame", item.getPath());
		mMe.startActivity(intent);
	}

	@Override
	public void onAttach(Activity activity)
	{
		super.onAttach(activity);

		// This makes sure that the container activity has implemented
		// the callback interface. If not, it throws an exception
		try
		{
			mCallback = (OnGameListZeroListener) activity;
			mMe = (GameListActivity) activity;
		}
		catch (ClassCastException e)
		{
			throw new ClassCastException(activity.toString()
					+ " must implement OnGameListZeroListener");
		}
	}
}
