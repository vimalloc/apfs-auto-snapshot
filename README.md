# apfs-auto-snapshot
Tool to automatically create and delete APFS snapshots on a sliding timeline.

# Shows Stopping Limitation
Something (I'm guessing Time Machine) deletes snapshots out from under us. As
`tmutil` seems to be the only way to make APFS snapshots at the moment, there
isn't much that can be done about this I fear. See [#1](/../../issues/1) for
the details. 

As it stands, I can't get more then 4 or 5 snapshots at a time
before the invisible hand steps in and starts deleting older backups.
Unless this is fixed or we find a new way around this, the sliding window
feature of this project is basically unusable, and it would be simplier to
just cron new snapshot creations with `tmutil`. Just don't expect them to
stick around.
  
### Current Status
Until the show stopping limitations get fix, this project is basically stalled. 
However, if we can get around that issue, development on this will continue in
full force. The project is not solidified at all, everything in it is prone to
change going forward.

### Gotchas
When using APFS snapshots, if you delete a file and it is still in your snapshot,
you will not get the space back from that file until all the snapshots that contain
that file have been deleted. You can manually delete snapshots using `tmutil`.
Perhaps there is a way to mount snapshots as Read/Write (like in BTRFS) so you
can remove only the deisred file instead of the entire snapshot, but I haven't
seen any information about this as of yet.

### Contributing
Contributions are 100% welcome! I am ususally hanging out on freenode, feel
free to PM me (vimalloc) if you want to discuss anything :)

### Running
* Make sure haskell is installed
* Clone this repo somewhere
* Wherever you cloned the repo, run `cabal sandbox init`
* Install the dependencies with `cabal install --only-dependencies`
* Run `cabal build` or `cabal run`

You can restore files from the created snapshots by using time machine.


If you want to have this work on a schedule, here is a quick and dirty crontab
you could use until something better is put in place. Note that this may
break out from under you as development continues on this project:
```cron
# NOTE: Make sure you properly set APFS_AUTO_SNAPSHOT_DIR to wherever you cloned
#       this project to

APFS_AUTO_SNAPSHOT_DIR=/Users/lgbland/code/apfs-auto-snapshot

# Run on the first day of the year at midnight
0 0 1 1 * cd $APFS_AUTO_SNAPSHOT_DIR; dist/build/apfs-auto-snapshot/apfs-auto-snapshot --quater-hourly --hourly --daily --monthly --yearly > /tmp/autosnap.log 2>&1

# Run on the first day of the month for every month except january first (handled by yearly)
0 0 2-12 1 * cd $APFS_AUTO_SNAPSHOT_DIR; dist/build/apfs-auto-snapshot/apfs-auto-snapshot --quater-hourly --hourly --daily --monthly > /tmp/autosnap.log 2>&1

# Run weekly on sunday
0 0 * * 0 cd $APFS_AUTO_SNAPSHOT_DIR; dist/build/apfs-auto-snapshot/apfs-auto-snapshot --quater-hourly --hourly --daily --weekly > /tmp/autosnap.log 2>&1

# Run daily at midnight, except for sunday as that is handled above by weekly
0 0 * * 1-6  cd $APFS_AUTO_SNAPSHOT_DIR; dist/build/apfs-auto-snapshot/apfs-auto-snapshot --quater-hourly --hourly --daily > /tmp/autosnap.log 2>&1

# Run hourly, excpet at midnight which is handled by one of the above jobs
0 1-23 * * * cd $APFS_AUTO_SNAPSHOT_DIR; dist/build/apfs-auto-snapshot/apfs-auto-snapshot --quater-hourly --hourly > /tmp/autosnap.log 2>&1

# This will run on only the quater (that isn't already handled by an above job)
15,30,45 * * * * cd $APFS_AUTO_SNAPSHOT_DIR; dist/build/apfs-auto-snapshot/apfs-auto-snapshot --quater-hourly > /tmp/autosnap.log 2>&1
```
