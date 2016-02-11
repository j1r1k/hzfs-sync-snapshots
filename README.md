# ZFS Sync Snapshots

This is helper utility to determine which snapshots should be send to remote zpool in order to have two zpools synchronized.

It is tailored to suit my snapshot naming scheme which includes timestamp. Format is `date +"name-%F-%T"` where name is up to you.

Haskell part does the parsing and matching only. Synchronization is executed via helper script located in `scripts/zfs-sync`

# Installation

Clone the repository

```
git clone https://github.com/j1r1k/hzfs-sync-snapshots.git
```

Install haskell binary using stack
```
cd hzfs-sync-snapshots
stack install
```

Optionally copy helper script to standard location
```
sudo cp scripts/zfs-sync /usr/local/sbin
```

# Usage
```
zfs import backup
zfs import backup-external

zfs-sync backup backup-external

zpool export backup
zpool export backup-external
```
