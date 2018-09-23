# repoScan

This is a small script to walk through the repository of an organization on GitHub and write the compliance relevant information to a folder.

This folder then can be handled via git to see the changes since the last scan.

# How to use

1. create a new folder and initialize git
2. `$ git remote add repoScan git@github.com:maxhbr/repoScan.git`
3. `$ git subtree add --prefix repoScan repoScan master`
4. create a file `/update.sh` with the following content:
  ```
  #!/usr/bin/env bash
  
  ghuser=$1
  ghtoken=$2
  org=[[org]]
  
  cd $(dirname $0)/repoScan
  stack build
  stack exec repoScan "$ghuser" "$ghtoken" "$org" ../

  git add -- "../$org/*.{json,csv}"
  git commit -m "Automatic sync @ $(date --iso-8601)" -- "../$org/*.{json,csv}"
  ```
  where you have to fill in `[[org]]`.
  
One can later update `repoScan` via:
```
$ git subtree pull --prefix repoScan repoScan master
```

### This project is licensed under BSD3
See `./LICENSE`.
