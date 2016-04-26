# YSUACM Website

Build with:

```
cabal install --only-dependencies
cabal build
./preview.sh
```

...from your clone of the repo, then go to http://localhost:8000/.

Changes to `site.hs` or `Meetings.hs` require a fresh `cabal build`. Changes
to HTML and CSS files do not and will automatically be built while the preview
server is running.

## To add meetings

Edit `Meetings.hs` and add them to the `upcomingEvents` list. Don't remove
old meetings from this list, as this list is what generates the archive as well.

# License

BSD-2, see `LICENSE`
