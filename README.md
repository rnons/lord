# lord

Lord is a unified command line interface to several online radios.

By default, lord uses [mpd] as backend.
When mpd is available, lord can be run as daemon (default) or in foreground (with `--no-daemon` option).
When mpd is unavailable (not running), it will fallback to use [mplayer]. Mplayer is run in to enable user control playback with mplayer keybindings.

Supported radios:

- [8tracks.com]
- [cmd.fm]
- [radioreddit.com]
- [douban.fm]  Only in China?
- [jing.fm] Only in China?

## Commands

```
lord -h
lord status
lord kill

lord 8tracks listen [<mix_id> | <mix_url>] [--no-daemon]
lord 8tracks search <keywords>
lord 8tracks [featured | trending | newest]

lord cmd listen <genre> [--no-daemon]
lord cmd genres

lord douban listen [<channel_id> | <musician>] [--no-daemon]
lord douban search <keywords>
lord douban [hot | trending]

lord jing listen <keywords> [--no-daemon]

lord reddit listen <genre> [--no-daemon]
lord reddit genres
```

[8tracks.com]: http://8tracks.com
[cmd.fm]: http://cmd.fm
[radioreddit.com]: http://radioreddit.com
[douban.fm]: http://douban.fm
[jing.fm]: http://jing.fm
[MPD]: http://musicpd.org/
[mplayer]: http://www.mplayerhq.hu/
