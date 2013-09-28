# lord

Online radio service commander using [mpd] as backend. Supported radios:

- [cmd.fm]
- [douban.fm]  Only in China?
- [jing.fm] Only in China?

## Commands

```
lord -h
lord status
lord kill

lord cmd listen [<genre>] [--no-daemon]
lord cmd genres

lord douban listen [<channel_id> | <musician>] [--no-daemon]
lord douban search <keywords>
lord douban hot
lord douban trending

lord jing listen <keywords> [--no-daemon]
```


## Don't like MPD？

You might be interested in

- [HadouRex]： [douban.fm] client using [mpg123] as backend
- [Jinkell]: [jing.fm] client using [mplayer] as backend

[cmd.fm]: http://cmd.fm
[douban.fm]: http://douban.fm
[jing.fm]: http://jing.fm
[last.fm]: http://last.fm
[MPD]: http://musicpd.org/
[MPD客户端]: http://mpd.wikia.com/wiki/Clients
[ArchWiki: MPD]: https://wiki.archlinux.org/index.php/Mpd
[faad2]: http://www.audiocoding.com/faad2.html
[mpg123]: http://www.mpg123.de/
[mplayer]: http://www.mplayerhq.hu/
[HadouRex]: http://github.com/rnons/HadouRex
[jinkell]: https://github.com/rnons/jinkell
