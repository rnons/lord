# lord

将[cmd.fm]，[douban.fm]和[jing.fm] (甚至更多的网络电台) 整合到一个框架下。后端使用[MPD]播放。

MPD有两大优势：占用资源小；客户端丰富。[MPD客户端]有图形界面，也有命令行界面，还可以运行在Android和iOS上，从而用手机遥控douban.fm和jing.fm。MPD客户端可以方便的实现搜索歌词、搜索专辑信息、搜索歌手信息，还可以同步收听纪录到[last.fm]。

## 安装配置

### 安装

    cabal install

安装完成后，可以在`~/.cabal/bin`路径下找到名为 **lord** 的可执行文件。

### 配置MPD

可以参考[ArchWiki: MPD]

### 收听[jing.fm]需要进行的配置

在`~`路径下新建`.lord`文件夹，并将其软链接到MPD的music文件夹。

比如MPD的music directory位于`~/music`，则需要执行
    
    ln -s ~/.lord ~/music/lord

## 使用lord

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

lord的操作方式非常简单，稍微需要注意的是

- 收听douban.fm时，listen后面的参数可以是电台id，也可以是歌手/乐队名，如

        lord douban listen 1002316
        lord douban listen "Sigur Ros"

- 收听jing.fm时，需要登录，登录成功后，token会被保存到`~/.lord/lord.yml`文件里，这样就不用每次都登录。这个token不是你的用户名、密码，它的有效期只有一至两周。所以如果在收听一段时间后突然无法收听，则需要手动删除`~/.lord/lord.yml`文件，然后lord会提示重新登录。
- 如果网络条件不好可以编辑`~/.lord/lord.yml`文件，将highquality的值设为false，收听普通音质。

## 故障排除

运行`mpd --version`，检查 **Decoders plugins** 里有没有 **mp4ff** 这一项，比如我的是这样的：

```
Decoders plugins:
 [mad] mp3 mp2
 [dsdiff] dff
 [dsf] dsf
 [faad] aac
 [mp4ff] m4a m4b mp4
 [ffmpeg] ... ... ... ...
 [pcm]
```

如果不幸没有 **mp4ff** 这一项，那你和我一样 (我用的Fedora)，需要手动编译 [faad2]和[mpd]，否则无法正常收听jing.fm

**注**: Arch Linux没有这个问题。douban.fm不受这个问题影响。

**再注**: 遇到问题时，可以通过`mpd --no-daemon --stdout`在前台运行mpd。提交bug时，也请把stdout的输出信息附上。

## 觉得MPD配置太麻烦？

可以试试这两个工具：

- [HadouRex]： 后端使用[mpg123]的douban.fm客户端
- [jinkell]：后端使用[mplayer]的jing.fm客户端

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
