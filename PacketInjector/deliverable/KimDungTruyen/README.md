## Cài Đặt

### 1. Install Haskell platform

Win 32bit:
https://haskell.org/platform/download/8.0.2/HaskellPlatform-8.0.2-a-minimal-i386-setup.exe

Win 64bit:
https://haskell.org/platform/download/8.0.2/HaskellPlatform-8.0.2-a-minimal-x86_64-setup.exe

### 2. Install missing packages

> cabal update

> cabal install http-conduit base16-bytestring split aeson-pretty

----------


## Hướng dẫn sử dụng

Để mở file .hs (Crosswar.hs, Play.hs, Hunt.hs), có thể dùng WinGHCi (có giao diện), hoặc double click vào file.

### 1. Add clone và Acc chính

Lưu ý, acc được add vào phần mềm sẽ vào server mặc định dựa vào lần cuối log vào game.

**Lệnh add clone buff xu (CrossWar.hs)**

> adb "username" "password"

**Lệnh add nhân vật chính cược xu (CrossWar.hs)**

> adp "username" "password"

### 2. Chỉnh kèo

**Lựa chọn kèo (Match.json)**

Chỉnh kèo thắng bằng cách thay thế mã số người chơi trong dòng "win" và kèo thua vào "lose". 
 Toàn bộ acc chính sẽ đặt vào nhân vật trong dòng win. Toàn bộ clone trong Buffs.json sẽ đặt vào nhân vật trong trong lose.
 
    {
	    "mid": "1",
	    "win": "736",
	    "lose": "73000185"
    }
**Chỉnh số xu đặt (Players.json và Buffs.json)**

Chỉnh số xu cược của từng acc bằng cách mở file Players (acc chính) hoặc Buffs (acc clone) để chỉnh. Số xu nằm ở dòng "amount".

0 : đặt 100 xu.
1 : đặt 1000 xu.
2 : đặt 2000 xu.
3: đặt 3000 xu.
... (theo cấp số nhân x1000)

Ví dụ acc sau sẽ cược 50k xu:

    {
        "amount": 50,
        "displayNovice": 1,
        "chNumber": "118000019",
        "uid": "1254848116",
        "key": "4b91b2cf558602fc21f9d379ff982a86",
        "defaultsid": "118",
        "opname": "10",
        "create_time": 1503040225,
        "acc": "reply1989"
    }

**Cược xu (CrossWar.hs)**

Mở file CrossWar.hs bằng GHCI hoặc double click
Canh giờ chính xác.

Đặt cùng lúc bằng cách bấm nút Play màu đỏ của GHCI hoặc gõ lệnh

> main


**Phần phụ**

Chỉ đặt kèo thắng:
>winBet

Chỉ đặt kèo thua:
>loseBet