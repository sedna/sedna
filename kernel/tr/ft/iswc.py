#!/usr/bin/env python


wc_arr = [0 for x in range(0, 14000/8)]


def _iswordchar(ch):
	return (not (
		ch <= 33
		or ch == ord(';') or ch == ord('-') or ch == ord('"') or ch == ord('\'')
		or (ch >= 35 and ch <= 38)
		or (ch >= 40 and ch <= 44)
		or (ch >= 46 and ch <= 47)
		or ch == 58
		or (ch >= 60 and ch <= 63)
		or (ch >= 91 and ch <= 94)
		or ch == 96
		or ch == 124
		or (ch >= 126 and ch <= 169 and ch not in [131,138,140,142,154,156,158,159])
		or (ch >= 171 and ch <= 177)
		or ch == 180
		or (ch >= 182 and ch <= 184)
		or ch == 187
		or ch == 191
		or ch == 215
		or ch == 247
		or (ch >= 697 and ch <= 698)
		or (ch >= 706 and ch <= 719)
		or (ch >= 722 and ch <= 735)
		or (ch >= 741 and ch <= 749)
		or (ch >= 884 and ch <= 885)
		or ch == 894
		or (ch >= 900 and ch <= 901)
		or ch == 903
		or ch == 1154
		or (ch >= 1160 and ch <= 1161)
		or (ch >= 1370 and ch <= 1375)
		or (ch >= 1417 and ch <= 1418)
		or ch == 1470
		or ch == 1472
		or ch == 1475
		or (ch >= 1523 and ch <= 1524)
		or ch == 1548
		or ch == 1563
		or ch == 1567
		or (ch >= 1642 and ch <= 1645)
		or ch == 1748
		or (ch >= 1757 and ch <= 1758)
		or ch == 1769
		or (ch >= 1789 and ch <= 1790)
		or (ch >= 1792 and ch <= 1805)
		or ch == 1807
		or (ch >= 2404 and ch <= 2405)
		or ch == 2416
		or (ch >= 2546 and ch <= 2547)
		or ch == 2554
		or ch == 2928
		or ch == 3572
		or ch == 3647
		or ch == 3663
		or (ch >= 3674 and ch <= 3675)
		or (ch >= 3841 and ch <= 3863)
		or (ch >= 3866 and ch <= 3871)
		or ch == 3892
		or ch == 3894
		or ch == 3896
		or (ch >= 3898 and ch <= 3901)
		or ch == 3973
		or (ch >= 4030 and ch <= 4037)
		or (ch >= 4039 and ch <= 4044)
		or ch == 4047
		or (ch >= 4170 and ch <= 4175)
		or ch == 4347
		or (ch >= 4961 and ch <= 4968)
		or (ch >= 5741 and ch <= 5742)
		or ch == 5760
		or (ch >= 5787 and ch <= 5788)
		or (ch >= 5867 and ch <= 5869)
		or (ch >= 6100 and ch <= 6108)
		or (ch >= 6144 and ch <= 6158)
		or ch == 8125
		or (ch >= 8127 and ch <= 8129)
		or (ch >= 8141 and ch <= 8143)
		or (ch >= 8157 and ch <= 8159)
		or (ch >= 8173 and ch <= 8175)
		or (ch >= 8189 and ch <= 8190)
		or (ch >= 8192 and ch <= 8262)
		or (ch >= 8264 and ch <= 8269)
		or (ch >= 8298 and ch <= 8303)
		or (ch >= 8314 and ch <= 8318)
		or (ch >= 8330 and ch <= 8334)
		or (ch >= 8352 and ch <= 8367)
		or (ch >= 8413 and ch <= 8416)
		or (ch >= 8418 and ch <= 8419)
		or (ch >= 8448 and ch <= 8449)
		or (ch >= 8451 and ch <= 8454)
		or (ch >= 8456 and ch <= 8457)
		or ch == 8468
		or (ch >= 8470 and ch <= 8472)
		or (ch >= 8478 and ch <= 8483)
		or ch == 8485
		or ch == 8487
		or ch == 8489
		or ch == 8494
		or ch == 8498
		or ch == 8506
		or (ch >= 8592 and ch <= 8691)
		or (ch >= 8704 and ch <= 8945)
		or (ch >= 8960 and ch <= 9083)
		or (ch >= 9085 and ch <= 9114)
		or (ch >= 9216 and ch <= 9254)
		or (ch >= 9280 and ch <= 9290)
		or (ch >= 9372 and ch <= 9449)
		or (ch >= 9472 and ch <= 9621)
		or (ch >= 9632 and ch <= 9719)
		or (ch >= 9728 and ch <= 9747)
		or (ch >= 9753 and ch <= 9841)
		or (ch >= 9985 and ch <= 9988)
		or (ch >= 9990 and ch <= 9993)
		or (ch >= 9996 and ch <= 10023)
		or (ch >= 10025 and ch <= 10059)
		or ch == 10061
		or (ch >= 10063 and ch <= 10066)
		or ch == 10070
		or (ch >= 10072 and ch <= 10078)
		or (ch >= 10081 and ch <= 10087)
		or ch == 10132
		or (ch >= 10136 and ch <= 10159)
		or (ch >= 10161 and ch <= 10174)
		or (ch >= 10240 and ch <= 10495)
		or (ch >= 11904 and ch <= 11929)
		or (ch >= 11931 and ch <= 12019)
		or (ch >= 12032 and ch <= 12245)
		or (ch >= 12272 and ch <= 12283)
		or (ch >= 12288 and ch <= 12292)
		or (ch >= 12296 and ch <= 12320)
		or ch == 12336
		or (ch >= 12342 and ch <= 12343)
		or (ch >= 12350 and ch <= 12351)
		or (ch >= 12443 and ch <= 12444)
		or ch == 12539
		or (ch >= 12688 and ch <= 12689)
		or (ch >= 12694 and ch <= 12703)
		or (ch >= 12800 and ch <= 12828)
		or (ch >= 12842 and ch <= 12867)
		or (ch >= 12896 and ch <= 12923)
		or ch == 12927
		or (ch >= 12938 and ch <= 12976)
		or (ch >= 12992 and ch <= 13003)
		or (ch >= 13008 and ch <= 13054)
		or (ch >= 13056 and ch <= 13174)
		or (ch >= 13179 and ch <= 13277)
		or (ch >= 13280 and ch <= 13310)
		))


for b in range(0,len(wc_arr)):
   for bit in range(0,8):
      if _iswordchar(b*8 + bit):
         wc_arr[b] |= 1 << bit



f = open("iswordchar.inc", "wt")
print >>f, "static const char _iswordchar_table[%s] = {" % len(wc_arr)
for i in range(0, len(wc_arr)):
  if i>0 and i % 16 == 0:
    print >>f
  f.write("0x%x" % wc_arr[i])
  if i+1 < len(wc_arr):
     f.write(",")
print >>f, "};"


print >>f, "static inline bool iswordchar(int ch)"
print >>f, "{"
print >>f, "    if (ch == SednaConvertJob::closetag_code || ch == SednaConvertJob::opentag_code)"
print >>f, "        return false;"
print >>f, "    if (ch >= %s)" % (len(wc_arr)*8)
print >>f, "        return true;"
print >>f, "    return ((_iswordchar_table[ch >> 3] & (1 << (ch & 7))) != 0);"
print >>f, "}"

f.close()



