-module(gbk).
-export([init/0, get_pinyin/1]).

-record(gbk_table, { name, value }).

init() ->
    GB2312 = #gbk_table { name = gb2312, value = 
	      "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabbbbbbbbbbbbbp" ++
	      "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbpbbbbbbbbbbbbbbbbbb" ++
	      "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" ++
	      "pbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" ++
	      "bbbbbbbbbbbbbbbbbbbbcccccccccccccccccccccccccccccc" ++
	      "ccccccccccccccccccccccccccccccccccczcccccccccccccc" ++
	      "ccccccccccccccccccccccccccccccccccccsccccccccccccc" ++
	      "cccccccccccccccccccccccccccccccccccccccccczccccccc" ++
	      "cccccccccccccccccccccccccccccccccccccccccccccccccc" ++
	      "cccddddddddddddddddddddddddddddddddddddddddddddddd" ++
	      "dddddddddddddddddddddzdddddddddddddddddddddddddddd" ++
	      "dddddddddddddddddddddddddddddddtdddddddddddddddddd" ++
	      "dddddddddddddddddddddddddddddddddddddeeeeeeeeeeeee" ++
	      "eeeeeeeeefffffffffffffffffffffffffffffffffffffffff" ++
	      "ffffffffffffffffffffffffffffffffffffffffffffffffff" ++
	      "fffffffffffffpffffffffffffffffffffgggggggggggggggg" ++
	      "ggggggggggggggggggghggggggggggggghgggggggggggggggg" ++
	      "gggggggggggggggggggggggggggggggggggggggggggggggggg" ++
	      "ggggggggggggggggggggggggggggggggggggggghhhhhhhhhhh" ++
	      "hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhmhhhhhhhhhhh" ++
	      "hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh" ++
	      "hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh" ++
	      "hhhhhhhhhhhhhhhhhhhhjjjjjjjjjjjjjjjjjjjjjjjjjjjjjj" ++
	      "jjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjj" ++
	      "jjjjjjjjjjjjjjkjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjj" ++
	      "jjjjjjjyjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjj" ++
	      "jjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjj" ++
	      "jjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjj" ++
	      "jjjjjjjjjjjjjjjkkkgkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkh" ++
	      "kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk" ++
	      "kkkkkkkkkkkkkkklllllllllllllllllllllllllllllllllll" ++
	      "llllllllllllllllllllllllllllllllllllllllllllllllll" ++
	      "llllllllllllllllllllllllllllllllllllllllllllllllll" ++
	      "llllllllllllllllllllllllllllllllllllllllllllllllll" ++
	      "llllllllllllllllllllllllllllllllllllllllllllllllll" ++
	      "lllllllllllllmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm" ++
	      "mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm" ++
	      "mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm" ++
	      "mmmmmmmmmmmmmmnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn" ++
	      "nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnooooo" ++
	      "oooppppppppppppppppppppppppppppppppppppppppppppppp" ++
	      "pppppppppppppppppppppppppppppppppppppppppppppppppp" ++
	      "ppppppppppppppppppppppppbqqqqqqqqqqqqqqqqqqqqqqqqq" ++
	      "qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq" ++
	      "qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq" ++
	      "qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqrrrrrrrrrrrrrrrrrr" ++
	      "rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrsssssssss" ++
	      "ssssssssssssssssssssssssssssssssssssssssssssssssss" ++
	      "ssssssssssssssssssssssssssssssssssssssssssssssssss" ++
	      "ssssssssssssssssssssssssssssssssssssssssssssssssss" ++
	      "ssssssssssssssssssssssssssssssssssssssssssssssssss" ++
	      "sssssssssssssssssssssssssssssssssssssssssssssssssx" ++
	      "sssssssssssssssssssssssssssttttttttttttttttttttttt" ++
	      "tttttttttttttttttttttttttttttttttttttttttttttttttt" ++
	      "tttttttttttttttttttttttttttttttttttttttttttttttttt" ++
	      "tttttttttttttttttttttttttttttttttwwwwwwwwwwwwwwwww" ++
	      "wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww" ++
	      "wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww" ++
	      "wwwxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxsx" ++
	      "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ++
	      "xxxxxxxxxxxxxxxxxxxxxjxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ++
	      "xxxxxhxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxcxxxxxxxxx" ++
	      "xxxxxxxxxxxxxxxxxxxxxxxxxxyyyyyyyyyyyyyyyyyyyyyyyy" ++
	      "yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy" ++
	      "yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy" ++
	      "yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy" ++
	      "yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy" ++
	      "yyyyyyyyyyyyyyyyyyyyyyyyxyyyyyyyyyyyyyyyyyyyyyyyyy" ++
	      "yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyzzzzzzzzzzzzzzzzzz" ++
	      "zzzzzzzzzzzzzzzzzzzzzczzzzzzzzzzzzzzzzzzzzzzzzzzzz" ++
	      "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz" ++
	      "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz" ++
	      "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz" ++
	      "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz" ++
	      "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz" ++
	      "zzzzz     cjwgnspgcgnesypbtyyzdxykygtdjnnjqmbsjzsc" ++
	      "yjsyyfpgkbzgylywjkgkljywkpjqhytwddzlsymrypywwcckzn" ++
	      "kyygttngjnykkzytcjnmcylqlypysfqrpzslwbtgkjfyxjwzlt" ++
	      "bncxjjjjtxdttsqzycdxxhgckbphffsswybgmxlpbylllhlxst" ++
	      "zmyjhsojnghdzqyklgjhsgqzhxqgkxzzwyscscjxyeyxadzpmd" ++
	      "ssmzjzqjyzcdjzwqjbyzbjgznzcpwhwxhqkmwfbpbydtjzzkxx" ++
	      "ylygxfptyjyyzpszlfchmqshgmxxsxjyqdcsbbqbefsjyhxwgz" ++
	      "kpylqbgldlcdtnmaeddkssngycsgxlyzaypnptsdkdylhgymyl" ++
	      "cxpycjndqjwxqxfyyfjlejpzrxccqwqqsbzkymgplbmjrqcfln" ++
	      "ymyqmtqyrbcjthztqfrxqhxmqjcjlyxgjmshzkbswyemyltxfs" ++
	      "ydsglycjqxsjnqbsctyhbftdcyjdjyyghqfsxwckqkxebptlpx" ++
	      "jzsrmebwhjlpjslyysmdxlclqkxlhxjrzjmfqhxhwywsbhtrxx" ++
	      "glhqhfnmgykldyxzpylggtmtcfpnjjzyljtyanjgbjplqgszyq" ++
	      "yaxbkysecjsznslyzhzxlzcghpxzhznytdsbcjkdlzayfmytle" ++
	      "bbgqyzkggldndnyskjshdlyxbcgyxypkdjmmzngmmclgezszxz" ++
	      "jfznmlzzthcsydbdllscddnlkjykjsycjlkwhqasdknhcsgaeh" ++
	      "daashtcplcpqybsdmpjlpzjoqlcdhjxysprchnwjnlhlyyqyhw" ++
	      "zptczgwwmzffjqqqqyxaclbhkdjxdgmmydqxzllsygxgkjrywz" ++
	      "wyclzmssjzldbydcpcxyhlxchyzjqsfqagmnyxpfrkssbjlyxy" ++
	      "syglnscmhcwwmnzjjlxxhchsyzsttxrycyxbyhcsmxjsznpwgp" ++
	      "xxtaybgajcxlysdccwzocwkccsbnhcpdyznfcyytyckxkybsqk" ++
	      "kytqqxfcwchcykelzqbsqyjqcclmthsywhmktlkjlycxwheqqh" ++
	      "tqkjpqsqscfymmdmgbwhwlgsllystlmlxpthmjhwljzyhzjxht" ++
	      "xjlhxrswlwzjcbxmhzqxsdzpsgfcsglsxymqshxpjxwmyqksmy" ++
	      "plrthbxftpmhyxlchlhlzylxgsssstclsldclrpbhzhxyyfhbb" ++
	      "gdmycnqqwlqhjjzywjzyejjdhpblqxtqkwhlchqxagtlxljxms" ++
	      "ljhtzkzjecxjcjnmfbycsfywybjzgnysdzsqyrsljpclpwxsdw" ++
	      "ejbjcbcnaytwgmpapclyqpclzxsbnmsggfnzjjbzsfzyndxhpl" ++
	      "qkzczwalsbccjxjyzgwkypsgxfzfcdkhjgxtlqfsgdslqwzkxt" ++
	      "mhsbgzmjzrglyjbpmlmsxlzjqzhzyjczydjwfmjklddpmjegxy" ++
	      "hylxhlqyqhkycwcjmyyxnatjhyccxzpcqlbzwwytwsqcmlpmyr" ++
	      "jcccxfpznzzljplxxyztzlgdltcklyrzzgqtkjhhgjljaxfgfj" ++
	      "zslcfdqzlclgjdjcsnzlljpjqdcclcjxmyzftsxgcgsbrzxjqq" ++
	      "ctzhgyqtjqqlzxjylylncyamcstylpdjbyregklzyzhlyszqlz" ++
	      "nwczcllwjqjjjkdgjzolbbzppglghtgzxyjhzmycnqsycyhbhg" ++
	      "xkamtxyxnbskyzzgjzlqjtfcjxdygjqjjpmgwgjjjpkqsbgbmm" ++
	      "cjssclpqpdxcdyykyfcjddyygywrhjrtgznyqldkljszzgzqzj" ++
	      "gdykshpzmtlcpwnjyfyzdjcnmwescyglbtzcgmssllyxqsxxbs" ++
	      "jsbbsgghfjlypmzjnlyywdqshzxtyywhmcyhywdbxbtlmsyyyf" ++
	      "sxjchtxxlhjhfssxzqhfzmzcztqcxzxrttdjhnnyzqqmtqdmmz" ++
	      " ytxmjgdxcdyzbffallztdltfxmxqzdngwqdbdczjdxbzgsqqd" ++
	      "djcmbkzffxmkdmdsyyszcmljdsynsprskmkmpcklgdbqtfzswt" ++
	      "fgglyplljzhgjjgypzltcsmcnbtjbqfkdhpyzgkpbbymtdssxt" ++
	      "bnpdkleycjnyddykzddhqhsdzsctarlltkzlgecllkjlqjaqnb" ++
	      "dkkghpjxzqksecshalqfmmgjnlyjbbtmlyzxdxjpldlpcqdhzy" ++
	      "cbzsczbzmsljflkrzjsnfrgjhxpdhyjybzgdlqcsezgxlblhyx" ++
	      "twmabchecmwyjyzlljjyhlgbdjlslygkdzpzxjyyzlwcxszfgw" ++
	      "yydlyhcljscmbjhblyzlycblydpdqysxqzbytdkyxlyycnrjmp" ++
	      "dqgklcljbcxbjddbblblczqrppxjcjlzcshltoljnmdddlngka" ++
	      "thqhjhykheznmshrphqqjchgmfprxhjgdychgklyrzqlcyqjnz" ++
	      "sqtkqjymszxwlcfqqqxyfggyptqwlmcrnfkkfsyylybmqammmy" ++
	      "xctpshcptxxzzsmphpshmclmldqfyqxszyjdjjzzhqpdszglst" ++
	      "jbckbxyqzjsgpsxqzqzrqtbdkwxzkhhgflbcsmdldgdzdblzyy" ++
	      "cxnncsybzbfglzzxswmsccmqnjqsbdqsjtxxmbltxcclzshzcx" ++
	      "rqjgjylxzfjphymzqqydfqjqlzznzjcdgzygztxmzysctlkpht" ++
	      "xhtlbjxjlxscdqxcbbtjfqzfsltjbtkqbxxjjljchczdbzjdcz" ++
	      "jdcprnpqcjpfczlclzxzdmxmphjsgzgszzqlylwtjpfsyaxmcj" ++
	      "btzyycwmytzsjjlqcqlwzmalbxyfbpnlsfhtgjwejjxxglljst" ++
	      "gshjqlzfkcgnndszfdeqfhbsaqtgylbxmmygszldydqmjjrgbj" ++
	      "tkgdhgkblqkbdmbylxwcxyttybkmrtjzxqjbhlmhmjjzmqasld" ++
	      "cyxyqdlqcafywyxqhz"
			},

    GBK3 = #gbk_table { name = gbk3, value =
	      "ksxsm sdqlybjjjgczbjfya jhphsyzgj   sn      xy  ng" ++
	      "    lggllyjds yssgyqyd xjyydldwjjwbbftbxthhbczcrfm" ++
	      "qwyfcwdzpyddwyxjajpsfnzyjxxxcxnnxxzzbpysyzhmzbqbzc" ++
	      "ycbxqsbhhxgfmbhhgqcxsthlygymxalelccxzrcsd njjtzzcl" ++
	      "jdtstbnxtyxsgkwyflhjqspxmxxdc lshxjbcfybyxhczbjyzl" ++
	      "wlcz gtsmtzxpqglsjfzzlslhdzbwjncjysnycqrzcwybtyftw" ++
	      "ecskdcbxhyzqyyxzcffzmjyxxsdcztbzjwszsxyrnygmdthjxs" ++
	      "qqccsbxrytsyfbjzgclyzzbszyzqscjhzqydxlbpjllmqxtydz" ++
	      "sqjtzplcgqtzwjbhcjdyfxjelbgxxmyjjqfzasyjnsydk jcjs" ++
	      "zcbatdclnjqmwnqncllkbybzzsyhjqltwlccxthllzntylnzxd" ++
	      "dtcenjyskkfksdkghwnlsjt jymrymzjgjmzgxykymsmjklfxm" ++
	      "tghpfmqjsmtgjqdgyalcmzcsdjlxdffjc f  ffkgpkhrcjqcj" ++
	      "dwjlfqdmlzbjjscgckdejcjdlzyckscclfcq czgpdqzjj hdd" ++
	      "wgsjdkccctllpskghzzljlgjgjjtjjjzczmlzyjkxzyzmljkyw" ++
	      "xmkjlkjgmclykjqlblkmdxwyxysllpsjqjqxyqfjtjdmxxllcr" ++
	      "qyjb xgg pjygegdjgnjyjkhqfqzkhyghdgllsdjjxkyoxnzsx" ++
	      "wwxdcskxxjyqscsqkjexsyzhydz ptqyzmtstzfsyldqagylcq" ++
	      "lyyyhlrq ldhsssadsjbrszxsjyrcgqc hmmxzdyohycqgphhy" ++
	      "nxrhgjlgwqwjhcstwasjpmmrdsztxyqpzxyhyqxtpbfyhhdwzb" ++
	      "txhqeexzxxkstexgltxydn  hyktmzhxlplbmlsfhyyggbhyqt" ++
	      "xwlqczydqdq gd lls zwjqwqajnytlxanzdecxzwwsgqqdyzt" ++
	      "chyqzlxygzglydqtjtadyzzcwyzymhyhyjzwsxhzylyskqysbc" ++
	      "yw  xjzgtyxqsyhxmchrwjpwxzlwjs sgnqbalzzmtjcjktsax" ++
	      "ljhhgoxzcpdmhgtysjxhmrlxjkxhmqxctxwzbkhzccdytxqhlx" ++
	      "hyx syydz znhxqyaygypdhdd pyzndltwxydpzjjcxmtlhbyn" ++
	      "yymhzllhnmylllmdcppxmxdkycydltxchhznaclcclylzsxzjn" ++
	      "zln lhyntkyjpychegttgqrgtgyhhlgcwyqkpyyyttttlhylly" ++
	      "ttplkyzqqzdq  nmjzxyqmktfbjdjjdxbtqzgtsyflqgxblzfh" ++
	      " zadpmjhlccyhdzfgydgcyxs hd d axxbpbyyaxcqffqyjxdl" ++
	      "jjzl bjydyqszwjlzkcdtctbkdyzdqjnkknjgyeglfykasntch" ++
	      "blwzbymjnygzyheyfjmctyfzjjhgck lxhdwxxjkyykssmwctq" ++
	      "zlpbzdtwzxzag kwxl lspbclloqmmzslbczzkdcz xgqqdcyt" ++
	      "zqwzqssfpktfqdcdshdtdwfhtdy jaqqkybdjyxtlj drqxxxa" ++
	      "ydrjlklytwhllrllcxylbw z  zzhkhxksmdsyyjpzbsqlcxxn" ++
	      "xwmdq gqmmczjgttybhyjbetpjxdqhkzbhfdxkawtwajldyjsf" ++
	      "hblddqjncxfjhdfjjwzpkzypcyzynxff ydbzznytxzembsehx" ++
	      "fzmbflzrsymzjrdjgxhjgjjnzzxhgxhymlpeyyxtgqshxssxmf" ++
	      "mkcctxnypszhzptxwywxyysljsqxzdleelmcpjclxsqhfwwtff" ++
	      "tnqjjjdxhwlyznflnkyyjldx hdynrjtywtrmdrqhwqcmfjdyz" ++
	      "hmyyxjwzqtxtlmrspwwchjb xygcyyrrlmpymkszyjrmysntpl" ++
	      "nbpyyxmykyngjzznlzhhanmpgwjdzmxxmllhgdzxyhxkrycjmf" ++
	      "fxyhjfssqlxxndyca nmtcjcyprrnytyqym sxndlylyljnlxy" ++
	      "shqmllyzljzxstyzsmcqynzlxbnnylrqtryyjzzhsytxcqgxzs" ++
	      "shmkczyqhzjnbh qsnjnzybknlqhznswxkhjyybqlbfl p bkq" ++
	      "zxsddjmessmlxxkwnmwwwydkzggtggxbjtdszxnxwmlptfxlcx" ++
	      "jjljzxnwxlyhhlrwhsc ybyawjjcwqqjzzyjgxpltzftpakqpt" ++
	      "lc  xtx hklefdleegqymsawhmljtwyqlyjeybqfnlyxrdsctg" ++
	      "gxyyn kyqctlhjlmkkcgygllldzydhzwpjzkdyzzhyyfqytyzs" ++
	      "ezzlymhjhtwyzlkyywzcskqqtdxwctyjklwqbdqyncs szjlkc" ++
	      "dcdtlzzacqqzzddxyplxzbqjylzllqdzqgyjyjsyxnyyynyjxk" ++
	      "xdazwrdljyyynjlxllhxjcykynqcclddnyyykyhhjcl pb qzz" ++
	      "yjxj fzdnfpzhddwfmyypqjrssqzsqdgpzjwdsjdhzxwybp gp" ++
	      "tmjthzsbgzmbjczwbbzmqcfmbdmcjxljbgjtz mqdyxjzyctyz" ++
	      "tzxtgkmybbcljssqymscx jeglxszbqjjlyxlyctsxmcwfa kb" ++
	      "qllljyxtyltxdphnhfqyzyes sdhwdjbsztfd czyqsyjdzjqp" ++
	      "bs j fbkjbxtkqhmkwjjlhhyyyyywyycdypczyjzwdlfwxwzzj" ++
	      "cxcdjzczlxjjtxbfwpxzptdzbccyhmlxbqlrtgrhqtlf mwwjx" ++
	      "jwcysctzqhxwxkjybmpkbnzhqcdtyfxbyxcbhxpsxt m sxlhk" ++
	      "mzxydhwxxshqhcyxglcsqypdh my ypyyykzljqtbqxmyhcwll" ++
	      "cyl ewcdcmlggqktlxkgndgzyjjlyhqdtnchxwszjydnytcqcb" ++
	      "hztbxwgwbxhmyqsycmqkaqyncs qhysqyshjgjcnxkzycxsbxx" ++
	      "hyylstyxtymgcpmgcccccmztasgqzjlosqylstmqsqdzljqqyp" ++
	      "lcycztcqqpbqjclpkhz yyxxdtddsjcxffllxmlwcjcxtspyxn" ++
	      "dtjsjwxqqjskyylsjhaykxcyydmamdqmlmczncybzkkyflmcsc" ++
	      "lhxrcjjgslnmtjzzygjddzjzk qgjyyxzxxqhheytmdsyyyqlf" ++
	      " zzdywhscyqwdrxqjyazzzdywbjwhyqszywnp  azjbznbyzzy" ++
	      "hnscpjmqcy zpnqtbzjkqqhngccxchbzkddnzhjdrlzlsjljyx" ++
	      "ytbgtcsqmnjpjsrxcfjqhtpzsyjwbzzzlstbwwqsmmfdwjyzct" ++
	      "bwzwqcslqgdhqsqlyzlgyxydcbtzkpj gm pnjkyjynhpwsnsz" ++
	      "zxybyhyzjqjtllcjthgdxxqcbywbwzggqrqzssnpkydznxqxjm" ++
	      "y dstzplthzwxwqtzenqzw ksscsjccgptcslccgllzxczqthn" ++
	      "jgyqznmckcstjskbjygqjpldxrgzyxcxhgdnlzwjjctsbcjxbf" ++
	      "zzpqdhjtywjynlzzpcjdsqjkdxyajyemmjtdljyryynhjbngzj" ++
	      "kmjxltbsllrzylcscnxjllhyllqqqlxymswcxsljmc zlnsdwt" ++
	      "jllggjxkyhbpdkmmscsgxjcsdybxdndqykjjtxdygmzzdzslo " ++
	      "yjsjzdlbtxxxqqjzlbylwsjjyjtdzqqzzzzjlzcdzjhpl qplf" ++
	      "fjzysj zfpfzksyjjhxttdxcysmmzcwbbjshfjxfqhyzfsjybx" ++
	      "pzlhmbxhzxfywdab lktshxkxjjzthgxh jxkzxszzwhwtzzzs" ++
	      "nxqzyawlcwxfxyyhxmyyswqmnlycyspjkhwcqhyljmzxhmcnzh" ++
	      "hxcltjplxyjhdyylttxfszhyxxsjbjyayrmlckd yhlrlllsty" ++
	      "zyyhscszqxkyqfpflk ntljmmtqyzwtlll s rbdmlqjbcc qy" ++
	      "wxfzrzdmcyggzjm  mxyfdxc shxncsyjjmpafyfnhyzxyezy " ++
	      "sdl zztxgfmyyysnbdnlhpfzdcyfssssn zzdgpafbdbzszbsg" ++
	      "cyjlm  z yxqcyxzlckbrbrbzcycjzeeyfgzlyzsfrtkqsxdcm" ++
	      "z  jl xscbykjbbrxllfqwjhyqylpzdxczybdhzrbjhwnjtjxl" ++
	      "kcfssdqyjkzcwjl b  tzlltlqblcqqccdfpphczlyygjdgwcf" ++
	      "czqyyyqyrqzslszfcqnwlhjcjjczkypzzbpdc   jgx gdz  f" ++
	      "gpsysdfwwjzjyxyyjyhwpbygxrylybhkjksftzmmkhtyysyyzp" ++
	      "yqydywmtjjrhl   tw  bjycfnmgjtysyzmsjyjhhqmyrszwtr" ++
	      "tzsskx gqgsptgcznjjcxmxgzt ydjz lsdglhyqgggthszpyj" ++
	      "hhgnygkggmdzylczlxqstgzslllmlcskbljzzsmmytpzsqjcj " ++
	      " zxzzcpshkzsxcdfmwrllqxrfzlysdctmxjthjntnrtzfqyhqg" ++
	      "llg   sjdjj tqjlnyhszxcgjzypfhdjspcczhjjjzjqdyb ss" ++
	      "lyttmqtbhjqnnygjyrqyqmzgcjkpd gmyzhqllsllclmholzgd" ++
	      "yyfzsljc zlylzqjeshnylljxgjxlyjyyyxnbzljsszcqqzjyl" ++
	      "lzldj llzllbnyl hxxccqkyjxxxklkseccqkkkcgyyxywtqoh" ++
	      "thxpyxx hcyeychbbjqcs szs lzylgezwmysx jqqsqyyycmd" ++
	      "zywctjsycjkcddjlbdjjzqysqqxxhqjohdyxgmajpchcpljsmt" ++
	      "xerxjqd pjdbsmsstktssmmtrzszmldj rn sqxqydyyzbdsln" ++
	      "fgpzmdycwfdtmypqwytjzzqjjrjhqbhzpjhnxxyydyhhnmfcpb" ++
	      "zpzzlzfmztzmyftskyjyjzhbzzygh pzcscsjssxfjgdyzyhzc" ++
	      "whcsexfqzywklytmlymqpxxskqjpxzhmhqyjs cjlqwhmybdhy" ++
	      "ylhlglcfytlxcjscpjskphjrtxteylssls yhxscznwtdwjslh" ++
	      "tqdjhgydphcqfzljlzptynlmjllqyshhylqqzypbywrfy js y" ++
	      "p yrhjnqtfwtwrchygmm yyhsmzhngcelqqmtcwcmpxjjfyysx" ++
	      "ztybmstsyjdtjqtlhynpyqzlcxznzmylflwby jgsylymzctdw" ++
	      "gszslmwzwwqzsayysssapxwcmgxhxdzyjgsjhygscyyxhbbzjk" ++
	      "ssmalxycfygmqyjycxjlljgczgqjcczotyxmtthlwtgfzkpzcx" ++
	      "kjycxctjcyh xsgckxzpsjpxhjwpjgsqxxsdmrszzyzwsykyzs" ++
	      "hbcsplwsscjhjlchhylhfhhxjsx lnylsdhzxysxlwzyhcldyh" ++
	      "zmdyspjtqznwqpsswctst zlmssmnyymjqjzwtyydchqlxkwbg" ++
	      "qybkfc jdlzllyylszydwhxpsbcmljscgbhxlqrljxysdwxzsl" ++
	      "df hlslymjljylyjcdrjlfsyjfnllcqyqfjy szlylmstdjcyh" ++
	      "zllnwlxxygyygxxhhzzxczqzfnwpypkpypmlgxgg dxzzkzfbx" ++
	      "xlzptytswhzyxhqhxxxywzyswdmzkxhzphgchj lfjxptzthly" ++
	      "xcrhxshxkjxxzqdcqyl jlkhtxcwhjfwcfpqryqxyqy gpggsc" ++
	      "sxngkchkzxhflxjbyzwtsxxncyjjmwzjqrhfqsyljzgynslgtc" ++
	      "ybyxxwyhhxynsqymlywgyqbbzljlpsytjzhyzwlrorjkczjxxy" ++
	      "xchdyxyxxjddsqfxyltsfxlmtyjmjjyyxltcxqzqhzlyyxzh n" ++
	      "lrhxjcdyhlbrlmrllaxksllljlxxxlycry lccgjcmtlzllyzz" ++
	      "pcw jyzeckzdqyqpcjcyzmbbcydcnltrmfgyqbsygmdqqzmkql" ++
	      "pgtbqcjfkjcxbljmswmdt  ldlppbxcwkcbjczhkphyyhzkzmp" ++
	      "jysylpnyyxdb"
		      },

    GBK4 = #gbk_table { name = gbk4, value = 
	      "kxxmzjxsttdzxxbzyshjpfxpqbyljqkyzzzwl zgfwyctjxjpy" ++
	      "yspmsmydyshqy zchmjmcagcfbbhplxtyqx djgxdhkxxnbhrm" ++
	      "lnjsltsmrnlxqjyzlsqglbhdcgyqyyhwfjybbyjyjjdpqyapfx" ++
	      "cgjscrssyz lbzjjjlgxzyxyxsqkxbxxgcxpld wetdwwcjmbt" ++
	      "xchxyxxfxllj fwdpzsmylmwytcbcecblgdbqzqfjdjhymcxtx" ++
	      "drmjwrh xcjzylqdyhlsrsywwzjymtllltqcjzbtckzcyqjzqa" ++
	      "lmyhwwdxzxqdllqsgjfjljhjazdjgtkhsstcyjfpszlxzxrwgl" ++
	      "dlzr lzqtgslllllyxxqgdzybphl x bpfd   hy jcc dmzpp" ++
	      "z cyqxldozlwdwyythcqsccrsslfzfp qmbjxlmyfgjb m jwd" ++
	      "n mmjtgbdzlp hsymjyl hdzjcctlcl ljcpddqdsznbgzxxcx" ++
	      "qycbzxzfzfjsnttjyhtcmjxtmxspdsypzgmljtycbmdkycsz z" ++
	      "yfyctgwhkyjxgyclndzscyzssdllqflqllxfdyhxggnywyllsd" ++
	      "lbbjcyjzmlhl xyyytdlllb b bqjzmpclmjpgehbcqax hhhz" ++
	      "chxyhjaxhlphjgpqqzgjjzzgzdqybzhhbwyffqdlzljxjpalxz" ++
	      "daglgwqyxxxfmmsypfmxsyzyshdzkxsmmzzsdnzcfp ltzdnmx" ++
	      "zymzmmxhhczjemxxksthwlsqlzllsjphlgzyhmxxhgzcjmhxtx" ++
	      "fwkmwkdthmfzzydkmsclcmghsxpslcxyxmkxyah jzmcsnxyym" ++
	      "mpmlgxmhlmlqmxtkzqyszjshyzjzybdqzwzqkdjlfmekzjpezs" ++
	      "wjmzyltemznplplbpykkqzkeqlwayyplhhaq jkqclhyxxmlyc" ++
	      "cyskg  lcnszkyzkcqzqljpmzhxlywqlnrydtykwszdxddntqd" ++
	      "fqqmgseltthpwtxxlwydlzyzcqqpllkcc ylbqqczcljslzjxd" ++
	      "dbzqdljxzqjyzqkzljcyqdypp pqykjyrpcbymxkllzllfqpyl" ++
	      "llmsglcyrytmxyzfdzrysyztfmsmcl ywzgxzggsjsgkdtggzl" ++
	      "ldzbzhyyzhzywxyzymsdbzyjgtsmtfxqyjssdgslnndlyzzlrx" ++
	      "trznzxnqfmyzjzykbpnlypblnzz jhtzkgyzzrdznfgxskgjtt" ++
	      "yllgzzbjzklplzylxyxbjfpnjzzxcdxzyxzggrs jksmzjlsjy" ++
	      "wq yhqjxpjzt lsnshrnypzt wchklpszlcyysjylybbwzpdwg" ++
	      "cyxckdzxsgzwwyqyytctdllxwkczkkcclgcqqdzlqcsfqchqhs" ++
	      "fmqzlnbbshzdysjqplzcd cwjkjlpcmz jsqyzyhcpydsdzngq" ++
	      "mbsflnffgfsm q lgqcyybkjsrjhzldcftlljgjhtxzcszztjg" ++
	      "gkyoxblzppgtgyjdhz zzllqfzgqjzczbxbsxpxhyyclwdqjjx" ++
	      "mfdfzhqqmqg yhtycrznqxgpdzcszcljbhbzcyzzppyzzsgyhc" ++
	      "kpzjljnsc sllxb mstldfjmkdjslxlsz p pgjllydszgql l" ++
	      "kyyhzttnt  tzzbsz ztlljtyyll llqyzqlbdzlslyyzyfszs" ++
	      "nhnc   bbwsk rbc zm  gjmzlshtslzbl q xflyljqbzg st" ++
	      "bmzjlxfnb xjztsfjmssnxlkbhsjxtnlzdntljjgzjyjczxygy" ++
	      "hwrwqnztn fjszpzshzjfyrdjfcjzbfzqchzxfxsbzqlzsgyft" ++
	      "zdcszxzjbqmszkjrhyjzckmjkhchgtxkjqalxbxfjtrtylxjhd" ++
	      "tsjx j jjzmzlcqsbtxhqgxtxxhxftsdkfjhzxjfj  zcdlllt" ++
	      "qsqzqwqxswtwgwbccgzllqzbclmqqtzhzxzxljfrmyzflxys x" ++
	      "xjk xrmqdzdmmyxbsqbhgcmwfwtgmxlzpyytgzyccddyzxs g " ++
	      "yjyznbgpzjcqswxcjrtfycgrhztxszzt cbfclsyxzlzqmzlmp" ++
	      " lxzjxslbysmqhxxz rxsqzzzsslyflczjrcrxhhzxq dshjsj" ++
	      "jhqcxjbcynsssrjbqlpxqpymlxzkyxlxcjlcycxxzzlxlll hr" ++
	      "zzdxytyxcxff bpxdgygztcqwyltlswwsgzjmmgtjfsgzyafsm" ++
	      "lpfcwbjcljmzlpjjlmdyyyfbygyzgyzyrqqhxy kxygy fsfsl" ++
	      "nqhcfhccfxblplzyxxxkhhxshjzscxczwhhhplqalpqahxdlgg" ++
	      "gdrndtpyqjjcljzljlhyhyqydhz zczywteyzxhsl jbdgwxpc" ++
	      "  tjckllwkllcsstknzdnqnttlzsszyqkcgbhcrrychfpfyrwq" ++
	      "pxxkdbbbqtzkznpcfxmqkcypzxehzkctcmxxmx nwwxjyhlstm" ++
	      "csqdjcxctcnd p lccjlsblplqcdnndscjdpgwmrzclodansyz" ++
	      "rdwjjdbcxwstszyljpxloclgpcjfzljyl c cnlckxtpzjwcyx" ++
	      "wfzdknjcjlltqcbxnw xbxklylhzlqzllzxwjljjjgcmngjdzx" ++
	      "txcxyxjjxsjtstp ghtxdfptffllxqpk fzflylybqjhzbmddb" ++
	      "cycld tddqlyjjwqllcsjpyyclttjpycmgyxzhsztwqwrfzhjg" ++
	      "azmrhcyy ptdlybyznbbxyxhzddnh msgbwfzzjcyxllrzcyxz" ++
	      "lwjgcggnycpmzqzhfgtcjeaqcpjcs dczdwldfrypysccwbxgz" ++
	      "mzztqscpxxjcjychcjwsnxxwjn mt mcdqdcllwnk zgglcczm" ++
	      "lbqjqdsjzzghqywbzjlttdhhcchflsjyscgc zjbypbpdqkxwy" ++
	      "yflxncwcxbmaykkjwzzzrxy yqjfljphhhytzqmhsgzqwbwjdy" ++
	      "sqzxslzyymyszg x hysyscsyznlqyljxcxtlwdqzpcycyppnx" ++
	      "fyrcmsmslxglgctlxzgz g tc dsllyxmtzalcpxjtjwtcyyjb" ++
	      "lbzlqmylxpghdlssdhbdcsxhamlzpjmcnhjysygchskqmc lwj" ++
	      "xsmocdrlyqzhjmyby lyetfjfrfksyxftwdsxxlysjslyxsnxy" ++
	      "yxhahhjzxwmljcsqlkydztzsxfdxgzjksxybdpwnzwpczczeny" ++
	      "cxqfjykbdmljqq lxslyxxylljdzbsmhpsttqqwlhogyblzzal" ++
	      "xqlzerrqlstmypyxjjxqsjpbryxyjlxyqylthylymlkljt llh" ++
	      "fzwkhljlhlj klj tlqxylmbtxchxcfxlhhhjbyzzkbxsdqc j" ++
	      "zsyhzxfebcqwyyjqtzyqhqqzmwffhfrbntpcjlfzgppxdbbztg" ++
	      " gchmfly xlxpqsywmngqlxjqjtcbhxspxlbyyjddhsjqyjxll" ++
	      "dtkhhbfwdysqrnwldebzwcydljtmxmjsxyrwfymwrxxysztzzt" ++
	      "ymldq xlyq jtscxwlprjwxhyphydnxhgmywytzcs tsdlwdcq" ++
	      "pyclqyjwxwzzmylclmxcmzsqtzpjqblgxjzfljjytjnxmcxs c" ++
	      "dl dyjdqcxsqyclzxzzxmxqrjhzjphfljlmlqnldxzlllfypny" ++
	      "ysxcqqcmjzzhnpzmekmxkyqlxstxxhwdcwdzgyyfpjzdyzjzx " ++
	      "rzjchrtlpyzbsjhxzypbdfgzzrytngxcqy b cckrjjbjerzgy" ++
	      "  xknsjkljsjzljybzsqlbcktylccclpfyadzyqgk tsfc xdk" ++
	      "dyxyfttyh  wtghrynjsbsnyjhkllslydxxwbcjsbbpjzjcjdz" ++
	      "bfxxbrjlaygcsndcdszblpz dwsbxbcllxxlzdjzsjy lyxfff" ++
	      "bhjjxgbygjpmmmpssdzjmtlyzjxswxtyledqpjmygqzjgdblqj" ++
	      "wjqllsdgytqjczcjdzxqgsgjhqxnqlzbxsgzhcxy ljxyxydfq" ++
	      "qjjfxdhctxjyrxysqtjxyebyyssyxjxncyzxfxmsyszxy schs" ++
	      "hxzzzgzcgfjdltynpzgyjyztyqzpbxcbdztzc zyxxyhhsqxsh" ++
	      "dhgqhjhgxwsztmmlhyxgcbtclzkkwjzrclekxtdbcykqqsayxc" ++
	      "jxwwgsbhjyzs  csjkqcxswxfltynytpzc czjqtzwjqdzzzqz" ++
	      "ljjxlsbhpyxxpsxshheztxfptjqyzzxhyaxncfzyyhxgnxmywx" ++
	      "tcspdhhgymxmxqcxtsbcqsjyxxtyyly pclmmszmjzzllcogxz" ++
	      "aajzyhjmzxhdxzsxzdzxleyjjzjbhzmzzzqtzpsxztdsxjjlny" ++
	      "azhhyysrnqdthzhayjyjhdzjzlsw cltbzyecwcycrylcxnhzy" ++
	      "dzydtrxxbzsxqhxjhhlxxlhdlqfdbsxfzzyychtyyjbhecjkgj" ++
	      "fxhzjfxhwhdzfyapnpgnymshk mamnbyjtmxyjcthjbzyfcgty" ++
	      "hwphftwzzezsbzegpbmtskftycmhbllhgpzjxzjgzjyxzsbbqs" ++
	      "czzlzccstpgxmjsftcczjz djxcybzlfcjsyzfgszlybcwzzby" ++
	      "zdzypswyjgxzbdsysxlgzybzfyxxxccxtzlsqyxzjqdcztdxzj" ++
	      "jqcgxtdgscxzsyjjqcc ldqztqchqqjzyezwkjcfypqtynlmkc" ++
	      "qzqzbqnyjddzqzxdpzjcdjstcjnxbcmsjqmjqwwjqnjnlllwqc" ++
	      "qqdzpzydcydzcttf znztqzdtjlzbclltdsxkjzqdpzlzntjxz" ++
	      "bcjltqjldgdbbjqdcjwynzyzcdwllxwlrxntqqczxkjld tdgl" ++
	      " lajjkly kqll dz td ycggjyxdxfrskstqdenqmrkq  hgkd" ++
	      "ldazfkypbggpzrebzzykyqspegjjglkqzzzslysywqzwfqzylz" ++
	      "zlzhwcgkyp qgnpgblplrrjyxcccyyhsbzfybnyytgzxylxczw" ++
	      "h zjzblfflgskhyjzeyjhlplllldzlyczblcybbxbcbpnnzc r" ++
	      " sycgyy qzwtzdxtedcnzzzty hdynyjlxdjyqdjszwlsh lbc" ++
	      "zpyzjyctdyntsyctszyyegdw ycxtscysmgzsccsdslccrqxyy" ++
	      "elsm xztebblyylltqsyrxfkbxsychbjbwkgskhhjh xgnlycd" ++
	      "lfyljgbxqxqqzzplnypxjyqymrbsyyhkxxstmxrczzywxyhymc" ++
	      "l lzhqwqxdbxbzwzmldmyskfmklzcyqyczqxzlyyzmddz ftqp" ++
	      "czcyypzhzllytztzxdtqcy ksccyyazjpcylzyjtfnyyynrs y" ++
	      "lmmnxjsmyb sljqyldzdpqbzzblfndsqkczfywhgqmrdsxycyt" ++
	      "xnq jpyjbfcjdyzfbrxejdgyqbsrmnfyyqpghyjdyzxgr htk " ++
	      "leq zntsmpklbsgbpyszbydjzsstjzytxzphsszsbzczptqfzm" ++
	      "yflypybbjgxzmxxdjmtsyskkbzxhjcelbsmjyjzcxt mljshrz" ++
	      "zslxjqpyzxmkygxxjcljprmyygadyskqs dhrzkqxzyztcghyt" ++
	      "lmljxybsyctbhjhjfcwzsxwwtkzlxqshlyjzjxe mplprcglt " ++
	      "zztlnjcyjgdtclklpllqpjmzbapxyzlkktgdwczzbnzdtdyqzj" ++
	      "yjgmctxltgcszlmlhbglk  njhdxphlfmkyd lgxdtwzfrjejz" ++
	      "tzhydxykshwfzcqshknqqhtzhxmjdjskhxzjzbzzxympagjmst" ++
	      "bxlskyynwrtsqlscbpspsgzwyhtlksssw hzzlyytnxjgmjszs" ++
	      "xfwnlsoztxgxlsmmlbwldszylkqcqctmycfjbslxclzzclxxks" ++
	      "bjqclhjpsqplsxxckslnhpsfqqytxy jzlqldtzqjzdyydjnzp" ++
	      "d cdskjfsljhylzsqzlbtxxdgtqbdyazxdzhzjnhhqbyknxjjq" ++
	      "czmlljzkspldsclbblzkleljlbq ycxjxgcnlcqplzlznjtzlx" ++
	      "yxpxmyzxwyczyhzbtrblxlcczjadjlmmmsssmybhb kkbhrsxx" ++
	      "jmxsdynzpelbbrhwghfchgm  klltsjyycqltskywyyhywxbxq" ++
	      "ywbawykqldq tmtkhqcgdqktgpkxhcpthtwthkshthlxyzyyda" ++
	      "spkyzpceqdltbdssegyjq xcwxssbz dfydlyjcls yzyexcyy" ++
	      "sdwnzajgyhywtjdaxysrltdpsyxfnejdy lxllqzyqqhgjhzyc" ++
	      "shwshczyjxllnxzjjn fxmfpycyawddhdmczlqzhzyztldywll" ++
	      "hymmylmbwwkxydtyldjpyw xjwmllsafdllyflb   bqtzcqlj" ++
	      "tfmbthydcqrddwr qnysnmzbyytbjhp ygtjahg tbstxkbtzb" ++
	      "kldbeqqhqmjdyttxpgbktlgqxjjjcthxqdwjlwrfwqgwqhckry" ++
	      "swgftgygbxsd wdfjxxxjzlpyyypayxhydqkxsaxyxgskqhykf" ++
	      "dddpplcjlhqeewxksyykdbplfjtpkjltcyyhhjttpltzzcdlsh" ++
	      "qkzjqyste eywyyzy xyysttjkllpwmcyhqgxyhcrmbxpllnqt" ++
	      "jhyylfd fxzpsftljxxjbswyysksflxlpplbbblbsfxyzsylff" ++
	      "fscjds tztryysyffsyzszbjtbctsbsdhrtjjbytcxyje xbne" ++
	      "bjdsysykgsjzbxbytfzwgenhhhhzhhtfwgzstbgxklsty mtmb" ++
	      "yxj skzscdyjrcwxzfhmymcxlzndtdh xdjggybfbnbpthfjaa" ++
	      "xwfpxmyphdttcxzzpxrsywzdlybbjd qwqjpzypzjznjpzjlzt" ++
	      " fysbttslmptzrtdxqsjehbzyj dhljsqmlhtxtjecxslzzspk" ++
	      "tlzkqqyfs gywpcpqfhqhytqxzkrsg gsjczlptxcdyyzss qz" ++
	      "slxlzmycbcqbzyxhbsxlzdltcdjtylzjyyzpzylltxjsjxhlbr" ++
	      "ypxqzskswwwygyabbztqktgpyspxbjcmllxztbklgqkq lsktf" ++
	      "xrdkbfpftbbrfeeqgypzsstlbtpszzsjdhlqlzpmsmmsxlqqnk" ++
	      "nbrddnxxdhddjyyyfqgzlxsmjqgxytqlgpbqxcyzy drj gtdj" ++
	      "yhqshtmjsbwplwhlzffny  gxqhpltbqpfbcwqdbygpnztbfzj" ++
	      "gsdctjshxeawzzylltyybwjkxxghlfk djtmsz sqynzggswqs" ++
	      "phtlsskmcl  yszqqxncjdqgzdlfnykljcjllzlmzjn   scht" ++
	      "hxzlzjbbhqzwwycrdhlyqqjbeyfsjxwhsr  wjhwpslmssgztt" ++
	      "ygyqqwr lalhmjtqjcmxqbjjzjxtyzkxbyqxbjxshzssfjlxmx" ++
	      "  fghkzszggylcls rjyhslllmzxelgl xdjtbgyzbpktzhkzj" ++
	      "yqsbctwwqjpqwxhgzgdyfljbyfdjf hsfmbyzhqgfwqsyfyjgp" ++
	      "hzbyyzffwodjrlmftwlbzgycqxcdj ygzyyyyhy xdwegazyhx" ++
	      "jlzythlrmgrxxzcl   ljjtjtbwjybjjbxjjtjteekhwslj lp" ++
	      "sfyzpqqbdlqjjtyyqlyzkdksqj yyqzldqtgjj  js cmraqth" ++
	      "tejmfctyhypkmhycwj cfhyyxwshctxrljhjshccyyyjltktty" ++
	      "tmxgtcjtzaxyoczlylbszyw jytsjyhbyshfjlygjxxtmzyylt" ++
	      "xxypzlxyjzyzyybnhmymdyylblhlsyygqllscxlxhdwkqgyshq" ++
	      "ywljyyhzmsljljxcjjyy cbcpzjmylcqlnjqjlxyjmlzjqlycm" ++
	      "hcfmmfpqqmfxlmcfqmm znfhjgtthkhchydxtmqzymyytyyyzz" ++
	      "dcymzydlfmycqzwzz mabtbcmzzgdfycgcytt fwfdtzqssstx" ++
	      "jhxytsxlywwkxexwznnqzjzjjccchyyxbzxzcyjtllcqxynjyc" ++
	      "yycynzzqyyyewy czdcjyhympwpymlgkdldqqbchjxy       " ++
	      "                                                  " ++
	      "                 sypszsjczc     cqytsjljjt   "
		      },
    ets:new(gbk_table, [public, set, named_table, {keypos, #gbk_table.name}]),
    ets:insert(gbk_table, GB2312),
    ets:insert(gbk_table, GBK3),
    ets:insert(gbk_table, GBK4).

get_pinyin(_) ->
    init(),
    [GBK] = ets:lookup(gbk_table, gb2312),
    lists:nth(66, GBK#gbk_table.value).
