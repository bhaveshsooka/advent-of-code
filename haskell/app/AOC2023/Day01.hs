module AOC2023.Day01 (
  printAoC2023Day01Answer
) where

import Data.List (intercalate)
import Data.List.Split (splitOn)

printAoC2023Day01Answer :: IO ()
printAoC2023Day01Answer = do
    putStrLn "------ Day 01 ------"
    putStrLn $ "Part 1:" ++ show part1
    putStrLn $ show part2
    putStrLn ""

part1 :: Int
part1 =
  let numeric_string = filter (\c -> c `elem` ['1'..'9'])
      f = unwrap .safeHead . numeric_string
      l = last . numeric_string
      unwrap = (\maybeChar -> case maybeChar of
        Just c -> c
        Nothing -> '0')
      extractFirstAndLast = (\s -> [f s] ++ [l s])
  in
    sum $ read <$> extractFirstAndLast <$> numeric_string <$> (lines input)

part2 :: Int
part2 =
  let numeric_string = filter (\c -> c `elem` ['1'..'9'])
      f = unwrap .safeHead . numeric_string
      l = last . numeric_string
      unwrap = (\maybeChar -> case maybeChar of
        Just c -> c
        Nothing -> '0')
      extractFirstAndLast = (\s -> [f s] ++ [l s])
  in
    sum $ read <$> extractFirstAndLast <$> numeric_string <$> replace_words_with_digits <$> (lines input)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

replace_words_with_digits :: String -> String
replace_words_with_digits str = 
  replace "one" "o1e" $
  replace "two" "t2o" $
  replace "three" "t3e" $
  replace "four" "f4r" $
  replace "five" "f5e" $
  replace "six" "s6x" $
  replace "seven" "s7n" $
  replace "eight" "e8t" $
  replace "nine" "n9e" str

-- Helper function to replace all instances of a substring
replace :: String -> String -> String -> String
replace old new = intercalate new . splitOn old

input :: String
input = "9dlvndqbddghpxc\nrtkrbtthree8sixfoureight6\nfdxrqmfxdkstpmcj7lmphgsmqqnmjrtwo3tcbc\nonetjcsmgk57nvmkvcvkdtqtsksgpchsfsjzkkmb\nsix8threepvlxttc85two\n8five9ttqst2one2vz\nhbrmhsnjeight64dgdnvdbspk7ninetzbvjczqrj\nfourtwofivesix5\n3gksfourqf48\n7one1tnqxfvhmjvjzfive\nsevenmcjs3lmlmxmcgptwobjggfive6four\nseven8five3\n5sfknxsn5sevenfour446\nbxc5two67seven2\njcsfivefive89seven85\nnine296\nseven5twoeight\n1eighttwo8jfnhmfivefivezdsxqxqsjkone\nfoureight48sbkkvc17zbksgvcbb\nlnzgspccsn4cxqqdbkj\nqlxrxkpeight48xbgqnlkpkoneseven\nz7onetwonec\n7cns\npnpfninefive79twoone7\n2hrqpjjjbn\n4gmlttgdzrhxbxnnine\n4sixfiveone76jctmjsxdh5jrkv\n3kvjmhpmglrdgmdnine\nfour63sevensevenone\njmz1eight4threej1\nfour21zxksf9jxdvjmtn337\nmsnronenine43three1threefrv\nrjfhd6eight4\n78blgveightfiveone7bnsfnrmxsmtwonemrb\nsixseven6four6\nmdjphcm9\nxsjmgdgqtwolg1nine45eight\nfive2six85npdqxgrshdjs4\njbbnine2ttrktc2hxpxfdxgf\nfngvqsgmjfmfslrmone2vtpsstpkhr2jmmxk\nf683glvfsdvnsghvrzcdmxnx81\nlqblzgj322kqfsjrbxgcgsct\nthreeldfnrbstbxqdpxpkbztbp84eight\nsevensevenmthprqg9six\nqldknljthdjthreeklttd6six\n7eightcdqxcftbgbfbnvqfive\ngxjzhvkbcjhscdxhjdqxnhsevenxrdrjbcl5fvlvlxjjvb\n9sixqnine9jk9six\nzjtdbzr6njdgflrmpshxn\nrktpknvmjknb7threefourhdxhcdtgtkvone\n276lzxhone3two2\n82fivelppqzjq\nlchslxtwohslsztgps5pdssctclhdkqtwo\n2five8three9dnine8\n68jpnqldjgfnpcmvbxnszhz2252\nq32\n2sixtwo87\nhlmdvlrqlrjdshone3five\nfivetwo7\nxsdcktrone29\neightfour9eighttwoxvhdth9lndg4\ntwo8jgddjhcj67eight\n8nrkrcrqhr21stqtvqn\ntvbonepzrrklninexmpxrlkcpgg9qgrkcjt\n6two5qjmoneclfhzhkxbntmvmdrc\nprhmt4xvlg\n3mghfgrhzkj5\nlq2lnrcj1pnlh\nsixqhfqrmf8fivenkkcqpgf85lone\ntlrlcbhdvd5\nthree8seven\nthfns325threefpvlntfvrf\ntwoblkldr2mmrsxpqxcms39seven6\n2mpcvttntg31mkznplfkbcgccsix\n2sbs\n828jnvjnbgrs1\nfhcglnxzss23bxfnpczvthree2\n7fhghprqvrbx1nxml2one32\n5fivethree7qrsixmcdveight6\nfive4seven\n1cfcfdzfjphsevenmdvnzh\nqhmnleightbhbcntwojjfxpvlxt2spponenine\nfivelskzvzsix5xtqkfl1\n2jfgpmdncvpjmqcgvnzpqlstzgfdvfxrlscjkzczsf\nrzzplmzsfivetxbhcqnzdq4\nmvrvfour9eightseventhree\n7smrdqkrh8qlzc\nmpf2fivefivefivepgm\n1sixpkrdjlszgdnccnllfsevenksdkvqbxbpbblthqpzqf\nsix91dxxdhrxcbmqpqm\nsevenfour8nine7\none2hnfvh\n55jmqkqgvsgqcrzvmzqhone8twopsrtgmqrj\nxnfjxqlrsjmgk68kvpptczxhkxcvrpgctddjklrmhzjgtcjh\nseveng6two\n1threexrzqcrknhc3\n58qtpqqz58888cmhs\npxvbhmczrvpnjnsrcdrnrjvzzpjnbgbxdseven6\n34two565\nfour4six\nsqtxsjsix54\nfour3xrxmrkn4nrcsmljqrninethreeone\n3six4bqddfivejnfrhnqqsgqhj\ngtbtrtzp8seveneight3seven4xzdnfrvzgn\nvggfdfvlrgvqqvjhkmxfsfbdpqfivexs16\nonegkvdhrfninerndk46nine\n5fiveeighthnrlzln\nthreeqctjkpxjx39six89two2\n99ninecbzck\nnxqlhpgnine6pvrrpfjpssix6seven\n1dbrzjkckthm5sixsix\nfivefmfqcsj19nhnzg\nfive91eightninecn\n7ljnl71eight7mzhzfht\n84sevenzrqvkxszdhg66\njskktpm5mphd1\nboneightfournq6ndnqpdbm97five\n5twonine\nscqpkxrjtwo8foureight11\n3eight7brgqfivezpgclhfj\nzkfrsdgbmczlrzchvfql78ftsdqk8vmqccbn\noneseven3kgdkjzkmq94\nsgtwo59kdmhbndzd\nthreeone37\nmqxvrhmrpqnltvt9lrpplmttkhdvtln9\nonecxqvr48six2\npjnx2eight7five\ntwo189one6gbqvllzb35\npsvxjhscstjfkbpxhbbb4zvdjbcdxqrqzqlzp\nn18hmhzrqjrpcxztwo\n6two46zblgrbmjcqbnxqcnbf\n7kxsjdqcmxrvmdtscvxgrnhrmrfour\nonedzeight8qfive2\nseven9bkjone2sixqnztq21\nfiveninesixfkzlsn8fivembfjnx4threetwonexb\n5zmxtcmzqqdthreetczccnxhkxrbntmfoursixjhhrsdxthree\n1four2xpkfgcn\n88trnvjtqsmseight8\npbbpbfgsrst5five\njvgvdseven2two\n962sixoneonectfgpknl8nine\nrkbnzz1l42eightflb4\ntwoxfll2fourbjfjgxfbtk\n35fourtwo\nlq56ninefour1three\n77ztlmqxcxrj745\nnnvqrthreedt2eight6hvrlnpbts\n6five6225sixfnzzbh\nnine1three\n8xgdsdfgcfourlhn\n2three3ninelckpddbmdrfournine\n89zmvxnlrj7658kjdjchq\npfhbgpb66twogpn7twobpvrbmmrvp\n8hgqmztnmhkcr6xrxrbhj\n6fivemnfcvvx8\n3mxpvgzq9ninebmlktwo\nthzhbsl49seventhree1bdxcrgjq\nrlcfour3ffkxxrhb\nrkj2eightfive\nxqncfnhkcqxqjgbsjhnrgm6\nninefour52ninefourtworhsix\nlpzcmxt97mlkjhlcone\ntwo4qkmqgrpltkrdsctpnsqmbtptklprx6ncfpseven\n61shxgxdqqqzngnnzeightkhmgrxprb2sixjx\n9eightfour3one6seven\neight4sixqjxdjnzmkfflpfmkf\n3bfc71\n14sevenfivezzmt8cpptl\n1sgrzdqdndsevenninembzvfive\nlxftrbfcqfoneeightrsdxlnp1x2jsgn2\nvlfcjfourmtxbcngpjvkcctvbgkgpvvqpsg1\n116five132seven\neightpvfsfbfzjcdcvh8kbrcz357\n99two\nfour6six\nfzzklmnxvfrhd62xmftbrhgsslrlqv\n5six55zbdlgc\n62nineone8qcglr1\nsddddseven55fourlpqzbgzfive5\neighteight17\nnine98\nthree9qqxgfpjfkeight81nine2\ntwotwosevenvkzzhrpgninecqvf9\nlkhf5onexsrtwo\n8hfzvnzrd\n9threeninethreeseven\nonedpgjzsixxs4cg5jlvzcsbd\nfgmfive97\n4fivefourckthree\nnine1fivefour\n7qlchtvd\ntmtmtnxdpsvfour4621four6\n5qfvzh7seven\nfourfivebqnpzbg8three\n34kd9four5seven8three\nkcqcxzmnkdt5twojdggp\n4lvbfdpzjsdthreeldvkcbqrspktwo\n2nine93foursixnine5\n8tbpjgmxltwojlpbnsvqhsjfcjcfvcrjqppdb5seven\n8twodcpglrxcq23sixfour\n3sixvczzqsfive\n1three45jh54tbdvkj\npdrzqxdtcnbrnine8zvffmrtwo11\n3eight54sksqfxhzcdfour\n7hsllsjtxtwo9jhjlhthreekdfssninetcjjrm\n9sgmnine1kjmhjthree\nnineseven9five\nggdcphlstwoonethreeffgmrseven2xsbsf2\n75ssbccflrrf4lpmptcqjcmfbpklqc\n5six2threedvvlxdxsixvxxghpddn8pbnr\ntlbjnlxfxvnine4grhj71hnf6\n964eighteight4eight6t\n5qbmnsone1fourseventwo\noneseven1five9two\n5fdpl37vklxpth672\n4czvrnxtlfiveseven74\nqdgqbsd5fourfive\nddrqjnkbq87six2fourmtghdtvtl7\nzdldjnfxms692rbdfgvtsevenxzfjpnsf\nnsqtkbbfhn8threesix3fivefoursix6\nsfxjzhpqqslfourmpph44threetwo\nonefive3seven21\n9eight1sevenfive7nine\n7twonrthgr73vzb\n18onefourhxxm\n3xvboneightlb\n3ftqct9\n8five55\n4eightmzrlfjqqddffgmfl\n74mhzvktwolgpvrvnphc\nsixnine694ninetwo\nbzpnktvcqleight2\n5oneeight1five2dszthreedncqzmhc\neightfrbmcblrghgmpkrdnh7fournzbvtrzvhchsix\nthreethreeoneone6kbghfst\n4fiveonexfchmclqhqfive\n1t3nine\n57jclthreefourbtdgsggzh\nqpc9fhddthreefive\n9eightnine7cgpbbnine8\n88twoxbzjp6fmqlznzgpgdqmmnpmkvctm\nthreelb9four168qpchgnfn\n4onekrlmslkd7five\n4jfxtsseven\nzsthvnxpf7\n656three\n453seven\nzxmr55hgnvtjbbqhfsxssjxzshcbkvsbzg1\npbkffqzrbvxvqjfgkpmp5\nninesix677lpqpgmc\nsixgmb67fzdhnnfk7\njlgjbltbtwotwoeight9sixninevlngvvj\nxdxqdhps9sevenzmzdlnvsix3oneightj\nninetwokpzvvqlgtcfx2\ndqmjchlbj7sevenfive\n61eight9kjmxclvdrdmvnthree\n8nine6three\n3ndpknone\nseven4one\n8dxljzpk8twoxfmhbqnmqeightdhxvpgxcnine5\n6five3seven\nrzgfivenxhpr6gxqflrskhjhvoneonegzbclk\njrvjhkfv7sqnsz86five5skvvmspvjd3\nsevensix3c6llkmsmmssf3\nsevendf73eight74\n7bqbbvmq2krkbhlsh\n3dqq55eightcptwo\n2bjgfrgrtgnfour\n44xznjjvlhzclsix\nseven6rtzponeseven\n6ghvvkhrzvjzpxlbkonejlmrhq\nzgsxthree6\n3crzdvbnhtsgldjvbpsixbjmlxsv3vrzjtv\nxcpnbsevengpffknsv84sixsixseven\nfour898xkqnhspbqmtcs\n5twonine\nsix2ltwosix\ncnine22sixeightwost\n6lx63two\nfbqoneight3five\nrbtwone2eight8\n9fivezrsdfkdlqone1nthjvdlmxhqvj\npmmfp8seven\n1qmttlsjpxjbsrzmbtlxqzfzvrs968\nfgdonexgvvnine5jsm\nrbgmbtwo9fsntcthreesix76\nfbrsxxshseven3gtv\nmkxfour66one9n\neightnine1fivecsixlqds2\nlhxnxhfnmlmhxkcxndmnine1kfpb5eight\n6eight13rseven6\nhxcfone64ninesevenbgsnrqppqmnnineeightwof\nthreebjpbtpzgx5mnthreensixoneightz\nbpdvfqlzthree2vvcmvzllqfbgjgp\ntwo926sixeightdsvdmnxthreetnqvktdbn\nbhntwonefourninejjcmlfphzpseven95\nprqoneightseven1nbcxnqjfourfivecmj2\nstwone1oneninevcrfzpfourfivetwo\nztkknhjr9six8\ntwomlvrzm934seveneight\nntnn1bqgbxf13rqdqtcrbpeighttwo\nseven58ninemdqonecrkbdblone\nfoursixthree1oneeightone62\n8385dhgcqjtrsixtwo8\nblpzxstwosevenqbdhkb8sfggkbdhx5rzftqsf\nmqbeightwoninefourlgcj9\nnjvpkpvjpmvmbsrdgdvx7frrgvnfrmmrfrhqdtwothree8\nrtwonesxxone8tnine\n2xqgrqmone73five\n5ggzdxvptwonineeightdj\nsvqhzhzbsmhljxeight7hljd\none81kmnmpmfkseven\n9ninefourbvbpljb3nineqxnkgzgj\nsixfour7nvdfhnd\n24twoseven52\n425vqnhmrpxthreenine3bkjpvs\n8fzqqspdp6\n427five9zhbxpbr\n1qjnxxkgqhp\neightxzthreelrbgfbvmrpmtgvrfgqmjlshbqv5\nonetwo4\nthree194bfivevknbqxv\n1mjhplcvheightlnvn7two\nrmptjk5seventhreeonesevenkdxfkvdz\nthreesix8kpmrvgkpsix\nfjfbx6eight3fourninesp\nsix3nhonebtdzppnxkr\nxgtltdtwoninesix62szphpcmtv6\nfiveeight9seventhreesevengndgdfv\nninetwosmcbfkvf5hvksvfbr3ssbntlk2\nqjrdlmone7tqrzlvfourvfbvtkjxfpkffdpjnine\nsevenfqnsnqt89five5\n16sevenxtdrtmdzrxhneightwot\n8ttreight4eight\nthree9nine3832four\nfivesncggngss8qzfvj\ntwotwo7sgtcgrmdnr545\n6xk7threejmgnqnsevenmczscqxqxfour\ntwosix4eight\nkxm15two844eight\ntwoeight5sevenfour\nfive56three\nthreekdjgcdd6one\nclchjoneeightrqgk8bdsdmzm\ntwog546two6x\n5nine4bmhbtbksxreightoneightkg\ngjfkbsr1gvtvlnbzqrlfour\none48vpone\neightbcklnvtgvthreeninefivetmgtbjrqnn3\nnkclzcvbbq8nineznbtkntgndnine1xrg\nlqpfhmrlmg32xtddcfffdbxbjckvlzghpldfgvhfhdhr\nfourpkcppxtmqz5\ndvb6sixvhdrzzjk\nbtwokhl99nine\n73rkgbsqgz7koneonemlhrbtxc\neighthlqqt4onenine\neight61oneightx\nninebbtcjjlqkthreenfvbcc4three946\nninepqtlffxvclgn7fqdhqbpmkkseveneighttwo\nbqzpfxntnkhhfkv98fhrqslfs9four\n4ktprrljbthreethreethree3blmddptfour\n5fourone\n9zhvgleightseven5nbcmscqms2two\n6czngsp895\n2threekrxmtfrr2ppfrzqkqgfoureightlf\nsix8eight6seventxgl\n1one9\n3qpmrtzsvgkjxsevengzdkjkbbdltlrjkznbbkmpb\nsclxoneeightfoursfbfm4rbxssmgngfxrvcv\nfhctmnpxdrfivemndlr3sdp\ntwoseven9sixlp\n962\ntsnsndjtrp27cbtsjdlkrqmtctxvxvnx\n5mnine\n7fgt6\nthree1ninertghhbhbcnjdsknine\ntwo8ninefxcqmxdhtnmcmjvtds6fivebnm\n7phdp\n2hnszbksg\n4ninedcnjbcxnrmoneeight1\nhnsszlsx7414two4\ngmkrn9gcgmffrvbfivethreefive2seven\n2bknqdqmrxpfftptwo2vmqffgmzr\ndsvf3nv14zklptjnhv\n54nchsnpknkx\noneone15\nonefvmxnzp575sixpxnpndqf\n6vqrzsbbxg3735\ntwojbldk2hfqqzgone5\nonesixlzznvc8nineseven359\n6vjbr7\nnineone32nmkszsthreefpscxcqtwofour\nfv1zpqrxvdlzqmeighthhbbbzt\ncskvllzszxzk9\n7ninebrlggdzqk7ninelvddrtt1\n2fpttpbq6b7ninefour3five\nbsslmmmhfxc6\nlfjkdzdm9eightkbdhrkpcnzcftlggh\nrnineninelmbhfour88onevfzhcmneightwofp\none59ktxrdrhc8six9\n86nxnfzzsixgmxxglvfivezmkdvmhjfzone\n9threefourfourrbxqpqbtbzxstwo\n3pvgtcxrfvthree39bjfivenxvbjone\nsixkmngfour3bfive\n6threehntsjhjpmqhl345\nfourjcnd49fivexqfbj\nksmszhfive7rrphsxxhgm\nzqtdztkhveightninepnctbcgqsb6\nninemlvzfcljfkeight1tppxrqtdzp2\ntwotwoeightnine5frzk3\nlfvqtzbsix3\nfour3bsgft28\nsixgksnkrmsix2rbjmfmnfzfiveeight\nvrtffvbtcmszfdc2sevenhvpfour8\ntwopvgzmthree3\nxqffprzsrjqkcbsevenczlbc5\n4ninefournmvninetwommnrctqjhg\n27four\nfheightwod4\n38zkgvdpgjzqfivenlr7mdxd\nninexxvninesevenfivedpzfgpbv5\nxjzgznvfsevensixthree3rgfqhkxbfp5vfrjcdncfkjk\n2kcbprjfhns68\nbndqgcmnine4qbzfgxmlffive\nffctnxgtwobldqhsix8djfmdnpgmt\nfourseven91ninesevenpksgt3\nrninesznvtbq5zqmthzrcntskdthree7\n8eight7\n1hnlnp79\n1three3\n414ninevqrccrf6lmzqmsjc6\n15mpeightxmhxlxjmn\nktdblrmqqxmktvqfour87\n1four121three56\nldfdgfqkr22nxtsrsntlsevenxxrcclrhtl6five\neightfourthree8pvlkkbmbrfnfr6\n8sixninefiveqcmjhpx\n2seveneight85lskjhn7\n31c3\nhpgmpmjmnk4twothree\ncqsxgbj23six11\nthreesxzmgtvhhheight99xsix\n6jlpt6rnsprllqhgbvhtqtjvft217\n22onesevenfour4sevenseven\nsbnjszzkdleight6three\n3twofourdmrsqqtnzgng1two\nldcvxqbsfqpgql9ngsbhfrmszhgvznnnjhnm5\n1oneseven28seven\nnsvhqtfivemjgcdqpbtwo1nine2\nsix8dj\nzdoneight67fivesevencqlr\nxlzrkgjrhmszmkzlkkmrxjdnzrtlzssfpcxkbxvp6jghldhvr9\n8sevenbzbnnx\nvzrmcfvc9shkxfblfkf\n9twoshzmktntwo5nqp\n3fourvpdfs3eightword\nthree24onesixgdzgkspgsjprjgn7eight\n4tz2sixsixqbqfsr4twonine\ntwoq2eight\nrccxnvtqnhdlgzqfzcnrmqtjqonesix732\nseightwo8ninekndqrdtbfp\ncgnmrkcjvbgfmpbpjckhfrzrtnine2sevenfivesxhqk\nfourxsncktrjtpnine6ccbgpl\n1sixone5vkzxnhgdlbsevendtwo\n9jbnineppf7\n5foursixlrn5vdgdbvnfhg\nfivecrtwo8zbqrvjgpfivenine2\nktvdhgvone6dbrvj5sqbr5\nbljtwonesixthree7dzdfrgqrkstwo4xvfmtsbpqjgxsgqcpc\nsixgg5\nvmrbqdqdrqtwo2onez\nonehlgrgndk9ninefsntwo1rnclttm\n4tqzjbhdhkm1\nnine1kffxzcrn189\nsbrxr9ninemqb8\nsix528mslnf\nsdvthree7dfivenine918\n3kvzqqkonepmghblzvljnineone47twonesdf\nfour9sevenfourjhmjjslqgd8\n3fkfmgqf6fourbldjfvbhr\neighttxpddhsnzrkplzqc1ninezpvqgnhxzftwo\n56lsxmkfpghsqptvhmmmzhcmfdeightsix\none72kdfdrtwothree8\n7kpgjhpnthree7\n37j2mtwo\nseven4mjponefivesdgncqm2gkklsqvthfeight\nmgckktt9six4eightwol\n4grzfdm499ninetbt\ntzmbtv84b\nxvzltd75dhpjckmpdrkgglskqrhd5\ncgnbhmdlj24\n7fivetpbtmone\nfivelpmbxvhktzkkonexsxktlzthgbgqkgcj94\n5sixhggcbrft\n43five7122tqlkhssczsr\neight1seven3\nfourtwo134fourmzpxspr9\nsnxjqnf8ctcnvjknghxpkpbrt2\n4qj2xtwoghknine\n1zjfivegtwotqgndnineg\nfjmmznxkrml7fiveninelfvkqtrg6\n8xjlqgqj395nineninefive\ndx47sixtwothdphj\nfivesevenxgsfb235cvgmspdgg6\nfivettfzvfg78dmzzdzcrt\nfpnvxp1\nseven24\n1three8oneeight\n7xglzsqrtb8fhgthmgdcdtwo\nseven8gdtfgd4fivepdq\ntwosix1qtttvvstmqbrhh\nptwomnjhlzjjkztwo4kdkgxv\n89zkvrhmrhdbmfourzdpss\nbfdsvnxflgqxgpzkmrptlvmvlkchjxrt67\n9mmhxsevennvvdngznine17twofive\nlszmqdmxz87mqbv51\n546sevenninencccndnr4\nnineeightsqgrpkrqlmzrlxm6hxchcjspnx\n8twoccdnbfive\n1ks98\ntwothree6\ntwoxsixtqdpp6dcclzgfvkv1\n16one7scvsgvmcdsixjtzszzrxdzzgv\nbjxzdsixkvqgjfzbbjnrtn212jplvrj4\nzb2three5cngqfczc1\n5fnine99zxvfourjgmn\none634rteightfoureight1\n76gvcdfourgtbzdlltzsjnxqg\nltmdrkqqnfour9eightpckdvbhlkvxlpdtp\n6two5kgncpnzkdsgnpspb\nthree8vjninekbbnnfrdstprcmklrgpkfpmbs\nckvvqjqzbpfhf52fivefivefour\n7ngxpdqptksix\nftp2eight\n8lnmpbqldqstwo\nthreetwo153644\n914\nplxjdxghsix17\nsevenonesevennine4three2seven\n7bvdgpghzhpeight512vxbnfqjctb\nxqptzkfive4xqbjzpqfkfspqv5kgqbdtfive\nbmltkhjsckhrc7two8nzdpkjjpnfive\nnnhppfvlhcmnmrjxhrbtzdflseventwojfprxmfn7\nfournfdtjtsbthree54fpzsq\nninenineone5two\n8694twomgxxzfxr6\nsixeight75seventhreerpchfour8\nllv5\nskxcbfffgc6sixone\n3lhjbzbsg4lsfgpkmcz7vjxzbrshbseven\n4onecjlkpqdljd3five5\neighttwo2mvdtvqpnonetvphxsk8\n15195one\nkdx9nine6qrnqrjxq59\ntwolhjmbh4fivetwo6pdzbnzt\npbvfour7\ntgxrsbk2\nhfournineone58sixthree9\nhmftwonesix5dhthkcnzqseventhreenineeightnine\n9jmmjclsnsdhhj\nhljjvctthreefourxsdlvsgtqj1335seven\n172\n2gbfpjff\nfive5sevengvddnphnine\nsixeightfourssptsqlhzxonetfg2hkmrcpfzcz\n94csjjgl42three\n4bkxxv6\npgdgoneqmhxfpnfqkrbkjeightbmjjlrnsc5\nnineninetwo55fvsxspzt\n32jqffghbqvfmjtjone78\n93eightfour6eight9eight1\n768jrdmfxgxpntx8nhpljjdx\neight33zpvpggklseven4lcqsixthree\nfourgrmone4\nbcknine9qsevenrjhjeight\n5onevqm\n578mmqhhljtwo39mfnpmfqthree\nfivecmts43eightfdphfivejsx\n8vkqvl\n8oneone27fourseventwo\njfsixfivethree3cmjtvkzhqkcksmljxxzbjd\nthree6sixpdvtrnrtffltrs\nthreeltvvfkcdqjtwo89two8j\n5sixeightfivesixjjmknrgd1qpsbpjrffjl\neightrmgfkckxxxsvfclhtgcjthree3\nrnfbp8eightjv35eight\n2rrssqrfkvmq\ndcmghdmg6threetzsdx\nqzhfivefivejvbtncm2\ngcspvjnsevenqhmkngfivesix15jtpk\nbkmlmh5six1sixone2hgtlsix\n8rmjshdhm681vc\nsixtx32\n7fourtwo6\nxgrcxeightsevenzgmcllfjqn7\n553fivethreefour8nine\none4mxbmct\nthree6ninetwofour\n1bbbxqhhlmj651eightkfdqdgvh9three\n4414\nmnxsixone92\n61six8\neight8mrcpfive1\ncrsvmfivezbkzkqsix7mxjdgtsqbfvdbnlqtfiveoneightgj\n4dmzznftdbqj\nseven25\nfhlhpvphqvh8one1ffkq\nmboneighteightonefive1ninenineninetwonine\n82fiveonexc5\n97qbhvbqnmxtlpczsx\nnhgzzjkx5\nhgxlrk9\n8twonine9fivetdxmjqppxr\nonethree99sevenfourkzvd2\nsdqjfvbndzcthree1ftoneightkm\n5rcllqcxt\nthreejdntvhsixsckfpndjzkeightsix3nlgpsvsfhk\n94fctwoxmczbkz\n4eight35tbqjxglldsevenxmthmmlhsix\nthree125\n3njtlmxtbr4541ninedz7\n9ptzpzqrjmxlrmbfbpn9ss4sqcprfmcqg\n8dpmkdtvjxzjbddn7pvkxzskddrhcsjvthree7\nxxshbfcmf3cfdkeight\nsix9ninefour472\nfourrvxfmjzd4five\n1fivesix\ntwonine3six9\nnine8onethreethree\nfoureighttwotwo7onethree\n9prtlnjptoneninefiveninefive\ncljqxpthqzdxpmmbvpjljjxhlhsql8kztpthreesevenoneights\n9sixtwonetfh\nkscmxpcqlthrmthcdhplpnqlq2\nfiveqjfvkmnineeightninefourtwo1\none5sixeightfxkshmninethreeeight\n4nine4\n27\nggtz73sixkgsjrtcxkb1\nqvxsgvrpbxqcgpb2fzmcvknkr99seven\n6eight8ksgdlxj638fivetcgb\nmmvc3fourrvbztjchbmqtxtgfrrqphninefive1\n3qzbzxsevenfivegrvtbckqcj\neighttwokzpgl2hrvqlhkthreenine4\n8twoeighteightxtsbrseven\nnine5mghp7vpnvtpx2c\nhkxkmx5qnpjhtdfjfsix2xqqplvm5gmvjm\n69cgvzhvgjvl8\nthreeblzj6three\n3bxpmnfbtpk5hcgqkbkqblznxgsdvklmtmqjxsxdcgqvmsprxrrnfchfbnd\n1fbrrcjgzzllmcbdrgmrcfsevenh\n58onezbp\n1bqfkmkk46cctvmstvhvrtwoone\nmmeightwo9sbjvleightdsevenseven\n9gdsqgflkvonetwo\n3lgpmxdgjtzx3two5foursixeightwomb\ndbkbeightfive9eighthxngnrbmp\n2cpkxggtrdsrh\nzsgcdcrlhlqdpone9eight17sixbbtzpmdf\n5rskkplgsbl9qqzfrzh67four4\nnine856threezgbhrzjcfour\nkqnnine87lflxddvtfb9lfjdknvgl8\nfsgtwo8zxvnfour7xxfnmqpzhzone4\nddd9sixnsrpqmvvjh2xghxhm\n836\nnxglt36ljcbvgc16hxcbtqjz\nnine19fourhnvh7fgqklf11\njkvsphrpmhbnfl2nhcflhsbks4\n7nine2dsgkmrzlrzptfpk672\n6vtmztrjdrbk\n4vgsshzzsrtwo\nninexfjcxdcnxs7seventhree2\nhsvvqcqp97twommjjlclbtdjbxkveightwos\nthreedlcvvseven3\n366xqfbhzfmqknine7\njzrqqfouronehhrmkg4one5\n8xcbccrp141kmcsrdlgcdzpcb1nine\n59rdrphcgk\nfivetwoqmlk22eightfive\nqnsphtvfourtwojljxnvgrkk3slv6four\n13dfbnfnpsevennqtjthreethree6\n4dtncvsix34oneeighthpfouroneightv\nxdhqninefive88nine\n6hnnl8nineonehm7four\nsevenfournine8five2six\nvnczlzjqdtmdgsgxch94one3knlxjvqtrjlsx2\ntwoeight14kkzrsqmgkhjb\njhqrzxrhskngj9ninenjcnbdtjhjtdh\n2grrlxnlvthreernfghspmc\ntwo8hjbsevenfive5threenpgtnkftp\nd8sixfive1five7\nsixthree9\nsixspqf2gptcsrvlln9\nseven8nine\nnqeightwo7svvjqs75qgp3hpvn\nqrsixxffsdvvrf74five3\n5threeskqgcgprrjmcxksixtwo969\nbdbtlvlseven13fh\n2two1eightfour7jqdd\nthree8ninefournffzbnbhkpjqh32\nsixqzmhnjttdnine41sevenpcxqkvr\nfivenine2fourseven2\nsixjjhrjbmvvngqd3three7ninekpneight\n11lttrkpcljbbrmponeightbb\nfrxgkvgrjtsix84\n76eightonesix8fivenlfhkfgp\nfourpqlrklpnfljvpfkdklkgrjp1ninevfour\n1six75tr1\ncjdvxhmjvstninejccxrqhb1qkpmnzx8xfpp\n8sixfourone6pzlnczvlsmmp\neightrkl37jqlvjjsbrtqsix6\nonesmrjvmrtlppm69857seven\n4rgsktnbone8sjxjzbrplnmfvkknpxqv\ntfteightwonsdffone8sixxdpeightseven\n639one2two\nsmk55\n6cjbckhbtmkcgrvmp3\nrdnf9\n1jjkdspeighthteight37\n1gh3dcmhx4sixnphphhbpninevbqhs\nvpstbqtdmbvk9jstvtgzrdl\n91seventwo\n7sdpflkxfzfivethreenine\ngvgcrpphhbzghtbcv6\nvvnine5ddgzroneone\n5four6684one\ndmspptjjtwocr2one\nsixftzx6fdsfv\n5six8twotccbsdnpxg32tsix\nmfkdbdfournktdmgqnt4vqkzbzonekdqhbcmfgfiveone\nnine2476\n8rxd2eightninefourmd\neight1hqfcqs4cpvpsqjfhptwo6six\n5z\nbqcj948nine6xmgnxmxnn\ntwo1lzdv65bhllvrc\nsevenvjkl3ninespltvdszvnfvzpcvrctphvp49\n4mzjxftf5eight\nfivehbrxcbgjhhxpzfn5lllsknk7six\n6n5ttwo166\nfqdfhc3\n6cph\nlzqqvnlkjv3sevenbdssvckmdm\nbbdpgpfsevenvzsix87\n754sznxkfb4npmjbv4one\ntwothree984dsxsninefour\nkmpjgh71jhfrjgrpbd2d7\n27tgptfvcjnk2\nonefive497cdcktxcjfivezdrdhczbp\n79176threeseven1\ncsseven1rmlbcpct\n1lzlz4tsngmgh\nzvxshddzg2eightpxkzfnvdfzeightseven\nninesixnine93seven8\n1twofiveeightmfour\nsix3cmbbbxbqr74flchvgjbr45\n7nine31two9dvsghkrjj\nfive5four\nseveneightthreefourseventrgxdqclq2\n5dfgddsevenldmzckmvxjmk92fourdcpfgcrpd\nninekbhrflzmgp3lqgxgszmzvndr47zcllcfg\nsqsixonefivekfvbfh66p\nbssrkdrrgsftlqjdz19\nbqcbhfive6\njt3fivemdrnrvngsb1seven5\nsvdninesbj11eight\n2phltdc\n1469sevenvksvlthqskfkx9\n7fcfivefourdbbvtnjbrc\n6seven4\nlrmfqtjkzfive69eight8\nzmngmr9ninenineninesh\nqrxvdxgtfour7one6\neightmjpkgdtrp52shpcrtb\n54k7fournine\n7fdbshl6874\njfjvrhccqrc1fourcrg96\n1sevenncqhkgtzmtncmxhflmfsxfsmsmqh\n79fourjmxfdbqnrsr\nsjthree6tcbp3x\nqvrzmdzfnpkkdcvone2lttcbzhhddbnlnhxgsblhtlvdcpnzjvvqszrthree\nninedfxcz63two515\n48sevenvgznbsxzhfgzq19\ndxjbtzvtfn3five3\nfive1gxfcfppffg74\n78ncrnhmv766nine\ntwo7qmgeightjhddgnineone\njhttksbpbhzmgglfour7\nbloneight3ckvpkxtwozzxr1onethree\nmhmppdtvfonecrjzlktlnkpzdbqtvtwo4\n179tjchxninethreesix\ndpcfnsftnjbhlpcjrc8pbnnhtlrjzmmjk\nninebn16one2crfour\nmfivesix8nine1zgpqpr\neight44fourfivegfive\n2qhsltnzsfivervftmdm1hthcml\nsjoneightvrxctb9sixhkhmfivejm\n7threehdctsfqflggzkhpn\nseightfourkhpkprrcl6six\nfourkdff7\n57seven3four1r1gcjtckvn\ngmfdzgv7fivesix4nvlq3\nthp7\nxd5plhtvtgxmgkmhlr5nine\n7fourdf\npnkjgdctpm2221four8pvnhxdtrvs\nhngtq27nineninesixeight\n2fivejpfbsqtx1fourseven\n19jpjsbljgz\nninethree5twoeighttwoqponefive\ngklmbcj5mstwo8\nphbeightwo31jjzltcqzhklm26\njtslpzsxh43\ndrk7\n5dxcsvgqkmz\njnrgv9\nnsttntwo6\n6qbfrlcfmmnqpmbbtqgdsjqndqfive\n2mgdjlpdsixfive6\nsevenfivesix6\n3nrfvb13onebsrclqnflppzchtp21\ndmpgdvfive7\nlpvqjzhvjoneghqxnvsixjxflzqj4\nonegqzxvnnfnlcffour2\nnsglbskbzd2drjzqhnq\nnine8seven\nrfspthhjbh92rseven1\nnine2ctjnb\ngzkgvtxjt8twoonefour\ntplxnlr8bfjklthree\njqrxrtdvmj8fournmj\n6gxvglbcqkdsdcsl\npzjk9ccbmkzmtsf\nqdzpknfhbsixczkrqbpfour76sevensbvltnjccjllng\nsix1zrhqxzfivetwo\nxjhmfkvjgxtbgszmpd54jbsscgrninethree\ncndbtlq22\nfouronesevenzrrv7mhsvjtwokqbfvvrs5\ntwoonefive59phkxdbndgch5seven\n8twoninethreeckdkd5\n9four6jfxnfgjvcrszrpsrkhh\n993psn4qdldmfnqsix4\n42tzxknj\nvvzpl8six\n8vpchdjxczsvjjqljnmpdeightone2seven\nfivenine51sixcdb\ndfkkvsone5kjzvqqc\n8seven8pxtrdrkqcqhdklgsixzxtjmtblzzcc\nddvzsqnxd9xscsix4rvpsix\n75nsvxf9one88\n38ninetmmqx2\nsxjqgrdthree2vhpgrcdtqeight\n63kthndjc\nqgbdkfm56two6fourthree5\nfive79\njpb2seven7one7\nfour5sbgbpzkjjkjhsvgksgjvfrsbpbdzgd2\nxlxpsixninextzlpbn6\njzbvzlflf8twoeight\n4vjtr6eightninenjtvt\n3twonine3vmrgkjzkpfoursixtbcktpn\nsix1fourbfhxlntrqfxxztmj4\njrbhfourfour6jspsxkn2eight5four\npcvjmdlnf15cfghpszt9\n4lxktgpvvq1sevenone9\ntwosixhllhbxkbd2three\nveightwo3eightvrhseven89onelxvhqxkhm\n2sixfkdpcjcgcbgfzlbgblj\n457jtmdfdjcnine\nthree64\npqgjfseven4sevenoneslgeight\n91vcptwo1twonineone\nseven63fourzjpdmk1\n8fivetwofrbtm6f9five\nsevenchrtbctkgpnine65fourbhpqnchhlz6\nrkdqrbtdbj9bt41hklvlrbrcpx\n1three1gmdeighteightqbgsevenhljbpzbdtt\nsrkbbkfcznine6nine6\n8kmzbjzsxtgr9drtbdl\n24six412smjfxscst\n9cktpshfdr5djvrngchrh176\n2mjknvteightmvsdgt8seventhree3mzfk\nvhldtk27two\n6three2ninepqnine\n2fourzbtqnclrtpsix2six94\nsix6five\none2eightppzmczmgsixnine\n15qljk7vchkfcfhkhmbjlkcfour\n99four\ndhrgkkbrnczkdt3n\n89211sevenhnbjbrxtk\n663rrbpnrknine911nine\n2ntffsix9\n4mdpcrvqfoursixqeight\njbjjqkfive3jqnqhvkmtbddrdqxseven\n2gvrgdsxptwovfkpgdlmlhz34\nrqfrx82\ntworzlxeight66eight1five\nmd1jgknfftttjbjz\ntctqctkone37cmpbslgpzh3\n3sixsixfpxg\nseven6bld42brzxr\n6two4seven9zpgb5\nnine13\nd68\n3lxvsgksbbtwo52eight3three8\nbpltlnlzc8\nseven2five1threedmkg\nhpkvhlkhknhjpq9tgpmnndgtlqjx1vkdkvtqhtwodrbr\nfourfivesxgpxhdvts7four5\n7frgrjkkrb1\n2threettvptwobjhmxvpsvmljtvnpkpsvr\n9fivelxtcbbn9xgtwohmmhnfhcknc\nfourqztfvd6gkhxnstjjxnbl69hzkghsjrd\npspgqcjfcqdgq6xsgxrls\nthreesix6lrfkmvjdfivetwo9\nkdjgpj54\noneeightsevenfive1\none4onefournine64\n45hhcpntsthreezl9hjdnnine\n17ninenineninegczplbj81\n31threetwosevencphdv\n5eightvsrzjmdbtqhhqtjfjrhllhbgzgzjzvdhddstxpp4\npgghlbtsevenqxxjbnd14onetwo\n3ljgmcxnqgxcrfourfourtworjtwo\neightseventhree59\n3nhsccljtzszftnqtfour5\nthreefourthreeone3five\n3pzgzjv7tz\nqxgntksdr45tvnxfcjdnn\n5seven9threejdzrzdfcpgbnhrrmfkkskg2two\n38bjnlcjbeightfivefourqvtbvjsb9nine\nnmxqtzlpfngzlsnl9\n9eight8five3two1gsknxznbf9\n518jkqmprjlqcpdthreefive16\n6lzjznblrj3three\nf7blbxznlvgk\n37jtlrvlzhzronehn\nkpckhlcsbeight6\n8dfptmfourtwoeightfoursixonefive\nninecdrxlgdkm7nineseventwotbbcgninekbxssd\nrrhkgmnsbxbhb2ccnncrbstjfbmnlbxsxbkr\ncflrvjtdsevenfivefive3sevensixeightseven\n41ninenrznrcpdqhxfglb\ngffivesixtqbbmzllvbnjnk2fivesix\neight311seven\n52mpqbgktxhs359btkzqdfzvtrzmltxt\nonegxnk2rvnmdcmqvqvgkml34\ngfzeightwoeightrbvknvpt7\n1ct2\nzctqtxtgseven66zhslvvdninetwo\n54sixbfn8mxfkthf5\n9974seven9dclxbmfive\nonefour6mjtssrxjjsleight2foureight\n818sixsmmzsvbpl9two\nrjqnpzlp83qxlj\nsxqztv22\n8tsvhfszvj\none5mninefour68\nthree7threeone\nsevencnq2jdjvmlh5mqnnnrsqgppkfxjfjsevendrq\n8zmxbvgschsqxbk5lltxpseventhreevrhsvdkk\nseven2ngbqlxkjl27eighttjprz\nccfskxtnqpqninevczrltkg18hmgjmqt\nnpgtv4nine5lnqvglvdrxvqmc\nkvg4zrtpxnknbone\nthreeeight1tsdcthree5zxrshttlsmseven9\n35448284\n6fiveqplkfftsj\npldmrjhzhfiverlgntcckbqzjgth4gfddcrz\n6fivesix2\neightksz6m6pneightnvpvvx\n1fourbqxtmbvzsfnrxqmvlbfzvdthree\ntwo43sixthree5one\ncqf2\ntwoltseven8three64\n6fourprlhcc\nfgqoneightsevenqthreebksixgdqt93dm\nxjczd3sixseven5\nkfzgshnxqnptrckbrt2\nfourrrdcl624\nkvhfqspcpsxndjqlonesixthree24kdmqvone\n8eightfivetwo\nonesevenseven5fourlrkkqtfkrmdlsmd\njffvtzkbjnkdtvfsfthree431lrpgmtv\nbbvsptrzbone4tfnpgrfourvsix\n4seventhreekmxsz335eight\neightbrcv5\ntwo2eightbppsplzgcfournine5seven\nfourthree5kcdhqzeighteightkbzszgv8nine\nrgxrddnnbv7rkt\n8ffmvpcsvfoureightqpnzzjksgchnine9jlgjqb\ntwo9tfvjqsgqsixnine\nbzn4two\nsqlfeighteight6hjddxzcone2\n3fivekmfnqlctddfivelcthnine\ntwodn8\none5six913lbrcc\nfoureightmppchbgz8lqbzqbjztwo7cksqxns\nzvhzgfpkhkone93nine"
