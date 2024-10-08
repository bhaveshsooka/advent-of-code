module AOC2015.Day07 (
  printAoC2015Day07Answer,
) where

import Data.Bits
import Data.Char (isDigit)
import Data.Map (Map, filterWithKey, fromList, lookup, mapWithKey, toList)
import Data.Maybe (fromJust)
import Data.Word (Word16)
import GHC.Base (List)
import Prelude hiding (lookup)

printAoC2015Day07Answer :: IO ()
printAoC2015Day07Answer = do
  putStrLn "------ Day 07 ------"
  putStrLn $ "part1: " ++ show part1
  -- putStrLn $ "part2: " ++ show part2
  putStrLn ""

input :: String
input = "NOT dq -> dr\nkg OR kf -> kh\nep OR eo -> eq\n44430 -> b\nNOT gs -> gt\ndd OR do -> dp\neg AND ei -> ej\ny AND ae -> ag\njx AND jz -> ka\nlf RSHIFT 2 -> lg\nz AND aa -> ac\ndy AND ej -> el\nbj OR bi -> bk\nkk RSHIFT 3 -> km\nNOT cn -> co\ngn AND gp -> gq\ncq AND cs -> ct\neo LSHIFT 15 -> es\nlg OR lm -> ln\ndy OR ej -> ek\nNOT di -> dj\n1 AND fi -> fj\nkf LSHIFT 15 -> kj\nNOT jy -> jz\nNOT ft -> fu\nfs AND fu -> fv\nNOT hr -> hs\nck OR cl -> cm\njp RSHIFT 5 -> js\niv OR jb -> jc\nis OR it -> iu\nld OR le -> lf\nNOT fc -> fd\nNOT dm -> dn\nbn OR by -> bz\naj AND al -> am\ncd LSHIFT 15 -> ch\njp AND ka -> kc\nci OR ct -> cu\ngv AND gx -> gy\nde AND dk -> dm\nx RSHIFT 5 -> aa\net RSHIFT 2 -> eu\nx RSHIFT 1 -> aq\nia OR ig -> ih\nbk LSHIFT 1 -> ce\ny OR ae -> af\nNOT ca -> cb\ne AND f -> h\nia AND ig -> ii\nck AND cl -> cn\nNOT jh -> ji\nz OR aa -> ab\n1 AND en -> eo\nib AND ic -> ie\nNOT eh -> ei\niy AND ja -> jb\nNOT bb -> bc\nha OR gz -> hb\n1 AND cx -> cy\nNOT ax -> ay\nev OR ew -> ex\nbn RSHIFT 2 -> bo\ner OR es -> et\neu OR fa -> fb\njp OR ka -> kb\nea AND eb -> ed\nk AND m -> n\net RSHIFT 3 -> ev\net RSHIFT 5 -> ew\nhz RSHIFT 1 -> is\nki OR kj -> kk\nNOT h -> i\nlv LSHIFT 15 -> lz\nas RSHIFT 1 -> bl\nhu LSHIFT 15 -> hy\niw AND ix -> iz\nlf RSHIFT 1 -> ly\nfp OR fv -> fw\n1 AND am -> an\nap LSHIFT 1 -> bj\nu LSHIFT 1 -> ao\nb RSHIFT 5 -> f\njq AND jw -> jy\niu RSHIFT 3 -> iw\nih AND ij -> ik\nNOT iz -> ja\nde OR dk -> dl\niu OR jf -> jg\nas AND bd -> bf\nb RSHIFT 3 -> e\njq OR jw -> jx\niv AND jb -> jd\ncg OR ch -> ci\niu AND jf -> jh\nlx -> a\n1 AND cc -> cd\nly OR lz -> ma\nNOT el -> em\n1 AND bh -> bi\nfb AND fd -> fe\nlf OR lq -> lr\nbn RSHIFT 3 -> bp\nbn AND by -> ca\naf AND ah -> ai\ncf LSHIFT 1 -> cz\ndw OR dx -> dy\ngj AND gu -> gw\njg AND ji -> jj\njr OR js -> jt\nbl OR bm -> bn\ngj RSHIFT 2 -> gk\ncj OR cp -> cq\ngj OR gu -> gv\nb OR n -> o\no AND q -> r\nbi LSHIFT 15 -> bm\ndy RSHIFT 1 -> er\ncu AND cw -> cx\niw OR ix -> iy\nhc OR hd -> he\n0 -> c\ndb OR dc -> dd\nkk RSHIFT 2 -> kl\neq LSHIFT 1 -> fk\ndz OR ef -> eg\nNOT ed -> ee\nlw OR lv -> lx\nfw AND fy -> fz\ndz AND ef -> eh\njp RSHIFT 3 -> jr\nlg AND lm -> lo\nci RSHIFT 2 -> cj\nbe AND bg -> bh\nlc LSHIFT 1 -> lw\nhm AND ho -> hp\njr AND js -> ju\n1 AND io -> ip\ncm AND co -> cp\nib OR ic -> id\nNOT bf -> bg\nfo RSHIFT 5 -> fr\nip LSHIFT 15 -> it\njt AND jv -> jw\njc AND je -> jf\ndu OR dt -> dv\nNOT fx -> fy\naw AND ay -> az\nge LSHIFT 15 -> gi\nNOT ak -> al\nfm OR fn -> fo\nff AND fh -> fi\nci RSHIFT 5 -> cl\ncz OR cy -> da\nNOT ey -> ez\nNOT ju -> jv\nNOT ls -> lt\nkk AND kv -> kx\nNOT ii -> ij\nkl AND kr -> kt\njk LSHIFT 15 -> jo\ne OR f -> g\nNOT bs -> bt\nhi AND hk -> hl\nhz OR ik -> il\nek AND em -> en\nao OR an -> ap\ndv LSHIFT 1 -> ep\nan LSHIFT 15 -> ar\nfo RSHIFT 1 -> gh\nNOT im -> in\nkk RSHIFT 1 -> ld\nhw LSHIFT 1 -> iq\nec AND ee -> ef\nhb LSHIFT 1 -> hv\nkb AND kd -> ke\nx AND ai -> ak\ndd AND do -> dq\naq OR ar -> as\niq OR ip -> ir\ndl AND dn -> do\niu RSHIFT 5 -> ix\nas OR bd -> be\nNOT go -> gp\nfk OR fj -> fl\njm LSHIFT 1 -> kg\nNOT cv -> cw\ndp AND dr -> ds\ndt LSHIFT 15 -> dx\net RSHIFT 1 -> fm\ndy RSHIFT 3 -> ea\nfp AND fv -> fx\nNOT p -> q\ndd RSHIFT 2 -> de\neu AND fa -> fc\nba AND bc -> bd\ndh AND dj -> dk\nlr AND lt -> lu\nhe RSHIFT 1 -> hx\nex AND ez -> fa\ndf OR dg -> dh\nfj LSHIFT 15 -> fn\nNOT kx -> ky\ngk OR gq -> gr\ndy RSHIFT 2 -> dz\ngh OR gi -> gj\nlj AND ll -> lm\nx OR ai -> aj\nbz AND cb -> cc\n1 AND lu -> lv\nas RSHIFT 3 -> au\nce OR cd -> cf\nil AND in -> io\ndd RSHIFT 1 -> dw\nNOT lo -> lp\nc LSHIFT 1 -> t\ndd RSHIFT 3 -> df\ndd RSHIFT 5 -> dg\nlh AND li -> lk\nlf RSHIFT 5 -> li\ndy RSHIFT 5 -> eb\nNOT kt -> ku\nat OR az -> ba\nx RSHIFT 3 -> z\nNOT lk -> ll\nlb OR la -> lc\n1 AND r -> s\nlh OR li -> lj\nln AND lp -> lq\nkk RSHIFT 5 -> kn\nea OR eb -> ec\nci AND ct -> cv\nb RSHIFT 2 -> d\njp RSHIFT 1 -> ki\nNOT cr -> cs\nNOT jd -> je\njp RSHIFT 2 -> jq\njn OR jo -> jp\nlf RSHIFT 3 -> lh\n1 AND ds -> dt\nlf AND lq -> ls\nla LSHIFT 15 -> le\nNOT fg -> fh\nat AND az -> bb\nau AND av -> ax\nkw AND ky -> kz\nv OR w -> x\nkk OR kv -> kw\nks AND ku -> kv\nkh LSHIFT 1 -> lb\n1 AND kz -> la\nNOT kc -> kd\nx RSHIFT 2 -> y\net OR fe -> ff\net AND fe -> fg\nNOT ac -> ad\njl OR jk -> jm\n1 AND jj -> jk\nbn RSHIFT 1 -> cg\nNOT kp -> kq\nci RSHIFT 3 -> ck\nev AND ew -> ey\n1 AND ke -> kf\ncj AND cp -> cr\nir LSHIFT 1 -> jl\nNOT gw -> gx\nas RSHIFT 2 -> at\niu RSHIFT 1 -> jn\ncy LSHIFT 15 -> dc\nhg OR hh -> hi\nci RSHIFT 1 -> db\nau OR av -> aw\nkm AND kn -> kp\ngj RSHIFT 1 -> hc\niu RSHIFT 2 -> iv\nab AND ad -> ae\nda LSHIFT 1 -> du\nNOT bw -> bx\nkm OR kn -> ko\nko AND kq -> kr\nbv AND bx -> by\nkl OR kr -> ks\n1 AND ht -> hu\ndf AND dg -> di\nNOT ag -> ah\nd OR j -> k\nd AND j -> l\nb AND n -> p\ngf OR ge -> gg\ngg LSHIFT 1 -> ha\nbn RSHIFT 5 -> bq\nbo OR bu -> bv\n1 AND gy -> gz\ns LSHIFT 15 -> w\nNOT ie -> if\nas RSHIFT 5 -> av\nbo AND bu -> bw\nhz AND ik -> im\nbp AND bq -> bs\nb RSHIFT 1 -> v\nNOT l -> m\nbp OR bq -> br\ng AND i -> j\nbr AND bt -> bu\nt OR s -> u\nhz RSHIFT 5 -> ic\ngk AND gq -> gs\nfl LSHIFT 1 -> gf\nhe RSHIFT 3 -> hg\ngz LSHIFT 15 -> hd\nhf OR hl -> hm\n1 AND gd -> ge\nfo OR fz -> ga\nid AND if -> ig\nfo AND fz -> gb\ngr AND gt -> gu\nhe OR hp -> hq\nfq AND fr -> ft\nga AND gc -> gd\nfo RSHIFT 2 -> fp\ngl OR gm -> gn\nhg AND hh -> hj\nNOT hn -> ho\ngl AND gm -> go\nhe RSHIFT 5 -> hh\nNOT gb -> gc\nhq AND hs -> ht\nhz RSHIFT 3 -> ib\nhz RSHIFT 2 -> ia\nfq OR fr -> fs\nhx OR hy -> hz\nhe AND hp -> hr\ngj RSHIFT 5 -> gm\nhf AND hl -> hn\nhv OR hu -> hw\nNOT hj -> hk\ngj RSHIFT 3 -> gl\nfo RSHIFT 3 -> fq\nhe RSHIFT 2 -> hf"

data Wire = Int Word16 | String String deriving (Show, Eq)

data Op
  = AND Wire Wire
  | OR Wire Wire
  | LSHIFT Wire Int
  | RSHIFT Wire Int
  | NOT Wire
  | DIRECT Wire
  deriving (Show, Eq)

parseLine :: String -> (String, Op)
parseLine s =
  case words s of
    [a, "->", b] -> (b, DIRECT (String a))
    [a, "AND", b, "->", c] -> (c, AND (String a) (String b))
    [a, "OR", b, "->", c] -> (c, OR (String a) (String b))
    [a, "LSHIFT", b, "->", c] -> (c, LSHIFT (String a) (read b))
    [a, "RSHIFT", b, "->", c] -> (c, RSHIFT (String a) (read b))
    ["NOT", a, "->", b] -> (b, NOT (String a))
    _ -> error ("Can't parse: " ++ s)

signals :: Map String Op
signals = fromList $ parseLine <$> lines input

part1 :: Word16
part1 =
  let final = processSignals signals
      v = fromJust $ lookup "a" final
   in case v of
        DIRECT (Int i) -> i
        _ -> error "nope"

processSignals :: Map String Op -> Map String Op
processSignals m = processSignals' m
  where
    processSignals' :: Map String Op -> Map String Op
    processSignals' m'
      | length directs == length m' = m'
      | otherwise = processSignals' $! ( processOp <$> updateOpsInMap' directs m')
      where
        directs :: List (String, Word16)
        directs = map (\(k, v) -> (k, case v of DIRECT (Int i) -> i; _ -> error "nope")) (toList (directSignals m'))

        updateOpsInMap' :: List (String, Word16) -> Map String Op -> Map String Op
        updateOpsInMap' ops m'' = updateOpsInMap'' ops m''
          where
            updateOpsInMap'' :: List (String, Word16) -> Map String Op -> Map String Op
            updateOpsInMap'' [] m''' = m'''
            updateOpsInMap'' ((k, v) : xs) m''' = updateOpsInMap'' xs $! updateOpInMap (k, v) m'''

updateOpInMap :: (String, Word16) -> Map String Op -> Map String Op
updateOpInMap (k, v) m =
  mapWithKey
    ( \_ v' ->
        case v' of
          AND (String s1) (String s2) ->
            if s1 == k
              then AND (Int v) (String s2)
              else
                if s2 == k
                  then AND (String s1) (Int v)
                  else v'
          AND (String s1) (Int i) ->
            if s1 == k
              then AND (Int v) (Int i)
              else v'
          AND (Int i) (String s1) ->
            if s1 == k
              then AND (Int v) (Int i)
              else v'
          OR (String s1) (String s2) ->
            if s1 == k
              then OR (Int v) (String s2)
              else
                if s2 == k
                  then OR (String s1) (Int v)
                  else v'
          OR (String s1) (Int i) ->
            if s1 == k
              then OR (Int v) (Int i)
              else v'
          OR (Int i) (String s1) ->
            if s1 == k
              then OR (Int v) (Int i)
              else v'
          LSHIFT (String s1) i ->
            if s1 == k
              then LSHIFT (Int v) i
              else v'
          RSHIFT (String s1) i ->
            if s1 == k
              then RSHIFT (Int v) i
              else v'
          NOT (String s1) ->
            if s1 == k
              then NOT (Int v)
              else v'
          a -> a
    )
    m

directSignals :: Map String Op -> Map String Op
directSignals = filterWithKey (\_ v -> isDirectInt v) . mapWithKey (\_ v -> processOp v)

processOp :: Op -> Op
processOp (DIRECT (String s)) = if isNumeric s then DIRECT (Int (read s)) else DIRECT (String s)
processOp (AND (Int i) (Int j)) = DIRECT (Int (i .&. j))
processOp (OR (Int i) (Int j)) = DIRECT (Int (i .|. j))
processOp (LSHIFT (Int i) j) = DIRECT (Int (i `shift` j))
processOp (RSHIFT (Int i) j) = DIRECT (Int (i `shiftR` j))
processOp (NOT (Int i)) = DIRECT (Int (complement i))
processOp a = a

isNumeric :: String -> Bool
isNumeric s = all isDigit s

isDirectInt :: Op -> Bool
isDirectInt (DIRECT (Int _)) = True
isDirectInt _ = False
