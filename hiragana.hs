import Data.List

vowels = "aeiou"

split :: String -> [String] --find a better function name
split "" = [""]
split x
    | elem (head x) (vowels ++ " .") = [[head x]] ++ split (tail x)
    | elem (head x) "mn" && elem (head $ tail x) vowels = [[head x]++[head $ tail x]] ++ split (tail $ tail x)
    | elem (head x) "mn" = [[head x]] ++ split (tail x)
    | elem (head $ tail x) vowels = [[head x]++[head $ tail x]] ++ split (tail $ tail x)
    | otherwise = [[head x] ++ [head $ tail x] ++ [head $ tail $ tail x]] ++ split (tail $ tail $ tail x)

translit :: String -> String
translit xs = concat [romajiToHiragana x patterns| x <- (split xs)]

romajiToHiragana :: String -> [(String, String)] -> String
romajiToHiragana i [] = i
romajiToHiragana i (x:xs)
    | fst x == i = snd x
    | otherwise = romajiToHiragana i xs

patterns :: [(String, String)]
patterns =
    [("a","あ")   ,("i","い")   ,("u","う")   ,("e","え")
    ,("o","お")   ,("ka","か")  ,("ki","き")  ,("ku","く")
    ,("ke","け")  ,("ko","こ")  ,("ga","が")  ,("gi","ぎ")
    ,("gu","ぐ")  ,("ge","げ")  ,("go","ご")  ,("sa","さ")
    ,("shi","し") ,("su","す")  ,("se","せ")  ,("so","そ")
    ,("za","ざ")  ,("ji","じ")  ,("zu","ず")  ,("ze","ぜ")
    ,("zo","ぞ")  ,("ta","た")  ,("chi","ち") ,("tsu","つ")
    ,("te","て")  ,("to","と")  ,("da","だ")  ,("de","で")
    ,("do","ど")  ,("na","な")  ,("ni","に")  ,("nu","ぬ")
    ,("ne","ね")  ,("no","の")  ,("ha","は")  ,("hi","ひ")
    ,("fu","ふ")  ,("he","へ")  ,("ho","ほ")  ,("ba","ば")
    ,("bi","び")  ,("bu","ぶ")  ,("be","べ")  ,("bo","ぼ")
    ,("pa","ぱ")  ,("pi","ぴ")  ,("pu","ぷ")  ,("pe","ぺ")
    ,("po","ぽ")  ,("ma","ま")  ,("mi","み")  ,("mu","む")
    ,("me","め")  ,("mo","も")  ,("ya","や")  ,("yu","ゆ")
    ,("yo","よ")  ,("ra","ら")  ,("ri","り")  ,("ru","る")
    ,("re","れ")  ,("ro","ろ")  ,("wa","わ")  ,("wo","を")
    ,("n","ん")   ,("m","ん")   ,(".","。") ,(" "," ")]
