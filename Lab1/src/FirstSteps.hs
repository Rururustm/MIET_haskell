module FirstSteps 
where
import Data.Word (Word8)

-- xor x y находит "исключающее или" x и y
-- xor True False == True
-- xor True True == False

-- используйте сопоставление с образцом
xor :: Bool -> Bool -> Bool
xor x y = if x /= y then True else False

-- max3 x y z находит максимум из x, y и z
-- max3 1 3 2 == 3
-- max3 5 2 5 == 5
-- median3 x y z находит второе по величине число (медиану)
-- median3 1 3 2 == 2
-- median3 5 2 5 == 5
max3, median3 :: Integer -> Integer -> Integer -> Integer
max3 x y z = if x >= y && x >= z then x else if y >= x && y >= z then y else z

median3 x y z = if x >= y && x <= z then x else if y >= x && y <= z then y else z

-- Типы данных, описывающие цвета в моделях 
-- RGB (https://ru.wikipedia.org/wiki/RGB), компоненты от 0 до 255
-- и CMYK (https://ru.wikipedia.org/wiki/CMYK), компоненты от 0.0 до 1.0
data RGB = RGB { red :: Word8, green :: Word8, blue :: Word8 } deriving (Eq, Show, Read)
data CMYK = CMYK { cyan :: Double, magenta :: Double, yellow :: Double, black :: Double } deriving (Eq, Show, Read)
-- Задайте функцию для их преобразования
-- (формулы из http://www.codeproject.com/Articles/4488/XCmyk-CMYK-to-RGB-Calculator-with-source-code):
-- Black   = min(1-Red, 1-Green, 1-Blue)
-- Cyan    = (1-Red-Black) / (1-Black)
-- Magenta = (1-Green-Black) / (1-Black)
-- Yellow  = (1-Blue-Black) / (1-Black) 
-- где значения Red, Green и Blue нормализованы от 0 до 1).

-- Заметьте, что (/) для Int не работает, и неявного преобразования Int в Double нет.
-- Это преобразование производится с помощью функции fromIntegral.
rbgToCmyk :: RGB -> CMYK
rbgToCmyk color = new_color where
  r         = fromIntegral (red color) / 255
  g         = fromIntegral (green color) / 255
  b         = fromIntegral (blue color) / 255
  k         = minimum [1-r, 1-g, 1-b]
  c         = (1-r-k) / (1-k)
  m         = (1-g-k) / (1-k)
  y         = (1-b-k) / (1-k) 
  new_color = if k == 1 then CMYK 0 0 0 k else CMYK c m y k

-- geomProgression b q n находит n-й (считая с 0) член 
-- геометрической прогрессии, нулевой член которой -- b, 
-- а знаменатель -- q.
-- geomProgression 3.0 2.0 2 == 12.0

-- используйте рекурсию
-- не забудьте случаи n < 0 и n == 0.
geomProgression :: Double -> Double -> Integer -> Double
geomProgression b q 0 = b
geomProgression b q n | n < 0 = error " n must be > 0 "
                      | otherwise = geomProgression (b * q) q (n - 1)

-- coprime a b определяет, являются ли a и b взаимно простыми
-- (определение: Целые числа называются взаимно простыми, 
-- если они не имеют никаких общих делителей, кроме +/-1)
-- coprime 10 15 == False
-- coprime 12 35 == True

-- Используйте рекурсию
-- Есть ли важные пограничные случаи или вспомогательные функции? Не забудьте добавить их в тесты.

-- Полезные функции в Prelude (автоматически загруженной
-- части стандартной библиотеки): quot, rem, quotRem 
-- (или div, mod, divMod в зависимости от того, как 
-- обрабатываете отрицательные числа)
-- https://hackage.haskell.org/package/base-4.9.0.0/docs/Prelude.html
coprime :: Integer -> Integer -> Bool
coprime a b = ((gcd' a b) == 1 || (gcd' a b) == (-1))

gcd' :: Integer -> Integer -> Integer
gcd' a b 
  | (a == 0) && (b == 0) = 0
  |  a == 0              = abs b
  |  b == 0              = abs a
  |  a == b              = abs a 
  | otherwise            = helper aa bb (aa `div` bb) (aa `mod` bb) where
                             aa = max a b
                             bb = min a b
                             helper a b u 0 = b
                             helper a b u v = helper b v (b `div` v) (b `mod` v)

