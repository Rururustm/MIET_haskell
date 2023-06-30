-- Не забудьте добавить тесты.

module Poly where

-- Многочлены
-- a -- тип коэффициентов, список начинается со свободного члена.
-- Бонус: при решении следующих заданий подумайте, какие стали бы проще или
-- сложнее при обратном порядке коэффициентов (и добавьте комментарий).
newtype Poly a = P [a]

-- Задание 1 -----------------------------------------

-- Определите многочлен $x$.
x :: Num a => Poly a
x = P [0, 1]

-- Задание 2 -----------------------------------------

-- Функция, считающая значение многочлена в точке
applyPoly :: Num a => Poly a -> a -> a
applyPoly (P []) _ = 0
applyPoly (P (a0:as)) x0 = a0 + x0 * applyPoly (P as) x0

-- Задание 3 ----------------------------------------

-- Определите равенство многочленов
-- Заметьте, что многочлены с разными списками коэффициентов
-- могут быть равны! Подумайте, почему.
instance (Num a, Eq a) => Eq (Poly a) where
    (==) (P []) (P []) = True
    (==) (P as) (P []) = all (== 0) as
    (==) (P []) (P bs) = all (== 0) bs
    (==) (P (a:as)) (P(b:bs)) = (a == 0 && b == 0 && all (== 0) as && all (== 0) bs) || (a == b && P as == P bs)
 
-- Задание 4 -----------------------------------------

-- Определите перевод многочлена в строку. 
-- Это должна быть стандартная математическая запись, 
-- например: show (3 * x * x + 1) == "3 * x^2 + 1").
-- (* и + для многочленов можно будет использовать после задания 6.)
instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P p) = let ps = reverse $ filter ((/= 0) . fst) $ zip p ([0..] :: [Integer])
                     showPoly [] = ["0"]
                     showPoly (a:as) = kfst a : map ksnd as where
                        showPlus c True = if head (show c) == '-' 
                                            then " - "
                                            else " + "
                        
                        showPlus c False = if head (show c) == '-' 
                                            then "-"
                                            else ""

                        showValue c pow | ((c == 1) || (c == -1))  && pow > 0 = ""
                                        | pow == 0 = show (abs c)
                                        | otherwise = show (abs c) ++ " * "

                        showPower 0 = ""
                        showPower 1 = "x"
                        showPower n = "x^" ++ show n

                        iter c state = showPlus (fst c) state ++ showValue (fst c) (snd c) ++ showPower (snd c)
                        kfst c  = iter c False
                        ksnd c = iter c True
                        
                  in concat $ showPoly ps

-- Задание 5 -----------------------------------------

-- Определите сложение многочленов
plus :: Num a => Poly a -> Poly a -> Poly a
plus (P p1) (P p2) = P $ res_plus p1 p2
    where
        res_plus [] b = b
        res_plus a [] = a
        res_plus (a : as) (b : bs) = (a + b) : res_plus as bs

-- Задание 6 -----------------------------------------

-- Определите умножение многочленов
times :: Num a => Poly a -> Poly a -> Poly a
times (P []) _ = P []
times _ (P []) = P []
times (P (a : as)) (P bs) = P (map (a *) bs) `plus` times (P as) (P (0:bs))

-- Задание 7 -----------------------------------------

-- Сделайте многочлены числовым типом
instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P poly) = P $ map negate poly
    fromInteger s = P [fromIntegral s]
    -- Эти функции оставить как undefined, поскольку для 
    -- многочленов они не имеют математического смысла
    abs    = undefined
    signum = undefined

-- Задание 8 -----------------------------------------

-- Реализуйте nderiv через deriv
class Num a => Differentiable a where
    -- взятие производной
    deriv  :: a -> a
    -- взятие n-ной производной
    nderiv :: Int -> a -> a
    nderiv n poly = iterate deriv poly !! n

-- Задание 9 -----------------------------------------

-- Определите экземпляр класса типов
instance Num a => Differentiable (Poly a) where
    deriv (P []) = P []
    deriv (P (_:ps)) = P $ deriv' ps 1 where
        deriv' [] _ = []
        deriv' (a:as) n = a * n : deriv' as (n + 1)
