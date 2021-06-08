-- Prática 05 de Haskell
-- Nome: Natã Schmitt

bmi :: Float -> Float -> String
bmi peso altura =
    let imc = peso * altura^2
    in if imc < 30 && imc > 18.5 then "NORMAL" else if peso >= 30 then "ACIMA" else "ABAIXO"

bmi' :: Float -> Float -> String
bmi' peso altura
  | imc < 30 && imc > 18.5 = "NORMAL"
  | peso >= 30 = "ACIMA"
  | otherwise = "ABAIXO"
  where
      imc = peso * altura ^ 2

cpfValid :: [Int] -> Bool
cpfValid cpf = dv1 == cpf !! 9 && dv2 == cpf !! 10
  where digits = take 9 cpf
        dv1 = cpfDV digits [10,9..]
        dv2 = cpfDV (digits ++ [dv1]) [11,10..]

cpfDV :: [Int] -> [Int] -> Int
cpfDV digits mults =
    let expr = sum (zipWith (*) digits mults) `mod` 11
    in if expr < 2 then 0 else 11 - expr

andTable :: [(Bool, Bool, Bool)]
andTable = [(x,y,x && y) | x <- [True,False], y <- [True, False]]

