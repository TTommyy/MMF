-- Tomasz Koczar --
-- 2023AD --

-- Modele Matematyki Finansowej --
-- Zestaw I --


{-

  I.1. Efektywna roczna stopa procentowa wynosi 7%.
   Wyliczyć nominalne stopy procentowe dające takie oprocentowanie
  przy kapitalizacji:
  • prostej
  • złożonej:
    – kwartalnej;
    – miesięcznej;
    – tygodniowej;
    – dziennej;
  • ciągłej.

-}

{-
  Przypadek dyskretny:
  Przkszatlcenia:
  p - nominalne op
  m - ilosc kapitazlicji w roku
  wzor na rrso: r = (1 + p/m)^m - 1
  r + 1 = (1 + p/m)^m
  root(r+1, m) = 1 + p/m
  (root(r+1, m) - 1) * m = p

  ostatecznie: p =  (root(r+1, m) - 1) * m
-}

-- | Oblicza nominalna stope procentowa dla czasu dyskretnego
-- @param r - Efyktywna stopa roczna
-- @param m - ilosc kapitalizacji w ciagu roku
-- @returns p - nominalna stopa procentowa
calculate_nominal_intrest_rates_discret_time :: Float -> Float -> Float
calculate_nominal_intrest_rates_discret_time r m = ((r + 1) ** (1 / m) - 1) * m * 100

numbersOfCapitalizations :: [Float]
numbersOfCapitalizations = [1, 4, 12, 52, 365]

{- Przypadek ciagly:
  wzor na rrso: r = (1 + p/m)^m -
  r + 1 = (1 + p/m)^ m = (1+ 1/(m/p))^m = e ^ p
  p = ln(r +1)
-}
-- | Oblicza nominalna stope procentowa dla czasu dyskretnego
-- @param r - Efyktywna stopa roczna
-- @returns p - nominalna stopa procentowa
calculate_nominal_intrest_rates_contious_time :: Float -> Float
calculate_nominal_intrest_rates_contious_time r = log(r + 1) * 100

main:: IO ()
main = do
  putStrLn "Numbers of capitalizations:"
  print numbersOfCapitalizations

  let fixed_intrest_rates_calculation = (\m -> calculate_nominal_intrest_rates_discret_time 0.07 m)
  let nominalIntrestRates = map fixed_intrest_rates_calculation numbersOfCapitalizations

  putStrLn "Nominal intrest rates:"
  print nominalIntrestRates
  putStrLn ""
-- [7.0000052,6.8233967,6.784916,6.77042,6.7660213] --

  let nominalIntrestRate_ConTime =  calculate_nominal_intrest_rates_contious_time 0.07

  putStrLn "Nominal intrest rate while continus time is assumed:"
  print nominalIntrestRate_ConTime
--6.76587